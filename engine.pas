{
Модуль классов движка

Версия: 0.0.1.2
}

unit engine;


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Crt,
    dictionary, settings, obj_proto;

{ Режимы запуска движка }
const
  RUN_MODE_SINGLE: AnsiString = 'single';
  RUN_MODE_LOOP: AnsiString = 'loop';
  RUN_MODE_DIAGNOSTIC: AnsiString = 'diagnostic';

  DEFAULT_TIMER_TICK: Integer = 1000;


type
    {
    TICLoggerProto - абстрактный тип движка
    РАГИСТРАТОРА ДАННЫХ из различных источников в таблице журнала БД.
    }
    TICLoggerProto = class(TObject)
    private
      { Менеджер настроек }
      FSettingsManager: TICSettingsManager;
      { Словарь зарегистрированных объектов-источников данных }
      FSources: TStrDictionary;
      { Словарь зарегистрированных объектов-получателей данных }
      FDestinations: TStrDictionary;

      { Флаг запущенного движка }
      FRunning: Boolean;

    public
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;
      procedure Free;

      {
      Проинициализировать конфигурационные переменные в соответствии с настройками
      @return True/False
      }
      function InitSettings(): Boolean;
      {
      Регистрация нового объекта в словаре внутренних объектов. Регистрация производиться по имени объекта.
      @param Obj Регистрируемый объект
      @param Objects: Словарь регистрации объектов.
      @return True -  регистрация прошла успешно / False - ошибка
      }
      function RegObject(Obj: TICObjectProto; Objects: TStrDictionary): Boolean;
      {Регистрация объекта-источника данных}
      function RegSource(Obj: TICObjectProto): Boolean;
      {Регистрация объекта-получателя данных}
      function RegDestination(Obj: TICObjectProto): Boolean;
      {
      Поиск объекта в зарегистрированных по имени
      @param sObjName Наименование объекта
      @param Objects: Словарь регистрации объектов.
      @return Найденный объект или nil если объект не найден среди зарегистрированных
      }
      function FindObject(sObjName: AnsiString; Objects: TStrDictionary): TICObjectProto;
      { Поиск объекта-источника данных }
      function FindSource(sObjName: AnsiString): TICObjectProto;
      { Поиск объекта-получателя данных }
      function FindDestination(sObjName: AnsiString): TICObjectProto;
      {
      Метод создания объекта контроллера данных с инициализацией его свойств
      @param Properties Словаряь свойств объекта
      @return Созданный объект или nil в случае ошибки
      }
      function CreateDataCtrl(Properties: TStrDictionary): TICObjectProto;

      {
      Создание объектов-источников данных по именам
      @param ObjectNames Список имен объектов
      @return Список созданных объектов
      }
      function CreateSources(ObjectNames: TStringList=nil): TList;
      {
      Создание объектов-источников данных по именам
      @param ObjectNames Список имен объектов
      @return Список созданных объектов
      }
      function CreateDestinations(ObjectNames: TStringList=nil): TList;

      {
      Получить состояние тега источника данных в виде строки
      @param aSourceName Наименование источника данных
      @param aTag Наименование тега-поля источника данных
      @return Значение тега-поля источника данных в виде строки или пустая строка,
              если не найдено
      }
      function GetSourceStateAsString(aSourceName, aTag: AnsiString): AnsiString;

    end;

    {
    TICLogger - Движок РЕГИСТРАТОРА ДАННЫХ из различных источников в таблице журнала БД.
    }
    TICLogger = class(TICLoggerProto)
    private
      { Признак запущеной обработки тика }
      FIsTick: Boolean;
    public
      { Конструктор }
      constructor Create(TheOwner: TComponent);
      destructor Destroy; override;
      procedure Free;

      {
      Прочитать значение из источника данных
      @param sSrcTypeName Наименование типа источника
      @param aArgs Массив дополнительных аргументов
      @param sAddress Адрес значения в источнике данных в строковом виде
      @return Строка прочитанного значения
      }
      function ReadValueAsString(sSrcTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString): AnsiString;
      {
      Прочитать список значений из источника данных
      @param sSrcTypeName Наименование типа источника
      @param aArgs Массив дополнительных аргументов
      @param aAddresses Массив строк читаемых адресов
      @return Список строк прочитанных значений
      }
      function ReadValuesAsStrings(sSrcTypeName: AnsiString; const aArgs : Array Of Const; aAddresses : Array Of String): TStringList;

      { Запуск движка }
      procedure Start;
      { Останов движка }
      procedure Stop;
      { Обработчик одного тика таймера }
      procedure Tick;

      { Обработчик одного тика таймера. Режим стандартной работы службы }
      procedure WorkTick;

      { Обработчик одного тика таймера. Режим тестирования службы }
      procedure Test;

    published
      property IsTick: Boolean read FIsTick;

    end;

var
  { Режим тестирования службы }
  TEST_SERVICE_MODE: Boolean = False;

  { Интервал таймера обработки в миллисекундах }
  TIMER_TICK: Integer = 1000;

  {
  Объявление глобального объекта движка

  ВНИМАНИЕ! Глобальные переменные описываются в секции interface.
  Переменные определенные в секции implementation являются статическими для
  модуля.
  }
  LOGGER_ENGINE: TICLogger;


implementation

uses
  log, config, reg_data_ctrl, strfunc;

constructor TICLoggerProto.Create(TheOwner: TComponent);
begin
  inherited Create;

  // Менеджер настроек
  FSettingsManager := TICSettingsManager.Create;

  // Словарь зарегистрированных объектов
  FSources := TStrDictionary.Create;
  FDestinations := TStrDictionary.Create;

  //
  FRunning := False;

end;

destructor TICLoggerProto.Destroy;
begin
  inherited Destroy;
end;

procedure TICLoggerProto.Free;
begin
  FDestinations.Free;
  FSources.Free;
  FSettingsManager.Free;

  Free;
end;

{
Проинициализировать конфигурационные переменные в соответствии с настройками.
@return True/False
}
function TICLoggerProto.InitSettings():Boolean;
var
  ini_filename: AnsiString;
begin
  log.InfoMsg('Настройка...');

  ini_filename := FSettingsManager.GenIniFileName();

  log.DebugMsgFmt('INI Файл <%s>', [ini_filename]);
  if (ini_filename <> '') and (not FileExists(ini_filename)) then
  begin
    log.WarningMsgFmt('Файл настроек <%s> не найден. Используется файл настроек по умолчанию', [ini_filename]);
    ini_filename := '';
  end;
  Result := FSettingsManager.LoadSettings(ini_filename);

  FSettingsManager.PrintSettings;
end;

{
Регистрация нового объекта в словаре внутренних объектов.
Регистрация производиться по имени объекта.
@param Obj Регистрируемый объект
@param Objects: Словарь регистрации объектов.
@return True -  регистрация прошла успешно / False - ошибка
}
function TICLoggerProto.RegObject(Obj: TICObjectProto; Objects: TStrDictionary): Boolean;
var
  name: AnsiString;

begin
  if not Obj.IsUnknown then
  begin
    // Регистрация по имени
    name := Obj.GetName();
    Objects.AddObject(name, Obj);
    Result := True;
    Exit;
  end
  else
    log.WarningMsgFmt('Не возможно зарегистрировать объект класса <%s>', [Obj.ClassName]);
  Result := False;
end;

{Регистрация объекта-источника данных}
function TICLoggerProto.RegSource(Obj: TICObjectProto): Boolean;
begin
  Result := RegObject(Obj, FSources);
end;

{Регистрация объекта-получателя данных}
function TICLoggerProto.RegDestination(Obj: TICObjectProto): Boolean;
begin
  Result := RegObject(Obj, FDestinations);
end;

{
Поиск объекта в зарегистрированных по имени.
}
function TICLoggerProto.FindObject(sObjName: AnsiString; Objects: TStrDictionary): TICObjectProto;
begin
  if Objects.HasKey(sObjName) then
    Result := Objects.GetByName(sObjName) As TICObjectProto
  else
  begin
    log.WarningMsgFmt('Объект <%s> не найден среди зарегистрированных %s', [sObjName, Objects.GetKeysStr()]);
    Result := nil;
  end;
end;

{ Поиск объекта-источника данных }
function TICLoggerProto.FindSource(sObjName: AnsiString): TICObjectProto;
begin
  Result := FindObject(sObjName, FSources);
end;

{ Поиск объекта-получателя данных }
function TICLoggerProto.FindDestination(sObjName: AnsiString): TICObjectProto;
begin
  Result := FindObject(sObjName, FDestinations);
end;

{
Метод создания объекта контроллера данных с инициализацией его свойств.
@param (Properties  Словарь свойств контроллера данных)
@return (Объект контроллера данных или nil в случае ошибки)
}
function TICLoggerProto.CreateDataCtrl(Properties: TStrDictionary): TICObjectProto;
var
  type_name, name: AnsiString;
  ctrl_obj: TICObjectProto;
begin
  // Сначала в любом случае определяем тип источника данных
  if Properties.HasKey('type') then
  begin
    type_name := Properties.GetStrValue('type');
    name := Properties.GetStrValue('name');
    log.InfoMsgFmt('Создание объекта <%s> : <%s>', [name, type_name]);
    ctrl_obj := reg_data_ctrl.CreateRegDataCtrl(self, type_name, Properties);
    if ctrl_obj <> nil then
      begin
        Result := ctrl_obj;
        Exit;
    end;
  end
  else
  begin
    name := Properties.GetStrValue('name');
    log.ErrorMsgFmt('Ошибка создания объекта источника данных. Не определен тип объекта <%s>', [name]);
  end;
  Result := nil;
end;

{
Создание объектов-источников данных по именам
}
function TICLoggerProto.CreateSources(ObjectNames: TStringList): TList;
var
  ctrl_objects: TList;
  obj: TICObjectProto;
  obj_names_str: AnsiString;
  i: Integer;
  obj_properties: TStrDictionary;
  is_obj_names_options: Boolean;

begin
  log.InfoMsg('Создание объектов-источников...');
  ctrl_objects := TList.Create;
  is_obj_names_options := False;
  if ObjectNames = nil then
  begin
    obj_names_str := FSettingsManager.GetOptionValue('OPTIONS', 'sources');
    ObjectNames := ParseStrList(obj_names_str);
    is_obj_names_options := True;
  end;

  for i := 0 to ObjectNames.Count - 1 do
  begin
    obj_properties := FSettingsManager.BuildSection(ObjectNames[i]);

    // Создаем объекты источников данных
    obj := CreateDataCtrl(obj_properties);
    if obj <> nil then
    begin
      // Регистрируем новый объект в словаре внутренних объектов
      RegSource(obj);
      ctrl_objects.Add(obj)
    end;
  end;

  // Освободить память если мы выделяли
  if is_obj_names_options then
    ObjectNames.Free;

  Result := ctrl_objects;
end;

{
Создание объектов-получателей данных по именам
}
function TICLoggerProto.CreateDestinations(ObjectNames: TStringList): TList;
var
  ctrl_objects: TList;
  obj: TICObjectProto;
  obj_names_str: AnsiString;
  i: Integer;
  obj_properties: TStrDictionary;
  is_obj_names_options: Boolean;

begin
  log.InfoMsg('Создание объектов-получателей...');
  ctrl_objects := TList.Create;
  is_obj_names_options := False;
  if ObjectNames = nil then
  begin
    obj_names_str := FSettingsManager.GetOptionValue('OPTIONS', 'destinations');
    ObjectNames := ParseStrList(obj_names_str);
    is_obj_names_options := True;
  end;

  for i := 0 to ObjectNames.Count - 1 do
  begin
    obj_properties := FSettingsManager.BuildSection(ObjectNames[i]);

    // Создаем объекты получателей данных
    obj := CreateDataCtrl(obj_properties);
    if obj <> nil then
    begin
      // Регистрируем новый объект в словаре внутренних объектов
      RegDestination(obj);
      ctrl_objects.Add(obj)
    end;
  end;

  // Освободить память если мы выделяли
  if is_obj_names_options then
    ObjectNames.Free;

  Result := ctrl_objects;
end;

{ Получить состояние тега источника данных в виде строки }
function TICLoggerProto.GetSourceStateAsString(aSourceName, aTag: AnsiString): AnsiString;
var
  src: TICObjectProto;
begin
  src := FindSource(aSourceName);
  if src <> nil then
    Result := src.State.GetStrValue(aTag)
  else
    Result := '';
end;

constructor TICLogger.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIsTick := False;
end;

destructor TICLogger.Destroy;
begin
  inherited Destroy;
end;

procedure TICLogger.Free;
begin
  inherited Free;
end;

{ Прочитать значение из источника данных }
function TICLogger.ReadValueAsString(sSrcTypeName: AnsiString; const aArgs: Array Of Const; sAddress: AnsiString): AnsiString;
var
  ctrl_obj: TICObjectProto;
  str_list: TStringList;

begin
  Result := '';
  ctrl_obj := nil;
  str_list := nil;
  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sSrcTypeName, aArgs);
    str_list := ctrl_obj.ReadAddresses([sAddress]);
    Result := str_list.Strings[0];
  except
    log.FatalMsgFmt('Ошибка чтения значения по адресу <%s>', [sAddress]);
  end;

  if str_list <> nil then
    str_list.Free;
  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Прочитать список значений из источника данных }
function TICLogger.ReadValuesAsStrings(sSrcTypeName: AnsiString; const aArgs: Array Of Const; aAddresses: Array Of String): TStringList;
var
  ctrl_obj: TICObjectProto;
  str_list: TStringList;

begin
  Result := nil;
  ctrl_obj := nil;
  str_list := nil;

  try
    ctrl_obj := CreateRegDataCtrlArgs(self, sSrcTypeName, aArgs);
    str_list := ctrl_obj.ReadAddresses(aAddresses);
    Result := str_list;
  except
    log.FatalMsg('Ошибка чтения значений по адресам:');
  end;

  if ctrl_obj <> nil then
    ctrl_obj.Free;
end;

{ Запустить движок }
procedure TICLogger.Start;
begin
  log.InfoMsg('Запуск');

  // Загрузить данные из настроечного файла
  if InitSettings() then
  begin
    // Создаем объекты
    CreateSources();
    CreateDestinations();

    FRunning := True;
  end
  else
    log.ErrorMsg('Ошибка загрузки данных настройки');

end;

procedure TICLogger.Stop;
begin
  FRunning := False;
  log.InfoMsg('Останов');
end;

{ Запустить движок в режиме тестирования }
procedure TICLogger.Test;
begin
  log.InfoMsg('Режим тестирования службы')
end;

procedure TICLogger.WorkTick;
var
  i: Integer;
  source: TICObjectProto;
  destination: TICObjectProto;
  keys: TStringList;
  key: AnsiString;
begin
  log.InfoMsg('Начало блока чтения/записи');

  // Сначала читаем значения источников данных
  try
    keys := FSources.GetKeys();
    // log.DebugMsgFmt('Всего источников данных <%d>', [keys.Count]);
    for i := 0 to keys.Count - 1 do
    begin
      key := FSources.GetKey(i);
      // log.DebugMsgFmt('Чтение данных из источника <%s>', [key]);
      source := FSources.GetByName(key) As TICObjectProto;
      // log.DebugMsg('Чтение всех данных');
      source.ReadAll();
    end;
  except
    log.FatalMsg('Ошибка чтения из источников данных');
  end;

  // Затем производим запись данных в объекты получатели данных
  try
    keys := FDestinations.GetKeys();
    log.DebugMsgFmt('Всего приемников данных <%d>', [keys.Count]);
    for i := 0 to keys.Count - 1 do
    begin
      key := FDestinations.GetKey(i);
      destination := FDestinations.GetByName(key) As TICObjectProto;
      destination.WriteAll();
    end;
  except
    log.FatalMsg('Ошибка записи данных в объекты-получатели');
  end;

  log.InfoMsg('Окончание блока чтения/записи');
end;

procedure TICLogger.Tick;
begin
  // ВНИМАНИЕ! Проверяем если предыдущий тик еще не закончен,
  // то новый не запускаем
  if FIsTick then
  begin
    log.WarningMsgFmt('Пропущена обработка тика в %s', [FormatDateTime('c', Now())]);
    Exit;
  end;

  // Выставить флаг запущенного тика
  FIsTick := True;

  if TEST_SERVICE_MODE then
     Test
  else
     WorkTick;

  // Сбросить флаг запущенного тика
  FIsTick := False;
end;

end.

