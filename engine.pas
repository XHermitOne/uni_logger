{
Модуль классов движка
}

unit engine;


{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Crt,
    //XmlRpcServer, XmlRpcTypes,
    dictionary, settings, obj_proto;

{ Режимы запуска движка }
const
  RUN_MODE_SINGLE: AnsiString = 'single';
  RUN_MODE_LOOP: AnsiString = 'loop';
  RUN_MODE_DIAGNOSTIC: AnsiString = 'diagnostic';

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

    end;

    {
    TICLogger - Движок РЕГИСТРАТОРА ДАННЫХ из различных источников в таблице журнала БД.
    }
    TICLogger = class(TICLoggerProto)
    private

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

      //{ --- Используемые процедуры удаленного вызова --- }
      //{ Тестовая функция для проверки удаленного вызова процедур }
      //procedure EchoTestRpcMethod(Thread: TRpcThread; const sMethodName: string;
      //                            List: TList; Return: TRpcReturn);
      //
      //{ Функция чтения данных из источника удаленного вызова процедур }
      //procedure ReadValueAsStringRpcMethod(Thread: TRpcThread; const sMethodName: string;
      //                                     List: TList; Return: TRpcReturn);
      //
      //{ Функция чтения данных из источника удаленного вызова процедур }
      //procedure ReadValuesAsStringsRpcMethod(Thread: TRpcThread; const sMethodName: string;
      //                                       List: TList; Return: TRpcReturn);
    end;

var
  //{ Порт по умолчанию для обработки XML RPC }
  //XML_RPC_PORT: Integer = 8080;

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
  //Free;
  //FRpcServer.Free;
  inherited Destroy;
end;

procedure TICLoggerProto.Free;
begin
  FSources.Free;
  FDestinations.Free;
  FSettingsManager.Free;
  inherited Free;
end;

{
Проинициализировать конфигурационные переменные в соответствии с настройками.
@return True/False
}
function TICLoggerProto.InitSettings():Boolean;
var
  ini_filename: AnsiString;
begin
  if not ENVIRONMENT.HasKey('SETTINGS_FILENAME') then
  begin
    ini_filename := FSettingsManager.GenIniFileName();
    ENVIRONMENT.AddStrValue('SETTINGS_FILENAME', ini_filename);
  end
  else
    ini_filename := ENVIRONMENT.GetStrValue('SETTINGS_FILENAME');

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
    ctrl_obj := CreateRegDataCtrl(self, type_name, Properties);
    if ctrl_obj <> nil then
      begin
        Result := ctrl_obj;
        Exit;
    end;
  end
  else
  begin
    name := Properties.GetStrValue('name');
    log.ErrorMsgFmt('Ошибка создания объекта источника данных. Не определен тип <%s>', [name]);
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


constructor TICLogger.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
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

{ Инициализировать методы удаленного вызова }
procedure TICLogger.Start;
var
  i: Integer;
  source: TICObjectProto;
  destination: TICObjectProto;
  keys: TStringList;
  key: AnsiString;

begin
  log.InfoMsg('Запуск');

  // Создаем объекты
  CreateSources();
  CreateDestinations();
  FRunning := True;

  while FRunning do
  begin
    // Сначала читаем значения источников данных
    keys := FSources.GetKeys();
    for i := 0 to keys.Count - 1 do
    begin
      key := keys.Names[i];
      source := FSources.GetByName(key) As TICObjectProto;
      source.ReadAll();
    end;

    // Затем производим запись данных в объекты получатели данных
    keys := FDestinations.GetKeys();
    for i := 0 to keys.Count - 1 do
    begin
      key := keys.Names[i];
      destination := FDestinations.GetByName(key) As TICObjectProto;
      destination.WriteAll();
    end;
  end;
end;

procedure TICLogger.Stop;
begin
  FRunning := False;
  log.InfoMsg('Останов');
end;

//{ Тестовая функция для проверки удаленного вызова процедур }
//procedure TICLogger.EchoTestRpcMethod(Thread: TRpcThread; const sMethodName: string;
//                                      List: TList; Return: TRpcReturn);
//var
//  Msg: string;
//
//begin
//  {The parameter list is sent to your method as a TList of parameters
//   this must be casted to a parameter to be accessed. If a error occurs
//   during the execution of your method the server will fall back to a global
//   handler and try to recover in which case the stack error will be sent to
//   the client}
//
//  {grab the sent string}
//  Msg := TRpcParameter(List[0]).AsString;
//
//  log.DebugMsgFmt('Test echo. You just sent: <%s>', [Msg]);
//
//  {return a message showing what was sent}
//  Return.AddItem('You just sent: ' + Msg);
//end;
//
//{ Функция чтения данных из источника удаленного вызова процедур }
//procedure TICLogger.ReadValueAsStringRpcMethod(Thread: TRpcThread; const sMethodName: string;
//                                               List: TList; Return: TRpcReturn);
//var
//  src_type_name: AnsiString;
//  opc_server_name: AnsiString;
//  address: AnsiString;
//  opc_result: AnsiString;
//
//begin
//  src_type_name := TRpcParameter(List[0]).AsString;
//  opc_server_name := TRpcParameter(List[1]).AsString;
//  address := TRpcParameter(List[2]).AsString;
//
//  opc_result := ReadValueAsString(src_type_name, [opc_server_name], address);
//
//  {return a message showing what was sent}
//  Return.AddItem(opc_result);
//end;
//
//{ Функция чтения данных из источника удаленного вызова процедур }
//procedure TICLogger.ReadValuesAsStringsRpcMethod(Thread: TRpcThread; const sMethodName: string;
//                                                 List: TList; Return: TRpcReturn);
//var
//  src_type_name: AnsiString;
//  opc_server_name: AnsiString;
//  addresses: Array of String;
//  opc_result: TStringList;
//  i: Integer;
//
//begin
//  src_type_name := TRpcParameter(List[0]).AsString;
//  opc_server_name := TRpcParameter(List[1]).AsString;
//
//  SetLength(addresses, List.Count - 2);
//  for i := 0 to List.Count - 3 do
//  begin
//    addresses[i] := TRpcParameter(List[i + 2]).AsString;
//    log.DebugMsgFmt('Чтение тега <tag%d>. Адрес <%s>', [i, addresses[i]]);
//  end;
//
//  opc_result := ReadValuesAsStrings(src_type_name, [opc_server_name], addresses);
//  addresses := nil;
//
//  {return a message showing what was sent}
//  if opc_result <> nil then
//  begin
//    for i := 0 to opc_result.Count - 1 do
//      Return.AddItem(opc_result.Strings[i]);
//    opc_result.Free;
//    opc_result := nil;
//  end;
//end;

end.

