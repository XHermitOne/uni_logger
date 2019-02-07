{
Модуль узла OPC DA сервера

ВНИМАНИЕ! Для удаленного использования на компьютере с OPC сервером
необходимо разрешить удаленный доступ к COM серверам (для Windows 7):
Панель управления -> Администрирование -> Службы компонентов ->
Компьютеры -> Мой компьютер -> Контекстное меню -> Свойства ->
Безопасность COM -> Для секций <Права доступа> и <Разрешения на запуск и активацию> ->
Изменить умолчания... -> Добавить -> Поиск -> <Все> и <АНОНИМНЫЙ ВХОД> -> OK ->
Выставить галки <Удаленный доступ>, <Удаленный запуск>, <Локальная активация>, <Удаленная активация> ->
OK

Версия: 0.0.3.1
}

unit opc_da_node;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ActiveX,
    obj_proto, dictionary, strfunc,
    opc_client, tag_list;

const
  OPC_DA_NODE_TYPE: AnsiString = 'OPC_DA';

  RESERV_PROPERTIES: Array [1..5] Of String = ('type', 'name', 'description', 'opc_server', 'topic');

  UNKNOWN_GROUP_NAME: AnsiString = 'UNKNOWN_GROUP';

type
  {
  Класс взаимодействия с OPC сервером.
  }
  TICOPCDANode = class(TICObjectProto)

  private
    { Объект OPC клиента }
    FOPCClient: TOPCClient;

    { Наименование OPC сервера }
    FOPCServerName: AnsiString;

  public
    constructor Create;
    destructor Destroy; override;

    {
    Установить наименование OPC сервера
    @param sName Наменование OPC сервера
    }
    procedure SetOPCServerName(sName: AnsiString);

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

    { Установить свойства в виде списка параметров }
    procedure SetPropertiesArray(aArgs: Array Of Const); override;

    { Установить свойства объекта в виде словаря }
    procedure SetProperties(dProperties: TStrDictionary); override;

    {
    Фунция чтения данных
    @param sAddresses Список адресов для чтения
    @param dtTime: Время актуальности за которое необходимо получить данные.
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function Read(sAddresses: TStringList; dtTime: TDateTime = 0): TStringList; override;
    {
    Чтение значений по адресам
    @param sAddresses Массив адресов для чтения
    @param dtTime: Время актуальности за которое необходимо получить данные.
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAddresses(sAddresses: Array Of String; dtTime: TDateTime = 0): TStringList; override;
    {
    Чтение значения по адресу
    @param sAddress Строка адреса для чтения
    @param dtTime: Время актуальности за которое необходимо получить данные.
                  Если не определено, то берется текущее системное время.
    @return Прочитанное значение в виде строки.
    }
    function ReadAddress(sAddress: AnsiString; dtTime: TDateTime = 0): AnsiString; override;
    {
    Чтение всех внутренних данных, описанных в свойствах.
    @param dtTime: Время актуальности за которое необходимо получить данные.
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAll(dtTime: TDateTime = 0): TStringList; override;

end;

implementation

uses
  LCLIntf, // Для вычисления времени выполнения
  log;

constructor TICOPCDANode.Create;
begin
  inherited Create;
  FOPCClient := nil;
end;

destructor TICOPCDANode.Destroy;
begin
  if FOPCClient <> nil then
  begin
    FOPCClient.Destroy;
    FOPCClient := nil;
  end;
  inherited Destroy;
end;

{ Установить наименование OPC сервера }
procedure TICOPCDANode.SetOPCServerName(sName: AnsiString);
begin
  FOPCServerName := sName;
end;

{
Установить свойства в виде списка параметров
}
procedure TICOPCDANode.SetPropertiesArray(aArgs: Array Of Const);
begin
  if Length(aArgs) >= 1 then
  begin
    try
      { Первый элемент - это имя OPC сервера }
      { ВНИМАНИЕ! Преобразование элемента массива параметров в строку:
                  AnsiString(item.vAnsiString) }
      SetOPCServerName(AnsiString(aArgs[0].vAnsiString));

    except
      log.FatalMsgFmt('Ошибка установки массива спойств в <%s>', [ClassName]);
    end;
  end;
end;

{
Фунция чтения данных
@param sAddresses Список адресов для чтения
@param dtTime: Время актуальности за которое необходимо получить данные.
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICOPCDANode.Read(sAddresses: TStringList; dtTime: TDateTime): TStringList;
var
  i: Integer;
  tags: TStrDictionary;
  grp: TGroup;
  tag_item: TTagItem;
  value: AnsiString;
  group_name: AnsiString;

begin
  Result := TStringList.Create;

  group_name := ClassName;

  try
    // Сначала адреса указать в свойствах
    FOPCClient := TOPCClient.Create(nil);
    FOPCClient.ServerName := FOPCServerName;

    tags := CreateTags;

    //log.DebugMsgFmt('Создание группы <%s>', [GetName()]);

    grp := TGroup.Create(group_name, 500, 0);
    for i := 0 to tags.Count - 1 do
    begin
      //log.ServiceMsgFmt('Добавление тега в OPC клиент <%s> : <%s>', [tags.GetKey(i), tags.GetStrValue(tags.GetKey(i))]);
      tag_item := TTagItem.Create(tags.GetKey(i), tags.GetStrValue(tags.GetKey(i)), VT_BSTR, acRead);
      grp.AddTag(tag_item);
    end;
    FOPCClient.TagList.AddGroup(grp);

    FOPCClient.Connect;

    for i := 0 to tags.Count - 1 do
    begin
      // Чтение значения тега
      value := FOPCClient.GetTagString(FOPCClient.FindSGroupSTag(group_name, tags.GetKey(i)));
      Result.Add(value);
    end;
    FOPCClient.Disconnect;

    tags.Destroy;
  except
    FOPCClient.Disconnect;
    tags.Destroy;
    log.FatalMsgFmt('Ошибка чтения в <%s>', [ClassName]);
  end;
end;

{
Чтение всех внутренних данных, описанных в свойствах.
@param dtTime: Время актуальности за которое необходимо получить данные.
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICOPCDANode.ReadAll(dtTime: TDateTime): TStringList;
var
  i: Integer;
  tag_name: AnsiString;
  tag_value: AnsiString;

begin
  //log.DebugMsgFmt('Чтение всех внутренних данных. Объект <%s>', [Name]);
  // Кроме чтения данных обновляем
  // внутреннее состояние источника данных
  State := CreateTags;
  Result := Read(nil);
  for i := 0 to State.Count - 1 do
  begin
    tag_name := State[i];
    tag_value := Result[i];
    //log.DebugMsgFmt('Значение состояния <%s : %s>', [tag_name, tag_value]);
    State.SetStrValue(tag_name, tag_value);
  end;
end;


{
Чтение значений по адресам
@param sAddresses Массив адресов для чтения
@param dtTime: Время актуальности за которое необходимо получить данные.
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICOPCDANode.ReadAddresses(sAddresses: Array Of String; dtTime: TDateTime): TStringList;
var
  i: Integer;
  log_tags: AnsiString;
  group_name: AnsiString;
  tags: TStrDictionary;
  grp: TGroup;
  tag_item: TTagItem;
  value, address: AnsiString;

begin
  Result := TStringList.Create;

  group_name := UNKNOWN_GROUP_NAME;

  log_tags := LineEnding;
  try
    // Сначала добавить адреса в свойства
    if Properties <> nil then
      Properties.Clear
    else
      Properties := TStrDictionary.Create;

    for i := 0 to Length(sAddresses) - 1 do
    begin
      log_tags := log_tags + Format('tag%d', [i]) + ' = ' + AnsiString(sAddresses[i]) + LineEnding;
      // log.DebugMsg(Format('tag%d', [i]) + ' = ' + AnsiString(aValues[i]));
      Properties.AddStrValue(Format('tag%d', [i]),
                             { Преобразование элемента списка параметров в AnsiString:}
                             AnsiString(sAddresses[i]));
    end;

    // Сначала адреса указать в свойствах
    FOPCClient := TOPCClient.Create(nil);
    FOPCClient.ServerName := FOPCServerName;

    tags := CreateTags;

    grp := TGroup.Create(group_name, 500, 0);
    for i := 0 to tags.Count - 1 do
    begin
      address := tags.GetStrValue(tags.GetKey(i));
      tag_item := TTagItem.Create(tags.GetKey(i), address, VT_BSTR, acRead);
      grp.AddTag(tag_item);
    end;
    FOPCClient.TagList.AddGroup(grp);

    FOPCClient.Connect;

    for i := 0 to tags.Count - 1 do
    begin
      // Чтение значения тега
      value := FOPCClient.GetTagString(FOPCClient.FindSGroupSTag(group_name, tags.GetKey(i)));
      Result.Add(value);
    end;
    FOPCClient.Disconnect;

    tags.Free;

  except
    FOPCClient.Disconnect;
    tags.Free;

    if Result <> nil then
    begin
      Result.Free;
      Result := nil;
    end;
    log.FatalMsgFmt('Ошибка чтения значений адресов в <%s> %s', [ClassName, log_tags]);
  end;
end;

{
Чтение значения по адресу
@param sAddress Строка адреса для чтения
@param dtTime: Время актуальности за которое необходимо получить данные.
              Если не определено, то берется текущее системное время.
@return Прочитанное значение в виде строки.
}
function TICOPCDANode.ReadAddress(sAddress: AnsiString; dtTime: TDateTime): AnsiString;
var
  addresses: Array Of String;
  values: TStringList;
begin
  Result := '';
  SetLength(addresses, 1);
  addresses[0] := sAddress;

  values := ReadAddresses(addresses);
  if values.Count and values.Count = 1 then
    Result := values[0];

  values.Free;
end;

{ Выбрать описания тегов из свойств }
function TICOPCDANode.CreateTags(): TStrDictionary;
var
  i: Integer;
  key, value: AnsiString;
  tags: TStrDictionary;

begin
  //log.DebugMsg('Создание тегов');
  tags := TStrDictionary.Create;
  for i := 0 to Properties.Count - 1 do
  begin
    key := Properties.GetKey(i);
    if not IsStrInList(key, RESERV_PROPERTIES) then
    begin
      value := Properties.GetStrValue(key);
      //log.DebugMsgFmt('Тег <%s : %s>', [key, value]);
      tags.AddStrValue(key, value);
    end;
  end;
  Result := tags;
end;

procedure TICOPCDANode.SetProperties(dProperties: TStrDictionary);
begin
  inherited SetProperties(dProperties);

  if Properties.HasKey('opc_server') then
    SetOPCServerName(Properties.GetStrValue('opc_server'))
end;

end.

