{
Модуль узла OPC сервера

ВНИМАНИЕ! Для удаленного использования на компьютере с OPC сервером
необходимо разрешить удаленный доступ к COM серверам (для Windows 7):
Панель управления -> Администрирование -> Службы компонентов ->
Компьютеры -> Мой компьютер -> Контекстное меню -> Свойства ->
Безопасность COM -> Для секций <Права доступа> и <Разрешения на запуск и активацию> ->
Изменить умолчания... -> Добавить -> Поиск -> <Все> и <АНОНИМНЫЙ ВХОД> -> OK ->
Выставить галки <Удаленный доступ>, <Удаленный запуск>, <Локальная активация>, <Удаленная активация> ->
OK
}

unit opc_server_node;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ActiveX,
    obj_proto, dictionary, strfunc,
    opc_client, tag_list;

const
  RESERV_PROPERTIES: Array [1..4] Of String = ('type', 'name', 'description', 'opc_server');

  UNKNOWN_GROUP_NAME: AnsiString = 'UNKNOWN_GROUP';

type
  {
  Класс взаимодействия с OPC сервером.
  }
  TICOPCServerNode = class(TICObjectProto)

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

    {
    Фунция чтения данных
    @param sValues Список строк адресов читаемых значений
    @param Список строк прочитанных значений
    }
    function Read(aValues: TStringList): TStringList; override;
    {
    Функция чтения данных по адресам
    @param sValues Массив адресов читаемых значений
    @param Список строк прочитанных значений
    }
    function ReadAddresses(aValues: Array Of String): TStringList; override;
    {
    Фунция записи данных
    @param aValues Список записываемых значений
    @return True - запись прошла успешно / False - ошибка записи
    }
    function Write(aValues: TStringList): Boolean; override;

    { Установить свойства в виде списка параметров }
    procedure SetPropertiesArray(aArgs: Array Of Const); override;

end;

implementation

uses
    LCLIntf, // Для вычисления времени выполнения
    log;

constructor TICOPCServerNode.Create;
begin
     inherited Create;
     FOPCClient := nil;
end;

destructor TICOPCServerNode.Destroy;
begin
  if FOPCClient <> nil then
  begin
     FOPCClient.Destroy;
     FOPCClient := nil;
  end;
  inherited Destroy;
end;

{ Установить наименование OPC сервера }
procedure TICOPCServerNode.SetOPCServerName(sName: AnsiString);
begin
  FOPCServerName := sName;
end;

{
Установить свойства в виде списка параметров
}
procedure TICOPCServerNode.SetPropertiesArray(aArgs: Array Of Const);
begin
  if Length(aArgs) >= 1 then
  begin
    try
      { Первый элемент - это имя OPC сервера }
      { ВНИМАНИЕ! Преобразование элемента массива параметров в строку:
                  AnsiString(item.vAnsiString) }
      SetOPCServerName(AnsiString(aArgs[0].vAnsiString));

    except
      log.FatalMsgFmt('Set propertirs array in <%s>', [ClassName]);
    end;
  end;
end;

{
Фунция чтения данных
}
function TICOPCServerNode.Read(aValues: TStringList): TStringList;
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

    // log.DebugMsg(Format('Создание группы <%s>', [GetName()]));

    grp := TGroup.Create(group_name, 500, 0);
    for i := 0 to tags.Count - 1 do
    begin
      // log.ServiceMsg(Format('Добавление тега в OPC клиент <%s> : <%s>', [tags.GetKey(i), tags.GetStrValue(tags.GetKey(i))]));
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
    log.FatalMsgFmt('Read in <%s>', [ClassName]);
  end;
end;

function TICOPCServerNode.ReadAddresses(aValues: Array Of String): TStringList;
var
  i: Integer;
  log_tags: AnsiString;
  group_name: AnsiString;
  tags: TStrDictionary;
  grp: TGroup;
  tag_item: TTagItem;
  value: AnsiString;

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

    for i := 0 to Length(aValues) - 1 do
    begin
      log_tags := log_tags + Format('tag%d', [i]) + ' = ' + AnsiString(aValues[i]) + LineEnding;
      // log.DebugMsg(Format('tag%d', [i]) + ' = ' + AnsiString(aValues[i]));
      Properties.AddStrValue(Format('tag%d', [i]),
                             { Преобразование элемента списка параметров в AnsiString:}
                             AnsiString(aValues[i]));
    end;

    // Сначала адреса указать в свойствах
    FOPCClient := TOPCClient.Create(nil);
    FOPCClient.ServerName := FOPCServerName;

    tags := CreateTags;

    grp := TGroup.Create(group_name, 500, 0);
    for i := 0 to tags.Count - 1 do
    begin
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

    tags.Free;

  except
    FOPCClient.Disconnect;
    tags.Free;

    if Result <> nil then
    begin
      Result.Free;
      Result := nil;
    end;
    log.FatalMsgFmt('Read addresses value in <%s> %s', [ClassName, log_tags]);
  end;
end;

{
Фунция записи данных
}
function TICOPCServerNode.Write(aValues: TStringList): Boolean;
begin
  Result := False;
end;

{ Выбрать описания тегов из свойств }
function TICOPCServerNode.CreateTags(): TStrDictionary;
var
  i: Integer;
  key, value: AnsiString;
  tags: TStrDictionary;

begin
  tags := TStrDictionary.Create;
  for i := 0 to Properties.Count - 1 do
  begin
    key := Properties.GetKey(i);
    if not IsStrInList(key, RESERV_PROPERTIES) then
    begin
      value := Properties.GetStrValue(key);
      tags.AddStrValue(key, value);
    end;
  end;
  Result := tags;
end;

end.

