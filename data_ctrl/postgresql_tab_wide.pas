{
Модуль класса доступа к таблице PostgreSQL для записи значений тегов.

ВНИМАНИЕ! При описании полей использовать следующий формат:
          'ИМЯ_ИСТОЧНИКА_ДАННЫХ.имя_тега:Тип_поля_в_PostgreSQL'.
          Например: 'SOURCE_FIRST.tag_mode:Varchar(20)'
Версия: 0.0.1.2
}
unit postgresql_tab_wide;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    SQLdb, pqconnection,
    obj_proto, dictionary, strfunc,
    tag_list;

const
  POSTGRESQL_TAB_WIDE_TYPE: AnsiString = 'POSTGRESQL_TAB_WIDE';

  RESERV_PROPERTIES: Array [1..9] Of String = ('type', 'name', 'description', 'db_name', 'db_port', 'db_username', 'db_password', 'table_name', 'fields');

  DATETIME_FIELD_NAME: AnsiString = 'dt_log';

type
  {
  Класс доступа к таблице PostgreSQL для записи значений тегов.
  }
  TICPostgreSQLTableWide = class(TICObjectProto)

  private
    FSQLQuery:       TSQLQuery;                     { коммандный процессор SQL }
    FSQLTransaction: TSQLTransaction;               { координатор транзакций }
    FPQConnection:   TPQConnection;                 { интерфейс PostgreSQL сервера }

    {
    Настроить параметры соединения с БД
    @param aDBHost Сервер БД
    @param aDBPort Порт
    @param aDBName Имя БД
    @param aDBUserName Имя пользователя БД
    @param aDBPassword Пароль пользователя БД
    @return True - успешно / False - ошибка
    }
    function Connect(aDBHost: AnsiString; aDBPort: Integer; aDBName: AnsiString; aDBUserName: AnsiString; aDBPassword: AnsiString): Boolean;
    {
    Закрыть соединение
    }
    function Disconnect(): Boolean;
    {
    Выполнить SQL выражение
    @param aSQL Текст SQL выражения
    @return True - успешно / False - ошибка
    }
    function ExecuteSQL(aSQL: AnsiString): Boolean;

    { Проверка на существование таблицы с указанными именем в БД }
    function ExistsTable(aTableName: AnsiString): Boolean;

    { Создать таблицу в БД с указанными именем и заданной структуры }
    function CreateTable(aTableName: AnsiString; aFields: TStringList): Boolean;

  public
    constructor Create;
    procedure Free;

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

    { Фунция чтения данных }
    function Read(aValues: TStringList): TStringList; override;
    { Фунция записи данных }
    function Write(aValues: TStringList): Boolean; override;

    { Запись всех внутренних данных }
    function WriteAll(): Boolean; override;

end;

implementation

uses
    LCLIntf, // Для вычисления времени выполнения
    log;

constructor TICPostgreSQLTableWide.Create;
begin
  FSQLQuery := TSQLQuery.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(nil);
  FPQConnection := TPQConnection.Create(nil);
  inherited Create;
end;

procedure TICPostgreSQLTableWide.Free;
begin
  FPQConnection.Close;
  FSQLQuery.Free;
  FSQLTransaction.Free;
  FPQConnection.Free;

  inherited Free;
end;

{ Настроить параметры соединения с БД }
function TICPostgreSQLTableWide.Connect(aDBHost: AnsiString; aDBPort: Integer;
  aDBName: AnsiString; aDBUserName: AnsiString; aDBPassword: AnsiString): Boolean;
begin
  {подключение к SQL}
  try
    FPQConnection.HostName := aDBHost;
    FPQConnection.DatabaseName := aDBName;
    FPQConnection.UserName := aDBUserName;
    FPQConnection.Password := aDBPassword;
    FPQConnection.Open;

    FSQLQuery.DataBase := FPQConnection;
    FSQLQuery.Transaction := FSQLTransaction;
    FSQLTransaction.DataBase := FPQConnection;
    FPQConnection.Transaction := FSQLTransaction;

    Result := FPQConnection.Connected;

    if not Result then
      log.WarningMsgFmt('Ошибка соединения с БД PostgreSQL <%s : %s : %s>', [aDBHost, aDBName, aDBUserName])
    else
      log.InfoMsgFmt('Соединение с БД PostgreSQL <%s : %s : %s>', [aDBHost, aDBName, aDBUserName]);
  except
    log.FatalMsg('Ошибка настройки параметров подключения к БД PostgreSQL');
    Result := False
  end;
end;

{
Закрыть соединение
}
function TICPostgreSQLTableWide.Disconnect(): Boolean;
begin
  try
    FPQConnection.Close;
    Result := not FPQConnection.Connected;
    if not Result then
      log.WarningMsgFmt('Ошибка разрыва соединения с БД PostgreSQL <%s : %s : %s>', [FPQConnection.HostName,
                                                                                     FPQConnection.DatabaseName,
                                                                                     FPQConnection.UserName])
    else
      log.InfoMsgFmt('Разрыв соединения с БД PostgreSQL <%s : %s : %s>', [FPQConnection.HostName,
                                                                          FPQConnection.DatabaseName,
                                                                          FPQConnection.UserName]);
  except
    log.FatalMsg('Ошибка закрытия соединения с БД PostgreSQL');
    Result := False
  end;
end;

{ Выполнить SQL выражение }
function TICPostgreSQLTableWide.ExecuteSQL(aSQL: AnsiString): Boolean;
begin
  Result := True;
  try
    FPQConnection.Connected := True;
  except
    log.FatalMsg('Ошибка соединения с сервером PostgreSQL');
    Result := False;
  end;

  try
    FSQLTransaction.Active := True;
  except
    log.FatalMsg('Ошибка открытия транзакции');
    Result := False;
  end;

  try
    FSQLQuery.Close;
    FSQLQuery.SQL.Clear;
    //FSQLQuery.Insert;
    FSQLQuery.SQL.Add(aSQL);
    FSQLQuery.Open;
  except
    log.FatalMsg('Ошибка выполнения SQL выражения');
    Result := False;
  end;
end;

{
Фунция чтения данных
}
function TICPostgreSQLTableWide.Read(aValues: TStringList): TStringList;
begin
  Result := nil;
end;

{
Фунция записи данных
}
function TICPostgreSQLTableWide.Write(aValues: TStringList): Boolean;
begin
  Result := False;
end;

{ Запись всех внутренних данных }
function TICPostgreSQLTableWide.WriteAll(): Boolean;
begin
  Result := True;
  properties := GetProperties();
  // Соединяемся с БД
  if not Connect(properties.GetStrValue('db_host'),
                 StrToInt(properties.GetStrValue('db_port')),
                 properties.GetStrValue('db_name'),
                 properties.GetStrValue('db_username'),
                 properties.GetStrValue('db_password')) then
  begin
    Result := False;
    Exit;
  end;

  // Если таблицы не существует, то создать ее
  if not ExistsTable(properties.GetStrValue('table_name')) then
  begin

  end;

  // Закрываем соединение
  if not Disconnect() then
  begin
    Result := False;
    Exit;
  end;
end;

{ Выбрать описания тегов из свойств }
function TICPostgreSQLTableWide.CreateTags(): TStrDictionary;
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

{ Проверка на существование таблицы с указанными именем в БД }
function TICPostgreSQLTableWide.ExistsTable(aTableName: AnsiString): Boolean;
var
  table_names: TStringList;
begin
  Result := False;
  table_names := TStringList.Create;
  try
    FPQConnection.GetTableNames(table_names, False);
    Result := table_names.IndexOf(aTableName) >= 0;
  finally
    table_names.Free;
  end;
end;

{ Создать таблицу в БД с указанными именем и заданной структуры }
function TICPostgreSQLTableWide.CreateTable(aTableName: AnsiString; aFields: TStringList): Boolean;
begin

end;

end.

