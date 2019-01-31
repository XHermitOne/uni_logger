{
Модуль класса доступа к таблице PostgreSQL для записи значений тегов.
}
unit postgresql_tab_wide;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    obj_proto, dictionary, strfunc,
    tag_list;

const
  RESERV_PROPERTIES: Array [1..8] Of String = ('type', 'name', 'description', 'db_name', 'db_port', 'db_username', 'db_password', 'table_name');

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
    @param aDBName Имя БД
    @param aDBUserName Имя пользователя БД
    @param aDBPassword Пароль пользователя БД
    @return True - успешно / False - ошибка
    }
    function Connect(aDBHost: AnsiString; aDBName: AnsiString; aDBUserName: AnsiString; aDBPassword: AnsiString): Boolean;
    {
    Выполнить SQL выражение
    @param aSQL Текст SQL выражения
    @return True - успешно / False - ошибка
    }
    function ExecuteSQL(aSQL: AnsiString): Boolean;

  public
    constructor Create;
    procedure Free;

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

    { Фунция чтения данных }
    function Read(aValues: TStringList): TStringList; override;
    { Фунция записи данных }
    function Write(aValues: TStringList): Boolean; override;


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
function TICPostgreSQLTableWide.Connect(aDBHost: AnsiString; aDBName: AnsiString; aDBUserName: AnsiString; aDBPassword: AnsiString): Boolean;
begin
  Result := True;
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
  except
    log.FatalMsg('Ошибка настройки параметров подключения к БД PostgreSQL');
    Result := False
  end;
end;

{ Выполнить SQL выражение }
function TICPostgreSQLTableWide.ExecuteSQL(aSQL: AnsiString): Boolean;
begin
  Result := True;
  try:
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

end.

