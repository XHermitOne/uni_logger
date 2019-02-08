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
    obj_proto, dictionary, strfunc, exttypes,
    tag_list;

const
  POSTGRESQL_TAB_WIDE_TYPE: AnsiString = 'POSTGRESQL_TAB_WIDE';

  RESERV_PROPERTIES: Array [1..9] Of String = ('type', 'name', 'description', 'db_name', 'db_port', 'db_username', 'db_password', 'table_name', 'fields');

  DATETIME_FIELD_NAME: AnsiString = 'dt_log';

  CREATE_TABLE_SQL_FMT: AnsiString = 'CREATE TABLE %s (id BIGSERIAL NOT NULL, %s, CONSTRAINT %s_pkey PRIMARY KEY (id));';
  INSERT_RECORD_SQL_FMT: AnsiString = 'INSERT INTO %s (%s) VALUES (%s)';

  DB_DATETIME_FMT: AnsiString = 'yyyy-mm-dd hh:mm:ss';


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

    { Получить список имен полей из описания }
    function GetFieldNames(aFields: TStringList = nil): TArrayOfString;

    { Получить список типов полей из описания }
    function GetFieldTypes(aFields: TStringList = nil): TArrayOfString;

    { Получить список Источник_данных.тег из описания }
    function GetSrcTagList(aFields: TStringList = nil): TStringList;

    { Функция добавления записи }
    function InsertRecord(aTableName: AnsiString; StringValues: Array Of String;  dtTime: TDateTime=0): Boolean;

    { Прочитать все значения состояния из источников }
    function ReadStateValues(aSrcTagList: TStringList = nil): TArrayOfString;

    { Функция добавления записей }
    function InsertRecords(aTableName: AnsiString; aRecordSet: TMemRecordSet): Boolean;

    { Прочитать все значения состояния из временных буферов источников }
    function ReadTimeStateValues(aSrcTagList: TStringList = nil): TMemRecordSet;

  public
    constructor Create;
    procedure Free;

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

    {
    Запись всех внутренних данных
    @param aTime: Время актуальности данных.
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function WriteAll(aTime: TDateTime = 0): Boolean; override;

end;

implementation

uses
    LCLIntf, // Для вычисления времени выполнения
    log,
    engine;

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
    Exit;
  end;

  try
    FSQLTransaction.Active := True;
  except
    log.FatalMsg('Ошибка открытия транзакции');
    Result := False;
    Exit;
  end;

  try
    FSQLQuery.Close;
    FSQLQuery.SQL.Clear;
    //FSQLQuery.Insert;
    FSQLQuery.SQL.Add(aSQL);
    FSQLQuery.Open;
  except
    log.ErrorMsg('Ошибка выполнения SQL выражения');
    log.FatalMsgFmt('%s', [aSQL]);
    Result := False;
  end;
end;

{
Запись всех внутренних данных
@param aTime: Время актуальности данных.
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICPostgreSQLTableWide.WriteAll(aTime: TDateTime): Boolean;
var
  values: Array Of String;
  time_values: TMemRecordSet;

begin
  log.DebugMsgFmt('Запись всех внутренних данных. Объект <%s>', [Name]);

  Result := True;
  // properties := GetProperties();
  // Соединяемся с БД
  if not Connect(Properties.GetStrValue('db_host'),
                 StrToInt(properties.GetStrValue('db_port')),
                 Properties.GetStrValue('db_name'),
                 Properties.GetStrValue('db_username'),
                 Properties.GetStrValue('db_password')) then
  begin
    log.WarningMsg('Ошибка соединения с БД');
    Result := False;
    Exit;
  end;

  // Если таблицы не существует, то создать ее
  if not ExistsTable(Properties.GetStrValue('table_name')) then
  begin
    CreateTable(Properties.GetStrValue('table_name'),
                strfunc.ParseStrList(Properties.GetStrValue('fields')));
  end;

  // Добавить запись
  values := ReadStateValues();
  if Length(values) > 0 then
    Result := Result and InsertRecord(Properties.GetStrValue('table_name'), values);
  // Добавить записи из временного буфера
  time_values := ReadTimeStateValues();
  if time_values.Count > 0 then
    Result := Result and InsertRecords(Properties.GetStrValue('table_name'), time_values);

  // Закрываем соединение
  if not Disconnect() then
  begin
    log.WarningMsg('Ошибка закрытия соединения с БД');
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
  // log.DebugMsgFmt('Проверка наличия таблицы <%s>', [aTableName]);

  Result := False;
  table_names := TStringList.Create;
  try
    FPQConnection.GetTableNames(table_names, False);
    Result := table_names.IndexOf(aTableName) >= 0;
  except
    log.FatalMsgFmt('Ошибка проверки существования таблицы <%s>', [aTableName]);
  end;
  table_names.Free;
end;

{ Создать таблицу в БД с указанными именем и заданной структуры }
function TICPostgreSQLTableWide.CreateTable(aTableName: AnsiString; aFields: TStringList): Boolean;
var
  sql: AnsiString;
  sql_params, field_name, field_type: AnsiString;
  i: Integer;
begin
  log.DebugMsgFmt('Создание таблицы <%s>', [aTableName]);

  Result := False;
  // Формирование строки параметров SQL выражения создания таблицы
  try
    sql_params := Format('%s timestamp WITH TIME ZONE DEFAULT NOW(), ', [DATETIME_FIELD_NAME]);
    for i := 0 to aFields.Count - 1 do
    begin
      field_name := strfunc.SplitStr(strfunc.SplitStr(aFields[i], ':')[0], '.')[1];
      field_type := strfunc.SplitStr(aFields[i], ':')[1];
      log.DebugMsgFmt('Описание поля <%s> : <%s>', [field_name, field_type]);
      sql_params := sql_params + Format('%s %s', [field_name, field_type]);
      if i < (aFields.Count - 1) then
        sql_params := sql_params + ', ';
    end;

  except
    log.FatalMsgFmt('Ошибка парсинга описания полей таблицы <%s>', [aTableName]);
  end;

  sql := Format(CREATE_TABLE_SQL_FMT, [aTableName, sql_params, aTableName]);
  try
    FPQConnection.ExecuteDirect(sql);
    FSQLTransaction.Commit;
    Result := True;
  except
    FSQLTransaction.Rollback;
    log.ErrorMsg('Ошибка выполнения SQL выражения:');
    log.FatalMsgFmt('%s', [sql]);
  end;
end;

{ Получить список имен полей из описания }
function TICPostgreSQLTableWide.GetFieldNames(aFields: TStringList): TArrayOfString;
var
  i: Integer;
begin
  if aFields = nil then
    // Если не указано описание полей, то берем его из свойств
    aFields := strfunc.ParseStrList(Properties.GetStrValue('fields'));

  SetLength(Result, aFields.Count);
  // Result[0] := DATETIME_FIELD_NAME;

  for i := 0 to aFields.Count - 1 do
    Result[i] := strfunc.SplitStr(strfunc.SplitStr(aFields[i], ':')[0], '.')[1];
end;

{ Получить список типов полей из описания }
function TICPostgreSQLTableWide.GetFieldTypes(aFields: TStringList): TArrayOfString;
var
  i: Integer;
begin
  if aFields = nil then
    // Если не указано описание полей, то берем его из свойств
    aFields := strfunc.ParseStrList(Properties.GetStrValue('fields'));

  SetLength(Result, aFields.Count);
  // Result[0] := 'timestamp';

  for i := 0 to aFields.Count-1 do
    Result[i] := strfunc.SplitStr(aFields[i], ':')[1];
end;

{ Получить список Источник_данных.тег из описания }
function TICPostgreSQLTableWide.GetSrcTagList(aFields: TStringList): TStringList;
var
  i: Integer;
begin
  if aFields = nil then
    // Если не указано описание полей, то берем его из свойств
    aFields := strfunc.ParseStrList(Properties.GetStrValue('fields'));

  Result := TStringList.Create;
  for i := 0 to aFields.Count - 1 do
    Result.Append(strfunc.SplitStr(aFields[i], ':')[0]);
end;

{ Функция добавления записи }
function TICPostgreSQLTableWide.InsertRecord(aTableName: AnsiString; StringValues: Array Of String; dtTime: TDateTime): Boolean;
var
  field_names, param_names: AnsiString;
  sql, field_type: AnsiString;
  field_name_list: Array Of String;
  field_type_list: Array Of String;
  i: Integer;
begin
  Result := False;
  try
    field_name_list := GetFieldNames();
    field_type_list := GetFieldTypes();

    field_names := strfunc.JoinStr(field_name_list, ', ');
    // log.DebugMsgFmt('Имена полей строкой <%s>', [field_names]);
    param_names := ':' + strfunc.JoinStr(field_name_list, ', :');
    // log.DebugMsgFmt('Параметры строкой <%s>', [param_names]);
    sql := Format(INSERT_RECORD_SQL_FMT, [aTableName, field_names, param_names]);
    //log.DebugMsgFmt('Добавление записи. SQL <%s>', [sql]);

    // FSQLQuery.Database := FPQConnection;
    FSQLQuery.SQL.Clear;
    FSQLQuery.SQL.Add(sql);

    // Заполнение параметров
    for i := 0 to Length(StringValues) - 1 do
    begin
      field_type := LowerCase(field_type_list[i]);
      //log.DebugMsgFmt('Параметр <%s> : <%s>', [field_name_list[i], field_type]);

      if strfunc.IsStrInList(field_type, ['float']) then
        FSQLQuery.Params.ParamByName(field_name_list[i]).AsFloat := StrToFloat(StringValues[i])
      else if strfunc.IsStrInList(field_type, ['integer']) then
        FSQLQuery.Params.ParamByName(field_name_list[i]).AsInteger := StrToInt(StringValues[i])
      else if strfunc.AnyWordInStr(['text', 'string', 'varchar'], field_type) then
        FSQLQuery.Params.ParamByName(field_name_list[i]).AsString := StringValues[i]
      else
      begin
        log.WarningMsgFmt('Тип поля <%s> не поддерживается', [field_type_list[i]]);
      end;
    end;
    FSQLQuery.ExecSQL;  //  record inserted
    FSQLTransaction.Commit;
    Result := True;
  except
    FSQLTransaction.Rollback;
    log.FatalMsg('Ошибка добавления записи');
  end;
end ;

{ Прочитать все значения состояния из источников }
function TICPostgreSQLTableWide.ReadStateValues(aSrcTagList: TStringList): TArrayOfString;
var
  src_name, tag, value: AnsiString;
  i: Integer;
  parent_engine: TICLogger;
  free_tag_list: Boolean;
begin
  free_tag_list := False;
  if aSrcTagList = nil then
  begin
    aSrcTagList := GetSrcTagList();
    free_tag_list := True;
  end;

  //log.DebugMsgFmt('Запуск чтения состояний источников данных. Количество <%d>', [aSrcTagList.Count]);

  SetLength(Result, aSrcTagList.Count);

  try
    for i := 0 to aSrcTagList.Count - 1 do
    begin
      src_name := strfunc.SplitStr(aSrcTagList[i], '.')[0];
      tag := strfunc.SplitStr(aSrcTagList[i], '.')[1];
      // Родительский менеджер обработки
      parent_engine := GetParent() As TICLogger;
      value := parent_engine.GetSourceStateAsString(src_name, tag);
      //log.DebugMsgFmt('Чтение состояния источника данных <%s>. Тег <%s>. Значение <%s>', [src_name, tag, value]);
      Result[i] := value;
    end;
  except
    log.FatalMsg('Ошибка чтения значений состояний источников данных');
  end;

  if free_tag_list then
    aSrcTagList.Free;
end;

{ Функция добавления записей }
function TICPostgreSQLTableWide.InsertRecords(aTableName: AnsiString; aRecordSet: TMemRecordSet): Boolean;
var
  i, i_rec: Integer;
  rec: Array Of String;
  dt_time: TDateTime;
  dt_format : TFormatSettings;
begin
  log.DebugMsg('Добавление записей в БД');
  Result := False;
  dt_format.ShortDateFormat := DB_DATETIME_FMT;

  if aRecordSet.Count > 0 then
  begin
    SetLength(rec, aRecordSet.Records[0].Count - 1);
    for i_rec := 0 to aRecordSet.Count - 1 do
    begin
      dt_time := StrToDateTime(aRecordSet.Records[i_rec][0], dt_format);
      for i := 1 to aRecordSet.Records[i_rec].Count - 1 do
        rec[i - 1] := aRecordSet.Records[i_rec][i];
      Result := Result and InsertRecord(aTableName, rec, dt_time);
    end;
  end;
end;

{ Прочитать все значения состояния из временных буферов источников }
function TICPostgreSQLTableWide.ReadTimeStateValues(aSrcTagList: TStringList = nil): TMemRecordSet;
var
  src_name, tag, value, dt_str: AnsiString;
  values: TMemVectorOfString;
  i, i_value, i_rec: Integer;
  parent_engine: TICLogger;
  free_tag_list: Boolean;
  is_find_dt: Boolean;
  new_record: TMemRecord;
begin
  free_tag_list := False;
  if aSrcTagList = nil then
  begin
    aSrcTagList := GetSrcTagList();
    free_tag_list := True;
  end;

  log.DebugMsg('Запуск чтения буфера состояний источников данных');
  Result := TMemRecordSet.Create;

  try
    for i := 0 to aSrcTagList.Count - 1 do
    begin
      src_name := strfunc.SplitStr(aSrcTagList[i], '.')[0];
      tag := strfunc.SplitStr(aSrcTagList[i], '.')[1];
      // Родительский менеджер обработки
      parent_engine := GetParent() As TICLogger;
      values := parent_engine.GetSourceTimeStateAsList(src_name, tag);
      for i_value := 0 to values.Count - 1 do
      begin
        dt_str := values.Points[i_value].datetime;
        value := values.Points[i_value].value;
        // Поиск такой же временной метки
        is_find_dt := False;
        for i_rec := 0 to Result.Count - 1 do
          if Result.Records[i_rec][0] = dt_str then
          begin
            // Нашли временную метку. Записываем
            Result.Records[i + 1][i_rec] := value;
            is_find_dt := True;
            break;
          end;

        if not is_find_dt then
        begin
          // Не нашли такую временную метку. Необходимо добавить строку
          new_record := TMemRecord.Create;
          Result.Add(Addr(new_record));
          // Ставим временную метку
          i_rec := Result.Count - 1;
          Result.Records[i_rec][0] := dt_str;
          Result.Records[i_rec][i_rec] := value;
        end;

      end;
    end;
  except
    log.FatalMsg('Ошибка чтения значений временного буфера состояний источников данных');
  end;

  if free_tag_list then
    aSrcTagList.Free;
end;

end.

