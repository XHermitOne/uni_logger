{
Модуль класса доступа к таблице PostgreSQL для записи значений тегов.

ВНИМАНИЕ! При описании полей использовать следующий формат:
          'ИМЯ_ИСТОЧНИКА_ДАННЫХ.имя_тега:Тип_поля_в_PostgreSQL'.
          Например: 'SOURCE_FIRST.tag_mode:Varchar(20)'

Версия: 0.0.5.1
}
unit postgresql_tab_wide;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    DateUtils,
    SQLdb, pqconnection,
    obj_proto, dictionary, strfunc, exttypes,
    tag_list;

const
  POSTGRESQL_TAB_WIDE_TYPE: AnsiString = 'POSTGRESQL_TAB_WIDE';

  RESERV_PROPERTIES: Array [1..9] Of String = ('type', 'name', 'description', 'db_name', 'db_port', 'db_username', 'db_password', 'table_name', 'fields');

  DATETIME_FIELD_NAME: AnsiString = 'dt_log';

  CREATE_TABLE_SQL_FMT: AnsiString = 'CREATE TABLE %s (id BIGSERIAL NOT NULL, %s, CONSTRAINT %s_pkey PRIMARY KEY (id));';
  INSERT_RECORD_SQL_FMT: AnsiString = 'INSERT INTO %s (%s) VALUES (%s);';
  UPDATE_RECORD_SQL_FMT: AnsiString = 'UPDATE %s SET (%s) = (%s) WHERE %s = %s;';

  // Добавление не существующей записи
  INSERT_NOT_EXISTS_RECORD_SQL_FMT: AnsiString = 'INSERT INTO %s (%s) SELECT %s WHERE NOT EXISTS (SELECT 1 FROM %s WHERE %s = %s);';
  UPSERT_RECORD_SQL_FMT: AnsiString = 'INSERT INTO %s (%s) VALUES (%s) ON CONFLICT (%s) DO UPDATE SET (%s) = (%s);';

  DB_DATETIME_FMT: AnsiString = 'yyyy-mm-dd hh:mm:ss';
  DB_DATE_SEPARATOR: Char = '-';


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

    { Получить SQL выражение пред-обработки }
    function GetPrevSQL(): AnsiString;
    { Получить SQL выражение пост-обработки }
    function GetPostSQL(): AnsiString;
    { Получить SQL выражение пред-обработки добавления данных }
    function GetPrevAddSQL(): AnsiString;
    { Получить SQL выражение пост-обработки добавления данных }
    function GetPostAddSQL(): AnsiString;

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
    destructor Destroy; override;

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

    {
    Запись всех внутренних данных
    @param dtTime: Время актуальности данных.
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function WriteAll(dtTime: TDateTime = 0): Boolean; override;

end;

implementation

uses
    LCLIntf, // Для вычисления времени выполнения
    log,
    memfunc,
    netfunc,
    engine;

constructor TICPostgreSQLTableWide.Create;
begin
  inherited Create;

  FSQLQuery := TSQLQuery.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(nil);
  FPQConnection := TPQConnection.Create(nil);
end;


destructor TICPostgreSQLTableWide.Destroy;
begin
  Disconnect();

  FSQLQuery.Destroy;
  FSQLTransaction.Destroy;
  FPQConnection.Destroy;

  // ВНИМАНИЕ! Нельзя использовать функции Free.
  // Если объект создается при помощи Create, то удаляться из
  // памяти должен с помощью Dуstroy
  // Тогда не происходит утечки памяти
  inherited Destroy;
end;

{ Настроить параметры соединения с БД }
function TICPostgreSQLTableWide.Connect(aDBHost: AnsiString; aDBPort: Integer;
  aDBName: AnsiString; aDBUserName: AnsiString; aDBPassword: AnsiString): Boolean;
begin
  {Проверка связи с сервером БД}
  if not netfunc.DoPing(aDBHost) then
  begin
    log.ErrorMsgFmt('Не доступен сервер БД PostgreSQL <%s>', [aDBHost]);
    Result := False;
    Exit;
  end;

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
    log.ErrorMsg('PostgreSQL connection string:');
    log.ErrorMsgFmt('    Host:    %s', [aDBHost]);
    log.ErrorMsgFmt('    Port:    %d', [aDBPort]);
    log.ErrorMsgFmt('    DatabaseName:    %s', [aDBName]);
    log.ErrorMsgFmt('    Username:    %s', [aDBUserName]);
    log.ErrorMsgFmt('    Password:    %s', [aDBPassword]);
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
    // FSQLQuery.Close;
    FSQLQuery.SQL.Clear;
    //FSQLQuery.Insert;
    FSQLQuery.SQL.Add(aSQL);
    // FSQLQuery.Open;
    FSQLQuery.ExecSQL;
    FSQLTransaction.Commit;
  except
    FSQLTransaction.Rollback;
    log.ErrorMsg('Ошибка выполнения SQL выражения');
    log.FatalMsgFmt('%s', [aSQL]);
    Result := False;
  end;
end;

{
Запись всех внутренних данных
@param dtTime: Время актуальности данных.
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICPostgreSQLTableWide.WriteAll(dtTime: TDateTime): Boolean;
var
  values: Array Of String;
  time_values: TMemRecordSet;
  fields: TStringList;
  prev_sql, post_sql: AnsiString;
begin
  log.DebugMsgFmt('Запись всех внутренних данных. Объект <%s>', [Name]);

  prev_sql := GetPrevSQL();
  post_sql := GetPostSQL();

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
    fields := strfunc.ParseStrList(Properties.GetStrValue('fields'));
    CreateTable(Properties.GetStrValue('table_name'), fields);
    fields.Destroy;
  end;

  // Пред-обработка
  if not strfunc.IsEmptyStr(prev_sql) then
  begin
    if ExecuteSQL(prev_sql) then
      log.InfoMsgFmt('Пред-обработка. SQL <%s>', [prev_sql])
    else
      log.InfoMsgFmt('Ошибка пред-обработки. SQL <%s>', [prev_sql])
  end;

  // Добавить запись
  values := ReadStateValues();
  if (Length(values) > 0) and (not values[0].IsEmpty) then
    Result := Result and InsertRecord(Properties.GetStrValue('table_name'), values, dtTime);
  // Добавить записи из временного буфера
  time_values := ReadTimeStateValues();
  if time_values.Count > 0 then
    Result := Result and InsertRecords(Properties.GetStrValue('table_name'), time_values);
  // После использования обязательно удаляем рекордсет
  time_values.Destroy;

  // Пост-обработка
  if not strfunc.IsEmptyStr(post_sql) then
  begin
    if ExecuteSQL(post_sql) then
      log.InfoMsgFmt('Пост-обработка. SQL <%s>', [post_sql])
    else
      log.InfoMsgFmt('Ошибка пост-обработки. SQL <%s>', [post_sql])
  end;

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
    sql_params := Format('%s timestamp DEFAULT NOW(), ', [DATETIME_FIELD_NAME]);
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
  bFreeFields: Boolean = False;
begin
  if aFields = nil then
  begin
    // Если не указано описание полей, то берем его из свойств
    aFields := strfunc.ParseStrList(Properties.GetStrValue('fields'));
    bFreeFields := True;
  end;

  SetLength(Result, aFields.Count);
  // Result[0] := DATETIME_FIELD_NAME;

  for i := 0 to aFields.Count - 1 do
    Result[i] := strfunc.SplitStr(strfunc.SplitStr(aFields[i], ':')[0], '.')[1];

  if bFreeFields then
    aFields.Destroy;
end;

{ Получить список типов полей из описания }
function TICPostgreSQLTableWide.GetFieldTypes(aFields: TStringList): TArrayOfString;
var
  i: Integer;
  bFreeFields: Boolean = False;
begin
  if aFields = nil then
  begin
    // Если не указано описание полей, то берем его из свойств
    aFields := strfunc.ParseStrList(Properties.GetStrValue('fields'));
    bFreeFields := True;
  end;

  SetLength(Result, aFields.Count);
  // Result[0] := 'timestamp';

  for i := 0 to aFields.Count-1 do
    Result[i] := strfunc.SplitStr(aFields[i], ':')[1];

  if bFreeFields then
    aFields.Destroy;
end;

{ Получить список Источник_данных.тег из описания }
function TICPostgreSQLTableWide.GetSrcTagList(aFields: TStringList): TStringList;
var
  i: Integer;
  bFreeFields: Boolean = False;
begin
  if aFields = nil then
  begin
    // Если не указано описание полей, то берем его из свойств
    aFields := strfunc.ParseStrList(Properties.GetStrValue('fields'));
    bFreeFields := True;
  end;

  Result := TStringList.Create;
  for i := 0 to aFields.Count - 1 do
    Result.Append(strfunc.SplitStr(aFields[i], ':')[0]);

  if bFreeFields then
    aFields.Destroy;
end;

{ Получить SQL выражение пред обработки }
function TICPostgreSQLTableWide.GetPrevSQL(): AnsiString;
begin
  Result := Properties.GetStrValue('prev_sql');
end;

{ Получить SQL выражение пост обработки }
function TICPostgreSQLTableWide.GetPostSQL(): AnsiString;
begin
  Result := Properties.GetStrValue('post_sql');
end;

{ Получить SQL выражение пред обработки добавления данных }
function TICPostgreSQLTableWide.GetPrevAddSQL(): AnsiString;
begin
  Result := Properties.GetStrValue('prev_add_sql');
end;

{ Получить SQL выражение пост обработки добавления данных }
function TICPostgreSQLTableWide.GetPostAddSQL(): AnsiString;
begin
  Result := Properties.GetStrValue('post_add_sql');
end;

{ Функция добавления записи }
function TICPostgreSQLTableWide.InsertRecord(aTableName: AnsiString; StringValues: Array Of String; dtTime: TDateTime): Boolean;
var
  field_names, param_names: AnsiString;
  sql, field_type: AnsiString;
  prev_sql, post_sql: AnsiString;
  field_name_list: Array Of String;
  field_type_list: Array Of String;
  i: Integer;
begin
  Result := False;
  try
    prev_sql := GetPrevAddSQL();
    post_sql := GetPostAddSQL();
    field_name_list := GetFieldNames();
    field_type_list := GetFieldTypes();

    field_names := strfunc.JoinStr(field_name_list, ', ');
    param_names := ':' + strfunc.JoinStr(field_name_list, ', :');
    if dtTime <> 0 then
      field_names := DATETIME_FIELD_NAME + ', ' + field_names;
    if dtTime <> 0 then
      param_names := ':' + DATETIME_FIELD_NAME + ', ' + param_names;

    log.DebugMsgFmt('Время: <%s>', [FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtTime)]);
    //log.DebugMsgFmt('Имена полей строкой <%s>', [field_names]);
    //log.DebugMsgFmt('Параметры строкой <%s>', [param_names]);

    // FSQLQuery.Database := FPQConnection;
    FSQLQuery.SQL.Clear;

    if not strfunc.IsEmptyStr(prev_sql) then
    begin
      FSQLQuery.SQL.Add(prev_sql);
      log.InfoMsgFmt('Пред-обработка добавления данных. SQL <%s>', [prev_sql]);
    end;

    // Добавить запись если не существует
    if dtTime <> 0 then
      sql := Format(INSERT_NOT_EXISTS_RECORD_SQL_FMT, [aTableName, field_names, param_names, aTableName, DATETIME_FIELD_NAME, ':' + DATETIME_FIELD_NAME])
    else
      sql := Format(INSERT_RECORD_SQL_FMT, [aTableName, field_names, param_names]);
    // sql := Format(UPSERT_RECORD_SQL_FMT, [aTableName, field_names, param_names, DATETIME_FIELD_NAME, field_names, param_names]);
    //log.DebugMsgFmt('Добавление записи. SQL <%s>', [sql]);

    FSQLQuery.SQL.Add(sql);

    // Обновить существующие записи
    sql := Format(UPDATE_RECORD_SQL_FMT, [aTableName, field_names, param_names, DATETIME_FIELD_NAME, ':' + DATETIME_FIELD_NAME]);
    FSQLQuery.SQL.Add(sql);

    if not strfunc.IsEmptyStr(post_sql) then
    begin
      FSQLQuery.SQL.Add(post_sql);
      log.InfoMsgFmt('Пост-обработка добавления данных. SQL <%s>', [post_sql]);
    end;

    if dtTime <> 0 then
      FSQLQuery.Params.ParamByName(DATETIME_FIELD_NAME).AsDateTime := dtTime;

    // Заполнение параметров
    for i := 0 to Length(StringValues) - 1 do
    begin
      field_type := LowerCase(field_type_list[i]);
      log.DebugMsgFmt('Параметр <%s> : <%s> : <%s>', [field_name_list[i], field_type, StringValues[i]]);

      if strfunc.IsStrInList(field_type, ['float']) then
        if StringValues[i] <> '' then
          FSQLQuery.Params.ParamByName(field_name_list[i]).AsFloat := StrToFloat(StringValues[i])
        else
          FSQLQuery.Params.ParamByName(field_name_list[i]).AsFloat := 0.0
      else if strfunc.IsStrInList(field_type, ['integer']) then
        if StringValues[i] <> '' then
          FSQLQuery.Params.ParamByName(field_name_list[i]).AsInteger := StrToInt(StringValues[i])
        else
          FSQLQuery.Params.ParamByName(field_name_list[i]).AsInteger := 0
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
    log.ErrorMsg('SQL:');
    log.ErrorMsgFmt('%s', [FSQLQuery.SQL.Text]);
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
begin
  log.DebugMsg('Добавление записей в БД');
  Result := False;

  if aRecordSet.Count > 0 then
  begin
    SetLength(rec, aRecordSet.Records[0].Count - 1);
    for i_rec := 0 to aRecordSet.Count - 1 do
    begin
      dt_time := DateUtils.ScanDateTime(DB_DATETIME_FMT, aRecordSet.Records[i_rec][0]);
      for i := 1 to aRecordSet.Records[i_rec].Count - 1 do
        rec[i - 1] := aRecordSet.Records[i_rec][i];
      Result := InsertRecord(aTableName, rec, dt_time) and Result;
    end;
  end;
end;

{ Прочитать все значения состояния из временных буферов источников }
function TICPostgreSQLTableWide.ReadTimeStateValues(aSrcTagList: TStringList): TMemRecordSet;
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

  // log.DebugMsg('Запуск чтения буфера состояний источников данных');
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
        //log.DebugMsgFmt('Обработка точки <%s : %s : %s>', [tag, dt_str, value]);
        // Поиск такой же временной метки
        is_find_dt := False;
        for i_rec := 0 to Result.Count - 1 do
          if Result.Records[i_rec][0] = dt_str then
          begin
            // Нашли временную метку. Записываем
            Result.Records[i_rec][i + 1] := value;
            is_find_dt := True;
            break;
          end;

        if not is_find_dt then
        begin
          // Не нашли такую временную метку. Необходимо добавить строку
          new_record := TMemRecord.Create;
          new_record.SetLength(aSrcTagList.Count + 1);
          Result.Add(new_record);
          // Ставим временную метку
          i_rec := Result.Count - 1;
          Result.Records[i_rec][0] := dt_str;
          Result.Records[i_rec][i+1] := value;
        end;

      end;
      // Обязательно очищем память после использования векторов
      values.Destroy;
    end;
  except
    log.FatalMsg('Ошибка чтения значений временного буфера состояний источников данных');
  end;

  if free_tag_list then
    aSrcTagList.Free;
end;

end.

