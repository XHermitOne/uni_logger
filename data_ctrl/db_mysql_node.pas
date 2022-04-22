{
Модуль узла MySQL БД сервера.

Версия: 0.0.2.2
}

unit db_mysql_node;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
  // Windows, ActiveX, ComObj,
  {$ENDIF}
  Classes, SysUtils, DateUtils, Variants, VarUtils,
  // OPCHDA, OPCtypes, OPCError,
  SQLdb, mysql55conn,
  obj_proto, dictionary, strfunc, dtfunc, exttypes;

const
  DB_MYSQL_NODE_TYPE: AnsiString = 'DB_MYSQL';

  RESERV_PROPERTIES: Array [1..11] Of String = ('type', 'name', 'description', 'db_host', 'db_port', 'db_name', 'db_username', 'db_password',
                                               'sql_fmt', 'dt_field_name', 'dt_frame_tick');

  DEFAULT_COMPUTER_NAME: AnsiString = 'localhost';

  // Имя поля даты-времени по умолчанию
  DEFAULT_DATETIME_FIELD_NAME: AnsiString = 'dt';

  DB_DATETIME_FMT: AnsiString = 'yyyy-mm-dd hh:mm:ss';
  DB_DATE_SEPARATOR: Char = '-';

type
  {
  Класс взаимодействия с БД MySQL сервером.
  }
  TICMySQLDBNode = class(TICObjectProto)

  private
    FSQLQuery:       TSQLQuery;                     { коммандный процессор SQL }
    FSQLTransaction: TSQLTransaction;               { координатор транзакций }
    FDBConnection:   TMySQL55Connection;            { интерфейс MySQL сервера }

    {
    Формат SQL выражения для получения данных.
    }
    FSQLFmt: AnsiString;
    {
    Это шаг регистрации временных данных в контроллере.
    Задется в настройках в формате <yyyy-mm-dd hh:nn:ss>
    }
    FDTFrameTick: dtfunc.TDateTimeDelta;

    {
    Имя поля времени.
    }
    FDTFieldName: AnsiString;

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

    {
    Вычислить начальное время запрашиваемого диапазона от указанного
    @param dtEnd Конечная дата-время вычисляемого диапазона.
                  Если не определена, то берется текущая системная.
    @param dtTick Временной шаг
    @param iCount: Количество шагов
    @param bNotMonth: С точностью до месяца?
    @param bNotDay: С точностью до дня?
    @param bNotHour: С точностью до часа?
    @param bNotMinute: С точностью до минут?
    @param bNotSecond: С точностью до секунд?
    @return Вычисленное временное значение начала диапазона
    }
    function CalcStartDateTime(dtEnd: TDateTime=0; dtTick: dtfunc.TDateTimeDelta=nil; iCount: Integer=0;
                               bNotMonth: Boolean=True; bNotDay: Boolean=True; bNotHour: Boolean=True; bNotMinute: Boolean=True; bNotSecond: Boolean=True): TDateTime;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Free;

    { Выбрать описания тегов из свойств }
    function CreateTags(bClearValue: Boolean = False): TStrDictionary;

    { Установить свойства в виде списка параметров }
    procedure SetPropertiesArray(aArgs: Array Of Const); override;

    { Установить свойства объекта в виде словаря }
    procedure SetProperties(dProperties: TStrDictionary); override;

    {
    Чтение всех внутренних данных, описанных в свойствах.
    @param dtTime: Время актуальности за которое необходимо получить данные.
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAll(dtTime: TDateTime = 0): TStringList; override;

  published
    property DTFrameTick: dtfunc.TDateTimeDelta read FDTFrameTick write FDTFrameTick;
    property DTFieldName: AnsiString read FDTFieldName write FDTFieldName;
    property SQLFmt: AnsiString read FSQLFmt write FSQLFmt;

end;


implementation

uses
  LCLIntf, // Для вычисления времени выполнения
  log,
  filefunc,
  netfunc,
  memfunc;


constructor TICMySQLDBNode.Create;
begin
  inherited Create;

  FDTFrameTick := dtfunc.TDateTimeDelta.Create;

  FSQLQuery := TSQLQuery.Create(nil);
  // Правильно использовать:
  //fSQLQuery.Open;
  //while not fSQLQuery.EOF do begin
  //  ...
  //  fSQLQuery.Next;
  //end;
  FSQLQuery.PacketRecords := 1000;
  FSQLTransaction := TSQLTransaction.Create(nil);
  FDBConnection := TMySQL55Connection.Create(nil);
end;

destructor TICMySQLDBNode.Destroy;
begin
  Free;

  inherited Destroy;

  FDTFrameTick.Destroy;
end;


procedure TICMySQLDBNode.Free;
begin
  Disconnect();

  FSQLQuery.Free;
  FSQLTransaction.Free;
  FDBConnection.Free;

  // inherited Free;
end;


{ Настроить параметры соединения с БД }
function TICMySQLDBNode.Connect(aDBHost: AnsiString; aDBPort: Integer;
                                aDBName: AnsiString; aDBUserName: AnsiString; aDBPassword: AnsiString): Boolean;
begin
  {Проверка связи с сервером БД}
  if not netfunc.DoPing(aDBHost) then
  begin
    log.ErrorMsgFmt('Не доступен сервер БД MySQL <%s>', [aDBHost]);
    Result := False;
    Exit;
  end;

  {подключение к SQL}
  try
    FDBConnection.HostName := aDBHost;
    FDBConnection.DatabaseName := aDBName;
    FDBConnection.UserName := aDBUserName;
    FDBConnection.Password := aDBPassword;
    FDBConnection.Open;

    FSQLQuery.DataBase := FDBConnection;
    FSQLQuery.Transaction := FSQLTransaction;
    FSQLTransaction.DataBase := FDBConnection;
    FDBConnection.Transaction := FSQLTransaction;

    Result := FDBConnection.Connected;

    if not Result then
      log.WarningMsgFmt('Ошибка соединения с БД MySQL <%s : %s : %s>', [aDBHost, aDBName, aDBUserName])
    else
      log.InfoMsgFmt('Соединение с БД MySQL <%s : %s : %s>', [aDBHost, aDBName, aDBUserName]);
  except
    log.ErrorMsg('MySQL connection string:');
    log.ErrorMsgFmt('    Host:    %s', [aDBHost]);
    log.ErrorMsgFmt('    Port:    %d', [aDBPort]);
    log.ErrorMsgFmt('    DatabaseName:    %s', [aDBName]);
    log.ErrorMsgFmt('    Username:    %s', [aDBUserName]);
    log.ErrorMsgFmt('    Password:    %s', [aDBPassword]);
    log.FatalMsg('Ошибка настройки параметров подключения к БД MySQL');
    Result := False
  end;
end;

{
Закрыть соединение
}
function TICMySQLDBNode.Disconnect(): Boolean;
begin
  try
    FDBConnection.Close;
    Result := not FDBConnection.Connected;
    if not Result then
      log.WarningMsgFmt('Ошибка разрыва соединения с БД MySQL <%s : %s : %s>', [FDBConnection.HostName,
                                                                                FDBConnection.DatabaseName,
                                                                                FDBConnection.UserName])
    else
      log.InfoMsgFmt('Разрыв соединения с БД MySQL <%s : %s : %s>', [FDBConnection.HostName,
                                                                          FDBConnection.DatabaseName,
                                                                          FDBConnection.UserName]);
  except
    log.FatalMsg('Ошибка закрытия соединения с БД MySQL');
    Result := False
  end;
end;

{ Выполнить SQL выражение }
function TICMySQLDBNode.ExecuteSQL(aSQL: AnsiString): Boolean;
begin
  Result := True;
  try
    FDBConnection.Connected := True;
  except
    log.FatalMsg('Ошибка соединения с сервером MySQL');
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
    FSQLQuery.SQL.Add(aSQL);
    FSQLQuery.Open;
  except
    log.ErrorMsg('Ошибка выполнения SQL выражения');
    log.FatalMsgFmt('%s', [aSQL]);
    Result := False;
  end;
end;

{
Установить свойства в виде списка параметров
}
procedure TICMySQLDBNode.SetPropertiesArray(aArgs: Array Of Const);
begin
  if Length(aArgs) >= 1 then
  begin
    try
      { Первый элемент - это имя OPC сервера }
      { ВНИМАНИЕ! Преобразование элемента массива параметров в строку:
                  AnsiString(item.vAnsiString) }
      // SetOPCServerName(AnsiString(aArgs[0].vAnsiString));

    except
      log.FatalMsgFmt('Ошибка установки массива спойств в <%s>', [ClassName]);
    end;
  end;
end;

{
Чтение всех внутренних данных, описанных в свойствах.
@param dtTime: Время актуальности за которое необходимо получить данные.
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICMySQLDBNode.ReadAll(dtTime: TDateTime = 0): TStringList;
var
  mStartTime: TDateTime;

  i, i_tag: Integer;
  tags: TStrDictionary;
  tag_name, address, value, dt_str: AnsiString;
  dt_time: TDateTime;
  new_state: TStrDictionary;
  cur_month, cur_day, cur_hour, cur_minute, cur_sec: Word;
  sql: AnsiString;
  str_start_dt, str_stop_dt: AnsiString;
  rec_count: Integer;

begin
  if dtTime = 0 then
    dtTime := Now();

  // Вычисление начального времени по базовому
  mStartTime := CalcStartDateTime(dtTime, nil, 1);
  // Строковое представление временного диапазона для SQL выражения
  str_start_dt := FormatDateTime(obj_proto.DATETIME_TXT_FMT, mStartTime);
  str_stop_dt := FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtTime);
  log.DebugMsgFmt('Запрашиваемый диапазон. <%s> - <%s>', [str_start_dt, str_stop_dt]);

  // Соединение с БД
  if not Connect(Properties.GetStrValue('db_host'),
                 StrToInt(properties.GetStrValue('db_port')),
                 Properties.GetStrValue('db_name'),
                 Properties.GetStrValue('db_username'),
                 Properties.GetStrValue('db_password')) then
  begin
    log.WarningMsg('Ошибка соединения с БД MySQL');
    Result := nil;
    Exit;
  end;

  // Список читаемых тегов
  tags := CreateTags();
  //log.DebugMsgFmt('Читаемых тегов <%d>', [tags.Count]);

  // Перед началом чтения необходимо очистить буфер
  ClearTimeState();

  Result := TStringList.Create;

  // Получить SQL выражение
  sql := Format(SQLFmt, [str_start_dt, str_stop_dt]);
  log.DebugMsgFmt('MySQL get DATA SQL: %s', [sql]);
  ExecuteSQL(sql);
  // Определить количество записей результата запроса
  rec_count := FSQLQuery.RecordCount;
  log.DebugMsgFmt('Количество записей [%d]', [rec_count]);

  try
    // Перебор по тегам
    for i_tag := 0 to tags.Count - 1 do
    begin
      tag_name := tags.GetKey(i_tag);
      address := tags.GetStrValue(tag_name);
      // log.DebugMsgFmt('Чтение данных тега <%s> по адресу <%s>', [tag_name, address]);

      // Переход на первую строку
      FSQLQuery.First;

      // Перебор по записям запроса
      for i := 0 to rec_count - 1 do
      begin
        // Получение значений тегов из результата запроса
        value := FSQLQuery.FieldByName(address).AsString;
        // Получить время из записи
        dt_time := FSQLQuery.FieldByName(DTFieldName).AsDateTime;
        // Время в строковом представлении
        dt_str := FormatDateTime(obj_proto.DATETIME_TXT_FMT, dt_time);

        log.DebugMsgFmt('Имя <%s : %s>. Тег <%s>. Адрес <%s>. Время <%s>. Значение <%s>', [self.Name, self.Description, address, dt_str, value]);

        // Записать в буфер
        if TimeState.HasKey(dt_str) then
        begin
          //log.DebugMsgFmt('Установка тега <%s> значение: <%s> запись буфера за <%s>', [tag_name, value, dt_str]);
          new_state := TimeState.GetByName(dt_str) As TStrDictionary;
          new_state.SetStrValue(tag_name, value);
        end
        else
        begin
          //log.DebugMsgFmt('Добавление тега <%s> значение: <%s>. Создание новой записи буфера за <%s>', [tag_name, value, dt_str]);
          new_state := CreateTags(True);
          new_state.SetStrValue(tag_name, value);
          TimeState.AddObject(dt_str, new_state);
        end;
        // Перейти на следующу запись
        FSQLQuery.Next;
      end;
    end;
  except
    log.FatalMsgFmt('Ошибка чтения всех данных из источника данных <%s>', [Name]);
  end;

  Disconnect();
  tags.Destroy();
end;

procedure TICMySQLDBNode.SetProperties(dProperties: TStrDictionary);
var
  value: AnsiString;
begin
  inherited SetProperties(dProperties);

  if Properties.HasKey('dt_frame_tick') then
  begin
    value := Properties.GetStrValue('dt_frame_tick');
    // log.DebugMsgFmt('Кадр реистрируемых данных в буфере <%s>', [value]);
    DTFrameTick.Scan(obj_proto.DATETIME_TXT_FMT, value);
  end;

  if Properties.HasKey('sql_fmt') then
  begin
    value := Properties.GetStrValue('sql_fmt');
    // log.DebugMsgFmt('Формат SQL <%s>', [value]);
    SQLFmt := value;
  end;

  if Properties.HasKey('dt_field_name') then
  begin
    value := Properties.GetStrValue('dt_field_name');
    // log.DebugMsgFmt('Имя поля даты-времени <%s>', [value]);
    DtFieldName := value;
  end;
end;

{ Выбрать описания тегов из свойств }
function TICMySQLDBNode.CreateTags(bClearValue: Boolean): TStrDictionary;
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
      if bClearValue then
        value := ''
      else
        value := Properties.GetStrValue(key);
      //log.DebugMsgFmt('Тег <%s : %s>', [key, value]);
      tags.AddStrValue(key, value);
    end;
  end;
  Result := tags;
end;

{
Вычислить начальное время запрашиваемого диапазона от указанного
@param dtEnd Конечная дата-время вычисляемого диапазона.
              Если не определена, то берется текущая системная.
@param dtTick Временной шаг
@param iCount: Количество шагов
@param bNotMonth: С точностью до месяца?
@param bNotDay: С точностью до дня?
@param bNotHour: С точностью до часа?
@param bNotMinute: С точностью до минут?
@param bNotSecond: С точностью до секунд?
@return Вычисленное временное значение начала диапазона
}
function TICMySQLDBNode.CalcStartDateTime(dtEnd: TDateTime; dtTick: dtfunc.TDateTimeDelta; iCount: Integer;
                                         bNotMonth: Boolean; bNotDay: Boolean; bNotHour: Boolean; bNotMinute: Boolean; bNotSecond: Boolean): TDateTime;
var
  curYear, curMonth, curDay : Word;
  curHour, curMin, curSec, curMilli : Word;
  i: Integer;
begin
  if dtEnd = 0 then
    dtEnd := Now();

  if dtTick = nil then
    dtTick := DTFrameTick;

  DateUtils.DecodeDateTime(dtEnd, curYear, curMonth, curDay,
                           curHour, curMin, curSec, curMilli);

  if not bNotSecond then
  begin
    dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, curHour, curMin, 0, 0);
    if not bNotMinute then
    begin
      dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, curHour, 0, 0, 0);
      if not bNotHour then
      begin
        dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, 0, 0, 0, 0);
        if not bNotDay then
        begin
          dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, 1, 0, 0, 0, 0);
          if not bNotMonth then
            dtEnd := DateUtils.EncodeDateTime(curYear, 1, 1, 0, 0, 0, 0);
        end;
      end;
    end;
  end;

  //log.DebugMsgFmt('<%s>', [FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtEnd)]);
  Result := dtEnd;
  for i := 0 to iCount - 1  do
    Result := dtTick.DecTo(Result);
end;

end.

