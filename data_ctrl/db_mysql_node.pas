{
Модуль узла MySQL БД сервера.

Версия: 0.0.1.1
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
  SQLdb, mysql57conn,
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
    FDBConnection:   TMySQL57Connection;            { интерфейс MySQL сервера }

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

  public
    constructor Create;
    destructor Destroy; override;
    procedure Free;

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

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

end;


implementation

uses
  LCLIntf, // Для вычисления времени выполнения
  log, filefunc, memfunc;


constructor TICMySQLDBNode.Create;
begin
  inherited Create;

  FDTFrameTick := dtfunc.TDateTimeDelta.Create;

  FSQLQuery := TSQLQuery.Create(nil);
  FSQLTransaction := TSQLTransaction.Create(nil);
  FDBConnection := TMySQL57Connection.Create(nil);
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
  item_handle: HANDLE;
  iClient: DWORD = 1;
  mStartTime, mEndTime: TDateTime;

  pItemValues: STRING_ARRAY;
  pTimeStamps: DATETIME_ARRAY;
  pQualities: STRING_ARRAY;
  // Количество прочитанных значений
  dwCount: DWORD;

  i, i_tag: Integer;
  tags: TStrDictionary;
  tag_name, address, value, dt_str: AnsiString;
  dt_time: TDateTime;
  new_state: TStrDictionary;
  cur_month, cur_day, cur_hour, cur_minute, cur_sec: Word;
begin
  if dtTime = 0 then
    dtTime := Now();

  Result := nil; //TStringList.Create;

  // Список читаемых тегов
  tags := CreateTags();
  log.DebugMsgFmt('Читаемых тегов <%d>', [tags.Count]);

  // Перед началом чтения необходимо очистить буфер
  ClearTimeState();

  Connect();

  try
    for i_tag := 0 to tags.Count - 1 do
    begin
      tag_name := tags.GetKey(i_tag);
      address := tags.GetStrValue(tag_name);
      log.DebugMsgFmt('Чтение данных тега <%s> по адресу <%s>', [tag_name, address]);
      try
        item_handle := GetItemHandle(address, iClient);
      except
        log.FatalMsgFmt('Ошибка получения хендла тега <%s>', [address]);
        break;
      end;
      // log.DebugMsgFmt('Получение хендла тега. Результат <%d : %d>', [HRes, iServerH]);

      cur_day := ValueTimeTick.DayDelta;
      cur_month := ValueTimeTick.MonthDelta;
      cur_hour := ValueTimeTick.HourDelta;
      cur_minute := ValueTimeTick.MinuteDelta;
      cur_sec := ValueTimeTick.SecondDelta;

      mStartTime := CalcStartDateTime(dtTime, nil, 0, cur_month <> 0, cur_day <> 0, cur_hour <> 0, cur_minute <> 0, cur_sec <> 0);
      log.DebugMsgFmt('Запрашиваемый диапазон. Базовое время %s. Начальное время %s', [FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtTime),
                                                                                       FormatDateTime(obj_proto.DATETIME_TXT_FMT, mStartTime)]);

      mEndTime := CalcEndDateTime(dtTime, cur_month <> 0, cur_day <> 0, cur_hour <> 0, cur_minute <> 0, cur_sec <> 0);
      log.DebugMsgFmt('Запрашиваемый диапазон. Базовое время %s. Конечное время %s', [FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtTime),
                                                                                      FormatDateTime(obj_proto.DATETIME_TXT_FMT, mEndTime)]);

      try
        dwCount := ReadItemValues(item_handle, mStartTime, mEndTime, FValueTimeCount, Addr(pTimeStamps), Addr(pItemValues), Addr(pQualities));
      except
        log.FatalMsgFmt('Ошибка чтения значений тега <%s>', [address]);
        break;
      end;

      if dwCount = 0 then
        log.WarningMsg('Нет данных. Возможно нет связи с контроллером');

      for i := 0 to dwCount - 1 do
      begin
        try
          value := Variants.VarToStr(pItemValues[i]);
        except
          log.FatalMsgFmt('Ошибка приведения типа значения к строке. Тег <%s>', [tag_name]);
          value := '';
        end;
        dt_time := pTimeStamps[i];
        dt_str := FormatDateTime(obj_proto.DATETIME_TXT_FMT, dt_time);
        log.DebugMsgFmt('Источник <%s>. OPC HDA. Прочитаны данные <%s> тега <%s> за <%s>', [Name, value, tag_name, dt_str]);
        // Записать в буфер
        if TimeState.HasKey(dt_str) then
        begin
          log.DebugMsgFmt('Установка тега <%s> значение: <%s> запись буфера за <%s>', [tag_name, value, dt_str]);
          new_state := TimeState.GetByName(dt_str) As TStrDictionary;
          new_state.SetStrValue(tag_name, value);
        end
        else
        begin
          log.DebugMsgFmt('Добавление тега <%s> значение: <%s>. Создание новой записи буфера за <%s>', [tag_name, value, dt_str]);
          new_state := CreateTags(True);
          new_state.SetStrValue(tag_name, value);
          TimeState.AddObject(dt_str, new_state);
        end;
        // Записываем в выходной список, если необходимо ,
        // то можно потом распарсить
        // Result.Add(Format('%s|%s|%s', [tag_name, dt_str, value]));
      end;
      // Освобождение хендла
      ReleaseItemHandle(item_handle);
    end;
  except
    log.FatalMsgFmt('Ошибка чтения всех данных из источника данных <%s>', [Name]);
  end;
  //TimeState.PrintContent();

  Disconnect();
  tags.Destroy();
end;

procedure TICMySQLDBNode.SetProperties(dProperties: TStrDictionary);
var
  value: AnsiString;
begin
  inherited SetProperties(dProperties);

  if Properties.HasKey('opc_server') then
    SetOPCServerName(Properties.GetStrValue('opc_server'));
  if Properties.HasKey('value_time_count') then
  begin
    value := Properties.GetStrValue('value_time_count');
    log.DebugMsgFmt('Количество регистрируемых данных в буфере <%s>', [value]);
    ValueTimeCount := StrToInt(value);
  end;
  if Properties.HasKey('value_time_tick') then
  begin
    value := Properties.GetStrValue('value_time_tick');
    log.DebugMsgFmt('Время одного тика регистрации данных в буфере <%s>', [value]);
    // ValueTimeTick := DateUtils.ScanDateTime(obj_proto.DATETIME_TXT_FMT, value);
    ValueTimeTick.Scan(obj_proto.DATETIME_TXT_FMT, value);
    log.DebugMsgFmt('Время одного тика регистрации данных в буфере <%s>. Временное значение <%s>', [value,
                                                                                                    ValueTimeTick.ToFormat(obj_proto.DATETIME_TXT_FMT)]);
  end;
end;

{  Установить связь }
function TICMySQLDBNode.Connect(sComputer: AnsiString; sOPCServerName: AnsiString): Boolean;
var
  mConnectHDA: WtHDAClientAPI.ConnectHDA;
begin
  Result := False;

  if FDllInstance = 0 then
  begin
    FDllInstance := LoadLibrary(PChar(DLL_NAME));
    if FDllInstance = 0 then
    begin
      log.ErrorMsgFmt('Не найдена динамическая библиотека DLL <%s>', [DLL_NAME]);
      Exit;
    end
    else
      log.InfoMsgFmt('Установка связи с DLL <%s>', [DLL_NAME]);
  end;

  if sComputer = '' then
    sComputer := FComputerName;
  if sOPCServerName = '' then
    sOPCServerName := FOPCServerName;
  //log.InfoMsgFmt('Установка связи с <%s : %s>', [sComputer, sOPCServerName]);

  // Подключение к OPC
  if Trim(sOPCServerName) <> '' then
  begin
    try
      mConnectHDA := WtHDAClientAPI.ConnectHDA(GetProcAddress(FDllInstance, '_ConnectHDA@8'));
      Assert(mConnectHDA <> nil);
      if mConnectHDA = nil then
      begin
        log.ErrorMsg('Не найдена функция ConnectHDA в DLL');
        Exit;
      end;
      FHConnect := mConnectHDA(PChar(sComputer), PChar(sOPCServerName));
      Result := True;
      log.InfoMsgFmt('Установка связи с OPC сервером [%s : %s]', [sComputer, sOPCServerName]);
    except
      log.FatalMsgFmt('Ошибка установки связи с OPC сервером [%s : %s]', [sComputer, sOPCServerName]);
    end;
  end;
end;

{ Разорвать связь }
function TICMySQLDBNode.Disconnect(): Boolean;
var
  mDisconnectHDA: WtHDAClientAPI.DisconnectHDA;
begin
  Result := False;

  // Библиотека выгружена и не надо ничего болше делать
  if FDllInstance = 0 then
    Exit;

  try
    mDisconnectHDA := WtHDAClientAPI.DisconnectHDA(GetProcAddress(FDllInstance, '_DisconnectHDA@4'));
    Assert(mDisconnectHDA <> nil);
    if mDisconnectHDA = nil then
      log.ErrorMsg('Не найдена функция DisconnectHDA в DLL')
    else
    begin
      mDisconnectHDA(FHConnect);
      log.InfoMsg('Разрыв связи с OPC сервером');
    end;
  except
    log.FatalMsg('Ошибка разрыва связи с OPC сервером');
  end;

  if FDllInstance <> 0 then
  begin
    FreeLibrary(FDllInstance);
    FDllInstance := 0;
    log.InfoMsgFmt('Разрыв связи с DLL <%s>', [DLL_NAME]);
    Result := True;
  end;
  //log.InfoMsg('Разрыв связи');
end;

{ Получить хендл сервера  }
function TICMySQLDBNode.GetItemHandle(sItemName: AnsiString; iClientHandle: DWORD): HANDLE;
var
  mGetHDAItemHandle: WtHDAClientAPI.GetHDAItemHandle;
begin
  Result := 0;

  try
    mGetHDAItemHandle := WtHDAClientAPI.GetHDAItemHandle(GetProcAddress(FDllInstance, '_GetHDAItemHandle@12'));
    Assert(mGetHDAItemHandle <> nil);
    if mGetHDAItemHandle = nil then
      log.ErrorMsg('Не найдена функция GetHDAItemHandle в DLL')
    else
    begin
      Result := mGetHDAItemHandle(FHConnect, PChar(sItemName), iClientHandle);
    end;
  except
    log.FatalMsg('Ошибка определения хендла элемента OPC HDA серверa');
  end;
end;

function TICMySQLDBNode.ReleaseItemHandle(hItem: HANDLE): Boolean;
var
  mReleaseHDAItemHandle: WtHDAClientAPI.ReleaseHDAItemHandle;
begin
  Result := False;

  try
    mReleaseHDAItemHandle := WtHDAClientAPI.ReleaseHDAItemHandle(GetProcAddress(FDllInstance, '_ReleaseHDAItemHandle@8'));
    Assert(mReleaseHDAItemHandle <> nil);
    if mReleaseHDAItemHandle = nil then
      log.ErrorMsg('Не найдена функция ReleaseHDAItemHandle в DLL')
    else
    begin
      Result := mReleaseHDAItemHandle(FHConnect, hItem);
      log.InfoMsg('Освобождение хендла тега');
    end;
  except
    log.FatalMsg('Ошибка освобождения хендла элемента OPC HDA серверa');
  end;
end;

function TICMySQLDBNode.ReadItemValues(hItem: HANDLE; aStart, aEnd: TDateTime; NumValues: DWORD; pTimeStamps: PDATETIME_ARRAY; pValues: PSTRING_ARRAY; pQualities: PSTRING_ARRAY): DWORD;
begin
  Result := ReadItemValuesStd(hItem, aStart, aEnd, NumValues, pTimeStamps, pValues, pQualities);
end;

function TICMySQLDBNode.ReadItemValuesStd(hItem: HANDLE; aStart, aEnd: TDateTime; NumValues: DWORD; pTimeStamps: PDATETIME_ARRAY; pValues: PSTRING_ARRAY; pQualities: PSTRING_ARRAY): DWORD;
var
  mReadHDAItemValues: WtHDAClientAPI.ReadHDAItemValues;
  mStart, mEnd: FILETIME;
  mTimeStamps: PFILETIME;
  mValues: PVARIANT;
  mQualities: PDWORD;
  i: Integer;
begin
  Result := 0;

  try
    mReadHDAItemValues := WtHDAClientAPI.ReadHDAItemValues(GetProcAddress(FDllInstance, '_ReadHDAItemValues@44'));
    Assert(mReadHDAItemValues <> nil);
    if mReadHDAItemValues = nil then
      log.ErrorMsg('Не найдена функция ReadHDAItemValues в DLL')
    else
    begin
      //ВНИМАНИЕ! Для функций чтения необходимо выделять память ОБЯЗАТЕЛЬНО
      mTimeStamps := PFILETIME(CoTaskMemAlloc(NumValues * SizeOf(FILETIME)));
      mValues := PVARIANT(CoTaskMemAlloc(NumValues * SizeOf(VARIANT)));
      mQualities := PDWORD(CoTaskMemAlloc(NumValues * SizeOf(DWORD)));

      mStart := filefunc.DateTimeToFileTime(aStart);
      mEnd := filefunc.DateTimeToFileTime(aEnd);
      Result := mReadHDAItemValues(FHConnect, hItem, False, mStart, mEnd, NumValues, mTimeStamps, mValues, mQualities);

      log.DebugMsgFmt('Прочитано <%d> значений', [Result]);
      // Заполнение результирующих массивов
      for i := 0 to Result - 1 do
      begin
        pTimeStamps^[i] := FileTimeToDateTime(mTimeStamps[i]);
        try
          pValues^[i] := Variants.VarToStr(mValues[i]);
        except
          log.FatalMsg('Ошибка приведения типа значения к строке');
          pValues^[i] := '';
        end;
        pQualities^[i] := '';
        // log.DebugMsgFmt('Чтение значения тега [%s : %s : %s]', [FormatDateTime('yyyy-mm-dd hh:nn:ss', pTimeStamps^[i]), pValues^[i], pQualities^[i]]);
      end;
      // Сразу очищаем память, выделенную сервером
      CoTaskMemFree(mQualities);
      CoTaskMemFree(mTimeStamps);
      CoTaskMemFree(mValues);
    end;
  except
    log.FatalMsg('Ошибка чтения данных элемента из OPC HDA серверa');
  end;
end;

function TICMySQLDBNode.ReadItemValuesVb(hItem: HANDLE; aStart, aEnd: TDateTime; NumValues: DWORD; pTimeStamps: PDATETIME_ARRAY; pValues: PSTRING_ARRAY; pQualities: PSTRING_ARRAY): DWORD;
var
  mReadHDAItemValues: WtHDAClientAPI.ReadHDAItemValuesVB;
  mStart, mEnd: VB_DATE;
  mTimeStamps: PVB_DATE;
  mValues: PVARIANT;
  mQualities: PDWORD;
  i: Integer;
begin
  Result := 0;

  try
    mReadHDAItemValues := WtHDAClientAPI.ReadHDAItemValuesVB(GetProcAddress(FDllInstance, '_ReadHDAItemValuesVB@36'));
    Assert(mReadHDAItemValues <> nil);
    if mReadHDAItemValues = nil then
      log.ErrorMsg('Не найдена функция ReadHDAItemValuesVB в DLL')
    else
    begin
      //ВНИМАНИЕ! Для функций чтения необходимо выделять память ОБЯЗАТЕЛЬНО
      mTimeStamps := PVB_DATE(CoTaskMemAlloc(NumValues * SizeOf(VB_DATE)));
      mValues := PVARIANT(CoTaskMemAlloc(NumValues * SizeOf(VARIANT)));
      mQualities := PDWORD(CoTaskMemAlloc(NumValues * SizeOf(DWORD)));

      mStart := VB_DATE(aStart);
      mEnd := VB_DATE(aEnd);
      Result := mReadHDAItemValues(FHConnect, hItem, False, Addr(mStart), Addr(mEnd), NumValues, mTimeStamps, mValues, mQualities);

      // Заполнение результирующих массивов
      for i := 0 to Result - 1 do
      begin
        pTimeStamps^[i] := TDateTime(mTimeStamps[i]);
        try
          pValues^[i] := Variants.VarToStr(mValues[i]);
        except
          log.FatalMsg('Ошибка приведения типа значения к строке');
          pValues^[i] := '';
        end;
        pQualities^[i] := '';
      end;
      // Сразу очищаем память, выделенную сервером
      CoTaskMemFree(mQualities);
      CoTaskMemFree(mTimeStamps);
      CoTaskMemFree(mValues);
    end;
  except
    log.FatalMsg('Ошибка чтения данных элемента из OPC HDA серверa');
  end;
end;

{
ВНИМАНИЕ! Функция не рабочая
}
function TICMySQLDBNode.ReadItemValuesVbNet(hItem: HANDLE; aStart, aEnd: TDateTime; NumValues: DWORD; pTimeStamps: PDATETIME_ARRAY; pValues: PSTRING_ARRAY; pQualities: PSTRING_ARRAY): DWORD;
var
  mReadHDAItemValues: WtHDAClientAPI.ReadHDAItemValuesVBnet;
  mStart, mEnd: VB_DATE;
  mTimeStamps: PPSAFEARRAY;
  mValues: PPSAFEARRAY;
  mQualities: PPSAFEARRAY;
  i: Integer;
begin
  Result := 0;

  try
    mReadHDAItemValues := WtHDAClientAPI.ReadHDAItemValuesVBnet(GetProcAddress(FDllInstance, '_ReadHDAItemValuesVBnet@36'));
    Assert(mReadHDAItemValues <> nil);
    if mReadHDAItemValues = nil then
      log.ErrorMsg('Не найдена функция ReadHDAItemValuesVBnet в DLL')
    else
    begin
      mStart := VB_DATE(aStart);
      mEnd := VB_DATE(aEnd);
      Result := mReadHDAItemValues(FHConnect, hItem, False, Addr(mStart), Addr(mEnd), NumValues, mTimeStamps, mValues, mQualities);

      // Заполнение результирующих массивов
      //for i := 0 to Result - 1 do
      //begin
      //  pTimeStamps^[i] := TDateTime(mTimeStamps[i]);
      //  try
      //    pValues^[i] := Variants.VarToStr(mValues[i]);
      //  except
      //    log.FatalMsg('Ошибка приведения типа значения к строке');
      //    pValues^[i] := '';
      //  end;
      //  pQualities^[i] := '';
      //end;
      // Сразу очищаем память, выделенную сервером
      //CoTaskMemFree(mQualities);
      //CoTaskMemFree(mTimeStamps);
      //CoTaskMemFree(mValues);
    end;
  except
    log.FatalMsg('Ошибка чтения данных элемента из OPC HDA серверa');
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
    dtTick := ValueTimeTick;
  if iCount = 0 then
    iCount := ValueTimeCount;

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

{
Коррекция конечного времени запрашиваемого диапазона.
@param dtEnd Конечная дата-время вычисляемого диапазона.
              Если не определена, то берется текущая системная.
@param bNotMonth: С точностью до месяца?
@param bNotDay: С точностью до дня?
@param bNotHour: С точностью до часа?
@param bNotMinute: С точностью до минут?
@param bNotSecond: С точностью до секунд?
@return Вычисленное временное значение конца диапазона
}
function TICMySQLDBNode.CalcEndDateTime(dtEnd: TDateTime;
                                       bNotMonth: Boolean; bNotDay: Boolean; bNotHour: Boolean; bNotMinute: Boolean; bNotSecond: Boolean): TDateTime;
var
  curYear, curMonth, curDay : Word;
  curHour, curMin, curSec, curMilli : Word;
begin
  if dtEnd = 0 then
    dtEnd := Now();

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

  Result := dtEnd;
end;

end.

