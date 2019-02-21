{
Модуль узла OPC HDA сервера

Версия: 0.0.1.3
}

unit opc_hda_node;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
  Windows, ActiveX, ComObj,
  {$ENDIF}
  Classes, SysUtils, DateUtils,
  OPCHDA, OPCtypes,
  obj_proto, dictionary, strfunc;

const
  OPC_HDA_NODE_TYPE: AnsiString = 'OPC_HDA';

  RESERV_PROPERTIES: Array [1..7] Of String = ('type', 'name', 'description', 'opc_server', 'topic', 'value_time_count', 'value_time_tick');

  UNKNOWN_GROUP_NAME: AnsiString = 'UNKNOWN_GROUP';

  DEFAULT_COMPUTER_NAME: AnsiString = 'localhost';


type
  {
  Класс взаимодействия с OPC HDA сервером.
  }
  TICOPCHDANode = class(TICObjectProto)

  private
    { Объект OPC клиента }
    // FOPCClient: TOPCClient;

    { Наименование OPC сервера }
    FOPCServerName: AnsiString;

    { Наименование компьютера сервера }
    FComputerName: AnsiString;

    { Внутренние переменные для работы с OPC HDA интерфейсами }
    FHDASyncRead: IOPCHDA_SyncRead;
    iServerH: DWORD;

    {
    Это количество считываемых елементов от выбраной даты.
    То есть если ваш тег заархивирован почасово, то
    чтобы получить данные за целые сутки вам надо считать 24 элемента
    }
    FValueTimeCount: Integer;

    {
    Это шаг регистрации временных данных в контроллере.
    Задется в настройках в формате <yyyy-mm-dd hh:nn:ss>
    }
    FValueTimeTick: TDateTime;

    { Получить хендл сервера  }
    function GetItemServerHandle(aServerInterface: IUnknown; sItem: String; iClient: DWORD; var iServer: DWORD): HRESULT;

    {
    Вычислить начальное время запрашиваемого диапазона от указанного
    @param dtEnd Конечная дата-время вычисляемого диапазона.
                  Если не определена, то берется текущая системная.
    @param dtTick Временной шаг
    @param iCount: Количество шагов
    @param bNotHour: С точностью до часа?
    @param bNotMinute: С точностью до минут?
    @param bNotSecond: С точностью до секунд?
    @return Вычисленное временное значение начала диапазона
    }
    function CalcStartDateTime(dtEnd: TDateTime=0; dtTick: TDateTime=0; iCount: Integer=0;
                               bNotHour: Boolean=True; bNotMinute: Boolean=True; bNotSecond: Boolean=True): TDateTime;
    {
    Коррекция конечного времени запрашиваемого диапазона.
    @param dtEnd Конечная дата-время вычисляемого диапазона.
                  Если не определена, то берется текущая системная.
    @param bNotHour: С точностью до часа?
    @param bNotMinute: С точностью до минут?
    @param bNotSecond: С точностью до секунд?
    @return Вычисленное временное значение конца диапазона
    }
    function CalcEndDateTime(dtEnd: TDateTime=0;
                             bNotHour: Boolean=True; bNotMinute: Boolean=True; bNotSecond: Boolean=True): TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    {
    Установить наименование OPC сервера
    @param sName Наменование OPC сервера
    }
    procedure SetOPCServerName(sName: AnsiString);

    {
    Установить наименование компьютера
    @param sName Наменование компьютера
    }
    procedure SetComputerName(sName: AnsiString);

    {  Установить связь }
    function Connect(sComputer: AnsiString = ''; sOPCServerName: AnsiString = ''): Boolean;

    { Разорвать связь }
    function Disconnect(): Boolean;


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

  published
    property ValueTimeCount: Integer read FValueTimeCount write FValueTimeCount;
    property ValueTimeTick: TDateTime read FValueTimeTick write FValueTimeTick;

end;

implementation

uses
  LCLIntf, // Для вычисления времени выполнения
  log, filefunc;

constructor TICOPCHDANode.Create;
begin
  inherited Create;

  FComputerName := DEFAULT_COMPUTER_NAME;
end;

destructor TICOPCHDANode.Destroy;
begin
  inherited Destroy;
end;

{ Установить наименование OPC сервера }
procedure TICOPCHDANode.SetOPCServerName(sName: AnsiString);
begin
  FOPCServerName := sName;
end;

{ Установить наименование компьютера }
procedure TICOPCHDANode.SetComputerName(sName: AnsiString);
begin
  FComputerName := sName;
end;

{
Установить свойства в виде списка параметров
}
procedure TICOPCHDANode.SetPropertiesArray(aArgs: Array Of Const);
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

procedure TICOPCHDANode.PrintOPCError(aHResult);
begin
  if aHResult = OPC_E_INVALIDHANDLE then
    log.ErrorMsg('  The value of the handle is invalid.')
  else if aHResult = OPC_E_BADTYPE then
    log.ErrorMsg('  The server cannot convert the data between the requested data type and the canonical data type.')
  else if aHResult = OPC_E_PUBLIC then
    log.ErrorMsg('  The requested operation cannot be done on a public group.')
  else if aHResult = OPC_E_BADRIGHTS then
    log.ErrorMsg('  The Items AccessRights do not allow the operation.')
  else if aHResult = OPC_E_UNKNOWNITEMID then
    log.ErrorMsg('  The item is no longer available in the server address space.')
  else if aHResult = OPC_E_INVALIDITEMID then
    log.ErrorMsg('  The item definition doesn't conform to the server's syntax.')
  else if aHResult = OPC_E_INVALIDFILTER then
    log.ErrorMsg('  The filter string was not valid.')
  else if aHResult = OPC_E_UNKNOWNPATH then
    log.ErrorMsg('  The item's access path is not known to the server.')
  else if
  //
  OPC_E_UNKNOWNPATH = HResult($C004000A);

  //
  // MessageId: OPC_E_RANGE
  //
  // MessageText:
  //
  //  The value was out of range.
  //
  OPC_E_RANGE = HResult($C004000B);

  //
  // MessageId: OPC_E_DUPLICATENAME
  //
  // MessageText:
  //
  //  Duplicate name not allowed.
  //
  OPC_E_DUPLICATENAME = HResult($C004000C);

  //
  // MessageId: OPC_S_UNSUPPORTEDRATE
  //
  // MessageText:
  //
  //  The server does not support the requested data rate
  //  but will use the closest available rate.
  //
  OPC_S_UNSUPPORTEDRATE = HResult($0004000D);

  //
  // MessageId: OPC_S_CLAMP
  //
  // MessageText:
  //
  //  A value passed to WRITE was accepted but the output was clamped.
  //
  OPC_S_CLAMP = HResult($0004000E);

  //
  // MessageId: OPC_S_INUSE
  //
  // MessageText:
  //
  //  The operation cannot be completed because the
  //  object still has references that exist.
  //
  OPC_S_INUSE = HResult($0004000F);

  //
  // MessageId: OPC_E_INVALIDCONFIGFILE
  //
  // MessageText:
  //
  //  The server's configuration file is an invalid format.
  //
  OPC_E_INVALIDCONFIGFILE = HResult($C0040010);

  //
  // MessageId: OPC_E_NOTFOUND
  //
  // MessageText:
  //
  //  The server could not locate the requested object.
  //
  OPC_E_NOTFOUND = HResult($C0040011);

  //
  // MessageId: OPC_E_INVALID_PID
  //
  // MessageText:
  //
  //  The server does not recognise the passed property ID.
  //
  OPC_E_INVALID_PID = HResult($C0040203);

  //
  // MessageId: OPC_E_DEADBANDNOTSET
  //
  // MessageText:
  //
  //  The item deadband has not been set for this item.
  //
  OPC_E_DEADBANDNOTSET = HResult($C0040400);

  //
  // MessageId: OPC_E_DEADBANDNOTSUPPORTED
  //
  // MessageText:
  //
  //  The item does not support deadband.
  //
  OPC_E_DEADBANDNOTSUPPORTED = HResult($C0040401);

  //
  // MessageId: OPC_E_NOBUFFERING
  //
  // MessageText:
  //
  //  The server does not support buffering of data items that are collected at
  //  a faster rate than the group update rate.
  //
  OPC_E_NOBUFFERING = HResult($C0040402);

  //
  // MessageId: OPC_E_INVALIDCONTINUATIONPOINT
  //
  // MessageText:
  //
  //  The continuation point is not valid.
  //
  OPC_E_INVALIDCONTINUATIONPOINT = HResult($C0040403);

  //
  // MessageId: OPC_S_DATAQUEUEOVERFLOW
  //
  // MessageText:
  //
  //  Data Queue Overflow - Some value transitions were lost.
  //
  OPC_S_DATAQUEUEOVERFLOW = HResult($00040404);

  //
  // MessageId: OPC_E_RATENOTSET
  //
  // MessageText:
  //
  //  Server does not support requested rate.
  //
  OPC_E_RATENOTSET = HResult($C0040405);

  //
  // MessageId: OPC_E_NOTSUPPORTED
  //
  // MessageText:
  //
  //  The server does not support writing of quality and/or timestamp.
  //
  OPC_E_NOTSUPPORTED = HResult($C0040406);

  //
  // MessageId: OPCCPX_E_TYPE_CHANGED
  //
  // MessageText:
  //
  //  The dictionary and/or type description for the item has changed.
  //
  OPCCPX_E_TYPE_CHANGED = HResult($C0040407);

  //
  // MessageId: OPCCPX_E_FILTER_DUPLICATE
  //
  // MessageText:
  //
  //  A data filter item with the specified name already exists.
  //
  OPCCPX_E_FILTER_DUPLICATE = HResult($C0040408);

  //
  // MessageId: OPCCPX_E_FILTER_INVALID
  //
  // MessageText:
  //
  //  The data filter value does not conform to the server's syntax.
  //
  OPCCPX_E_FILTER_INVALID = HResult($C0040409);

  //
  // MessageId: OPCCPX_E_FILTER_ERROR
  //
  // MessageText:
  //
  //  An error occurred when the filter value was applied to the source data.
  //
  OPCCPX_E_FILTER_ERROR = HResult($C004040A);

  //
  // MessageId: OPCCPX_S_FILTER_NO_DATA
  //
  // MessageText:
  //
  //  The item value is empty because the data filter has excluded all fields.
  //
  OPCCPX_S_FILTER_NO_DATA = HResult($0004040B);

  // OPC Alarms & Events

  //
  // MessageId: OPC_S_ALREADYACKED
  //
  // MessageText:
  //
  //  The condition has already been acknowleged
  //
  OPC_S_ALREADYACKED = HResult($00040200);

  //
  // MessageId: OPC_S_INVALIDBUFFERTIME
  //
  // MessageText:
  //
  //  The buffer time parameter was invalid
  //
  OPC_S_INVALIDBUFFERTIME = HResult($00040201);

  //
  // MessageId: OPC_S_INVALIDMAXSIZE
  //
  // MessageText:
  //
  //  The max size parameter was invalid
  //
  OPC_S_INVALIDMAXSIZE = HResult($00040202);

  //
  // MessageId: OPC_S_INVALIDKEEPALIVETIME
  //
  // MessageText:
  //
  //  The KeepAliveTime parameter was invalid
  //
  OPC_S_INVALIDKEEPALIVETIME = HResult($00040203);

  //
  // MessageId: OPC_E_INVALIDBRANCHNAME
  //
  // MessageText:
  //
  //  The string was not recognized as an area name
  //
  OPC_E_INVALIDBRANCHNAME = HResult($C0040203);

  //
  // MessageId: OPC_E_INVALIDTIME
  //
  // MessageText:
  //
  //  The time does not match the latest active time
  //
  OPC_E_INVALIDTIME = HResult($C0040204);

  //
  // MessageId: OPC_E_BUSY
  //
  // MessageText:
  //
  //  A refresh is currently in progress
  //
  OPC_E_BUSY = HResult($C0040205);

  //
  // MessageId: OPC_E_NOINFO
  //
  // MessageText:
  //
  //  Information is not available
  //
  OPC_E_NOINFO = HResult($C0040206);

  // OPC Historical Data Access

  //
  // MessageId: OPC_E_MAXEXCEEDED
  //
  // MessageText:
  //
  //  The maximum number of values requested exceeds the server's limit.
  //
  OPC_E_MAXEXCEEDED = HResult($C0041001);

  //
  // MessageId: OPC_S_NODATA
  //
  // MessageText:
  //
  //  There is no data within the specified parameters
  //
  OPC_S_NODATA = HResult($40041002);

  //
  // MessageId: OPC_S_MOREDATA
  //
  // MessageText:
  //
  // There is more data satisfying the query than was returned
  //
  OPC_S_MOREDATA = HResult($40041003);

  //
  // MessageId: OPC_E_INVALIDAGGREGATE
  //
  // MessageText:
  //
  //  The aggregate requested is not valid.
  //
  OPC_E_INVALIDAGGREGATE = HResult($C0041004);

  //
  // MessageId: OPC_S_CURRENTVALUE
  //
  // MessageText:
  //
  //  The server only returns current values for the requested item attributes.
  //
  OPC_S_CURRENTVALUE = HResult($40041005);

  //
  // MessageId: OPC_S_EXTRADATA
  //
  // MessageText:
  //
  //  Additional data satisfying the query was found.
  //
  OPC_S_EXTRADATA = HResult($40041006);

  //
  // MessageId: OPC_W_NOFILTER
  //
  // MessageText:
  //
  //  The server does not support this filter.
  //
  OPC_W_NOFILTER = HResult($80041007);

  //
  // MessageId: OPC_E_UNKNOWNATTRID
  //
  // MessageText:
  //
  //  The server does not support this attribute.
  //
  OPC_E_UNKNOWNATTRID = HResult($C0041008);

  //
  // MessageId: OPC_E_NOT_AVAIL
  //
  // MessageText:
  //
  //  The requested aggregate is not available for the specified item.
  //
  OPC_E_NOT_AVAIL = HResult($C0041009);

  //
  // MessageId: OPC_E_INVALIDDATATYPE
  //
  // MessageText:
  //
  //  The supplied value for the attribute is not a correct data type.
  //
  OPC_E_INVALIDDATATYPE = HResult($C004100A);

  //
  // MessageId: OPC_E_DATAEXISTS
  //
  // MessageText:
  //
  //  Unable to insert - data already present.
  //
  OPC_E_DATAEXISTS = HResult($C004100B);

  //
  // MessageId: OPC_E_INVALIDATTRID
  //
  // MessageText:
  //
  //  The supplied attribute ID is not valid.
  //
  OPC_E_INVALIDATTRID = HResult($C004100C);

  //
  // MessageId: OPC_E_NODATAEXISTS
  //
  // MessageText:
  //
  //  The server has no value for the specified time and item ID.
  //
  OPC_E_NODATAEXISTS = HResult($C004100D);

  //
  // MessageId: OPC_S_INSERTED
  //
  // MessageText:
  //
  //  The requested insert occurred.
  //
  OPC_S_INSERTED = HResult($4004100E);

  //
  // MessageId: OPC_S_REPLACED
  //
  // MessageText:
  //
  //  The requested replace occurred.
  //
  OPC_S_REPLACED = HResult($4004100F);

  // OPC Security

  //
  // MessageId: OPC_E_PRIVATE_ACTIVE
  //
  // MessageText:
  //
  //  OPC Security: A session using private OPC credentials is already active.
  //
  OPC_E_PRIVATE_ACTIVE = HResult($C0040301);

  //
  // MessageId: OPC_E_LOW_IMPERS_LEVEL
  //
  // MessageText:
  //
  //  OPC Security: Server requires higher impersonation level to access secured
  //  data.
  //
  OPC_E_LOW_IMPERS_LEVEL = HResult($C0040302);

  //
  // MessageId: OPC_S_LOW_AUTHN_LEVEL
  //
  // MessageText:
  //
  //  OPC Security: Server expected higher level of package privacy.
  //
  OPC_S_LOW_AUTHN_LEVEL = HResult($00040303);

end;

{
Чтение всех внутренних данных, описанных в свойствах.
@param dtTime: Время актуальности за которое необходимо получить данные.
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICOPCHDANode.ReadAll(dtTime: TDateTime = 0): TStringList;
var
  HRes: HRESULT;
  htStartTime: OPCHDA_TIME;
  htEndTime: OPCHDA_TIME;
  arrServer: Array [0..0] of DWORD;
  phServer: POPCHANDLEARRAY;
  ppErrors: PResultList;

  ppItemValues: POPCHDA_ITEMARRAY;
  ppItemValuesItem: OPCHDA_ITEM;
  pvDataValues: POleVariantArray;
  pftTimeStamps: PFileTimeArray;
  //haAggregate: DWORD;
  //dwCount: DWORD;

  i, i_tag: Integer;
  tags: TStrDictionary;
  tag_name, address, value, dt_str: AnsiString;
  dt_time: TDateTime;
  //value_variant: Variant;

  new_state: TStrDictionary;

  cur_hour, cur_minute, cur_sec, cur_msec: Word;

begin
  if dtTime = 0 then
    dtTime := Now();

  Result := TStringList.Create;

  // Список читаемых тегов
  tags := CreateTags();
  log.DebugMsgFmt('Читаемых тегов <%d>', [tags.Count]);

  // Перед началом чтения необходимо очистить буфер
  if not TimeState.IsEmpty() then
    TimeState.Clear;

  Connect();

  try
    for i_tag := 0 to tags.Count - 1 do
    begin
      tag_name := tags.GetKey(i_tag);
      address := tags.GetStrValue(tag_name);
      log.DebugMsgFmt('Чтение данных тега <%s> по адресу <%s>', [tag_name, address]);
      try
        HRes := GetItemServerHandle(FHDASyncRead , address, 1, iServerH);
      except
        log.FatalMsgFmt('Ошибка получения хендла сервера по адресу тега <%s>', [address]);
        Disconnect();
        tags.Free();
        Exit;
      end;
      //log.DebugMsgFmt('Получение хендла сервера. Результат <%d : %d>', [HRes, iServerH]);

      htStartTime.bString := False;
      SysUtils.DecodeTime(ValueTimeTick, cur_hour, cur_minute, cur_sec, cur_msec);
      dt_time := CalcStartDateTime(dtTime, 0, 0, cur_hour<>0, cur_minute<>0, cur_sec<>0);
      //log.DebugMsgFmt('Запрашиваемый диапазон. Базовое время %s. Начальное время %s', [FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtTime),
      //                                                                                 FormatDateTime(obj_proto.DATETIME_TXT_FMT, dt_time)]);
      htStartTime.ftTime := filefunc.DateTimeToFileTime(dt_time);
      htEndTime.bString := False;
      dt_time := CalcEndDateTime(dtTime, cur_hour<>0, cur_minute<>0, cur_sec<>0);
      //log.DebugMsgFmt('Запрашиваемый диапазон. Базовое время %s. Конечное время %s', [FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtTime),
      //                                                                                FormatDateTime(obj_proto.DATETIME_TXT_FMT, dt_time)]);
      htEndTime.ftTime := filefunc.DateTimeToFileTime(dt_time);

      arrServer[0] := iServerH;
      phServer := @arrServer;

      //log.DebugMsg('Начало чтения ReadRaw');
      try
        HRes := FHDASyncRead.ReadRaw(htStartTime, htEndTime,
                                     ValueTimeCount, False, 1,
                                     phServer, ppItemValues, ppErrors);
      except
        log.FatalMsg('Ошибка чтения ReadRaw');
        Disconnect();
        tags.Free();
        Exit;
      end;
      log.DebugMsgFmt('Результат чтения ReadRaw <%d>', [HRes]);

      if HRes = Windows.E_FAIL then
      begin
        log.ErrorMsg('Ошибка чтения данных из OPC HDA сервера');
        Disconnect();
        tags.Free();
        Exit;
      end;
      if ppItemValues = nil then
      begin
        log.ErrorMsgFmt('Ошибка чтения значения по адресу <%s> из OPC HDA сервера <%s>', [address, FOPCServerName]);
        Disconnect();
        tags.Free();
        Exit;
      end;

      ppItemValuesItem := ppItemValues^[0];
      pvDataValues := ppItemValuesItem.pvDataValues;
      pftTimeStamps := ppItemValuesItem.pftTimeStamps;
      //haAggregate := ppItemValuesItem.haAggregate;
      //dwCount := ppItemValuesItem.dwCount;

      for i := 0 to ValueTimeCount - 1 do
      begin
        try
          // value_variant := pvDataValues^[i];
          value := pvDataValues^[i];
        except
          log.FatalMsgFmt('Ошибка приведения типа значения к строке. Тег <%s>', [tag_name]);
          value := '';
        end;
        dt_time := FileTimeToDateTime(pftTimeStamps^[i]);
        dt_str := FormatDateTime(obj_proto.DATETIME_TXT_FMT, dt_time);
        //log.DebugMsgFmt('Источник <%s>. OPC HDA. Прочитаны данные <%s> тега <%s> за <%s>', [Name, value, tag_name, dt_str]);
        // Записать в буфер
        if TimeState.HasKey(dt_str) then
        begin
          //log.DebugMsgFmt('Добавление тега <%s> значение: <%s> к существующей записи буфера за <%s>', [tag_name, value, dt_str]);
          new_state := TimeState.GetByName(dt_str) As TStrDictionary;
          new_state.SetStrValue(tag_name, value);
        end
        else
        begin
          //log.DebugMsgFmt('Добавление тега <%s> значение: <%s>. Создание новой записи буфера за <%s>', [tag_name, value, dt_str]);
          new_state := CreateTags();
          new_state.SetStrValue(tag_name, value);
          TimeState.AddObject(dt_str, new_state);
        end;
        // Записываем в выходной список, если необходимо ,
        // то можно потом распарсить
        Result.Add(Format('%s|%s|%s', [tag_name, dt_str, value]));
      end;
    end;
  except
    log.FatalMsgFmt('Ошибка чтения всех данных из источника данных <%s>', [Name]);
  end;
  //TimeState.PrintContent();
  Disconnect();
  tags.Free();
end;

procedure TICOPCHDANode.SetProperties(dProperties: TStrDictionary);
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
    ValueTimeTick := DateUtils.ScanDateTime(obj_proto.DATETIME_TXT_FMT, value);
    log.DebugMsgFmt('Время одного тика регистрации данных в буфере <%s>. Временное значение <%s>', [value,
                                                                                                    FormatDateTime(obj_proto.DATETIME_TXT_FMT, ValueTimeTick)]);
  end;
end;

{  Установить связь }
function TICOPCHDANode.Connect(sComputer: AnsiString; sOPCServerName: AnsiString): Boolean;
var
  HRes: HRESULT;
  ServerProgID: POLeStr;
  ServerCLSID: TCLSID;

  Container: IConnectionPointContainer;

begin
  if sComputer = '' then
    sComputer := FComputerName;
  if sOPCServerName = '' then
    sOPCServerName := FOPCServerName;

  log.InfoMsgFmt('Установка связи с <%s : %s>', [sComputer, sOPCServerName]);

  Result := False;
  if Trim(sOPCServerName) <> '' then
  begin
    ServerProgID := StringToOleStr(sOPCServerName);
    HRes := CLSIDFromProgID(ServerProgID, ServerCLSID);

    { ВНИМАНИЕ! Необходимо производить CoInitialize и CoUnintialize
    иначе будет возникать исключение:
    <EOLESysError не был произведен вызов CoInitialize> }
    HRes := CoInitialize(nil);
    try
      FHDASyncRead := ComObj.CreateRemoteComObject(sComputer, ServerCLSID) as IOPCHDA_SyncRead;
    except
      CoUninitialize;
      FHDASyncRead := nil;
      log.FatalMsg('Класс не зарегистрирован');
      Exit;
    end;
    FHDASyncRead.QueryInterface(ServerCLSID, Container);
    Result := True;
  end;
end;

{ Разорвать связь }
function TICOPCHDANode.Disconnect(): Boolean;
var
  HRes: HRESULT;

begin
 FHDASyncRead._AddRef;
 HRes := FHDASyncRead._Release;

 { ВНИМАНИЕ! Необходимо производить CoInitialize и CoUnintialize
 иначе будет возникать исключение:
 <EOLESysError не был произведен вызов CoInitialize> }
 CoUninitialize;

 log.InfoMsg('Разрыв связи');

 FHDASyncRead := nil;
 Result := True;
end;

{ Получить хендл сервера  }
function TICOPCHDANode.GetItemServerHandle(aServerInterface: IUnknown; sItem: String; iClient: DWORD; var iServer: DWORD): HRESULT;
var
 sItemW: WideString;
 PsItemW: POleStr;
 arrPsItemW: Array [0..0] Of pointer;
 arrClient: Array [0..0] Of DWORD;
 phClient, pphServer: POPCHANDLEARRAY;
 Errors: PResultList;
 ServerInterface: IOPCHDA_Server;
 HRes: HRESULT;

begin
 Result := E_FAIL;
 iServerH := 0;
 try
   ServerInterface := aServerInterface As IOPCHDA_Server;
 except
   ServerInterface := nil;
 end;

 if ServerInterface <> nil then
 begin
   sItemW := sItem;
   PsItemW := POleStr(sItemW);
   arrPsItemW[0] := PsItemW;
   arrClient[0] := iClient;
   phClient := @arrClient;

   //log.DebugMsgFmt('Получение хендела OPC HDA сервера по адресу <%s>', [sItem]);
   try
     Result := ServerInterface.GetItemHandles(1, @arrPsItemW, phClient, pphServer, Errors);
   except
     log.FatalMsg('Ошибка получения хендела OPC HDA сервера');
   end;
   // log.DebugMsgFmt('Результат выполнения <%d : %d>', [Result, E_FAIL]);

   if Succeeded(Result) then
   begin
     Result := Errors^[0];
     CoTaskMemFree(Errors);
     iServerH := pphServer^[0];
     CoTaskMemFree(pphServer);
   end;
 end
 else
   log.ErrorMsg('Не определен интерфейс OPC HDA сервера');
end;

{ Выбрать описания тегов из свойств }
function TICOPCHDANode.CreateTags(): TStrDictionary;
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

{
Вычислить начальное время запрашиваемого диапазона от указанного
@param dtEnd Конечная дата-время вычисляемого диапазона.
              Если не определена, то берется текущая системная.
@param dtTick Временной шаг
@param iCount: Количество шагов
@param bNotHour: С точностью до часа?
@param bNotMinute: С точностью до минут?
@param bNotSecond: С точностью до секунд?
@return Вычисленное временное значение начала диапазона
}
function TICOPCHDANode.CalcStartDateTime(dtEnd: TDateTime; dtTick: TDateTime; iCount: Integer;
                                         bNotHour: Boolean; bNotMinute: Boolean; bNotSecond: Boolean): TDateTime;
var
 curYear, curMonth, curDay : Word;
 curHour, curMin, curSec, curMilli : Word;
begin
  if dtEnd = 0 then
    dtEnd := Now();

  if dtTick = 0 then
    dtTick := ValueTimeTick;
  if iCount = 0 then
    iCount := ValueTimeCount;

  DateUtils.DecodeDateTime(dtEnd, curYear, curMonth, curDay,
                 curHour, curMin, curSec, curMilli);

  if not bNotSecond then
    dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, curHour, curMin, 0, 0);
  if not bNotMinute then
    dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, curHour, 0, 0, 0);
  if not bNotHour then
    dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, 0, 0, 0, 0);

  Result := dtEnd - (dtTick * iCount);
end;

{
Коррекция конечного времени запрашиваемого диапазона.
@param dtEnd Конечная дата-время вычисляемого диапазона.
              Если не определена, то берется текущая системная.
@param bNotHour: С точностью до часа?
@param bNotMinute: С точностью до минут?
@param bNotSecond: С точностью до секунд?
@return Вычисленное временное значение конца диапазона
}
function TICOPCHDANode.CalcEndDateTime(dtEnd: TDateTime;
                                       bNotHour: Boolean; bNotMinute: Boolean; bNotSecond: Boolean): TDateTime;
var
 curYear, curMonth, curDay : Word;
 curHour, curMin, curSec, curMilli : Word;
begin
  if dtEnd = 0 then
    dtEnd := Now();

  DateUtils.DecodeDateTime(dtEnd, curYear, curMonth, curDay,
                curHour, curMin, curSec, curMilli);

  if not bNotSecond then
    dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, curHour, curMin, 0, 0);
  if not bNotMinute then
    dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, curHour, 0, 0, 0);
  if not bNotHour then
    dtEnd := DateUtils.EncodeDateTime(curYear, curMonth, curDay, 0, 0, 0, 0);

  Result := dtEnd;
end;

end.

