{
Модуль узла OPC HDA сервера

Версия: 0.0.1.2
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

    //{
    //Фунция чтения данных
    //@param sAddresses Список адресов для чтения
    //@param dtTime: Время актуальности за которое необходимо получить данные.
    //              Если не определено, то берется текущее системное время.
    //@return Список прочитанных значений.
    //}
    //function Read(sAddresses: TStringList; dtTime: TDateTime = 0): TStringList; override;

    //{
    //Чтение значений по адресам
    //@param sAddresses Массив адресов для чтения
    //@param dtTime: Время актуальности за которое необходимо получить данные.
    //              Если не определено, то берется текущее системное время.
    //@return Список прочитанных значений.
    //}
    //function ReadAddresses(sAddresses: Array Of String; dtTime: TDateTime = 0): TStringList; override;
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
  // FOPCClient := nil;

  FComputerName := DEFAULT_COMPUTER_NAME;
end;

destructor TICOPCHDANode.Destroy;
begin
  //if FOPCClient <> nil then
  //begin
  //  FOPCClient.Destroy;
  //  FOPCClient := nil;
  //end;
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

  new_state: TStrDictionary;

  cur_hour, cur_minute, cur_sec, cur_msec: Word;

begin
  if dtTime = 0 then
    dtTime := Now();

  Result := TStringList.Create;

  // Список читаемых тегов
  tags := CreateTags();
  log.DebugMsgFmt('Читаемых тегов <%d>', [tags.Count]);

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
      end;
      log.DebugMsgFmt('Получение хендла сервера. Результат <%d : %d>', [HRes, iServerH]);

      htStartTime.bString := False;
      SysUtils.DecodeTime(ValueTimeTick, cur_hour, cur_minute, cur_sec, cur_msec);
      dt_time := CalcStartDateTime(dtTime, 0, 0, cur_hour<>0, cur_minute<>0, cur_sec<>0);
      log.DebugMsgFmt('Запрашиваемый диапазон. Базовое время %s. Начальное время %s', [FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtTime),
                                                                                       FormatDateTime(obj_proto.DATETIME_TXT_FMT, dt_time)]);
      htStartTime.ftTime := filefunc.DateTimeToFileTime(dt_time);
      htEndTime.bString := False;
      dt_time := CalcEndDateTime(dtTime, cur_hour<>0, cur_minute<>0, cur_sec<>0);
      log.DebugMsgFmt('Запрашиваемый диапазон. Базовое время %s. Конечное время %s', [FormatDateTime(obj_proto.DATETIME_TXT_FMT, dtTime),
                                                                                      FormatDateTime(obj_proto.DATETIME_TXT_FMT, dt_time)]);
      htEndTime.ftTime := filefunc.DateTimeToFileTime(dt_time);

      arrServer[0] := iServerH;
      phServer := @arrServer;

      log.DebugMsg('Начало чтения ReadRaw');
      try
        HRes := FHDASyncRead.ReadRaw(htStartTime, htEndTime,
                                     ValueTimeCount, False, 1,
                                     phServer, ppItemValues, ppErrors);
      except
        log.FatalMsg('Ошибка чтения ReadRaw');
      end;
      log.DebugMsgFmt('Результат чтения ReadRaw <%d>', [HRes]);

      if ppItemValues = nil then
        log.WarningMsgFmt('Ошибка чтения значения по адресу <%s> из OPC HDA сервера <%s>', [address, FOPCServerName]);
      if HRes = Windows.E_FAIL then
        log.ErrorMsg('Ошибка чтения данных');

      ppItemValuesItem := ppItemValues^[0];
      pvDataValues := ppItemValuesItem.pvDataValues;
      pftTimeStamps := ppItemValuesItem.pftTimeStamps;
      //haAggregate := ppItemValuesItem.haAggregate;
      //dwCount := ppItemValuesItem.dwCount;

      if not TimeState.IsEmpty() then
        TimeState.Clear;

      for i := 0 to ValueTimeCount - 1 do
      begin
        value := pvDataValues^[i];
        dt_time := FileTimeToDateTime(pftTimeStamps^[i]);
        dt_str := FormatDateTime(obj_proto.DATETIME_TXT_FMT, dt_time);
        log.DebugMsgFmt('Источник <%s>. OPC HDA. Прочитаны данные <%s> тега <%s> за <%s>', [Name, value, tag_name, dt_str]);
        // Записать в буфер
        if TimeState.HasKey(dt_str) then
        begin
          new_state := TimeState.GetByName(dt_str) As TStrDictionary;
          new_state.SetStrValue(tag_name, value);
        end
        else
        begin
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
  Disconnect();
  tags.Free();
end;

{
Чтение значения по адресу
@param sAddress Строка адреса для чтения
@param dtTime: Время актуальности за которое необходимо получить данные.
              Если не определено, то берется текущее системное время.
@return Прочитанное значение в виде строки.
}
function TICOPCHDANode.ReadAddress(sAddress: AnsiString; dtTime: TDateTime): AnsiString;
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
  haAggregate: DWORD;
  dwCount: DWORD;

  NewDataArray: TStrings;
  DataArray: TStrings;
  DateArray: TStrings;

  i: Integer;

begin
  Connect();

  HRes := GetItemServerHandle(FHDASyncRead , 'TagName', 1, iServerH);

  htStartTime.bString := False;
  htStartTime.ftTime := filefunc.DateTimeToFileTime(dtTime);

  arrServer[0] := iServerH;
  phServer := @arrServer;
  HRes := FHDASyncRead.ReadRaw(htStartTime, htEndTime,
                               ValueTimeCount, False, 1,
                               phServer, ppItemValues, ppErrors);
  if ppItemValues = nil then
    log.WarningMsgFmt('Ошибка чтения значения по адресу <%s> из OPC HDA сервера <%s>', [sAddress, FOPCServerName]);
  if HRes = Windows.E_FAIL then
    log.ErrorMsg('Ошибка чтения данных');

  DataArray.Clear;
  DateArray.Clear;
  NewDataArray.Clear;

  ppItemValuesItem := ppItemValues^[0];
  pvDataValues := ppItemValuesItem.pvDataValues;
  pftTimeStamps := ppItemValuesItem.pftTimeStamps;
  haAggregate := ppItemValuesItem.haAggregate;
  dwCount := ppItemValuesItem.dwCount;

  for i := 0 to ValueTimeCount - 1 do
  begin
    DataArray.Append(pvDataValues^[i]);
  end;

  for i := 0 to ValueTimeCount - 1 do
  begin
    DateArray.Append(DateTimeToStr(FileTimeToDateTime(pftTimeStamps^[i])));
  end;

  Disconnect();
end;

{ Выбрать описания тегов из свойств }
//function TICOPCHDANode.CreateTags(): TStrDictionary;
//var
//  i: Integer;
//  key, value: AnsiString;
//  tags: TStrDictionary;
//
//begin
//  log.DebugMsg('Создание тегов');
//  tags := TStrDictionary.Create;
//  for i := 0 to Properties.Count - 1 do
//  begin
//    key := Properties.GetKey(i);
//    if not IsStrInList(key, RESERV_PROPERTIES) then
//    begin
//      value := Properties.GetStrValue(key);
//      log.DebugMsgFmt('Тег <%s : %s>', [key, value]);
//      tags.AddStrValue(key, value);
//    end;
//  end;
//  Result := tags;
//end;

procedure TICOPCHDANode.SetProperties(dProperties: TStrDictionary);
var
  // dt_format: TFormatSettings;
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
    //dt_format := DefaultFormatSettings;
    //dt_format.DateSeparator := obj_proto.DATE_TXT_SEPARATOR;
    //dt_format.ShortDateFormat := obj_proto.DATETIME_TXT_FMT;

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

   log.DebugMsgFmt('Получение хендела OPC HDA сервера по адресу <%s>', [sItem]);
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
  log.DebugMsg('Создание тегов');
  tags := TStrDictionary.Create;
  for i := 0 to Properties.Count - 1 do
  begin
    key := Properties.GetKey(i);
    if not IsStrInList(key, RESERV_PROPERTIES) then
    begin
      value := Properties.GetStrValue(key);
      log.DebugMsgFmt('Тег <%s : %s>', [key, value]);
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

