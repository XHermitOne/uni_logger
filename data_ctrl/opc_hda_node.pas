{
Модуль узла OPC HDA сервера

Версия: 0.0.1.1
}

unit opc_hda_node;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
  Windows, ActiveX, ComObj,
  {$ENDIF}
  Classes, SysUtils,
  OPCHDA, OPCtypes,
  obj_proto, dictionary, strfunc;

const
  OPC_HDA_NODE_TYPE: AnsiString = 'OPC_HDA';

  RESERV_PROPERTIES: Array [1..6] Of String = ('type', 'name', 'description', 'opc_server', 'topic', 'value_time_count');

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

    { Получить хендл сервера  }
    function GetItemServerHandle(aServerInterface: IUnknown; sItem: String; iClient: DWORD; var iServer: DWORD): HRESULT;

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
  haAggregate: DWORD;
  dwCount: DWORD;

  NewDataArray: TStrings;
  DataArray: TStrings;
  DateArray: TStrings;

  i, i_tag: Integer;
  tags: TStrDictionary;
  address: AnsiString;

begin
  Result := TStringList.Create;

  // Список читаемых тегов
  tags := CreateTags();

  Connect();

  for i_tag := 0 to tags.Count - 1 do
  begin
    address := tags.GetStrValue(tags[i]);
    HRes := GetItemServerHandle(FHDASyncRead , address, 1, iServerH);

    htStartTime.bString := False;
    htStartTime.ftTime := filefunc.DateTimeToFileTime(dtTime);

    arrServer[0] := iServerH;
    phServer := @arrServer;
    HRes := FHDASyncRead.ReadRaw(htStartTime, htEndTime,
                                 ValueTimeCount, False, 1,
                                 phServer, ppItemValues, ppErrors);
    if ppItemValues = nil then
      log.WarningMsgFmt('Ошибка чтения значения по адресу <%s> из OPC HDA сервера <%s>', [address, FOPCServerName]);
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
  end;
  Disconnect();
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
begin
  inherited SetProperties(dProperties);

  if Properties.HasKey('opc_server') then
    SetOPCServerName(Properties.GetStrValue('opc_server'));
  if Properties.HasKey('value_time_count') then
      ValueTimeCount := StrToInt(Properties.GetStrValue('value_time_count'));
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

  Result := False;
  if Trim(sOPCServerName) <> '' then
  begin
    ServerProgID := StringToOleStr(sOPCServerName);
    HRes := CLSIDFromProgID(ServerProgID, ServerCLSID);
    try
      FHDASyncRead := ComObj.CreateRemoteComObject(sComputer, ServerCLSID) as IOPCHDA_SyncRead;
    except
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
   Result := ServerInterface.GetItemHandles(1, @arrPsItemW, phClient, pphServer, Errors);

   if Succeeded(Result) then
   begin
     Result := Errors^[0];
     CoTaskMemFree(Errors);
     iServerH := pphServer^[0];
     CoTaskMemFree(pphServer);
   end;
 end;
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

end.

