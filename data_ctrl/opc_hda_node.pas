{
Модуль узла OPC HDA сервера

Версия: 0.0.1.1
}

unit opc_hda_node;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ActiveX,
    OPCHDA,
    obj_proto, dictionary, strfunc;

const
  OPC_SERVER_NODE_TYPE: AnsiString = 'OPC_HDA';

  RESERV_PROPERTIES: Array [1..5] Of String = ('type', 'name', 'description', 'opc_server', 'topic');

  UNKNOWN_GROUP_NAME: AnsiString = 'UNKNOWN_GROUP';

  DEFAULT_COMPUTER_NAME: AnsiString = 'localhost';


type
  {
  Класс взаимодействия с OPC HDA сервером.
  }
  TICOPCHDANode = class(TICObjectProto)

  private
    { Объект OPC клиента }
    FOPCClient: TOPCClient;

    { Наименование OPC сервера }
    FOPCServerName: AnsiString;

    { Наименование компьютера сервера }
    FComputerName: AnsiString;

    { Получить хендл сервера  }
    function GetItemServerHandle(aServerIf: IUnknown; sItem: String; iClient: DWORD; var iServer: DWORD): HRESULT;

    { Внутренние переменные для работы с OPC HDA интерфейсами }
    //HDAServerInterface:         IOPCHDA_Server;
    FHDASyncRead:                IOPCHDA_SyncRead;
    iServerH:                    DWORD;
    //HResult:                    HRESULT;
    //// чтение
    //htStartTime:                OPCHDA_TIME;
    //htEndTime:                  OPCHDA_TIME;
    //dwNumValues:                DWORD;
    //bBounds:                    BOOL;
    //dwNumItems:                 DWORD;
    //phServer:                   POPCHANDLEARRAY;
    //arrServer:                  array [0..0] of DWORD;
    //ppItemValues:               POPCHDA_ITEMARRAY;
    //ppErrors:                   PResultList;
    //ppItemValuesItem:           OPCHDA_ITEM;
    //pvDataValues:               POleVariantArray;
    //pftTimeStamps:              PFileTimeArray;
    //haAggregate:                DWORD;
    //dwCount:                    DWORD;
    //
    //NewDataArray:               TStrings;
    //DataArray:                  TStrings;
    //DateArray:                  TStrings;
    //CountArray, List:           TStrings;
    //AggregateArray:             TStrings;
    //Computers, ListHDA:         TStringList;
    //szNode:                     POleStr;
    //pszItemID:                  PWideChar;
    //pszName:                    PWideChar;
    //pdwAttrID:                  PDWORDARRAY;
    //pOperator:                  POPCHDA_OPERATORCODESARRAY;
    //Ini:                        TIniFile;
    //
    //flTree, flIn:               bool;
    //flDb, flAuto:               bool;
    //Container:                  IConnectionPointContainer;
    //ServerProgID:               POLeStr;
    //ServerCLSID:                TCLSID;
    //pdwCount:                   DWORD;
    //ppdwAttrID:                 PDWORDARRAY;
    //ppszAttrName:               POleStrList;
    //ppszAttrDesc:               POleStrList;
    //ppvtAttrDataType:           PVarTypeList;

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
  //  function CreateTags(): TStrDictionary;

    {
    Фунция чтения данных
    @param aValues Список строк адресов читаемых значений
    @param Список строк прочитанных значений
    }
    function Read(aValues: TStringList): TStringList; override;
    {
    Фунция чтения всех внутренних данных
    @param Список строк прочитанных значений
    }
    function ReadAll(): TStringList; override;
    {
    Функция чтения данных по адресам
    @param aValues Массив адресов читаемых значений
    @param Список строк прочитанных значений
    }
    function ReadAddresses(aValues: Array Of String): TStringList; override;
    {
    Функция чтения значения тега по адресу
    @param aAddress Адрес читаемого значения тега
    @param Прочитанное значение в виде строки
    }
    function ReadAddress(aAddress: AnsiString): AnsiString;
    {
    Фунция записи данных
    @param aValues Список записываемых значений
    @return True - запись прошла успешно / False - ошибка записи
    }
    function Write(aValues: TStringList): Boolean; override;
    {
    Фунция записи всех внутренних данных
    @return True - запись прошла успешно / False - ошибка записи
    }
    function WriteAll(): Boolean; override;

    { Установить свойства в виде списка параметров }
    procedure SetPropertiesArray(aArgs: Array Of Const); override;

    { Установить свойства объекта в виде словаря }
    procedure SetProperties(dProperties: TStrDictionary); override;

end;

implementation

uses
  LCLIntf, // Для вычисления времени выполнения
  log, filefunc;

constructor TICOPCHDANode.Create;
begin
  inherited Create;
  FOPCClient := nil;

  FComputerName := DEFAULT_COMPUTER_NAME;
end;

destructor TICOPCHDANode.Destroy;
begin
  if FOPCClient <> nil then
  begin
    FOPCClient.Destroy;
    FOPCClient := nil;
  end;
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
Фунция чтения данных
}
function TICOPCHDANode.Read(aValues: TStringList): TStringList;
begin
end;

{
Фунция чтения всех внутренних данных
}
function TICOPCHDANode.ReadAll(): TStringList;
var
  i: Integer;
  tag_name: AnsiString;
  tag_value: AnsiString;

begin
  log.DebugMsgFmt('Чтение всех внутренних данных. Объект <%s>', [Name]);
  // Кроме чтения данных обновляем
  // внутреннее состояние источника данных
  State := CreateTags;
  Result := Read(nil);
  for i := 0 to State.Count - 1 do
  begin
    tag_name := State[i];
    tag_value := Result[i];
    log.DebugMsgFmt('Значение состояния <%s : %s>', [tag_name, tag_value]);
    State.SetStrValue(tag_name, tag_value);
  end;
end;


function TICOPCHDANode.ReadAddresses(aValues: Array Of String): TStringList;
begin
end;

{
Функция чтения значения тега по адресу
@param aAddress Адрес читаемого значения тега
@param Прочитанное значение в виде строки
}
function TICOPCHDANode.ReadAddress(aAddress: AnsiString): AnsiString;
var
  HResult: HRESULT;
  htStartTime: OPCHDA_TIME;
  htEndTime: OPCHDA_TIME;

begin
  Connect();

  HResult := GetItemServerHandle(FHDASyncRead , 'TagName', 1, iServerH);

  htStartTime.bString := False;
  htStartTime.ftTime := filefunc.DateTimeToFileTime(EncodeDateTime(CYear, CMonth, CDAy, 00, 00, 00, 000));

  arrServer[0] := iServerH;
  phServer := @arrServer;
  HResult := FHDASyncRead.ReadRaw(htStartTime, htEndTime, 24, False, 1, phServer, ppItemValues, ppErrors);
  if ppItemValues = nil then
    ListBox1.Items.Add(TimeToStr(Now) + ' Error! ');
  if Res=E_FAIL then
    ShowMessage('fail');

  DataArray.Clear;
  DateArray.Clear;
  NewDataArray.Clear;

  ppItemValuesItem := ppItemValues^[0];
  pvDataValues := ppItemValuesItem.pvDataValues;
  pftTimeStamps := ppItemValuesItem.pftTimeStamps;
  haAggregate := ppItemValuesItem.haAggregate;
  dwCount := ppItemValuesItem.dwCount;
  for i := 0 to vt do
  begin
    DataArray.Append(pvDataValues[i]);
  end;
  for i := 0 to vt-1 do
  begin
    DateArray.Append(DateTimeToStr(FileTimeToDateTime(pftTimeStamps[i])));
  end;

  Disconnect();
end;

{
Фунция записи данных
}
function TICOPCHDANode.Write(aValues: TStringList): Boolean;
begin
  Result := False;
end;

{ Фунция записи всех внутренних данных }
function TICOPCHDANode.WriteAll(): Boolean;
begin
  Result := Write(nil);
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

procedure TICOPCHDANode.SetProperties(dProperties: TStrDictionary);
begin
  inherited SetProperties(dProperties);

  if Properties.HasKey('opc_server') then
    SetOPCServerName(Properties.GetStrValue('opc_server'))
end;

{  Установить связь }
function TICOPCHDANode.Connect(sComputer: AnsiString; sOPCServerName: AnsiString): Boolean;
var
  HResult: HRESULT;

begin
  if sComputer = '' then
    sComputer := FComputerName;
  if sOOPCServerName = '' then
    sOOPCServerName := FOOPCServerName;

  Result := False;
  if Trim(sOPCServerName) <> '' then
  begin
    ServerProgID := StringToOleStr(sOPCServerName);
    HResult := CLSIDFromProgID(ServerProgID, ServerCLSID);
    try
      FHDASyncRead := CreateRemoteComObject(sComputer, ServerCLSID) as IOPCHDA_SyncRead;
    except
      FHDASyncRead := nil;
      log.FatalMsg('Класс не зарегистрирован');
      Exit;
    end;
    FHDASyncRead.QueryInterface(iid, Container);
    Result := True;
  end;
end;

{ Разорвать связь }
function TICOPCHDANode.Disconnect(): Boolean;
var
  HResult: HRESULT;

begin
 FHDASyncRead._AddRef;
 HResult := FHDASyncRead._Release;
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
 HResult: HRESULT;

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
     Result := Errors[0];
     CoTaskMemFree(Errors);
     iServerH := pphServer^[0];
     CoTaskMemFree(pphServer);
   end;
 end;
end;

end.

