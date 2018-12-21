{
Список сотоящий из перечня Групп и входящих в них Тэгов
Copyright (C) 2013 Чигрин В.Н. vchigrin@mail.ru
}

unit tag_list;

{$mode objfpc}{$H+}
{$TYPEINFO ON}

interface

uses
  Classes, OPCDA, OPCTypes, type_value, ActiveX;

resourcestring
  eIndex = 'Выход из диапазона индекса в %s';
  { Для TTagItem.GetQualitySrting }
  sBadQuality = 'Плохо';
  sUncertainQuality = 'Сомнительно';
  sGoodQuality = 'Хорошо';
  { Для TTagItem.GetErrorSrting }
  sOK = 'OK';
  sFAIL = 'Ошибка!';
  sBADRIGHTS = 'Нечитаем';
  sUNKNOWNITEMID = 'Недоступен';
  sDATAQUEUEOVERFLOW = 'Переполнение';
  ///
  msgItemIndex   = 'TGroup.GetItem';
  msgDeleteTag   = 'TGroup.DeleteTag';
  msgGetGroup    = 'TTagList.GetGroup';
  msgDeleteGroup = 'TTagList.DeleteGroup';
  msgGetTag      = 'TOPCTagList.GetTag';


type

  {Тип доступа к тэгам}
  tAccess = (acNone, acRead := OPC_READABLE, acWrite, acReadWrite);

  {Вызывать inhelided методы Create и Distroy класса TObject НЕ НУЖНО}

  { TTagItem - один тэг}

  {PTagItem = ^TTagItem;}
  TTagItem = class(TObject)
  private
    {доступ на запись/чтение тэга}
    FAccess: tAccess;
    FErrors: HResult;
    {Текущее состояние (OPC_QUALITY_BAD, OPC_QUALITY_UNCERTAIN, OPC_QUALITY_GOOD и т.д.)}
    FQuality: Word;
    FTagName: String;
    FTagPath: String;
    FValue: TValue;
  public
    hClient, hClientCb: OPCHANDLE;
    property Errors: HResult read FErrors write FErrors;
    property Quality: Word read FQuality write FQuality;
    constructor Create(const aName, aPath: string; const aTyp: TVarType;
                       const aAccess: tAccess);
    constructor Assign(const aTagItem: TTagItem);
    constructor Load(Reader: TReader);
    procedure WriteTagItem(Writer: TWriter);
    destructor Destroy; override;
    //class function ReadTagItem(Reader: TReader): TTagItem;
    property Access:tAccess read FAccess;
    property TagName:String read FTagName;
    property TagPath:String read FTagPath;
    property Value: TValue read FValue;
    function GetValueAsString: string;
    function GetQualitySrting: string;
    function GetErrorSrting: string;
    procedure SetTagProp(const aTN, aTP: string);
  end;


  { TGroup - группа тегов}

  {PGroup = ^TGroup;}
  TGroup = class(TObject)
  private
    FGroupName: string; {Имя группы}
    FPercentDeadBand: Single;{Процент, на сколько изменится показания тега ля обновления}
    ItemList: TList; {Список Тэгов}
    function GetCount: integer;
    function GetItem(Index: integer): TTagItem;
  public
    UpdateRate: DWord; {Период обновления, мсек}
    hSGroup, hSGroupCb:     OPCHANDLE;
    m_pIConnectionPoint: IConnectionPoint;
    m_Cookie: DWord;
    m_pIOPCItemMgt: IOPCItemMgt;
    m_pIOPCSyncIO: IOPCSyncIO;
    constructor Create(const aName: string; const aUR: DWord; aPDB: single);
    constructor Assign(const aGroup: TGroup);
    constructor Load(Reader: TReader);
    procedure WriteGroup(Writer: TWriter);
    destructor Destroy; override;
    property PercentDeadBand: Single read FPercentDeadBand;
    { Имя группы }
    property GroupName: string read FGroupName;
    function AddTag(const aTag: TTagItem): integer;
    procedure DeleteTag(const Index: integer);
    procedure Clear;
    //class function ReadGroup(Reader: TReader): TGroup;
    property Count: integer read GetCount;
    property ItList[Index: integer]: TTagItem read GetItem; default;
    procedure SetGrooupProp(const aGN: string; const aUR: DWord; const aPDB: single);
    procedure Swap(const I1, I2 :Integer);
    procedure Insert(const Index: Integer; const aTag: TTagItem);
    //function  GetNTagName(const Name:String):Integer;
    function  GetNTagName(const Name:String;
                          const Ignore:Integer = -1): Integer;
  end;


  { TTagList - Список всех Груп}

  {PTagList= ^TTagList;}
  TTagList = class(TObject)
  private
    {Список Групп}
    ListGroup: TList;
    function GetCount: integer;
    function GetGroup(Index: integer): TGroup;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: integer read GetCount;
    property GroupList[Index: integer]: TGroup read GetGroup; default;
    function AddGroup(const aGroup: TGroup): integer;
    procedure DeleteGroup(const Index: integer);
    procedure Clear;
    procedure Assign(const aTagList: TTagList);
    procedure ReadTagList(Reader: TReader);
    procedure WriteTagList(Writer: TWriter);
    procedure Swap(const I1, I2: Integer);
    procedure Insert(const Index: Integer; const aGroup: TGroup);
    //function  GetNGroupName(const Name:String):Integer;
    function  GetNGroupName(const Name: String;
                            const Ignore:Integer = -1): Integer;
  end;

implementation

uses
  SysUtils, Windows, TypInfo, OPCError;

function TypToStr(const Typ: TVarType): String; forward;
function StrToTyp(const sTyp: String): TVarType; forward;

{ TTagItem }

function TTagItem.GetValueAsString: string;
begin
  Result := Value.GetValueAsString;
end;

function TTagItem.GetQualitySrting: string;
begin
  case WordRec(Quality).Lo and %11000000 of      {Двоичное}
    OPC_QUALITY_BAD:                             {%00000000}
      Result := sBadQuality;
    OPC_QUALITY_UNCERTAIN:                       {%01000000}
      Result := sUncertainQuality;
    OPC_QUALITY_GOOD:                            {%11000000}
      Result := sGoodQuality;
  else
    Result := '';
  end;
end;

function TTagItem.GetErrorSrting: string;
begin
  case Errors of
    S_OK:                        Result := sOK;
    E_FAIL:                      Result := sFAIL;
    OPC_E_BADRIGHTS:             Result := sBADRIGHTS;
    OPC_E_UNKNOWNITEMID:         Result := sUNKNOWNITEMID;
    OPC_S_DATAQUEUEOVERFLOW:     Result := sDATAQUEUEOVERFLOW;
  else
    Result := '';
  end;
end;

procedure TTagItem.SetTagProp(const aTN, aTP: string);
begin
  FTagName := aTN;
  FTagPath := aTP;
end;

constructor TTagItem.Create(const aName, aPath: string; const aTyp: TVarType;
                            const aAccess: tAccess);
begin
  //inherited Create;
  FTagName := aName;
  FTagPath := aPath;
  FAccess  := aAccess;
  FValue   := TValue.Create(aTyp);
end;

constructor TTagItem.Assign(const aTagItem: TTagItem);
begin
  if Assigned(aTagItem) then
  begin
    FTagName := aTagItem.TagName;
    FTagPath := aTagItem.TagPath;
    FAccess  := aTagItem.Access;
    FValue   := TValue.Create(aTagItem.Value.Typ);
  end;
end;

constructor TTagItem.Load(Reader: TReader);
begin
  with Reader do
  begin
    FTagName  := ReadString;  //TagName
    FTagPath  := ReadString;  //TagPath
    FValue    := TValue.Create(StrToTyp(ReadIdent)); //Typ
    FAccess   := tAccess(GetEnumValue( TypeInfo(tAccess), ReadIdent));
  end;
end;

destructor TTagItem.Destroy;
begin
  //inherited Destroy;
  if Assigned(Value) then
    Value.Destroy;
end;

//class function TTagItem.ReadTagItem(Reader: TReader): TTagItem;
//var
//  sName, sPath: string;
//  wTyp: TVarType;
//  bAcc: tAccess;
//begin
//  with Reader do
//    begin
//      sName  := ReadString;  //TagName
//      sPath  := ReadString;  //TagPath
//      wTyp   := StrToTyp(ReadIdent); //Typ
//      bAcc := tAccess(GetEnumValue( TypeInfo(tAccess), ReadIdent));
//    end;
//  Result := TTagItem.Create(sName, sPath, wTyp, bAcc);
//end;

procedure TTagItem.WriteTagItem(Writer: TWriter);
begin
  with Writer do
  begin
    WriteString(TagName);
    WriteString(TagPath);
    WriteIdent(TypToStr(Value.Typ));
    WriteIdent(GetEnumName(TypeInfo(tAccess), byte(Access)));
  end;
end;

{ TGroup }

function TGroup.GetCount: integer;
begin
  Result := ItemList.Count;
end;

function TGroup.GetItem(Index: integer): TTagItem;
begin
  try
    Result := TTagItem(ItemList[Index]);
  except
    Result := nil;
    raise EListError.CreateFmt(eIndex,[msgItemIndex]);
  end;
end;

constructor TGroup.Create(const aName: string; const aUR: DWord; aPDB: single);
begin
  //inherited Create;
  FGroupName  := aName;
  UpdateRate := aUR;
  FPercentDeadBand := aPDB;
  ItemList    := TList.Create;
  m_pIOPCItemMgt := nil;
end;

constructor TGroup.Assign(const aGroup: TGroup);
var
  i: integer;
begin
  if Assigned(aGroup) then
  begin
    FGroupName := aGroup.GroupName;
    UpdateRate := aGroup.UpdateRate;
    FPercentDeadBand := aGroup.PercentDeadBand;
    m_pIOPCItemMgt := aGroup.m_pIOPCItemMgt;
    ItemList := TList.Create;
    for i := 0 to aGroup.Count - 1 do
      ItemList.Add(TTagItem.Assign(aGroup.GetItem(i)));
  end;
end;

constructor TGroup.Load(Reader: TReader);
var
  Tag:TTagItem;
begin
  with Reader do
  begin
    {sTem}FGroupName        := ReadString;
    {iTem}UpdateRate       := ReadInteger;
    {siTem}FPercentDeadBand := ReadFloat;
    (*Result := TGroup.Create(sTem, // GroupName:String; {Имя группы}
                              iTem, // UpdateRate:DWord; {Период обновления, мсек}
                              siTem //PercentDeadBand:Single; {процент}
                              );*)
    ItemList := TList.Create;
    ReadListBegin;
    while not (EndOfList) do {Result.AddTag(TTagItem.ReadTagItem(Reader));}
    begin
      Tag:=TTagItem.Load(Reader);
      if GetNTagName(Tag.TagName) = -1 then
        AddTag(Tag)
      else
        Tag.Free;
    end;
    ReadListEnd;
  end;
end;

destructor TGroup.Destroy;
begin
  Clear;
  ItemList.Free;
end;

function TGroup.AddTag(const aTag: TTagItem): integer;
begin
  Result := ItemList.Add(aTag);
end;

procedure TGroup.DeleteTag(const Index: integer);
begin
  try
    TTagItem(ItemList[Index]).Free;
    ItemList.Delete(Index);
  except
    raise EListError.CreateFmt(eIndex,[msgDeleteTag]);
  end;
end;

procedure TGroup.Clear;
var
  i: integer;
begin
  for i := 0 to ItemList.Count - 1 do
    TTagItem(ItemList[i]).Free;
  ItemList.Clear;
end;

//class function TGroup.ReadGroup(Reader: TReader): TGroup;
//var
//  sTem:  string;
//  iTem:  integer;
//  siTem: single;
//begin
//  with Reader do
//    begin
//      sTem   := ReadString;
//      iTem   := ReadInteger;
//      siTem  := ReadFloat;
//      Result := TGroup.Create(sTem, // GroupName:String; {Имя группы}
//                              iTem, // UpdateRate:DWord; {Период обновления, мсек}
//                              siTem //PercentDeadBand:Single; {процент}
//                              );
//      ReadListBegin;
//      while not (EndOfList) do Result.AddTag(TTagItem.ReadTagItem(Reader));
//      ReadListEnd;
//    end;
//end;

procedure TGroup.WriteGroup(Writer: TWriter);
var
  i: integer;
begin
  with Writer do
  begin
    WriteString(GroupName);
    WriteInteger(UpdateRate);
    WriteFloat(PercentDeadBand);
    WriteListBegin;
    for i := 0 to Count - 1 do
      ItList[i].WriteTagItem(Writer);
    WriteListEnd;
  end;
end;

procedure TGroup.SetGrooupProp(const aGN: string; const aUR: DWord; const aPDB: single);
begin
  FGroupName  := aGN;
  UpdateRate := aUR;
  FPercentDeadBand := aPDB;
end;

procedure TGroup.Swap(const I1, I2: Integer);
begin
  ItemList.Exchange(I1,I2);
end;

procedure TGroup.Insert(const Index: Integer; const aTag: TTagItem);
begin
  ItemList.Insert(Index, aTag);
end;

{ Возвращаем номер тэга или -1(исли тэг отсутствует) }
function TGroup.GetNTagName(const Name:String;
                            const Ignore:Integer = -1):Integer;
var
  i:Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if i = Ignore then
      Continue;
    if ItList[i].TagName = Name then
    begin
      Result := i;Break;
    end;
  end;
end;

//function TGroup.GetNTagName(const Name:String): Integer;
//begin
//  Result := GetNTagName(Name, -1);
//end;

{ TTagList }

function TTagList.GetCount: integer;
begin
  Result := ListGroup.Count;
end;

function TTagList.GetGroup(Index: integer): TGroup;
begin
  try
    Result := TGroup(ListGroup[Index]);
  except
    Result := nil;
    raise EListError.CreateFmt(eIndex,[msgGetGroup]);     {EOutIndex}
  end;
end;

constructor TTagList.Create;
begin
  //inherited Create;
  ListGroup := TList.Create;
end;

destructor TTagList.Destroy;
begin
  Clear;
  ListGroup.Free;
  //inherited Destroy;
end;

function TTagList.AddGroup(const aGroup: TGroup): integer;
begin
  Result := ListGroup.Add(aGroup);
end;

procedure TTagList.DeleteGroup(const Index: integer);
begin
  try
    TGroup(ListGroup[Index]).Free;
    ListGroup.Delete(Index);
  except
    raise EListError.CreateFmt(eIndex,[msgDeleteGroup]);  {EOutIndex}
  end;
end;

procedure TTagList.Clear;
var
  i: integer;
begin
  for i := 0 to ListGroup.Count - 1 do
    TGroup(ListGroup[i]).Free;
  ListGroup.Clear;
end;

procedure TTagList.Assign(const aTagList: TTagList);
var
  i: integer;
begin
  if aTagList is TTagList then
  begin
    ListGroup.Clear;
    for i := 0 to aTagList.Count - 1 do
      ListGroup.Add(TGroup.Assign(aTagList.GetGroup(i)));
  end;
end;

procedure TTagList.ReadTagList(Reader: TReader);
var
  Gr:TGroup;
begin
  with Reader do
  begin
    ReadListBegin;
    while not (EndOfList) do
    begin
      Gr:=TGroup.{ReadGroup}Load(Reader);
      if GetNGroupName(Gr.GroupName) = -1 then
        AddGroup(Gr)
      else
        Gr.Free;
    end;
    ReadListEnd;
  end;
end;

procedure TTagList.WriteTagList(Writer: TWriter);
var
  i: integer;
begin
  with Writer do
  begin
    WriteListBegin;
    for i := 0 to Count - 1 do
      GroupList[i].WriteGroup(Writer);
    WriteListEnd;
  end;
end;

procedure TTagList.Swap(const I1, I2: Integer);
begin
  ListGroup.Exchange(I1,I2);
end;

procedure TTagList.Insert(const Index: Integer; const aGroup: TGroup);
begin
  ListGroup.Insert(Index, aGroup);
end;

{Возвращаем номер группы или -1(исли группа отсутствует)}
function TTagList.GetNGroupName(const Name:String;
                                const Ignore:Integer = -1): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if i = Ignore then Continue;
      if GroupList[i].GroupName = Name then
      begin
        Result := i;Break;
      end;
  end;
end;

//function TTagList.GetNGroupName(const Name: String): Integer;
//begin
//  Result := GetNGroupName(Name, -1);
//end;

// Общие функции Модутя

function TypToStr(const Typ: TVarType):String;
begin
  case Typ of
    VT_EMPTY:           Result := 'VT_EMPTY';
    VT_NULL:            Result := 'VT_NULL';
    VT_I2:              Result := 'VT_I2';
    VT_I4:              Result := 'VT_I4';
    VT_R4:              Result := 'VT_R4';
    VT_R8:              Result := 'VT_R8';
    VT_CY:              Result := 'VT_CY';
    VT_DATE:            Result := 'VT_DATE';
    VT_BSTR:            Result := 'VT_BSTR';
    VT_DISPATCH:        Result := 'VT_DISPATCH';
    VT_ERROR:           Result := 'VT_ERROR';
    VT_BOOL:            Result := 'VT_BOOL';
    VT_VARIANT:         Result := 'VT_VARIANT';
    VT_UNKNOWN:         Result := 'VT_UNKNOWN';
    VT_DECIMAL:         Result := 'VT_DECIMAL';
    VT_I1:              Result := 'VT_I1';
    VT_UI1:             Result := 'VT_UI1';
    VT_UI2:             Result := 'VT_UI2';
    VT_UI4:             Result := 'VT_UI4';
    VT_I8:              Result := 'VT_I8';
    VT_UI8:             Result := 'VT_UI8';
    VT_INT:             Result := 'VT_INT';
    VT_UINT:            Result := 'VT_UINT';
  else
    Result := 'VT_EMPTY';
  end;
end;

function StrToTyp(const sTyp:String):TVarType;
begin
  case sTyp of
    'VT_EMPTY':         Result := VT_EMPTY;
    'VT_NULL':          Result := VT_NULL;
    'VT_I2':            Result := VT_I2;
    'VT_I4':            Result := VT_I4;
    'VT_R4':            Result := VT_R4;
    'VT_R8':            Result := VT_R8;
    'VT_CY':            Result := VT_CY;
    'VT_DATE':          Result := VT_DATE;
    'VT_BSTR':          Result := VT_BSTR;
    'VT_DISPATCH':      Result := VT_DISPATCH;
    'VT_ERROR':         Result := VT_ERROR;
    'VT_BOOL':          Result := VT_BOOL;
    'VT_VARIANT':       Result := VT_VARIANT;
    'VT_UNKNOWN':       Result := VT_UNKNOWN;
    'VT_DECIMAL':       Result := VT_DECIMAL;
    'VT_I1':            Result := VT_I1;
    'VT_UI1':           Result := VT_UI1;
    'VT_UI2':           Result := VT_UI2;
    'VT_UI4':           Result := VT_UI4;
    'VT_I8':            Result := VT_I8;
    'VT_UI8':           Result := VT_UI8;
    'VT_INT':           Result := VT_INT;
    'VT_UINT':          Result := VT_UINT;
  else
    Result := VT_EMPTY;
  end;
end;

end.

//try
//  ole....
// Код где ошибка
//  except
//   on e: Exception do
//    ShowMessage(SysToUTF8(e.Message));
//  end;
