{
Перечень значаний
Copyright (C) 2013 Чигрин В.Н. vchigrin@mail.ru
}
unit type_value;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, ActiveX;

type
  { TValue }
  TValue = class
  private
    FType  :  TVarType;
    FValue  : Pointer;
    FStr  :   String;
  public
    constructor Create(const aTyp : TVarType);
    destructor Destroy; override;
    function GetValueAsString : String;
    function GetTypName : String;
    property Typ  : TVarType read FType;
    /////////////////////////////////////////
    procedure SetValue(const aVal : OleVariant);
    /////////////////////////////////////////
    {VT_BOOL  (VARIANT_BOOL) wordbool}
    procedure SetVT_BOOL(const Val : WordBool);
    function GetVT_BOOL : WordBool;
    {VT_I2 (SmallInt)}
    procedure SetVT_I2(const Val : SmallInt);
    function GetVT_I2 : SmallInt;
    {VT_I4 (Longint)}
    {VT_INT (Longint)}
    procedure SetVT_I4(const Val : LongInt);
    function GetVT_I4 : LongInt;
    {VT_I1 (ShortInt)}
    procedure SetVT_I1(const Val : ShortInt);
    function GetVT_I1 : ShortInt;
    {VT_I8 (Int64)}
    procedure SetVT_I8(const Val : Int64);
    function GetVT_I8 : Int64;
    {VT_UI1 (Byte)}
    procedure SetVT_UI1(const Val : Byte);
    function GetVT_UI1 : Byte;
    {VT_UI2 (Word)}
    procedure SetVT_UI2(const Val : Word);
    function GetVT_UI2 : Word;
    {VT_UI4 (LongWord)}
    {VT_UINT (LongWord)}
    procedure SetVT_UI4(const Val : LongWord);
    function GetVT_UI4 : LongWord;
    {VT_UI8 (QWord)}
    procedure SetVT_UI8(const Val : QWord);
    function GetVT_UI8 : QWord;
    {VT_CY (Currency)}
    procedure SetVT_CY(const Val : Currency);
    function GetVT_CY : Currency;
    {VT_R4 (Single)}
    procedure SetVT_R4(const Val : Single);
    function GetVT_R4 : Single;
    {VT_R8 (Double)}
    procedure SetVT_R8(const Val : Double);
    function GetVT_R8 : Double;
    {VT_DATE (TOleDate)}
    procedure SetVT_DATE(const Val : TOleDate);
    function GetVT_DATE : TOleDate;
    {String
    VT_BSTR (POleStr (PWideChar)
    VT_LPSTR (PChar)
    VT_LPWSTR (PWideChar)}
    procedure SetString(const Val : String);
    function GetString : String;
  end;

implementation

uses
  Windows, Variants;

{ TValue }

constructor TValue.Create(const aTyp : TVarType);
begin
  FType := aTyp;
  case FType of
    {VT_BOOL True=-1, False=0  (VARIANT_BOOL)}
    VT_BOOL  : GetMem(FValue, SizeOf(WordBool));
    {VT_I2 (SmallInt)}
    VT_I2  : GetMem(FValue, SizeOf(SmallInt));
    {VT_I4 (Longint)}
    VT_I4,
    {VT_INT (Longint)}
    VT_INT  : GetMem(FValue, SizeOf(LongInt));
    {VT_R4 (Single)}
    VT_R4  : GetMem(FValue, SizeOf(Single));
    {VT_R8 (Double)}
    VT_R8  : GetMem(FValue, SizeOf(Double));
    {VT_CY (Currency)}
    VT_CY  : GetMem(FValue, SizeOf(Currency));
    {VT_DATE (TOleDate)}
    VT_DATE  : GetMem(FValue, SizeOf(TOleDate));
    {VT_I1 (ShortInt)}
    VT_I1  : GetMem(FValue, SizeOf(ShortInt));
    {VT_UI1 (Byte)}
    VT_UI1  : GetMem(FValue, SizeOf(Byte));
    {VT_UI2 (Word)}
    VT_UI2  : GetMem(FValue, SizeOf(Word));
    {VT_UI4 (LongWord)}
    VT_UI4,
    {VT_UINT (LongWord)}
    VT_UINT  : GetMem(FValue, SizeOf(LongWord));
    {VT_I8 (Int64)}
    VT_I8  : GetMem(FValue, SizeOf(Int64));
    {VT_UI8 (QWord)}
    VT_UI8  : GetMem(FValue, SizeOf(QWord));
      {VT_LPSTR (PChar)}
      //VT_LPSTR : FValue :=  Nil; //VT_LPSTR
      {VT_BSTR (POleStr (PWideChar))}
      //VT_BSTR : FValue :=  Nil; //VT_BSTR
      {VT_LPWSTR (PWideChar)}
      //VT_LPWSTR //VT_LPWSTR
    else
      FValue := nil;
    end;
end;

destructor TValue.Destroy;
begin
  case FType of
    {VT_BOOL True=-1, False=0  (VARIANT_BOOL)}
    VT_BOOL  : FreeMem(FValue, SizeOf(WordBool));
    {VT_I2 (SmallInt)}
    VT_I2  : FreeMem(FValue, SizeOf(SmallInt));
    {VT_I4 (Longint)}
    VT_I4,
    {VT_INT (Longint)}
    VT_INT  : FreeMem(FValue, SizeOf(LongInt));
    {VT_R4 (Single)}
    VT_R4  : FreeMem(FValue, SizeOf(Single));
    {VT_R8 (Double)}
    VT_R8  : FreeMem(FValue, SizeOf(Double));
    {VT_CY (Currency)}
    VT_CY  : FreeMem(FValue, SizeOf(Currency));
    {VT_DATE (TOleDate)}
    VT_DATE  : FreeMem(FValue, SizeOf(TOleDate));
    {VT_I1 (ShortInt)}
    VT_I1  : FreeMem(FValue, SizeOf(ShortInt));
    {VT_UI1 (Byte)}
    VT_UI1  : FreeMem(FValue, SizeOf(Byte));
    {VT_UI2 (Word)}
    VT_UI2  : FreeMem(FValue, SizeOf(Word));
    {VT_UI4 (LongWord)}
    VT_UI4,
    {VT_UINT (LongWord)}
    VT_UINT  : FreeMem(FValue, SizeOf(LongWord));
    {VT_I8 (Int64)}
    VT_I8  : FreeMem(FValue, SizeOf(Int64));
    {VT_UI8 (QWord)}
    VT_UI8  : FreeMem(FValue, SizeOf(QWord));
      {VT_LPSTR (PChar)}
      //VT_LPSTR : StrDispose(PChar(FValue)); //VT_LPSTR
      {VT_BSTR (POleStr !(PWideChar) ?(WideString))}{VT_LPWSTR (PWideChar)}
      //VT_BSTR, VT_LPWSTR : StrDispose(PChar(FValue)); //VT_LPWSTR
    else
      FValue := nil;
    end;
end;

function TValue.GetValueAsString : String;
begin
  case FType of
    {VT_BOOL  (VARIANT_BOOL) wordbool}
    VT_BOOL  : if WordBool(FValue^)
                   then
                     Result := 'True'
                   else
                     Result := 'False';
    {VT_I2 (SmallInt)}
    VT_I2  : Result := IntToStr(SmallInt(FValue^));
    {VT_I4 (Longint)}
    {VT_INT (Longint)}
    VT_I4,
    VT_INT  : Result  := IntToStr(LongInt(FValue^));
    {VT_I1 (ShortInt)}
    VT_I1  : Result   := IntToStr(ShortInt(FValue^));
    {VT_I8 (Int64)}
    VT_I8  : Result   := IntToStr(Int64(FValue^));
    {VT_UI1 (Byte)}
    VT_UI1  : Result  := IntToStr(Byte(FValue^));
    {VT_UI2 (Word)}
    VT_UI2  : Result  := IntToStr(Word(FValue^));
    {VT_UI4 (LongWord)}
    {VT_UINT (LongWord)}
    VT_UI4,
    VT_UINT  : Result := IntToStr(LongWord(FValue^));
    {VT_UI8 (QWord)}
    VT_UI8  : Result  := IntToStr(QWord(FValue^));
    {VT_CY (Currency)}
    VT_CY  : Result   := CurrToStr(Currency(FValue^));
    {VT_R4 (Single)}
    VT_R4  : Result   := FloatToStr(Single(FValue^));
    {VT_R8 (Double)}
    VT_R8  : Result   := FloatToStr(Double(FValue^));
    {VT_DATE (TOleDate)}
    VT_DATE  : Result := DateTimeToStr(TDate(FValue^));
    {VT_BSTR (POleStr (PWideChar)}
    {VT_LPWSTR (PWideChar)}
    {VT_LPSTR (PChar)}
    //UniqueString(Result);
    VT_BSTR, VT_LPWSTR, VT_LPSTR  : Result := FStr;
  else
    Result := '';
  end;
end;

///////////////////////////////////////////////////////////////
procedure TValue.SetValue(const aVal : OleVariant);
begin
  case FType of
    {VT_BOOL  (VARIANT_BOOL) wordbool}
    VT_BOOL  : SetVT_BOOL(aVal);
    {VT_I2 (SmallInt)}
    VT_I2  : SetVT_I2(aVal);
    {VT_I4 (Longint)}
    {VT_INT (Longint)}
    VT_I4,
    VT_INT  : SetVT_I4(aVal);
    {VT_I1 (ShortInt)}
    VT_I1  : SetVT_I1(aVal);
    {VT_I8 (Int64)}
    VT_I8  : SetVT_I8(aVal);
    {VT_UI1 (Byte)}
    VT_UI1  : SetVT_UI1(aVal);
    {VT_UI2 (Word)}
    VT_UI2  : SetVT_UI2(aVal);
    {VT_UI4 (LongWord)}
    {VT_UINT (LongWord)}
    VT_UI4,
    VT_UINT  : SetVT_UI4(aVal);
    {VT_UI8 (QWord)}
    VT_UI8  : SetVT_UI8(aVal);
    {VT_CY (Currency)}
    VT_CY  : SetVT_CY(aVal);
    {VT_R4 (Single)}
    VT_R4  : SetVT_R4(aVal);
    {VT_R8 (Double)}
    VT_R8  : SetVT_R8(aVal);
    {VT_DATE (TOleDate)}
    VT_DATE  : SetVT_DATE(VarToDateTime(aVal));
    {VT_BSTR (POleStr (PWideChar)}
    {VT_LPWSTR (PWideChar)}
    //VT_BSTR, VT_LPWSTR  :
    //  SetString(UTF8Encode(VarToStr(aVal)));//?????
    ////OutputDebugString(PChar(Format('%s',[SS])));
    ////SetString(UTF8Encode(WideCharToString(@widechar(VarToWideStr(aVal)[1])))); //????
    //
    //{VT_LPSTR (PChar)}
    //VT_LPSTR  : SetString(UTF8Encode(VarToStr(aVal))); //?????
    //UniqueString(Result);
    VT_BSTR, VT_LPWSTR, VT_LPSTR  : SetString(UTF8Encode(VarToStr(aVal))); //????
  end;
end;

///////////////////////////////////////////////////////////////
procedure TValue.SetVT_BOOL(const Val : WordBool);
begin
  if FType = VT_BOOL then
    WordBool(FValue^) := Val;
end;

function TValue.GetVT_BOOL : WordBool;
begin
  if FType = VT_BOOL then
    Result := WordBool(FValue^)
  else
    Result := False;
end;

procedure TValue.SetVT_I2(const Val : SmallInt);
begin
  if FType = VT_I2 then
    SmallInt(FValue^) := Val;
end;

function TValue.GetVT_I2 : SmallInt;
begin
  if FType = VT_I2 then
    Result := SmallInt(FValue^)
  else
    Result := 0;
end;

procedure TValue.SetVT_I4(const Val : LongInt);
begin
  if FType = VT_I4 then
    LongInt(FValue^) := Val;
end;

function TValue.GetVT_I4 : LongInt;
begin
  if FType = VT_I4 then
    Result := LongInt(FValue^)
  else
    Result := 0;
end;

procedure TValue.SetVT_I1(const Val : ShortInt);
begin
  if FType = VT_I1 then
    ShortInt(FValue^) := Val;
end;

function TValue.GetVT_I1 : ShortInt;
begin
  if FType = VT_I1 then
    Result := ShortInt(FValue^)
  else
    Result := 0;
end;

procedure TValue.SetVT_I8(const Val : Int64);
begin
  if FType = VT_I8 then
    Int64(FValue^) := Val;
end;

function TValue.GetVT_I8 : Int64;
begin
  if FType = VT_I8 then
    Result := Int64(FValue^)
  else
    Result := 0;
end;

procedure TValue.SetVT_UI1(const Val : Byte);
begin
  if FType = VT_UI1 then
    Byte(FValue^) := Val;
end;

function TValue.GetVT_UI1 : Byte;
begin
  if FType = VT_UI1 then
    Result := Byte(FValue^)
  else
    Result := 0;
end;

procedure TValue.SetVT_UI2(const Val : Word);
begin
  if FType = VT_UI2 then
    Word(FValue^) := Val;
end;

function TValue.GetVT_UI2 : Word;
begin
  if FType = VT_UI2 then
    Result := Word(FValue^)
  else
    Result := 0;
end;

procedure TValue.SetVT_UI4(const Val : LongWord);
begin
  if FType = VT_UI4 then
    LongWord(FValue^) := Val;
end;

function TValue.GetVT_UI4 : LongWord;
begin
  if FType = VT_UI4 then
    Result := LongWord(FValue^)
  else
    Result := 0;
end;

procedure TValue.SetVT_UI8(const Val : QWord);
begin
  if FType = VT_UI8 then
    QWord(FValue^) := Val;
end;

function TValue.GetVT_UI8 : QWord;
begin
  if FType = VT_UI8 then
    Result := QWord(FValue^)
  else
    Result := 0;
end;

procedure TValue.SetVT_CY(const Val : Currency);
begin
  if FType = VT_CY then
    Currency(FValue^) := Val;
end;

function TValue.GetVT_CY : Currency;
begin
  if FType = VT_CY then
    Result := Currency(FValue^)
  else
    Result := 0.0;
end;

procedure TValue.SetVT_R4(const Val : Single);
begin
  if FType = VT_R4 then
    Single(FValue^) := Val;
end;

function TValue.GetVT_R4 : Single;
begin
  if FType = VT_R4 then
    Result := Single(FValue^)
  else
    Result := 0.0;
end;

procedure TValue.SetVT_R8(const Val : Double);
begin
  if FType = VT_R8 then
    Double(FValue^) := Val;
end;

function TValue.GetVT_R8 : Double;
begin
  if FType = VT_R8 then
    Result := Double(FValue^)
  else
    Result := 0.0;
end;

procedure TValue.SetVT_DATE(const Val : TOleDate);
begin
  if FType = VT_DATE then
    TOleDate(FValue^) := Val;
end;

function TValue.GetVT_DATE : TOleDate;
begin
  if FType = VT_DATE then
    Result := TOleDate(FValue^)
  else
    Result := 0;
end;

{String
VT_BSTR (POleStr (PWideChar)
VT_LPSTR (PChar)
VT_LPWSTR (PWideChar)}
procedure TValue.SetString(const Val : String);
begin
  // FType in [VT_BSTR, VT_LPSTR, VT_LPWSTR]
  if FType in [VT_BSTR, VT_LPSTR, VT_LPWSTR] then
    FStr := Val;
end;

function TValue.GetString : String;
begin
  if FType in [VT_BSTR, VT_LPSTR, VT_LPWSTR] then
    Result := FStr
  else
    Result := '';
end;

function OPCTypToStr(ATyp : TVarType) : String;
begin
  Result := '';
  if (ATyp >= VT_VECTOR) and (ATyp < VT_ARRAY) then
    begin
    ATyp   := ATyp - VT_VECTOR;
    Result := 'VECTOR ';
    end
  else
    if (ATyp >= VT_ARRAY) and (ATyp < VT_BYREF) then
      begin
      ATyp   := ATyp - VT_ARRAY;
      Result := 'ARRAY ';
      end
    else
      if (ATyp >= VT_BYREF) and (ATyp < VT_RESERVED) then
        begin
        ATyp   := ATyp - VT_BYREF;
        Result := 'BYREF ';
        end;
  case ATyp of
    VT_EMPTY  : Result   := Result + 'EMPTY';
    VT_NULL  : Result    := Result + 'NULL';
    VT_I2  : Result      := Result + 'I2';
    VT_I4  : Result      := Result + 'I4';
    VT_R4  : Result      := Result + 'R4';
    VT_R8  : Result      := Result + 'R8';
    VT_CY  : Result      := Result + 'CY';
    VT_DATE  : Result    := Result + 'DATE';
    VT_BSTR  : Result    := Result + 'BSTR';
    VT_DISPATCH  : Result := Result + 'DISPATCH*';
    VT_ERROR  : Result   := Result + 'ERROR';
    VT_BOOL  : Result    := Result + 'BOOL';
    VT_VARIANT  : Result := Result + 'VARIANT';
    VT_UNKNOWN  : Result := Result + 'UNKNOWN';
    VT_I1  : Result      := Result + 'I1';
    VT_UI1  : Result     := Result + 'UI1';
    VT_UI2  : Result     := Result + 'UI2';
    VT_UI4  : Result     := Result + 'UI4';
    VT_I8  : Result      := Result + 'I8';
    VT_UI8  : Result     := Result + 'UI8';
    VT_INT  : Result     := Result + 'INT';
    VT_UINT  : Result    := Result + 'UINT';
    VT_VOID  : Result    := Result + 'VOID';
    VT_HRESULT  : Result := Result + 'RESULT';
    VT_PTR  : Result     := Result + 'PTR';
    VT_SAFEARRAY  : Result := Result + 'SAFEARRAY';
    VT_CARRAY  : Result  := Result + 'CARRAY';
    VT_USERDEFINED  : Result := Result + 'USERDEFINED';
    VT_LPSTR  : Result   := Result + 'LPSTR';
    VT_LPWSTR  : Result  := Result + 'LPWSTR';
    VT_FILETIME  : Result := Result + 'FILETIME';
    VT_BLOB  : Result    := Result + 'BLOB';
    VT_STREAM  : Result  := Result + 'STREAM';
    VT_STORAGE  : Result := Result + 'STORAGE';
    VT_STREAMED_OBJECT  : Result := Result + 'STREAMED_OBJECT';
    VT_STORED_OBJECT  : Result := Result + 'STORED_OBJECT';
    VT_BLOB_OBJECT  : Result := Result + 'BLOB_OBJECT';
    VT_CF  : Result      := Result + 'CF';
    VT_CLSID  : Result   := Result + 'CLSID';
    else
      Result := Result + Format('[Unknown type = %d]', [ATyp]);
    end;
end;

//function OPCTypToStr(ATyp : TVarType) : String; forward;

function TValue.GetTypName : String;
begin
  Result := Format('%s (%s)', [OPCTypToStr(FType), VarTypeAsText(FType)]);
end;

end.

































