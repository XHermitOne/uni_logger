{
Функции работы со строками
}
unit strfunc;

{$mode objfpc}{$H+}

interface

uses
    {$IFDEF windows}
    Windows,
    {$ENDIF}
    Classes, SysUtils, StrUtils,
    LConvEncoding;

type
    TArrayOfString = Array Of String;
{
Разбивает строку с разделителями на части и возвращает массив частей
@param sString Строка
@param cDelim Символ разделителя
@return Массив строк
}
function SplitStr(sString: String; cDelim: Char): TArrayOfString;

{
Объединение массива строк в одну строку
@param StringArray Массив строк
@param cDelim Символ разделителя
@return Объединенная строка
}
function JoinStr(StringArray : Array Of String; cDelim : Char): AnsiString;

{
Удалить обрамление кавычками одинарными и двойными из строки
@param sString Строка
@param sChar Обрамляющий символ
@return Строка с удаленными обрамляющими симвлами
}
function StripStr(sString: AnsiString; sChar: Char = ' '): AnsiString;

{
Распарсить строку, представляющую собой список строк
@param sString Обрабатываемая строка
@return Массив строк
}
function ParseStrArray(sString: AnsiString): TArrayOfString;

{
Проверка является ли строка серилизованным списком
@param sString Строка
@return True/False
}
function IsParseStrList(sString: AnsiString): Boolean;

{
Распарсить строку, представляющую собой список строк. Возврашает список строк.
@param sString Строка
@return Список строк
}
function ParseStrList(sString: AnsiString): TStringList;

{
Преобразование списка строк в строку вида [ aaaa, bbbb, cccc ]
@param StrList Спсок строк
@return Преобразованная строка
}
function ConvertStrListToString(StrList: TStringList): AnsiString;

{
Проверка есть ли строка в списке строк
@param sString Строка
@param StringArray Массив строк
@return True/False
}
function IsStrInList(sString: AnsiString; StringArray: Array of String): Boolean;

{ Перекодировка CP866 -> CP1251 и обратно }
{$IFDEF windows}
function StrOemToAnsi(const S: AnsiString): AnsiString;
function StrAnsiToOem(const S: AnsiString): AnsiString;
{$ENDIF}

{
Попытка перекодировать строку в UTF8. Функция сама определеяет кодировку строки и
затем преобразует в UTF8.
@param sStr Исходная строка
@return Преобразованная строка в UTF8
}
function ToUTF8(var sStr: AnsiString): AnsiString;

implementation

{
Разбивает строку с разделителями на части
и возвращает массив частей.
}
function SplitStr(sString: String; cDelim: Char): TArrayOfString;
var
  iCounter, iBegin: Integer;
begin
     if Length(sString) > 0 then
     begin
          //Include(cDelim, #0);
          iBegin := 1;
          SetLength(result, 0);

          for iCounter := 1 to Length(sString) + 1 do
          begin
               if (sString[iCounter] = cDelim) or (iCounter = Length(sString)) then
               begin
                    SetLength(result, Length(result) + 1);
                    result[Length(result) - 1] := Copy(sString, iBegin, iCounter - iBegin + 1);
                    iBegin := iCounter + 1;
               end;
          end;//for
     end;//if
end;

{
Объединение массива строк в одну строку.
}
function JoinStr(StringArray: Array Of String; cDelim: Char): AnsiString;
var
  i : Integer;
begin
  result := '';
  for i := Low(StringArray) to High(StringArray) do
    result := result + StringArray[i] + cDelim;
  Delete(result, Length(result), 1);
end;

{
Удалить обрамление кавычками одинарными и двойными из строки.
}
function StripStr(sString: AnsiString; sChar: Char): AnsiString;
begin
     result := sString;
     if AnsiStartsStr(sChar, sString) then
        result := Copy(sString, 2, Length(sString) - 1);
     if AnsiEndsStr(sChar, sString) then
        result := Copy(sString, 1, Length(sString) - 1);

     if AnsiStartsStr(sChar, sString) or AnsiEndsStr(sChar, sString) then
        result := StripStr(result, sChar);
end;

{
Распарсить строку, представляющую собой список строк
Например:
  [ aaaa, bbbb, cccc ] или ['aaa', 'bbb', 'ccc']
}
function ParseStrArray(sString: AnsiString): TArrayOfString;
var
  i: Integer;
  result_list: Array Of String;
begin
     if AnsiStartsStr('[', sString) then
        sString := Copy(sString, 2, Length(sString) - 1);
     if AnsiEndsStr(']', sString) then
        sString := Copy(sString, 1, Length(sString) - 1);
     result_list := SplitStr(sString, ',');
     for i := 0 to Length(result_list) - 1 do
     begin
          result_list[i] := StripStr(result_list[i], ' ');
          result_list[i] := StripStr(result_list[i], '''');
          result_list[i] := StripStr(result_list[i], '"');
     end;
     result := result_list;
end;

{
Проверка является ли строка серилизованным списком
}
function IsParseStrList(sString: AnsiString): Boolean;
begin
     result := AnsiStartsStr('[', sString) and  AnsiEndsStr(']', sString);
end;

{
Распарсить строку, представляющую собой список строк
Например:
  [ aaaa, bbbb, cccc ] или ['aaa', 'bbb', 'ccc']
Возврашает список строк.
}
function ParseStrList(sString: AnsiString): TStringList;
var
  str_array: Array Of String;
  i: Integer;
  str_list: TStringList;
begin
     str_list := TStringList.Create;
     str_array := ParseStrArray(sString);
     for i := 0 to Length(str_array) - 1 do
         str_list.Add(str_array[i]);
     result := str_list;
end;

{
Преобразование списка строк в строку вида:
[ aaaa, bbbb, cccc ]
}
function ConvertStrListToString(StrList: TStringList): AnsiString;
begin
  // ExtractStrings([','], [' '], PChar(result), StrList);
  result := '[' + Trim(StrList.Text) + ']';
end;

{ Проверка есть ли строка в списке строк }
function IsStrInList(sString: AnsiString; StringArray: Array of String): Boolean;
var
  i: Integer;
begin
  result := False;
  for i := 0 to Length(StringArray) - 1 do
    if StringArray[i] = sString then
    begin
         result := True;
         exit;
    end;
end;

{$IFDEF windows}
function StrOemToAnsi(const S: AnsiString): AnsiString;
begin
  if Length(S) = 0 then Result := ''
  else
    begin
      SetLength(Result, Length(S));
      OemToAnsiBuff(@S[1], @Result[1], Length(S));
    end;
end;
{$ENDIF windows}

//--------------------------------------------------------------------------------------------------

{$IFDEF windows}
function StrAnsiToOem(const S: AnsiString): AnsiString;
begin
  if Length(S) = 0 then Result := ''
  else
    begin
      SetLength(Result, Length(S));
      AnsiToOemBuff(@S[1], @Result[1], Length(S));
    end;
end;
{$ENDIF windows}

{
Попытка перекодировать строку в UTF8
}
function ToUTF8(var sStr: AnsiString): AnsiString;
var
  from: string;
begin
  from := GuessEncoding(sStr);
  Result := ConvertEncoding(sStr, from, EncodingUTF8);
end;

end.

