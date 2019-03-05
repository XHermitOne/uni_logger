{
Расширенные типы для использования в программе

Версия: 0.0.2.1
}
unit exttypes;


{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs;


type
  { Массив строк }
  TArrayOfString = Array Of String;

  STRING_ARRAY = Array[0..65535] of String;
  PSTRING_ARRAY = ^STRING_ARRAY;

  { Массив вариантов }
  VARIANT_ARRAY = Array[0..65535] of Variant;
  PVARIANT_ARRAY = ^VARIANT_ARRAY;

  { Запись строковых значений }
  TMemRecord = class(TStringList)
    public
      {
      Принудительно установить длину записи
      @param iCount Количество полей записи
      }
      procedure SetLength(iCount: Integer);
  end;

  { Набор значений строковых значений }
  TMemRecordSet = class(TObjectList)
    public
      function GetRecord(Index: Integer): TMemRecord;

      property Records[Index: Integer]: TMemRecord read GetRecord;
  end;

  { Таблица строковых значений }
  TMemTableOfString = TMemRecordSet;

  { Вектор строковых значений }
  TMemVectorItem = class(TObject)
    public
      datetime: AnsiString;
      value: AnsiString;
  end;


  TMemVectorOfString = class(TObjectList)
    public
      function GetPoint(Index: Integer): TMemVectorItem;

      function AddNewPoint(sDateTime: AnsiString; sValue: AnsiString): Integer;
      { Вывести на экран все точки. Для отладки }
      procedure PrintPoints();

      property Points[Index: Integer]: TMemVectorItem read GetPoint;
  end;

implementation

uses
  log;

{
Принудительно установить длину записи
@param iCount Количество полей записи
}
procedure TMemRecord.SetLength(iCount: Integer);
var
  i: Integer;
begin
  if iCount > Count then
    for  i := Count to iCount - 1 do
      Add('')
  else
    for  i := Count - 1 downto iCount  do
      Delete(i);
end;

function TMemRecordSet.GetRecord(Index: Integer): TMemRecord;
begin
  Result := TMemRecord(Items[Index]);
end;

function TMemVectorOfString.GetPoint(Index: Integer): TMemVectorItem;
begin
  Result := TMemVectorItem(Items[Index]);
end;

{ Вывести на экран все точки. Для отладки }
procedure TMemVectorOfString.PrintPoints();
var
  i: Integer;
  point: TMemVectorItem;
begin
  for i := 0 to Count - 1 do
  begin
    point := GetPoint(i);
    log.ServiceMsgFmt('Точка вектора <%s : %s>', [point.datetime, point.value]);
  end;
end;

function TMemVectorOfString.AddNewPoint(sDateTime: AnsiString; sValue: AnsiString): Integer;
var
  new_point: TMemVectorItem;
begin
  new_point := TMemVectorItem.Create;
  new_point.datetime := sDateTime;
  new_point.value := sValue;
  Result := Add(new_point);
end;

end.

