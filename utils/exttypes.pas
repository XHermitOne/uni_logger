{
Расширенные типы для использования в программе

Версия: 0.0.1.1
}
unit exttypes;


{$mode objfpc}{$H+}

interface

uses
  Classes;


type
  { Массив строк }
  TArrayOfString = Array Of String;

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
  TMemRecordSet = class(TList)
    public
      function GetRecord(Index: Integer): TMemRecord;

      property Records[Index: Integer]: TMemRecord read GetRecord;
  end;

  { Таблица строковых значений }
  TMemTableOfString = TMemRecordSet;

  { Вектор строковых значений }
  TMemVectorItem = class(TCollectionItem)
    public
      datetime: AnsiString;
      value: AnsiString;
  end;

  TMemVectorOfString = class(TCollection)
    public
      function GetPoint(Index: Integer): TMemVectorItem;

      procedure AddNewPoint(sDateTime: AnsiString; sValue: AnsiString);
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
  Result := TMemRecord(Items[Index]^);
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

procedure TMemVectorOfString.AddNewPoint(sDateTime: AnsiString; sValue: AnsiString);
var
  new_point: TMemVectorItem;
begin
  new_point := Add();
  // new_point := TMemVectorItem.Create(Self);
  new_point.datetime := sDateTime;
  new_point.value := sValue;
end;

end.

