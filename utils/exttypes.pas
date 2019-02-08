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
  TMemRecord = TStringList;

  { Набор значений строковых значений }
  TMemRecordSet = class(TList)
    public
      function GetRecord(Index: Integer): TMemRecord;

      property Records[Index: Integer]: TMemRecord read GetRecord;
  end;

  { Таблица строковых значений }
  TMemTableOfString = TMemRecordSet;

  { Вектор строковых значений }
  TMemVectorItem = class
    public
      datetime: AnsiString;
      value: AnsiString;
  end;

  TMemVectorOfString = class(TList)
    public
      function GetPoint(Index: Integer): TMemVectorItem;

      property Points[Index: Integer]: TMemVectorItem read GetPoint;
  end;

implementation

function TMemRecordSet.GetRecord(Index: Integer): TMemRecord;
begin
  Result := TMemRecord(Items[Index]^);
end;

function TMemVectorOfString.GetPoint(Index: Integer): TMemVectorItem;
begin
  Result := TMemVectorItem(Items[Index]^);
end;

end.

