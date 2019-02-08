{
����������� ���� ��� ������������� � ���������

������: 0.0.1.1
}
unit exttypes;


{$mode objfpc}{$H+}

interface

uses
  Classes;


type
  { ������ ����� }
  TArrayOfString = Array Of String;

  { ������ ��������� �������� }
  TMemRecord = TStringList;

  { ����� �������� ��������� �������� }
  TMemRecordSet = class(TList)
    public
      function GetRecord(Index: Integer): TMemRecord;

      property Records[Index: Integer]: TMemRecord read GetRecord;
  end;

  { ������� ��������� �������� }
  TMemTableOfString = TMemRecordSet;

  { ������ ��������� �������� }
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

