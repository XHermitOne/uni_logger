{
Модуль класса доступа к таблице PostgreSQL для записи значений тегов.
}
unit postgresql_tab_wide;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    obj_proto, dictionary, strfunc,
    tag_list;

const
  RESERV_PROPERTIES: Array [1..6] Of String = ('type', 'name', 'description', 'db_name', 'db_port', 'db_username', 'db_password', 'table_name');

type
  {
  Класс доступа к таблице PostgreSQL для записи значений тегов.
  }
  TICPostgreSQLTableWide = class(TICObjectProto)

  private

  public
    constructor Create;
    procedure Free;

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

    { Фунция чтения данных }
    function Read(aValues: TStringList): TStringList; override;
    { Фунция записи данных }
    function Write(aValues: TStringList): Boolean; override;


end;

implementation

uses
    LCLIntf, // Для вычисления времени выполнения
    log;

constructor TICPostgreSQLTableWide.Create;
begin
     inherited Create;
end;

procedure TICPostgreSQLTableWide.Free;
begin
  inherited Free;
end;

{
Фунция чтения данных
}
function TICPostgreSQLTableWide.Read(aValues: TStringList): TStringList;
begin
  Result := nil;
end;

{
Фунция записи данных
}
function TICPostgreSQLTableWide.Write(aValues: TStringList): Boolean;
begin
  Result := False;
end;

{ Выбрать описания тегов из свойств }
function TICPostgreSQLTableWide.CreateTags(): TStrDictionary;
var
  i: Integer;
  key, value: AnsiString;
  tags: TStrDictionary;
begin
  tags := TStrDictionary.Create;
  for i := 0 to Properties.Count - 1 do
  begin
    key := Properties.GetKey(i);
    if not IsStrInList(key, RESERV_PROPERTIES) then
    begin
      value := Properties.GetStrValue(key);
      tags.AddStrValue(key, value);
    end;
  end;
  result := tags;
end;

end.

