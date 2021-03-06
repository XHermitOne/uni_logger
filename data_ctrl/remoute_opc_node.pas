{
Модуль удаленного узла OPC сервера

Версия: 0.0.1.2
}

unit remoute_opc_node;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, ActiveX,
    obj_proto, dictionary, strfunc,
    opc_client, tag_list;

const
  REMOUTE_OPC_NODE_TYPE: AnsiString = 'REMOUTE_OPC_DA';
  RESERV_PROPERTIES: Array [1..6] Of String = ('type', 'name', 'description', 'opc_server', 'opc_host', 'topic');

type
  {
  Класс взаимодействия с удаленным OPC сервером.
  }
  TICRemouteOPCNode = class(TICObjectProto)

  private
    { Объект OPC клиента }
    FOPCClient: TOPCClient;

  public
    constructor Create;
    procedure Free;

    { Выбрать описания тегов из свойств }
    function CreateTags(): TStrDictionary;

end;

implementation

uses
  LCLIntf, // Для вычисления времени выполнения
  log;

constructor TICRemouteOPCNode.Create;
begin
  inherited Create;
  FOPCClient := nil;
end;

procedure TICRemouteOPCNode.Free;
begin
  if FOPCClient <> nil then
    FOPCClient.Destroy;
  inherited Free;
end;

{ Выбрать описания тегов из свойств }
function TICRemouteOPCNode.CreateTags(): TStrDictionary;
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

