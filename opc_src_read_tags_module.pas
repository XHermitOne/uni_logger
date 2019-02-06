unit opc_src_read_tags_module;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls,
  settings;

const
  INI_FILENAME: AnsiString = 'settings.ini';

type

  { TProgForm }

  TProgForm = class(TForm)
    StartButton: TButton;
    TagStringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);

    { Заполнение грида тегами }
    function RefreshTagList(aSettingsManager: settings.TICSettingsManager): Boolean;
  private

  public

  end;

var
  ProgForm: TProgForm;

implementation

uses
  log, strfunc, opc_server_node;

{$R *.lfm}

{ TProgForm }

procedure TProgForm.FormCreate(Sender: TObject);
var
  settings_manager: settings.TICSettingsManager;
begin
  // Чтение списка читаемых тегов из INI файла
  settings_manager := settings.TICSettingsManager.Create;

  if settings_manager.LoadSettings(INI_FILENAME) then
  begin
    // Заполняем грид данными тегов
    RefreshTagList(settings_manager);
  end;

  settings_manager.Destroy();
end;

{ Заполнение грида тегами }
function TProgForm.RefreshTagList(aSettingsManager: settings.TICSettingsManager): Boolean;
var
  i: Integer;
  tag_names: TStringList;
  opc: opc_server_node.TICOPCServerNode;
  address: AnsiString;
begin
  tag_names := aSettingsManager.GetOptionNameList('SPT_961');
  opc := opc_server_node.TICOPCServerNode.Create;

  for i := tag_names.Count - 1 downto 0  do
    if strfunc.IsStrInList(tag_names[i], ['description', 'opc_server']) then
      tag_names.Delete(i)
    else if tag_names[i] = 'opc_server' then
      opc.SetOPCServerName(tag_names[i]);

  TagStringGrid.RowCount := tag_names.Count;

  for i := 0 to tag_names.Count - 1 do
  begin
    TagStringGrid.Cells[1, i] := tag_names[i];
    address := aSettingsManager.GetOptionValue('SPT_961', tag_names[i]);
    TagStringGrid.Cells[2, i] := address;
    TagStringGrid.Cells[3, i] := opc.ReadAddress(address);
  end;

  opc.Free;
  tag_names.Free;
end;

end.

