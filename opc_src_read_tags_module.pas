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
    ToEncodingComboBox: TComboBox;
    FromEncodingComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    StartButton: TButton;
    TagStringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);

    { Заполнение грида тегами }
    function RefreshTagList(aSettingsManager: settings.TICSettingsManager; aFromEncoding, aToEncoding: AnsiString): Boolean;

    procedure RefreshAll();

    procedure StartButtonClick(Sender: TObject);
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
begin
  RefreshAll();
end;

{ Заполнение грида тегами }
function TProgForm.RefreshTagList(aSettingsManager: settings.TICSettingsManager; aFromEncoding, aToEncoding: AnsiString): Boolean;
var
  i: Integer;
  tag_names: TStringList;
  opc: opc_server_node.TICOPCServerNode;
  address: AnsiString;
  option_name: AnsiString;
begin
  tag_names := aSettingsManager.GetOptionNameList('SPT_961');
  opc := opc_server_node.TICOPCServerNode.Create;

  for i := tag_names.Count - 1 downto 0  do
  begin
    option_name := tag_names[i];
    if option_name = 'opc_server' then
      opc.SetOPCServerName(aSettingsManager.GetOptionValue('SPT_961', option_name));
    if strfunc.IsStrInList(option_name, ['description', 'opc_server', 'type']) then
      tag_names.Delete(i);
  end;

  TagStringGrid.RowCount := tag_names.Count + 1;

  for i := 0 to tag_names.Count - 1 do
  begin
    option_name := tag_names[i];
    TagStringGrid.Cells[1, i+1] := option_name;
    address := aSettingsManager.GetOptionValue('SPT_961', option_name);
    address := strfunc.EncodeString(address, aFromEncoding, aToEncoding);
    TagStringGrid.Cells[2, i+1] := address;
    TagStringGrid.Cells[3, i+1] := opc.ReadAddress(address);
  end;

  opc.Free;
  tag_names.Free;
end;

procedure TProgForm.RefreshAll();
var
  settings_manager: settings.TICSettingsManager;
begin
  // Чтение списка читаемых тегов из INI файла
  settings_manager := settings.TICSettingsManager.Create;

  if settings_manager.LoadSettings(INI_FILENAME) then
  begin
    // Заполняем грид данными тегов
    RefreshTagList(settings_manager, FromEncodingComboBox.Text, ToEncodingComboBox.Text);
  end;

  settings_manager.Destroy();
end;


procedure TProgForm.StartButtonClick(Sender: TObject);
begin
  RefreshAll();
end;

end.

