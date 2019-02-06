program opc_src_read_tags;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  opc_src_read_tags_module
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;

  Application.Initialize;
  Application.CreateForm(TProgForm, ProgForm);
  Application.Run;
end.

