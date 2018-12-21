Program uni_logger;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, uni_daemonmapperunit, uni_daemonunit
  { add your units here };

begin
  Application.Initialize;
  Application.Run;
end.
