{
Модуль обработчиков демона/службы
}
unit uni_daemonunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp;

type

  { TUniLoggerDaemon -   непосредственно экземпляр сервиса.
  Именно этот объект реализует сам сервис (его мы видим в Windows в «Управление компьютерами/Сервисы»).}
  TUniLoggerDaemon = class(TDaemon)
    { Обработчик после инсталяции }
    procedure DataModuleAfterInstall(Sender: TCustomDaemon);
    { Обработчик после деинсталяции }
    procedure DataModuleAfterUnInstall(Sender: TCustomDaemon);
    { Обработчик запуска демона/службы }
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    { Обработчик останова демона/службы }
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private

  public

  end;

var
  UniLoggerDaemon: TUniLoggerDaemon;

implementation

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TUniLoggerDaemon)
end;

{$R *.lfm}

{ TUniLoggerDaemon }

procedure TUniLoggerDaemon.DataModuleAfterInstall(Sender: TCustomDaemon);
begin
  WriteLn('The ' + Name + ' service is installing');
end;

procedure TUniLoggerDaemon.DataModuleAfterUnInstall(Sender: TCustomDaemon);
begin
  WriteLn('The ' + Name + ' service is deinstalling');
end;

procedure TUniLoggerDaemon.DataModuleStart(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  //engine.READER_ENGINE := TICReader.Create(nil);
  //engine.READER_ENGINE.StartServer;
end;

procedure TUniLoggerDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  //engine.READER_ENGINE.StopServer;
  //engine.READER_ENGINE.Free;
  //engine.READER_ENGINE := nil;
end;


initialization
  RegisterDaemon;
end.

