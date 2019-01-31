{
Модуль обработчиков демона/службы

Памятка:
'uni_logger.exe --install' - Производим инсталляцию службы
'uni_logger.exe --run' - Запуск службы
'net start UniLoggerService' - Запуск службы
'net stop UniLoggerService' - Останов службы
'uni_logger.exe --uninstall' - Деинсталяция службы
Все команды должны производиться из под Администратора. Для этого
открываем консоль (cmd.exe) с правами Администратора и
в ней выполняем эти комманды.
}
unit uni_daemonunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp,
  log, engine;

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
  // WriteLn('The ' + Name + ' service is installing');
  log.InfoMsg('Служба ' + Name + ' проинсталирована');
end;

procedure TUniLoggerDaemon.DataModuleAfterUnInstall(Sender: TCustomDaemon);
begin
  // WriteLn('The ' + Name + ' service is deinstalling');
  log.InfoMsg('Служба ' + Name + ' деинсталирована');
end;

procedure TUniLoggerDaemon.DataModuleStart(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  engine.LOGGER_ENGINE := TICLogger.Create(nil);
  engine.LOGGER_ENGINE.Start;
end;

procedure TUniLoggerDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  engine.LOGGER_ENGINE.Stop;
  engine.LOGGER_ENGINE.Free;
  engine.LOGGER_ENGINE := nil;
end;


initialization
  RegisterDaemon;
end.

