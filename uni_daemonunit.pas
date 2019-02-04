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
  Classes, SysUtils, FileUtil, DaemonApp, crt,
  log, engine, ExtCtrls;

type

  { TUniLoggerDaemon -   непосредственно экземпляр сервиса.
  Именно этот объект реализует сам сервис (его мы видим в Windows в «Управление компьютерами/Сервисы»).}
  TUniLoggerDaemon = class(TDaemon)
    TickTimer: TTimer;
    { Обработчик после инсталяции }
    procedure DataModuleAfterInstall(Sender: TCustomDaemon);
    { Обработчик после деинсталяции }
    procedure DataModuleAfterUnInstall(Sender: TCustomDaemon);
    { Обработчик запуска демона/службы }
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    { Обработчик останова демона/службы }
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
    procedure TickTimerTimer(Sender: TObject);
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
  log.InfoMsgFmt('Служба %s проинсталирована', [Definition.DisplayName], True);
end;

procedure TUniLoggerDaemon.DataModuleAfterUnInstall(Sender: TCustomDaemon);
begin
  log.InfoMsgFmt('Служба %s деинсталирована', [Definition.DisplayName], True);
end;

procedure TUniLoggerDaemon.DataModuleStart(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  // Инициализируем объект движка и всех его внутренних объектов
  engine.LOGGER_ENGINE := TICLogger.Create(nil);
  engine.LOGGER_ENGINE.Start;

  // Запускаем таймер отсчета тиков
  TickTimer.Interval := engine.TIMER_TICK;
  log.InfoMsgFmt('Установлен временной интервал таймера обработки <%d> миллисекунд', [TickTimer.Interval]);
  TickTimer.Enabled := True;
end;

procedure TUniLoggerDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  // Останавливаем таймер отсчета тиков
  TickTimer.Enabled := False;

  // Корректно завершаем работу движка
  engine.LOGGER_ENGINE.Stop;
  engine.LOGGER_ENGINE.Free;
  engine.LOGGER_ENGINE := nil;
end;

{ Обработчик одного тика таймера }
procedure TUniLoggerDaemon.TickTimerTimer(Sender: TObject);
begin
  // Выполнить обработчик одного тика
  if not engine.LOGGER_ENGINE.IsTick then
    engine.LOGGER_ENGINE.Tick
  else
    log.WarningMsgFmt('Пропущена обработка тика в %s', [FormatDateTime('c', Now())]);
end;


initialization
  RegisterDaemon;
end.

