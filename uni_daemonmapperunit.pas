{
Модуль маппера демона/службы

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
unit uni_daemonmapperunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtCtrls, DaemonApp,
  engine, log;

type

  { TUniLoggerDaemonMapper - это компонент, отвечающий за управление работой сервисов.
  Особенно актуален он в случае включения в один исполняемый модуль нескольких сервисов.}
  TUniLoggerDaemonMapper = class(TDaemonMapper)
    {
    Обработчик выполнения инсталяции демона/службы

    ВНИМАНИЕ! Здесь добавляем дополнительные ключи
    Дополнительные ключи укаазываются при инсталляции службы:

    uni_logger.exe --test --install

    с помощью RunArguments этот ключ переносится в комманду запуска службы:

    uni_logger.exe --run --test
    }
    procedure UniLoggerDaemonMapperInstall(Sender: TObject);
  private

  public

  end;

var
  UniLoggerDaemonMapper: TUniLoggerDaemonMapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TUniLoggerDaemonMapper)
end;

{$R *.lfm}

{ TUniLoggerDaemonMapper }

procedure TUniLoggerDaemonMapper.UniLoggerDaemonMapperInstall(Sender: TObject);
var
  i: Integer;
begin
  // Перекодируем описания из utf-8 в кодовую страницу Windows
  // для корректного отображения в списке служб
  for i := 0 to DaemonDefs.Count - 1 do
  begin
    DaemonDefs[i].Description := log.EncodeUnicodeString(DaemonDefs[i].Description, 'cp1251');
  end;

  { ВНИМАНИЕ! Здесь добавляем дополнительные ключи
  Дополнительные ключи укаазываются при инсталляции службы:
  uni_logger.exe --test --install
  с помощью RunArguments этот ключ переносится в комманду запуска службы:
  uni_logger.exe --run --test}
  if engine.TEST_SERVICE_MODE then
    DaemonDefs[0].RunArguments := '--test';
  if engine.TIMER_TICK <> engine.DEFAULT_TIMER_TICK then
    DaemonDefs[0].RunArguments := DaemonDefs[0].RunArguments + ' ' + Format('--tick=%d', [engine.TIMER_TICK]);
  log.DebugMsgFmt('Параметры командной строки <%s>', [DaemonDefs[0].RunArguments]);
end;


initialization
  RegisterMapper;
end.

