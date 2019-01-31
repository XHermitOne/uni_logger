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
  log;

type

  { TUniLoggerDaemonMapper - это компонент, отвечающий за управление работой сервисов.
  Особенно актуален он в случае включения в один исполняемый модуль нескольких сервисов.}
  TUniLoggerDaemonMapper = class(TDaemonMapper)
    {
    Обработчик выполнения инсталяции демона/службы
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

  { ВНИМАНИЕ! Здесь добавляем ключи для запуска прослушки не стандартного порта
  Прослушиваемый порт указывается при инсталляции службы:
  uni_logger.exe --port=8081 --install
  с помощью RunArguments этот ключ переносится в комманду запуска службы:
  uni_logger.exe --run --port=8081}
  //if engine.XML_RPC_PORT <> DEFAULT_XML_RPC_PORT then
  //  DaemonDefs[0].RunArguments := Format('--port=%d', [engine.XML_RPC_PORT]);
  log.DebugMsgFmt('Параметры командной строки <%s>', [DaemonDefs[0].RunArguments]);
end;


initialization
  RegisterMapper;
end.

