{
Консольный режим работы программы UniLogger.
Этот вид запуска предназначен для вызова в шедулере по расписанию.

Версия 0.0.1.1
}

program uni_logger_single;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {
  ПРИМЕЧАНИЕ: Если при компиляции проект не находит компоненты
              Ошибка компиляции:
              Error: Undefined symbol: WSRegisterCustomImageList ... и т.п.
              то необходимо в секцию uses необходимо добавить модуль Interfaces
  }
  Interfaces,
  Classes, SysUtils, CustApp,
  engine
  { you can add units after this };

type

  { TUniLoggerSingleApplication }

  TUniLoggerSingleApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TUniLoggerSingleApplication }

procedure TUniLoggerSingleApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  // Инициализируем объект движка и всех его внутренних объектов
  engine.LOGGER_ENGINE := TICLogger.Create(nil);
  engine.LOGGER_ENGINE.Start;

  // Выполнить обработчик одного тика
  if (engine.LOGGER_ENGINE <> nil) then
    engine.LOGGER_ENGINE.Tick;

  // Корректно завершаем работу движка
  engine.LOGGER_ENGINE.Stop;
  // ВНИМАНИЕ! Вместо Free я вызываю Destroy.
  // Предположительно из-за этого на останавливается корректно служба
  //engine.LOGGER_ENGINE.Free;
  engine.LOGGER_ENGINE.Destroy;
  engine.LOGGER_ENGINE := nil;

  // stop program loop
  Terminate;
end;

constructor TUniLoggerSingleApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TUniLoggerSingleApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TUniLoggerSingleApplication.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExeName, ' -h');
end;

var
  Application: TUniLoggerSingleApplication;
begin
  Application := TUniLoggerSingleApplication.Create(nil);
  Application.Title := 'UniLogger single mode';
  Application.Run;
  Application.Free;
end.

