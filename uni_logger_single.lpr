{
Консольный режим работы программы UniLogger.
Этот вид запуска предназначен для вызова в шедулере по расписанию.

Версия 0.0.1.1

@bold(Поиск утечек памяти)

Включение поиска утечек:
Меню Lazarus -> Проект -> Параметры проекта... ->
Параметры проекта -> Отладка -> Выставить галки для ключей -gl и -gh

Вывод делаем в текстовый файл *.mem в:

@longcode(#
***********************************************************
if UseHeapTrace then     // Test if reporting is on
   SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.mem'));
***********************************************************
#)

Допустим, имеем код, который заведомо без утечек:

@longcode(#
***********************************************************
uses heaptrc;
var
  p1, p2, p3: pointer;

begin
  getmem(p1, 100);
  getmem(p2, 200);
  getmem(p3, 300);

  // ...

  freemem(p3);
  freemem(p2);
  freemem(p1);
end.
***********************************************************
#)

, после запуска и завершения работы программы, в консоли наблюдаем отчет:

@longcode(#
***********************************************************
Running "f:\programs\pascal\tst.exe "
Heap dump by heaptrc unit
3 memory blocks allocated : 600/608
3 memory blocks freed     : 600/608
0 unfreed memory blocks : 0
True heap size : 163840 (80 used in System startup)
True free heap : 163760
***********************************************************
#)

Утечек нет, раз "0 unfreed memory blocks"
Теперь внесем утечку, "забудем" вернуть память выделенную под p2:

@longcode(#
***********************************************************
uses heaptrc;
var
  p1, p2, p3: pointer;

begin
  getmem(p1, 100);
  getmem(p2, 200);
  getmem(p3, 300);

  // ...

  freemem(p3);
  // freemem(p2);
  freemem(p1);
end.
***********************************************************
#)

и смотрим на результат:

@longcode(#
***********************************************************
Running "f:\programs\pascal\tst.exe "
Heap dump by heaptrc unit
3 memory blocks allocated : 600/608
2 memory blocks freed     : 400/408
1 unfreed memory blocks : 200
True heap size : 163840 (80 used in System startup)
True free heap : 163488
Should be : 163496
Call trace for block $0005D210 size 200
  $00408231
***********************************************************
#)

200 байт - утечка...
Если будешь компилировать еще и с ключом -gl,
то ко всему прочему получишь и место, где была выделена "утекающая" память.

ВНИМАНИЕ! Если происходят утечки памяти в модулях Indy
необходимо в C:\lazarus\fpc\3.0.4\source\packages\indy\IdCompilerDefines.inc
добавить @code($DEFINE IDFREEONFINAL) в секции FPC (2+)
и перекомпилировать проект.
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

  // Учет утечек памяти. Вывод делаем в текстовый файл *.mem
  //{$if declared(UseHeapTrace)}
  //if UseHeapTrace then // Test if reporting is on
  //   SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.mem'));
  //{$ifend}
end.

