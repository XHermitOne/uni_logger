{
@bold(Поиск утечек памяти)

Включение поиска утечек:
Меню Lazarus -> Проект -> Параметры проекта... ->
Параметры компилятора -> Отладка -> Выставить галки для ключей -gl и -gh

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

Program uni_logger;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  {
  ПРИМЕЧАНИЕ: Если при компиляции проект не находит компоненты
              Ошибка компиляции:
              Error: Undefined symbol: WSRegisterCustomImageList ... и т.п.
              то необходимо в секцию uses необходимо добавить модуль Interfaces
  }
  DaemonApp, EventLog, SysUtils, Interfaces,
  lazdaemonapp, uni_daemonmapperunit, uni_daemonunit,
  engine, log, settings
  { add your units here };

begin
  { Чтение параметров коммандной строки }
  if 
  Application.Title:='Daemon application';
 Application.HasOption('T', 'test') then
    { Если указан режим тестирования, то запоминаем его в переменной }
    engine.TEST_SERVICE_MODE := True;
  if Application.HasOption('t', 'tick') then
    try
      { Если указан временной интервал обработки, то запоминаем его в переменной }
      engine.TIMER_TICK := StrToInt(Application.GetOptionValue('t', 'tick'));
    except
      log.FatalMsg('Ошибка параметра коммандной строки. Время интервала обработки');
    end;
  if Application.HasOption('l', 'log') then
  begin
    log.LOG_MODE := True;
    log.APP_LOG_MODE := True;
  end;
  if Application.HasOption('s', 'settings') then
    settings.SETTINGS_INI_FILENAME := Application.GetOptionValue('s', 'settings');

  // Запуск по умолчанию
  // vvvvvvvvvvvvvvvvvvvvvvv
  // Application.Initialize;
  // Application.Run;
  // ^^^^^^^^^^^^^^^^^^^^^^^^
  with Application do
   begin
     Title := 'UniLoggerService Daemon';
     { Указываем режим журналирования в log файл }
     if APP_LOG_MODE then
     begin
       EventLog.LogType := ltFile;
       EventLog.DefaultEventType := etDebug;
       EventLog.AppendContent := True;
       { Имя файла журнала }
       EventLog.FileName := ChangeFileExt(ParamStr(0), '.log');
     end;

     Initialize;
     Run;
   end;

  // Учет утечек памяти. Вывод делаем в текстовый файл *.mem
  {$if declared(UseHeapTrace)}
  if UseHeapTrace then // Test if reporting is on
     SetHeapTraceOutput(ChangeFileExt(ParamStr(0), '.mem'));
  {$ifend}
end.
