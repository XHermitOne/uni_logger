{
Функции журналирования.

Цветовая расскраска сообщений в коммандной оболочке
производиться только под Linux.
Для Windows систем цветовая раскраска отключена.

Шаблон для использования в современных
командных оболочках и языках
программирования таков: \x1b[...m.
Это ESCAPE-последовательность,
где \x1b обозначает символ ESC
(десятичный ASCII код 27), а вместо "..."
подставляются значения из таблицы,
приведенной ниже, причем они могут
комбинироваться, тогда нужно их
перечислить через точку с запятой.

атрибуты

0 	нормальный режим

1 	жирный

4 	подчеркнутый

5 	мигающий

7 	инвертированные цвета

8 	невидимый

цвет текста

30 	черный

31 	красный

32 	зеленый

33 	желтый

34 	синий

35 	пурпурный

36 	голубой

37 	белый

цвет фона

40 	черный

41 	красный

42 	зеленый

43 	желтый

44 	синий

45 	пурпурный

46 	голубой

47 	белый

Версия: 0.0.5.3

ВНИМАНИЕ! Вывод сообщений под Linux проверять только в терминале.
Только он выводит корректно сообщения.
}

unit log;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    { Для функций перекодировки UTF8ToWinCP }
    LazUTF8,
    DaemonApp,
    sysfunc;

const
  DEFAULT_LOG_FILENAME: AnsiString = 'uni_reader.log';

  { Цвета в консоли Linux }
  RED_COLOR_TEXT: AnsiString = Chr($1b) + '[31;1m';       // red
  GREEN_COLOR_TEXT: AnsiString = Chr($1b) + '[32m';       // green
  YELLOW_COLOR_TEXT: AnsiString = Chr($1b) + '[33;1m';    // yellow
  BLUE_COLOR_TEXT: AnsiString = Chr($1b) + '[34m';        // blue
  PURPLE_COLOR_TEXT: AnsiString = Chr($1b) + '[35m';      // purple
  CYAN_COLOR_TEXT: AnsiString = Chr($1b) + '[36m';        // cyan
  WHITE_COLOR_TEXT: AnsiString = Chr($1b) + '[37m';       // white
  NORMAL_COLOR_TEXT: AnsiString = Chr($1b) + '[0m';       // normal

  { Режим отладки по умолчанию. Позволяет принудительно установить режим в обход параметров из окружения }
  DEFAULT_DEBUG_MODE: Boolean = True;
  { Режим журналирования по умолчанию. Позволяет принудительно установить режим в обход параметров из окружения }
  DEFAULT_LOG_MODE: Boolean = False;

  { Режим журналирования Application.Log. Включается для служб Windows }
  DEFAULT_APP_LOG_MODE = True;


{
Определить актуальную кодировку для вывода текста.
@return Актуальная кодировка для вывода текста
}
function GetDefaultEncoding(): AnsiString;
{Определить включен ли режим отладки}
function GetDebugMode(): Boolean;
{Определить включен ли режим журналирования}
function GetLogMode(): Boolean;

{
Перекодирование AnsiString строки в AnsiString.
@param sTxt Текст в AnsiString
@param sCodePage Указание кодировки
@return Перекодированный текст
}
function EncodeUnicodeString(sTxt: AnsiString; sCodePage: AnsiString = 'utf-8'): AnsiString;
{
Печать цветового текста
@param sTxt Печатаемый текст
@param sColor Дополнительное указание цветовой раскраски
}
procedure PrintColorTxt(sTxt: AnsiString; sColor: AnsiString);

{
Инициализация файла лога.
@param sLogFileName Имя файла лога. Если имя файла не определено, то пробуем его взять из оружения системы: Ключ LOG_FILENAME
}
function OpenLog(sLogFileName: AnsiString = ''): boolean;
{ Закрыть файл лога. }
function CloseLog(): boolean;
{
Регистрация сообщения в файле лога.
@param sMsg Регистрируемое сообщение
@param bForceLog Признак принудительной регистрации
}
function LogMsg(sMsg: AnsiString = ''): boolean;

{
Вывести ОТЛАДОЧНУЮ информацию.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure DebugMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
{
Вывести ТЕКСТОВУЮ информацию.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure InfoMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
{
Вывести информацию об ОШИБКЕ.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure ErrorMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
{
Вывести ПРЕДУПРЕЖДЕНИЕ.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure WarningMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
{
Вывести СООБЩЕНИЕ об ИСКЛЮЧИТЕЛЬНОЙ СИТУАЦИИ.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure FatalMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);
{
Вывести СЕРВИСНУЮ информацию.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure ServiceMsg(sMsg: AnsiString; bForcePrint: boolean = False; bForceLog: boolean = False);

{
Вывести ОТЛАДОЧНУЮ информацию с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure DebugMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean = False; bForceLog: Boolean = False);
{
Вывести текстовую ИНФОРМАЦИЮ с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure InfoMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean = False; bForceLog: Boolean = False);
{
Вывести СЕРВИСНУЮ информацию с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure ServiceMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean = False; bForceLog: Boolean = False);
{
Вывести информацию об ОШИБКЕ с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure ErrorMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean = False; bForceLog: Boolean = False);
{
Вывести ПРЕДУПРЕЖДЕНИЕ с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure WarningMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean = False; bForceLog: Boolean = False);
{
Вывести СООБЩЕНИЕ об ИСКЛЮЧИТЕЛЬНОЙ СИТУАЦИИ с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure FatalMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean = False; bForceLog: Boolean = False);

var
    {
    Объявление глобального объекта файла журнала регистрации сообщений программы

    ВНИМАНИЕ! Глобальные переменные описываются в секции interface.
    Переменные определенные в секции implementation являются статическими для
    модуля.
    }
    LOG_FILE: Text;
    IS_OPEN_LOG_FILE: Boolean = False;


implementation

uses
    filefunc;

{
Определить актуальную кодировку для вывода текста.
@return Актуальная кодировка для вывода текста
}
function GetDefaultEncoding(): AnsiString;
begin
    // writeln(DefaultSystemCodePage, ' ', CP_UTF8);
    result := 'utf-8';
    if sysfunc.IsOSWindows() then
        result := 'cp866';
end;

{
Определить включен ли режим отладки
}
function GetDebugMode(): Boolean;
begin
  if DEFAULT_DEBUG_MODE then
    Result := True
  else
    Result := False; // config.ENVIRONMENT.HasKey('DEBUG_MODE');

  if not Result then
    PrintColorTxt('Режим отладки отключен', YELLOW_COLOR_TEXT);
end;

{
Определить включен ли режим журналирования
}
function GetLogMode(): Boolean;
begin
  if DEFAULT_LOG_MODE then
    Result := IS_OPEN_LOG_FILE
  else
    Result := False; // config.ENVIRONMENT.HasKey('LOG_MODE') and IS_OPEN_LOG_FILE;

  if not Result and not DEFAULT_APP_LOG_MODE then
    PrintColorTxt('Режим журналирования отключен', YELLOW_COLOR_TEXT);

end;

{
Перекодирование AnsiString строки в AnsiString.
@param sTxt Текст в AnsiString
@param sCodePage Указание кодировки
@return Перекодированный текст
}
function EncodeUnicodeString(sTxt: AnsiString; sCodePage: AnsiString): AnsiString;
begin
    result := '';
    if (sCodePage = 'utf-8') or (sCodePage = 'UTF-8') or (sCodePage = 'utf8') or (sCodePage = 'UTF8') then
    begin
        // ВНИМАНИЕ! Мы везде работаем с UTF-8 кодировкой
        // Поэтому перекодировать здесь не надо
        result := sTxt;
    end
    else if (sCodePage = 'cp866') or (sCodePage = 'CP866') then
    begin
        // С Windows системами мы можем пользоваться
        // функциями UTF8ToWinCP и WinCPToUTF8 модуль LazUTF8
        result := LazUTF8.UTF8ToWinCP(sTxt);
    end
    else if (sCodePage = 'cp1251') or (sCodePage = 'CP1251') then
    begin
       result := LazUTF8.UTF8ToWinCP(sTxt);
    end
    else
        WriteLn('Не поддерживаемая кодировка <%s>', sCodePage);
end;

{
Печать цветового текста
@param sTxt Печатаемый текст
@param sColor Дополнительное указание цветовой раскраски
}
procedure PrintColorTxt(sTxt: AnsiString; sColor: AnsiString);
var
    str_txt: AnsiString;
begin
    str_txt := EncodeUnicodeString(sTxt, GetDefaultEncoding());
    // Для Windows систем цветовая раскраска отключена
    if sysfunc.IsOSLinux() then
        // Добавление цветовой раскраски для Linux систем
        str_txt := sColor + str_txt + NORMAL_COLOR_TEXT;

    // Если журналирование переведено в SysLog, то ничего не делать
    WriteLn(str_txt);
end;

{
Инициализация файла лога.
@param sLogFileName Имя файла лога. Если имя файла не определено, то пробуем его взять из оружения системы: Ключ LOG_FILENAME
}
function OpenLog(sLogFileName: AnsiString): Boolean;
begin
    Result := False;

    // Если журналирование переведено в SysLog, то ничего не делать
    if DEFAULT_APP_LOG_MODE then
       Exit;

    // Если имя файла не определено, то пробуем его взять из оружения системы
    //if (sLogFileName = '') and config.ENVIRONMENT.HasKey('LOG_FILENAME') then
    //   sLogFileName := (config.ENVIRONMENT.GetByName('LOG_FILENAME') As TObjString).Value;
    if sLogFileName = '' then
    begin
       WarningMsg('Не определено имя файла лога регистрации сообщений программы');
       Exit;
    end;

    // Проверить наличие папки
    filefunc.CreateDirPath(ExtractFileDir(sLogFileName));
    filefunc.CreateEmptyFileIfNotExists(sLogFileName);

    try
       InfoMsg(Format('Файл регистрации сообщений программы <%s>', [sLogFileName]));
       AssignFile(LOG_FILE, sLogFileName);
       Append(LOG_FILE);
       if IOResult = 0 then
          IS_OPEN_LOG_FILE := True;

       LogMsg('vvv Начало регистрации сообщений программы vvv');
       Result := True;
    except
       CloseLog();
       FatalMsg('Ошибка открытия файла лога', True);
    end;
end;

{
Закрыть файл лога.
}
function CloseLog(): Boolean;
begin
  Result := False;

  // Если журналирование переведено в SysLog, то ничего не делать
  if DEFAULT_APP_LOG_MODE then
     Exit;

  // Если журналирование отключено, то ничего не делать
  if not GetLogMode() then
    Exit;

  try
    if IS_OPEN_LOG_FILE then
      begin
        LogMsg('^^^ Окончание регистрации сообщений программы ^^^');
        CloseFile(LOG_FILE);
        IS_OPEN_LOG_FILE := False;
        Result := True;
        Exit;
      end;
  except
    on E: EInOutError do
      FatalMsg('Ошибка закрытия файла лога', True);
  end;
end;

{
Регистрация сообщения в файле лога.
@param sMsg Регистрируемое сообщение
@param bForceLog Признак принудительной регистрации
}
function LogMsg(sMsg: AnsiString = ''): Boolean;
var
  new_msg: AnsiString;
begin
  Result := False;

  // Если журналирование переведено в SysLog, то ничего не делать
  if DEFAULT_APP_LOG_MODE then
    Exit;
  if not IS_OPEN_LOG_FILE then
    Exit;

  new_msg := Format('%s %s', [FormatDateTime('YYYY-MM-DD hh:mm:ss', Now), sMsg]);
  try
    WriteLn(LOG_FILE, new_msg);
    Result := True;
  except
    CloseLog();
    FatalMsg('Ошибка регистрации сообщения в лог файле', True);
  end;
end;

{
Вывести ОТЛАДОЧНУЮ информацию.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure DebugMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      if DEFAULT_APP_LOG_MODE then
        Application.Log(etDebug, EncodeUnicodeString(sMsg, GetDefaultEncoding()))
      else
        PrintColorTxt('DEBUG. ' + sMsg, BLUE_COLOR_TEXT);

    if (GetLogMode()) or (bForceLog) then
      LogMsg('DEBUG. ' + sMsg);
end;

{
Вывести ТЕКСТОВУЮ информацию.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure InfoMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      if DEFAULT_APP_LOG_MODE then
        Application.Log(etInfo, EncodeUnicodeString(sMsg, GetDefaultEncoding()))
      else
        PrintColorTxt('INFO. ' + sMsg, GREEN_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('INFO. ' + sMsg);
end;

{
Вывести информацию об ОШИБКЕ.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure ErrorMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      if DEFAULT_APP_LOG_MODE then
        Application.Log(etError, EncodeUnicodeString(sMsg, GetDefaultEncoding()))
      else
        PrintColorTxt('ERROR. ' + sMsg, RED_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('ERROR. ' + sMsg);
end;

{
Вывести ПРЕДУПРЕЖДЕНИЕ.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure WarningMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      if DEFAULT_APP_LOG_MODE then
        Application.Log(etWarning, EncodeUnicodeString(sMsg, GetDefaultEncoding()))
      else
        PrintColorTxt('WARNING. ' + sMsg, YELLOW_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('WARNING. ' + sMsg);
end;


{
Вывести СООБЩЕНИЕ об ИСКЛЮЧИТЕЛЬНОЙ СИТУАЦИИ.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure FatalMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
var
    buf : array[0..511] of char;
    msg, except_msg: AnsiString;
begin
    msg := Format('FATAL. %s', [sMsg]);

    // StrPCopy(buf, DateTimeToStr(Now)+'. ');
    ExceptionErrorMessage(ExceptObject, ExceptAddr, @buf, SizeOf(buf));
    // StrCat(buf, #13#10);
    except_msg := buf;

    if (GetDebugMode()) or (bForcePrint) then
      if DEFAULT_APP_LOG_MODE then
        Application.Log(etError, EncodeUnicodeString(msg + ' ' + except_msg, GetDefaultEncoding()))
      else
      begin
        PrintColorTxt(msg, RED_COLOR_TEXT);
        PrintColorTxt(except_msg, RED_COLOR_TEXT);
      end;
    if (GetLogMode()) or (bForceLog) then
    begin
       LogMsg(msg);
       LogMsg(except_msg);
    end;
end;

{
Вывести СЕРВИСНУЮ информацию.
@param sMsg Текстовое сообщение
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure ServiceMsg(sMsg: AnsiString; bForcePrint: Boolean; bForceLog: Boolean);
begin
    if (GetDebugMode()) or (bForcePrint) then
      if DEFAULT_APP_LOG_MODE then
        Application.Log(etCustom, EncodeUnicodeString(sMsg, GetDefaultEncoding()))
      else
        PrintColorTxt('SERVICE. ' + sMsg, CYAN_COLOR_TEXT);
    if (GetLogMode()) or (bForceLog) then
       LogMsg('SERVICE. ' + sMsg);
end;

{
Вывести ОТЛАДОЧНУЮ информацию с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure DebugMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean; bForceLog: Boolean);
begin
  DebugMsg(Format(sMsgFmt, aArgs), bForcePrint, bForceLog);
end;

{
Вывести текстовую ИНФОРМАЦИЮ с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure InfoMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean; bForceLog: Boolean);
begin
  InfoMsg(Format(sMsgFmt, aArgs), bForcePrint, bForceLog);
end;

{
Вывести СЕРВИСНУЮ информацию с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure ServiceMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean; bForceLog: Boolean);
begin
  ServiceMsg(Format(sMsgFmt, aArgs), bForcePrint, bForceLog);
end;

{
Вывести информацию об ОШИБКЕ с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure ErrorMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean; bForceLog: Boolean);
begin
  ErrorMsg(Format(sMsgFmt, aArgs), bForcePrint, bForceLog);
end;

{
Вывести ПРЕДУПРЕЖДЕНИЕ с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure WarningMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean; bForceLog: Boolean);
begin
  WarningMsg(Format(sMsgFmt, aArgs), bForcePrint, bForceLog);
end;

{
Вывести СООБЩЕНИЕ об ИСКЛЮЧИТЕЛЬНОЙ СИТУАЦИИ с форматированным текстовым сообщением.
@param sMsgFmt Формат текстового сообщения
@param aArgs Аргументы текстового сообщения
@param bForcePrint Принудительно вывести на экран
@param bForceLog Принудительно записать в журнале
}
procedure FatalMsgFmt(sMsgFmt: AnsiString; const aArgs : Array Of Const; bForcePrint: Boolean; bForceLog: Boolean);
begin
  FatalMsg(Format(sMsgFmt, aArgs), bForcePrint, bForceLog);
end;

end.

