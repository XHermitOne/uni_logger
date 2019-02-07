{
Функции взаимодействия с операционной системой

Версия: 0.0.2.2
}
unit sysfunc;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
  Windows,
  {$ENDIF}
  Classes, SysUtils;

{$IFDEF linux}
{ some linux-specific code }
const OS: AnsiString = 'linux';
{$ENDIF}

{$IFDEF windows}
{ some M$-specific code }
const OS: AnsiString = 'windows';
{$ENDIF}

{ Тип операционной системы: linux/windows}
function GetOSType(): AnsiString;

{ Проверка является ли ОС Linux }
function IsOSLinux(): Boolean;

{ Проверка является ли ОС Windows }
function IsOSWindows(): Boolean;

{ Наименование компьютера }
function GetNetComputerName(): AnsiString;

implementation

{
Тип операционной системы: linux/windows
}
function GetOSType(): AnsiString;
begin
  Result := OS;
end;

{ Проверка является ли ОС Linux }
function IsOSLinux(): Boolean;
begin
  Result := OS = 'linux';
end;

{ Проверка является ли ОС Windows }
function IsOSWindows(): Boolean;
begin
  Result := OS = 'windows';
end;

{ Наименование компьютера }
function GetNetComputerName(): AnsiString;
{$IFDEF windows}
var
  buffer: Array[0..255] Of char;
  size: dword;
{$ENDIF}

begin
  Result := '';

  {$IFDEF windows}
  size := 256;
  if GetComputerName(buffer, size) then
    Result := buffer;
  {$ENDIF}
end;

end.

