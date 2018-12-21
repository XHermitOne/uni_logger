{
Функции взаимодействия с операционной системой
}
unit sysfunc;

{$mode objfpc}{$H+}

interface

uses
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

implementation

{
Тип операционной системы: linux/windows
}
function GetOSType(): AnsiString;
begin
  result := OS;
end;

{ Проверка является ли ОС Linux }
function IsOSLinux(): Boolean;
begin
  result := OS = 'linux';
end;

{ Проверка является ли ОС Windows }
function IsOSWindows(): Boolean;
begin
  result := OS = 'windows';
end;

end.

