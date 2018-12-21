{
Функции отладки утечек памяти.
}
unit memfunc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

var HPStart : THeapStatus;
var HPEnd : THeapStatus;
var Lost: Integer;

{ Инициализировать состояние памяти на данный момент }
function InitStatusMemory(): LongInt;
{ Подсчитать количество утерянной памяти с момента инициализации состояния }
function GetLostMemory(): LongInt;
{ Распечатать количество утерянной памяти }
procedure PrintLostMemory();


implementation

uses
    log;

{
Инициализировать состояние памяти на данный момент.
}
function InitStatusMemory(): LongInt;
begin
  HPStart := GetHeapStatus;
  result := HPStart.TotalAllocated;
end;

{
Подсчитать количество утерянной памяти с момента инициализации состояния
}
function GetLostMemory(): LongInt;
begin
  HPEnd := GetHeapStatus;
  result := HPEnd.TotalAllocated - HPStart.TotalAllocated;
end;

{
Распечатать количество утерянной памяти
}
procedure PrintLostMemory();
var
  lost_memory: LongInt;
begin
  lost_memory := GetLostMemory;
  if lost_memory > 0 then
     log.WarningMsg(Format('Обнаружена утечка памяти <%d>', [lost_memory]), True)
  else
     log.ServiceMsg('Утечек памяти не обнаружено', True);
end;

end.

