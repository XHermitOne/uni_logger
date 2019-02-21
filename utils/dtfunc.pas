{
Модуль функция работы со временными данными

Версия: 0.0.1.1
}

unit dtfunc;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

{ Секунды }
function GetSeconds(dtDateTime: TDateTime): Integer;

{ Минуты }
function GetMinutes(dtDateTime: TDateTime): Integer;

{ Часы }
function GetHours(dtDateTime: TDateTime): Integer;

{ Дни }
function GetDays(dtDateTime: TDateTime): Integer;

{ Месяцы }
function GetMonths(dtDateTime: TDateTime): Integer;

{ Годы }
function GetYears(dtDateTime: TDateTime): Integer;

implementation

{ Секунды }
function GetSeconds(dtDateTime: TDateTime): Integer;
begin

end;

{ Минуты }
function GetMinutes(dtDateTime: TDateTime): Integer;
begin

end;

{ Часы }
function GetHours(dtDateTime: TDateTime): Integer;
begin

end;

{ Дни }
function GetDays(dtDateTime: TDateTime): Integer;
var
  dt: Double;
begin
  dt := Double(dtDateTime);
  if Integer(dt) = 0 then
    Result := 0
  else
    Result := DayOfTheMonth(dtDateTime);
end;

{ Месяцы }
function GetMonths(dtDateTime: TDateTime): Integer;
var
  dt: Double;
begin
  dt := Double(dtDateTime);
  Result := Integer(dt) / ;
end;

{ Годы }
function GetYears(dtDateTime: TDateTime): Integer;
begin

end;

end.


