{
Модуль функция работы со временными данными

Версия: 0.0.1.1
}

unit dtfunc;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

{   DecodeDate unpacks the value Date into three values:
    Year, Month and Day   }
procedure DecodeDateDelta(Date: TDateTime; out Year, Month, Day: Word);

{ Секунды }
function GetSecondDelta(dtDateTime: TDateTime): Integer;

{ Минуты }
function GetMinuteDelta(dtDateTime: TDateTime): Integer;

{ Часы }
function GetHourDelta(dtDateTime: TDateTime): Integer;

{ Дни }
function GetDayDelta(dtDateTime: TDateTime): Integer;

{ Месяцы }
function GetMonthDelta(dtDateTime: TDateTime): Integer;

{ Годы }
function GetYearDelta(dtDateTime: TDateTime): Integer;

implementation

{   DecodeDate unpacks the value Date into three values:
    Year, Month and Day   }
procedure DecodeDateDelta(Date: TDateTime; out Year, Month, Day: Word);
var
  ly, ld, lm, j : cardinal;
  dt: Double;
begin
  dt :=  Double(Date);
  // If Date is before 1-1-1 then return 0-0-0
  if dt = 0 then
  begin
    Year := 0;
    Month := 0;
    Day := 0;
  end
  else
  begin
    if Date > 0 then
      Date := Date + 1 / (msecsperday * 2)
    else
      Date := Date - 1 / (msecsperday * 2);
    if Date > MaxDateTime then
      Date := MaxDateTime;
//       Raise EConvertError.CreateFmt('%f is not a valid TDatetime encoding, maximum value is %f.',[Date,MaxDateTime]);
    j := Trunc(System.Int(Date)) * 4;
    //j := Trunc(System.Int(Date));
    // Количество веков
    ly := j DIV 146097;
    j := j - 146097 * cardinal(ly);
    ld := j DIV 4;
    j := (ld * 4 + 3) DIV 1461;
    ld := (cardinal(ld) * 4 + 7 - 1461 * j) DIV 4;
    lm := (5 * ld - 3) DIV 153;
    ld := (5 * ld + 2 - 153 * lm) DIV 5;
    ly := 100 * cardinal(ly) + j;
    if lm < 10 then
      Inc(lm, 3)
    else
    begin
      Dec(lm, 9);
      Inc(ly);
    end;
    Year := ly;
    Month := lm;
    Day := ld;
  end;
end;

{ Секунды }
function GetSecondDelta(dtDateTime: TDateTime): Integer;
begin
  Result := DateUtils.SecondOf(dtDateTime);
end;

{ Минуты }
function GetMinuteDelta(dtDateTime: TDateTime): Integer;
begin
  Result := DateUtils.MinuteOf(dtDateTime);
end;

{ Часы }
function GetHourDelta(dtDateTime: TDateTime): Integer;
begin
  Result := DateUtils.HourOf(dtDateTime);
end;

{ Дни }
function GetDayDelta(dtDateTime: TDateTime): Integer;
var
  cur_year, cur_month, cur_day: Word;
begin
  DecodeDateDelta(dtDateTime, cur_year, cur_month, cur_day);
  Result := cur_day;
end;

{ Месяцы }
function GetMonthDelta(dtDateTime: TDateTime): Integer;
var
  cur_year, cur_month, cur_day: Word;
begin
  DecodeDateDelta(dtDateTime, cur_year, cur_month, cur_day);
  Result := cur_month;
end;

{ Годы }
function GetYearDelta(dtDateTime: TDateTime): Integer;
var
  cur_year, cur_month, cur_day: Word;
begin
  DecodeDateDelta(dtDateTime, cur_year, cur_month, cur_day);
  Result := cur_year;
end;

end.


