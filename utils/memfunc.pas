{
Функции отладки утечек памяти.

ВНИМАНИЕ! Из Destroy необходимо вызывать Free.
В Free не должно быть вызова inherited Free.
Тогда не происходит утечки памяти.
Везде необходимо использовать функцию Destroy
для освобождения памяти!!!

Версия: 0.0.3.1
}
unit memfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  {
  Класс отслеживания памяти.
  Применяется для отлавливания утечек памяти.
  }
  TMemoryStatus = class(TObject)
    private
      FHPStart: THeapStatus;
      FHPEnd: THeapStatus;
      FLost: LongInt;

    public
      constructor Create();
      destructor Destroy; override;

      { Инициализировать состояние памяти на данный момент }
      function Init(): LongInt;
      { Подсчитать количество утерянной памяти с момента инициализации состояния }
      function GetLost(): LongInt;
      { Распечатать количество утерянной памяти }
      procedure PrintLost();

      //
      property Lost: LongInt read FLost;
      property MemStart: THeapStatus read FHPStart;
      property MemEnd: THeapStatus read FHPEnd;
  end;

{ Инициализировать состояние памяти на данный момент }
function InitStatusMemory(): Boolean;
{ Подсчитать количество утерянной памяти с момента инициализации состояния }
function GetLostMemory(): LongInt;
{ Распечатать количество утерянной памяти }
procedure PrintLostMemory(bAutoDestroy: Boolean = True);

var
  GlobMemoryStatus: TMemoryStatus = nil;

implementation

uses
  log;

constructor TMemoryStatus.Create();
begin
  inherited Create;
  Init();
end;

destructor TMemoryStatus.Destroy;
begin
  inherited Destroy;
end;

{
Инициализировать состояние памяти на данный момент.
}
function TMemoryStatus.Init(): LongInt;
begin
  FHPStart := GetHeapStatus;
  Result := FHPStart.TotalAllocated;
end;

{
Подсчитать количество утерянной памяти с момента инициализации состояния
}
function TMemoryStatus.GetLost(): LongInt;
begin
  FHPEnd := GetHeapStatus;
  Result := FHPEnd.TotalAllocated - FHPStart.TotalAllocated;
end;

{
Распечатать количество утерянной памяти
}
procedure TMemoryStatus.PrintLost();
var
  lost_memory: LongInt;
begin
  lost_memory := GetLost();
  if lost_memory > 0 then
    log.WarningMsg(Format('Обнаружена утечка памяти <%d>', [lost_memory]), True)
  else if lost_memory < 0 then
    log.WarningMsg(Format('Обнаружено освобождение памяти <%d>', [lost_memory]), True)
  else
    log.ServiceMsg('Утечек памяти не обнаружено', True);
end;

{ Инициализировать состояние памяти на данный момент }
function InitStatusMemory(): Boolean;
begin
  log.InfoMsgFmt('Создание глобального объекта состояния памяти. Память [%d]', [GetHeapStatus().TotalAllocated]);

  Result := False;
  if GlobMemoryStatus = nil then
  begin
    GlobMemoryStatus := TMemoryStatus.Create();
    Result := True;
  end;
end;

{ Подсчитать количество утерянной памяти с момента инициализации состояния }
function GetLostMemory(): LongInt;
begin
  Result := 0;
  if GlobMemoryStatus <> nil then
    Result := GlobMemoryStatus.GetLost();
end;

{ Распечатать количество утерянной памяти }
procedure PrintLostMemory(bAutoDestroy: Boolean);
begin
  if GlobMemoryStatus <> nil then
  begin
    GlobMemoryStatus.PrintLost();
    if bAutoDestroy then
    begin
       GlobMemoryStatus.Destroy();
       GlobMemoryStatus := nil;

       log.InfoMsgFmt('Удаление глобального объекта состояния памяти. Память [%d]', [GetHeapStatus().TotalAllocated]);
    end;
  end
  else
    log.WarningMsg('Не определен глобальный объект состояния памяти');
end;

end.

