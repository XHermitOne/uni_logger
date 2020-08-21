{
Функции работы с сетью.
}
unit netfunc;

{$mode objfpc}{$H+}

interface

uses
    //Classes, SysUtils,
    //blcksock,
    //synautil,
    //synsock,
    pingsend;

{
Проверка связи Ping
@param sHost Имя/IP адрес пингуемого компьютера
@return True - Связь есть / False - Нет пинга
}
function DoPing(sHost: AnsiString): Boolean;

implementation

{
Проверка связи Ping
@param sHost Имя/IP адрес пингуемого компьютера
@return True - Связь есть / False - Нет пинга
}
function DoPing(sHost: AnsiString): Boolean;
var
  ping_send: TPingSend;
begin
  Result := False;
  ping_send := TPingSend.Create;
  try
    ping_send.Timeout := 750;
    Result := ping_send.Ping(sHost);
  finally
    ping_send.Free;
  end;
end;

end.

