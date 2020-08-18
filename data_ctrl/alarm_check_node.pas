{
Модуль узла проверки аварийных ситуаций.

Версия: 0.0.0.1
}

unit alarm_check_node;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
  // Windows, ActiveX, ComObj,
  {$ENDIF}
  Classes, SysUtils, DateUtils, Variants, VarUtils,
  uPSCompiler,
  uPSR_std, uPSC_std,
  uPSR_classes, uPSC_classes,
  //uPSC_controls, uPSR_controls,
  //uPSC_forms, uPSR_forms,
  uPSRuntime,
  uPSComponent,
  uPSDisassembly,
  uPSR_dateutils, uPSC_dateutils,
  uPSR_dll, uPSC_dll,

  obj_proto, dictionary, strfunc, dtfunc, exttypes;

const
  ALARM_CHECK_NODE_TYPE: AnsiString = 'ALARM_CHECK';

  RESERV_PROPERTIES: Array [1..5] Of String = ('type', 'name', 'description', 'script', 'default_state');


type
  {
  Класс проверки аварии.
  }
  TICAlarmCheckNode = class(TICObjectProto)

  private
    {
    Скрипт проверки аварии.
    }
    FScript: TPSScript;

    {
    Текущее состояние:
    True - есть авария False - авария не зарегистрирована.
    }
    FAlarmState: Boolean;

    {
    Выполнить скрипт проверки аварии.
    @param aScript Текст скрипта проверки аварии.
    @return True - есть авария False - авария не зарегистрирована
    }
    function Execute(aScriptTxt: AnsiString=''): Boolean;


  public
    constructor Create;
    destructor Destroy; override;
    procedure Free;

    {
    Чтение всех внутренних данных, описанных в свойствах.
    @param dtTime: Время актуальности за которое необходимо получить данные.
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAll(dtTime: TDateTime = 0): TStringList; override;

  published
    property AlarmState: Boolean read FAlarmState;
    property Script: TPSScript read FScript;

end;


implementation

uses
  log, filefunc, memfunc;


constructor TICAlarmCheckNode.Create;
begin
  inherited Create;

  FScript:= TPSScript.Create(nil);
  //FScript.OnCompile:= OnCompile;
  //FScript.OnExecImport:= OnExecImport;
end;

destructor TICAlarmCheckNode.Destroy;
begin
  Free;

  inherited Destroy;
end;


procedure TICAlarmCheckNode.Free;
begin
  FScript.Free;
end;


{ Выполнить скрипт проверки аварии }
function TICAlarmCheckNode.Execute(aScriptTxt: AnsiString): Boolean;
var
  i: Integer;
begin
  Result := False;

  if strfunc.IsEmptyStr(aScriptTxt) then
     aScriptTxt := Properties.GetStrValue('script');

  try
    FScript.Script.Text := aScriptTxt;
    if FScript.Compile then
    begin
      if not FScript.Execute then
      begin
        log.ErrorMsgFmt('Объект <%s>. Ошибка выполнения скрипта определения аварии <%s>', [Name, aScriptTxt]);
        log.ErrorMsg(FScript.ExecErrorToString);
      end
      else
        Result := True;
    end
    else
      log.ErrorMsgFmt('Объект <%s>. Ошибка компиляции скрипта проверки аварии <%s>', [Name, aScriptTxt]);
      if FScript.CompilerMessageCount > 0 then
        for i := 0 to FScript.CompilerMessageCount - 1 do
          log.ErrorMsg(FScript.CompilerErrorToStr(i));
  except
    log.FatalMsgFmt('Объект <%s>. Ошибка выполнения скрипта определения аварии <%s>', [Name, aScriptTxt]);
  end;

end;

{
Чтение всех внутренних данных, описанных в свойствах.
@param dtTime: Время актуальности за которое необходимо получить данные.
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICAlarmCheckNode.ReadAll(dtTime: TDateTime): TStringList;
var
  execute_result: Boolean;
begin
  execute_result := Execute();
  log.InfoMsgFmt('Объект <%s>. Результат проверки аварии [%s]', [Name, BoolToStr(execute_result)]);

  Result := nil;
end;

end.

