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


  protected
    procedure PSScriptCompile(Sender: TPSScript);
    procedure PSScriptExecImport(Sender: TObject; se: TPSExec;
                                   x: TPSRuntimeClassImporter);
    procedure PSScriptExecute(Sender: TPSScript);

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

  FScript := TPSScript.Create(nil);
  FScript.OnCompile := @PSScriptCompile;
  FScript.OnExecImport := @PSScriptExecImport;
  FScript.OnExecute := @PSScriptExecute;
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

procedure TICAlarmCheckNode.PSScriptExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  //RIRegister_Controls(x);
  //RIRegister_Forms(x);
  RegisterDateTimeLibrary_R(se);
  RegisterDLLRuntime(se);
end;

procedure TICAlarmCheckNode.PSScriptCompile(Sender: TPSScript);
begin
  RegisterDateTimeLibrary_C(Sender.Comp);

  Sender.AddFunction(@log.DebugMsg, 'procedure DebugMsg(sMsg: AnsiString; bForcePrint: Boolean = False; bForceLog: Boolean = False)');
  //Sender.AddFunction(@MWrites, 'procedure Writes(const s: string)');
  //Sender.AddFunction(@MWritedt,'procedure WriteDT(d : TDateTime)');
  //Sender.AddFunction(@MWritei, 'procedure Writei(const i: Integer)');
  //Sender.AddFunction(@MWrited, 'procedure Writed(const f: Double)');
  //Sender.AddFunction(@MWriteln, 'procedure Writeln');
  //Sender.AddFunction(@MyVal, 'procedure Val(const s: string; var n, z: Integer)');

  Sender.AddFunction(@FileCreate, 'function FileCreate(const FileName: string): integer)');
  Sender.AddFunction(@FileWrite, 'function FileWrite(Handle: Integer; const Buffer: pChar; Count: LongWord): Integer)');
  Sender.AddFunction(@FileClose, 'procedure FileClose(handle: integer)');

  //Sender.AddRegisteredVariable('Application', 'TApplication');

  //Sender.AddRegisteredVariable('Self', 'TICAlarmCheckNode');

  SIRegister_Std(Sender.Comp);
  SIRegister_Classes(Sender.Comp,true);
  //SIRegister_Controls(Sender.Comp);
  //SIRegister_Forms(Sender.Comp);
end;

procedure TICAlarmCheckNode.PSScriptExecute(Sender: TPSScript);
begin
  //FScript.SetVarToInstance('APPLICATION', Application);
  //FScript.SetVarToInstance('SELF', Self);
  //FScript.SetVarToInstance('MEMO1', Memo1);
  //FScript.SetVarToInstance('MEMO2', Memo2);
  //PPSVariantVariant(PSScript.GetVariable('VARS'))^.Data := VarArrayCreate([0, 1], varShortInt)
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

