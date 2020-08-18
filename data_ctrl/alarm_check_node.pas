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
    Текст скрипт проверки аварии.
    В формате Pascal Script (https://wiki.lazarus.freepascal.org/Pascal_Script/ru).
    Результат выполнения скрипта - Текущее состояние аварии.
    }
    FScriptTxt: AnsiString;

    {
    Скрипт проверки аварии.
    Результат выполнения скрипта - Текущее состояние аварии.
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

    //{ Выбрать описания тегов из свойств }
    //function CreateTags(bClearValue: Boolean = False): TStrDictionary;
    //
    //{ Установить свойства в виде списка параметров }
    //procedure SetPropertiesArray(aArgs: Array Of Const); override;
    //
    //{ Установить свойства объекта в виде словаря }
    //procedure SetProperties(dProperties: TStrDictionary); override;
    //
    {
    Чтение всех внутренних данных, описанных в свойствах.
    @param dtTime: Время актуальности за которое необходимо получить данные.
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAll(dtTime: TDateTime = 0): TStringList; override;

  published
    property AlarmState: Boolean read FAlarmState;
    property ScriptTxt: AnsiString read FScriptTxt write FScriptTxt;
    property Script: TPSScript read FScript;

end;


implementation

uses
  //LCLIntf, // Для вычисления времени выполнения
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
begin
  Result := False;

  if strfunc.IsEmptyStr(aScriptTxt) then
  begin
     aScriptTxt := FScriptTxt;
  end;

  try
    FScript.Script.Text := aScriptTxt;
    if FScript.Compile then
      Result := FScript.Execute
    else
      log.ErrorMsgFmt('Ошибка компиляции скрипта проверки аварии\n%s', [aScriptTxt]);
  except
    log.FatalMsgFmt('Ошибка выполнения скрипта определения аварии\n%s', [aScriptTxt]);
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

