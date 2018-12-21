{
Модуль абстрактного объекта системы
}
unit obj_proto;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dictionary;

type
  {
  Абстрактный объект системы.
  Реализует общие функции для всех объектов.
  }
  TICObjectProto = class(TObject)
  private
    { Объект родительского управляющего объекта }
    FParent: TObject;
    { Наменование объекта }
    FName: AnsiString;
    { Описание объекта }
    FDescription: AnsiString;
    { Имена читаемых значений из контроллера данных }
    FReadValues: TStringList;
    { ВНИМАНИЕ! Источники данных запоминают после чтения состояние переменных для
     последующего доступа к ним объектов приемников данных
     Вот это словарь переменных }
    FState: TStrDictionary;

    { Свойства контроллера данных. Прописаны в INI файле }
    FProperties: TStrDictionary;

  public
    constructor Create;
    destructor Destroy; override;
    //procedure Free;

    { Получить наименование объекта }
    function GetName(): AnsiString;
    { Установить наименование объекта }
    procedure SetName(sName: AnsiString);

    { Получить родительский объект }
    function GetParent(): TObject;
    { Установить родительский объект }
    procedure SetParent(oParent: TObject);

    { Получить словарь свойств объекта }
    function GetProperties(): TStrDictionary;
    { Установить свойства объекта в виде словаря }
    procedure SetProperties(dProperties: TStrDictionary);

    { Установить свойства в виде списка параметров }
    procedure SetPropertiesArray(aArgs: Array Of Const); virtual;

    { Проверка на то что объект не именованный }
    function IsUnknown(): Boolean;
    { Фунция чтения данных }
    function Read(aValues: TStringList): TStringList; virtual;
    { Чтение значений по адресам }
    function ReadAddresses(aValues: Array Of String): TStringList; virtual;

    { Фунция записи данных }
    function Write(aValues: TStringList): Boolean; virtual;
    { Зарегистрировать значения переменных в словаре внутренного состояния }
    function RegState(aValues: TStrDictionary): Boolean;
    { Получить имена записываемых значений в контроллер данных }
    function GetReadValues(): TStringList;

  published
    property Properties: TStrDictionary read GetProperties write SetProperties;

end;


implementation

uses
    log;

constructor TICObjectProto.Create;
begin
     inherited Create;
     FParent := nil;
     FName := 'Unknown';
     FDescription := '';
     FReadValues := TStringList.Create;
     FState := TStrDictionary.Create;
end;

destructor TICObjectProto.Destroy;
begin
  if FReadValues <> nil then
  begin
    FReadValues.Free;
    FReadValues := nil;
  end;
  if FState <> nil then
  begin
    FState.Free;
    FState := nil;
  end;

  if FProperties <> nil then
  begin
    FProperties.Free;
    FProperties := nil;
  end;
  inherited Destroy;
end;

//procedure TICObjectProto.Free;
//begin
//  inherited Free;
//end;

function TICObjectProto.GetName(): AnsiString;
begin
  Result := FName;
end;

procedure TICObjectProto.SetName(sName: AnsiString);
begin
  FName := sName;
end;

function TICObjectProto.GetParent(): TObject;
begin
  Result := FParent;
end;

procedure TICObjectProto.SetParent(oParent: TObject);
begin
     FParent := oParent;
end;

function TICObjectProto.GetProperties(): TStrDictionary;
begin
  result := FProperties;
end;

procedure TICObjectProto.SetProperties(dProperties: TStrDictionary);
begin
  FProperties := dProperties;
  if FProperties.HasKey('name') then
    SetName(FProperties.GetStrValue('name'))
  //else
  //  WarningMsg(Format('Не определено имя объекта в свойствах. Класс <%s>', [ClassName]));
end;

{
Проверка на то что объект не именованный.
}
function TICObjectProto.IsUnknown(): Boolean;
begin
     result := FName = 'Unknown';
end;

{
Зарегистрировать значения переменных в словаре внутренного состояния.
@param (Values Словарь переменных)
}
function TICObjectProto.RegState(aValues: TStrDictionary): Boolean;
begin
  Result := FState.Update(aValues);
end;

{
Получить имена записываемых значений в контроллер данных
}
function TICObjectProto.GetReadValues(): TStringList;
begin
  Result := FReadValues;
end;

{
Фунция чтения данных
}
function TICObjectProto.Read(aValues: TStringList): TStringList;
begin
  Result := nil;
end;

function TICObjectProto.ReadAddresses(aValues: Array of String): TStringList;
begin
  Result := nil;
end;

{
Фунция записи данных
}
function TICObjectProto.Write(aValues: TStringList): Boolean;
begin
  Result := False;
end;

{
Установить свойства в виде списка параметров
}
procedure TICObjectProto.SetPropertiesArray(aArgs: Array Of Const);
begin

end;

end.

