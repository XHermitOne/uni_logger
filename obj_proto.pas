{
Модуль абстрактного объекта системы

Версия: 0.0.4.1
}
unit obj_proto;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dictionary;

const
  DATETIME_TXT_FMT: AnsiString = 'yyyy-mm-dd hh:nn:ss';
  DATE_TXT_SEPARATOR: Char = '-';

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
    {
    Буфер изменения состояний во времени
    На верхнем уровне ключи представляют из себя время в виде строки:
    yyyy-mm-dd hh:nn:ss
    Некоторые источники данных могут предоставлять значения только порционно,
    кадрами-буферами. Для таких случаев и используется этот буфер
    }
    FTimeStateBuffer: TStrDictionary;

    { Свойства контроллера данных. Прописаны в INI файле }
    FProperties: TStrDictionary;

  public
    constructor Create;
    destructor Destroy; override;

    {
    Функция очистки читаемых значений
    @param bAutoFree Автоматическое удаление элементов из памяти
    }
    function ClearReadValues(bAutoFree: Boolean = True): Boolean;
    {
    Функция очистки текущего состояния
    @param bAutoFree Автоматическое удаление элементов из памяти
    }
    function ClearState(bAutoFree: Boolean = True): Boolean;
    {
    Функция очистки буфера изменения состояния
    @param bAutoFree Автоматическое удаление элементов из памяти
    }
    function ClearTimeState(bAutoFree: Boolean = True): Boolean;

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
    procedure SetProperties(dProperties: TStrDictionary); virtual;

    { Установить свойства в виде списка параметров }
    procedure SetPropertiesArray(aArgs: Array Of Const); virtual;

    { Проверка на то что объект не именованный }
    function IsUnknown(): Boolean;

    { 
    Фунция чтения данных 
    @param sAddresses Список адресов для чтения
    @param dtTime: Время актуальности за которое необходимо получить данные. 
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function Read(sAddresses: TStringList; dtTime: TDateTime = 0): TStringList; virtual;
    { 
    Чтение значений по адресам 
    @param sAddresses Массив адресов для чтения
    @param dtTime: Время актуальности за которое необходимо получить данные. 
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAddresses(sAddresses: Array Of String; dtTime: TDateTime = 0): TStringList; virtual;
    { 
    Чтение значения по адресу 
    @param sAddress Строка адреса для чтения
    @param dtTime: Время актуальности за которое необходимо получить данные. 
                  Если не определено, то берется текущее системное время.
    @return Прочитанное значение в виде строки.
    }
    function ReadAddress(sAddress: AnsiString; dtTime: TDateTime = 0): AnsiString; virtual;
    { 
    Чтение всех внутренних данных, описанных в свойствах.
    @param dtTime: Время актуальности за которое необходимо получить данные. 
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAll(dtTime: TDateTime = 0): TStringList; virtual;

    { 
    Фунция записи данных 
    @param sAddresses Список адресов для записи
    @param aValues Список значений для записи
    @param dtTime: Время актуальности данных. 
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function Write(sAddresses, aValues: TStringList; dtTime: TDateTime = 0): Boolean; virtual;
    { 
    Запись значений по адресам 
    @param sAddresses Массив адресов для записи
    @param aValues Массив значений для записи
    @param dtTime: Время актуальности данных. 
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function WriteAddresses(sAddresses,aValues: Array Of String; dtTime: TDateTime = 0): Boolean; virtual;
    { 
    Запись значения по адресу 
    @param sAddress Значение адреса для записи
    @param aValue Значение для записи в строковом представлении
    @param dtTime: Время актуальности данных. 
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function WriteAddress(sAddress, aValue: AnsiString; dtTime: TDateTime = 0): Boolean; virtual;
    { 
    Запись всех внутренних данных 
    @param dtTime: Время актуальности данных. 
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function WriteAll(dtTime: TDateTime = 0): Boolean; virtual;

    { Зарегистрировать значения переменных в словаре внутренного состояния }
    function RegState(aValues: TStrDictionary): Boolean;
    { Получить имена записываемых значений в контроллер данных }
    function GetReadValues(): TStringList;

  published
    property Name: AnsiString read GetName write SetName;
    property Description: AnsiString read FDescription;
    property Properties: TStrDictionary read GetProperties write SetProperties;
    property State: TStrDictionary read FState write FState;
    property TimeState: TStrDictionary read FTimeStateBuffer write FTimeStateBuffer;

end;


implementation

uses
  log, memfunc;

constructor TICObjectProto.Create;
begin
  inherited Create;
  FParent := nil;
  FName := 'Unknown';
  FDescription := '';

  FReadValues := TStringList.Create;
  FState := TStrDictionary.Create;
  FTimeStateBuffer := TStrDictionary.Create;
end;

destructor TICObjectProto.Destroy;
begin
  if FReadValues <> nil then
  begin
    FReadValues.Destroy;
    FReadValues := nil;
  end;
  if FState <> nil then
  begin
    FState.Destroy;
    FState := nil;
  end;
  if FTimeStateBuffer <> nil then
  begin
    FTimeStateBuffer.Destroy;
    FTimeStateBuffer := nil;
  end;

  if FProperties <> nil then
  begin
    FProperties.Destroy;
    FProperties := nil;
  end;
  inherited Destroy;
end;

function TICObjectProto.ClearReadValues(bAutoFree: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FReadValues.Count > 0 then
    if not bAutoFree then
    begin
      FReadValues.Clear;
      Result := True;
    end
    else
    begin
      for i := FReadValues.Count - 1 downto 0 do
      begin
        FReadValues.Objects[i].Destroy;
        FReadValues.Delete(i);
      end;
    end;
end;

function TICObjectProto.ClearState(bAutoFree: Boolean): Boolean;
begin
  Result := FState.ClearContent(bAutoFree);
end;

function TICObjectProto.ClearTimeState(bAutoFree: Boolean): Boolean;
begin
  Result := FTimeStateBuffer.ClearContent(bAutoFree);
end;


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
  Result := FProperties;
end;

procedure TICObjectProto.SetProperties(dProperties: TStrDictionary);
begin
  FProperties := dProperties;
  if FProperties.HasKey('name') then
    SetName(FProperties.GetStrValue('name'))
  else
    log.WarningMsgFmt('Не определено имя объекта в свойствах. Класс <%s>', [ClassName]);
end;

{
Проверка на то что объект не именованный.
}
function TICObjectProto.IsUnknown(): Boolean;
begin
  Result := FName = 'Unknown';
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
Установить свойства в виде списка параметров
}
procedure TICObjectProto.SetPropertiesArray(aArgs: Array Of Const);
begin

end;

{ 
Фунция чтения данных 
@param sAddresses Список адресов для чтения
@param dtTime: Время актуальности за которое необходимо получить данные. 
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICObjectProto.Read(sAddresses: TStringList; dtTime: TDateTime): TStringList;
begin
  log.WarningMsgFmt('Вызов не определенного метода Read объекта <%s>', [FName]);
  Result := nil;
end;

{ 
Чтение значений по адресам 
@param sAddresses Массив адресов для чтения
@param dtTime: Время актуальности за которое необходимо получить данные. 
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICObjectProto.ReadAddresses(sAddresses: Array Of String; dtTime: TDateTime): TStringList;
begin
  log.WarningMsgFmt('Вызов не определенного метода ReadAddresses объекта <%s>', [FName]);
  Result := nil;
end;

{ 
Чтение значения по адресу 
@param sAddress Строка адреса для чтения
@param dtTime: Время актуальности за которое необходимо получить данные. 
              Если не определено, то берется текущее системное время.
@return Прочитанное значение в виде строки.
}
function TICObjectProto.ReadAddress(sAddress: AnsiString; dtTime: TDateTime): AnsiString;
begin
  log.WarningMsgFmt('Вызов не определенного метода ReadAddress объекта <%s>', [FName]);
  Result := '';
end;

{ 
Чтение всех внутренних данных, описанных в свойствах.
@param dtTime: Время актуальности за которое необходимо получить данные. 
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICObjectProto.ReadAll(dtTime: TDateTime): TStringList;
begin
  log.WarningMsgFmt('Вызов не определенного метода ReadAll объекта <%s>', [FName]);
  Result := nil;
end;

{ 
Фунция записи данных 
@param sAddresses Список адресов для записи
@param aValues Список значений для записи
@param dtTime: Время актуальности данных. 
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICObjectProto.Write(sAddresses, aValues: TStringList; dtTime: TDateTime): Boolean;
begin
  log.WarningMsgFmt('Вызов не определенного метода Write объекта <%s>', [FName]);
  Result := False;
end;

{ 
Запись значений по адресам 
@param sAddresses Массив адресов для записи
@param aValues Массив значений для записи
@param dtTime: Время актуальности данных. 
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICObjectProto.WriteAddresses(sAddresses,aValues: Array Of String; dtTime: TDateTime): Boolean;
begin
  log.WarningMsgFmt('Вызов не определенного метода WriteAddresses объекта <%s>', [FName]);
  Result := False;
end;

{ 
Запись значения по адресу 
@param sAddress Значение адреса для записи
@param aValue Значение для записи в строковом представлении
@param dtTime: Время актуальности данных. 
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICObjectProto.WriteAddress(sAddress, aValue: AnsiString; dtTime: TDateTime): Boolean;
begin
  log.WarningMsgFmt('Вызов не определенного метода WriteAddress объекта <%s>', [FName]);
  Result := False;
end;

{ 
Запись всех внутренних данных 
@param dtTime: Время актуальности данных. 
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICObjectProto.WriteAll(dtTime: TDateTime): Boolean;
begin
  log.WarningMsgFmt('Вызов не определенного метода WriteAll объекта <%s>', [FName]);
  Result := False;
end;

end.

