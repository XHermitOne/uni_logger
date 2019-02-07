{
Модуль абстрактного объекта системы

Версия: 0.0.2.2
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
    procedure SetProperties(dProperties: TStrDictionary); virtual;

    { Установить свойства в виде списка параметров }
    procedure SetPropertiesArray(aArgs: Array Of Const); virtual;

    { Проверка на то что объект не именованный }
    function IsUnknown(): Boolean;

    { 
    Фунция чтения данных 
    @param aAddresses Список адресов для чтения
    @param aTime: Время актуальности за которое необходимо получить данные. 
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function Read(aAddresses: TStringList; aTime: TDateTime = 0): TStringList; virtual;
    { 
    Чтение значений по адресам 
    @param aAddresses Массив адресов для чтения
    @param aTime: Время актуальности за которое необходимо получить данные. 
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAddresses(aAddresses: Array Of String; aTime: TDateTime = 0): TStringList; virtual;
    { 
    Чтение значения по адресу 
    @param aAddress Строка адреса для чтения
    @param aTime: Время актуальности за которое необходимо получить данные. 
                  Если не определено, то берется текущее системное время.
    @return Прочитанное значение в виде строки.
    }
    function ReadAddress(aAddress: AnsiString; aTime: TDateTime = 0): AnsiString; virtual;
    { 
    Чтение всех внутренних данных, описанных в свойствах.
    @param aTime: Время актуальности за которое необходимо получить данные. 
                  Если не определено, то берется текущее системное время.
    @return Список прочитанных значений.
    }
    function ReadAll(aTime: TDateTime = 0): TStringList; virtual;

    { 
    Фунция записи данных 
    @param aAddresses Список адресов для записи
    @param aValues Список значений для записи
    @param aTime: Время актуальности данных. 
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function Write(aAddresses, aValues: TStringList; aTime: TDateTime = 0): Boolean; virtual;
    { 
    Запись значений по адресам 
    @param aAddresses Массив адресов для записи
    @param aValues Массив значений для записи
    @param aTime: Время актуальности данных. 
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function WriteAddresses(aAddresses,aValues: Array Of String; aTime: TDateTime = 0): Boolean; virtual;
    { 
    Запись значения по адресу 
    @param aAddress Значение адреса для записи
    @param aValue Значение для записи в строковом представлении
    @param aTime: Время актуальности данных. 
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function WriteAddress(aAddress, aValue: AnsiString; aTime: TDateTime = 0): Boolean; virtual;
    { 
    Запись всех внутренних данных 
    @param aTime: Время актуальности данных. 
                  Если не определено, то берется текущее системное время.
    @return Результат записи - True - запись прошла успешно False - ошибка
    }
    function WriteAll(aTime: TDateTime = 0): Boolean; virtual;

    { Зарегистрировать значения переменных в словаре внутренного состояния }
    function RegState(aValues: TStrDictionary): Boolean;
    { Получить имена записываемых значений в контроллер данных }
    function GetReadValues(): TStringList;

  published
    property Name: AnsiString read GetName write SetName;
    property Properties: TStrDictionary read GetProperties write SetProperties;
    property State: TStrDictionary read FState write FState;

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
@param aAddresses Список адресов для чтения
@param aTime: Время актуальности за которое необходимо получить данные. 
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICObjectProto.Read(aAddresses: TStringList; aTime: TDateTime): TStringList;
begin
  log.WarningMsgFmt('Вызов не определенного метода Read объекта <%s>', [FName]);
  Result := nil;
end;

{ 
Чтение значений по адресам 
@param aAddresses Массив адресов для чтения
@param aTime: Время актуальности за которое необходимо получить данные. 
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICObjectProto.ReadAddresses(aAddresses: Array Of String; aTime: TDateTime): TStringList;
begin
  log.WarningMsgFmt('Вызов не определенного метода ReadAddresses объекта <%s>', [FName]);
  Result := nil;
end;

{ 
Чтение значения по адресу 
@param aAddress Строка адреса для чтения
@param aTime: Время актуальности за которое необходимо получить данные. 
              Если не определено, то берется текущее системное время.
@return Прочитанное значение в виде строки.
}
function TICObjectProto.ReadAddress(aAddress: AnsiString; aTime: TDateTime): AnsiString;
begin
  log.WarningMsgFmt('Вызов не определенного метода ReadAddress объекта <%s>', [FName]);
  Result := '';
end;

{ 
Чтение всех внутренних данных, описанных в свойствах.
@param aTime: Время актуальности за которое необходимо получить данные. 
              Если не определено, то берется текущее системное время.
@return Список прочитанных значений.
}
function TICObjectProto.ReadAll(aTime: TDateTime): TStringList;
begin
  log.WarningMsgFmt('Вызов не определенного метода ReadAll объекта <%s>', [FName]);
  Result := nil;
end;

{ 
Фунция записи данных 
@param aAddresses Список адресов для записи
@param aValues Список значений для записи
@param aTime: Время актуальности данных. 
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICObjectProto.Write(aAddresses, aValues: TStringList; aTime: TDateTime): Boolean;
begin
  log.WarningMsgFmt('Вызов не определенного метода Write объекта <%s>', [FName]);
  Result := False;
end;

{ 
Запись значений по адресам 
@param aAddresses Массив адресов для записи
@param aValues Массив значений для записи
@param aTime: Время актуальности данных. 
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICObjectProto.WriteAddresses(aAddresses,aValues: Array Of String; aTime: TDateTime): Boolean;
begin
  log.WarningMsgFmt('Вызов не определенного метода WriteAddresses объекта <%s>', [FName]);
  Result := False;
end;

{ 
Запись значения по адресу 
@param aAddress Значение адреса для записи
@param aValue Значение для записи в строковом представлении
@param aTime: Время актуальности данных. 
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICObjectProto.WriteAddress(aAddress, aValue: AnsiString; aTime: TDateTime): Boolean;
begin
  log.WarningMsgFmt('Вызов не определенного метода WriteAddress объекта <%s>', [FName]);
  Result := False;
end;

{ 
Запись всех внутренних данных 
@param aTime: Время актуальности данных. 
              Если не определено, то берется текущее системное время.
@return Результат записи - True - запись прошла успешно False - ошибка
}
function TICObjectProto.WriteAll(aTime: TDateTime): Boolean;
begin
  log.WarningMsgFmt('Вызов не определенного метода WriteAll объекта <%s>', [FName]);
  Result := False;
end;

end.

