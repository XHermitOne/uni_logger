{
Модуль поддержки словарей

Версия: 0.0.2.3
}
unit dictionary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  {
  Класс объектов строк для хранения в словаре
  }
  TObjString = class(TObject)
    private
      { Значение }
      FValue: AnsiString;
    public
      property Value: AnsiString read FValue write FValue;
  end;

  {
  Класс объектов даты-времени для хранения в словаре
  }
  TObjDateTime = class(TObject)
    private
      { Значение }
      FValue: TDateTime;
    public
      property Value: TDateTime read FValue write FValue;
  end;

  {
  Класс объектов списка строк для хранения в словаре
  }
  TObjStringList = class(TObject)
    private
      { Значение }
      FValue: TStringList;
    public
      { Значение. Свойство }
      property Value: TStringList read FValue write FValue;
    end;

  {
  TStrDictionary - Простой словарь в качастве ключей у которого строки.
  }
  TStrDictionary = class(TStringList)
    public
      constructor Create();
      destructor Destroy; override;
      procedure Free;

      {
      Функция очистки содержимого словаря
      @param bAutoFree Автоматическое удаление элементов из памяти
      }
      function ClearContent(bAutoFree: Boolean = True): Boolean;

      {Распечатать содержимое словаря}
      procedure PrintContent(aDictionary: TStrDictionary = nil);

      {Распечатать ключи словаря}
      procedure PrintKeys(aDictionary: TStrDictionary = nil);

      {
      Проверка на существование переменной окружения с таким именем
      @param sKey Проверяемый ключ словаря
      }
      function HasKey(sKey: AnsiString): Boolean;
      {
      Получить ключ по индексу
      @param iIndex Индекс
      }
      function GetKey(iIndex: Integer): AnsiString;
      { Список ключей }
      function GetKeys(): TStringList;
      { Список ключей }
      function GetKeysStr(): AnsiString;
      {
      Получить объект по имени/ключу
      @param sKey Ключ
      }
      function GetByName(sKey: AnsiString): TObject;
      {
      Установить объект в словаре
      @param sKey Ключ
      @param oObject Устанавливаемый объект
      }
      function SetObject(sKey: AnsiString; oObject: TObject): Boolean;

      { Проверка на тустой словарь }
      function IsEmpty(): Boolean;
      {
      Функция обновления словаря по другому словарю
      @param Dictionary Объект словаря
      }
      function Update(Dictionary: TStrDictionary; bAutoFree: Boolean = False): Boolean;
      {
      Функция удаления элемента словаря по ключу
      @param sKey Ключ
      }
      function DelItem(sKey: AnsiString): Boolean;
      {
      Добавить строку в словарь
      @param sKey Ключ
      @param sValue Строковое значение
      }
      function AddStrValue(sKey: AnsiString; sValue: AnsiString): LongInt;
      {
      Получить строку из словаря по ключу
      @param sKey Ключ
      }
      function GetStrValue(sKey: AnsiString): AnsiString;
      {
      Установить строку в словарь
      @param sKey Ключ
      @param sValue Строковое значение
      }
      function SetStrValue(sKey: AnsiString; sValue: AnsiString): Boolean;

      {
      Добавить дату-время в словарь
      @param sKey Ключ
      @param dtValue Значение TDateTime
      }
      function AddDateTimeValue(sKey: AnsiString; dtValue: TDateTime): LongInt;
      {
      Получить дату-время из словаря
      @param sKey Ключ
      }
      function GetDateTimeValue(sKey: AnsiString): TDateTime;
      {
      Установить дату-время в словарь
      @param sKey Ключ
      @param dtValue Значение TDateTime
      }
      function SetDateTimeValue(sKey: AnsiString; dtValue: TDateTime): Boolean;

      {
      Добавить список строк в словарь
      @param sKey Ключ
      @param slValue Значение TStringList
      }
      function AddStrList(sKey: AnsiString; slValue: TStringList): LongInt;
      {
      Получить список строк из словаря
      @param sKey Ключ
      }
      function GetStrList(sKey: AnsiString): TStringList;
      {
      Установить список строк в словарь
      @param sKey Ключ
      @param slValue Значение TStringList
      }
      function SetStrList(sKey: AnsiString; slValue: TStringList): Boolean;

  end;


implementation

uses
  log, memfunc;


constructor TStrDictionary.Create();
begin
  inherited Create;
end;

destructor TStrDictionary.Destroy;
begin
  // ВНИМАНИЕ! Из Destroy необходимо вызывать Free.
  // В Free не должно быть вызова inherited Free.
  // Тогда не происходит утечки памяти
  Free;
  inherited Destroy;
end;

procedure TStrDictionary.Free;
begin
  ClearContent(True);
end;

{
Функция очистки содержимого словаря
@param bAutoFree Автоматическое удаление элементов из памяти
}
function TStrDictionary.ClearContent(bAutoFree: Boolean = True): Boolean;
var
  i: Integer;
  obj: TObject;
begin
  Result := False;
  if Count > 0 then
    if not bAutoFree then
    begin
      Clear;
      Result := True;
    end
    else
    begin
      for i := Count - 1 downto 0 do
      begin
        obj := Objects[i];
        obj.Destroy;
        Delete(i);
      end;
    end;
end;

{
Печать содержимое словаря
}
procedure TStrDictionary.PrintContent(aDictionary: TStrDictionary);
var
  i: Integer;
  item_name: AnsiString;
  item_class: AnsiString;
  msg: AnsiString;
  item_obj: TObject;
begin
  if aDictionary = nil then
    aDictionary := self;

  try
    msg := Format('Содержимое <%s : %s>:', [aDictionary.UnitName, aDictionary.ClassName]);
    log.ServiceMsg(msg);

    for i := 0 to aDictionary.GetCount-1 do
    begin
      item_name := aDictionary.Strings[i];
      item_obj := aDictionary.Objects[i];
      if item_obj <> nil then
      begin
        item_class := item_obj.ClassName;
        if item_class = 'TObjString' then
          item_class := (item_obj As TObjString).Value
        else if item_class = 'TStrDictionary' then
          aDictionary.PrintContent(item_obj As TStrDictionary)
        else
          item_class := Format('<%s>', [item_class]);
      end
      else
        item_class := '<nil>';

        msg := Format(#9'%s'#9'='#9'%s', [item_name, item_class]);
        log.ServiceMsg(msg);
      end;

  except
    log.FatalMsg('Ошибка печати содержания словаря');
  end;
end;

{Распечатать ключи словаря}
procedure TStrDictionary.PrintKeys(aDictionary: TStrDictionary);
var
  i: Integer;
  item_name: AnsiString;
begin
  if aDictionary = nil then
    aDictionary := self;

  try
    log.ServiceMsgFmt('Ключи <%s : %s>:', [UnitName, ClassName]);

    for i := 0 to GetCount-1 do
    begin
      item_name := Strings[i];
      log.ServiceMsgFmt(#9'%s', [item_name]);
    end;
  except
    log.FatalMsg('Ошибка печати ключей словаря');
  end;
end;

{
Проверка на существование переменной окружения с таким именем
}
function TStrDictionary.HasKey(sKey: AnsiString): Boolean;
var
  idx: Integer;
begin
  // PrintContent;
  idx := IndexOf(sKey);
  Result := idx >= 0;
  // log.ServiceMsgFmt('Проверка наличия ключа <%s : %d>', [sKey, idx]);
end;

{
Получить объект по имени.
}
function TStrDictionary.GetByName(sKey: AnsiString): TObject;
var
  idx: Integer;
begin
  idx := IndexOf(sKey);
  if idx >= 0 then
    Result := GetObject(idx)
  else
    Result := nil;
end;

{
Установить объект в словаре
}
function TStrDictionary.SetObject(sKey: AnsiString; oObject: TObject): Boolean;
var
  idx: Integer;
begin
  if not HasKey(sKey) then
  begin
    AddObject(sKey, oObject);
    Result := True;
    Exit;
  end;
  idx := IndexOf(sKey);
  Delete(idx);
  AddObject(sKey, oObject);
  Result := True;
end;

{
Проверка на пустой словарь
}
function TStrDictionary.IsEmpty(): Boolean;
begin
  Result := Count = 0;
end;

{
Добавить строку
}
function TStrDictionary.AddStrValue(sKey: AnsiString; sValue: AnsiString): LongInt;
var
  obj: TObjString;
begin
  obj := TObjString.Create;
  obj.Value := sValue;
  Result := AddObject(sKey, obj);
end;


{ Получить строку из словаря }
function TStrDictionary.GetStrValue(sKey: AnsiString): AnsiString;
var
  obj: TObjString;
begin
  Result := '';
  obj := GetByName(sKey) As TObjString;
  if obj <> nil then
    Result := obj.Value;
end;

{
Установить строку
}
function TStrDictionary.SetStrValue(sKey: AnsiString; sValue: AnsiString): Boolean;
var
  obj: TObjString;
begin
  if not HasKey(sKey) then
  begin
    AddStrValue(sKey, sValue);
    Result := True;
    Exit;
  end;

  obj := GetByName(sKey) As TObjString;
  obj.Value := sValue;
  Result := True;
end;

{
Получить ключ по индексу
}
function TStrDictionary.GetKey(iIndex: Integer): AnsiString;
begin
  Result := Strings[iIndex];
end;

{
Функция обновления словаря по другому словарю.
}
function TStrDictionary.Update(Dictionary: TStrDictionary; bAutoFree: Boolean): Boolean;
var
  i: Integer;
  key: AnsiString;
  obj: TObject;
begin
  if Dictionary = nil then
  begin
    Result := False;
    Exit;
  end;

  for i := 0 to Dictionary.Count - 1 do
  begin
    key := Dictionary.GetKey(i);
    obj := Dictionary.GetObject(i);
    if obj.ClassName = 'TObjString' then
      if HasKey(key) then
        SetStrValue(key, (obj As TObjString).Value)
      else
        // Добавление строкового объекта
        AddStrValue(key, (obj As TObjString).Value)
    else
      //Добавление объекта
      AddObject(key, obj);
  end;

  if bAutoFree then
    Dictionary.Destroy;

  Result := True;
end;

{
Функция удаления элемента словаря
}
function TStrDictionary.DelItem(sKey: AnsiString): Boolean;
var
  idx: Integer;
begin
  idx := IndexOf(sKey);
  Delete(idx);
  Result := True;
end;

{
Список ключей.
}
function TStrDictionary.GetKeys(): TStringList;
var
  keys: TStringList;
  i: Integer;
begin
  keys := TStringList.Create;
  for i := 0 to Count -1 do
    keys.Add(Strings[i]);
  Result := keys;
end;


function TStrDictionary.GetKeysStr(): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    Result := Result + ', ' + Format('''%s''', [Strings[i]]);
  Result := '[' + result + ']';
end;

{
Добавить дату-время
}
function TStrDictionary.AddDateTimeValue(sKey: AnsiString; dtValue: TDateTime): LongInt;
var
  obj: TObjDateTime;
begin
  obj := TObjDateTime.Create;
  obj.Value := dtValue;
  Result := AddObject(sKey, obj);
end;


{
Получить дату-время из словаря
}
function TStrDictionary.GetDateTimeValue(sKey: AnsiString): TDateTime;
var
  obj: TObjDateTime;
begin
  Result := 0;
  obj := GetByName(sKey) As TObjDateTime;
  if obj <> nil then
    Result := obj.Value;
end;

{
Установить дату-время
}
function TStrDictionary.SetDateTimeValue(sKey: AnsiString; dtValue: TDateTime): Boolean;
var
  obj: TObjDateTime;
begin
  if not HasKey(sKey) then
  begin
    AddDateTimeValue(sKey, dtValue);
    Result := True;
    Exit;
  end;

  obj := GetByName(sKey) As TObjDateTime;
  obj.Value := dtValue;
  Result := True;
end;

{
Добавить список строк
}
function TStrDictionary.AddStrList(sKey: AnsiString; slValue: TStringList): LongInt;
var
  obj: TObjStringList;
begin
  obj := TObjStringList.Create;
  obj.Value := slValue;
  Result := AddObject(sKey, obj);
end;


{
Получить дату-время из словаря
}
function TStrDictionary.GetStrList(sKey: AnsiString): TStringList;
var
  obj: TObjStringList;
begin
  Result := nil;
  obj := GetByName(sKey) As TObjStringList;
  if obj <> nil then
    Result := obj.Value;
end;

{
Установить дату-время
}
function TStrDictionary.SetStrList(sKey: AnsiString; slValue: TStringList): Boolean;
var
  obj: TObjStringList;
begin
  if not HasKey(sKey) then
  begin
    AddStrList(sKey, slValue);
    Result := True;
    Exit;
  end;

  obj := GetByName(sKey) As TObjStringList;
  obj.Value := slValue;
  Result := True;
end;

end.

