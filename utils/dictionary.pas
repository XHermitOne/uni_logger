{
Модуль поддержки словарей
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

       {Распечатать содержимое словаря}
       procedure PrintContent();
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
       function Update(Dictionary: TStrDictionary): Boolean;
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
    log;


constructor TStrDictionary.Create();
begin
     inherited Create;
end;

destructor TStrDictionary.Destroy;
begin
     // Free;
     inherited Destroy;
end;

procedure TStrDictionary.Free;
var
   i: Integer;
   obj: TObject;
begin
     for i := 0 to Count - 1 do
     begin
       obj := GetObject(i);
       obj.Free;
     end;
     inherited Free;
end;

{
Печать содержимое словаря
}
procedure TStrDictionary.PrintContent();
var
  i: Integer;
  item_name: AnsiString;
  item_class: AnsiString;
  msg: AnsiString;
  item_obj: TObject;
begin
    try
        msg:=Format('Содержимое <%s : %s>:', [UnitName, ClassName]);
        ServiceMsg(msg);

        for i := 0 to GetCount-1 do
        begin
             item_name := Strings[i];
             item_obj := Objects[i];
             if item_obj <> nil then
             begin
                item_class := item_obj.ClassName;
                if item_class = 'TObjString' then
                   item_class := (item_obj As TObjString).Value
                else
                   item_class := Format('<%s>', [item_class]);
             end
             else
                 item_class := '<nil>';

             msg:=Format(#9'%s'#9'='#9'%s', [item_name, item_class]);
             ServiceMsg(msg);
        end;

    except
          FatalMsg('Ошибка печати содержания окружения');
    end;
end;

{
Проверка на существование переменной окружения с таким именем
}
function TStrDictionary.HasKey(sKey: AnsiString): Boolean;
var
  idx: Integer;
begin
  idx := IndexOf(sKey);
  result := idx >= 0;
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
     result := GetObject(idx)
  else
      result := nil;
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
        result := True;
        exit;
     end;
     idx := IndexOf(sKey);
     Delete(idx);
     AddObject(sKey, oObject);
     result := True;
end;

{
Проверка на пустой словарь
}
function TStrDictionary.IsEmpty(): Boolean;
begin
    result := Count = 0;
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
     result := AddObject(sKey, obj);
end;


{ Получить строку из словаря }
function TStrDictionary.GetStrValue(sKey: AnsiString): AnsiString;
var
   obj: TObjString;
begin
    result := '';
    obj := GetByName(sKey) As TObjString;
    if obj <> nil then
       result := obj.Value;
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
        result := True;
        exit;
     end;

     obj := GetByName(sKey) As TObjString;
     obj.Value := sValue;
     result := True;
end;

{
Получить ключ по индексу
}
function TStrDictionary.GetKey(iIndex: Integer): AnsiString;
begin
     result := Strings[iIndex];
end;

{
Функция обновления словаря по другому словарю.
}
function TStrDictionary.Update(Dictionary: TStrDictionary): Boolean;
var
   i: Integer;
   key: AnsiString;
   obj: TObject;
begin
  if (Dictionary = nil) or (Dictionary.IsEmpty) then
  begin
       result := False;
       exit;
  end;

  for i := 0 to Dictionary.Count - 1 do
  begin
       key := Dictionary.GetKey(i);
       obj := Dictionary.GetObject(i);
       if obj.ClassName = 'TObjString' then
          // Добавление строкового объекта
          AddStrValue(key, (obj As TObjString).Value)
       else
          // Добавление объекта
          AddObject(key, obj);
  end;
  result := True;
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
  result := True;
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
  result := keys;
end;


function TStrDictionary.GetKeysStr(): AnsiString;
var
   i: Integer;
begin
  result := '';
  for i := 0 to Count - 1 do
      result := result + ', ' + Format('''%s''', [Strings[i]]);
  result := '[' + result + ']';
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
     result := AddObject(sKey, obj);
end;


{
Получить дату-время из словаря
}
function TStrDictionary.GetDateTimeValue(sKey: AnsiString): TDateTime;
var
   obj: TObjDateTime;
begin
    result := 0;
    obj := GetByName(sKey) As TObjDateTime;
    if obj <> nil then
       result := obj.Value;
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
        result := True;
        exit;
     end;

     obj := GetByName(sKey) As TObjDateTime;
     obj.Value := dtValue;
     result := True;
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
     result := AddObject(sKey, obj);
end;


{
Получить дату-время из словаря
}
function TStrDictionary.GetStrList(sKey: AnsiString): TStringList;
var
   obj: TObjStringList;
begin
    result := nil;
    obj := GetByName(sKey) As TObjStringList;
    if obj <> nil then
       result := obj.Value;
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
        result := True;
        exit;
     end;

     obj := GetByName(sKey) As TObjStringList;
     obj.Value := slValue;
     result := True;
end;

end.

