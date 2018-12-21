{
Функции регистрации объектов источников данных
}
unit reg_data_ctrl;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, obj_proto, dictionary;

{
Функция создания объекта контроллера данных по имени типа

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param oParent Родительский объект
@param sTypeName Наименование типа источника/контроллера данных. Прописывается в INI файле в секции контроллера данных параметр 'type'
@param Properties Словарь свойств
@return Созданный объект. Необходимо для использования сделать преобразование типа
}
function CreateRegDataCtrl(oParent: TObject; sTypeName: AnsiString; Properties: TStrDictionary=nil): TICObjectProto;

{
Функция создания объекта контроллера данных по имени типа

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param oParent Родительский объект
@param sTypeName Наименование типа источника/контроллера данных. Прописывается в INI файле в секции контроллера данных параметр 'type'
@param Args Массив свойств
@return Созданный объект. Необходимо для использования сделать преобразование типа
}
function CreateRegDataCtrlArgs(oParent: TObject; sTypeName: AnsiString; const aArgs: Array Of Const): TICObjectProto;

implementation

uses
    log, remoute_opc_node, opc_server_node;
{
Функция создания объекта контроллера данных по имени типа.

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param sTypeName Наименование типа. Прописывается в INI файле в секции контроллера данных параметр 'type'
}
function CreateRegDataCtrl(oParent: TObject; sTypeName: AnsiString; Properties: TStrDictionary): TICObjectProto;
begin
  if sTypeName = 'OPC_DA' then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := TICRemouteOPCNode.Create;
    if oParent <> nil then
        Result.SetParent(oParent);
    if Properties <> nil then
        Result.SetProperties(Properties);
    Exit;
  end
  else if sTypeName = 'POSTGRESQL_TAB_WIDE' then
  begin
    { Создание и инициализация журнала таблицы PostgreSQL широкого формата }
    Result := nil;
    Exit;
  end;

  WarningMsg(Format('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]));
  Result := nil;
end;

{
Функция создания объекта контроллера данных по имени типа.

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param sTypeName Наименование типа. Прописывается в INI файле в секции контроллера данных параметр 'type'
}
function CreateRegDataCtrlArgs(oParent: TObject; sTypeName: AnsiString; const aArgs: Array Of Const): TICObjectProto;
begin
  if sTypeName = 'OPC_DA' then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := TICOPCServerNode.Create;
    if oParent <> nil then
        Result.SetParent(oParent);
    Result.SetPropertiesArray(aArgs);
    exit;
  end
  else if sTypeName = 'POSTGRESQL_TAB_WIDE' then
  begin
    { Создание и инициализация журнала таблицы PostgreSQL широкого формата }
    Result := nil;
    Exit;
  end;

  WarningMsg(Format('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]));
  result := nil;
end;

end.

