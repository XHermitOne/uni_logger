{
Функции регистрации объектов источников данных

Версия: 0.0.3.1
}
unit reg_data_ctrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  obj_proto, dictionary;

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
  log,
  // Компоненты - источники данных
  opc_da_node, opc_hda_node, opc_wt_hda_node, db_mysql_node,
  // Компоненты - приемники данных
  postgresql_tab_wide;

{
Функция создания объекта контроллера данных по имени типа.

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param sTypeName Наименование типа. Прописывается в INI файле в секции контроллера данных параметр 'type'
}
function CreateRegDataCtrl(oParent: TObject; sTypeName: AnsiString; Properties: TStrDictionary): TICObjectProto;
begin
  if sTypeName = opc_da_node.OPC_DA_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := opc_da_node.TICOPCDANode.Create;
  end
  else if sTypeName = opc_hda_node.OPC_HDA_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := opc_hda_node.TICOPCHDANode.Create;
  end
  else if sTypeName = opc_wt_hda_node.OPC_WT_HDA_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := opc_wt_hda_node.TICWtOPCHDANode.Create;
  end
  //else if sTypeName = remoute_opc_node.REMOUTE_OPC_NODE_TYPE then
  //begin
  //  { Создание и инициализация OPC DA сервера }
  //  Result := TICRemouteOPCNode.Create;
  //end
  else if sTypeName = db_mysql_node.DB_MYSQL_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := db_mysql_node.TICMySQLDBNode.Create;
  end
  else if sTypeName = postgresql_tab_wide.POSTGRESQL_TAB_WIDE_TYPE then
  begin
    { Создание и инициализация журнала таблицы PostgreSQL широкого формата }
    Result := postgresql_tab_wide.TICPostgreSQLTableWide.Create;
  end
  else
  begin
    log.WarningMsgFmt('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]);
    Result := nil;
  end;

  if Result <> nil then
  begin
    if oParent <> nil then
      Result.SetParent(oParent);
    if Properties <> nil then
      Result.SetProperties(Properties);
  end;
end;

{
Функция создания объекта контроллера данных по имени типа.

ВНИМАНИЕ! После создания нового типа контроллера данных необходимо
прописать блок создания объекта по наименованию типа.
@param sTypeName Наименование типа. Прописывается в INI файле в секции контроллера данных параметр 'type'
}
function CreateRegDataCtrlArgs(oParent: TObject; sTypeName: AnsiString; const aArgs: Array Of Const): TICObjectProto;
begin
  if sTypeName = opc_da_node.OPC_DA_NODE_TYPE then
  begin
    { Создание и инициализация OPC DA сервера }
    Result := opc_da_node.TICOPCDANode.Create;
    if oParent <> nil then
        Result.SetParent(oParent);
    Result.SetPropertiesArray(aArgs);
    Exit;
  end
  else if sTypeName = 'POSTGRESQL_TAB_WIDE' then
  begin
    { Создание и инициализация журнала таблицы PostgreSQL широкого формата }
    Result := nil;
    Exit;
  end;

  log.WarningMsgFmt('Не поддерживаемый тип объекта контроллера данных <%s>', [sTypeName]);
  Result := nil;
end;

end.

