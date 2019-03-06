{
Модуль поддержки настроек программы

Версия: 0.0.2.4
}
unit settings;

{$mode objfpc}{$H+}

interface

uses
  {INIFiles - модуль который содержит класс для работы с INI-файлами}
  Classes, SysUtils, INIFiles, StrUtils,
  inifunc, dictionary, strfunc, exttypes;

// const DEFAULT_SETTINGS_INI_FILENAME: AnsiString = 'settings.ini';

type
  {
  TICSettingsManager - Менеджер управления настройками программы
  }
  TICSettingsManager = class(TObject)
  private
    { Полное наименование INI файла }
    FIniFileName: AnsiString;
    { Содержимое INI файла в виде словаря словарей (разложено по секциям)}
    FContent: TIniDictionary;

  public

    constructor Create;
    destructor Destroy; override;
    procedure Free;

    {Генерация имени настроечного INI файла}
    function GenIniFileName(): AnsiString;
    { Вывод на экран текущих настроек для отладки. }
    procedure PrintSettings();
    {
    Загрузка параметров из INI файла
    @param sINIFileName Полное наименование INI Файла
    @return True/False
    }
    function LoadSettings(sINIFileName: AnsiString): Boolean;
    {
    Проверка существования файла настройки
    @param sINIFileName Полное наименование INI Файла
    @return True/False
    }
    function ExistsIniFile(sINIFileName: AnsiString): Boolean;
    {
    Собрать полное описание секции с учетом ключа parent
    @param sSectionName Наименование секции
    @return Словарь описания
    }
    function BuildSection(sSectionName: AnsiString): TStrDictionary;

    {
    Получить значение опции
    @param sSectionName Наименование секции
    @param sOptionName Наименование параметра
    @return Значение параметра в виде строки
    }
    function GetOptionValue(sSectionName: AnsiString; sOptionName: AnsiString): AnsiString;

    {
    Получить список имен опций одной секции
    @param sSectionName Наименование секции
    @return Список имен параметров указанной секции
    }
    function GetOptionNameList(sSectionName: AnsiString): TStringList;

    {
    Получить список имен секций
    @return Список имен секций
    }
    function GetSectionNameList(): TStringList;
    {
    Получить список имен опций одной секции
    @param sSectionName Наименование секции
    @return Список имен параметров указанной секции
    }
    function GetOptionNames(sSectionName: AnsiString): TArrayOfString;

    {
    Получить список имен секций
    @return Список имен секций
    }
    function GetSectionNames(): TArrayOfString;

  end;

var
  { Имя файла настроек }
  SETTINGS_INI_FILENAME: AnsiString = 'settings.ini';

  SETTINGS_MANAGER: TICSettingsManager;


implementation

uses
  filefunc, log;

constructor TICSettingsManager.Create;
begin
  inherited Create;
  FContent := TIniDictionary.Create;
end;

destructor TICSettingsManager.Destroy;
begin
  Free;
  inherited Destroy;
end;

procedure TICSettingsManager.Free;
begin
  FContent.Destroy;
end;

{
Генерация имени настроечного INI файла
}
function TICSettingsManager.GenIniFileName(): AnsiString;
var
  cur_path: AnsiString;
begin
  cur_path := ExtractFileDir(ParamStr(0));

  FIniFileName := JoinPath([cur_path, SETTINGS_INI_FILENAME]);

  log.DebugMsgFmt('Файл настроек: <%s>', [FIniFileName]);

  Result := FIniFileName;
end;

{
Вывод на экран текущих настроек для отладки.
}
procedure TICSettingsManager.PrintSettings();
var
  i_section, i_option: Integer;
  ini_file: TIniFile;
  sections: TStringList;
  section_name: AnsiString;
  options: TStringList;
  option: AnsiString;
begin
  if FIniFileName = '' then
  begin
    log.WarningMsg('Не определен INI файл настроек для отображения');
    Exit;
  end;
  if not FileExists(FIniFileName) then
  begin
    log.WarningMsgFmt('Файл настроек программы <%s> не найден', [FIniFileName]);
    Exit;
  end;

  ini_file := TIniFile.Create(FIniFileName);
  log.ServiceMsgFmt('Файл настроек программы <%s>:', [FIniFileName]);
  // ВНИМАНИЕ! Перед использованием списков строк в функции
  // надо их создать/выделить под них память
  sections := TStringList.Create;
  options := TStringList.Create;
  try
    try
      ini_file.ReadSections(sections);
      for i_section :=0 to sections.Count - 1 do
      begin
        section_name := sections[i_section];
        log.ServiceMsgFmt('[%s]', [section_name]);

        options.Clear;
        ini_file.ReadSectionValues(section_name, options);
        for i_option :=0 to options.Count - 1 do
        begin
          option := options[i_option];
          if AnsiStartsStr(';', option) then
            // Это коментарий обрабатывать не надо
            continue;
          log.ServiceMsgFmt(#9'%s', [option]);
        end;
      end;
    finally
      ini_file.Free;
    end;
  except
    log.FatalMsg('Ошибка печати настроек программы');
  end;
  // ВНИМАНИЕ! В конце обязательно освободить память
  options.Free;
  sections.Free;
end;

{
Загрузка настроек из INI файла.
@param (sINIFileName Полное имя конфигурационного файла.
        Если не определено, то генерируется.)
@return: (True - загрузка параметров прошла успешно,
          False - загрузка не прошла по какой-либо причине)
}
function TICSettingsManager.LoadSettings(sINIFileName: AnsiString): Boolean;
begin
  if sINIFileName = '' then
    sINIFileName := GenINIFileName();

  Result := False;
  if FileExists(sINIFileName) then
  begin
    Result := FContent.LoadIniFile(sIniFileName);
    if FContent.IsEmpty then
    begin
      Result := False;
      log.WarningMsgFmt('Не определены настройки в INI файле <%s>' , [sIniFileName]);
    end;
  end;
end;

{
Собрать полное описание секции с учетом ключа parent.
Через ключ parent можно наследовать описание секции.
@param (sSectionName Наименование запрашиваемой секции)
@return (Словарь секции дополненный переменными из секции указанной в parent.
    Сборка данных производиться рекурсивно.)
}
function TICSettingsManager.BuildSection(sSectionName: Ansistring): TStrDictionary;
var
  section, parent_section, result_section: TStrDictionary;
  obj_section: TStrDictionary;
  i: Integer;
  parent_section_list: Array Of String;
  parent_section_name: AnsiString;
begin
  section := TStrDictionary.Create;
  if FContent.HasKey(sSectionName) then
  begin
    obj_section := FContent.GetByName(sSectionName) As TStrDictionary;
    obj_section.AddStrValue('name', sSectionName);
    // DebugMsg(Format('Класс секции <%s>', [obj_section.ClassName]));
    if obj_section <> nil then
      section.Update(obj_section)
    else
      log.WarningMsgFmt('Не определена секция <%s> в настройках', [sSectionName]);
  end
  else
    section.AddStrValue('name', sSectionName);

  if not section.HasKey('parent') then
  begin
    Result := section;
    Exit;
  end
  else if section.GetStrValue('parent') = '' then
  begin
    section.DelItem('parent');
    Result := section;
    Exit;
  end
  else if not FContent.HasKey(section.GetStrValue('parent')) then
  begin
    log.WarningMsgFmt('Запрашиваемая секция <%s> как родительская для <%s> не найдена', [section.GetStrValue('parent'), sSectionName]);
    section.DelItem('parent');
    Result := section;
    Exit;
  end
  else
    if IsParseStrList(section.GetStrValue('parent')) then
    begin
      // Список имен
      parent_section_list := ParseStrArray(section.GetStrValue('parent'));
      result_section := TStrDictionary.Create;
      for i := 0 to Length(parent_section_list) - 1 do
      begin
        parent_section_name := parent_section_list[i];
        parent_section := BuildSection(parent_section_name);
        result_section.Update(parent_section);
      end;
      result_section.Update(section);
      result_section.DelItem('parent');
      Result := result_section;
      Exit;
    end
    else
    begin
      // Имя родительской секции
      parent_section := BuildSection(section.GetStrValue('parent'));
      parent_section.Update(section);
      parent_section.DelItem('parent');
      Result := parent_section;
      Exit;
    end;

  Result := section;
end;

{
Проверка существования файла настройки.
@param (sINIFileName Полное имя конфигурационного файла.
        Если None, то генерируется )
}
function TICSettingsManager.ExistsIniFile(sINIFileName: AnsiString): Boolean;
begin
  if sINIFileName = '' then
    sINIFileName := GenIniFileName();

  Result := FileExists(sINIFileName);
end;

{
Получить значение опции
}
function TICSettingsManager.GetOptionValue(sSectionName: Ansistring; sOptionName: Ansistring): AnsiString;
begin
  Result := FContent.GetOptionValue(sSectionName, sOptionName);
end;

{
Получить список имен опций одной секции
@param sSectionName Наименование секции
@return Список имен параметров указанной секции
}
function TICSettingsManager.GetOptionNameList(sSectionName: AnsiString): TStringList;
var
  section: TStrDictionary;
begin
  if FContent.HasKey(sSectionName) then
  begin
    section := FContent.GetByName(sSectionName) As TStrDictionary;
    if section <> nil then
    begin
      Result := section.GetKeys();
    end;
  end;
end;

{
Получить список имен секций
@return Список имен секций
}
function TICSettingsManager.GetSectionNameList(): TStringList;
begin
  Result := FContent.GetKeys();
end;

{
Получить список имен опций одной секции
@param sSectionName Наименование секции
@return Список имен параметров указанной секции
}
function TICSettingsManager.GetOptionNames(sSectionName: AnsiString): TArrayOfString;
var
  i: Integer;
  option_names: TStringList;
  section: TStrDictionary;
begin

  if FContent.HasKey(sSectionName) then
  begin
    section := FContent.GetByName(sSectionName) As TStrDictionary;
    if section <> nil then
    begin
      // Result := section.GetStrValue(sOptionName);
      option_names := section.GetKeys();

      SetLength(Result, option_names.Count);
      for i := 0 to option_names.Count - 1 do
        Result[i] := option_names[i];

      option_names.Free;
    end;
  end;
end;

{
Получить список имен секций
@return Список имен секций
}
function TICSettingsManager.GetSectionNames(): TArrayOfString;
var
  i: Integer;
  section_names: TStringList;
begin
  section_names := FContent.GetKeys();

  SetLength(Result, section_names.Count);
  for i := 0 to section_names.Count - 1 do
    Result[i] := section_names[i];

  section_names.Free;
end;

end.

