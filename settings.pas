{
Модуль поддержки настроек программы
}
unit settings;

{$mode objfpc}{$H+}

interface

uses
    {INIFiles = модуль который содержит класс для работы с INI-файлами}
    Classes, SysUtils, INIFiles, StrUtils,
    inifunc, dictionary;

const DEFAULT_SETTINGS_INI_FILENAME: AnsiString = 'settings.ini';

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
      function BuildSection(sSectionName: Ansistring): TStrDictionary;

      {
      Получить значение опции
      @param sSectionName Наименование секции
      @param sOptionName Наименование параметра
      @return Значение параметра в виде строки
      }
      function GetOptionValue(sSectionName: Ansistring; sOptionName: Ansistring): AnsiString;
    end;

var
   SETTINGS_MANAGER: TICSettingsManager;


implementation

uses
    filefunc, log, config, strfunc;

constructor TICSettingsManager.Create;
begin
     inherited Create;
     FContent := TIniDictionary.Create;
end;

destructor TICSettingsManager.Destroy;
begin
     //FContent.Free;
     inherited Destroy;
end;

procedure TICSettingsManager.Free;
begin
   FContent.Free;
   inherited Free;
end;

{
Генерация имени настроечного INI файла
}
function TICSettingsManager.GenIniFileName(): AnsiString;
var
  cur_path: AnsiString;
begin
     cur_path := ExtractFileDir(ParamStr(0));

     FIniFileName := JoinPath([cur_path, DEFAULT_SETTINGS_INI_FILENAME]);

     DebugMsg(Format('Файл настроек: <%s>', [FIniFileName]));

     result := FIniFileName;
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
        WarningMsg('Не определен INI файл настроек для отображения');
        exit;
     end;
     if not FileExists(FIniFileName) then
     begin
        WarningMsg(Format('Файл настроек программы <%s> не найден', [FIniFileName]));
        exit;
     end;

     ini_file := TIniFile.Create(FIniFileName);
     ServiceMsg(Format('Файл настроек программы <%s>:', [FIniFileName]));
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
                ServiceMsg(Format('[%s]', [section_name]));

                options.Clear;
                ini_file.ReadSectionValues(section_name, options);
                for i_option :=0 to options.Count - 1 do
                begin
                     option := options[i_option];
                     if AnsiStartsStr(';', option) then
                        // Это коментарий обрабатывать не надо
                        continue;
                     ServiceMsg(Format(#9'%s', [option]));
                end;
           end;
        finally
               ini_file.Free;
        end;
     except
           FatalMsg('Ошибка печати настроек программы');
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

    if FileExists(sINIFileName) then
    begin
       FContent.LoadIniFile(sIniFileName);
       if not FContent.IsEmpty then
          // Прописать настройки в окружении
          result := ENVIRONMENT.SetObject('SETTINGS', self)
       else
          WarningMsg(Format('Не определены настройки в INI файле <%s>' , [sIniFileName]));
    end;
    result := False;
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
          WarningMsg(Format('Не определена секция <%s> в настройках', [sSectionName]));
    end
    else
       section.AddStrValue('name', sSectionName);

    if not section.HasKey('parent') then
    begin
        result := section;
        exit;
    end
    else if section.GetStrValue('parent') = '' then
    begin
        section.DelItem('parent');
        result := section;
        exit;
    end
    else if not FContent.HasKey(section.GetStrValue('parent')) then
    begin
        WarningMsg(Format('Запрашиваемая секция <%s> как родительская для <%s> не найдена', [section.GetStrValue('parent'), sSectionName]));
        section.DelItem('parent');
        result := section;
        exit;
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
           result := result_section;
           exit;
        end
        else
        begin
            // Имя родительской секции
            parent_section := BuildSection(section.GetStrValue('parent'));
            parent_section.Update(section);
            parent_section.DelItem('parent');
            result := parent_section;
            exit;
        end;

    result := section;
end;

//def saveSettings(self, sINIFileName=None):
//    """
//    Сохранение настрек в INI файле.
//    @type sINIFileName: C{string}
//    @param sINIFileName: Полное имя конфигурационного файла.
//        Если None, то генерируется.
//    @return: True - запись параметров прошла успешно,
//        False - запись не прошла по какой-либо причине.
//    """
//    if sINIFileName is None:
//        sINIFileName = genINIFileName()
//
//    settings = dict()
//    # Сохранение настроечных переменных в словаре настроек
//
//    log.info('SAVE SETTINGS')
//    if utils.isDebugMode():
//        printSettings(settings)
//
//    return ini.Dict2INI(settings, sINIFileName)

{
Проверка существования файла настройки.
@param (sINIFileName Полное имя конфигурационного файла.
        Если None, то генерируется )
}
function TICSettingsManager.ExistsIniFile(sINIFileName: AnsiString): Boolean;
begin
    if sINIFileName = '' then
        sINIFileName := GenIniFileName();

    result := FileExists(sINIFileName);
end;

{
Получить значение опции
}
function TICSettingsManager.GetOptionValue(sSectionName: Ansistring; sOptionName: Ansistring): AnsiString;
begin
   result := FContent.GetOptionValue(sSectionName, sOptionName);
end;

//var
//  ini_filename: AnsiString;
//
//begin
//  //SETTINGS_MANAGER := TICSettingsManager.Create;
//  //ini_filename := SETTINGS_MANAGER.GenIniFileName();
//  //ENVIRONMENT.AddStrValue('SETTINGS_FILENAME', ini_filename);
//  // SETTINGS_MANAGER.PrintSettings;
end.

