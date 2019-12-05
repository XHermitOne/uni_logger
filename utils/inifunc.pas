{
Классы работы с INI файлами

Версия: 0.0.3.1
}
unit inifunc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, INIFiles, StrUtils, dictionary;

const
  MEMO_SIGNATURE: AnsiString = 'MEMO: ';

type
    {
    TIniDictionary - Словарь словарей для хранения содержимого INI файла
    с разделением данных по секциям.
    }
    TIniDictionary = class(TStrDictionary)
    private
      { Полное наименование последнего загруженного INI файла }
      FINIFileName: AnsiString;

    public

      constructor Create();
      constructor Create(sINIFileName: AnsiString);
      destructor Destroy; override;
      procedure Free;

      {
      Загрузить содержимое INI файла
      @param sINIFileName Полное наименование INI файла
      @return True - загрузка прошла успешно / False - ошибка
      }
      function LoadIniFile(sINIFileName: AnsiString): Boolean;
      {
      Получить значение параметра
      @param sSectionName Наименование секции
      @param sOptionName Наименование параметра
      @return Строка значения указанного параметра. Если параметр не существует то возвращает пустую строку
      }
      function GetOptionValue(sSectionName: AnsiString; sOptionName: AnsiString): AnsiString;

      {
      Загрузить текст из текстового файла.
      Связь с текстовыми файлами используется для вынесения многострочного текста
      за пределы INI файла.
      Текстовый файл должен находиться в той же папке что и INI файл.
      Загрузка производиться по сигнатуре MEMO:
      @param sTxtFileName: Полное имя текстового файла.
      @return: Текст, содержащийся внутри файла в виде строки
      или пустая строка в случае ошибки.
      }
      function ReadTxtFile(sTxtFileName: AnsiString): AnsiString;

    published
      property INIFileName: AnsiString read FINIFileName;

    end;

implementation

uses
  log, filefunc, strfunc;

constructor TIniDictionary.Create();
begin
  inherited Create;
end;

constructor TIniDictionary.Create(sINIFileName: AnsiString);
begin
  inherited Create;
  LoadIniFile(sINIFileName);
end;

destructor TIniDictionary.Destroy;
begin
  Free;
  inherited Destroy;
end;

procedure TIniDictionary.Free;
begin
  ClearContent(True);
end;

{
Загрузить содержимое INI файла
}
function TIniDictionary.LoadIniFile(sINIFileName: AnsiString): Boolean;
var
  i_section, i_option, idx: Integer;
  ini_file: TIniFile;
  sections, options: TStringList;
  section_name, option, option_name, option_value: AnsiString;
  section_dict: TStrDictionary;

begin
  Result := False;
  if sIniFileName = '' then
  begin
    log.WarningMsg('Не определен INI файл для загрузки данных');
    Exit;
  end;
  if not FileExists(sIniFileName) then
  begin
    log.WarningMsgFmt('Файл INI <%s> не найден', [sIniFileName]);
    Exit;
  end;

  // Запомнить полное имя последнего открытого INI файла
  FINIFileName := sINIFileName;

  ini_file := TIniFile.Create(sIniFileName);

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
        section_dict := TStrDictionary.Create;

        options.Clear;
        ini_file.ReadSectionValues(section_name, options);
        for i_option :=0 to options.Count - 1 do
        begin
          option := Trim(options[i_option]);
          if AnsiStartsStr(';', option) then
            // Это коментарий обрабатывать не надо
            continue;
          idx := Pos('=', option);
          option_name := Copy(option, 0, idx - 1);
          option_value := Copy(option, idx + 1, Length(option) - idx);

          // Значение опции может начинаться с сигнатуры.
          // Такие случаи необходимо обрабатывать отдельно
          if strfunc.IsStartsWith(option_value, MEMO_SIGNATURE) then
          begin
            // Значение опции храниться в отдельном текстовом многострочном файле
            option_value := filefunc.ReadTxtFile(strfunc.ReplaceStart(option_value, MEMO_SIGNATURE, ''));
          end;

          section_dict.AddStrValue(option_name, option_value);
        end;
        AddObject(section_name, section_dict);
      end;
      Result := True;
    finally
      ini_file.Free;
    end;
  except
    log.FatalMsg('Ошибка загрузки настроек программы');
  end;
  // ВНИМАНИЕ! В конце обязательно освободить память
  options.Destroy;
  sections.Destroy;
end;

{
Получить значение параметра
}
function TIniDictionary.GetOptionValue(sSectionName: AnsiString; sOptionName: AnsiString): AnsiString;
var
  section: TStrDictionary;
begin
  Result := '';
  if HasKey(sSectionName) then
  begin
    section := GetByName(sSectionName) As TStrDictionary;
    if section <> nil then
      Result := section.GetStrValue(sOptionName);
  end;
end;

{
Загрузить текст из текстового файла.
Связь с текстовыми файлами используется для вынесения многострочного текста
за пределы INI файла.
Текстовый файл должен находиться в той же папке что и INI файл.
Загрузка производиться по сигнатуре TXT:
@param sTxtFileName: Краткое имя текстового файла.
@return: Текст, содержащийся внутри файла в виде строки
или пустая строка в случае ошибки.
}
function TIniDictionary.ReadTxtFile(sTxtFileName: AnsiString): AnsiString;
var
  txt_filename: AnsiString;
  parent_path: AnsiString;

begin
  Result := '';

  if sTxtFileName = '' then
  begin
    log.WarningMsg('Не определен текстовый файл многострочных данных');
    Exit;
  end;

  // Получить полное имя файла
  parent_path := ExtractFileDir(INIFileName);
  txt_filename := parent_path + PathDelim + sTxtFileName;

  if not FileExists(txt_filename) then
  begin
    log.WarningMsgFmt('Текстовый файл многострочных данных <%s> не найден', [txt_filename]);
    Exit;
  end;

  Result := filefunc.ReadTxtFile(txt_filename);
end;

end.

