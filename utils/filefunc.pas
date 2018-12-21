{
Функции работы с файлами.
}
unit filefunc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, sysfunc, strfunc;

{ Определить папку домашней директории }
function GetHomeDir(): AnsiString;

{ Домашняя папка в Linux системах }
function GetOSLinuxHomeDir(): AnsiString;
{ Домашняя папка в Windows системах }
function GetOSWindowsHomeDir(): AnsiString;

{ Функция соединяет пути с учётом особенностей операционной системы }
function JoinPath(PathParts: Array Of String): AnsiString;

{ Функция разделяет путь на составляющие }
function SplitPath(sPath: AnsiString): TArrayOfString;

{ Создать весь путь папки }
function CreateDirPath(sPath: AnsiString): Boolean;
{ Создать весь путь папки. Путь должен быть нормализован. }
function CreateDirPathTree(sPath: AnsiString): Boolean;

{ Создать пустой файл }
function CreateEmptyFile(sPath: AnsiString): Boolean;

{ Создать пустой файл если он не существует }
function CreateEmptyFileIfNotExists(sPath: AnsiString): Boolean;

{ Нормализовать путь до файла }
function NormalPathFileName(sPath: AnsiString): AnsiString;

implementation

uses
    log;
{
Определить папку домашней директории
}
function GetHomeDir(): AnsiString;
begin
  result := '';
  if IsOSLinux() then
     result := GetOSLinuxHomeDir()
  else if IsOSWindows() then
     result := GetOSWindowsHomeDir()
     else
       WarningMsg(Format('Не поддерживаемая ОС <%s>', [GetOSType()]));
end;

{
Домашняя папка в Linux системах.
}
function GetOSLinuxHomeDir(): AnsiString;
begin
  result := '';
  {$IFDEF linux}
  result := GetEnvironmentVariable('HOME');
  {$ENDIF}
end;

{
Домашняя папка в Windows системах.
}
function GetOSWindowsHomeDir(): AnsiString;
begin
    result := '';
    {$IFDEF windows}
    result := GetAppConfigDir(False);
    {$ENDIF}
end;

{
Функция соединяет пути с учётом особенностей операционной системы.
}
function JoinPath(PathParts: Array Of String): AnsiString;
begin
     result := JoinStr(PathParts, PathDelim);
end;

{
Функция разделяет путь на составляющие.
}
function SplitPath(sPath: AnsiString): TArrayOfString;
begin
     result := SplitStr(sPath, PathDelim);
end;


{
Создать весь путь папки
}
function CreateDirPath(sPath: AnsiString): Boolean;
begin
  result := False;

  // Нормализация пути
  sPath := NormalPathFileName(sPath);

  if not DirectoryExists(sPath) then
  begin
     InfoMsg(Format('Создание папки <%s>', [sPath]));
     result := CreateDirPathTree(sPath);
  end;
end;

{
Создать весь путь папки. Путь должен быть нормализован.
}
function CreateDirPathTree(sPath: AnsiString): Boolean;
var
  parent_path: AnsiString;
begin

  if not DirectoryExists(sPath) then
  begin
    parent_path := ExtractFileDir(sPath);
    if not DirectoryExists(parent_path) then
       result := CreateDirPathTree(parent_path);
    CreateDir(sPath);
    result := True;
    exit;
  end;
  result := False;
end;

{
Создать пустой файл.
}
function CreateEmptyFile(sPath: AnsiString): Boolean;
var
    file_tmp: Text;
begin
    // Нормализация пути
    sPath := NormalPathFileName(sPath);

    InfoMsg(Format('Создание пустого файла <%s>', [sPath]));
    AssignFile(file_tmp, sPath);
    try
       Rewrite(file_tmp);
       Writeln(file_tmp, '');   //Remember AnsiStrings are case sensitive
       CloseFile(file_tmp);
       result := True;
    except
       result := False;
       CloseFile(file_tmp);
    end;
end;

{
Создать пустой файл если он не существует.
}
function CreateEmptyFileIfNotExists(sPath: AnsiString): Boolean;
begin
     result := False;
     if not FileExists(sPath) then
        result := CreateEmptyFile(sPath)
end;

{
Нормализовать путь до файла.
}
function NormalPathFileName(sPath: AnsiString): AnsiString;
begin
     // Замена двойных слешей
     sPath := StringReplace(sPath, PathDelim + PathDelim, PathDelim, [rfReplaceAll]);
     result := ExpandFileName(sPath);
end;

end.

