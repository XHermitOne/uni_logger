; uni_logger_install.nsi
;--------------------------------

; Подключение поддержки диалоговых окон
!include nsDialogs.nsh
!include LogicLib.nsh

; Наименование инсталятора
Name "Система журналирования данных из разных источников в БД. Сборка от ${__DATE__}"

; Установить иконку
Icon "logger.ico"

; Результирующий файл
OutFile "uni_logger-setup.exe"

; Инсталяционная директория
InstallDir "c:\uni_logger\"

; Ключ реестра для проверки директории (если вы инсталлируете снова,  то
; старые файлы будут заменены новыми автоматически)
InstallDirRegKey HKLM SOFTWARE\uni_logger "Install_Dir"

; Текст, предлагающий пользователю ввести каталог
ComponentText "Инсталяция системы журналирования данных из разных источников в БД"

; Текст для выбора папки инсталяции
DirText "Инсталяционная папка:"

; Стиль XP
XPStyle on

; Страницы инсталятора
Page components ;Страница выбора инсталируемых пакетов по секциям
Page instfiles ;Страница инсталяции выбранных файлов
Page custom nsDialogsPage nsDialogsPageLeave ;Дополнительная страница пользовательских настроек

; Элементы управления диалога настройки
Var DIALOG_SETTINGS
Var LABEL_HOST
Var LABEL_DBNAME
Var LABEL_USER
Var LABEL_PASSWORD
Var EDIT_HOST
Var EDIT_DBNAME
Var EDIT_USER
Var EDIT_PASSWORD


; === Функции ===
Function nsDialogsPage

    nsDialogs::Create 1018
    Pop $DIALOG_SETTINGS
    ${If} $DIALOG_SETTINGS == error
          Abort
    ${EndIf}

    GetFunctionAddress $0 OnBack
    nsDialogs::OnBack $0

    ${NSD_CreateLabel} 0 0 100% 12u "Хост:"
    Pop $LABEL_HOST
    
    ${NSD_CreateText} 0 20 100% 12u localhost
    Pop $EDIT_HOST

    ${NSD_CreateLabel} 0 50 100% 12u "Имя БД:"
    Pop $LABEL_DBNAME

    ${NSD_CreateText} 0 70 100% 12u registr
    Pop $EDIT_DBNAME
    
    ${NSD_CreateLabel} 0 100 100% 12u "Пользователь:"
    Pop $LABEL_USER

    ${NSD_CreateText} 0 120 100% 12u registr
    Pop $EDIT_USER
    
    ${NSD_CreateLabel} 0 150 100% 12u "Пароль:"
    Pop $LABEL_PASSWORD

    ${NSD_CreatePassword} 0 170 100% 12u ""
    Pop $EDIT_PASSWORD

    nsDialogs::Show

FunctionEnd

Function nsDialogsPageLeave

    ; Сохранить в INI файле настройки
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_port" "5432"

    ${NSD_GetText} $EDIT_HOST $0
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_host" "$0"

    ${NSD_GetText} $EDIT_DBNAME $1
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_name" "$1"

    ${NSD_GetText} $EDIT_USER $2
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_username" "$2"

    ${NSD_GetText} $EDIT_PASSWORD $3
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_password" "$3"

    MessageBox MB_OK "Параметры связи с БД сохранены в проекте:$\nХост: $0$\nБД: $1$\nПользователь: $2"

FunctionEnd

Function OnBack

    MessageBox MB_YESNO "Прекратить настройку?" IDYES +2
    Abort

FunctionEnd

; === Пакеты для инсталляции ===
; --- Обязательные пакеты ---
Section "Система журналирования данных <UNI_LOGGER>"

  ; Установите выходной путь к каталогу установки.
  SetOutPath $INSTDIR

  ; Скопировать туда саморазворачивающийся архив
  File "packages\uni_logger_pack.exe"

  ; Запустить разархивирование
  ExecWait "$INSTDIR\uni_logger_pack.exe"

  ; Удалить в конце архив
  Delete "$INSTDIR\uni_logger_pack.exe"

  ; Скопировать иконку
  File "logger.ico"
  
  ; Создать программу деинсталяции
  WriteUninstaller $INSTDIR\uninstall.exe

SectionEnd


Section "Системные библиотеки"
  ; Скопировать *.dll в system32
  CopyFiles "packages\*.dll" "$SYSDIR"
SectionEnd


Section "Cygwin"
  ; Запустить инсталляцию Cygwin
  ExecWait "packages\setup-x86.exe"
SectionEnd

; необязательный раздел (может быть отключен пользователем)
;--------------------------------
; Деинсталятор

UninstallText "Деинсталяция системы журналирования данных из разных источников в БД. Нажмите Next для продолжения."

Section "Uninstall"

  ; Удалить инсталяционную папку
  RMDir /r "$INSTDIR"
  
  MessageBox MB_OK "Деинсталяция UNI_LOGGER завершена успешно"

SectionEnd
