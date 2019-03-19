; uni_logger_install.nsi
;--------------------------------

; ����������� ��������� ���������� ����
!include nsDialogs.nsh
!include LogicLib.nsh

; ������������ �����������
Name "������� �������������� ������ �� ������ ���������� � ��. ������ �� ${__DATE__}"

; ���������� ������
Icon "logger.ico"

; �������������� ����
OutFile "uni_logger-setup.exe"

; �������������� ����������
InstallDir "c:\uni_logger\"

; ���� ������� ��� �������� ���������� (���� �� ������������� �����,  ��
; ������ ����� ����� �������� ������ �������������)
InstallDirRegKey HKLM SOFTWARE\uni_logger "Install_Dir"

; �����, ������������ ������������ ������ �������
ComponentText "���������� ������� �������������� ������ �� ������ ���������� � ��"

; ����� ��� ������ ����� ����������
DirText "�������������� �����:"

; ����� XP
XPStyle on

; �������� �����������
Page components ;�������� ������ ������������� ������� �� �������
Page instfiles ;�������� ���������� ��������� ������
Page custom nsDialogsPage nsDialogsPageLeave ;�������������� �������� ���������������� ��������

; �������� ���������� ������� ���������
Var DIALOG_SETTINGS
Var LABEL_HOST
Var LABEL_DBNAME
Var LABEL_USER
Var LABEL_PASSWORD
Var EDIT_HOST
Var EDIT_DBNAME
Var EDIT_USER
Var EDIT_PASSWORD


; === ������� ===
Function nsDialogsPage

    nsDialogs::Create 1018
    Pop $DIALOG_SETTINGS
    ${If} $DIALOG_SETTINGS == error
          Abort
    ${EndIf}

    GetFunctionAddress $0 OnBack
    nsDialogs::OnBack $0

    ${NSD_CreateLabel} 0 0 100% 12u "����:"
    Pop $LABEL_HOST
    
    ${NSD_CreateText} 0 20 100% 12u localhost
    Pop $EDIT_HOST

    ${NSD_CreateLabel} 0 50 100% 12u "��� ��:"
    Pop $LABEL_DBNAME

    ${NSD_CreateText} 0 70 100% 12u registr
    Pop $EDIT_DBNAME
    
    ${NSD_CreateLabel} 0 100 100% 12u "������������:"
    Pop $LABEL_USER

    ${NSD_CreateText} 0 120 100% 12u registr
    Pop $EDIT_USER
    
    ${NSD_CreateLabel} 0 150 100% 12u "������:"
    Pop $LABEL_PASSWORD

    ${NSD_CreatePassword} 0 170 100% 12u ""
    Pop $EDIT_PASSWORD

    nsDialogs::Show

FunctionEnd

Function nsDialogsPageLeave

    ; ��������� � INI ����� ���������
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_port" "5432"

    ${NSD_GetText} $EDIT_HOST $0
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_host" "$0"

    ${NSD_GetText} $EDIT_DBNAME $1
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_name" "$1"

    ${NSD_GetText} $EDIT_USER $2
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_username" "$2"

    ${NSD_GetText} $EDIT_PASSWORD $3
    WriteINIStr "$INSTDIR\settings.ini" "LOG_DB" "db_password" "$3"

    MessageBox MB_OK "��������� ����� � �� ��������� � �������:$\n����: $0$\n��: $1$\n������������: $2"

FunctionEnd

Function OnBack

    MessageBox MB_YESNO "���������� ���������?" IDYES +2
    Abort

FunctionEnd

; === ������ ��� ����������� ===
; --- ������������ ������ ---
Section "������� �������������� ������ <UNI_LOGGER>"

  ; ���������� �������� ���� � �������� ���������.
  SetOutPath $INSTDIR

  ; ����������� ���� ��������������������� �����
  File "packages\uni_logger_pack.exe"

  ; ��������� ����������������
  ExecWait "$INSTDIR\uni_logger_pack.exe"

  ; ������� � ����� �����
  Delete "$INSTDIR\uni_logger_pack.exe"

  ; ����������� ������
  File "logger.ico"
  
  ; ������� ��������� ������������
  WriteUninstaller $INSTDIR\uninstall.exe

SectionEnd


Section "��������� ����������"
  ; ����������� *.dll � system32
  CopyFiles "packages\*.dll" "$SYSDIR"
SectionEnd


Section "Cygwin"
  ; ��������� ����������� Cygwin
  ExecWait "packages\setup-x86.exe"
SectionEnd

; �������������� ������ (����� ���� �������� �������������)
;--------------------------------
; ������������

UninstallText "������������ ������� �������������� ������ �� ������ ���������� � ��. ������� Next ��� �����������."

Section "Uninstall"

  ; ������� �������������� �����
  RMDir /r "$INSTDIR"
  
  MessageBox MB_OK "������������ UNI_LOGGER ��������� �������"

SectionEnd
