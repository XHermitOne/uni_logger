# uni_logger
Универсальная система журналирования данных из разных источников в БД

## Инсталяция

Для инсталляции необходимо:

1. uni_logger-setup.exe
2. logger.ico
3. Папка packages

Запустите программу инсталяции uni_logger-setup.exe и следуйте инструкциям мастера.

По умолчанию программа инсталлируется в папку c:\uni_logger

## Введение

Использовать программу можно в 2-х режимах:
1. Как службу Windows
2. В режиме одиночного регистрирования для запуска при помощи планировщика задач

## Использование как службы:

В качестве службы используется программа uni_logger.exe

Инсталляция службы:

``` sh
uni_logger.exe --tick=5000 --install
```

где ключ --tick определяет период обработки в миллисекундах

Запуск службы:

``` sh
net start UniLoggerService
```

Останов службы

``` sh
net stop UniLoggerService
```

Деинсталяция службы

``` sh
uni_logger.exe --uninstall
```

## Режим одиночного регистрирования для запуска при помощи планировщика задач

Для запуска в одиночном режиме используется программа uni_logger_single.exe

Параметры коммандной строки:
  Помощь: uni_logger_single.exe --help
  Режим вывода сообщений в консоль: uni_logger_single.exe --debug
  Режим вывода сообщений в журнал: uni_logger_single.exe --log
  Файл настройки: uni_logger_single.exe --settings=имя_файла_настройки.ini

## Файл настройки

Настройки программа хранит в ini файлах.

Основная секция:

```
[OPTIONS]
; --- Источники данных ---
sources = ['ИМЯ_ИСТОЧНИКА', ...]

; --- Получатели данных ---
destinations = ['ИМЯ_ПРИЕМНИКА', ...]
```

где 
- ИМЯ_ИСТОЧНИКА - имя источника данных латиницей
- ИМЯ_ПРИЕМНИКА - имя БД - приемника данных латиницей

За один цикл обработки программа:
- создает внутренние объекты источников данных
- создает внутеренние объекты приемника данных
- читает запрашиваемые значения из каждого источника 
- и передает значения для записи объектам - приемникам данных

Описание источника данных:
```
[ИМЯ_ИСТОЧНИКА]
type = OPC_HDA
description = Текстовое описание источника данных на русском.
opc_server = Наименование OPC сервера
value_time_count = 24
value_time_tick = 0000-00-00 01:00:00
имя_тега = адрес_тега
```

Поддерживаемые типы источников данных :
1. OPC_DA - OPC Data Access сервера.
2. OPC_HDA - Historical Data Access сервера (В настоящее время не работает)
3. OPC_WT_HDA - Historical Data Access сервера. Взаимодействие через WtHDAClient.dll

Наименование сервера - наименование COM сервера OPC.

Для OPC HDA серверов испоьзуются дополнительные параметры:
1. value_time_count - Количество считываемых записей
2. value_time_tick - Период регистрации контроллера в формате yyyy-mm-dd hh:nn:ss

## Дополнения
Для доступа к OPC серверу через WtHDAClient.dll необходимо скопировать DLL из папки wthdaclient в %SYSTEMROOT%\System32
Регистрация DLL:
%windir%\System32\regsvr32.exe /u WtHDAClient.dll

Описание приемника данных:

```
[LOG_DB]
type = POSTGRESQL_TAB_WIDE
description = Текстовое описание приемника данных на русском.
db_host = хост_БД
db_port = порт_БД
db_name = имя_БД
db_username = имя_пользователя_БД
db_password = пароль_пользователя_БД
table_name = имя_таблицы_бд
fields = ['ИМЯ_ИСТОЧНИКА.имя_тега:тип_поля_в_бд', ...]
```

Поддерживаемые типы приемников данных :
1. POSTGRESQL_TAB_WIDE - БД PostgreSQL таблица широкого формата: Время_регистрации, Значение_тега1, ... Значение_тегаN.

Для сервера PostgreSQL испоьзуются дополнительные параметры:
1. db_host - хост БД
2. db_port - порт БД
3. db_name - имя БД
4. db_username - имя пользователя БД
5. db_password - пароль пользователя БД
6. table_name - имя таблицы в БД
7. fields - описание полей в строковом представлении 'ИМЯ_ИСТОЧНИКА.имя_тега:тип_поля_в_бд'. 
    Тип поля необходимо указывать как в PostgreSQL. Например Text, Float, Integer, Varchar(20) и т.п.

## Дополнения
Для записи данных в PostgreSQL необходимо скопировать DLL из папки packages в %SYSTEMROOT%\System32

Регистрация DLL (x32):
%windir%\System32\regsvr32.exe /u libmysql.dll
%windir%\System32\regsvr32.exe /u libpq.dll

Регистрация DLL (x64):
%windir%\SysWOW64\regsvr32.exe /u libmysql.dll
%windir%\SysWOW64\regsvr32.exe /u libpq.dll

В случае запуска программы черех планировщик задач необходимо проверить запущенали служба Windows OpcEnum.
В случае если эта служба не запущена, то программа не может опросить/запустить OPC сервера и зависает.
Повторный запуск в таком случае программы не производится.

## Дополнения
В случае если не запускается автоматически OPC Server при установке связи с ним
можно его принудительно запускать в автозагрузке.
По пути (для Windows 10) C:\Users\user\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup\
саздается файл *.lnk ярлыка запуска с указанием команды запуска OPC Server

Например для Logika OPC Server необходимо указать комманду:
```
"C:\Program Files (x86)\Logika\OpcServer\DAS.exe"" /autostart
```
