{
Модуль поддержки окружения программы

ВНИМАНИЕ! В этом модуле нельзя использовать модуль log т.к. происходит
      взаимное использование модулей и выполнение программы сваливается
      в <Segmentation fault>.

Версия: 0.0.1.1
}
unit config;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, dictionary;

type

    {
    TICEnvironment - Менеджер окружения программы
    }

    TICEnvironment = class(TStrDictionary)
    end;

var
  {
  Объявление глобального объекта окружения

  ВНИМАНИЕ! Глобальные переменные описываются в секции interface.
  Переменные определенные в секции implementation являются статическими для
  модуля.
  }
  ENVIRONMENT: TICEnvironment;

implementation

begin
  ENVIRONMENT := TICEnvironment.Create;
end.

