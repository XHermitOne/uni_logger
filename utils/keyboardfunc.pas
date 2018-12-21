{
Функции работы с клавиатурой
}
unit keyboardfunc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

{ Коды клавиш }
const ESC_KEY: Integer = 27;
const ENTER_KEY: Integer = 13;
const SPACE_KEY: Integer = 32;


{
Проверка соответствия символа коду клавиши
@param cChar Символ
@param iKeyCode Код клавиши
@return True - соответствует / False - не соответствует
}
function SameKey(cChar: Char; iKeyCode: Integer): Boolean;

implementation

{
Проверка соответствия символа коду клавиши.
@param cChar Символ
@param iKeyCode Код клавиши
@return True - соответствует / False - не соответствует
}
function SameKey(cChar: Char; iKeyCode: Integer): Boolean;
begin
    result := Ord(cChar) = iKeyCode;
end;

end.

