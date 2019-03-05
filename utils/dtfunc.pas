{
Модуль функция работы со временными данными

Версия: 0.0.2.2
}

unit dtfunc;
{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWindows}
  Windows,
  {$ENDIF MSWindows}
  Classes, SysUtils, DateUtils, SysConst;

const
  { Шаблон по умолчанию }
  DEFAULT_DT_DELTA_FMT: AnsiString = 'yyyy-mm-dd hh:nn:ss';
  { Разделитель даты }
  DEFAULT_DATE_DELIM: Char = '-';
  { Расчетное количество дней в месяце }
  DEFAULT_DAYS_IN_MONTH: Integer = 30;

  whitespace  = [' ',#13,#10];
  hrfactor    = 1/(24);
  minfactor   = 1/(24*60);
  secfactor   = 1/(24*60*60);
  mssecfactor = 1/(24*60*60*1000);
  AMPMformatting : array[0..2] of string =('am/pm','a/p','ampm');

type

  { Изменения DateTime }
  TDateTimeDelta = class(TObject)
    private
      { Миллисекунды }
      FMSecond: Integer;
      { Секунды }
      FSecond: Integer;
      { Минуты }
      FMinute: Integer;
      { Часы }
      FHour: Integer;
      { Дни }
      FDay: Integer;
      { Месяцы }
      FMonth: Integer;
      { Годы }
      FYear: Integer;

      { Расчетное количество дней в месяце }
      FDaysInMonth: Integer;

    public
      constructor Create();
      destructor Destroy; override;

      {
      Распарсить строку TDateTimeDelta по шаблону
      @param sStrDateTimeDelta Строка для разбора
      @param sFmtDateTimeDelta Строка шаблона.
                               Если указана пустая строка, то берется по умолчанию
      @return TDateTime эквивалент прочитанных данных
      }
      function Scan(sFmtDateTimeDelta: String; const sStrDateTimeDelta: String;
                    const fmt:TFormatSettings; startpos:integer=1): TDateTime;
      function Scan(pattern:string; const s:string; startpos: Integer=1) : TDateTime;
      {
      Пребразовать в строку согласно формату.
      @param sFmtDateTimeDelta Строка шаблона.
                               Если указана пустая строка, то берется по умолчанию
      @return Заполненная строка
      }
      function ToFormat(sFmtDateTimeDelta: AnsiString='';  Options: TFormatDateTimeOptions=[]): AnsiString;

      {
      Преобразование в строку
      }
      procedure ToString(out Result: string; const FormatStr: string; Options : TFormatDateTimeOptions = []);
      procedure ToString(out Result: string; const FormatStr: string; const FormatSettings: TFormatSettings; Options : TFormatDateTimeOptions = []);

      { Увеличить dtDateTime на Delta }
      function IncTo(dtDateTime: TDateTime): TDateTime;
      { Уменьшить dtDateTime на Delta }
      function DecTo(dtDateTime: TDateTime): TDateTime;

      // Свойства
      property MilliSecondDelta: Integer read FMSecond write FMSecond;
      property SecondDelta: Integer read FSecond write FSecond;
      property MinuteDelta: Integer read FMinute write FMinute;
      property HourDelta: Integer read FHour write FHour;
      property DayDelta: Integer read FDay write FDay;
      property MonthDelta: Integer read FMonth write FMonth;
      property YearDelta: Integer read FYear write FYear;
      property DaysInMonth: Integer read FDaysInMonth write FDaysInMonth;
  end;

  // Список даты-времени
  DATETIME_ARRAY = Array[0..65535] of TDateTime;
  PDATETIME_ARRAY = ^DATETIME_ARRAY;

implementation

uses
  log;


procedure RaiseException(const s: string);
begin
  raise EConvertError.Create(s);
end;

constructor TDateTimeDelta.Create();
begin
  inherited Create;
  FMSecond := 0;
  FSecond := 0;
  FMinute := 0;
  FHour := 0;
  FDay := 0;
  FMonth := 0;
  FYear := 0;
  FDaysInMonth := DEFAULT_DAYS_IN_MONTH;
end;

destructor TDateTimeDelta.Destroy;
begin
  inherited Destroy;
end;

{
Распарсить строку TDateTimeDelta по шаблону
@param sStrDateTimeDelta Строка для разбора
@param sFmtDateTimeDelta Строка шаблона.
                         Если указана пустая строка, то берется по умолчанию
@return TDateTime эквивалент прочитанных данных
}
function TDateTimeDelta.Scan(sFmtDateTimeDelta: String; const sStrDateTimeDelta: String;
                             const fmt: TFormatSettings; startpos: integer): TDateTime;
var
  len, ind: Integer;
  yy, mm, dd: Integer;
  timeval: TDateTime;
  activequote: Char;

  procedure IntScanDate(ptrn: PChar; plen: Integer; poffs: Integer);
  // poffs is the offset to
  var
    pind : Integer;

  function FindIMatch(const mnts: Array Of String; p: PChar): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    i := 0;
    while (i <= high(mnts)) and (result = -1) do
      begin
        if AnsiStrLIComp(p, @mnts[i][1], Length(mnts[i])) = 0 then
          Result := i;
        Inc(i);
      end;
  end;

  procedure ArrayMatchError;
  begin
    RaiseException(Format(SNoArrayMatch, [pind + 1, ind]));
    // raise EConvertError.Create(Format(SNoArrayMatch, [pind+1,ind]))
  end;

  function FindMatch(const mnts : Array Of String; const s: String): Integer;
  begin
    Result := FindIMatch(mnts, @s[ind]);
    if Result = -1 then
      ArrayMatchError
    else
      begin
        Inc(ind, Length(mnts[result]) + 1);
        Inc(pind, Length(mnts[result]) + 1);
        Inc(Result); // was 0 based.
      end;
  end;

  var
    pivot,
    i: Integer;

  function ScanFixedInt(maxv: Integer): Integer;
  var
    c : Char;
    oi: Integer;
  begin
    Result := 0;
    oi := ind;
    c := ptrn[pind];
    while (pind < plen) and (ptrn[pind] = c) do
      Inc(pind);
    while (maxv > 0) and (ind <= len) and (sStrDateTimeDelta[ind] IN ['0'..'9']) do
    begin
      Result := Result * 10 + Ord(sStrDateTimeDelta[ind]) - 48;
      Inc(ind);
      Dec(maxv);
    end;
    if oi = ind then
      RaiseException(Format(SPatternCharMismatch, [c, oi]));
  end;

  procedure MatchChar(c: Char);
  begin
    if (ind > len) or (sStrDateTimeDelta[ind] <> c) then
      RaiseException(Format(SNoCharMatch, [sStrDateTimeDelta[ind], c, pind + poffs + 1, ind]));
    Inc(pind);
    Inc(ind);
  end;

  function ScanPatLen: Integer;
  var
    c : Char;
    lind : Integer;
  begin
    Result := pind;
    lind := pind;
    c := ptrn[lind];

    while (lind <= plen) and (ptrn[lind] = c) do
      Inc(lind);
    Result := lind - Result;
  end;

  procedure MatchPattern(const lptr: String);
  var
    len: Integer;
  begin
    len := Length(lptr);
    if len > 0 then
      IntScanDate(@lptr[1], len, pind + poffs);
  end;

  var
    lasttoken, lch : char;
  begin
    pind := 0;
    lasttoken := ' ';
    while (ind <= len) and (pind < plen) do
       begin
         lch := UpCase(ptrn[pind]);
         if activequote = #0 then
            begin
              if (lch = 'M') and (lasttoken = 'H') then
                begin
                  i := ScanPatLen;
                  if i > 2 then
                    RaiseException(Format(Shhmmerror,[poffs+pind+1]));
                  timeval:=timeval+scanfixedint(2)* minfactor;
                end
              else
              case lch of
                 'H':  timeval:=timeval+scanfixedint(2)* hrfactor;
                 'D':  begin
                         i:=scanpatlen;
                         case i of
                            1,2 : dd:=scanfixedint(2);
                            3   : dd:=findmatch(fmt.shortDayNames, sStrDateTimeDelta);
                            4   : dd:=findmatch(fmt.longDayNames, sStrDateTimeDelta);
                            5   : matchpattern(fmt.shortdateformat);
                            6   : matchpattern(fmt.longdateformat);
                           end;
                       end;
                 'N':  timeval:=timeval+scanfixedint(2)* minfactor;
                 'S':  timeval:=timeval+scanfixedint(2)* secfactor;
                 'Z':  timeval:=timeval+scanfixedint(3)* mssecfactor;
                 'Y':  begin
                         i:=scanpatlen;
                         yy:=scanfixedint(i);
                         if i<=2 then
                           begin
                             pivot:=YearOf(now)-fmt.TwoDigitYearCenturyWindow;
                             inc(yy, pivot div 100 * 100);
                             if (fmt.TwoDigitYearCenturyWindow > 0) and (yy < pivot) then
                                inc(yy, 100);
                           end;
                        end;
                 'M': begin
                         i:=scanpatlen;
                         case i of
                            1,2: mm:=scanfixedint(2);
                            3:   mm:=findmatch(fmt.ShortMonthNames, sStrDateTimeDelta);
                            4:   mm:=findmatch(fmt.LongMonthNames, sStrDateTimeDelta);
                            end;
                      end;
                 'T' : begin
                         i:=scanpatlen;
                         case i of
                         1: matchpattern(fmt.shortdateformat);
                         2: matchpattern(fmt.longtimeformat);
                         end;
                       end;
                 'A' : begin
                              i:=findimatch(AMPMformatting, @ptrn[pind]);
                              case i of
                                0: begin
                                     i:=findimatch(['AM','PM'],@sStrDateTimeDelta[ind]);
                                     case i of
                                       0: ;
                                       1: timeval:=timeval+12*hrfactor;
                                     else
                                       arraymatcherror
                                       end;
                                     inc(pind,length(AMPMformatting[0]));
                                     inc(ind,2);
                                   end;
                                1: begin
                                      case upcase(sStrDateTimeDelta[ind]) of
                                       'A' : ;
                                       'P' : timeval:=timeval+12*hrfactor;
                                     else
                                       arraymatcherror
                                       end;
                                     inc(pind,length(AMPMformatting[1]));
                                     inc(ind);
                                   end;
                                 2: begin
                                      i:=findimatch([fmt.timeamstring,fmt.timepmstring],@sStrDateTimeDelta[ind]);
                                      case i of
                                       0: inc(ind,length(fmt.timeamstring));
                                       1: begin
                                            timeval:=timeval+12*hrfactor;
                                            inc(ind,length(fmt.timepmstring));
                                          end;
                                     else
                                       arraymatcherror
                                       end;
                                     inc(pind,length(AMPMformatting[2]));
                                     inc(pind,2);
                                     inc(ind,2);
                                   end;
                              else  // no AM/PM match. Assume 'a' is simply a char
                                  matchchar(ptrn[pind]);
                               end;
                           end;
                 '/' : matchchar(fmt.dateSeparator);
                 ':' : begin
                         matchchar(fmt.TimeSeparator);
                         lch:=lasttoken;
                       end;
                 #39,'"' : begin
                             activequote:=lch;
                             inc(pind);
                           end;
                 'C' : begin
                         intscandate(@fmt.shortdateformat[1],length(fmt.ShortDateFormat),pind+poffs);
                         intscandate(@fmt.longtimeformat[1],length(fmt.longtimeformat),pind+poffs);
                         inc(pind);
                       end;
                 '?' : begin
                         inc(pind);
                         inc(ind);
                       end;
                 #9  : begin
                         while (ind<=len) and (sStrDateTimeDelta[ind] in whitespace) do
                           inc(ind);
                         inc(pind);
                       end;
                 else
                   matchchar(ptrn[pind]);
               end; {case}
               lasttoken:=lch;
              end
            else
              begin
                if activequote=lch then
                  begin
                    activequote:=#0;
                    inc(pind);
                  end
                else
                  matchchar(ptrn[pind]);
              end;
       end;
     if (pind<plen) and (plen>0) and (ptrn[plen-1]<>#9) then  // allow omission of trailing whitespace
       RaiseException(format(SFullpattern,[poffs+pind+1]));
  end;

var plen:integer;

begin
  activequote := #0;
  yy := 0;
  mm := 0;
  dd := 0;
  timeval := 0.0;
  len := length(sStrDateTimeDelta);
  ind := startpos;

  if sFmtDateTimeDelta = '' then
    sFmtDateTimeDelta := DEFAULT_DT_DELTA_FMT;
  plen := length(sFmtDateTimeDelta);
  intscandate(@sFmtDateTimeDelta[1], plen, 0);

  // Блок инициализации внутренних переменных
  FSecond := DateUtils.SecondOf(timeval);
  FMinute := DateUtils.MinuteOf(timeval);
  FHour := DateUtils.HourOf(timeval);
  FDay := dd;
  FMonth := mm;
  FYear := yy;

  // Формаирование результирующего значения
  result := timeval;
  if (yy>0) and (mm>0) and (dd>0) then
     result := result + encodedate(yy, mm, dd);
end;

function TDateTimeDelta.Scan(pattern: String; const s: String; startpos: Integer = 1): TDateTime;
begin
  Result := Scan(pattern, s, DefaultFormatSettings, startpos);
end;
{
Пребразовать в строку согласно формату.
@param sFmtDateTimeDelta Строка шаблона.
                         Если указана пустая строка, то берется по умолчанию
@return Заполненная строка
}
function TDateTimeDelta.ToFormat(sFmtDateTimeDelta: AnsiString; Options: TFormatDateTimeOptions): AnsiString;
begin
  ToString(Result, sFmtDateTimeDelta, FormatSettings, Options);
end;

{   DateTimeToString formats DateTime to the given format in FormatStr   }

procedure TDateTimeDelta.ToString(out Result: string; const FormatStr: string; Options : TFormatDateTimeOptions = []);
begin
  ToString(Result, FormatStr, DefaultFormatSettings, Options);
end;

procedure TDateTimeDelta.ToString(out Result: string; const FormatStr: string; const FormatSettings: TFormatSettings; Options : TFormatDateTimeOptions = []);
var
  ResultLen: integer;
  ResultBuffer: array[0..255] of char;
  ResultCurrent: pchar;
{$IFDEF MSWindows}
  isEnable_E_Format : Boolean;
  isEnable_G_Format : Boolean;
  eastasiainited : boolean;
{$ENDIF MSWindows}

{$IFDEF MSWindows}
  procedure InitEastAsia;
  var
    ALCID : LCID;
    PriLangID , SubLangID : Word;

  begin
    ALCID := GetThreadLocale;
    PriLangID := ALCID and $3FF;
    if (PriLangID>0) then
       SubLangID := (ALCID and $FFFF) shr 10
      else
        begin
          PriLangID := SysLocale.PriLangID;
          SubLangID := SysLocale.SubLangID;
        end;
    isEnable_E_Format := (PriLangID = LANG_JAPANESE)
                  or
                  (PriLangID = LANG_KOREAN)
                  or
                  ((PriLangID = LANG_CHINESE)
                   and
                   (SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                  );
    isEnable_G_Format := (PriLangID = LANG_JAPANESE)
                  or
                  ((PriLangID = LANG_CHINESE)
                   and
                   (SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                  );
    eastasiainited :=true;
  end;
{$ENDIF MSWindows}

  procedure StoreStr(Str: PChar; Len: Integer);
  begin
    if ResultLen + Len < SizeOf(ResultBuffer) then
    begin
      StrMove(ResultCurrent, Str, Len);
      ResultCurrent := ResultCurrent + Len;
      ResultLen := ResultLen + Len;
    end;
  end;

  procedure StoreString(const Str: string);
  var Len: integer;
  begin
   Len := Length(Str);
   if ResultLen + Len < SizeOf(ResultBuffer) then
     begin
       StrMove(ResultCurrent, pchar(Str), Len);
       ResultCurrent := ResultCurrent + Len;
       ResultLen := ResultLen + Len;
     end;
  end;

  procedure StoreInt(Value, Digits: Integer);
  var
    S: string[16];
    Len: integer;
  begin
    System.Str(Value:Digits, S);
    for Len := 1 to Length(S) do
    begin
      if S[Len] = ' ' then
        S[Len] := '0'
      else
        Break;
    end;
    StoreStr(pchar(@S[1]), Length(S));
  end ;

var
  Year, Month, Day, DayOfWeek, Hour, Minute, Second, MilliSecond: word;
  DT : TDateTime;

  procedure StoreFormat(const FormatStr: string; Nesting: Integer; TimeFlag: Boolean);
  var
    Token, lastformattoken, prevlasttoken: char;
    FormatCurrent: pchar;
    FormatEnd: pchar;
    Count: integer;
    Clock12: boolean;
    P: pchar;
    tmp: integer;
    isInterval: Boolean;

  begin
    if Nesting > 1 then  // 0 is original string, 1 is included FormatString
      Exit;

    FormatCurrent := PChar(FormatStr);
    FormatEnd := FormatCurrent + Length(FormatStr);
    Clock12 := false;
    isInterval := false;
    P := FormatCurrent;
    // look for unquoted 12-hour clock token
    while P < FormatEnd do
    begin
      Token := P^;
      case Token of
        '''', '"':
        begin
          Inc(P);
          while (P < FormatEnd) and (P^ <> Token) do
            Inc(P);
        end;
        'A', 'a':
        begin
          if (StrLIComp(P, 'A/P', 3) = 0) or
             (StrLIComp(P, 'AMPM', 4) = 0) or
             (StrLIComp(P, 'AM/PM', 5) = 0) then
          begin
            Clock12 := true;
            break;
          end;
        end;
      end;  // case
      Inc(P);
    end ;
    token := #255;
    lastformattoken := ' ';
    prevlasttoken := 'H';
    while FormatCurrent < FormatEnd do
    begin
      Token := UpCase(FormatCurrent^);
      Count := 1;
      P := FormatCurrent + 1;
      case Token of
        '''', '"':
        begin
          while (P < FormatEnd) and (p^ <> Token) do
            Inc(P);
          Inc(P);
          Count := P - FormatCurrent;
          StoreStr(FormatCurrent + 1, Count - 2);
        end ;
        'A':
        begin
          if StrLIComp(FormatCurrent, 'AMPM', 4) = 0 then
          begin
            Count := 4;
            if Hour < 12 then
              StoreString(FormatSettings.TimeAMString)
            else
              StoreString(FormatSettings.TimePMString);
          end
          else if StrLIComp(FormatCurrent, 'AM/PM', 5) = 0 then
          begin
            Count := 5;
            if Hour < 12 then StoreStr(FormatCurrent, 2)
                         else StoreStr(FormatCurrent+3, 2);
          end
          else if StrLIComp(FormatCurrent, 'A/P', 3) = 0 then
          begin
            Count := 3;
            if Hour < 12 then StoreStr(FormatCurrent, 1)
                         else StoreStr(FormatCurrent+2, 1);
          end
          else
            raise EConvertError.Create('Illegal character in format string');
        end ;
        '/': StoreStr(@FormatSettings.DateSeparator, 1);
        ':': StoreStr(@FormatSettings.TimeSeparator, 1);
    '[': if (fdoInterval in Options) then isInterval := true else StoreStr(FormatCurrent, 1);
    ']': if (fdoInterval in Options) then isInterval := false else StoreStr(FormatCurrent, 1);
        ' ', 'C', 'D', 'H', 'M', 'N', 'S', 'T', 'Y', 'Z', 'F' :
        begin
          while (P < FormatEnd) and (UpCase(P^) = Token) do
            Inc(P);
          Count := P - FormatCurrent;
          case Token of
            ' ': StoreStr(FormatCurrent, Count);
            'Y': begin
              if Count > 2 then
                StoreInt(Year, 4)
              else
                StoreInt(Year mod 100, 2);
            end;
            'M': begin
          if isInterval and ((prevlasttoken = 'H') or TimeFlag) then
            // StoreInt(Minute + (Hour + Trunc(Abs(DateTime)) * 24) * 60, 0)
            StoreInt(Minute + Hour * 60, 0)
          else
              if (lastformattoken = 'H') or TimeFlag then
              begin
                if Count = 1 then
                  StoreInt(Minute, 0)
                else
                  StoreInt(Minute, 2);
              end
              else
              begin
                case Count of
                  1: StoreInt(Month, 0);
                  2: StoreInt(Month, 2);
                  3: StoreString(FormatSettings.ShortMonthNames[Month]);
                else
                  StoreString(FormatSettings.LongMonthNames[Month]);
                end;
              end;
            end;
            'D': begin
              case Count of
                1: StoreInt(Day, 0);
                2: StoreInt(Day, 2);
                3: StoreString(FormatSettings.ShortDayNames[DayOfWeek]);
                4: StoreString(FormatSettings.LongDayNames[DayOfWeek]);
                5: StoreFormat(FormatSettings.ShortDateFormat, Nesting+1, False);
              else
                StoreFormat(FormatSettings.LongDateFormat, Nesting+1, False);
              end ;
            end ;
            'H':
          if isInterval then
            // StoreInt(Hour + trunc(abs(DateTime))*24, 0)
            StoreInt(Hour, 0)
          else
          if Clock12 then
              begin
                tmp := hour mod 12;
                if tmp=0 then tmp:=12;
                if Count = 1 then
                  StoreInt(tmp, 0)
                else
                  StoreInt(tmp, 2);
              end
              else begin
                if Count = 1 then
          StoreInt(Hour, 0)
                else
                  StoreInt(Hour, 2);
              end;
            'N': if isInterval then
               //StoreInt(Minute + (Hour + trunc(abs(DateTime))*24)*60, 0)
               StoreInt(Minute + Hour * 60, 0)
         else
         if Count = 1 then
                   StoreInt(Minute, 0)
                 else
                   StoreInt(Minute, 2);
            'S': if isInterval then
               // StoreInt(Second + (Minute + (Hour + trunc(abs(DateTime))*24)*60)*60, 0)
               StoreInt(Second + (Minute + Hour * 60) * 60, 0)
             else
             if Count = 1 then
                   StoreInt(Second, 0)
                 else
                   StoreInt(Second, 2);
            'Z': if Count = 1 then
                   StoreInt(MilliSecond, 0)
                 else
           StoreInt(MilliSecond, 3);
            'T': if Count = 1 then
           StoreFormat(FormatSettings.ShortTimeFormat, Nesting+1, True)
                 else
               StoreFormat(FormatSettings.LongTimeFormat, Nesting+1, True);
            'C': begin
                   StoreFormat(FormatSettings.ShortDateFormat, Nesting+1, False);
                   if (Hour<>0) or (Minute<>0) or (Second<>0) then
                     begin
                      StoreString(' ');
                      StoreFormat(FormatSettings.LongTimeFormat, Nesting+1, True);
                     end;
                 end;
            'F': begin
                   StoreFormat(FormatSettings.ShortDateFormat, Nesting+1, False);
                   StoreString(' ');
                   StoreFormat(FormatSettings.LongTimeFormat, Nesting+1, True);
                 end;
{$IFDEF MSWindows}
         //   'E':
         //      begin
         //        if not Eastasiainited then InitEastAsia;
         //        if Not(isEnable_E_Format) then StoreStr(@FormatCurrent^, 1)
         //         else
         //          begin
         //            while (P < FormatEnd) and (UpCase(P^) = Token) do
         //            P := P + 1;
         //            Count := P - FormatCurrent;
         //            StoreString(SysUtils.ConvertEraYearString(Count, Year, Month, Day));
         //          end;
         //prevlasttoken := lastformattoken;
         //        lastformattoken:=token;
         //      end;
         //    'G':
         //      begin
         //        if not Eastasiainited then InitEastAsia;
         //        if Not(isEnable_G_Format) then StoreStr(@FormatCurrent^, 1)
         //         else
         //          begin
         //            while (P < FormatEnd) and (UpCase(P^) = Token) do
         //            P := P + 1;
         //            Count := P - FormatCurrent;
         //            StoreString(SysUtils.ConvertEraString(Count, Year, Month, Day));
         //          end;
         //prevlasttoken := lastformattoken;
         //        lastformattoken:=token;
         //      end;
{$ENDIF MSWindows}
          end;
      prevlasttoken := lastformattoken;
          lastformattoken := token;
        end;
        else
          StoreStr(@Token, 1);
      end ;
      Inc(FormatCurrent, Count);
    end;
  end;

begin
{$ifdef MSWindows}
  eastasiainited:=false;
{$endif MSWindows}

Year := FYear;
  Month := FMonth;
  Day := FDay;
  Hour := FHour;
  Minute := FMinute;
  Second := FSecond;
  MilliSecond := FMSecond;

  //DecodeDateFully(DateTime, Year, Month, Day, DayOfWeek);
  //DecodeTime(DateTime, Hour, Minute, Second, MilliSecond);
  ResultLen := 0;
  ResultCurrent := @ResultBuffer[0];
  if FormatStr <> '' then
    StoreFormat(FormatStr, 0, False)
  else
    StoreFormat('C', 0, False);
  ResultBuffer[ResultLen] := #0;
  Result := StrPas(@ResultBuffer[0]);
end ;

{ Увеличить dtDateTime на Delta }
function TDateTimeDelta.IncTo(dtDateTime: TDateTime): TDateTime;
begin
  Result := dtDateTime;

  if FMSecond <> 0 then
    Result := DateUtils.IncMilliSecond(Result, FMSecond);
  if FSecond <> 0 then
    Result := DateUtils.IncSecond(Result, FSecond);
  if FMinute <> 0 then
    Result := DateUtils.IncMinute(Result, FMinute);
  if FHour <> 0 then
    Result := DateUtils.IncHour(Result, FHour);
  if FDay <> 0 then
    Result := DateUtils.IncDay(Result, FDay);
  if FMonth <> 0 then
    Result := DateUtils.IncDay(Result, FMonth * FDaysInMonth);
  if FYear <> 0 then
    Result := DateUtils.IncYear(Result, FYear);
end;

{ Уменьшить dtDateTime на Delta }
function TDateTimeDelta.DecTo(dtDateTime: TDateTime): TDateTime;
begin
  Result := dtDateTime;

  if FMSecond <> 0 then
    Result := DateUtils.IncMilliSecond(Result, -FMSecond);
  if FSecond <> 0 then
    Result := DateUtils.IncSecond(Result, -FSecond);
  if FMinute <> 0 then
    Result := DateUtils.IncMinute(Result, -FMinute);
  if FHour <> 0 then
    Result := DateUtils.IncHour(Result, -FHour);
  if FDay <> 0 then
    Result := DateUtils.IncDay(Result, -FDay);
  if FMonth <> 0 then
    Result := DateUtils.IncDay(Result, -(FMonth * FDaysInMonth));
  if FYear <> 0 then
    Result := DateUtils.IncYear(Result, -FYear);
end;

end.


