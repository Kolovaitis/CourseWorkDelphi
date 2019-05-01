unit Parser;

interface

uses System.SysUtils;
function TimeToStr(TimeSec: Integer): String;
function FieldToStr(Cols, Rows: Integer): String;
function CompareStrings(Str1, Str2: String): Boolean;

implementation

function TimeToStr(TimeSec: Integer): String;
var
   Minutes: Integer;
   Seconds: Integer;
   Strmin: string;
   StrSec: string;
begin
   Minutes := TimeSec div 60;
   Seconds := TimeSec mod 60;
   Strmin := IntToStr(Minutes);
   StrSec := IntToStr(Seconds);
   if (Length(Strmin) < 2) then
      Strmin := '0' + Strmin;
   if (Length(StrSec) < 2) then
      StrSec := '0' + StrSec;
   TimeToStr := Strmin + ':' + StrSec;
end;

function FieldToStr(Cols, Rows: Integer): String;
begin
   FieldToStr := IntToStr(Cols) + 'x' + IntToStr(Rows);
end;

function CompareStrings(Str1, Str2: String): Boolean;
var
   Length1, Length2, MaxIndex: Integer;
   i: Integer;
begin
   Length1 := Length(Str1);
   Length2 := Length(Str2);
   Result := Length2 < Length1;
   if (Result) then
      SetLength(Str1, Length2);
   Length1 := Length(Str1);
   i := 1;
   while (i < Length1) and (Str1[i] = Str2[i]) do
      Inc(i);
   if (Str1[i] > Str2[i]) then
      Result := true
   else if (Str1[i] < Str2[i]) then
      Result := false;
end;

end.
