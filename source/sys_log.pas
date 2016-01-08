unit sys_log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure LogException(const aLocationHint: String; E: Exception);

implementation

procedure LogException(const aLocationHint: String; E: Exception);
var
  I: Integer;
  Frames: PPointer;
begin
  WriteLn(ApplicationName + ' Exception');
  WriteLn('Location Hint: ' + aLocationHint);
  if E <> nil then
  begin
    WriteLn('Exception Class: ' + E.ClassName);
    WriteLn('Message: ' + E.Message);
  end;
  WriteLn('Stacktrace:');
  WriteLn(BackTraceStrFunc(ExceptAddr));
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    WriteLn(BackTraceStrFunc(Frames[I]));
end;

end.

