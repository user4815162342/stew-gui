unit sys_log;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SetLogOutput(const aFilename: UTF8String);
procedure LogException(const aLocationHint: String; E: Exception);
procedure LogAction(const aLocationHint: String; aMessage: String);
procedure LogHeader;

implementation

uses
  LazLogger,
  sys_versionsupport;

procedure SetLogOutput(const aFilename: UTF8String);
begin
  // if we don't set this then it goes to stdout.
  DebugLogger.LogName:=aFilename;
  DebugLogger.CloseLogFileBetweenWrites := true;

end;

procedure LogException(const aLocationHint: String; E: Exception);
var
  I: Integer;
  Frames: PPointer;
begin
  DebugLnEnter(ApplicationName + ' Exception');
  DebugLn('Location Hint: ' + aLocationHint);
  if E <> nil then
  begin
    DebugLn('Exception Class: ' + E.ClassName);
    DebugLn('Message: ' + E.Message);
  end;
  // don't debuglnenter here, because the BackTractStrFunc already adds an indent.
  DebugLn('Stacktrace:');
  DebugLn(BackTraceStrFunc(ExceptAddr));
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    DebugLn(BackTraceStrFunc(Frames[I]));
  DebugLnExit();
end;

procedure LogAction(const aLocationHint: String; aMessage: String);
begin
  DebugLn(ApplicationName + ' ' + aLocationHint + ' ' + aMessage);
end;

procedure LogHeader;
begin
  // Note that this returns a slightly different value before the application
  // finishes loading, but I don't care at this point.
  DebugLnEnter(ApplicationName);
  DebugLn('Run On: ' + DateTimeToStr(Now));
  DebugLn('Version: ' + GetFileVersion);
  DebugLn('Target: ' + GetTargetInfo);
  DebugLnExit();
end;

end.

