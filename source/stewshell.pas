unit stewshell;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure EditFile(aFile: TFilename);

implementation

uses
  lclintf, FileUtil, UTF8Process;


procedure EditFile(aFile: TFilename);
{$IFDEF Linux}
var
  lApp: string;
begin
  // OpenDocument fails on my linux system (Mint 13 XFCE), at least when
  // the filename contains spaces: the error reported from xdg-open is that
  // the file could not be found. The problem appears to be that it
  // automatically 'single quotes' the file, which xdg-open doesn't like,
  // but I'm not sure if this has anything to do with the spaces in it.
  // Note that it only adds those if I don't already have quotes around it,
  // but if I put quotes around it, then it fails a FileExists check.
  // Anyway, so I've rewritten OpenDocument so that it 1) raises exceptions
  // on failure, and 2) does double quotes instead of single quotes.
  // (my xdg-open appears to handle the spaces, but older versions might
  // not since there was a bug-fix a few years ago, so I'm continuing to
  // put quotes on it, just double instead of single.).

  // TODO: File a bug on this.
    // Android uses this
    if Assigned(OpenDocumentWidgetsetImplementation) then
    begin
      if not OpenDocumentWidgetsetImplementation(aFile) then
        raise Exception.Create('Can''t open file: ' + aFile);
      exit;
    end;

    if not FileExistsUTF8(aFile) then
       raise Exception.Create('File does not exist: ' + aFile);

    lApp:=FindFilenameOfCmd('xdg-open'); // Portland OSDL/FreeDesktop standard on Linux
    if lApp='' then
      lApp:=FindFilenameOfCmd('kfmclient'); // KDE command
    if lApp='' then
      lApp:=FindFilenameOfCmd('gnome-open'); // GNOME command
    if lApp='' then
      raise Exception.Create('No opening program is installed for desktop environment');

    if (aFile<>'') and (aFile[1]<>'"') then
      aFile:=AnsiQuotedStr(aFile,'"');
    RunCmdFromPath(lApp,aFile);

end;
{$ELSE}
begin
  if not OpenDocument(aFile) then
     raise Exception.Create('Can''t open file: ' + aFile);
end;
{$ENDIF}


end.

