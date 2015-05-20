unit sys_os;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_async, sys_file;

type

  { TRunSimpleCommand }

  TRunSimpleCommand = class(TDeferredTask)
  private
    fCmd: String;
    fArgs: Array of String;
    fCallback: TDeferredStringCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(aCmd: String; const aArgs: array of string;
      aOutput: TDeferredStringCallback; aErrorBack: TDeferredExceptionCallback);

  end;

  { TTemplateLister }

  { TListTemplates }

  TListTemplates = class
  private
    fExt: String;
    fCallback: TTemplateListCallback;
    fErrorback: TDeferredExceptionCallback;
    {$IFDEF Linux}
      fTemplatePath: String;
    {$ENDIF}
  protected
    procedure Failed(Data: String);
  {$IFDEF Linux}
    procedure XdgUserDirDone(Data: String);
    procedure TemplateFilesListed(Data: TFile.TFileArray);
  {$ENDIF}
  public
    constructor Create(aExt: String; aCallback: TTemplateListCallback; aErrorBack: TDeferredExceptionCallback);
    procedure Enqueue;
  end platform;

procedure EditFile(aFile: TFile);
procedure RunSimpleCommand(aCmd: String; const aArgs: array of string;
      aOutput: TDeferredStringCallback; aErrorBack: TDeferredExceptionCallback);
procedure GetTemplatesForExt(const aExt: String; aCallback: TTemplateListCallback; aErrorback: TDeferredExceptionCallback);
procedure CreateFileFromTemplate(aTemplate: TTemplate; aFile: TFile; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
procedure RunDetachedProcess(const aExecutable: String; aArgs: array of String);

implementation

uses
  lclintf, FileUtil, process, UTF8Process, sys_localfile, dialogs;



procedure EditFile(aFile: TFile);
{$IFDEF Linux}
var
  lApp: string;
  lArg: String;
{$ENDIF}
begin
{$IFDEF Linux}
  // The official FPC OpenDocument fails on my linux system (Mint 13 XFCE), at least when
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
      if not OpenDocumentWidgetsetImplementation(aFile.ID) then
        raise Exception.Create('Can''t open file: ' + aFile.ID);
      exit;
    end;

    if not FileExistsUTF8(aFile.ID) then
       raise Exception.Create('File does not exist: ' + aFile.ID);

    lApp:=FindFilenameOfCmd('xdg-open'); // Portland OSDL/FreeDesktop standard on Linux
    if lApp='' then
      lApp:=FindFilenameOfCmd('kfmclient'); // KDE command
    if lApp='' then
      lApp:=FindFilenameOfCmd('gnome-open'); // GNOME command
    if lApp='' then
      raise Exception.Create('No known opening command is available for your desktop environment');

    lArg := aFile.ID;
    if (lArg <> '') and (lArg[1] <>'"') then
      lArg:=AnsiQuotedStr(lArg,'"');
    RunCmdFromPath(lApp,lArg);

{$ELSE}
  if not OpenDocument(aFile) then
     raise Exception.Create('Can''t open file: ' + aFile);
{$ENDIF}
end;

procedure RunSimpleCommand(aCmd: String; const aArgs: array of string;
  aOutput: TDeferredStringCallback; aErrorBack: TDeferredExceptionCallback);
begin
  TRunSimpleCommand.Create(aCmd,aArgs,aOutput,aErrorBack).Enqueue;
end;

procedure GetTemplatesForExt(const aExt: String;
  aCallback: TTemplateListCallback; aErrorback: TDeferredExceptionCallback
  );
begin
  TListTemplates.Create(aExt,aCallback,aErrorback).Enqueue;
end;

procedure CreateFileFromTemplate(aTemplate: TTemplate; aFile: TFile;
  aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
begin
{$IFDEF Linux}
  LocalFile(aTemplate.ID).CopyTo(aFile,aCallback,aErrorback)
{$ELSE}
{$ERROR Required code is not yet written for this platform.}
{$ENDIF}
end;

procedure RunDetachedProcess(const aExecutable: String; aArgs: array of String);
var
  Process: TProcess;
  i: Integer;
begin
  // NOTE: Trust me, this works. Just not when debugging the application, in that
  // case, something is killing off all of the orphaned processes created this
  // way -- I wonder if the debugger process becomes the parent when this is
  // detached, the way a shell becomes a parent if I detach from there.
  Process := TProcess.Create(nil);
  try
    Process.InheritHandles := False;
    Process.Options := [];
    Process.ShowWindow := swoShow;

    // Copy default environment variables including DISPLAY variable for GUI application to work
    for I := 0 to GetEnvironmentVariableCount - 1 do
      Process.Environment.Add(GetEnvironmentString(I));

    Process.Executable := aExecutable;

    for i := 0 to Length(aArgs) - 1 do
      Process.Parameters.Add(aArgs[i]);
    Process.Execute;
  finally
    Process.Free;
  end;
end;

{ TRunSimpleCommand }

procedure TRunSimpleCommand.DoTask;
var
  lApp: String;
  lOut: String;
begin
  lApp := FindDefaultExecutablePath(fCmd);
  if lApp <> '' then
  begin
    lOut := '';
    if RunCommand(lApp,fArgs,lOut) then
       fCallback(lOut)
    else
      raise Exception.Create('Command "' + fCmd + '" failed to run properly');
  end
  else
     raise Exception.Create('Command "' + fCmd + '" could not be found on your system');
end;

constructor TRunSimpleCommand.Create(aCmd: String; const aArgs: array of string;
  aOutput: TDeferredStringCallback; aErrorBack: TDeferredExceptionCallback);
var
  i: Integer;
  l: Integer;
begin
  inherited Create(aErrorBack);
  fCmd := aCmd;
  l := Length(aArgs);
  // can't assign "open array" to "dynamic array". Which really doesn't
  // make sense, since they're both declared the same, but it's always been
  // a problem, IIRC.
  SetLength(fArgs,l);
  for i := 0 to l - 1 do
  begin
    fArgs[i] := aArgs[i];
  end;
  fCallback := aOutput;
end;

{ TTemplateLister }

{$IFDEF Linux}

procedure TListTemplates.TemplateFilesListed(Data: TFile.TFileArray);
var
  aAnswer: TTemplateArray;
  l: Integer;
  i: Integer;
  j: Integer;
begin
  try
    l := Length(Data);
    j := 0;
    for i := 0 to l - 1 do
    begin
      if (fExt = '') or (Data[i].Extension = fExt) then
      begin
        SetLength(aAnswer,j + 1);
        aAnswer[j].Name := Data[i].BaseName;
        aAnswer[j].ID := Data[i].ID;
        j := j + 1;
      end;
    end;
    fCallback(aAnswer);
  except
    on E: Exception do
    begin
      Failed(E.Message);
      // Failed already freed us, so don't do anything more.
      exit;
    end;
  end;
  // process is complete, so free it.
  Free;
end;

procedure TListTemplates.XdgUserDirDone(Data: String);
begin
  try
    fTemplatePath := IncludeTrailingPathDelimiter(Trim(Data));
    LocalFile(fTemplatePath).List(@TemplateFilesListed,@Failed);
  except
    on E: Exception do
      Failed(E.Message);
  end;
end;
{$ENDIF}

procedure TListTemplates.Failed(Data: String);
begin
  fErrorback(Data);
  // an error occurred, so we're done.
  Free;
end;


constructor TListTemplates.Create(aExt: String;
  aCallback: TTemplateListCallback; aErrorBack: TDeferredExceptionCallback);
begin
  inherited Create;
  fErrorback := aErrorBack;
  fCallback := aCallback;
  fExt := ExcludeExtensionDelimiter(aExt);
end;

procedure TListTemplates.Enqueue;
begin
{$IFDEF Linux}
{ There are two possible ways to do this:
1. Read the configuration files directly:
   a. Figure out where the config home directory is
      i. get $XDG_CONFIG_HOME
      ii. If that's blank, get $HOME and add '.config'
   b. Read $XDG_CONFIG_HOME/user-dirs.dirs
   c. Look for a XDG_TEMPLATES_DIR and read in the value
   d. Expand environment variables in the value, such as $HOME
   e. And now we have our directory. Or,
2. Run xdg-user-dir "TEMPLATES" and the directory will be in the output.

I think I know which way *I* want to do it, at least until we find out that it
doesn't work. }
  RunSimpleCommand('xdg-user-dir',['TEMPLATES'],@XdgUserDirDone,@Failed);

{$ELSE}
{$ERROR Required code is not yet written for this platform.}
{-- Windows
   - much more complex, since it uses the registry
   - http://mc-computing.com/WinExplorer/WinExplorerRegistry_ShellNew.htm
     -- that document is marked for Windows 95, however, so I'm not certain
        how it pertains to Windows 7, 8, etc. But, other sources seem to
        imply it should still work.
   - because the process of *adding* a template is much more involved, I
     may want to allow a secondary search through an application directory
     for templates as well.
-- Mac
   - after rudimentary research, it appears that Mac doesn't support this
     type of thing. The closest thing they have is the ability to mark a
     file as a "stationery pad", which means that when you open it, it will
     copy the file (in a temp directory?) and open that in some way. Assuming
     I still have to choose the location when saving the document later,
     this isn't at all helpful.
     - So, I think for the Mac, I have to create my own template directory
       anyway.}
{$ENDIF}
end;

end.

