unit sys_os;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_async, sys_file;

type

  { TRunSimpleCommandTask }

  TRunSimpleCommandTask = class(TQueuedTask)
  private
    fCmd: String;
    fArgs: Array of String;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aCmd: String; const aArgs: array of string);
  end;

  TRunSimpleCommandPromise = class(TPromise)
  private
    fData: String;
  public
    property Data: String read fData;
  end;

  { TLocalFileListTemplatesFromOSTask }

  TLocalFileListTemplatesFromOSTask = class(TQueuedTask)
  protected type
    TLocalFileListTemplatesPromise = class(TFileListTemplatesPromise);
  protected
    fPath: Tfile;
  {$IFDEF Linux}
    procedure XdgUserDirDone(Sender: TPromise);
    procedure TemplateFilesListed(Sender: TPromise);
  {$ENDIF}
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aPath: TFile);
  end platform;

function RunSimpleCommand(aCmd: String; const aArgs: array of string): TRunSimpleCommandTask;
function CreateFileFromTemplate(aTemplate: TTemplate; aFile: TFile): TFileCopyPromise;
procedure EditFile(aFile: TFile);
procedure RunDetachedProcess(const aExecutable: String; aArgs: array of String);

implementation

uses
  lclintf, FileUtil, process, UTF8Process, sys_localfile;



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

function RunSimpleCommand(aCmd: String; const aArgs: array of string
  ): TRunSimpleCommandTask;
begin
  result := TRunSimpleCommandTask.Enqueue(aCmd,aArgs);
end;

function CreateFileFromTemplate(aTemplate: TTemplate; aFile: TFile
  ): TFileCopyPromise;
begin
{$IFDEF Linux}
  result := LocalFile(aTemplate.ID).CopyTo(aFile)
{$ELSE}
{$ERROR Required code is not yet written for this platform.}
{$ENDIF}
end;

procedure RunDetachedProcess(const aExecutable: String; aArgs: array of String);
var
  Process: TProcess;
  i: Integer;
begin
  // FROM:
  // http://wiki.lazarus.freepascal.org/Executing_External_Programs#Run_detached_program

  // NOTE: Trust me, this works. Just not when debugging the application, in that
  // case, something is killing off all of the orphaned processes created this
  // way -- I wonder if the debugger process becomes the parent when this is
  // detached, the way a shell becomes a parent if I detach from there. However,
  // when not debugging, the process remains alive after I kill the first.
  Process := TProcess.Create(nil);
  try
    Process.InheritHandles := False;
    Process.Options := [];
    Process.ShowWindow := swoShow;

    // Copy default environment variables including DISPLAY variable for GUI application to work
    for I := 1 to GetEnvironmentVariableCount do
    begin
      Process.Environment.Add(GetEnvironmentString(I));
    end;

    Process.Executable := aExecutable;

    for i := 0 to Length(aArgs) - 1 do
      Process.Parameters.Add(aArgs[i]);
    Process.Execute;
  finally
    Process.Free;
  end;
end;

{ TRunSimpleCommandTask }

procedure TRunSimpleCommandTask.DoTask;
var
  lApp: String;
  lOut: String;
begin
  lApp := FindDefaultExecutablePath(fCmd);
  if lApp <> '' then
  begin
    lOut := '';
    if RunCommand(lApp,fArgs,lOut) then
    begin
      (Promise as TRunSimpleCommandPromise).fData := lOut;
      Resolve;
    end
    else
      raise Exception.Create('Command "' + fCmd + '" failed to run properly');
  end
  else
     raise Exception.Create('Command "' + fCmd + '" could not be found on your system');
end;

function TRunSimpleCommandTask.CreatePromise: TPromise;
begin
  result := TRunSimpleCommandPromise.Create;
end;

constructor TRunSimpleCommandTask.Enqueue(aCmd: String;
  const aArgs: array of string);
var
  i: Integer;
  l: Integer;
begin
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
  inherited Enqueue;
end;

{ TLocalFileListTemplatesFromOSTask }

procedure TLocalFileListTemplatesFromOSTask.TemplateFilesListed(
  Sender: TPromise);
var
  lAnswer: TTemplateArray;
  lExt: String;
  l: Integer;
  i: Integer;
  j: Integer;
  lData: TFileArray;
begin
  lExt := fPath.Extension;
  lData := (Sender as TFileListPromise).GetJustFiles;
  l := Length(lData);
  j := 0;
  for i := 0 to l - 1 do
  begin
    if (lExt = '') or (lData[i].Extension = lExt) then
    begin
      SetLength(lAnswer,j + 1);
      lAnswer[j].Name := lData[i].BaseName;
      lAnswer[j].ID := lData[i].ID;
      j := j + 1;
    end;
  end;
  (Promise as TLocalFileListTemplatesPromise).fTemplates := lAnswer;
  Resolve;
end;

procedure TLocalFileListTemplatesFromOSTask.XdgUserDirDone(Sender: TPromise);
var
  lTemplatePath: String;
begin
  lTemplatePath := IncludeTrailingPathDelimiter(Trim((Sender as TRunSimpleCommandPromise).Data));
  LocalFile(lTemplatePath).List.After(@TemplateFilesListed,@SubPromiseRejected);
end;

procedure TLocalFileListTemplatesFromOSTask.DoTask;
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
  TRunSimpleCommandTask.Enqueue('xdg-user-dir',['TEMPLATES']).After(@XdgUserDirDone,@SubPromiseRejected);

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

function TLocalFileListTemplatesFromOSTask.CreatePromise: TPromise;
begin
  result := TLocalFileListTemplatesPromise.Create(fPath);
end;

constructor TLocalFileListTemplatesFromOSTask.Enqueue(aPath: TFile);
begin
  fPath := aPath;
  inherited Enqueue;
end;

end.

