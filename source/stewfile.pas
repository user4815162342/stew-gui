unit stewfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewasync, FileUtil;

// TODO: Need to turn this whole system into something which will allow
// me to work with other file systems.
// - Instead of string-based file paths, create a record that has an enum
//   value indicating what type of file system it is, and a string-based id.
// - The main functions would accept these file paths, and would look
//   at that enum value to figure out what object will be used to
//   do the job. This object will be retrieved from a class factory,
//   and the functionality will be called on that object.
// - Alternatively, instead of enum, use a "Class of TFileSystemHandler"
//   or something like that. If I can do virtual static functions, than
//   that's all I need. Otherwise, can use a static constructor to establish
//   an incremental ID of some sort, if we were to keep it in an array,
//   and retrieve itself that way.
// - Actually, can use extended records to add procedures and the like,
//   and we have an almost real *TFile* class.

const NewFileAge: Longint = -1;
function ExtractPacketName(const aPath: String): String;
function ExtractFileDescriptor(const Filename: TFilename): string;
function IncludeExtensionDelimiter(const aExt: String): String;


type
  TFileSystemKind = (fskLocalFile);
  // FUTURE: The following systems might be available some day.
  // - ioskHTTP: read-only system that reads stuff off of a web server
  // - ioskWebDAV: writable http system
  // - ioskSSHFS: secure networked file system
  // - ioskGoogleDrive: access Google drive
  // - ioskDropBox: access Dropbox.
  // The biggest problem with most of these is actually the directory browser
  // to choose the location.

  // TODO: Should be TFileArray
  TFileList = array of TFilename;
  TDeferredFileListCallback = specialize GDeferredCallback<TFileList>;
  TReadFileCallback = procedure(aData: TStream; aFileAge: Longint) of object;
  TWriteFileCallback = procedure(aFileAge: Longint) of object;

  { TListFiles }

  TListFiles = class(TDeferredTask)
  private
    fPath: TFilename;
    fCallback: TDeferredFileListCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFilename; aCallback: TDeferredFileListCallback; aErrorBack: TDeferredExceptionCallback);
  end;

  { TFileExists }

  TFileExists = class(TDeferredTask)
  private
    fPath: TFilename;
    fCallback: TDeferredBooleanCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFilename; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
  end;

  { TReadFile }

  TReadFile = class(TDeferredTask)
  private
    fPath: TFilename;
    fCallback: TReadFileCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFilename; aCallback: TReadFileCallback; aErrorback: TDeferredExceptionCallback);
  end;


  { TWriteFile }

  TWriteFile = class(TDeferredTask)
  private
    fPath: TFilename;
    fFileAge: Longint;
    fCheckFileAge: Boolean;
    fCreateDir: Boolean;
    fData: UTF8String;
    fCallback: TWriteFileCallback;
    fConflictBack: TWriteFileCallback;
  protected
    procedure DoTask; override;
  public
    // NOTE: for FileAge, pass the mtime retrieved by the TReadFile.
    // If the file was new, pass NewFileAge(-1).
    constructor Create(const aPath: TFilename; aCreateDir: Boolean; aCheckFileAge: Boolean; aFileAge: Longint;
      const aData: UTF8String; aCallback: TWriteFileCallback; aConflictBack: TWriteFileCallback;
  aErrorback: TDeferredExceptionCallback);
  end;

  { TCopyFile }

  TCopyFile = class(TDeferredTask)
  private
    fSource: TFilename;
    fTarget: TFilename;
    fOptions: TCopyFileFlags;
    fCallback: TDeferredCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(aSource: TFileName; aTarget: TFileName; aFlags: TCopyFileFlags; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
  end;

  { TRenameFiles }

  TRenameFiles = class(TDeferredTask)
  private
    fSources: array of string;
    fTargets: array of string;
    fCallback: TDeferredCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(aSources: array of string; aTargets: array of string; aCallback: TDeferredCallback; aErrorBack: TDeferredExceptionCallback);
  end;



  { TIOSystem }

  TFileSystem = class
  private
    class var fList: array[TFileSystemKind] of TFileSystem;
  public
    procedure ListFiles(const aPath: TFilename; aCallback: TDeferredFileListCallback; aErrorBack: TDeferredExceptionCallback); virtual; abstract;
    procedure CheckFileExistence(const aPath: TFilename; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;
    procedure ReadFile(const aPath: TFilename; aCallback: TReadFileCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;
    procedure WriteFile(const aPath: TFilename;
                        const aData: UTF8String;
                        aCallback: TWriteFileCallback;
                        aErrorback: TDeferredExceptionCallback); overload; virtual; abstract;
    procedure WriteFile(const aPath: TFilename;
                        aCreateDir: Boolean;
                        aCheckFileAge: Boolean;
                        aFileAge: Longint;
                        const aData: UTF8String;
                        aCallback: TWriteFileCallback;
                        aConflictBack: TWriteFileCallback;
                        aErrorback: TDeferredExceptionCallback); overload; virtual; abstract;
    procedure CopyFile(aSource: TFileName; aTarget: TFileName; aFlags: TCopyFileFlags; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); overload; virtual; abstract;
    procedure CopyFile(aSource: TFileName; aTarget: TFileName; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); overload; virtual; abstract;
    procedure RenameFiles(aPath: TFilename; aSourceFiles: array of string; aTargetFiles: array of string; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;
    class function Get(aKind: TFileSystemKind): TFileSystem;
    class constructor Create;
    class destructor Destroy;
  end;

  TLocalFileIOSystem = class(TFileSystem)
    // TODO: Move the below "global" procedures into here.
    // TODO: Also, create a class system for each kind of asynchronous
    // thing, so that the actual task is implemented by another system.
  end;

procedure ListFiles(const aPath: TFilename; aCallback: TDeferredFileListCallback; aErrorBack: TDeferredExceptionCallback);
procedure CheckFileExistence(const aPath: TFilename; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
procedure ReadFile(const aPath: TFilename; aCallback: TReadFileCallback; aErrorback: TDeferredExceptionCallback);
procedure WriteFile(const aPath: TFilename;
                    const aData: UTF8String;
                    aCallback: TWriteFileCallback;
                    aErrorback: TDeferredExceptionCallback); overload;
procedure WriteFile(const aPath: TFilename;
                    aCreateDir: Boolean;
                    aCheckFileAge: Boolean;
                    aFileAge: Longint;
                    const aData: UTF8String;
                    aCallback: TWriteFileCallback;
                    aConflictBack: TWriteFileCallback;
                    aErrorback: TDeferredExceptionCallback); overload;
procedure CopyFile(aSource: TFileName; aTarget: TFileName; aFlags: TCopyFileFlags; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); overload;
procedure CopyFile(aSource: TFileName; aTarget: TFileName; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); overload;
procedure RenameFiles(aSourceFiles: array of string; aTargetFiles: array of string; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); overload;

implementation

uses
  strutils;

function ExtractPacketName(const aPath: String): String;
var
  _p: Integer;
begin
  result := ChangeFileExt(ExtractFileName(aPath),'');
  _p := RPos('_',Result);
  if _p <> 0 then
  begin
     result := Copy(Result,0,_p - 1);
  end;
end;

function ExtractFileDescriptor(const Filename: TFilename): string;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+['_'];
  while (I > 0) and not (FileName[I] in EndSep) do
    Dec(I);
  if (I > 0) and (FileName[I] = '_') then
  begin
    Result := Copy(FileName, I, MaxInt);
    Result := ChangeFileExt(Result,'');
  end
  else
    Result := '';
end;

function IncludeExtensionDelimiter(const aExt: String): String;
begin
  result := aExt;
  if (Length(result) > 0) then
  begin
    if result[1] <> '.' then
      result := '.' + result;
  end
  else
    result := '.';
end;

procedure ListFiles(const aPath: TFilename;
  aCallback: TDeferredFileListCallback; aErrorBack: TDeferredExceptionCallback);
begin
  TListFiles.Create(aPath,aCallback,aErrorBack).Enqueue;
end;

procedure CheckFileExistence(const aPath: TFilename;
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  TFileExists.Create(aPath,aCallback,aErrorback).Enqueue;
end;

procedure ReadFile(const aPath: TFilename; aCallback: TReadFileCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TReadFile.Create(aPath,aCallback,aErrorback).Enqueue;
end;

procedure WriteFile(const aPath: TFilename; const aData: UTF8String;
  aCallback: TWriteFileCallback; aErrorback: TDeferredExceptionCallback);
begin
  WriteFile(aPath,false,false,NewFileAge,aData,aCallback,nil,aErrorback);
end;

procedure WriteFile(const aPath: TFilename; aCreateDir: Boolean;
  aCheckFileAge: Boolean; aFileAge: Longint; const aData: UTF8String;
  aCallback: TWriteFileCallback; aConflictBack: TWriteFileCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TWriteFile.Create(aPath,aCreateDir,aCheckFileAge,aFileAge,aData,aCallback,aConflictBack,aErrorback).Enqueue;
end;

procedure CopyFile(aSource: TFileName; aTarget: TFileName;
  aFlags: TCopyFileFlags; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TCopyFile.Create(aSource,aTarget,aFlags,aCallback,aErrorback).Enqueue;
end;

procedure CopyFile(aSource: TFileName; aTarget: TFileName;
  aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
begin
  CopyFile(aSource,aTarget,[],aCallback,aErrorback);
end;

procedure RenameFiles(aSourceFiles: array of string;
  aTargetFiles: array of string; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TRenameFiles.Create(aSourceFiles,aTargetFiles,aCallback,aErrorback).Enqueue;
end;


const
  // formats in something like ISO8601, but I also need to quote the
  // hyphens, because otherwise they will be replaced with locale
  // date separators.
  TimestampFormat: String = 'yyyy"-"mm"-"dd"T"hh"-"mm"-"ss';

{ TIOSystem }

class function TFileSystem.Get(aKind: TFileSystemKind): TFileSystem;
begin
  if fList[aKind] = nil then
  begin
    case aKind of
      fskLocalFile:
        fList[aKind] := TLocalFileIOSystem.Create;
    end;
  end;
  result := fList[aKind];
end;

class constructor TFileSystem.Create;
var
  i: TFileSystemKind;
begin
  for i := Low(TFileSystemKind) to High(TFileSystemKind) do
    fList[i] := nil;
end;

class destructor TFileSystem.Destroy;
var
  i: TFileSystemKind;
begin
  for i := Low(TFileSystemKind) to High(TFileSystemKind) do
    FreeAndNil(fList[i]);
end;

{ TRenameFiles }

procedure TRenameFiles.DoTask;
var
  i: Integer;
begin
  for i := 0 to Length(fSources) - 1 do
  begin
    if fSources[i] <> fTargets[i] then
      if not RenameFile(fSources[i],fTargets[i]) then
         raise Exception.Create('An error occurred while renaming ' + fSources[i] + ' to ' + fTargets[i]);
  end;
  fCallback;
end;

constructor TRenameFiles.Create(aSources: array of string;
  aTargets: array of string; aCallback: TDeferredCallback;
  aErrorBack: TDeferredExceptionCallback);
var
  i: Integer;
  l: Integer;
begin
  inherited Create(aErrorBack);
  l := Length(aSources);
  if l <> Length(aTargets) then
     raise Exception.Create('Number of targets does not match number of sources.');
  SetLength(fSources,l);
  SetLength(fTargets,l);
  for i := 0 to l - 1 do
  begin
    fSources[i] := aSources[i];
    fTargets[i] := aTargets[i];
  end;
  fCallback:= aCallback;
end;

{ TCopyFile }

procedure TCopyFile.DoTask;
begin
  // have to raise our own exceptions here, since CopyFile doesn't do that
  // if it's not in the options.
  if not (cffOverwriteFile in fOptions) and
     FileExists(fTarget) then
     raise Exception.Create('File ' + fTarget + ' already exists.');

  if not (cffCreateDestDirectory in fOptions) and
     not DirectoryExists(ExtractFilePath(fTarget)) then
     raise Exception.Create('Directory for file ' + fTarget + ' does not exist');

  if not CopyFile(fSource,fTarget,fOptions) then
     raise Exception.Create('Copy file failed. Source: ' + fSource + '; Target: ' + fTarget);
  fCallback;
end;

constructor TCopyFile.Create(aSource: TFileName; aTarget: TFileName;
  aFlags: TCopyFileFlags; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fSource := aSource;
  fTarget := aTarget;
  fOptions := aFlags;
  fCallback := aCallback;
end;

{ TListFiles }

procedure TListFiles.DoTask;
var
  SR: TSearchRec;
  Answer: TFileList;
  L: Integer;
begin
  L := 0;
  if DirectoryExists(fPath) then
  begin
       if FindFirst(IncludeTrailingPathDelimiter(fPath) + '*',faDirectory,SR) = 0 then
       begin
          try
            repeat
              if (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                 L := L + 1;
                 SetLength(Answer,L);
                 Answer[L-1] := SR.Name;
              end;
            until FindNext(SR) <> 0;
          finally
            FindClose(SR);
          end;
       end;
  end;
  fCallback(Answer);
end;

constructor TListFiles.Create(const aPath: TFilename;
  aCallback: TDeferredFileListCallback; aErrorBack: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fCallback := aCallback;
end;

{ TWriteFile }

// TODO: At some point, I need to 'test' the save conflicts.
procedure TWriteFile.DoTask;
var
  stream: TFileStream;
  aCurrentAge: Longint;
begin
  // FUTURE: There is a potential race condition here. The file could
  // be modified between this file age check and the actual write.
  // However, by the time we get done with TFileStream.Create, there's
  // no way to check the previous creation time. So... we'll just have
  // to hope things aren't going too fast. I could possibly fix this by locking
  // the file, checking the mtime, then opening it, but I've never done that
  // before so I'd have to do some research that I don't have time for right now.
  if fCheckFileAge then
  begin
    aCurrentAge := FileAge(fPath);
    // if the file does not exist yet, it should return -1. If the file
    // did not exist before, the caller should have passed -1 to indicate a
    // new file. These are the possible conditions I see:
    // fFileAge  fCurrentAge   means
    // -1        -1            file did not exist before, file does not exist now, no conflict, write as normal.
    // -1        > -1          file did not exist before, file does exist now, conflict, so report and don't write.
    // > -1      = fFileAge    file existed before, file has same mtime as before, no conflict, write as normal.
    // > -1      <> fFileAgge  file existed before, file has different mtime now, conflict, so report and don't write.
    if aCurrentAge <> fFileAge then
    begin
      if fConflictBack <> nil then
        fConflictBack(aCurrentAge)
      else
        raise Exception.Create('File has been changed since last read. Can''t write.');
    end;
  end;

  if not DirectoryExists(ExtractFileDir(fPath)) and fCreateDir then
  begin
     ForceDirectories(ExtractFileDir(fPath));
  end;
  // otherwise, an error might occur, but that should be handled and reported by the base class here.

  // TODO: Once I'm sure that saving is being done right, get rid
  // of this.
  if FileExists(fPath) then
    CopyFile(fPath,fPath + FormatDateTime(TimestampFormat, Now));
  stream := TFileStream.Create(fPath,fmCreate or fmShareDenyWrite);
  try
    aCurrentAge := FileAge(fPath);
    stream.Write(fData[1],Length(fData));
  finally
    stream.Free;
  end;
  fCallback(aCurrentAge);

end;

constructor TWriteFile.Create(const aPath: TFilename; aCreateDir: Boolean;
  aCheckFileAge: Boolean; aFileAge: Longint; const aData: UTF8String;
  aCallback: TWriteFileCallback; aConflictBack: TWriteFileCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fFileAge := aFileAge;
  fCheckFileAge := aCheckFileAge;
  fCreateDir := aCreateDir;
  fData := aData;
  fCallback := aCallback;
  fConflictBack := aConflictBack;

end;


{ TReadFile }

procedure TReadFile.DoTask;
var
  stream: TFileStream;
  age: Longint;
begin
  if FileExists(fPath) then
  begin
     stream := TFileStream.Create(fPath,fmOpenRead or fmShareDenyWrite);
     try
       // get the age of the file now, while it's locked, to prevent certain
       // race conditions
       age := FileAge(fPath);
       fCallback(stream,age);
     finally
       stream.Free;
     end;
  end
  else
  begin
    fCallback(nil,NewFileAge);
  end;

end;

constructor TReadFile.Create(const aPath: TFilename;
  aCallback: TReadFileCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fCallback := aCallback;
  fPath := aPath;
end;

{ TFileExists }

procedure TFileExists.DoTask;
begin
  fCallback(FileExists(fPath));

end;

constructor TFileExists.Create(const aPath: TFilename;
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fCallback  := aCallback;
end;

end.

