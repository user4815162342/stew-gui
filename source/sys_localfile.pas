unit sys_localfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_file, sys_async, FileUtil;

type
  { TListFiles }

  TListFiles = class(TDeferredTask)
  private
    fPath: TFile;
    fFilesCallback: TFile.TDeferredFileListCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFile;
      aCallback: TFile.TDeferredFileListCallback;
      aErrorback: TDeferredExceptionCallback); overload;
  end;

  { TFileExists }

  TFileExists = class(TDeferredTask)
  private
    fPath: TFile;
    fCallback: TDeferredBooleanCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFile; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
  end;

  { TReadFile }

  TReadFile = class(TDeferredTask)
  private
    fPath: TFile;
    fCallback: TFile.TReadFileCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFile; aCallback: TFile.TReadFileCallback; aErrorback: TDeferredExceptionCallback);
  end;


  { TWriteFile }

  TWriteFile = class(TDeferredTask)
  private
    fPath: TFile;
    fFileAge: Longint;
    fCheckFileAge: Boolean;
    fCreateDir: Boolean;
    fData: UTF8String;
    fCallback: TFile.TWriteFileCallback;
    fConflictBack: TFile.TWriteFileCallback;
  protected
    procedure DoTask; override;
  public
    // NOTE: for FileAge, pass the mtime retrieved by the TReadFile.
    // If the file was new, pass NewFileAge(-1).
    constructor Create(const aPath: TFile; aCreateDir: Boolean; aCheckFileAge: Boolean; aFileAge: Longint;
      const aData: UTF8String; aCallback: TFile.TWriteFileCallback; aConflictBack: TFile.TWriteFileCallback;
  aErrorback: TDeferredExceptionCallback);
  end;

  { TCopyFile }

  TCopyFile = class(TDeferredTask)
  private
    fSource: TFile;
    fTarget: TFile;
    fOptions: TCopyFileFlags;
    fCallback: TDeferredCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(aSource: TFile; aTarget: TFile; aFlags: TCopyFileFlags; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
  end;

  { TRenameFiles }

  TRenameFiles = class(TDeferredTask)
  private
    fSources: TFile.TFileArray;
    fTargets: TFile.TFileArray;
    fCallback: TDeferredCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(aSources: TFile.TFileArray; aTargets: TFile.TFileArray; aCallback: TDeferredCallback; aErrorBack: TDeferredExceptionCallback);
  end;

  { TLocalFileSystem }

  TLocalFileSystem = class(TFileSystem)
  protected
    class procedure CheckFileExistence(aFile: TFile;
      aCallback: TDeferredBooleanCallback;
      aErrorback: TDeferredExceptionCallback); override;
    class procedure CopyFile(aSource: TFile; aTarget: TFile;
      aFlags: TCopyFileFlags; aCallback: TDeferredCallback;
      aErrorback: TDeferredExceptionCallback); override;
    class function GetContainedFile(aDir: TFile; aFileName: UTF8String
      ): TFile; override;
    class function GetDirectory(aFile: TFile): TFile; override;
    class function GetFileSystemClass: TFileSystemClass; override;
    class function GetName(aFile: TFile): UTF8String; override;
    class procedure ListFiles(aFile: TFile;
      aCallback: TFile.TDeferredFileListCallback;
      aErrorBack: TDeferredExceptionCallback); override;
    class procedure ReadFile(aFile: TFile; aCallback: TFile.TReadFileCallback;
      aErrorback: TDeferredExceptionCallback); override;
    class procedure DoRenameFiles(aSource: TFile.TFileArray;
      aTarget: TFile.TFileArray; aCallback: TDeferredCallback;
      aErrorback: TDeferredExceptionCallback); override;
    class procedure WriteFile(aFile: TFile; aCreateDir: Boolean;
      aCheckFileAge: Boolean; aFileAge: Longint; const aData: UTF8String;
      aCallback: TFile.TWriteFileCallback; aConflictBack: TFile.TWriteFileCallback;
      aErrorback: TDeferredExceptionCallback); override;
  end;



const
  // formats in something like ISO8601, but I also need to quote the
  // hyphens, because otherwise they will be replaced with locale
  // date separators.
  TimestampFormat: String = 'yyyy"-"mm"-"dd"T"hh"-"mm"-"ss';


implementation

{ TLocalFileSystem }

class procedure TLocalFileSystem.CheckFileExistence(aFile: TFile;
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  TFileExists.Create(aFile,aCallback,aErrorback).Enqueue;
end;

class procedure TLocalFileSystem.CopyFile(aSource: TFile; aTarget: TFile;
  aFlags: TCopyFileFlags; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TCopyFile.Create(aSource,aTarget,aFlags,aCallback,aErrorback).Enqueue;
end;

class function TLocalFileSystem.GetContainedFile(aDir: TFile;
  aFileName: UTF8String): TFile;
begin
  if ExtractFileName(aFileName) <> aFileName then
     raise Exception.Create('Filename contains a path or drive delimiter.');
  result := GetFile(IncludeTrailingPathDelimiter(aDir.ID) + aFileName);
end;

class function TLocalFileSystem.GetDirectory(aFile: TFile): TFile;
begin
  result := GetFile(ExcludeTrailingPathDelimiter(ExtractFileDir(aFile.ID)));
end;

class function TLocalFileSystem.GetFileSystemClass: TFileSystemClass;
begin
  result := TLocalFileSystem;
end;

class function TLocalFileSystem.GetName(aFile: TFile): UTF8String;
begin
  result := ExtractFileName(aFile.ID);
end;

class procedure TLocalFileSystem.ListFiles(aFile: TFile;
  aCallback: TFile.TDeferredFileListCallback; aErrorBack: TDeferredExceptionCallback);
begin
  TListFiles.Create(aFile,aCallback,aErrorBack).Enqueue;
end;

class procedure TLocalFileSystem.ReadFile(aFile: TFile;
  aCallback: TFile.TReadFileCallback; aErrorback: TDeferredExceptionCallback);
begin
  TReadFile.Create(aFile,aCallback,aErrorback).Enqueue;
end;

class procedure TLocalFileSystem.DoRenameFiles(aSource: TFile.TFileArray;
  aTarget: TFile.TFileArray; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TRenameFiles.Create(aSource,aTarget,aCallback,aErrorback).Enqueue;
end;

class procedure TLocalFileSystem.WriteFile(aFile: TFile; aCreateDir: Boolean;
  aCheckFileAge: Boolean; aFileAge: Longint; const aData: UTF8String;
  aCallback: TFile.TWriteFileCallback; aConflictBack: TFile.TWriteFileCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TWriteFile.Create(aFile,aCreateDir,aCheckFileAge,aFileAge,aData,aCallback,aConflictBack,aErrorback).Enqueue;
end;

{ TRenameFiles }

procedure TRenameFiles.DoTask;
var
  i: Integer;
begin
  for i := 0 to Length(fSources) - 1 do
  begin
    if fSources[i] <> fTargets[i] then
      if not RenameFile(fSources[i].ID,fTargets[i].ID) then
         raise Exception.Create('An error occurred while renaming ' + fSources[i].ID + ' to ' + fTargets[i].ID);
  end;
  fCallback;
end;

constructor TRenameFiles.Create(aSources: TFile.TFileArray;
  aTargets: TFile.TFileArray; aCallback: TDeferredCallback;
  aErrorBack: TDeferredExceptionCallback);
var
  i: Integer;
  l: Integer;
begin
  inherited Create(aErrorBack);
  l := Length(aSources);
  if l <> Length(aTargets) then
     raise Exception.Create('Number of targets does not match number of sources.');
  fSources := aSources;
  fTargets := aTargets;
  fCallback:= aCallback;
end;

{ TCopyFile }

procedure TCopyFile.DoTask;
begin
  // have to raise our own exceptions here, since CopyFile doesn't do that
  // if it's not in the options.
  if not (cffOverwriteFile in fOptions) and
     FileExists(fTarget.ID) then
     raise Exception.Create('File ' + fTarget.ID + ' already exists.');

  if not (cffCreateDestDirectory in fOptions) and
     not DirectoryExists(fTarget.Directory.ID) then
     raise Exception.Create('Directory ' + fTarget.Directory.ID + ' does not exist');

  if not CopyFile(fSource.ID,fTarget.ID,fOptions) then
     raise Exception.Create('Copy file failed. Source: ' + fSource.ID + '; Target: ' + fTarget.ID);
  fCallback;
end;

constructor TCopyFile.Create(aSource: TFile; aTarget: TFile;
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
  Answer: TFile.TFileArray;
  L: Integer;
  i: Integer;
begin
  L := 0;
  if DirectoryExists(fPath.ID) then
  begin
       if FindFirst(IncludeTrailingPathDelimiter(fPath.ID) + '*',faDirectory,SR) = 0 then
       begin
          try
            repeat
              if (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                 L := L + 1;
                 SetLength(Answer,L);
                 Answer[L-1] := fPath.GetContainedFile(SR.Name);
              end;
            until FindNext(SR) <> 0;
          finally
            FindClose(SR);
          end;
       end;
  end;
  fFilesCallback(Answer);
end;

constructor TListFiles.Create(const aPath: TFile;
  aCallback: TFile.TDeferredFileListCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fFilesCallback:=aCallback;
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
    aCurrentAge := FileAge(fPath.ID);
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

  if not DirectoryExists(fPath.Directory.ID) and fCreateDir then
  begin
     ForceDirectories(fPath.Directory.ID);
  end;
  // otherwise, an error might occur, but that should be handled and reported by the base class here.

  // TODO: Once I'm sure that saving is being done right, get rid
  // of this.
  if FileExists(fPath.ID) then
    CopyFile(fPath.ID,fPath.ID + FormatDateTime(TimestampFormat, Now));
  stream := TFileStream.Create(fPath.ID,fmCreate or fmShareDenyWrite);
  try
    aCurrentAge := FileAge(fPath.ID);
    stream.Write(fData[1],Length(fData));
  finally
    stream.Free;
  end;
  fCallback(aCurrentAge);

end;

constructor TWriteFile.Create(const aPath: TFile; aCreateDir: Boolean;
  aCheckFileAge: Boolean; aFileAge: Longint; const aData: UTF8String;
  aCallback: TFile.TWriteFileCallback; aConflictBack: TFile.TWriteFileCallback;
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
  if FileExists(fPath.ID) then
  begin
     stream := TFileStream.Create(fPath.ID,fmOpenRead or fmShareDenyWrite);
     try
       // get the age of the file now, while it's locked, to prevent certain
       // race conditions
       age := FileAge(fPath.ID);
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

constructor TReadFile.Create(const aPath: TFile;
  aCallback: TFile.TReadFileCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fCallback := aCallback;
  fPath := aPath;
end;

{ TFileExists }

procedure TFileExists.DoTask;
begin
  fCallback(FileExists(fPath.ID));

end;

constructor TFileExists.Create(const aPath: TFile;
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fCallback  := aCallback;
end;



end.

