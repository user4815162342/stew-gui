unit sys_localfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_file, sys_async, FileUtil;

type

  { TFileListPromise }

  TLocalFileListPromise = class(TFileListPromise)
  protected
    procedure DoTask; override;
  end;

  { TLocalFileCheckExistencePromise }

  TLocalFileCheckExistencePromise = class(TCheckExistencePromise)
  protected
    procedure DoTask; override;
  end;

  { TLocalFileReadPromise }

  TLocalFileReadPromise = class(TFileReadPromise)
  protected
    procedure DoTask; override;
  public
    destructor Destroy; override;
  end;

  { TLocalFileWritePromise }

  TLocalFileWritePromise = class(TFileWritePromise)
  protected
    procedure DoTask; override;
  end;

  { TLocalFileCopyPromise }

  TLocalFileCopyPromise = class(TFileCopyPromise)
  protected
    procedure DoTask; override;
  end;

  { TLocalFileRenamePromise }

  TLocalFileRenamePromise = class(TFileRenamePromise)
  protected
    procedure DoTask; override;
  end;

  { TLocalFileSystem }

  TLocalFileSystem = class(TFileSystem)
  protected
    class function CheckFileExistence(aFile: TFile): TBooleanPromise; override;
    class function CopyFile(aSource: TFile; aTarget: TFile;
      aOptions: TFileCopyOptions): TFileCopyPromise; override;
    class function CreateFileFromTemplate(aFile: TFile; aTemplate: TTemplate): TFileCopyPromise;
      override;
    class function GetContainedFile(aDir: TFile; aFileName: UTF8String
      ): TFile; override;
    class function GetDirectory(aFile: TFile): TFile; override;
    class function GetFileSystemClass: TFileSystemClass; override;
    class function GetName(aFile: TFile): UTF8String; override;
    class function GetTemplatesFor(aFile: TFile): TFileListTemplatesPromise; override;
    class function ListFiles(aFile: TFile): TFile.TFileListPromise; override;
    class procedure OpenInEditor(aFile: TFile); override;
    class function ReadFile(aFile: TFile): TFileReadPromise; override;
    class function WriteFile(aFile: TFile; aOptions: TFileWriteOptions; aFileAge: Longint; const aData: UTF8String): TFileWritePromise; override;
  public
    class function RenameFiles(aSource: TFileArray;
      aTarget: TFileArray): TFileRenamePromise; override;
  end;

  function LocalFile(aPath: TFilename): TFile;

const
  // formats in something like ISO8601, but I also need to quote the
  // hyphens, because otherwise they will be replaced with locale
  // date separators.
  TimestampFormat: String = 'yyyy"-"mm"-"dd"T"hh"-"mm"-"ss';


implementation

uses
  sys_os;

function LocalFile(aPath: TFilename): TFile;
begin
  result := TLocalFileSystem.GetFile(aPath);
end;

{ TLocalFileRenamePromise }

procedure TLocalFileRenamePromise.DoTask;
var
  i: Integer;
begin
  for i := 0 to Length(Source) - 1 do
  begin
    if Source[i] <> Target[i] then
      if not RenameFile(Source[i].ID,Target[i].ID) then
         raise Exception.Create('An error occurred while renaming ' + Source[i].ID + ' to ' + Target[i].ID);
  end;
  Resolve;
end;

{ TLocalFileCopyPromise }

procedure TLocalFileCopyPromise.DoTask;
var
  lFlags: TCopyFileFlags;
begin
  lFlags := [];
  // have to raise our own exceptions here, since CopyFile doesn't do that
  // if it's not in the options.
  if not (fcoOverwrite in Options) and
     FileExists(Target.ID) then
     raise Exception.Create('File ' + Target.ID + ' already exists.');

  if not (fcoCreateDir in Options) and
     not DirectoryExists(Target.Directory.ID) then
     raise Exception.Create('Directory ' + Target.Directory.ID + ' does not exist');

  if fcoCreateDir in Options then
    lFlags := lFlags + [cffCreateDestDirectory];
  if fcoOverwrite in Options then
    lFlags := lFlags + [cffOverwriteFile];
  if not CopyFile(Source.ID,Target.ID,lFlags) then
     raise Exception.Create('Copy file failed. Source: ' + Source.ID + '; Target: ' + Target.ID);
  Resolve;

end;

{ TLocalFileWritePromise }

// TODO: At some point, I need to 'test' the save conflicts.
procedure TLocalFileWritePromise.DoTask;
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
  if fwoCheckAge in Options then
  begin
    aCurrentAge := FileAge(Path.ID);
    // if the file does not exist yet, it should return -1. If the file
    // did not exist before, the caller should have passed -1 to indicate a
    // new file. These are the possible conditions I see:
    // fFileAge  fCurrentAge   means
    // -1        -1            file did not exist before, file does not exist now, no conflict, write as normal.
    // -1        > -1          file did not exist before, file does exist now, conflict, so report and don't write.
    // > -1      = fFileAge    file existed before, file has same mtime as before, no conflict, write as normal.
    // > -1      <> fFileAgge  file existed before, file has different mtime now, conflict, so report and don't write.
    if aCurrentAge <> Age then
    begin
      fIsConflict := true;
      fAge := aCurrentAge;
      Reject('File has been changed since last read. Can''t write.');
      exit;
    end;
  end;

  if not DirectoryExists(Path.Directory.ID) and (fwoCreateDir in Options) then
  begin
     ForceDirectories(Path.Directory.ID);
  end;
  // otherwise, an error might occur, but that should be handled and reported by the base class here.

  // TODO: Once I'm sure that saving is being done right, get rid
  // of this.
  if FileExists(Path.ID) then
    CopyFile(Path.ID,Path.ID + '-' + FormatDateTime(TimestampFormat, Now));
  stream := TFileStream.Create(Path.ID,fmCreate or fmShareDenyWrite);
  try
    fAge := FileAge(Path.ID);
    stream.Write(Data[1],Length(Data));
  finally
    stream.Free;
  end;
  Resolve;

end;

{ TLocalFileReadPromise }

procedure TLocalFileReadPromise.DoTask;
begin
  if FileExists(Path.ID) then
  begin
    fData := TFileStream.Create(Path.ID,fmOpenRead or fmShareDenyWrite);
    // get the age of the file now, while it's locked, to prevent certain
    // race conditions
    fAge := FileAge(Path.ID);
  end
  else
  begin
    fData := nil;
    fAge := NewFileAge;
  end;
  Resolve;
end;

destructor TLocalFileReadPromise.Destroy;
begin
  if fData <> nil then
    FreeAndNil(fData);
  inherited Destroy;
end;

{ TLocalFileCheckExistencePromise }

procedure TLocalFileCheckExistencePromise.DoTask;
begin
  fAnswer := FileExists(Path.ID);
  Resolve;
end;

{ TFileListPromise }
procedure TLocalFileListPromise.DoTask;
var
  SR: TSearchRec;
  Answer: TFileArray;
  L: Integer;
begin
  L := 0;
  if DirectoryExists(Path.ID) then
  begin
    if FindFirst(IncludeTrailingPathDelimiter(Path.ID) + '*',faDirectory,SR) = 0 then
    begin
      try
        repeat
          if (SR.Name <> '.') and (SR.Name <> '..') then
          begin
             L := L + 1;
             SetLength(Answer,L);
             Answer[L-1] := Path.GetContainedFile(SR.Name);
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;
  fFiles := Answer;
  Resolve;
end;

{ TLocalFileSystem }

class function TLocalFileSystem.CheckFileExistence(aFile: TFile
  ): TBooleanPromise;
begin
  result := TLocalFileCheckExistencePromise.Enqueue(aFile);
end;

class function TLocalFileSystem.CopyFile(aSource: TFile; aTarget: TFile;
  aOptions: TFileCopyOptions): TFileCopyPromise;
begin
  result := TLocalFileCopyPromise.Enqueue(aSource,aTarget,aOptions);
end;

class function TLocalFileSystem.CreateFileFromTemplate(aFile: TFile;
  aTemplate: TTemplate): TFileCopyPromise;
begin
  result := sys_os.CreateFileFromTemplate(aTemplate,aFile);
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

class function TLocalFileSystem.GetTemplatesFor(aFile: TFile
  ): TFileListTemplatesPromise;
begin
  result := TLocalFileListTemplatesFromOSPromise.Enqueue(aFile);
end;

class function TLocalFileSystem.ListFiles(aFile: TFile): TFile.TFileListPromise;
begin
  result := TLocalFileListPromise.Enqueue(aFile);
end;

class procedure TLocalFileSystem.OpenInEditor(aFile: TFile);
begin
  EditFile(aFile);
end;

class function TLocalFileSystem.ReadFile(aFile: TFile): TFileReadPromise;
begin
  result := TLocalFileReadPromise.Enqueue(aFile);
end;

class function TLocalFileSystem.RenameFiles(aSource: TFileArray;
  aTarget: TFileArray): TFileRenamePromise;
begin
  result := TLocalFileRenamePromise.Enqueue(aSource,aTarget);
end;

class function TLocalFileSystem.WriteFile(aFile: TFile;
  aOptions: TFileWriteOptions; aFileAge: Longint; const aData: UTF8String
  ): TFileWritePromise;
begin
  result := TLocalFileWritePromise.Enqueue(aFile,aOptions,aFileAge,aData);
end;

end.

