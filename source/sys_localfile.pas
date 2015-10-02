unit sys_localfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_file, sys_async, FileUtil, sys_types;

type

  { TFileListPromise }

  { TLocalFileListTask }

  TLocalFileListTask = class(TQueuedTask)
  strict private
    fPath: TFile;
  strict protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aFile: TFile);
  end;

  { TLocalFileExistsTask }

  TLocalFileExistsTask = class(TQueuedTask)
  strict private
    fPath: TFile;
  strict protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aPath: Tfile);
  end;

  { TLocalFileReadTask }

  TLocalFileReadTask = class(TQueuedTask)
  strict private
    fPath: TFile;
  strict protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aFile: TFile);
  end;

  { TLocalFileWriteTask }

  TLocalFileWriteTask = class(TQueuedTask)
  strict private
    fPath: TFile;
    fOptions: TFileWriteOptions;
    fAge: Longint;
  strict protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aFile: TFile; aOptions: TFileWriteOptions; aFileAge: Longint);
  end;

  { TLocalFileCopyTask }

  TLocalFileCopyTask = class(TQueuedTask)
  strict private
    fOptions: TFileCopyOptions;
    fTarget: TFile;
    fSource: TFile;
  strict protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aSource: TFile; aTarget: TFile; aOptions: TFileCopyOptions);
  end;

  { TLocalFileRenameTask }

  TLocalFileRenameTask = class(TQueuedTask)
  strict private
    fSource: TFileArray;
    fTarget: TFileArray;
  strict protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aSource: TFileArray; aTarget: TFileArray);
  end;

  { TLocalFileSystem }

  TLocalFileSystem = class(TFileSystem)
  protected
    class function CheckFileExistence(aFile: TFile): TFileExistencePromise; override;
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
    class function WriteFile(aFile: TFile; aOptions: TFileWriteOptions; aFileAge: Longint): TFileWritePromise; override;
    class function SplitPath(aFile: TFile; aBasePath: TFile): TStringArray; override;
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

{ TLocalFileRenameTask }

procedure TLocalFileRenameTask.DoTask;
var
  i: Integer;
begin
  for i := 0 to Length(fSource) - 1 do
  begin
    if fSource[i] <> fTarget[i] then
    begin
      if (not ForceDirectories(fTarget[i].Directory.ID)) or
         (not RenameFile(fSource[i].ID,fTarget[i].ID)) then
         raise Exception.Create('An error occurred while renaming ' + fSource[i].ID + ' to ' + fTarget[i].ID);
    end;
  end;
  Resolve;
end;

function TLocalFileRenameTask.CreatePromise: TPromise;
begin
  result := TFileRenamePromise.Create(fSource,fTarget);
end;

constructor TLocalFileRenameTask.Enqueue(aSource: TFileArray;
  aTarget: TFileArray);
begin
  fSource := aSource;
  fTarget := aTarget;
  inherited Enqueue;
end;

{ TLocalFileCopyTask }

procedure TLocalFileCopyTask.DoTask;
var
  lFlags: TCopyFileFlags;
begin
  lFlags := [];
  // have to raise our own exceptions here, since CopyFile doesn't do that
  // if it's not in the options.
  if not (fcoOverwrite in fOptions) and
     FileExists(fTarget.ID) then
     raise Exception.Create('File ' + fTarget.ID + ' already exists.');

  if not (fcoCreateDir in fOptions) and
     not DirectoryExists(fTarget.Directory.ID) then
     raise Exception.Create('Directory ' + fTarget.Directory.ID + ' does not exist');

  if fcoCreateDir in fOptions then
    lFlags := lFlags + [cffCreateDestDirectory];
  if fcoOverwrite in fOptions then
    lFlags := lFlags + [cffOverwriteFile];
  if not CopyFile(fSource.ID,fTarget.ID,lFlags) then
     raise Exception.Create('Copy file failed. Source: ' + fSource.ID + '; Target: ' + fTarget.ID);
  Resolve;

end;

function TLocalFileCopyTask.CreatePromise: TPromise;
begin
  result := TFileCopyPromise.Create(fSource,fTarget);
end;

constructor TLocalFileCopyTask.Enqueue(aSource: TFile; aTarget: TFile;
  aOptions: TFileCopyOptions);
begin
  fSource := aSource;
  fTarget := aTarget;
  fOptions := aOptions;
  inherited Enqueue;
end;

{ TLocalFileWriteTask }

// FUTURE: At some point, I need to 'test' the save conflicts.
procedure TLocalFileWriteTask.DoTask;
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
  if fwoCheckAge in fOptions then
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
    if aCurrentAge <> fAge then
    begin
      (Promise as TFileWritePromise).SetAnswer(aCurrentAge,true);
      Reject('File has been changed since last read. Can''t write.');
      exit;
    end;
  end;

  if not DirectoryExists(fPath.Directory.ID) and (fwoCreateDir in fOptions) then
  begin
     ForceDirectories(fPath.Directory.ID);
  end;
  // otherwise, an error might occur, but that should be handled and reported by the base class here.

  // TODO: Once I'm sure that saving is being done right, get rid
  // of this.
  if FileExists(fPath.ID) then
    CopyFile(fPath.ID,fPath.ID + '-' + FormatDateTime(TimestampFormat, Now));
  stream := TFileStream.Create(fPath.ID,fmCreate or fmShareDenyWrite);
  try
    (Promise as TFileWritePromise).SetAnswer(FileAge(fPath.ID),false);
    stream.CopyFrom((Promise as TFileWritePromise).Data,0);
  finally
    stream.Free;
  end;
  Resolve;

end;

function TLocalFileWriteTask.CreatePromise: TPromise;
begin
  result := TFileWritePromise.Create(fPath,TMemoryStream.Create);
end;

constructor TLocalFileWriteTask.Enqueue(aFile: TFile;
  aOptions: TFileWriteOptions; aFileAge: Longint);
begin
  fPath := aFile;
  fOptions := aOptions;
  fAge := aFileAge;
  inherited Enqueue;
end;

{ TLocalFileReadTask }

procedure TLocalFileReadTask.DoTask;
var
  lStream: TFileStream;
  lIsDir: Boolean;
begin
  lIsDir := DirectoryExists(fPath.ID);
  if (not lIsDir) and FileExists(fPath.ID) then
  begin
    lStream := TFileStream.Create(fPath.ID,fmOpenRead or fmShareDenyWrite);
    // get the age of the file now, while it's locked, to prevent certain
    // race conditions
    // Also, stream is now *owned* by the promise... It should still close
    // relatively quickly though.

    (Promise as TFileReadPromise).SetAnswer(lStream,FileAge(fPath.ID),true,false);
  end
  else
  begin
    (Promise as TFileReadPromise).SetAnswer(nil,NewFileAge,false,lIsDir);
  end;
  Resolve;
end;

function TLocalFileReadTask.CreatePromise: TPromise;
begin
  result := TFileReadPromise.Create(fPath);
end;

constructor TLocalFileReadTask.Enqueue(aFile: TFile);
begin
  fPath := aFile;
  inherited Enqueue;
end;

{ TLocalFileExistsTask }

procedure TLocalFileExistsTask.DoTask;
var
  lExists: Boolean;
  lIsFolder: Boolean;
begin
  lIsFolder := DirectoryExists(fPath.ID);
  lExists := FileExists(fPath.ID) or lIsFolder;
  (Promise as TFileExistencePromise).SetAnswer(lExists,lIsFolder);
  Resolve;
end;

function TLocalFileExistsTask.CreatePromise: TPromise;
begin
  result := TFileExistencePromise.Create(fPath);
end;

constructor TLocalFileExistsTask.Enqueue(aPath: Tfile);
begin
  fPath := aPath;
  inherited Enqueue;
end;

{ TFileListPromise }
procedure TLocalFileListTask.DoTask;
var
  SR: TSearchRec;
  lAnswer: TFileInfoArray;
  lInfo: TFileInfo;
  L: Integer;
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
            lInfo.Item := fPath.GetContainedFile(SR.Name);
            lInfo.IsFolder := DirectoryExists(lInfo.Item.ID);
            L := L + 1;
            SetLength(lAnswer,L);
            lAnswer[L-1] := lInfo;
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
    (Promise as TFileListPromise).SetAnswer(lAnswer,true,true)
  end
  else
    (Promise as TFileListPromise).SetAnswer(lAnswer,FileExists(fPath.ID),false);
  Resolve;
end;

function TLocalFileListTask.CreatePromise: TPromise;
begin
  result := TFileListPromise.Create(fPath);
end;

constructor TLocalFileListTask.Enqueue(aFile: TFile);
begin
  fPath := aFile;
  inherited Enqueue;
end;

{ TLocalFileSystem }

class function TLocalFileSystem.CheckFileExistence(aFile: TFile
  ): TFileExistencePromise;
begin
  result := TLocalFileExistsTask.Enqueue(aFile).Promise as TFileExistencePromise;
end;

class function TLocalFileSystem.CopyFile(aSource: TFile; aTarget: TFile;
  aOptions: TFileCopyOptions): TFileCopyPromise;
begin
  result := TLocalFileCopyTask.Enqueue(aSource,aTarget,aOptions).Promise as TFileCopyPromise;
end;

class function TLocalFileSystem.CreateFileFromTemplate(aFile: TFile;
  aTemplate: TTemplate): TFileCopyPromise;
begin
  result := TOperatingSystemInterface.CreateFileFromTemplate(aTemplate,aFile);
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
  result := TLocalFileListTemplatesFromOSTask.Enqueue(aFile).Promise as TFileListTemplatesPromise;
end;

class function TLocalFileSystem.ListFiles(aFile: TFile): TFile.TFileListPromise;
begin
  result := TLocalFileListTask.Enqueue(aFile).Promise as TFileListPromise;
end;

class procedure TLocalFileSystem.OpenInEditor(aFile: TFile);
begin
  TOperatingSystemInterface.EditFile(aFile);
end;

class function TLocalFileSystem.ReadFile(aFile: TFile): TFileReadPromise;
begin
  result := TLocalFileReadTask.Enqueue(aFile).Promise as TFileReadPromise;
end;

class function TLocalFileSystem.RenameFiles(aSource: TFileArray;
  aTarget: TFileArray): TFileRenamePromise;
begin
  result := TLocalFileRenameTask.Enqueue(aSource,aTarget).Promise as TFileRenamePromise;
end;

class function TLocalFileSystem.WriteFile(aFile: TFile;
  aOptions: TFileWriteOptions; aFileAge: Longint): TFileWritePromise;
begin
  result := TLocalFileWriteTask.Enqueue(aFile,aOptions,aFileAge).Promise as TFileWritePromise;
end;

class function TLocalFileSystem.SplitPath(aFile: TFile; aBasePath: TFile
  ): TStringArray;
var
  lList: TStringArray;
  i: Integer;
  l: Integer;
  lFile: TFile;
begin
  l := 0;
  lFile := aFile;
  while (lFile <> aBasePath) and (lFile.ID <> '') do
  begin
    SetLength(lList,l + 1);
    lList[l] := lFile.Name;
    inc(l);
    lFile := lFile.Directory;
  end;
  // now reverse it...
  SetLength(result,l);
  for i := 0 to l - 1 do
  begin
    result[i] := lList[(l - 1) - i];
  end;
end;

end.

