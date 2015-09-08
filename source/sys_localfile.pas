unit sys_localfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_file, sys_async, FileUtil;

type

  { TFileListPromise }

  { TLocalFileListTask }

  TLocalFileListTask = class(TQueuedTask)
  private
    fPath: TFile;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aFile: TFile);
  end;

  { TLocalFileExistsTask }

  TLocalFileExistsTask = class(TQueuedTask)
  private
    fPath: TFile;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aPath: Tfile);
  end;

  { TLocalFileReadTask }

  TLocalFileReadTask = class(TQueuedTask)
  private
    fPath: TFile;
  // TODO: Is this getting deleted appropriately?
  // I can probably just shift over to a TMemoryStream...
  // Or, maybe, the only thing that gets the data is
  // the primary caller, and for the rest the promise
  // simply acts as a completion event.
  // The Answer:
  // - Behave like a usual network request/response paradigm:
  // - The writer *request* involves writing to a stream already,
  // even if it's buffered. WriteFile doesn't return a promise,
  // it returns a request that the caller must send to get the promise.
  // -  The Reader Response contains a stream which can be perused
  // all you want. This would be the case with the resulting
  // promise as well. The stream would get deleted as soon
  // as all listeners are done responding to it. Alternately,
  // only the caller would be able to read the data, and the
  // listeners would only get to know if the reading is complete,
  // just as you wouldn't have more than one request handler for
  // a request (although the handler might be able to pipe it
  // to others).
    fReader: TFileReader;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aFile: TFile; aHandler: TFileReader);
  end;

  { TLocalFileWriteTask }

  TLocalFileWriteTask = class(TQueuedTask)
  private
    fPath: TFile;
    fOptions: TFileWriteOptions;
    fAge: Longint;
    fWriter: TFileWriter;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aFile: TFile; aOptions: TFileWriteOptions; aFileAge: Longint; aWriter: TFileWriter);
  end;

  { TLocalFileCopyTask }

  TLocalFileCopyTask = class(TQueuedTask)
  private
    fOptions: TFileCopyOptions;
    fTarget: TFile;
    fSource: TFile;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aSource: TFile; aTarget: TFile; aOptions: TFileCopyOptions);
  end;

  { TLocalFileRenameTask }

  TLocalFileRenameTask = class(TQueuedTask)
  private
    fSource: TFileArray;
    fTarget: TFileArray;
  protected
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
    class function ReadFile(aFile: TFile; aHandler: TFileReader): TFileReadPromise; override;
    class function WriteFile(aFile: TFile; aOptions: TFileWriteOptions; aFileAge: Longint; aWriter: TFileWriter): TFileWritePromise; override;
  public
    class function RenameFiles(aSource: TFileArray;
      aTarget: TFileArray): TFileRenamePromise; override;
  end;

  function LocalFile(aPath: TFilename): TFile;

type
  TCachedLocalFileSystem = class(TLocalFileSystem)
  end;

  function CachedLocalFile(aPath: TFilename): TFile;

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

function CachedLocalFile(aPath: TFilename): TFile;
begin
  result := TCachedLocalFileSystem.GetFile(aPath);
end;

{ TLocalFileRenameTask }

procedure TLocalFileRenameTask.DoTask;
var
  i: Integer;
begin
  for i := 0 to Length(fSource) - 1 do
  begin
    if fSource[i] <> fTarget[i] then
      if not RenameFile(fSource[i].ID,fTarget[i].ID) then
         raise Exception.Create('An error occurred while renaming ' + fSource[i].ID + ' to ' + fTarget[i].ID);
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

// TODO: At some point, I need to 'test' the save conflicts.
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
    fWriter.Write(stream);
  finally
    stream.Free;
  end;
  Resolve;

end;

function TLocalFileWriteTask.CreatePromise: TPromise;
begin
  result := TFileWritePromise.Create(fPath,fWriter);
end;

constructor TLocalFileWriteTask.Enqueue(aFile: TFile;
  aOptions: TFileWriteOptions; aFileAge: Longint; aWriter: TFileWriter);
begin
  fPath := aFile;
  fOptions := aOptions;
  fAge := aFileAge;
  fWriter := aWriter;
  inherited Enqueue;
end;

{ TLocalFileReadTask }

procedure TLocalFileReadTask.DoTask;
var
  lStream: TFileStream;
begin
  if FileExists(fPath.ID) then
  begin
    lStream := TFileStream.Create(fPath.ID,fmOpenRead or fmShareDenyWrite);
    try
      // get the age of the file now, while it's locked, to prevent certain
      // race conditions
      (Promise as TFileReadPromise).SetAnswer(FileAge(fPath.ID),False);
      fReader.Read(lStream);
    finally
      lStream.Free;
    end;
  end
  else
  begin
    fReader.Read(nil);
    (Promise as TFileReadPromise).SetAnswer(NewFileAge,true);
  end;
  Resolve;
end;

function TLocalFileReadTask.CreatePromise: TPromise;
begin
  result := TFileReadPromise.Create(fPath,fReader);
end;

constructor TLocalFileReadTask.Enqueue(aFile: TFile; aHandler: TFileReader);
begin
  fPath := aFile;
  fReader := aHandler;
  inherited Enqueue;
end;

{ TLocalFileExistsTask }

procedure TLocalFileExistsTask.DoTask;
begin
  (Promise as TFileExistencePromise).SetAnswer(FileExists(fPath.ID) or DirectoryExists(fPath.ID));
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
  lAnswer: TFileArray;
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
             L := L + 1;
             SetLength(lAnswer,L);
             lAnswer[L-1] := fPath.GetContainedFile(SR.Name);
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
    (Promise as TFileListPromise).SetAnswer(lAnswer,false)
  end
  else
    (Promise as TFileListPromise).SetAnswer(lAnswer,not FileExists(fPath.ID));
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
  result := TLocalFileListTemplatesFromOSTask.Enqueue(aFile).Promise as TFileListTemplatesPromise;
end;

class function TLocalFileSystem.ListFiles(aFile: TFile): TFileListPromise;
begin
  result := TLocalFileListTask.Enqueue(aFile).Promise as TFileListPromise;
end;

class procedure TLocalFileSystem.OpenInEditor(aFile: TFile);
begin
  EditFile(aFile);
end;

class function TLocalFileSystem.ReadFile(aFile: TFile;
  aHandler: TFileReader): TFileReadPromise;
begin
  result := TLocalFileReadTask.Enqueue(aFile,aHandler).Promise as TFileReadPromise;
end;

class function TLocalFileSystem.RenameFiles(aSource: TFileArray;
  aTarget: TFileArray): TFileRenamePromise;
begin
  result := TLocalFileRenameTask.Enqueue(aSource,aTarget).Promise as TFileRenamePromise;
end;

class function TLocalFileSystem.WriteFile(aFile: TFile;
  aOptions: TFileWriteOptions; aFileAge: Longint; aWriter: TFileWriter): TFileWritePromise;
begin
  result := TLocalFileWriteTask.Enqueue(aFile,aOptions,aFileAge,aWriter).Promise as TFileWritePromise;
end;

end.

