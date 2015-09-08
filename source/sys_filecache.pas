unit sys_filecache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_file, contnrs, stew_types, sys_async;

type

  TLongStringMapIteratorCallback = procedure(aKey: UTF8String; aValue: TObject) of object;

  TKeyValue = record
    Key: UTF8String;
    Data: TObject;
  end;
  TKeyValueArray = array of TKeyValue;

  { TLongStringMap }

  { NOTE: This isn't the most efficiently done, it's more of a kludge. It should
  work fine, but I wouldn't use this for cases where you're going to create a whole
  bunch of them with just a few keys in them (hence, don't use for JSON stuff)}
  TLongStringMap = class
  private type
    // Used to handle keys which are present but contain no data.
    TPlaceholder = class(TObject)
    end;
  private
    fShortStringMap: TFPHashObjectList;
    function GetItem(const AName: UTF8String): TObject;
    procedure SetItem(const AName: UTF8String; AValue: TObject);
  public
    constructor Create(aOwnsObjects: Boolean);
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(const AName: UTF8String);
    function Has(const aName: UTF8String): Boolean;
    function GetKeys: TStringArray;
    function GetKeyValues: TKeyValueArray;
    function AppendKeys(aBase: UTF8String; var aArray: TStringArray): Integer;
    function AppendKeyValues(aBase: UTF8String; var aArray: TKeyValueArray): Integer;
    property Items[const AName: UTF8String]: TObject read GetItem write SetItem; default;
    function SplitKey(const aName: UTF8String; out aBase: ShortString; out aRest: UTF8String): Boolean;
    procedure ForEachCall(aPrefix: UTF8String; aCallback: TLongStringMapIteratorCallback);
    // Since we can't iterate by index (without some additional complexity),
    // the following is a simple way to iterate through each item, and a little
    // bit better performance than iterating through GetKeys.
    procedure ForEachCall(aCallback: TLongStringMapIteratorCallback);
  end;


  { TFileSystemCacheData }

  TFileSystemCacheData = class(TObject)
  private
    fTime: TDateTime;
  public
    constructor Create;
    property TimeCreated: TDateTime read fTime;
  end;

  { TFileSystemCacheExists }

  TFileSystemCacheExists = class(TFileSystemCacheData)
  private
    fExists: Boolean;
  public
    constructor Create(aExists: Boolean);
    property Exists: Boolean read fExists;
  end;

  { TFileSystemCacheLists }

  TFileSystemCacheLists = class(TFileSystemCacheData)
  private
    fFiles: TFileArray;
  public
    constructor Create(aFiles: TFileArray);
    property Files: TFileArray read fFiles;
  end;

  { TFileSystemCacheContents }

  TFileSystemCacheContents = class(TFileSystemCacheData)
  private
    fContents: TMemoryStream;
    fAge: Longint;
  public
    constructor Create(aData: TStream; aAge: Longint);
    destructor Destroy; override;
    property Data: TMemoryStream read fContents;
    property Age: Longint read fAge;
  end;

  { TFileSystemCacheExistenceKnownTask }

  TFileSystemCacheExistenceKnownTask = class(TQueuedTask)
  private
     fFile: TFile;
  protected
     procedure DoTask; override;
     function CreatePromise: TPromise; override;
  public
     constructor Enqueue(aFile: TFile; aData: TFileSystemCacheExists);
  end;


  { TFileSystemCacheExistenceDeferredTask }

  TFileSystemCacheExistenceDeferredTask = class(TDeferredTask2)
  private
    fPath: TFile;
  protected
    procedure  DoTask(Input: TPromise); override;
    function CreatePromise: TPromise; override;
  public
    constructor Defer(aFile: TFile; aPromise: TFileReadPromise);
    constructor Defer(aFile: TFile; aPromise: TFileListPromise);
  end;

  { TFileSystemCacheListKnownTask }

  TFileSystemCacheListKnownTask = class(TQueuedTask)
  private
    fFile: TFile;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
     constructor Enqueue(aFile: TFile; aData: TFileSystemCacheLists; aExistence: TFileSystemCacheExists);
  end;

  { TFileSystemCacheReadDeferredTask }

  TFileSystemCacheReadDeferredTask = class(TDeferredTask2)
  private
    fFile: TFile;
    fReader: TFileReader;
  protected
    procedure DoTask(Input: TPromise); override;
    function CreatePromise: TPromise; override;
  public
    constructor Defer(aFile: TFile; aHandler: TFileReader; aPromise: TFileReadPromise);
  end;

  { TFileSystemCacheReadKnownTask }

  TFileSystemCacheReadKnownTask = class(TQueuedTask)
  private
    fFile: TFile;
    fReader: TFileReader;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aFile: TFile; aHandler: TFileReader;
      aData: TFileSystemCacheContents; aExistence: TFileSystemCacheExists);
  end;

  { TFileSystemCache }

  // I played around with creating a separate 'cached' file system, but that
  // was actually more complex than I needed it to be. I basically had to
  // abstract a lot of stuff away that simply wasn't going to be cached,
  // and I wouldn't have access to a bunch of uncaching methods which would
  // be necessary to use. One of the biggest complications was having to
  // convert the TFile back and forth between systems.

  TFileSystemCache = class(TObject)
  private
    fSystem: TFileSystemClass;
    fPromiseCache: TLongStringMap;
    fDataCache: TLongStringMap;
    procedure FileRead(Sender: TPromise);
    procedure FileExistenceChecked(Sender: TPromise);
    procedure FileListed(Sender: TPromise);
    procedure FilesRenamed(Sender: TPromise);
    procedure FileWritten(Sender: TPromise);
  public
    constructor Create(aSystem: TFileSystemClass);
    destructor Destroy; override;
    function CheckExistence(aFile: TFile): TFileExistencePromise;
    function ListFiles(aFile: TFile): TFileListPromise;
    function ReadFile(aFile: TFile; aHandler: TFileReader): TFileReadPromise;
    // Although I'm not actually caching the write, I want to make sure
    // that we "uncache" files after they are saved.
    function WriteFile(aFile: TFile; aOptions: TFileWriteOptions;
      aFileAge: Longint): TFileWritePromise;
    function WriteFile(aFile: TFile): TFileWritePromise;
    function WriteFile(aFile: TFile; aOptions: TFileWriteOptions): TFileWritePromise;
    function WriteFile(aFile: TFile; aOptions: TFileWriteOptions;
      aFileAge: Longint; aText: UTF8String): TFileWritePromise;
    function WriteFile(aFile: TFile; aText: UTF8String): TFileWritePromise;
    function WriteFile(aFile: TFile; aOptions: TFileWriteOptions; aText: UTF8String): TFileWritePromise;
    function RenameFiles(aSource: TFileArray; aTarget: TFileArray): TFileRenamePromise;
    // Uncaches all data, promises and everything.
    // promises always check if they are the correct promise for the file
    // before they do anything, so if the promises are left lying around
    // after being uncached, they won't effect anything.
    procedure Uncache; overload;
    // Uncaches specified file and all contained files.
    procedure Uncache(aFile: TFile; aRecursive: Boolean); overload;
    // Uncaches a list of files and all contained files...
    procedure Uncache(aFiles: TFileArray; aRecursive: Boolean); overload;
    // Uncaches files older than a certain TDataTime.
    procedure Uncache(aAsOf: TDateTime); overload;
    class function ExistsKey(aFile: TFile): UTF8String;
    class function ContentsKey(aFile: TFile): UTF8String;
    class function ListKey(aFile: TFile): UTF8String;
    class function FileForKey(aKey: UTF8String; aSystem: TFileSystemClass): TFile;
    class procedure DeleteKeysWhichRepresentContainedFiles(aCache: TLongStringMap;
      aFiles: TFileArray);
    const
      ListIdentifier = '?list';
      ContentsIdentifier = '?contents';
      ExistsIdentifier = '?exists';
  end;



implementation

{ TFileSystemCacheReadPromiseKnown }

procedure TFileSystemCacheReadKnownTask.DoTask;
begin
  // we have all the data, just resolve
  Resolve;
end;

function TFileSystemCacheReadKnownTask.CreatePromise: TPromise;
begin
  result := TFileReadPromise.Create(fFile,fReader);
end;

constructor TFileSystemCacheReadKnownTask.Enqueue(aFile: TFile;
  aHandler: TFileReader; aData: TFileSystemCacheContents; aExistence: TFileSystemCacheExists);
begin
  fFile := aFile;
  fReader := aHandler;
  inherited Enqueue;
  (Promise as TFileReadPromise).Reader.Read(aData.Data);
  (Promise as TFileReadPromise).SetAnswer(aData.Age,not aExistence.Exists);
end;

{ TFileSystemCacheReadDeferredTask }

procedure TFileSystemCacheReadDeferredTask.DoTask(Input: TPromise);
var
  lPromise: TFileReadPromise;
  lInput: TFileReadPromise;
  lReader: TFileStreamReader;
  lNewReader: TFileReader;
begin
  // we don't "own" the stream here, so we just want the stream here.
  lPromise := Promise as TFileReadPromise;
  lInput := Input as TFileReadPromise;
  lReader := lInput.Reader as TFileStreamReader;
  lNewReader := lPromise.Reader;
  lNewReader.Read(lReader.Stream);

  (Promise as TFileReadPromise).Reader.Read(((Input as TFileReadPromise).Reader as TFileStreamReader).Stream);
  (Promise as TFileReadPromise).SetAnswer((Input as TFileReadPromise).Age,(Input as TFileReadPromise).DoesNotExist);
  Resolve;
end;

function TFileSystemCacheReadDeferredTask.CreatePromise: TPromise;
begin
  result := TFileReadPromise.Create(fFile,fReader);
end;

constructor TFileSystemCacheReadDeferredTask.Defer(aFile: TFile;
  aHandler: TFileReader; aPromise: TFileReadPromise);
begin
  fFile := aFile;
  fReader := aHandler;
  inherited Defer(aPromise);
end;

{ TFileSystemCacheListKnownTask }

procedure TFileSystemCacheListKnownTask.DoTask;
begin
  // we have the data already, just resolve
  Resolve;
end;

function TFileSystemCacheListKnownTask.CreatePromise: TPromise;
begin
  result := TFileListPromise.Create(fFile);
end;

constructor TFileSystemCacheListKnownTask.Enqueue(aFile: TFile;
  aData: TFileSystemCacheLists; aExistence: TFileSystemCacheExists);
begin
  fFile := aFile;
  inherited Enqueue;
  (Promise as TFileListPromise).SetAnswer(aData.Files,not aExistence.Exists);

end;

{ TFileSystemCacheExistenceDeferredTask }

procedure TFileSystemCacheExistenceDeferredTask.DoTask(Input: TPromise);
begin
  if Input is TFileReadPromise then
     (Promise as TFileExistencePromise).SetAnswer(not (Input as  TFileReadPromise).DoesNotExist)
  else if Input is TFileListPromise then
     (Promise as TFileExistencePromise).SetAnswer(not (Input as TFileListPromise).DoesNotExist)
  else
  begin
     // shouldn't happen if the constructor is taking care of things right.
     Reject('Invalid sub-promise');
     Exit;
  end;
  Resolve;
end;

function TFileSystemCacheExistenceDeferredTask.CreatePromise: TPromise;
begin
  result := TFileExistencePromise.Create(fPath);
end;

constructor TFileSystemCacheExistenceDeferredTask.Defer(aFile: TFile;
  aPromise: TFileReadPromise);
begin
  fPath := aFile;
  inherited Defer(aPromise);
end;

constructor TFileSystemCacheExistenceDeferredTask.Defer(aFile: TFile;
  aPromise: TFileListPromise);
begin
  fPath := aFile;
  inherited Defer(aPromise);
end;

{ TFileSystemCacheExistenceKnownTask }

procedure TFileSystemCacheExistenceKnownTask.DoTask;
begin
  Resolve;
end;

function TFileSystemCacheExistenceKnownTask.CreatePromise: TPromise;
begin
  result := TFileExistencePromise.Create(fFile);
end;

constructor TFileSystemCacheExistenceKnownTask.Enqueue(aFile: TFile;
  aData: TFileSystemCacheExists);
begin
  fFile := aFile;
  inherited Enqueue;
  (Promise as TFileExistencePromise).SetAnswer(aData.Exists);
end;

{ TFileSystemCacheContents }

constructor TFileSystemCacheContents.Create(aData: TStream; aAge: Longint);
begin
  inherited Create;
  fContents := TMemoryStream.Create;
  if aData <> nil then
    fContents.CopyFrom(aData,0);
  fAge := aAge;
end;

destructor TFileSystemCacheContents.Destroy;
begin
  FreeAndNil(fContents);
  inherited Destroy;
end;

{ TFileSystemCacheLists }

constructor TFileSystemCacheLists.Create(aFiles: TFileArray);
begin
  inherited Create;
  fFiles := aFiles;
end;

{ TFileSystemCacheExists }

constructor TFileSystemCacheExists.Create(aExists: Boolean);
begin
  inherited Create;
  fExists := aExists;
end;

{ TFileSystemCacheData }

constructor TFileSystemCacheData.Create;
begin
  inherited Create;
  fTime := Now;
end;

{ TFileSystemCache }

procedure TFileSystemCache.FileExistenceChecked(Sender: TPromise);
var
  lFile: TFile;
  lAnswer: Boolean;
  lExistsKey: UTF8String;
begin
  lFile := (Sender as TFileExistencePromise).Path;
  lExistsKey := ExistsKey(lFile);
  if fPromiseCache[lExistsKey] = Sender then
  begin
    lAnswer := (Sender as TFileExistencePromise).Exists;
    fPromiseCache.Delete(lExistsKey);
    fDataCache[lExistsKey] := TFileSystemCacheExists.Create(lAnswer);
  end;
  // otherwise, there must be a newer promise with better information...
end;

procedure TFileSystemCache.FileListed(Sender: TPromise);
var
  lFile: TFile;
  lExists: Boolean;
  lList: TFileArray;
  lExistsKey: UTF8String;
  lListKey: UTF8String;
begin
  lFile := (Sender as TFileListPromise).Path;
  lExistsKey := ExistsKey(lFile);
  lListKey := ListKey(lFile);
  if fPromiseCache[lListKey] = Sender then
  begin
    lExists := not (Sender as TFileListPromise).DoesNotExist;
    lList := (Sender as TFileListPromise).Files;
    fPromiseCache.Delete(lListKey);
    fDataCache[lExistsKey] := TFileSystemCacheExists.Create(lExists);
    fDataCache[lListKey] := TFileSystemCacheLists.Create(lList);
  end;
  // otherwise, there must be a newer promise with better information...
end;

procedure TFileSystemCache.FilesRenamed(Sender: TPromise);
var
  l: Integer;
  i: Integer;
  lSource: TFileArray;
  lList: TFileArray;
begin
  // we need to uncache *both* sets of files, just in case, recursively.
  // It's going to be more efficient to do both of them at once, I feel.
  lSource := (Sender as TFileRenamePromise).Source;
  l := Length(lSource);
  SetLength(lList,l+l);
  for i := 0 to l - 1 do
  begin
    lList[i] := lSource[i];
  end;
  lSource := (Sender as TFileRenamePromise).Source;
  for i := 0 to l - 1 do
  begin
    lList[l + i] := lSource[i];
  end;
  Uncache(lList,true);



end;

procedure TFileSystemCache.FileWritten(Sender: TPromise);
begin
  Uncache((Sender as TFileWritePromise).Path,false);
end;

procedure TFileSystemCache.FileRead(Sender: TPromise);
var
  lFile: TFile;
  lExists: Boolean;
  lStream: TStream;
  lAge: Longint;
  lExistsKey: UTF8String;
  lContentsKey: UTF8String;
begin
  lFile := (Sender as TFileReadPromise).Path;
  lExistsKey := ExistsKey(lFile);
  lContentsKey := ContentsKey(lFile);
  if fPromiseCache[lContentsKey] = Sender then
  begin
    fPromiseCache.Delete(lContentsKey);
    lExists := not (Sender as TFileReadPromise).DoesNotExist;
    fDataCache[lExistsKey] := TFileSystemCacheExists.Create(lExists);
    lAge := (Sender as TFileReadPromise).Age;
    // we are actually going to take ownership of the stream for the contents.
    lStream := ((Sender as TFileReadPromise).Reader as TFileStreamReader).TakeOwnershipOfStream;
    fDataCache[lContentsKey] := TFileSystemCacheContents.Create(lStream,lAge);
  end;
  // otherwise, there must be a newer promise with better information...
end;

class function TFileSystemCache.ExistsKey(aFile: TFile): UTF8String;
begin
  result := aFile.ID + ExistsIdentifier;
end;

class function TFileSystemCache.ContentsKey(aFile: TFile): UTF8String;
begin
  result := aFile.ID + ContentsIdentifier;
end;

class function TFileSystemCache.ListKey(aFile: TFile): UTF8String;
begin
  result := aFile.ID + ListIdentifier;
end;

class function TFileSystemCache.FileForKey(aKey: UTF8String;
  aSystem: TFileSystemClass): TFile;
var
  i : Integer;
  id: UTF8String;
const Delim: Char = '?';
begin
  I := Length(aKey);
  while (I > 0) and not (aKey[I] = Delim) do
    Dec(I);
  id := Copy(aKey,1,i);
  case Copy(aKey,i,Length(aKey)) of
    ListIdentifier, ContentsIdentifier, ExistsIdentifier:
    begin
      result := aSystem.GetFile(id);
    end
    else
      // I don't have time to deal with invalid keys right now...
      raise Exception.Create('Key is not valid.');
  end;
end;

class procedure TFileSystemCache.DeleteKeysWhichRepresentContainedFiles(
  aCache: TLongStringMap; aFiles: TFileArray);
var
  lKeys: TStringArray;
  lKeysLen: Integer;
  i: Integer;
  j: Integer;
  lFilesLen: Integer;
  lCheckFile: TFile;
begin
  lKeys := aCache.GetKeys;
  lKeysLen := Length(lKeys);
  lFilesLen := Length(aFiles);
  if lFilesLen < 1 then
     exit;
  for i := 0 to lKeysLen - 1 do
  begin
    lCheckFile := FileForKey(lKeys[i],aFiles[0].System);
    for j := 0 to lFilesLen - 1 do
    begin
      if (aFiles[j] = lCheckFile) or aFiles[j].Contains(lCheckFile) then
      begin
         aCache.Delete(lKeys[i]);
         break;
      end;
    end;
  end;
end;

constructor TFileSystemCache.Create(aSystem: TFileSystemClass);
begin
  inherited Create;
  fSystem := aSystem;
  fDataCache := TLongStringMap.Create(True);
  fPromiseCache := TLongStringMap.Create(False);
end;

destructor TFileSystemCache.Destroy;
begin
  FreeAndNil(fPromiseCache);
  FreeAndNil(fDataCache);
  inherited Destroy;
end;

function TFileSystemCache.CheckExistence(aFile: TFile): TFileExistencePromise;
var
  lExistsKey: UTF8String;
  lListKey: UTF8String;
  lContentsKey: UTF8String;
  lCached: TObject;
begin
  lExistsKey := ExistsKey(aFile);
  lCached := fPromiseCache[lExistsKey];
  if lCached = nil then
  begin
    lContentsKey := ContentsKey(aFile);
    lCached := fPromiseCache[lContentsKey];
    if lCached = nil then
    begin
      lListKey := ListKey(aFile);
      lCached := fPromiseCache[lListKey];
      if lCached = nil then
      begin
        lCached := fDataCache[lExistsKey];
        if lCached = nil then
        begin
          result := aFile.CheckExistence;
          result.After(@FileExistenceChecked);
          fPromiseCache[lExistsKey] := Result;
        end
        else
          result := TFileSystemCacheExistenceKnownTask.Enqueue(aFile,lCached as TFileSystemCacheExists).Promise as TFileExistencePromise;
      end
      else
        result := TFileSystemCacheExistenceDeferredTask.Defer(aFile,lCached as TFileListPromise).Promise as TFileExistencePromise;
    end
    else
      result := TFileSystemCacheExistenceDeferredTask.Defer(aFile,lCached as TFileReadPromise).Promise as TFileExistencePromise;
  end
  else
     result := lCached as TFileExistencePromise;


end;

function TFileSystemCache.ListFiles(aFile: TFile): TFileListPromise;
var
  lListKey: UTF8String;
  lExistsKey: UTF8String;
  lCached: TObject;
begin
  lListKey := ListKey(aFile);
  lExistsKey := ExistsKey(aFile);
  lCached := fPromiseCache[lListKey];
  if lCached = nil then
  begin
    lCached := fDataCache[lListKey];
    if lCached = nil then
    begin
      result := aFile.List;
      result.After(@FileListed);
      fPromiseCache[lListKey] := Result;
    end
    else
      result := TFileSystemCacheListKnownTask.Enqueue(aFile,lCached as TFileSystemCacheLists, fDataCache[lExistsKey] as TFileSystemCacheExists).Promise as TFileListPromise;
  end
  else
     result := lCached as TFileListPromise;
end;

function TFileSystemCache.ReadFile(aFile: TFile; aHandler: TFileReader): TFileReadPromise;
var
  lContentsKey: UTF8String;
  lExistsKey: UTF8String;
  lCached: TObject;
begin
  lContentsKey := ContentsKey(aFile);
  lExistsKey := ExistsKey(aFile);
  lCached := fPromiseCache[lContentsKey];
  if lCached = nil then
  begin
    lCached := fDataCache[lContentsKey];
    if lCached = nil then
    begin
      result := aFile.Read(TFileStreamReader.Create);
      result.After(@FileRead);
      fPromiseCache[lContentsKey] := Result;
      // Now, wrap that in another promise that will do the reading
      // with the correct reader.
      result := TFileSystemCacheReadDeferredTask.Defer(aFile,aHandler,result).Promise as TFileReadPromise;
    end
    else
      result := TFileSystemCacheReadKnownTask.Enqueue(aFile,aHandler,lCached as TFileSystemCacheContents, fDataCache[lExistsKey] as TFileSystemCacheExists).Promise as TFileReadPromise;
  end
  else
  begin
    // the promise is directly from the file system and returns a stream.
    // So, we need to return a promise that wwill do the reading with
    // the correct reader.
     result := TFileSystemCacheReadDeferredTask.Defer(aFile,aHandler,lCached as TFileReadPromise).Promise as TFileReadPromise;
  end;
end;

function TFileSystemCache.WriteFile(aFile: TFile; aOptions: TFileWriteOptions;
  aFileAge: Longint): TFileWritePromise;
begin
  // We don't need to cache the writes, but I want to 'clear' the cache
  // once it's written.
  result := aFile.Write(aOptions,aFileAge);
  result.After(@FileWritten);
end;

function TFileSystemCache.WriteFile(aFile: TFile): TFileWritePromise;
begin
  result := aFile.Write;
  result.After(@FileWritten);
end;

function TFileSystemCache.WriteFile(aFile: TFile; aOptions: TFileWriteOptions): TFileWritePromise;
begin
  result := aFile.Write(aOptions);
  result.After(@FileWritten);
end;

function TFileSystemCache.WriteFile(aFile: TFile; aOptions: TFileWriteOptions;
  aFileAge: Longint; aText: UTF8String): TFileWritePromise;
begin
  result := WriteFile(aFile,aOptions,aFileAge);
  result.WriteString(aText);
end;

function TFileSystemCache.WriteFile(aFile: TFile; aText: UTF8String
  ): TFileWritePromise;
begin
  result := WriteFile(aFile);
  result.WriteString(aText);

end;

function TFileSystemCache.WriteFile(aFile: TFile; aOptions: TFileWriteOptions;
  aText: UTF8String): TFileWritePromise;
begin
  result := WriteFile(aFile,aOptions);
  result.WriteString(aText);

end;

function TFileSystemCache.RenameFiles(aSource: TFileArray; aTarget: TFileArray
  ): TFileRenamePromise;
begin
  // I don't need to cache these, but I need to clear the cache once
  // their all renamed (those files and all contained files)
  result := fSystem.RenameFiles(aSource,aTarget);
  result.After(@FilesRenamed);
end;

procedure TFileSystemCache.Uncache;
begin
  // just clear everything. In both caches.
  // if a promise is removed from the cache, it won't have any effects
  // when it returns, because the after code checks to see if it's the correct
  // response for that file before it does anything.
  fPromiseCache.Clear;
  fDataCache.Clear;

end;

procedure TFileSystemCache.Uncache(aFile: TFile; aRecursive: Boolean);
var
  lExistsKey: UTF8String;
  lContentsKey: UTF8String;
  lListKey: UTF8String;
  lFiles: TFileArray;
begin

  if aRecursive then
  begin
    SetLength(lFiles,1);
    lFiles[0] := aFile;
    DeleteKeysWhichRepresentContainedFiles(fPromiseCache,lFiles);
    DeleteKeysWhichRepresentContainedFiles(fDataCache,lFiles);
  end
  else
  begin
    // it's easier to just delete all three possible keys, since
    // we know where they are.
    lExistsKey := ExistsKey(aFile);
    lContentsKey := ContentsKey(aFile);
    lListKey := ListKey(aFile);
    fPromiseCache.Delete(lContentsKey);
    fPromiseCache.Delete(lListKey);
    fPromiseCache.Delete(lExistsKey);
    fDataCache.Delete(lContentsKey);
    fDataCache.Delete(lListKey);
    fDataCache.Delete(lExistsKey);
  end;

end;

procedure TFileSystemCache.Uncache(aFiles: TFileArray; aRecursive: Boolean);
var
  l: Integer;
  i: Integer;
begin
  if aRecursive then
  begin
    DeleteKeysWhichRepresentContainedFiles(fPromiseCache,aFiles);
    DeleteKeysWhichRepresentContainedFiles(fDataCache,aFiles);
  end
  else
  begin
    // it's easier to just delete the files specifically...
    l := Length(aFiles);
    for i := 0 to l - 1 do
      Uncache(aFiles[i],false);
  end;

end;

procedure TFileSystemCache.Uncache(aAsOf: TDateTime);
var
  lData: TKeyValueArray;
  l: Integer;
  i: Integer;
  lCachedData: TFileSystemCacheData;
begin
  // In this case, I *don't* get rid of the promises, because their
  // results are going to end up being newer than what's here anyway.
  lData := fDataCache.GetKeyValues;
  l := Length(lData);
  for i := 0 to l - 1 do
  begin
    lCachedData := lData[i].Data as TFileSystemCacheData;
    if lCachedData.TimeCreated < aAsOf then
       fDataCache.Delete(lData[i].Key);
  end;

end;

{ TLongStringMap }

function TLongStringMap.GetItem(const AName: UTF8String): TObject;
var
  lBase: ShortString;
  lRest: UTF8String;
begin
  if SplitKey(AName,lBase,lRest) then
  begin
     // the key was too long for a short string, so we would have
     // another map inside.
     result := fShortStringMap.Find(lBase);
     if result <> nil then
     begin
        // we found another map so look for the next.
        // It *should* be another map, or an error if it's not.
        result := (result as TLongStringMap).GetItem(lRest);
     end
     else
        // no such map was found, so we don't contain anything.
        result := nil;
  end
  else
  begin
    result := fShortStringMap.Find(lBase);
    // See .GetItem for more information about TPlaceHolder.
    if result is TPlaceholder then
       result := nil;
  end;
end;

procedure TLongStringMap.SetItem(const AName: UTF8String; AValue: TObject);
var
  lBase: ShortString;
  lRest: UTF8String;
  lSubmap: TLongStringMap;
  lIndex: Integer;
begin
  if SplitKey(AName,lBase,lRest) then
  begin
     // the key was too long for a short string, then we need to
     // create another map to store the rest. It might already be
     // created.
     lSubmap := fShortStringMap.Find(lBase) as TLongStringMap;
     if lSubmap = nil then
     begin
        // there was no map, so we have to create it.
        lSubmap := TLongStringMap.Create(fShortStringMap.OwnsObjects);
        fShortStringMap.Add(lBase,lSubmap);
        // we found another map so look for the next.
     end;
     lSubmap[lRest] := AValue;
  end
  else
  begin
    // delete the old one to make sure it's freed.
    lIndex := fShortStringMap.FindIndexOf(lBase);
    if lIndex > -1 then
      fShortStringMap.Delete(lIndex);
    if AValue <> nil then
       fShortStringMap.Add(lBase,AValue)
    else
       // The HashMap object seems to have problems with 'nil'.
       // It keeps the key, which will show up with NameOfIndex, but
       // it won't be able to do a FindIndexOf with the name. So,
       // as a workaround, a tiny object means that the key exists
       // but contains no data.
       fShortStringMap.Add(lBase,TPlaceholder.Create);
  end;
end;

function TLongStringMap.SplitKey(const aName: UTF8String; out
  aBase: ShortString; out aRest: UTF8String): Boolean;
const
  cSplitSize = Sizeof(ShortString) - 1;
begin
  // We need to return
  // true if it's exactly the length as well, since we don't want a
  // collission with something with a longer key that starts the same.
  result := Length(aName) >= cSplitSize;
  if Result then
  begin
     aBase := Copy(aName,1,cSplitSize);
     aRest := Copy(aName,cSplitSize + 1,Length(aName));
  end
  else
  begin
    aBase := aName;
    aRest := '';
  end;


end;

procedure TLongStringMap.ForEachCall(aPrefix: UTF8String;
  aCallback: TLongStringMapIteratorCallback);
var
  i: Integer;
  lKey: UTF8String;
  lItem: TObject;
begin
  for i := 0 to fShortStringMap.Count - 1 do
  begin
    lKey := aPrefix + fShortStringMap.NameOfIndex(i);
    lItem := fShortStringMap[i];
    if lItem is TLongStringMap then
    begin
      (lItem as TLongStringMap).ForEachCall(lKey,aCallback);
    end
    else
    begin
      aCallback(lKey,lItem);
    end;
  end;

end;

procedure TLongStringMap.ForEachCall(aCallback: TLongStringMapIteratorCallback);
begin
  ForEachCall('',aCallback);
end;

constructor TLongStringMap.Create(aOwnsObjects: Boolean);
begin
  inherited Create;
  fShortStringMap := TFPHashObjectList.Create(aOwnsObjects);
end;

destructor TLongStringMap.Destroy;
begin
  if not fShortStringMap.OwnsObjects then
  begin
    Clear;
    // otherwise, the map will delete everything itself,
    // possibly much quicker.
  end;
  FreeAndNil(fShortStringMap);
  inherited Destroy;
end;

procedure TLongStringMap.Clear;
var
  i: Integer;
begin
  if not fShortStringMap.OwnsObjects then
  begin
    // we still need to destroy all of the submaps, even if we
    // don't own the other ones.
    for i := 0 to fShortStringMap.Count - 1 do
    begin
      if fShortStringMap[i] is TLongStringMap then
      begin
        fShortStringMap[i].Free;
        fShortStringMap[i] := nil;
      end;
    end;
  end;
  fShortStringMap.Clear;
end;

procedure TLongStringMap.Delete(const AName: UTF8String);
var
  lBase: ShortString;
  lRest: UTF8String;
  lSubmap: TLongStringMap;
  lIndex: Integer;
begin
  if SplitKey(AName,lBase,lRest) then
  begin
     // the key was too long for a short string, then we need to
     // look for another map store the rest.
     lIndex := fShortStringMap.FindIndexOf(lBase);
     if lIndex > -1 then
     begin
        // this *should* be a sub-map.
        lSubmap := fShortStringMap[lIndex] as TLongStringMap;
        // we did find a map, so delete the rest.
        lSubmap.Delete(lRest);
        // if it's empty, then we can delete this one...
        if lSubmap.fShortStringMap.Count = 0 then
        begin
           fShortStringMap.Delete(lIndex);
           if not fShortStringMap.OwnsObjects then
              // we still have to destroy the map itself...
              lSubmap.Free;
        end;
     end;
  end
  else
  begin
    lIndex := fShortStringMap.FindIndexOf(lBase);
    if lIndex > -1 then
      fShortStringMap.Delete(fShortStringMap.FindIndexOf(lBase));
  end;
end;

function TLongStringMap.Has(const aName: UTF8String): Boolean;
var
  lBase: ShortString;
  lRest: UTF8String;
  lSubmap: TLongStringMap;
  lIndex: Integer;
begin
  result := false;
  if SplitKey(AName,lBase,lRest) then
  begin
     // the key was too long for a short string, then we need to
     // look for another map store the rest.
     lIndex := fShortStringMap.FindIndexOf(lBase);
     if lIndex > -1 then
     begin
        // this *should* be a sub-map.
        lSubmap := fShortStringMap[lIndex] as TLongStringMap;
        // we did find a map, so check that for the rest.
        result := lSubmap.Has(lRest);
     end;
  end
  else
  begin
    lIndex := fShortStringMap.FindIndexOf(lBase);
    result := lIndex > -1;
  end;
end;

function TLongStringMap.GetKeys: TStringArray;
begin
  SetLength(Result,0);
  AppendKeys('',Result);
end;

function TLongStringMap.GetKeyValues: TKeyValueArray;
begin
  SetLength(Result,0);
  AppendKeyValues('',Result);

end;

function TLongStringMap.AppendKeys(aBase: UTF8String; var aArray: TStringArray
  ): Integer;
var
  i: Integer;
  lSubMap: TLongStringMap;
begin
  // the result is going to be the next index in the array at which data
  // can be placed.
  result := Length(aArray);
  for i := 0 to fShortStringMap.Count - 1 do
  begin
    if fShortStringMap[i] is TLongStringMap then
    begin
       lSubMap := fShortStringMap[i] as TLongStringMap;
       // append the keys of this to the array, and update result to indicate
       // the current index.
       result := lSubMap.AppendKeys(aBase + fShortStringMap.NameOfIndex(i),aArray);
    end
    else
    begin
      // increase array by 1.
      SetLength(aArray,result + 1);
      // set the last item to this key
      aArray[result] := aBase + fShortStringMap.NameOfIndex(i);
      // increment result so the next one goes in one more.
      result += 1;
    end;
  end;

end;

function TLongStringMap.AppendKeyValues(aBase: UTF8String;
  var aArray: TKeyValueArray): Integer;
var
  i: Integer;
  lSubMap: TLongStringMap;
begin
  // the result is going to be the next index in the array at which data
  // can be placed.
  result := Length(aArray);
  for i := 0 to fShortStringMap.Count - 1 do
  begin
    if fShortStringMap[i] is TLongStringMap then
    begin
       lSubMap := fShortStringMap[i] as TLongStringMap;
       // append the keys of this to the array, and update result to indicate
       // the current index.
       result := lSubMap.AppendKeyValues(aBase + fShortStringMap.NameOfIndex(i),aArray);
    end
    else
    begin
      // increase array by 1.
      SetLength(aArray,result + 1);
      // set the last item to this key
      aArray[result].Key := aBase + fShortStringMap.NameOfIndex(i);
      aArray[result].Data := fShortStringMap[i];
      // increment result so the next one goes in one more.
      result += 1;
    end;
  end;

end;

end.

