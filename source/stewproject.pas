unit stewproject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewfile, stewasync, stewproperties, stewtypes, contnrs, stewattachments;

// TODO: When opening 'attachments', need a system of events and handlers
// for cases where there is ambiguity. Or, just use defaults.

// TODO: For property filing, instead of having multiple different event handlers,
// just have two: one for state change and one for errors. The filingstate property
// can be checked to get actual value. This simplifies the code hookups here, and
// just has to be translated into a main form event quickly.

// TODO: I think that, perhaps, it should be possible to "lock" a property object,
// to prevent deletion and other changes, at least from inside the application.
// The document editor tabs can then lock it, this will prevent the cache from
// being cleared, it will also prevent changing location or name while it is open,
// or at least make sure the opener can get notified.

type
  // All of these are just strings, representing paths off of the root
  // of the stew project (names always use '/' as separators, and must
  // start with a '/' if not a relative name. In the future I may
  // make use of more structured types.
  TDocumentID = String;

  TDocumentList = array of TDocumentID;
  TDeferredDocumentListCallback = procedure(Path: TFilename; Data: TDocumentList; aTarget: TObject) of object deprecated;

  TDocumentNotifyEvent = procedure(Sender: TObject; Document: TDocumentID) of object;
  TDocumentExceptionEvent = procedure(Sender: TObject; Document: TDocumentID; Error: String) of object;

  TStewProject = class;

  { TMetadataCache }
  // FUTURE: Currently, there's no way to *release* the cache, mostly because
  // I don't have any way for the project to know when cached data is no longer
  // needed (properties are needed by at least two UI areas: listing documents and
  // editing the properties). If this program gets unwieldy with large projects that
  // remain open for a long time, that would be the place to look.
  // -- I could just, periodically, remove any properties that are not marked
  // as modified. But it would be nicer to remove only those properties which
  // are not being modified in a tab.
  TMetadataCache = class
  strict private type
  type
  // this allows me access to the protected events on Document Properties,
  // but avoids making those events accessible from outside of the project,
  // which I don't want.
    TProtectedDocumentProperties = class(TDocumentProperties)
    end;
  strict private
    fProject: TStewProject;
    fDisk: TFilename;
    fID: TDocumentID;
    fFiles: TStringList;
    fDocuments: TFPHashObjectList;
    fProperties: TDocumentProperties;
  protected
    procedure PropertiesLoaded(Sender: TObject);
    procedure PropertiesLoadFailed(Sender: TObject; aError: String);
    procedure PropertiesSaveConflicted(Sender: TObject);
    procedure PropertiesSaved(Sender: TObject);
    procedure PropertiesSaveFailed(Sender: TObject; aError: String);
    procedure FilesListed(Data: TFileList);
    procedure FilesListError(Data: String);
    procedure ClearFiles;
    procedure ClearChildFiles;
    procedure AddFile(const aFile: TFilename);
    function SortDocuments(List: TStringList; Index1, Index2: Integer): Integer;
  public
    constructor Create(aProject: TStewProject; aDiskPath: TFilename;
      aID: TDocumentID; aIsRoot: Boolean);
    destructor Destroy; override;
    function PutPath(const aPath: String): TMetadataCache;
    function PutDocument(aKey: String): TMetadataCache;
    property Properties: TDocumentProperties read fProperties;
    procedure ListDocuments;
    function GetDocumentList: TDocumentList;
    procedure EditPrimary;
    procedure EditNotes;
    function GetAttachments(const aDescriptor: String): TStringArray;
  end;

  { TStewProject }

  TStewProject = class
  strict private type

    // this allows me access to the protected events on Project Properties,
    // but avoids making those events accessible from outside of the project,
    // which I don't want.
    TProtectedProjectProperties = class(TProjectProperties)
    end;

  private
    fDisk: TFilename;
    fMetadataCache: TMetadataCache;
    fOnDocumentsListed: TDocumentNotifyEvent;
    fOnDocumentListError: TDocumentExceptionEvent;
    FOnDocumentPropertiesError: TDocumentExceptionEvent;
    FOnDocumentPropertiesLoaded: TDocumentNotifyEvent;
    FOnDocumentPropertiesSaveConflicted: TDocumentNotifyEvent;
    FOnDocumentPropertiesSaved: TDocumentNotifyEvent;
    fProperties: TProjectProperties;
    FOnOpened: TNotifyEvent;
    FOnPropertiesError: TExceptionMessageEvent;
    FOnPropertiesLoaded: TNotifyEvent;
    FOnPropertiesSaveConflicted: TNotifyEvent;
    FOnPropertiesSaved: TNotifyEvent;
    function GetIsOpened: Boolean;
    function GetProperties: TProjectProperties;
    procedure OpenProjectProperties;
    procedure DoOpened;
    procedure ProjectPropertiesLoaded(Sender: TObject);
    procedure ProjectPropertiesLoadFailed(Sender: TObject; aError: String);
    procedure ProjectPropertiesSaveConflicted(Sender: TObject);
    procedure ProjectPropertiesSaved(Sender: TObject);
    procedure ProjectPropertiesSaveFailed(Sender: TObject; aError: String);
  protected
    // These events are protected. In general, the project itself handles
    // these events, and I want all UI code to handle these actions via the
    // MainForm observation API. By making these protected, I can control
    // this better (The form overrides this by creating a subclass of
    // this that can handle the events).
    property OnOpened: TNotifyEvent read FOnOpened write fOnOpened;
    property OnPropertiesLoaded: TNotifyEvent read FOnPropertiesLoaded write fOnPropertiesLoaded;
    property OnPropertiesSaved: TNotifyEvent read FOnPropertiesSaved write fOnPropertiesSaved;
    property OnPropertiesError: TExceptionMessageEvent read FOnPropertiesError write fOnPropertiesError;
    property OnPropertiesSaveConflicted: TNotifyEvent read FOnPropertiesSaveConflicted write fOnPropertiesSaveConflicted;
    property OnDocumentPropertiesLoaded: TDocumentNotifyEvent read FOnDocumentPropertiesLoaded write FOnDocumentPropertiesLoaded;
    property OnDocumentPropertiesSaved: TDocumentNotifyEvent read FOnDocumentPropertiesSaved write FOnDocumentPropertiesSaved ;
    property OnDocumentPropertiesError: TDocumentExceptionEvent read FOnDocumentPropertiesError write FOnDocumentPropertiesError;
    property OnDocumentPropertiesSaveConflicted: TDocumentNotifyEvent read FOnDocumentPropertiesSaveConflicted write FOnDocumentPropertiesSaveConflicted;
    property OnDocumentsListed: TDocumentNotifyEvent read fOnDocumentsListed write fOnDocumentsListed;
    property OnDocumentListError: TDocumentExceptionEvent read fOnDocumentListError write fOnDocumentListError;
  public
    constructor Create(const Path: TFilename);
    destructor Destroy; override;
    property DiskPath: TFilename read fDisk;
    // TODO: Figure out filtering...
    // TODO: More importantly, need to 'sort' by directory index.
    procedure ListDocuments(const aDocumentID: TDocumentID);
    function GetDocumentList(const aDocumentID: TDocumentID): TDocumentList;
    function GetDocumentProperties(const aDocument: TDocumentID): TDocumentProperties;
    procedure EditDocumentPrimary(const aDocumentID: TDocumentID);
    procedure EditDocumentNotes(const aDocumentID: TDocumentID);
    // TODO: Figure out patterns and reg ex...
    // TODO: function Match: TProjectContentEnumerator;
    // TODO: function Add(Name: TPacketBaseName): T;
    // TODO: function Get(Name: TPacketName): T;
    // TODO: procedure MoveHere(NewChild: T);
    function GetDiskPath(const ADocument: TDocumentID): TFileName;
    procedure OpenAtPath(aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
    procedure OpenInParentDirectory(aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
    procedure OpenNewAtPath;
    property IsOpened: Boolean read GetIsOpened;
    function GetProjectName: String;
    property Properties: TProjectProperties read GetProperties;
  end;

  function ExcludeLeadingSlash(Const Path: string): string;
  function IncludeLeadingSlash(Const Path : String) : String;
  function IncludeTrailingSlash(Const Path : String) : String;
  function ExcludeTrailingSlash(Const Path: string): string;
  function IsParentDocument(aParent: TDocumentID; aChild: TDocumentID): Boolean;
  function ExtractDocumentName(const Path: TDocumentID): string;
  function ExtractFileDescriptor(const Filename: TFilename): string;

  const
    RootDocument: TDocumentID = '/';

implementation

uses
  stewpersist, stewshell;

function IncludeLeadingSlash(const Path: String): String;
Var
  L : Integer;
begin
  Result:=Path;
  L:=Length(Result);
  If (L=0) or (Result[1] <> '/') then
    result := '/' + Result;
end;

// document paths are always '/' whether that's the OS path delimiter or not.
function IncludeTrailingSlash(const Path: String): String;
Var
  l : Integer;
begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or (Result[l] <> '/') then
    Result:=Result+'/';
end;

function ExcludeTrailingSlash(const Path: string): string;
Var
  L : Integer;
begin
  Result:=Path;
  L:=Length(Result);
  If (L>0) and (Result[L] = '/') then
    Delete(Result,L,1);
end;

function IsParentDocument(aParent: TDocumentID; aChild: TDocumentID): Boolean;
begin
  result := (Pos(IncludeTrailingSlash(aParent),aChild) = 1)
end;

function ExtractDocumentName(const Path: TDocumentID): string;
var
  i : longint;
begin
  I := Length(Path);
  while (I > 0) and not (Path[I] = '/') do
    Dec(I);
  Result := Copy(Path, I + 1, MaxInt);
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

function ExcludeLeadingSlash(const Path: string): string;
Var
  L : Integer;
begin
  Result:=Path;
  L:=Length(Result);
  If (L>0) and (Result[1] = '/') then
    Delete(Result,1,1);
end;

function GetDiskPath(const DiskPath: TFileName; const ADocument: TDocumentID): TFilename;
begin
  result := DiskPath + ADocument;
end;

{ TStewProject.TMetadataCache }

procedure TMetadataCache.PropertiesLoadFailed(Sender: TObject;
  aError: String);
begin
  if fProject.FOnDocumentPropertiesError <> nil then
    fProject.FOnDocumentPropertiesError(fProject,fID,aError);
end;

procedure TMetadataCache.PropertiesSaveConflicted(Sender: TObject);
begin
  if fProject.FOnDocumentPropertiesSaveConflicted <> nil then
    fProject.FOnDocumentPropertiesSaveConflicted(fProject,fID);
end;

procedure TMetadataCache.PropertiesSaved(Sender: TObject);
begin
  if fProject.FOnDocumentPropertiesSaved <> nil then
    fProject.FOnDocumentPropertiesSaved(fProject,fID);
  // just like when loaded, refresh the list.
  ListDocuments;
end;

procedure TMetadataCache.PropertiesSaveFailed(Sender: TObject;
  aError: String);
begin
  if fProject.FOnDocumentPropertiesError <> nil then
    fProject.FOnDocumentPropertiesError(fProject,fID,aError);
end;

procedure TMetadataCache.FilesListed(Data: TFileList);
var
  aPacket: TFilename;
  i: Integer;
  aChild: TMetadataCache;
begin
  // clear out the old ones.
  // NOTE: We don't want to actuall delete all of the items in the cache,
  // we just want to delete the files. If we delete the actual items, we
  // run to risk of deleting a properties object that is maintaining some
  // data which hasn't been applied to disk yet.
  ClearChildFiles;
  for i := 0 to Length(Data) - 1 do
  begin
    if (Data[i][1] <> '_') then
    begin
      aPacket := ExtractPacketName(Data[i]);
      aChild := PutDocument(aPacket);
      aChild.AddFile(Data[i]);
    end
    else
    // the file is actually 'attached' to this one. This becomes important
    // in the root metadata.
      AddFile(IncludeTrailingPathDelimiter(ExtractFileName(fDisk)) + Data[i]);
  end;
  if fProject.fOnDocumentsListed <> nil then
     fProject.fOnDocumentsListed(fProject,fID);
end;

procedure TMetadataCache.FilesListError(Data: String);
begin
  if fProject.fOnDocumentListError <> nil then
     fProject.fOnDocumentListError(fProject,fID,Data);
end;

procedure TMetadataCache.ClearFiles;
begin
  FreeAndNil(fFiles);
  ClearChildFiles;
end;

procedure TMetadataCache.ClearChildFiles;
var
  i: Integer;
begin
  if fFiles <> nil then
  begin
    for i := fFiles.Count - 1 downto 0 do
    begin
      if (Length(fFiles[i]) > 0) and (fFiles[i][1] = '_') then
         fFiles.Delete(i);
    end;
  end;
  for i := 0 to fDocuments.Count - 1 do
  begin
    (fDocuments[i] as TMetadataCache).ClearFiles;
  end;
end;

procedure TMetadataCache.AddFile(const aFile: TFilename);
begin
  if fFiles = nil then
  begin
    fFiles := TStringList.Create;
    fFiles.Duplicates := dupIgnore;
  end;
  fFiles.Add(aFile);
end;

function SimpleIndexOf(List: TStrings; aText: String): Integer; inline;
begin
  // TStringList.IndexOf has a little bit of overhead that I don't want
  // and returns -1 for not found, when I want the final value.
  result:=0;
  While (result < List.Count) and (CompareText(list[Result],aText) <> 0) do
    result:=result+1;
end;

function TMetadataCache.SortDocuments(List: TStringList; Index1,
  Index2: Integer): Integer;
var
  s1: String;
  s2: String;
  i1: Integer;
  i2: integer;
  index: TStrings;
begin
  s1 := ExtractDocumentName(List[Index1]);
  s2 := ExtractDocumentName(List[Index2]);
  index := Properties.index;
  i1 := SimpleIndexOf(index,s1);
  i2 := SimpleIndexOf(index,s2);
  if (i1 = i2) then
    result := CompareText(s1,s2)
  else
    result := i1 - i2;
end;

procedure TMetadataCache.PropertiesLoaded(Sender: TObject);
begin
  if fProject.fOnDocumentPropertiesLoaded <> nil then
     fProject.fOnDocumentPropertiesLoaded(fProject,fID);
  // since the properties might have changed the index, automatically
  // trigger a reload of the list. This means that, in order to refresh
  // the project list, you could just load the properties.
  ListDocuments;
end;

constructor TMetadataCache.Create(aProject: TStewProject;
  aDiskPath: TFilename; aID: TDocumentID; aIsRoot: Boolean);
var
  aCached: TProtectedDocumentProperties;
begin
  inherited Create;
  fProject := aProject;
  fDisk := ExcludeTrailingPathDelimiter(aDiskPath);
  fID := aID;
  fFiles := nil; // this is created as needed. A nil item here means the
                 // file has not been listed yet.
  fDocuments := TFPHashObjectList.Create(true);
  aCached := TProtectedDocumentProperties.Create(aDiskPath,aIsRoot);
  fProperties := aCached;
  aCached.OnFileLoaded:=@PropertiesLoaded;
  aCached.OnFileLoadFailed:=@PropertiesLoadFailed;
  aCached.OnFileSaveConflicted:=@PropertiesSaveConflicted;
  aCached.OnFileSaved:=@PropertiesSaved;
  aCached.OnFileSaveFailed:=@PropertiesSaveFailed;
end;

function TMetadataCache.PutPath(const aPath: String
  ): TMetadataCache;
var
  p: Integer;
  aKey: String;
  aChildren: String;
  aSearch: String;
begin
  aSearch := ExcludeLeadingSlash(ExcludeTrailingSlash(aPath));
  p := Pos('/',aSearch);
  if p > 0 then
  begin
    aKey := Copy(aSearch,1,p-1);
    aChildren := Copy(aSearch,p+1,length(aSearch));
    result := PutDocument(aKey).PutPath(aChildren);
  end
  else
    result := PutDocument(aSearch);
end;

function TMetadataCache.PutDocument(aKey: String): TMetadataCache;
begin
  // the documents are case insensitive, so make these lowercase.
  result := fDocuments.Find(LowerCase(aKey)) as TMetadataCache;
  if result = nil then
  begin
    result := TMetadataCache.Create(fProject,IncludeTrailingPathDelimiter(fDisk) + aKey,IncludeTrailingSlash(fID) + aKey,false);
    fDocuments.Add(LowerCase(aKey),result);
  end;
end;

destructor TMetadataCache.Destroy;
begin
  FreeAndNil(fFiles);
  FreeAndNil(fProperties);
  FreeAndNil(fDocuments);
  inherited Destroy;
end;

procedure TMetadataCache.ListDocuments;
begin
  TListFiles.Create(fDisk,@FilesListed,@FilesListError).Enqueue;
  // also need to get the properties in order to know the right sort order.
end;

function TMetadataCache.GetDocumentList: TDocumentList;
var
  i: Integer;
  list: TEZSortStringList;
  aChild: TMetadataCache;
begin
  list := TEZSortStringList.Create;
  try
    list.OnEZSort:=@SortDocuments;
    for i := 0 to fDocuments.Count - 1 do
    begin
      // I'm only interested in documents that have associated disk files.
      // other documents can be retrieved by specific name, but I don't
      // want to display them as available if they aren't.
      aChild := fDocuments[i] as TMetadataCache;
      if (aChild.fFiles <> nil) and (aChild.fFiles.Count > 0) then
         list.Add(aChild.fID);
    end;
    // sort by property index.
    list.EZSort;
    SetLength(result,list.Count);
    for i := 0 to list.Count - 1 do
    begin
      result[i] := list[i];
    end;
  finally
    list.Free;
  end;
end;

procedure TMetadataCache.EditPrimary;
var
  aPrimaryFiles: TStringArray;
begin
  aPrimaryFiles := GetAttachments('');
  case Length(aPrimaryFiles) of
    0:
      // TODO: If the file does not exist, ask the user if they wish
      // to attempt to create the file. This requires an event.
      raise Exception.Create('Primary file does not exist yet.');
    1:
      // TODO: Open up the specified file in an application appropriate
      // for the platform and file system.
      //EditFile(IncludeTrailingPathDelimiter()
      EditFile(aPrimaryFiles[0]);
    2:
      // TODO: Should ask user which one to edit instead. This requires
      // an event.
      raise Exception.Create('Too many primary files');
  end;

end;

procedure TMetadataCache.EditNotes;
var
  aNoteFiles: TStringArray;
begin
  aNoteFiles := GetAttachments('_notes');
  case Length(aNoteFiles) of
    0:
      // TODO: If the file does not exist, ask the user if they wish
      // to attempt to create the file. This requires an event.
      raise Exception.Create('Notes file does not exist yet.');
    1:
      // TODO: Open up the specified file in an application appropriate
      // for the platform and file system.
      //EditFile(IncludeTrailingPathDelimiter()
      EditFile(aNoteFiles[0]);
    2:
      // TODO: Should ask user which one to edit instead. This requires
      // an event.
      raise Exception.Create('Too many note files');
  end;
end;

function TMetadataCache.GetAttachments(const aDescriptor: String): TStringArray;
var
  aItemDesc: String;
  aExt: String;
  i: Integer;
  l: Integer;
begin
  // <document name><descriptor><extension>
  // where descriptor includes the '_' and extension includes the '.'. Of,
  // if descriptor is blank:
  // <document name>_<extension> or <document name><extension>
  // Also, "attachments" must have an extension. If a document does not
  // have an extension, it is a directory.
  SetLength(result,0);
  for i := 0 to fFiles.Count - 1 do
  begin
    aExt := ExtractFileExt(fFiles[i]);
    if aExt <> '' then
    begin
      aItemDesc := ExtractFileDescriptor(fFiles[i]);
      if (aItemDesc = aDescriptor) or ((aDescriptor = '') and (aItemDesc = '_')) then
      begin
        l := Length(Result);
        SetLength(Result,l + 1);
        Result[l] := IncludeTrailingPathDelimiter(ExtractFileDir(fDisk)) + fFiles[i];
      end;
    end;
  end;

end;

{ TStewProject }

procedure TStewProject.ProjectPropertiesLoaded(Sender: TObject);
begin
  if fProperties.Modified then // it was new, so we want to immediately save
                               // it to make sure the file was created.
    fProperties.Save;
  if FOnPropertiesLoaded <> nil then
    FOnPropertiesLoaded(Self);
end;

procedure TStewProject.ProjectPropertiesLoadFailed(Sender: TObject;
  aError: String);
begin
  if fOnPropertiesError <> nil then
    fOnPropertiesError(Self,aError);
end;

procedure TStewProject.ProjectPropertiesSaveConflicted(Sender: TObject);
begin
  if fOnPropertiesSaveConflicted <> nil then
    fOnPropertiesSaveConflicted(Self);
end;

procedure TStewProject.ProjectPropertiesSaved(Sender: TObject);
begin
  if FOnPropertiesSaved <> nil then
    FOnPropertiesSaved(Self);
end;

procedure TStewProject.ProjectPropertiesSaveFailed(Sender: TObject;
  aError: String);
begin
  if fOnPropertiesError <> nil then
    fOnPropertiesError(Self,aError);
end;

procedure TStewProject.OpenProjectProperties;
var
  aProtectedProjectProperties: TProtectedProjectProperties;
begin
  if fProperties = nil then
  begin
    aProtectedProjectProperties := TProtectedProjectProperties.Create(fDisk);
    fProperties := aProtectedProjectProperties;
    aProtectedProjectProperties.OnFileLoaded:=@ProjectPropertiesLoaded;
    aProtectedProjectProperties.OnFileLoadFailed:=@ProjectPropertiesLoadFailed;
    aProtectedProjectProperties.OnFileSaveConflicted:=@ProjectPropertiesSaveConflicted;
    aProtectedProjectProperties.OnFileSaved:=@ProjectPropertiesSaved;
    aProtectedProjectProperties.OnFileSaveFailed:=@ProjectPropertiesSaveFailed;
    fProperties.Load;
  end;
  if fMetadataCache = nil then
  begin
    fMetadataCache := TMetadataCache.Create(Self,fDisk,RootDocument,true);
  end;
end;

function TStewProject.GetProperties: TProjectProperties;
begin
  if fProperties = nil then
     raise Exception.Create('You must open the project before you can see the properties');
  result := fProperties;
end;

function TStewProject.GetIsOpened: Boolean;
begin
  result := fProperties <> nil;
end;

procedure TStewProject.DoOpened;
begin
  // open and save project properties immediately, so that the file exists.
  OpenProjectProperties;
  if FOnOpened <> nil then
    FOnOpened(Self);
end;

constructor TStewProject.Create(const Path: TFilename);
begin
  fDisk := IncludeTrailingPathDelimiter(Path);
  fMetadataCache := nil; // created on open.
  fProperties := nil;
end;

destructor TStewProject.Destroy;
begin
  FreeAndNil(fMetadataCache);
  if fProperties <> nil then
    fProperties.Free;
  inherited Destroy;
end;


procedure TStewProject.ListDocuments(const aDocumentID: TDocumentID);
var
  props: TDocumentProperties;
begin
  if fMetadataCache = nil then
     raise Exception.Create('Please open project first');
  if aDocumentID = RootDocument then
  begin
    // TODO: Do the same as with documents in order to get the list sorted.
    props := fMetadataCache.Properties;
    if props.FilingState = fsNotLoaded then
       props.Load
    else
       fMetadataCache.ListDocuments;
  end
  else
  begin
    // the list sorting depends on the properties, which means we have to
    // load them first. The documents will be automatically listed when that
    // calls back anyway.
    props := GetDocumentProperties(aDocumentID);
    if props.FilingState = fsNotLoaded then
       props.Load
    else
       fMetadataCache.PutPath(aDocumentID).ListDocuments;
  end;
end;

function TStewProject.GetDocumentList(const aDocumentID: TDocumentID
  ): TDocumentList;
begin
  if fMetadataCache = nil then
     raise Exception.Create('Please open project first');
  if aDocumentID = RootDocument then
     result := fMetadataCache.GetDocumentList
  else
    result := fMetadataCache.PutPath(aDocumentID).GetDocumentList;

end;

function TStewProject.GetDocumentProperties(const aDocument: TDocumentID
  ): TDocumentProperties;
begin
  if fMetadataCache = nil then
     raise Exception.Create('Please open project first');
  if aDocument = RootDocument then
    result := fMetadataCache.Properties
  else
    result := fMetadataCache.PutPath(aDocument).Properties;
end;

procedure TStewProject.EditDocumentPrimary(const aDocumentID: TDocumentID);
begin
  if fMetadataCache = nil then
     raise Exception.Create('Please open project first');
  if aDocumentID = RootDocument then
     raise Exception.Create('The root document primary can not be edited');
  fMetadataCache.PutPath(aDocumentID).EditPrimary;

end;

procedure TStewProject.EditDocumentNotes(const aDocumentID: TDocumentID);
begin
  if fMetadataCache = nil then
     raise Exception.Create('Please open project first');
  if aDocumentID = RootDocument then
     fMetadataCache.EditNotes
  else
     fMetadataCache.PutPath(aDocumentID).EditNotes;

end;

function TStewProject.GetDiskPath(const ADocument: TDocumentID): TFileName;
begin
  result := stewproject.GetDiskPath(fDisk,ADocument);
end;

type

  { TProjectExists }

  TProjectExists = class(TFileExists)
  private
    fProject: TStewProject;
    fCallback: TDeferredBooleanCallback;
    procedure FileExistsCallback(Data: Boolean);
  public
    constructor Create(aProject: TStewProject; aCallback: TDeferredBooleanCallback;
      aErrorback: TDeferredExceptionCallback);
  end;

{ TProjectExists }

procedure TProjectExists.FileExistsCallback(Data: Boolean);
begin
  if (data) then
    fProject.DoOpened;
  fCallback(Data);
end;

constructor TProjectExists.Create(aProject: TStewProject;
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(TProjectProperties.GetPath(aProject.fDisk),@FileExistsCallback,aErrorback);
  fProject := aProject;
  fCallback := aCallback;
end;

procedure TStewProject.OpenAtPath(aCallback: TDeferredBooleanCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TProjectExists.Create(Self,aCallback,aErrorback).Enqueue;
end;

type

  { TSearchParentDirectories }

  TSearchParentDirectories = class
  private
    fProject: TStewProject;
    fPath: TFilename;
    fCallback: TDeferredBooleanCallback;
    fErrorback: TDeferredExceptionCallback;
    procedure FileExistsCallback(Data: Boolean);
  public
    constructor Create(aProject: TStewProject; aPath: TFilename; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
    procedure Enqueue;
  end;

{ TSearchParentDirectories }

procedure TSearchParentDirectories.FileExistsCallback(Data: Boolean);
var
  aPath: TFilename;
begin
  if (Data) then
  begin
    fProject.fDisk := fPath;
    fProject.DoOpened;
    fCallback(true);
  end
  else
  begin
    aPath := ExtractFileDir(ExcludeTrailingPathDelimiter(fPath));
    if aPath = fPath then
    begin
      fCallback(false);
    end
    else
    begin
      fPath := aPath;
      TFileExists.Create(TProjectProperties.GetPath(fPath),@FileExistsCallback,fErrorback).Enqueue;
    end;
  end;
end;

constructor TSearchParentDirectories.Create(aProject: TStewProject;
  aPath: TFilename; aCallback: TDeferredBooleanCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  inherited Create;
  fProject := aProject;
  fPath := aPath;
  fCallback := aCallback;
  fErrorback := aErrorback;
end;

procedure TSearchParentDirectories.Enqueue;
begin
  TFileExists.Create(TProjectProperties.GetPath(fPath),@FileExistsCallback,fErrorback).Enqueue;
end;


procedure TStewProject.OpenInParentDirectory(
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  TSearchParentDirectories.Create(Self,fDisk,aCallback,aErrorback).Enqueue;
end;

procedure TStewProject.OpenNewAtPath;
begin
  DoOpened;
end;

function TStewProject.GetProjectName: String;
begin
  result := ExtractFileName(ExcludeTrailingPathDelimiter(fDisk));
end;

end.

