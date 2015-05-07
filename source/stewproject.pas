unit stewproject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewfile, stewasync, stewproperties, stewtypes, contnrs;

// TODO: The additional overhead of working through the project with documents
// is getting unwieldly. Instead, expose the MetadataCache objects, keep a flag
// on them for whether they are 'root' or not, and just have a single GetDocument
// which returns the metadata.

// TODO: So that we can deal with attachments better, maybe move those off into
// a separate object:
// - edit: opens it up for editing
// - load: loads contents into a string if that's possible.
// - contents: gets the contents of the string if it's loaded.
// - loaded: Returns whether it's loaded
// - onloaded: etc.

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
  TAttachmentNotifyEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String) of object;
  TAttachmentExceptionEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String; Error: String) of object;

  TDocumentMetadata = class;

  { TAttachmentMetadata }

  TAttachmentMetadata = class
  private
    fDocument: TDocumentMetadata;
    fFileAge: Longint;
    fFilingState: TFilingState;
    fContents: String;
  protected
    function GetCandidateFiles: TStringArray; virtual;
    function GetDescriptor: String; virtual; abstract;
    function GetDefaultExtension: String; virtual; abstract;
    function GetName: String; virtual; abstract;
    procedure FileLoaded(aData: TStream; aFileAge: Longint);
    procedure FileLoadFailed(Data: String);
    procedure FileSaveConflicted({%H-}aFileAge: Longint);
    procedure FileSaved(aFileAge: Longint);
    procedure FileSaveFailed(Data: String);
  public
    constructor Create(aDocument: TDocumentMetadata);
    procedure Load;
    procedure Save(aNewContents: String; aForce: Boolean = false);
    procedure OpenInEditor;
    function GetContents: String;
    property FilingState: TFilingState read fFilingState;
  end;

  { TPrimaryMetadata }

  TPrimaryMetadata = class(TAttachmentMetadata)
  protected
    function GetDescriptor: String; override;
    function GetName: String; override;
    function GetDefaultExtension: String; override;
  end;

  { TNotesMetadata }

  TNotesMetadata = class(TAttachmentMetadata)
  protected
    function GetDescriptor: String; override;
    function GetName: String; override;
    function GetDefaultExtension: String; override;
  end;

  { TSynopsisMetadata }

  TSynopsisMetadata = class(TAttachmentMetadata)
  protected
    function GetCandidateFiles: TStringArray; override;
    function GetDescriptor: String; override;
    function GetName: String; override;
    function GetDefaultExtension: String; override;
  end;

  TStewProject = class;

  { TDocumentMetadata }
  // FUTURE: Currently, there's no way to *release* the cache, mostly because
  // I don't have any way for the project to know when cached data is no longer
  // needed (properties are needed by at least two UI areas: listing documents and
  // editing the properties). If this program gets unwieldy with large projects that
  // remain open for a long time, that would be the place to look.
  // -- I could just, periodically, remove any properties that are not marked
  // as modified. But it would be nicer to remove only those properties which
  // are not being modified in a tab.
  TDocumentMetadata = class
  strict private type
  type
  // this allows me access to the protected events on Document Properties,
  // but avoids making those events accessible from outside of the project,
  // which I don't want.
    TProtectedDocumentProperties = class(TDocumentProperties)
    end;
  strict private
    fListingState: TListingState;
    fProject: TStewProject;
    fDisk: TFilename;
    fID: TDocumentID;
    fFiles: TStringList;
    fContents: TFPHashObjectList;
    fProperties: TDocumentProperties;
    fSynopsis: TSynopsisMetadata;
    fPrimary: TPrimaryMetadata;
    fNotes: TNotesMetadata;
    function GetPrimary: TPrimaryMetadata;
  protected
    procedure PropertiesLoaded(Sender: TObject);
    procedure PropertiesLoadFailed(Sender: TObject; aError: String);
    procedure PropertiesSaveConflicted(Sender: TObject);
    procedure PropertiesSaved(Sender: TObject);
    procedure PropertiesSaveFailed(Sender: TObject; aError: String);
    procedure PropertiesLoading(Sender: TObject);
    procedure PropertiesSaving(Sender: TObject);
    procedure AttachmentLoading(const aName: String);
    procedure AttachmentLoaded(const aName: String);
    procedure AttachmentLoadFailed(const aName: String; aError: String);
    procedure AttachmentSaving(const aName: String);
    procedure AttachmentSaved(const aName: String);
    procedure AttachmentSaveConflicted(const aName: String);
    procedure AttachmentSaveFailed(const aName: String; aError: String);
    procedure FilesListed(Data: TFileList; aRecursive: Boolean);
    procedure FilesListedNonrecursive(Data: TFileList);
    procedure FilesListedRecursive(Data: TFileList);
    procedure FilesListError(Data: String);
    procedure ClearFiles;
    procedure AddFile(const aFile: TFilename);
    function SortDocuments(List: TStringList; Index1, Index2: Integer): Integer;
    function PutPath(const aPath: String): TDocumentMetadata;
    function PutDocument(aKey: String): TDocumentMetadata;
    function GetAttachments(const aDescriptor: String; const aExtension: String): TStringArray;
    function GetNewAttachmentName(const aDescriptor: String; const aExtension: String): String;
    function GetSynopsis: TSynopsisMetadata;
    property Project: TStewProject read fProject;
  public
    constructor Create(aProject: TStewProject; aDiskPath: TFilename;
      aID: TDocumentID; aIsRoot: Boolean);
    destructor Destroy; override;
    property Properties: TDocumentProperties read fProperties;
    // This isn't a true recursive. It is more of a recursive refresh that doesn't
    // cause new documents to appear. It will list the current document, yes.
    // But it will only cause children docs to list themselves if they have already
    // been listed.
    procedure ListDocuments(Recursive: Boolean);
    property ListingState: TListingState read fListingState;
    function GetContents: TDocumentList;
    function AreAttachmentsListed: Boolean;
    property Synopsis: TSynopsisMetadata read GetSynopsis;
    property Primary: TPrimaryMetadata read GetPrimary;
    property Notes: TNotesMetadata read fNotes;
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
    fMetadataCache: TDocumentMetadata;
    FOnDocumentAttachmentError: TAttachmentExceptionEvent;
    FOnDocumentAttachmentLoaded: TAttachmentNotifyEvent;
    FOnDocumentAttachmentLoading: TAttachmentNotifyEvent;
    FOnDocumentAttachmentSaveConflicted: TAttachmentNotifyEvent;
    FOnDocumentAttachmentSaved: TAttachmentNotifyEvent;
    FOnDocumentAttachmentSaving: TAttachmentNotifyEvent;
    fOnDocumentsListed: TDocumentNotifyEvent;
    fOnDocumentListError: TDocumentExceptionEvent;
    FOnDocumentPropertiesError: TDocumentExceptionEvent;
    FOnDocumentPropertiesLoaded: TDocumentNotifyEvent;
    fOnDocumentPropertiesLoading: TDocumentNotifyEvent;
    fOnDocumentPropertiesSaving: TDocumentNotifyEvent;
    FOnDocumentPropertiesSaveConflicted: TDocumentNotifyEvent;
    FOnDocumentPropertiesSaved: TDocumentNotifyEvent;
    FOnPropertiesLoading: TNotifyEvent;
    fOnPropertiesSaving: TNotifyEvent;
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
    procedure ProjectPropertiesLoading(Sender: TObject);
    procedure ProjectPropertiesSaving(Sender: TObject);
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
    property OnPropertiesSaving: TNotifyEvent read fOnPropertiesSaving write FOnPropertiesSaving;
    property OnPropertiesLoading: TNotifyEvent read FOnPropertiesLoading write FOnPropertiesLoading;
    property OnDocumentPropertiesLoaded: TDocumentNotifyEvent read FOnDocumentPropertiesLoaded write FOnDocumentPropertiesLoaded;
    property OnDocumentPropertiesSaved: TDocumentNotifyEvent read FOnDocumentPropertiesSaved write FOnDocumentPropertiesSaved ;
    property OnDocumentPropertiesError: TDocumentExceptionEvent read FOnDocumentPropertiesError write FOnDocumentPropertiesError;
    property OnDocumentPropertiesSaveConflicted: TDocumentNotifyEvent read FOnDocumentPropertiesSaveConflicted write FOnDocumentPropertiesSaveConflicted;
    property OnDocumentPropertiesSaving: TDocumentNotifyEvent read fOnDocumentPropertiesSaving write fOnDocumentPropertiesSaving;
    property OnDocumentPropertiesLoading: TDocumentNotifyEvent read fOnDocumentPropertiesLoading write fOnDocumentPropertiesLoading;
    property OnDocumentAttachmentLoading: TAttachmentNotifyEvent read FOnDocumentAttachmentLoading write FOnDocumentAttachmentLoading;
    property OnDocumentAttachmentLoaded: TAttachmentNotifyEvent read FOnDocumentAttachmentLoaded write FOnDocumentAttachmentLoaded;
    property OnDocumentAttachmentError: TAttachmentExceptionEvent read FOnDocumentAttachmentError write FOnDocumentAttachmentError;
    property OnDocumentAttachmentSaving: TAttachmentNotifyEvent read FOnDocumentAttachmentSaving write FOnDocumentAttachmentSaving;
    property OnDocumentAttachmentSaved: TAttachmentNotifyEvent read FOnDocumentAttachmentSaved write FOnDocumentAttachmentSaved;
    property OnDocumentAttachmentSaveConflicted: TAttachmentNotifyEvent read FOnDocumentAttachmentSaveConflicted write FOnDocumentAttachmentSaveConflicted;
    property OnDocumentsListed: TDocumentNotifyEvent read fOnDocumentsListed write fOnDocumentsListed;
    property OnDocumentListError: TDocumentExceptionEvent read fOnDocumentListError write fOnDocumentListError;
  public
    constructor Create(const Path: TFilename);
    destructor Destroy; override;
    property DiskPath: TFilename read fDisk;
    // TODO: Figure out filtering...
    // TODO: More importantly, need to 'sort' by directory index.
    function GetDocument(const aDocumentID: TDocumentID): TDocumentMetadata;
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

  const
    RootDocument: TDocumentID = '/';

implementation

uses
  stewshell;

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

{ TSynopsisMetadata }

function TSynopsisMetadata.GetCandidateFiles: TStringArray;
begin
  Result:=fDocument.GetAttachments(GetDescriptor,GetDefaultExtension);
end;

function TSynopsisMetadata.GetDescriptor: String;
begin
  result := '_synopsis';
end;

function TSynopsisMetadata.GetName: String;
begin
  result := 'Synopsis';

end;

function TSynopsisMetadata.GetDefaultExtension: String;
begin
  result := '.txt';
end;

{ TNotesMetadata }

function TNotesMetadata.GetDescriptor: String;
begin
  result := '_notes';
end;

function TNotesMetadata.GetName: String;
begin
  result := 'Notes';
end;

function TNotesMetadata.GetDefaultExtension: String;
begin
  result := fDocument.Project.Properties.defaultNotesExtension;
end;

{ TPrimaryMetadata }

function TPrimaryMetadata.GetDescriptor: String;
begin
  result := '';
end;

function TPrimaryMetadata.GetName: String;
begin
  result := 'Primary';
end;

function TPrimaryMetadata.GetDefaultExtension: String;
begin
  // TODO: Do I need a '.'?
  result := fDocument.Project.GetProperties.defaultDocExtension;
end;

{ TAttachmentMetadata }

procedure TAttachmentMetadata.FileLoaded(aData: TStream; aFileAge: Longint);
begin
  if (aData = nil) then
  begin
    // the file does not exist yet, so create a blank data object.
    fContents := '';
  end
  else
  begin
    with TStringStream.Create('') do
    try
      CopyFrom(aData,0);
      fContents := DataString;
    finally
      Free;
    end;
  end;
  fFileAge := aFileAge;
  fFilingState := fsLoaded;
  fDocument.AttachmentLoaded(GetName);

end;

procedure TAttachmentMetadata.FileLoadFailed(Data: String);
begin
  fFilingState := fsError;
  fDocument.AttachmentLoadFailed(GetName,Data);

end;

procedure TAttachmentMetadata.FileSaveConflicted(aFileAge: Longint);
begin
  fFilingState := fsConflict;
  fDocument.AttachmentSaveConflicted(GetName);
end;

procedure TAttachmentMetadata.FileSaved(aFileAge: Longint);
begin
  fFileAge := aFileAge;
  fFilingState := fsLoaded;
  fDocument.AttachmentSaved(GetName);
end;

procedure TAttachmentMetadata.FileSaveFailed(Data: String);
begin
  // this doesn't make the state an error, because the data in the
  // document is still valid, so just return it to loaded.
  fFilingState := fsLoaded;
  fDocument.AttachmentSaveFailed(GetName,Data);
end;

function TAttachmentMetadata.GetCandidateFiles: TStringArray;
begin
  result := fDocument.GetAttachments(GetDescriptor,'');
end;

constructor TAttachmentMetadata.Create(aDocument: TDocumentMetadata);
begin
  inherited Create;
  fDocument := aDocument;
  fFilingState := fsNotLoaded;
  fContents := '';
end;

procedure TAttachmentMetadata.Load;
var
  aCandidates: TStringArray;
begin
  if fFilingState in [fsNotLoaded,fsLoaded,fsError,fsConflict] then
  begin
    aCandidates := GetCandidateFiles;
    case Length(aCandidates) of
      0:
      // just pretend it's loaded.
        FileLoaded(nil,NewFileAge);
      1:
      begin
        fFilingState := fsLoading;
        fDocument.AttachmentLoading(GetName);
        ReadFile(aCandidates[0],@FileLoaded,@FileLoadFailed);
      end
    else
        // TODO: Should ask user which one to load instead. This requires
        // an event.
        raise Exception.Create('Too many ' + GetName + ' files');
    end;
  end
  else if fFilingState = fsSaving then
     raise Exception.Create('Can''t load attachment data while saving.');
  // otherwise, already loading, so ignore.
end;

procedure TAttachmentMetadata.Save(aNewContents: String; aForce: Boolean);
var
  aCandidates: TStringArray;
begin
  if aNewContents <> fContents then
  begin
    fContents := aNewContents;
    if fFilingState in [fsLoaded,fsConflict] then
    begin
      aCandidates := GetCandidateFiles;
      case Length(aCandidates) of
        0:
        // not ready yet, so create a new name.
        begin
          SetLength(aCandidates,1);
          aCandidates[0] := fDocument.GetNewAttachmentName(GetDescriptor,GetDefaultExtension);
        end;
        1: // do nothing, we save in a minute.
      else
          // TODO: Should ask user which one to load instead. This requires
          // an event.
          raise Exception.Create('Too many ' + GetName + ' files');
      end;

      fFilingState := fsSaving;
      fDocument.AttachmentSaving(GetName);
      WriteFile(aCandidates[0],false,not aForce,fFileAge,aNewContents,@FileSaved,@FileSaveConflicted,@FileSaveFailed);
    end
    else if fFilingState = fsNotLoaded then
      raise Exception.Create('Can''t save attachment data when it has not yet been loaded')
    else if fFilingState = fsLoading then
      raise Exception.Create('Can''t save attachment data while still loading.')
    else if fFilingState = fsError then
      raise Exception.Create('Can''t save attachment data when the last load failed.');
    // otherwise, already saving, so don't worry about it.
  end;
end;

procedure TAttachmentMetadata.OpenInEditor;
var
  aCandidates: TStringArray;
begin
  aCandidates := GetCandidateFiles;
  case Length(aCandidates) of
    0:
      // TODO: If the file does not exist, ask the user if they wish
      // to attempt to create the file. This requires an event.
      raise Exception.Create(GetName + ' file does not exist yet.');
    1:
      // TODO: Open up the specified file in an application appropriate
      // for the platform and file system.
      //EditFile(IncludeTrailingPathDelimiter()
      EditFile(aCandidates[0]);
  else
      // TODO: Should ask user which one to edit instead. This requires
      // an event.
      raise Exception.Create('Too many ' + GetName + ' files');
  end;

end;

function TAttachmentMetadata.GetContents: String;
begin
  result := fContents;
end;

{ TStewProject.TDocumentMetadata }

procedure TDocumentMetadata.PropertiesLoadFailed(Sender: TObject;
  aError: String);
begin
  if fProject.FOnDocumentPropertiesError <> nil then
    fProject.FOnDocumentPropertiesError(fProject,fID,aError);
end;

procedure TDocumentMetadata.PropertiesSaveConflicted(Sender: TObject);
begin
  if fProject.FOnDocumentPropertiesSaveConflicted <> nil then
    fProject.FOnDocumentPropertiesSaveConflicted(fProject,fID);
end;

procedure TDocumentMetadata.PropertiesSaved(Sender: TObject);
begin
  if fProject.FOnDocumentPropertiesSaved <> nil then
    fProject.FOnDocumentPropertiesSaved(fProject,fID);
end;

procedure TDocumentMetadata.PropertiesSaveFailed(Sender: TObject;
  aError: String);
begin
  if fProject.FOnDocumentPropertiesError <> nil then
    fProject.FOnDocumentPropertiesError(fProject,fID,aError);
end;

procedure TDocumentMetadata.FilesListed(Data: TFileList; aRecursive: Boolean);
var
  aPacket: TFilename;
  i: Integer;
  aChild: TDocumentMetadata;
begin
  // TODO: If recursive, I want to refresh all existing lists, but I don't
  // want to do any lists that aren't already listed. I need a listing state
  // for this.

  // clear out the old ones.
  // NOTE: We don't want to actuall delete all of the items in the cache,
  // we just want to delete the files. If we delete the actual items, we
  // run to risk of deleting a properties object that is maintaining some
  // data which hasn't been applied to disk yet. Also, we don't
  // want to delete everything recursively.
  ClearFiles;
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

  if aRecursive then
  begin
    for i := 0 to fContents.Count - 1 do
    begin
      aChild := fContents[i] as TDocumentMetadata;
      if aChild.ListingState = lsListed then
        aChild.ListDocuments(true);
    end;

  end;

  fListingState := lsListed;
  if fProject.fOnDocumentsListed <> nil then
     fProject.fOnDocumentsListed(fProject,fID);
end;

procedure TDocumentMetadata.FilesListedNonrecursive(Data: TFileList);
begin
  FilesListed(Data,false);
end;

procedure TDocumentMetadata.FilesListError(Data: String);
begin
  if fProject.fOnDocumentListError <> nil then
     fProject.fOnDocumentListError(fProject,fID,Data);
end;

procedure TDocumentMetadata.ClearFiles;
var
  i: Integer;
  j: Integer;
  aChild: TDocumentMetadata;
begin
  if fFiles <> nil then
  begin
    for i := fFiles.Count - 1 downto 0 do
    begin
      if (Length(fFiles[i]) = 0) or (fFiles[i][1] = '_') then
         fFiles.Delete(i);
    end;
  end;
  for i := 0 to fContents.Count - 1 do
  begin
    aChild := fContents[i] as TDocumentMetadata;
    for j := aChild.fFiles.Count - 1 downto 0 do
    begin
      if (Length(aChild.fFiles[j]) = 0) or (aChild.fFiles[j][1] <> '_') then
         aChild.fFiles.Delete(j);
    end;
  end;
end;

procedure TDocumentMetadata.AddFile(const aFile: TFilename);
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

function TDocumentMetadata.SortDocuments(List: TStringList; Index1,
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

procedure TDocumentMetadata.PropertiesLoading(Sender: TObject);
begin
  if fProject.fOnDocumentPropertiesLoading <> nil then
    fProject.fOnDocumentPropertiesLoading(fProject,fID);
end;

procedure TDocumentMetadata.PropertiesSaving(Sender: TObject);
begin
  if fProject.fOnDocumentPropertiesSaving <> nil then
    fProject.fOnDocumentPropertiesSaving(fProject,fID);
end;

procedure TDocumentMetadata.AttachmentLoading(const aName: String);
begin
  if fProject.fOnDocumentAttachmentLoading <> nil then
    fProject.fOnDocumentAttachmentLoading(fProject,fID,aName);

end;

procedure TDocumentMetadata.AttachmentLoaded(const aName: String);
begin
  if fProject.fOnDocumentAttachmentLoaded <> nil then
    fProject.fOnDocumentAttachmentLoaded(fProject,fID,aName);

end;

procedure TDocumentMetadata.AttachmentLoadFailed(const aName: String;
  aError: String);
begin
  if fProject.FOnDocumentAttachmentError <> nil then
    fProject.FOnDocumentAttachmentError(fProject,fID,aName,aError);

end;

procedure TDocumentMetadata.AttachmentSaving(const aName: String);
begin
  if fProject.fOnDocumentAttachmentSaving <> nil then
    fProject.fOnDocumentAttachmentSaving(fProject,fID,aName);

end;

procedure TDocumentMetadata.AttachmentSaved(const aName: String);
begin
  if fProject.fOnDocumentAttachmentSaved <> nil then
    fProject.fOnDocumentAttachmentSaved(fProject,fID,aName);

end;

procedure TDocumentMetadata.AttachmentSaveConflicted(const aName: String);
begin
  if fProject.fOnDocumentAttachmentSaveConflicted <> nil then
    fProject.fOnDocumentAttachmentSaveConflicted(fProject,fID,aName);

end;

procedure TDocumentMetadata.AttachmentSaveFailed(const aName: String;
  aError: String);
begin
  if fProject.FOnDocumentAttachmentError <> nil then
    fProject.FOnDocumentAttachmentError(fProject,fID,aName,aError);

end;

procedure TDocumentMetadata.FilesListedRecursive(Data: TFileList);
begin
  FilesListed(Data,true);
end;

function TDocumentMetadata.GetPrimary: TPrimaryMetadata;
begin
  if fPrimary <> nil then
    result := fPrimary
  else
     raise Exception.Create('The root document does not have a primary file');
end;

procedure TDocumentMetadata.PropertiesLoaded(Sender: TObject);
begin
  if fProject.fOnDocumentPropertiesLoaded <> nil then
     fProject.fOnDocumentPropertiesLoaded(fProject,fID);
end;

constructor TDocumentMetadata.Create(aProject: TStewProject;
  aDiskPath: TFilename; aID: TDocumentID; aIsRoot: Boolean);
var
  aCached: TProtectedDocumentProperties;
begin
  inherited Create;
  fProject := aProject;
  fDisk := ExcludeTrailingPathDelimiter(aDiskPath);
  fID := aID;
  fListingState := lsNotListed;
  fFiles := nil; // this is created as needed. A nil item here means the
                 // file has not been listed yet.
  if aIsRoot then
  begin
    fSynopsis := nil;
    fPrimary := nil;
  end
  else
  begin
    fSynopsis := TSynopsisMetadata.Create(Self);
    fPrimary := TPrimaryMetadata.Create(Self);
  end;
  // root document *can* have notes.
  fNotes := TNotesMetadata.Create(Self);

  fContents := TFPHashObjectList.Create(true);
  aCached := TProtectedDocumentProperties.Create(aDiskPath,aIsRoot);
  fProperties := aCached;
  aCached.OnFileLoaded:=@PropertiesLoaded;
  aCached.OnFileLoadFailed:=@PropertiesLoadFailed;
  aCached.OnFileSaveConflicted:=@PropertiesSaveConflicted;
  aCached.OnFileSaved:=@PropertiesSaved;
  aCached.OnFileSaveFailed:=@PropertiesSaveFailed;
  aCached.OnFileSaving:=@PropertiesSaving;
  aCached.OnFileLoading:=@PropertiesLoading;
end;

function TDocumentMetadata.PutPath(const aPath: String
  ): TDocumentMetadata;
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

function TDocumentMetadata.PutDocument(aKey: String): TDocumentMetadata;
begin
  // the documents are case insensitive, so make these lowercase.
  result := fContents.Find(LowerCase(aKey)) as TDocumentMetadata;
  if result = nil then
  begin
    result := TDocumentMetadata.Create(fProject,IncludeTrailingPathDelimiter(fDisk) + aKey,IncludeTrailingSlash(fID) + aKey,false);
    fContents.Add(LowerCase(aKey),result);
  end;
end;

destructor TDocumentMetadata.Destroy;
begin
  FreeAndNil(fFiles);
  FreeAndNil(fNotes);
  FreeAndNil(fPrimary);
  FreeAndNil(fSynopsis);
  FreeAndNil(fProperties);
  FreeAndNil(fContents);
  inherited Destroy;
end;

procedure TDocumentMetadata.ListDocuments(Recursive: Boolean);
begin
  // the list sorting depends on the properties, which means we have to
  // make sure they are loaded first. The documents will be automatically
  // listed when that calls back anyway.
  if fListingState <> lsListing then
  begin
    fListingState := lsListing;
    if Recursive then
       ListFiles(fDisk,@FilesListedRecursive,@FilesListError)
    else
       ListFiles(fDisk,@FilesListedNonrecursive,@FilesListError);
  end;

end;

function TDocumentMetadata.GetContents: TDocumentList;
var
  i: Integer;
  list: TEZSortStringList;
  aChild: TDocumentMetadata;
begin
  list := TEZSortStringList.Create;
  try
    list.OnEZSort:=@SortDocuments;
    for i := 0 to fContents.Count - 1 do
    begin
      // I'm only interested in documents that have associated disk files.
      // other documents can be retrieved by specific name, but I don't
      // want to display them as available if they aren't.
      aChild := fContents[i] as TDocumentMetadata;
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

function TDocumentMetadata.AreAttachmentsListed: Boolean;
begin
  result := fFiles <> nil;
end;

function TDocumentMetadata.GetAttachments(const aDescriptor: String;
  const aExtension: String): TStringArray;
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
  if fFiles <> nil then
    for i := 0 to fFiles.Count - 1 do
    begin
      aExt := ExtractFileExt(fFiles[i]);
      if (aExt <> '') and ((aExtension = '') or (aExt = aExtension)) then
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

function TDocumentMetadata.GetNewAttachmentName(const aDescriptor: String;
  const aExtension: String): String;
begin
  result := fDisk + aDescriptor + aExtension;
end;

function TDocumentMetadata.GetSynopsis: TSynopsisMetadata;
begin
  if fSynopsis <> nil then
    result := fSynopsis
  else
     raise Exception.Create('The root document does not have a synopsis');
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
    aProtectedProjectProperties.OnFileSaving:=@ProjectPropertiesSaving;
    aProtectedProjectProperties.OnFileLoading:=@ProjectPropertiesLoading;
    fProperties.Load;
  end;
  if fMetadataCache = nil then
  begin
    fMetadataCache := TDocumentMetadata.Create(Self,fDisk,RootDocument,true);
  end;
end;

function TStewProject.GetProperties: TProjectProperties;
begin
  if fProperties = nil then
     raise Exception.Create('You must open the project before you can see the properties');
  result := fProperties;
end;

procedure TStewProject.ProjectPropertiesLoading(Sender: TObject);
begin
  if FOnPropertiesLoading <> nil then
    FOnPropertiesLoading(Self);

end;

procedure TStewProject.ProjectPropertiesSaving(Sender: TObject);
begin
  if FOnPropertiesSaving <> nil then
    FOnPropertiesSaving(Self);
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
  // TODO: Should I be including, or excluding? Documents have it
  // excluded (for good reason, since it's easier to find attachments and
  // they're not always directories).
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

function TStewProject.GetDocument(const aDocumentID: TDocumentID
  ): TDocumentMetadata;
begin
  if fMetadataCache = nil then
     raise Exception.Create('Please open project first');
  if aDocumentID = RootDocument then
    result := fMetadataCache
  else
     result := fMetadataCache.PutPath(aDocumentID);
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
    fProject.fDisk := IncludeTrailingPathDelimiter(fPath);
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
      CheckFileExistence(TProjectProperties.GetPath(fPath),@FileExistsCallback,fErrorback);
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
  CheckFileExistence(TProjectProperties.GetPath(fPath),@FileExistsCallback,fErrorback);
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

