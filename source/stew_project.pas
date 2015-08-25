unit stew_project;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, sys_file, sys_async, stew_properties, stew_types, contnrs;

type
  { TDocumentID }

  TDocumentID = record
  strict private
    fID: UTF8String;
    function GetContainer: TDocumentID;
    function GetIsNull: Boolean;
    function GetIsSystem: Boolean;
    function GetName: UTF8String;
  public
    property IsNull: Boolean read GetIsNull;
    property IsSystem: Boolean read GetIsSystem;
    property ID: String read fID;
    property Name: UTF8String read GetName;
    property Container: TDocumentID read GetContainer;
    function GetContainedDocument(aName: UTF8String): TDocumentID;
    function Contains(aChild: TDocumentID): Boolean;
    class function FromString(const aPath: UTF8String): TDocumentID; static;
    const Root: TDocumentID = ( fID: '/');
    const Null: TDocumentID = ( fID: '');
    class function GetSystemDocument(aName: UTF8String): TDocumentID; static;
  end;

  TDocumentList = array of TDocumentID;

  TDocumentNotifyEvent = procedure(Sender: TObject; Document: TDocumentID) of object;
  TDocumentExceptionEvent = procedure(Sender: TObject; Document: TDocumentID; Error: String) of object;
  TAttachmentNotifyEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String) of object;
  TAttachmentExceptionEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String; Error: String) of object;
  TAttachmentConfirmationEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String; out Answer: Boolean) of object;
  TAttachmentChoiceEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String; aChoices: TStringArray; var Answer: String; out Accepted: Boolean) of object;

  TOrderDocumentPosition = (odpBefore,odpAfter);

  TDocumentMetadata = class;

  { TAttachmentMetadata }

  TAttachmentMetadata = class
  private
    fDocument: TDocumentMetadata;
    fFileAge: Longint;
    fFilingState: TFilingState;
    fContents: String;
    fModified: Boolean;
  protected
    function GetCandidateFiles: TFile.TFileArray; virtual;
    function GetDescriptor: String; virtual; abstract;
    function GetDefaultExtension: String; virtual; abstract;
    function GetDefaultFile: TFile; virtual;
    function GetName: String; virtual; abstract;
    procedure FileLoaded(aSender: TPromise); overload;
    procedure FileLoaded(const aData: String; aAge: Longint); overload;
    procedure FileLoadFailed({%H-}aSender: TPromise; Data: String);
    // TODO: Get rid of this once we're using promises everywhere...
    procedure FileLoadFailedNonPromise(Data: String);
    procedure FileSaveConflictCheck(aSender: TPromise; aData: TPromiseException);
    procedure FileSaveConflicted({%H-}aFileAge: Longint);
    procedure FileSaved(aSender: TPromise);
    procedure FileSaveFailed(Data: String);
    function GetContents: String;
    procedure SetContents(AValue: String);
    procedure EditorTemplatesListed(aSender: TPromise);
    procedure EditableFileWritten({%H-}aSender: TPromise);
    procedure EditableFileReady({%H-}aSender: TPromise);
  public
    constructor Create(aDocument: TDocumentMetadata);
    procedure Load;
    procedure Save(aForce: Boolean = false);
    // FUTURE: Consider this possibility if I ever add internal editing which requires a RichMemo or something.
    // procedure Read(aCallback: TDeferredStreamCallback), would
    //   allow reading directly from the stream, for things like
    //   rich text content. You couldn't use GetContents, and the
    //   'loading' and 'loaded' events wouldn't work, but you could
    //   still use the 'loadfailed'. And 'age' would still be kept.
    // procedure Write(aStream: TStream; aCallback: TDeferredCallback), again
    //   would allow writing directly. 'saveFailed' and 'saveConflicted' would
    //   still happen though.
    procedure OpenInEditor;
    property Contents: String read GetContents write SetContents;
    property Modified: Boolean read fModified;
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
    function GetCandidateFiles: TFile.TFileArray; override;
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
  // this allows me access to the protected events on Document Properties,
  // but avoids making those events accessible from outside of the project,
  // which I don't want.
    TProtectedDocumentProperties = class(TDocumentProperties)
    end;
  strict private
    fListingState: TListingState;
    fIsNew: Boolean;
    fLock: TObject;
    fProject: TStewProject;
    // NOTE: This is a 'virtual' TFile, since the actual data is split
    // across multiple files.
    fDisk: TFile;
    fID: TDocumentID;
    fFiles: TFileList;
    fContents: TFPHashObjectList;
    fProperties: TDocumentProperties;
    fSynopsis: TSynopsisMetadata;
    fPrimary: TPrimaryMetadata;
    fNotes: TNotesMetadata;
    function GetIsLocked: Boolean;
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
    procedure FileRenameFailed({%H-}aSender: TPromise; Data: String);
    procedure FilesRenamed({%H-}aSender: TPromise);
    procedure FilesListed(Data: TFile.TFileArray; aRecursive: Boolean);
    procedure FilesListedNonrecursive(aSender: TPromise);
    procedure FilesListedRecursive(aSender: TPromise);
    procedure FilesListError({%H-}aSender: TPromise; Data: String);
    procedure StateChanged;
    function DoConfirmNewAttachment(aName: String): Boolean;
    function DoChooseTemplate(aAttachmentName: String;
      const aTemplates: TTemplateArray; out aTemplate: TTemplate): Boolean;
    procedure ClearFiles;
    procedure AddFile(const aFile: TFile);
    function SortDocuments(List: TStringList; Index1, Index2: Integer): Integer;
    function PutPath(const aPath: String): TDocumentMetadata;
    function PutDocument(aKey: String): TDocumentMetadata;
    function GetDocument(aKey: String): TDocumentMetadata;
    function GetAttachmentFiles(aDescriptor: String; aExtension: String): TFile.TFileArray;
    function GetUncreatedAttachmentFile(const aDescriptor: String; const aExtension: String): TFile;
    function GetSynopsis: TSynopsisMetadata;
    property Project: TStewProject read fProject;
    procedure DirectoryCreated;
  public
    constructor Create(aProject: TStewProject; aDiskPath: TFile;
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
    property IsNew: Boolean read fIsNew;
    property IsLocked: Boolean read GetIsLocked;
    procedure Lock(aLockingObject: TObject);
    procedure Unlock(aLockingObject: TObject);
    procedure CreateDocument(aName: String);
    class function IsTroublesome(aName: String): Boolean;
    function HasDocument(aName: String): Boolean;
    function GetParent: TDocumentMetadata;
    function GetName: String;
    procedure Rename(aOldName: String; aNewName: String);
    procedure MoveDocToHere(aOldChild: TDocumentMetadata);
    procedure OrderDocument(aDoc: TDocumentMetadata; aPosition: TOrderDocumentPosition; aRelative: TDocumentMetadata); overload;
  end;

  { TProjectPromise }

  TProjectPromise = class(TPromise)
  protected
    fPath: TFile;
    fProject: TStewProject;
    procedure ResolveCreateProject(aPath: TFile);
    procedure ProjectOpened(Sender: TPromise);
  public
    constructor Enqueue(aPath: TFile);
    property Project: TStewProject read fProject;
    property Path: TFile read FPath;
  end;

  { TProjectOpenAtPromise }

  TProjectOpenAtPromise = class(TProjectPromise)
  private
    procedure FileExists(Sender: TPromise);
  protected
    procedure DoTask; override;
  end;

  { TProjectOpenInParentPromise }

  TProjectOpenInParentPromise = class(TProjectPromise)
  private
    procedure FileExistsInParent(Sender: TPromise);
    procedure FileExists(Sender: TPromise);
  protected
    procedure DoTask; override;
  end;

  { TProjectCreateAtPromise }

  TProjectCreateAtPromise = class(TProjectPromise)
  protected
    procedure DoTask; override;
  end;

  { TStewProject }

  TStewProject = class
  strict private type

    // this allows me access to the protected events on Project Properties,
    // but avoids making those events accessible from outside of the project,
    // which I don't want.
    TProtectedProjectProperties = class(TProjectProperties)
    end;

    { TProjectExists }

    TProjectExists = class
    private
      fProject: TStewProject;
      fCallback: TDeferredBooleanCallback;
      fErrorback: TDeferredExceptionCallback;
      procedure FileExistsCallback(aSender: TPromise);
      procedure FileExistsFailed({%H-}aSender: TPromise; Error: String);
    public
      constructor Create(aProject: TStewProject; aCallback: TDeferredBooleanCallback;
        aErrorback: TDeferredExceptionCallback);
      procedure Enqueue;
    end;

    { TSearchParentDirectories }

    TSearchParentDirectories = class
    private
      fProject: TStewProject;
      fPath: TFile;
      fCallback: TDeferredBooleanCallback;
      fErrorback: TDeferredExceptionCallback;
      procedure FileExistsCallback(aSender: TPromise);
      procedure FileExistsFailed({%H-}aSender: TPromise; Data: String);
    public
      constructor Create(aProject: TStewProject; aPath: TFile; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
      procedure Enqueue;
    end;

  private
    fDisk: TFile;
    fMetadataCache: TDocumentMetadata;
    FOnChooseTemplate: TAttachmentChoiceEvent;
    FOnConfirmNewAttachment: TAttachmentConfirmationEvent;
    FOnDocumentAttachmentError: TAttachmentExceptionEvent;
    FOnDocumentAttachmentLoaded: TAttachmentNotifyEvent;
    FOnDocumentAttachmentLoading: TAttachmentNotifyEvent;
    FOnDocumentAttachmentSaveConflicted: TAttachmentNotifyEvent;
    FOnDocumentAttachmentSaved: TAttachmentNotifyEvent;
    FOnDocumentAttachmentSaving: TAttachmentNotifyEvent;
    FOnDocumentChanged: TDocumentNotifyEvent;
    FOnDocumentCreated: TDocumentNotifyEvent;
    FOnDocumentRenameFailed: TDocumentExceptionEvent;
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
    FOnPropertiesError: TExceptionMessageEvent;
    FOnPropertiesLoaded: TNotifyEvent;
    FOnPropertiesSaveConflicted: TNotifyEvent;
    FOnPropertiesSaved: TNotifyEvent;
    function GetIsOpened: Boolean;
    function GetProperties: TProjectProperties;
    function OpenProjectProperties: TFileReadPromise;
    function DoOpened: TFileReadPromise;
    procedure ProjectPropertiesLoading(Sender: TObject);
    procedure ProjectPropertiesSaving(Sender: TObject);
    procedure ProjectPropertiesLoaded(Sender: TObject);
    procedure ProjectPropertiesLoadFailed(Sender: TObject; aError: String);
    procedure ProjectPropertiesSaveConflicted(Sender: TObject);
    procedure ProjectPropertiesSaved(Sender: TObject);
    procedure ProjectPropertiesSaveFailed(Sender: TObject; aError: String);
  protected
    // NOTE: This is protected because project objects should be created
    // async in order to check.
    {%H-}constructor Create(const Path: TFile);
  public
    // TODO: Do I want these events protected?
    // In general, the project itself handles
    // these events, and I want all UI code to handle these actions via the
    // MainForm observation API. By making these protected, I can control
    // this better (The form overrides this by creating a subclass of
    // this that can handle the events).

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
    property OnDocumentCreated: TDocumentNotifyEvent read FOnDocumentCreated write FOnDocumentCreated;
    property OnDocumentChanged: TDocumentNotifyEvent read FOnDocumentChanged write FOnDocumentChanged;
    property OnDocumentRenameFailed: TDocumentExceptionEvent read FOnDocumentRenameFailed write FOnDocumentRenameFailed;
    property OnConfirmNewAttachment: TAttachmentConfirmationEvent read FOnConfirmNewAttachment write FOnConfirmNewAttachment;
    property OnChooseTemplate: TAttachmentChoiceEvent read FOnChooseTemplate write FOnChooseTemplate;
  public
    constructor Create;
    destructor Destroy; override;
    property DiskPath: TFile read fDisk;
    function GetDocument(const aDocumentID: TDocumentID): TDocumentMetadata;
    property IsOpened: Boolean read GetIsOpened;
    function GetProjectName: String;
    property Properties: TProjectProperties read GetProperties;
    class function Open(aPath: TFile): TProjectPromise;
    class function OpenInParent(aPath: TFile): TProjectPromise;
    class function CreateNew(aPath: TFile): TProjectPromise;
  end;


  function ExcludeLeadingSlash(Const Path: UTF8String): UTF8String;
  function IncludeLeadingSlash(Const Path : UTF8String) : UTF8String;
  function IncludeTrailingSlash(Const Path : UTF8String) : UTF8String;
  function ExcludeTrailingSlash(Const Path: UTF8String): UTF8String;

  operator = (a: TDocumentID; b: TDocumentID): Boolean;

  const
    AlwaysForbiddenNameCharacters: set of char = [#0..#$1F,#$7F,'<','>',':','"','/','\','|','?','*','%','[',']','~','{','}',';'];
    WhitespaceCharacters: set of char = [' ',#$A0];//,#$1680,#$180e,#$2000..#$200A,#$2028,#$2029,#$202F,#$3000]


implementation

function IncludeLeadingSlash(const Path: UTF8String): UTF8String;
Var
  L : Integer;
begin
  Result:=Path;
  L:=Length(Result);
  If (L=0) or (Result[1] <> '/') then
    result := '/' + Result;
end;

// document paths are always '/' whether that's the OS path delimiter or not.
function IncludeTrailingSlash(const Path: UTF8String): UTF8String;
Var
  l : Integer;
begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or (Result[l] <> '/') then
    Result:=Result+'/';
end;

function ExcludeTrailingSlash(const Path: UTF8String): UTF8String;
Var
  L : Integer;
begin
  Result:=Path;
  L:=Length(Result);
  If (L>0) and (Result[L] = '/') then
    Delete(Result,L,1);
end;

function ExcludeLeadingSlash(const Path: UTF8String): UTF8String;
Var
  L : Integer;
begin
  Result:=Path;
  L:=Length(Result);
  If (L>0) and (Result[1] = '/') then
    Delete(Result,1,1);
end;

operator=(a: TDocumentID; b: TDocumentID): Boolean;
begin
  result := CompareText(a.ID,b.ID) = 0;
end;

{ TProjectCreateAtPromise }

procedure TProjectCreateAtPromise.DoTask;
begin
  ResolveCreateProject(Path);
end;

{ TProjectOpenInParentPromise }

procedure TProjectOpenInParentPromise.FileExistsInParent(Sender: TPromise);
begin
  fProject := (Sender as TProjectOpenInParentPromise).Project;
  Resolve;
end;

procedure TProjectOpenInParentPromise.FileExists(Sender: TPromise);
var
  lParent: TFile;
begin
  if ((Sender as TBooleanPromise).Answer) then
  begin
    ResolveCreateProject(Path);
  end
  else
  begin
    lParent := Path.Directory;
    if lParent = Path then
    begin
      fProject := nil;
      Resolve;
    end
    else
    begin
      TProjectOpenInParentPromise.Enqueue(lParent).After(@FileExistsInParent,@SubPromiseRejected);
    end;
  end;
end;

procedure TProjectOpenInParentPromise.DoTask;
begin
  TProjectProperties.GetPath(Path).CheckExistence.After(@FileExists,@SubPromiseRejected);
end;

{ TProjectPromise }

procedure TProjectPromise.ProjectOpened(Sender: TPromise);
begin
  Resolve;
end;

procedure TProjectPromise.ResolveCreateProject(aPath: TFile);
begin
  fProject := TStewProject.Create(aPath);
  fProject.DoOpened.After(@ProjectOpened,@SubPromiseRejected);
end;

constructor TProjectPromise.Enqueue(aPath: TFile);
begin
  fPath := aPath;
  inherited Enqueue;
end;

{ TProjectOpenAtPromise }

procedure TProjectOpenAtPromise.FileExists(Sender: TPromise);
begin
  if ((Sender as TBooleanPromise).Answer) then
  begin
    ResolveCreateProject(Path);
  end
  else
  begin
    fProject := nil;
    Resolve;
  end;
end;

procedure TProjectOpenAtPromise.DoTask;
begin
  // TODO: Once we move over to the new Properties format, just
  // make this a 'read', and pass the data onto the project constructor.
  TProjectProperties.GetPath(Path).CheckExistence.After(@FileExists,@SubPromiseRejected);
end;

{ TDocumentID }

function TDocumentID.GetContainer: TDocumentID;
var
  i : longint;
begin
  // the root is always it's own container... mind blown, dude!
  if fID <> '/' then
  begin
    I := Length(fID);
    while (I > 0) and not (fID[I] = '/') do
      Dec(I);
    if i > 1 then
       result.fID := Copy(fID, 1, I - 1)
    else
       result.fID := '/';
  end
  else
    result.fID := '/';
end;

function TDocumentID.GetIsNull: Boolean;
begin
  result := fID = '';
end;

function TDocumentID.GetIsSystem: Boolean;
begin
  if Length(fID) > 0 then
    result := fID[1] = ':';
end;

function TDocumentID.GetName: UTF8String;
var
  i : longint;
begin
  I := Length(fID);
  while (I > 0) and not (fID[I] = '/') do
    Dec(I);
  Result := Copy(fID, I + 1, MaxInt);
end;

function TDocumentID.GetContainedDocument(aName: UTF8String): TDocumentID;
begin
  result.fID := IncludeTrailingSlash(fID) + aName;
end;

function TDocumentID.Contains(aChild: TDocumentID): Boolean;
var
  aChildContainer: TDocumentID;
begin
  aChildContainer := aChild.Container;
  result := (aChildContainer = Self) or // child's parent is this guy
            ((aChildContainer <> TDocumentID.Root) and // child's parent is not root
            Contains(aChildContainer)); // this guy is a parent of the child's parent
end;

class function TDocumentID.FromString(const aPath: UTF8String): TDocumentID;
begin
  result.fID := IncludeLeadingSlash(ExcludeTrailingSlash(aPath));
end;

class function TDocumentID.GetSystemDocument(aName: UTF8String): TDocumentID;
begin
  result.fID := ':' + aName;
end;

{ TSynopsisMetadata }

function TSynopsisMetadata.GetCandidateFiles: TFile.TFileArray;
begin
  Result:=fDocument.GetAttachmentFiles(GetDescriptor,GetDefaultExtension);
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
  result := fDocument.Project.GetProperties.defaultDocExtension;
end;

{ TAttachmentMetadata }

procedure TAttachmentMetadata.FileLoaded(aSender: TPromise);
begin
  FileLoaded(((aSender as TFileReadPromise).Reader as TFileTextReader).Data,(aSender as TFileReadPromise).Age);

end;

procedure TAttachmentMetadata.FileLoaded(const aData: String; aAge: Longint);
begin
  fContents := aData;
  fFileAge := aAge;
  fFilingState := fsLoaded;
  fDocument.AttachmentLoaded(GetName);

end;

procedure TAttachmentMetadata.FileLoadFailed(aSender: TPromise; Data: String);
begin
  fFilingState := fsError;
  fDocument.AttachmentLoadFailed(GetName,Data);

end;

procedure TAttachmentMetadata.FileLoadFailedNonPromise(Data: String);
begin
  fFilingState := fsError;
  fDocument.AttachmentLoadFailed(GetName,Data);

end;

procedure TAttachmentMetadata.FileSaveConflictCheck(aSender: TPromise;
  aData: TPromiseException);
begin
  if (aSender as TFileWritePromise).IsConflict then
     FileSaveConflicted((aSender as TFileWritePromise).Age)
  else
     FileSaveFailed(aData);
end;

procedure TAttachmentMetadata.FileSaveConflicted(aFileAge: Longint);
begin
  fFilingState := fsConflict;
  fDocument.AttachmentSaveConflicted(GetName);
end;

procedure TAttachmentMetadata.FileSaved(aSender: TPromise);
begin
  fFileAge := (aSender as TFileWritePromise).Age;
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

procedure TAttachmentMetadata.SetContents(AValue: String);
begin
  if fContents <> AValue then
  begin;
    fContents := AValue;
    fModified := true;

  end;
end;

const EmptyFileTemplate: TTemplate = ( Name: 'Empty File'; ID: '');

procedure TAttachmentMetadata.EditorTemplatesListed(aSender: TPromise);
var
  aTemplate: TTemplate;
  lData: TTemplateArray;
begin
  lData := (aSender as TFileListTemplatesPromise).Templates;
  if Length(lData) = 0 then
  // create a simple, basic, blank file.
     GetDefaultFile.Write(TFileTextWriter.Create('')).After(@EditableFileWritten,@FileLoadFailed)
  else
  begin
    if Length(lData) > 1 then
    begin
      if (GetDefaultExtension = '') then
      begin
        // no extension is known, so it's possible that they might
        // want to just create a blank template, like above.
        SetLength(lData,Length(lData) + 1);
        lData[Length(lData) - 1] := EmptyFileTemplate;
      end;
      if not fDocument.DoChooseTemplate(GetName,lData,aTemplate) then
        Exit;
    end
    else
      aTemplate := lData[0];
    if aTemplate.Name = EmptyFileTemplate.Name then
      // we're creating a blank file anyway, but in this case,
      // we know that we don't know what extension it is, so set
      // the extension to '.txt'.
       GetDefaultFile.WithDifferentExtension('txt').Write(TFileTextWriter.Create('')).After(@EditableFileWritten,@FileLoadFailed)
    else
       GetDefaultFile.CreateFromTemplate(aTemplate).After(@EditableFileReady,@FileLoadFailed);
  end;

end;

procedure TAttachmentMetadata.EditableFileWritten(aSender: TPromise);
begin
  EditableFileReady(aSender);
end;

procedure TAttachmentMetadata.EditableFileReady(aSender: TPromise);
var
  aFile: TFile;
begin
  aFile := GetDefaultFile;
  fDocument.AddFile(aFile);
  aFile.OpenInEditor;
end;


function TAttachmentMetadata.GetCandidateFiles: TFile.TFileArray;
begin
  result := fDocument.GetAttachmentFiles(GetDescriptor,'');
end;

function TAttachmentMetadata.GetDefaultFile: TFile;
begin
  result := fDocument.GetUncreatedAttachmentFile(GetDescriptor,GetDefaultExtension);
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
  aCandidates: TFileArray;
begin
  if fFilingState in [fsNotLoaded,fsLoaded,fsError,fsConflict] then
  begin
    aCandidates := GetCandidateFiles;
    case Length(aCandidates) of
      0:
      // just pretend it's loaded, but with no contents.
        FileLoaded('',NewFileAge);
      1:
      begin
        fFilingState := fsLoading;
        fDocument.AttachmentLoading(GetName);
        aCandidates[0].Read(TFileTextReader.Create).After(@FileLoaded,@FileLoadFailed);
      end
    else
        raise Exception.Create('Too many ' + GetName + ' files');
    end;
  end
  else if fFilingState = fsSaving then
     raise Exception.Create('Can''t load attachment data while saving.');
  // otherwise, already loading, so ignore.
end;

procedure TAttachmentMetadata.Save(aForce: Boolean);
var
  aCandidates: TFileArray;
  lOptions: TFileWriteOptions;
begin
  if Modified then
  begin
    if fFilingState in [fsLoaded,fsConflict] then
    begin
      aCandidates := GetCandidateFiles;
      case Length(aCandidates) of
        0:
        // not ready yet, so create a new name.
        begin
          SetLength(aCandidates,1);
          aCandidates[0] := GetDefaultFile;
        end;
        1: // do nothing, we save in a minute.
      else
          raise Exception.Create('Too many ' + GetName + ' files');
      end;

      fFilingState := fsSaving;
      fDocument.AttachmentSaving(GetName);
      lOptions := [];
      if not aForce then
        lOptions := lOptions + [fwoCheckAge];
      aCandidates[0].Write(lOptions,fFileAge,TFileTextWriter.Create(fContents)).After(@FileSaved,@FileSaveConflictCheck);
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
  aCandidates: TFileArray;
begin
  aCandidates := GetCandidateFiles;
  case Length(aCandidates) of
    0:
    if fDocument.DoConfirmNewAttachment(GetName) then
    begin
      GetDefaultFile.ListTemplatesFor.After(@EditorTemplatesListed,@FileLoadFailed);
    end;
    1:
      aCandidates[0].OpenInEditor;
  else
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
  if fIsNew then
    // we need to mark that the directory was automatically created.
    DirectoryCreated;
  if fProject.FOnDocumentPropertiesSaved <> nil then
    fProject.FOnDocumentPropertiesSaved(fProject,fID);
end;

procedure TDocumentMetadata.PropertiesSaveFailed(Sender: TObject;
  aError: String);
begin
  if fProject.FOnDocumentPropertiesError <> nil then
    fProject.FOnDocumentPropertiesError(fProject,fID,aError);
end;

procedure TDocumentMetadata.FilesListed(Data: TFile.TFileArray; aRecursive: Boolean);
var
  aPacket: TFilename;
  i: Integer;
  aChild: TDocumentMetadata;
begin

  // clear out the old ones.
  // NOTE: We don't want to actuall delete all of the items in the cache,
  // we just want to delete the files. If we delete the actual items, we
  // run to risk of deleting a properties object that is maintaining some
  // data which hasn't been applied to disk yet. Also, we don't
  // want to delete everything recursively.
  ClearFiles;
  for i := 0 to Length(Data) - 1 do
  begin
    if (Data[i].BaseName[1] <> '_') then
    begin
      aPacket := Data[i].PacketName;
      aChild := PutDocument(aPacket);
      aChild.AddFile(Data[i]);
    end
    else
    // the file is actually 'attached' to this one. This becomes important
    // in the root metadata.
      AddFile(Data[i]);
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

procedure TDocumentMetadata.FilesListedNonrecursive(aSender: TPromise);
begin
  FilesListed((aSender as TFileListPromise).Files,false);
end;

procedure TDocumentMetadata.FilesListError(aSender: TPromise; Data: String);
begin
  if fProject.fOnDocumentListError <> nil then
     fProject.fOnDocumentListError(fProject,fID,Data);
end;

procedure TDocumentMetadata.StateChanged;
begin
  if fProject.fOnDocumentChanged <> nil then
     fProject.fOnDocumentChanged(fProject,fID);
end;

function TDocumentMetadata.DoConfirmNewAttachment(aName: String): Boolean;
begin
  if fProject.fOnConfirmNewAttachment <> nil then
     fProject.fOnConfirmNewAttachment(fProject,fID,aName,Result)
  else
     raise Exception.Create('Can''t confirm a new ' + aName + ' file for ' + fID.ID);
end;

function TDocumentMetadata.DoChooseTemplate(aAttachmentName: String; const aTemplates: TTemplateArray;
  out aTemplate: TTemplate): Boolean;
var
  aNames: TStringArray;
  aName: String;
  i: Integer;
  l: Integer;
begin
  if fProject.fOnChooseTemplate <> nil then
  begin
    l := Length(aTemplates);
    SetLength(aNames,l);
    for i := 0 to l - 1 do
    begin
      aNames[i] := aTemplates[i].Name;
    end;
    aName := '';
    fProject.fOnChooseTemplate(fProject,fID,aAttachmentName,aNames,aName,Result);
    aTemplate.Name:='';
    aTemplate.ID := '';
    if result then
    begin
      for i := 0 to l - 1 do
      begin
        if aTemplates[i].Name = aName then
        begin
          aTemplate := aTemplates[i];
          break;
        end;

      end;
      if aTemplate.Name = '' then
        raise Exception.Create('Invalid result from choose template');
    end;

  end
  else
     raise Exception.Create('Too many possible templates are available for the new ' + aName + ' file for ' + fID.ID);
end;

procedure TDocumentMetadata.ClearFiles;
var
  i: Integer;
  j: Integer;
  aChild: TDocumentMetadata;
begin
  for i := fFiles.Count - 1 downto 0 do
  begin
    if (fFiles[i].Name[1] = '_') then
       fFiles.Delete(i);
  end;
  for i := 0 to fContents.Count - 1 do
  begin
    aChild := fContents[i] as TDocumentMetadata;
    for j := aChild.fFiles.Count - 1 downto 0 do
    begin
      if (aChild.fFiles[j].Name[1] <> '_') then
         aChild.fFiles.Delete(j);
    end;
  end;
end;

procedure TDocumentMetadata.AddFile(const aFile: TFile);
begin
  if fFiles.IndexOf(aFile) = -1 then
     fFiles.Add(aFile);
  if fIsNew then
    // we now know that it has disk stuff available, so the document
    // is definitely not "new";
    fIsNew := false;
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
  s1 := TDocumentID.FromString(List[Index1]).Name;
  s2 := TDocumentID.FromString(List[Index2]).Name;
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

procedure TDocumentMetadata.FilesListedRecursive(aSender: TPromise);
begin
  FilesListed((aSender as TFileListPromise).Files,true);
end;

function TDocumentMetadata.GetPrimary: TPrimaryMetadata;
begin
  if fPrimary <> nil then
    result := fPrimary
  else
     raise Exception.Create('The root document does not have a primary file');
end;

procedure TDocumentMetadata.FileRenameFailed(aSender: TPromise; Data: String);
begin
  if fProject.fOnDocumentRenameFailed <> nil then
     fProject.fOnDocumentRenameFailed(fProject,fID,Data);
  ListDocuments(true);
end;

procedure TDocumentMetadata.FilesRenamed(aSender: TPromise);
begin
  // now, refresh the parent so that this thing appears under the new location.
  // TODO: With the new Promise system, we keep track of the old file and the
  // new, in the Promise, which means we can refresh both of them.
  ListDocuments(true);
end;

function TDocumentMetadata.GetIsLocked: Boolean;
begin
  result := fLock <> nil;
end;

procedure TDocumentMetadata.PropertiesLoaded(Sender: TObject);
begin
  if fProject.fOnDocumentPropertiesLoaded <> nil then
     fProject.fOnDocumentPropertiesLoaded(fProject,fID);
end;

constructor TDocumentMetadata.Create(aProject: TStewProject;
  aDiskPath: TFile; aID: TDocumentID; aIsRoot: Boolean);
var
  aCached: TProtectedDocumentProperties;
begin
  inherited Create;
  fProject := aProject;
  fDisk := aDiskPath;
  fID := aID;
  fIsNew := false;
  fLock := nil;
  fListingState := lsNotListed;
  fFiles := TFileList.Create(fDisk.System);
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
  result := GetDocument(aKey);
  if result = nil then
  begin
    result := TDocumentMetadata.Create(fProject,fDisk.GetContainedFile(aKey),fID.GetContainedDocument(aKey),false);
    fContents.Add(LowerCase(aKey),result);
  end;
end;

function TDocumentMetadata.GetDocument(aKey: String): TDocumentMetadata;
begin
  result := fContents.Find(LowerCase(aKey)) as TDocumentMetadata;
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
       fDisk.List.After(@FilesListedRecursive,@FilesListError)
    else
       fDisk.List.After(@FilesListedNonrecursive,@FilesListError);
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
      // I'm only interested in documents that have associated disk files,
      // or ones marked specifically as 'new'.
      // Other documents can be retrieved by specific name, but I don't
      // want to display them as available if they aren't.
      aChild := fContents[i] as TDocumentMetadata;
      if (aChild.fFiles.Count > 0) or (aChild.fIsNew) then
         list.Add(aChild.fID.ID);
    end;
    // sort by property index.
    list.EZSort;
    SetLength(result,list.Count);
    for i := 0 to list.Count - 1 do
    begin
      result[i] := TDocumentID.FromString(list[i]);
    end;
  finally
    list.Free;
  end;
end;

function TDocumentMetadata.AreAttachmentsListed: Boolean;
begin
  result := fProject.GetDocument(fID.Container).ListingState = lsListed;
end;

procedure TDocumentMetadata.Lock(aLockingObject: TObject);
begin
  if fLock <> aLockingObject then
  begin;
    if IsLocked then
      raise Exception.Create('Document ' + fID.ID + ' is already locked');
    fLock := aLockingObject;
    StateChanged;
  end;
end;

procedure TDocumentMetadata.Unlock(aLockingObject: TObject);
begin
  if not IsLocked then
    raise Exception.Create('Document ' + fID.ID + ' is not locked');
  if fLock <> aLockingObject then
    raise Exception.Create('Document ' + fID.ID + ' was locked with a different object');
  fLock := nil;
  StateChanged;
end;

procedure TDocumentMetadata.CreateDocument(aName: String);
var
  aChild: TDocumentMetadata;
begin
  if ListingState <> lsListed then
     raise Exception.Create('Can''t create content in unlisted documents');
  if HasDocument(aName) then
     raise Exception.Create('Document named "' + aName + '" already exists.');
  if IsTroublesome(aName) then
     raise Exception.Create('Document name "' + aName + '" isn''t going to work.');
  aChild := PutDocument(aName);
  // mark it as new so it shows up in the listings...
  aChild.fIsNew := true;
  // alert the main form that the document listing has changed.
  if fProject.fOnDocumentCreated <> nil then
     fProject.fOnDocumentCreated(fProject,aChild.fID);
  // but also automatically 'list' the files, which shouldn't exist yet, right?
  // so that it will appear as 'listed'. Note that this has to be done
  // after the oncreated, or we're getting events for an uncreated document,
  // which is weird.
  aChild.FilesListedNonrecursive(nil);
end;

class function TDocumentMetadata.IsTroublesome(aName: String): Boolean;
var
  i: Integer;
  l: Integer;

begin
  result := false;
  if aName <> '' then
  begin
    l := Length(aName);
    for i := 1 to l do
    begin
      result :=
         (aName[i] in AlwaysForbiddenNameCharacters) or
         (((i = 1) or (i = l)) and ((aName[i] in WhitespaceCharacters) or (aName[i] = '-'))) or
         ((i > 1) and (aName[i] in WhitespaceCharacters) and (aName[i-1] in WhitespaceCharacters));
      if result then
        break;
    end;
  end
  else
    result := true;

end;

function TDocumentMetadata.HasDocument(aName: String): Boolean;
begin
  result := GetDocument(aName) <> nil;
end;

function TDocumentMetadata.GetParent: TDocumentMetadata;
begin
  if fID <> TDocumentID.Root then
    result := fProject.GetDocument(fID.Container)
  else
    result := nil;
end;

function TDocumentMetadata.GetName: String;
begin
  result := fID.Name;
end;

procedure TDocumentMetadata.Rename(aOldName: String; aNewName: String);
var
  i: Integer;
  aOld: TDocumentMetadata;
  source: TFileArray;
  target: TFileArray;
begin
  if ListingState <> lsListed then
    raise Exception.Create('Please make sure the document is listed before attempting to rename');
  if HasDocument(aNewName) then
     raise Exception.Create('A document named "' + aNewName + '" already exists.');
  if IsTroublesome(aNewName) then
     raise Exception.Create('Document name "' + aNewName + '" isn''t going to work.');

  aOld := GetDocument(aOldName);
  if aOld = nil then
     raise Exception.Create('There is no document named ' + aOldName);
  aOld.Lock(Self);

  SetLength(source,aOld.fFiles.Count);
  SetLength(target,aOld.fFiles.Count);
  for i := 0 to aOld.fFiles.Count - 1 do
  begin
    source[i] := aOld.fFiles[i];
    // only change the name if it's actually supposed to be associated with it...
    if source[i].PacketName = aOldName then
       target[i] := aOld.fFiles[i].WithDifferentPacketName(aNewName)
    else
       target[i] := source[i];
  end;

  // finally, remove the old one, it will get relisted later.
  fContents.Remove(aOld);

  TFileSystem.RenameFiles(source,target).After(@FilesRenamed,@FileRenameFailed);
end;

procedure TDocumentMetadata.MoveDocToHere(aOldChild: TDocumentMetadata);
var
  i: Integer;
  aOldParent: TDocumentMetadata;
  lSource: TFileArray;
  lTarget: TFileArray;
begin
  aOldParent := aOldChild.GetParent;
  if (aOldParent.ListingState <> lsListed) or
     (ListingState <> lsListed) then
    raise Exception.Create('Please make sure the documents are listed before attempting to move');
  if HasDocument(aOldChild.GetName) then
     raise Exception.Create('A document named "' + aOldChild.GetName + '" already exists there.');

  aOldChild.Lock(Self);

  SetLength(lSource,aOldChild.fFiles.Count);
  SetLength(lTarget,aOldChild.fFiles.Count);
  for i := 0 to aOldChild.fFiles.Count - 1 do
  begin
    lSource[i] := aOldChild.fFiles[i];
    lTarget[i] := fDisk.GetContainedFile(aOldChild.fFiles[i].Name);
  end;

  // finally, remove the old one, it will get relisted later.
  aOldParent.fContents.Remove(aOldChild);
  // report a change in state so the listings get refreshed where wanted.
  aOldParent.StateChanged;

  TFileSystem.RenameFiles(lSource,lTarget).After(@FilesRenamed,@FileRenameFailed);
end;

procedure TDocumentMetadata.OrderDocument(aDoc: TDocumentMetadata;
  aPosition: TOrderDocumentPosition; aRelative: TDocumentMetadata);
var
  docIndex: Integer;
  relIndex: Integer;
  aList: TDocumentList;
  i: Integer;
begin
  if Properties.FilingState <> fsLoaded then
     raise Exception.Create('Properties must be loaded before document order can be changed');
  Lock(Self);
  try

    // remove the document from the index.
    docIndex := Properties.index.IndexOf(aDoc.GetName);
    if docIndex > -1 then
       Properties.index.Delete(docIndex);

    relIndex := Properties.index.IndexOf(aRelative.GetName);
    if relIndex = -1 then
    begin
      // we need to fix the positions up until the relative or the doc.
      // in order to get the effect we need.
      aList := GetContents;
      for i := 0 to Length(aList) - 1 do
      begin
        if Properties.index.Count <= i then
        begin
          if aList[i] = aRelative.fID then
          begin
            if aPosition = odpAfter then
              Properties.index.Add(aList[i].Name);
            Properties.index.Add(aDoc.GetName);
            // don't actually put the relative in if it's 'before',
            // because we'll still retain the same effect without adding
            // a whole bunch of things to the list.
            break;

          end
          else
            Properties.index.Add(aList[i].Name);
        end;
      end;

    end
    else
    begin
      case aPosition of
        odpBefore:
          docIndex := relIndex;
        odpAfter:
          docIndex := relIndex + 1;
      end;
      Properties.index.Insert(docIndex,aDoc.GetName);
    end;

    // and finally, save the index.
    Properties.Save;

  finally
    Unlock(Self);
  end;

end;

function TDocumentMetadata.GetAttachmentFiles(aDescriptor: String;
  aExtension: String): TFile.TFileArray;
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
  aExtension := ExcludeExtensionDelimiter(aExtension);
  aDescriptor := ExcludeDescriptorDelimiter(aDescriptor);
  SetLength(result,0);
  if fFiles <> nil then
    for i := 0 to fFiles.Count - 1 do
    begin
      aExt := fFiles[i].Extension;
      if (aExt <> '') and ((aExtension = '') or (aExt = aExtension)) then
      begin
        aItemDesc := fFiles[i].Descriptor;
        if (aItemDesc = aDescriptor) then
        begin
          l := Length(Result);
          SetLength(Result,l + 1);
          Result[l] := fFiles[i];
        end;
      end;
    end;

end;

function TDocumentMetadata.GetUncreatedAttachmentFile(const aDescriptor: String;
  const aExtension: String): TFile;
begin
  result := fDisk.WithDifferentDescriptorAndExtension(aDescriptor,aExtension);
end;

function TDocumentMetadata.GetSynopsis: TSynopsisMetadata;
begin
  if fSynopsis <> nil then
    result := fSynopsis
  else
     raise Exception.Create('The root document does not have a synopsis');
end;

procedure TDocumentMetadata.DirectoryCreated;
var
  aParent: TDocumentMetadata;
begin
  fIsNew := false;
  if not (fID = TDocumentID.Root) then
  begin
    // Add the 'directory' name to the files.
    AddFile(fDisk);
    aParent := fProject.GetDocument(fID.Container);
    if aParent.IsNew and not (aParent.fID = TDocumentID.Root) then
      aParent.DirectoryCreated;
  end;
  StateChanged;

end;

{ TStewProject }

procedure TStewProject.ProjectPropertiesLoaded(Sender: TObject);
begin
  if fProperties.Modified then // it was new, so we want to immediately save
                               // it to make sure the file was created, and
                               // that the project now exists.
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

function TStewProject.OpenProjectProperties: TFileReadPromise;
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
    result := fProperties.Load;
  end;
  if fMetadataCache = nil then
  begin
    fMetadataCache := TDocumentMetadata.Create(Self,fDisk,TDocumentID.Root,true);
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

function TStewProject.DoOpened: TFileReadPromise;
begin
  // open and save project properties immediately, so that the file exists.
  result := OpenProjectProperties;
end;

constructor TStewProject.Create(const Path: TFile);
begin
  fDisk := Path;
  fMetadataCache := nil; // created on open.
  fProperties := nil;
end;

constructor TStewProject.Create;
begin
  raise Exception.Create('Please use one of the static functions ("Open...") to open a new project');
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
  if aDocumentID.IsNull then
     raise Exception.Create('Can''t retrieve a null document');
  if aDocumentID = TDocumentID.Root then
    result := fMetadataCache
  else
     result := fMetadataCache.PutPath(aDocumentID.ID);
end;

{ TProjectExists }

procedure TStewProject.TProjectExists.FileExistsCallback(aSender: TPromise);
begin
  if ((aSender as TBooleanPromise).Answer) then
    fProject.DoOpened;
  fCallback((aSender as TBooleanPromise).Answer);
  Free;
end;

procedure TStewProject.TProjectExists.FileExistsFailed(aSender: TPromise;
  Error: String);
begin
  fErrorback(Error);
  Free;
end;

constructor TStewProject.TProjectExists.Create(aProject: TStewProject;
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create;
  fProject := aProject;
  fCallback := aCallback;
  fErrorback := aErrorBack;
end;

procedure TStewProject.TProjectExists.Enqueue;
begin
  TProjectProperties.GetPath(fProject.fDisk).CheckExistence.After(@FileExistsCallback,@FileExistsFailed);
end;

{ TSearchParentDirectories }

procedure TStewProject.TSearchParentDirectories.FileExistsFailed(
  aSender: TPromise; Data: String);
begin
  fErrorback(Data);
  Free;
end;

procedure TStewProject.TSearchParentDirectories.FileExistsCallback(
  aSender: TPromise);
var
  aPath: TFile;
begin
  if ((aSender as TBooleanPromise).Answer) then
  begin
    fProject.fDisk := fPath;
    fProject.DoOpened;
    fCallback(true);
  end
  else
  begin
    aPath := fPath.Directory;
    if aPath = fPath then
    begin
      fCallback(false);
    end
    else
    begin
      TSearchParentDirectories.Create(fProject,aPath,fCallback,fErrorback).Enqueue;
//      fPath := aPath;
//      fPath.CheckExistence(@FileExistsCallback,fErrorback);
    end;
  end;
  Free;
end;

constructor TStewProject.TSearchParentDirectories.Create(aProject: TStewProject;
  aPath: TFile; aCallback: TDeferredBooleanCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  inherited Create;
  fProject := aProject;
  fPath := aPath;
  fCallback := aCallback;
  fErrorback := aErrorback;
end;

procedure TStewProject.TSearchParentDirectories.Enqueue;
begin
  TProjectProperties.GetPath(fPath).CheckExistence.After(@FileExistsCallback,@FileExistsFailed);
end;


function TStewProject.GetProjectName: String;
begin
  result := fDisk.PacketName;
end;

class function TStewProject.Open(aPath: TFile): TProjectPromise;
begin
  result := TProjectOpenAtPromise.Enqueue(aPath);
end;

class function TStewProject.OpenInParent(aPath: TFile): TProjectPromise;
begin
  result := TProjectOpenInParentPromise.Enqueue(aPath);
end;

class function TStewProject.CreateNew(aPath: TFile): TProjectPromise;
begin
  result := TProjectCreateAtPromise.Enqueue(aPath);
end;

end.

