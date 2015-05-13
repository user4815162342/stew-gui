unit stewproject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewfile, stewshell, stewasync, stewproperties, stewtypes, contnrs;

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
  // TODO: If I make a more structure type, Free Pascal has extended records
  // which allow me to add procedures to a record, then I can make sure things
  // are controlled as I want them instead of relying on string and TFilename
  // functionality).
  TDocumentID = String;

  TDocumentList = array of TDocumentID;
  TDeferredDocumentListCallback = procedure(Path: TFilename; Data: TDocumentList; aTarget: TObject) of object deprecated;

  TDocumentNotifyEvent = procedure(Sender: TObject; Document: TDocumentID) of object;
  TDocumentExceptionEvent = procedure(Sender: TObject; Document: TDocumentID; Error: String) of object;
  TAttachmentNotifyEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String) of object;
  TAttachmentExceptionEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String; Error: String) of object;
  TAttachmentConfirmationEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String; out Answer: Boolean) of object;
  TAttachmentChoiceEvent = procedure(Sender: TObject; Document: TDocumentID; AttachmentName: String; aChoices: TStringArray; var Answer: String; out Accepted: Boolean) of object;

  TOrderDocumentPosition = (odpBefore = -1,odpAfter = -2);

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
    function GetCandidateFiles: TStringArray; virtual;
    function GetDescriptor: String; virtual; abstract;
    function GetDefaultExtension: String; virtual; abstract;
    function GetDefaultFilename: String; virtual;
    function GetName: String; virtual; abstract;
    procedure FileLoaded(aData: TStream; aFileAge: Longint);
    procedure FileLoadFailed(Data: String);
    procedure FileSaveConflicted({%H-}aFileAge: Longint);
    procedure FileSaved(aFileAge: Longint);
    procedure FileSaveFailed(Data: String);
    function GetContents: String;
    procedure SetContents(AValue: String);
    procedure EditorTemplatesListed(Data: TTemplateArray);
    procedure EditableFileWritten({%H-}aAge: Longint);
    procedure EditableFileReady;
  public
    constructor Create(aDocument: TDocumentMetadata);
    procedure Load;
    procedure Save(aForce: Boolean = false);
    // TODO: Consider this possibility if I ever add RichText.
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
    fIsNew: Boolean;
    fLock: TObject;
    fProject: TStewProject;
    fDisk: TFilename;
    fID: TDocumentID;
    fFiles: TStringList;
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
    procedure FileRenameFailed(Data: String);
    procedure FilesRenamed;
    procedure FilesListed(Data: TFileList; aRecursive: Boolean);
    procedure FilesListedNonrecursive(Data: TFileList);
    procedure FilesListedRecursive(Data: TFileList);
    procedure FilesListError(Data: String);
    procedure StateChanged;
    function DoConfirmNewAttachment(aName: String): Boolean;
    function DoChooseTemplate(aAttachmentName: String;
      const aTemplates: TTemplateArray; out aTemplate: TTemplate): Boolean;
    procedure ClearFiles;
    procedure AddFile(const aFile: TFilename);
    function SortDocuments(List: TStringList; Index1, Index2: Integer): Integer;
    function PutPath(const aPath: String): TDocumentMetadata;
    function PutDocument(aKey: String): TDocumentMetadata;
    function GetDocument(aKey: String): TDocumentMetadata;
    function GetAttachments(const aDescriptor: String; const aExtension: String): TStringArray;
    function GetNewAttachmentName(const aDescriptor: String; const aExtension: String): String;
    function GetSynopsis: TSynopsisMetadata;
    property Project: TStewProject read fProject;
    procedure DirectoryCreated;
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
    property OnDocumentCreated: TDocumentNotifyEvent read FOnDocumentCreated write FOnDocumentCreated;
    property OnDocumentChanged: TDocumentNotifyEvent read FOnDocumentChanged write FOnDocumentChanged;
    property OnDocumentRenameFailed: TDocumentExceptionEvent read FOnDocumentRenameFailed write FOnDocumentRenameFailed;
    property OnConfirmNewAttachment: TAttachmentConfirmationEvent read FOnConfirmNewAttachment write FOnConfirmNewAttachment;
    property OnChooseTemplate: TAttachmentChoiceEvent read FOnChooseTemplate write FOnChooseTemplate;
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

  function ExcludeLeadingSlash(Const Path: TDocumentID): string;
  function IncludeLeadingSlash(Const Path : TDocumentID) : String;
  function IncludeTrailingSlash(Const Path : TDocumentID) : String;
  function ExcludeTrailingSlash(Const Path: TDocumentID): string;
  function IsParentDocument(aParent: TDocumentID; aChild: TDocumentID): Boolean;
  function ExtractDocumentName(const Path: TDocumentID): string;
  function ExtractParentDocument(const Path: TDocumentID): string;

  const
    RootDocument: TDocumentID = '/';
    AlwaysForbiddenNameCharacters: set of char = [#0..#$1F,#$7F,'<','>',':','"','/','\','|','?','*','%','[',']','~','{','}',';'];
    WhitespaceCharacters: set of char = [' ',#$A0];//,#$1680,#$180e,#$2000..#$200A,#$2028,#$2029,#$202F,#$3000]


implementation

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

function ExtractParentDocument(const Path: TDocumentID): string;
var
  i : longint;
begin
  I := Length(Path);
  while (I > 0) and not (Path[I] = '/') do
    Dec(I);
  Result := Copy(Path, 1, I - 1);
  if Result = '' then
    Result := RootDocument;
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

procedure TAttachmentMetadata.SetContents(AValue: String);
begin
  if fContents <> AValue then
  begin;
    fContents := AValue;
    fModified := true;

  end;
end;

const EmptyFileTemplate: TTemplate = ( Name: 'Empty File'; Path: '');

procedure TAttachmentMetadata.EditorTemplatesListed(Data: TTemplateArray);
var
  aTemplate: TTemplate;
begin
  if Length(Data) = 0 then
  // create a simple, basic, blank file.
     WriteFile(GetDefaultFilename,'',@EditableFileWritten,@FileLoadFailed)
  else
  begin
    if Length(Data) > 1 then
    begin
      if (GetDefaultExtension = '') then
      begin
        // no extension is known, so it's possible that they might
        // want to just create a blank template, like above.
        SetLength(Data,Length(Data) + 1);
        Data[Length(Data) - 1] := EmptyFileTemplate;
      end;
      if not fDocument.DoChooseTemplate(GetName,Data,aTemplate) then
        Exit;
    end
    else
      aTemplate := Data[0];
    if aTemplate.Name = EmptyFileTemplate.Name then
      // we're creating a blank file anyway, but in this case,
      // we know that we don't know what extension it is, so set
      // the extension to '.txt'.
       WriteFile(ChangeFileExt(GetDefaultFilename,'.txt'),'',@EditableFileWritten,@FileLoadFailed)
    else
       CreateFileFromTemplate(aTemplate,GetDefaultFilename,@EditableFileReady,@FileLoadFailed);
  end;

end;

procedure TAttachmentMetadata.EditableFileWritten(aAge: Longint);
begin
  EditableFileReady;
end;

procedure TAttachmentMetadata.EditableFileReady;
var
  aName: String;
begin
  aName := GetDefaultFilename;
  fDocument.AddFile(ExtractFileName(aName));
  EditFile(aName);
end;


function TAttachmentMetadata.GetCandidateFiles: TStringArray;
begin
  result := fDocument.GetAttachments(GetDescriptor,'');
end;

function TAttachmentMetadata.GetDefaultFilename: String;
begin
  result := fDocument.GetNewAttachmentName(GetDescriptor,GetDefaultExtension);
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
      // just pretend it's loaded, but with no contents.
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

procedure TAttachmentMetadata.Save(aForce: Boolean);
var
  aCandidates: TStringArray;
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
          aCandidates[0] := GetDefaultFilename;
        end;
        1: // do nothing, we save in a minute.
      else
          // TODO: Should ask user which one to load instead. This requires
          // an event.
          raise Exception.Create('Too many ' + GetName + ' files');
      end;

      fFilingState := fsSaving;
      fDocument.AttachmentSaving(GetName);
      WriteFile(aCandidates[0],false,not aForce,fFileAge,fContents,@FileSaved,@FileSaveConflicted,@FileSaveFailed);
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
    if fDocument.DoConfirmNewAttachment(GetName) then
    begin
      // TODO: This should actually be deferred, shouldn't it?
      GetTemplatesForExt(GetDefaultExtension,@EditorTemplatesListed,@FileLoadFailed);
    end;
    1:
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

procedure TDocumentMetadata.FilesListed(Data: TFileList; aRecursive: Boolean);
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
     raise Exception.Create('Can''t confirm a new ' + aName + ' file for ' + fID);
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
    aTemplate.Name := '';
    aTemplate.Path := '';
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
     raise Exception.Create('Too many possible templates are available for the new ' + aName + ' file for ' + fID);
end;

procedure TDocumentMetadata.ClearFiles;
var
  i: Integer;
  j: Integer;
  aChild: TDocumentMetadata;
begin
  for i := fFiles.Count - 1 downto 0 do
  begin
    if (Length(fFiles[i]) = 0) or (fFiles[i][1] = '_') then
       fFiles.Delete(i);
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

procedure TDocumentMetadata.FileRenameFailed(Data: String);
begin
  if fProject.fOnDocumentRenameFailed <> nil then
     fProject.fOnDocumentRenameFailed(fProject,fID,Data);
  ListDocuments(true);
end;

procedure TDocumentMetadata.FilesRenamed;
begin
  // now, refresh the parent so that this thing appears under the new location.
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
  aDiskPath: TFilename; aID: TDocumentID; aIsRoot: Boolean);
var
  aCached: TProtectedDocumentProperties;
begin
  inherited Create;
  fProject := aProject;
  fDisk := ExcludeTrailingPathDelimiter(aDiskPath);
  fID := aID;
  fIsNew := false;
  fLock := nil;
  fListingState := lsNotListed;
  fFiles := TStringList.Create;
  fFiles.Duplicates := dupIgnore;
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
    result := TDocumentMetadata.Create(fProject,IncludeTrailingPathDelimiter(fDisk) + aKey,IncludeTrailingSlash(fID) + aKey,false);
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
      // I'm only interested in documents that have associated disk files,
      // or ones marked specifically as 'new'.
      // Other documents can be retrieved by specific name, but I don't
      // want to display them as available if they aren't.
      aChild := fContents[i] as TDocumentMetadata;
      if (aChild.fFiles.Count > 0) or (aChild.fIsNew) then
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
  result := fProject.GetDocument(ExtractParentDocument(fID)).ListingState = lsListed;
end;

procedure TDocumentMetadata.Lock(aLockingObject: TObject);
begin
  if fLock <> aLockingObject then
  begin;
    if IsLocked then
      raise Exception.Create('Document ' + fID + ' is already locked');
    fLock := aLockingObject;
    StateChanged;
  end;
end;

procedure TDocumentMetadata.Unlock(aLockingObject: TObject);
begin
  if not IsLocked then
    raise Exception.Create('Document ' + fID + ' is not locked');
  if fLock <> aLockingObject then
    raise Exception.Create('Document ' + fID + ' was locked with a different object');
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
  if fID <> RootDocument then
    result := fProject.GetDocument(ExtractParentDocument(fID))
  else
    result := nil;
end;

function TDocumentMetadata.GetName: String;
begin
  result := ExtractDocumentName(fID);
end;

procedure TDocumentMetadata.Rename(aOldName: String; aNewName: String);
var
  i: Integer;
  aOld: TDocumentMetadata;
  aFile: String;
  source: TStringArray;
  target: TStringArray;
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
    aFile := aOld.fFiles[i];
    source[i] := IncludeTrailingPathDelimiter(fDisk) + aFile;
    // only change the name if it's actually supposed to be associated with it...
    if Pos(aOldName,aFile) = 1 then
       target[i] := IncludeTrailingPathDelimiter(fDisk) + aNewName + Copy(aFile,Length(aOldName) + 1,Length(aFile))
    else
       target[i] := source[i];
  end;

  // finally, remove the old one, it will get relisted later.
  fContents.Remove(aOld);

  RenameFiles(source,target,@FilesRenamed,@FileRenameFailed);
end;

procedure TDocumentMetadata.MoveDocToHere(aOldChild: TDocumentMetadata);
var
  i: Integer;
  aOldParent: TDocumentMetadata;
  fSource: TStringArray;
  fTarget: TStringArray;
begin
  aOldParent := aOldChild.GetParent;
  if (aOldParent.ListingState <> lsListed) or
     (ListingState <> lsListed) then
    raise Exception.Create('Please make sure the documents are listed before attempting to move');
  if HasDocument(aOldChild.GetName) then
     raise Exception.Create('A document named "' + aOldChild.GetName + '" already exists there.');

  aOldChild.Lock(Self);

  SetLength(fSource,aOldChild.fFiles.Count);
  SetLength(fTarget,aOldChild.fFiles.Count);
  for i := 0 to aOldChild.fFiles.Count - 1 do
  begin
    fSource[i] := IncludeTrailingPathDelimiter(aOldParent.fDisk) + aOldChild.fFiles[i];
    fTarget[i] := IncludeTrailingPathDelimiter(fDisk) + aOldChild.fFiles[i];
  end;

  // finally, remove the old one, it will get relisted later.
  aOldParent.fContents.Remove(aOldChild);
  // report a change in state so the listings get refreshed where wanted.
  aOldParent.StateChanged;

  // TODO: Similar to RenameFiles, except this gets the original parent
  // directory and the new parent directory.
  // TODO: I could fix rename files to use the whole path in both cases,
  // instead of just being given the path and a set of targets off of that
  // path, and then I could just use the same function for both.
  RenameFiles(fSource,fTarget,@FilesRenamed,@FileRenameFailed);
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
              Properties.index.Add(ExtractDocumentName(aList[i]));
            Properties.index.Add(aDoc.GetName);
            // don't actually put the relative in if it's 'before',
            // because we'll still retain the same effect without adding
            // a whole bunch of things to the list.
            break;

          end
          else
            Properties.index.Add(ExtractDocumentName(aList[i]));
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
  result := fDisk + aDescriptor + IncludeExtensionDelimiter(aExtension);
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
  if not (fID = RootDocument) then
  begin
    // Add the 'directory' name to the files.
    AddFile(ExtractFileName(fDisk));
    aParent := fProject.GetDocument(ExtractParentDocument(fID));
    if aParent.IsNew and not (aParent.fID = RootDocument) then
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

