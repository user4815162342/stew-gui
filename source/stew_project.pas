unit stew_project;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, sys_file, sys_async, stew_properties, sys_types, contnrs, sys_filecache;

{
FUTURE: Something I might add later, if I feel it's necessary. Loading should have a refresh mode, I think I've put this elsewhere:
- retrieve-from-cache-only: if the file is not cached, load it, otherwise just
  return the file data.
- reload-if-modified: if the file on disk has been modified, load it, otherwise
  just return the file data.
- reload-if-stale: if the timestamp of the data is old (not sure how old this
  would be, is it a constant? a parameter?) then reload. This might be better
  done by clearing out old data instead.
- reload-always: reload the file completely from disk, no matter what it's cache
  status.

}

type
  { TDocumentPath }

  TDocumentPath = record
  strict private
    fID: UTF8String;
    function GetContainer: TDocumentPath;
    function GetIsNull: Boolean;
    function GetIsSystem: Boolean;
    function GetName: UTF8String;
  public
    property IsNull: Boolean read GetIsNull;
    property IsSystem: Boolean read GetIsSystem;
    property ID: String read fID;
    property Name: UTF8String read GetName;
    property Container: TDocumentPath read GetContainer;
    function GetContainedDocument(aName: UTF8String): TDocumentPath;
    function Contains(aChild: TDocumentPath): Boolean;
    function Split: TStringArray;
    function ToFile(aBase: TFile; aDescriptor: UTF8String; aExtension: UTF8String): TFile;
    class function FromString(const aPath: UTF8String): TDocumentPath; static;
    const Root: TDocumentPath = ( fID: '/');
    const Null: TDocumentPath = ( fID: '');
    class function GetSystemDocument(aName: UTF8String): TDocumentPath; static;
    class operator = (a: TDocumentPath; b: TDocumentPath): Boolean;
    class function FromFile(aBase: TFile; aFile: TFile): TDocumentPath; static;
  end;

  TDocumentArray = array of TDocumentPath;

  { TDocumentInfo }

  TDocumentInfo = record
    Document: TDocumentPath;
    IsFolder: Boolean;
    IsShadow: Boolean;
    class function Make(aDocument: TDocumentPath; aIsFolder: Boolean; aIsShadow: Boolean): TDocumentInfo; static;
  end;

  TDocumentInfoArray = array of TDocumentInfo;

  TAttachmentKind = (atUnknown, atFolder, atPrimary, atProperties, atNotes, atThumbnail, atSynopsis, atBackup);

  { TAttachment }

  TAttachment = record
    Kind: TAttachmentKind;
    Descriptor: UTF8String;
    Extension: UTF8String;
    class function MakeUnknown(aDescriptor: UTF8String; aExtension: UTF8String): TAttachment; static;
    class function MakeDirectory: TAttachment; static;
    class function MakePrimary(aExtension: UTF8String): TAttachment; static;
    class function MakeProperties: TAttachment; static;
    class function MakeNotes(aExtension: UTF8String): TAttachment; static;
    class function MakeThumbnail(aExtension: UTF8String): TAttachment; static;
    class function MakeSynopsis: TAttachment; static;
    class function MakeBackup(aDescriptor: UTF8String; aExtension: UTF8String): TAttachment; static;
    class function Make(aKind: TAttachmentKind; aDescriptor: UTF8String; aExtension: UTF8String): TAttachment; static;
    class function FromFile(aBase: TFile; aFile: TFile): TAttachment; static;
    const
      AttachmentKindStrings: array[TAttachmentKind] of UTF8String =
    ('unknown file',
     'folder',
     'primary',
     'property file',
     'note',
     'thumbnail',
     'synopsis',
     'backup'
    );
  end;

  TAttachmentArray = array of TAttachment;


  TProjectEventKind = (
  // The "File" events indicate actual file-based activity. These are necessary
  // to:
  //  - enable/disable UI elements, since certain file activities can't happen
  //    at the same time for the same file. For example, the same file can't
  //    be saved while loading, and vice versa. The same file can't be saved
  //    a separate time while it's saving either.
  //  - alert to more low-level errors in the file system. The "Failed" file
  //    events will only occurr if there is an error while the cache is going
  //    against the file system. The non-file versions of these events will
  //    include errors in parsing and the like. The non-file versions should be
  //    used for reporting errors in the UI.
     paLoadingProjectPropertiesFile,
     paProjectPropertiesFileLoaded,
     paProjectPropertiesFileLoadingFailed,
     paSavingProjectPropertiesFile,
     paProjectPropertiesFileSaved,
     paProjectPropertiesFileSavingFailed,
     paListingDocumentFiles,
     paDocumentFilesListed,
     paDocumentsFileListingFailed,
     paCheckingDocumentFile,
     paDocumentFileChecked,
     paDocumentFileCheckingFailed,
     paLoadingAttachmentFile,
     paAttachmentFileLoaded,
     paAttachmentFileLoadingFailed,
     paSavingAttachmentFile,
     paAttachmentFileSaved,
     paAttachmentFileSavingFailed,
  // There is no corresponding RenamingFile or FileRenamed, because these can't
  // easily be mapped to documents, and the actual document renaming is probably
  // much more useful. But, we still want the low-level error message sometimes.
     paDocumentFileRenameFailed,
  // The non-file events indicate activity at the project context itself. These
  // *always* happen when a method is called on the project, whereas the file
  // events only occur if the cache actually has to go against the file system.
  // Also, the DataReceived event provides the actual data, whereas the FileLoaded
  // event only contains a notification that the loading is complete.
  // These should be used to indicate progress.
  // Note that there are generally no 'begging' actions here (i.e. AttachmentSaving)
  // because these things are not very useful or informative.
     paProjectPropertiesDataReceived,
     paProjectPropertiesLoadingFailed,
     paProjectPropertiesSavingFailed,
     paProjectPropertiesSaveConflictOccurred,
     paDocumentListDataReceived,
     paDocumentsListingFailed,
     paAttachmentListDataReceived,
     paAttachmentListingFailed,
     paDocumentFolderCheckFailed,
     paShadowCreated,
     paShadowUncreated,
     paDocumentShifted,
     paDocumentShiftFailed,
     paRenamingDocument,
     paDocumentRenamed,
     paDocumentRenamingFailed,
     paAttachmentDataReceived,
     paAttachmentLoadingFailed,
     paAttachmentSavingFailed,
     paAttachmentSaveConflictOccurred,
     paEditingAttachment,
     paAttachmentEditingFailed,
     paUnexpectedProjectError,
     paUnexpectedDocumentError,
     paUnexpectedAttachmentError);

  { TProjectEvent }

  TProjectEvent = class(TObject)
  strict private
    fAction: TProjectEventKind;
    fIsError: Boolean;
  public
    constructor Create(aAction: TProjectEventKind);
    property Action: TProjectEventKind read fAction;
    property IsError: Boolean read fIsError;
    function GetDescription: UTF8String; virtual;
    const
      EventKindStrings: array[TProjectEventKind] of UTF8String =
    ('LoadingProjectPropertiesFile',
     'ProjectPropertiesFileLoaded',
     'ProjectPropertiesFileLoadingFailed',
     'SavingProjectPropertiesFile',
     'ProjectPropertiesFileSaved',
     'ProjectPropertiesFileSavingFailed',
     'ListingDocumentFiles',
     'DocumentFilesListed',
     'DocumentsFileListingFailed',
     'CheckingDocumentFile',
     'DocumentFileChecked',
     'DocumentFileCheckingFailed',
     'LoadingAttachmentFile',
     'AttachmentFileLoaded',
     'AttachmentFileLoadingFailed',
     'SavingAttachmentFile',
     'AttachmentFileSaved',
     'AttachmentFileSavingFailed',
     'DocumentFileRenameFailed',
     'ProjectPropertiesDataReceived',
     'ProjectPropertiesLoadingFailed',
     'ProjectPropertiesSavingFailed',
     'ProjectPropertiesSaveConflictOccurred',
     'DocumentListDataReceived',
     'DocumentsListingFailed',
     'DocumentAttachmentsListed',
     'DocumentAttachmentsListingFailed',
     'DocumentFolderCheckFailed',
     'ShadowCreated',
     'ShadowUncreated',
     'DocumentShifted',
     'DocumentShiftFailed',
     'RenamingDocument',
     'DocumentRenamed',
     'DocumentRenamingFailed',
     'AttachmentDataReceived',
     'AttachmentLoadingFailed',
     'AttachmentSavingFailed',
     'AttachmentSaveConflictOccurred',
     'EditingAttachment',
     'AttachmentEditingFailed',
     'UnexpectedProjectError',
     'UnexpectedDocumentError',
     'UnexpectedAttachmentError');

  end;



  TStewProject = class;

  TProjectObserver = procedure(Sender: TStewProject; Event: TProjectEvent) of object;
  TProjectObserverList = specialize GMethodList<TProjectObserver>;

  { TProjectPropertiesDataReceivedEvent }

  TProjectPropertiesDataReceivedEvent = class(TProjectEvent)
  strict private
    fProperties: TProjectProperties;
  public
    constructor Create(aProperties: TProjectProperties);
    property Properties: TProjectProperties read fProperties;
  end;

  { TProjectError }

  TProjectError = class(TProjectEvent)
  strict private
    fError: TPromiseError;
  public
    constructor Create(aAction: TProjectEventKind; aError: TPromiseError);
    function GetDescription: UTF8String; override;
    property Error: TPromiseError read fError;
  end;

  { TDocumentEvent }

  TDocumentEvent = class(TProjectEvent)
  strict private
    fDocument: TDocumentPath;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentListedEvent }

  { TDocumentListDataReceivedEvent }

  TDocumentListDataReceivedEvent = class(TDocumentEvent)
  strict private
    fList: TDocumentInfoArray;
  public
    constructor Create(aDocument: TDocumentPath; aList: TDocumentInfoArray);
    property List: TDocumentInfoArray read FList;
    function GetDescription: UTF8String; override;
  end;

  { TAttachmentListDataReceivedEvent }

  TAttachmentListDataReceivedEvent = class(TDocumentEvent)
  strict private
    fList: TAttachmentArray;
  public
    constructor Create(aDocument: TDocumentPath; aList: TAttachmentArray);
    property List: TAttachmentArray read fList;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentError }

  TDocumentError = class(TDocumentEvent)
  strict private
    fError: TPromiseError;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aError: TPromiseError);
    property Error: TPromiseError read fError;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentRenameEvent }

  TDocumentRenameEvent = class(TDocumentEvent)
  strict private
    fNewDocument: TDocumentPath;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
      aNewDocument: TDocumentPath);
    property NewDocument: TDocumentPath read fNewDocument;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentRenameErrorEvent }

  TDocumentRenameErrorEvent = class(TDocumentRenameEvent)
  strict private
    fError: TPromiseError;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aNewDocument: TDocumentPath; aError: TPromiseError);
    property Error: TPromiseError read fError;
    function GetDescription: UTF8String; override;
  end;


  { TAttachmentEvent }

  TAttachmentEvent = class(TDocumentEvent)
  strict private
    fAttachment: TAttachment;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aAttachment: TAttachment);
    property Attachment: TAttachmentKind read fAttachment.Kind;
    property Extension: UTF8String read fAttachment.Extension;
    property Descriptor: UTF8String read fAttachment.Descriptor;
    function AttachmentName: UTF8String;
    function GetDescription: UTF8String; override;
  end;

  { TAttachmentEditingEvent }

  TAttachmentEditingEvent = class(TAttachmentEvent)
  strict private
    fCancelled: Boolean;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aAttachment: TAttachment; aCancelled: Boolean);
    property Cancelled: Boolean read fCancelled;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentPropertiesDataReceivedEvent }

  TDocumentPropertiesDataReceivedEvent = class(TAttachmentEvent)
  strict private
    fProperties: TDocumentProperties;
  public
    constructor Create(aDocument: TDocumentPath; aProperties: TDocumentProperties);
    property Properties: TDocumentProperties read fProperties;
  end;

  { TDocumentSynopsisDataReceived }

  TDocumentSynopsisDataReceived = class(TAttachmentEvent)
  strict private
    fSynopsis: UTF8String;
  public
    constructor Create(aDocument: TDocumentPath; aSynopsis: UTF8String);
    property Synopsis: UTF8String read fSynopsis;
  end;

  { TAttachmentError }

  TAttachmentError = class(TAttachmentEvent)
  strict private
    fError: TPromiseError;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aAttachment: TAttachment; aError: TPromiseError);
    property Error: TPromiseError read fError;
    function GetDescription: UTF8String; override;
  end;

  TAttachmentConfirmationEvent = procedure(Sender: TObject; Document: TDocumentPath; AttachmentName: String; out Answer: Boolean) of object;
  TAttachmentChoiceEvent = procedure(Sender: TObject; Document: TDocumentPath; AttachmentName: String; aChoices: TStringArray; var Answer: Integer; out Accepted: Boolean) of object;



  { TShadowDocument }

  TShadowDocument = class
  strict protected
    fContents: TFPHashObjectList;
    fIsShadow: Boolean;
    function CreatePath(aPath: UTF8String): TShadowDocument;
    function GetIsFolder: Boolean;
    function GetPath(aPath: UTF8String): TShadowDocument;
    procedure DeletePath(aPath: UTF8String);
    procedure SplitPath(aPath: UTF8String; out aChildName: UTF8String; out aChildPath: UTF8String);
  public
    constructor Create(aIsShadow: Boolean);
    destructor Destroy; override;
    property IsFolder: Boolean read GetIsFolder;
    property IsShadow: Boolean read fIsShadow;
  end;

  { TShadowCache }

  TShadowCache = class(TShadowDocument)
  public
    function CreateShadow(aDocument: TDocumentPath): TShadowDocument;
    procedure DeleteShadow(aDocument: TDocumentPath);
    function HasShadow(aDocument: TDocumentPath): Boolean;
    function IsFolder(aDocument: TDocumentPath): Boolean;
    function GetContents(aDocument: TDocumentPath): TDocumentInfoArray;
  end;

  { TProjectPromise }

  TProjectPromise = class(TPromise)
  strict private
    fPath: TFile;
    fProject: TStewProject;
  public
    constructor Create(aPath: TFile);
    procedure SetAnswer(aProject: TStewProject);
    property Project: TStewProject read fProject;
    property Path: TFile read FPath;
  end;

  { TProjectPropertiesPromise }

  TProjectPropertiesPromise = class(TPromise)
  strict private
    fProperties: TProjectProperties;
  private
    procedure LoadAnswer(aStream: TStream);
  public
    destructor Destroy; override;
    property Properties: TProjectProperties read fProperties;
  end;

  TWriteProjectPropertiesPromise = class(TPromise)
  private
    fIsConflict: Boolean;
  public
    property IsConflict: Boolean read fIsConflict;
  end;

  { TWriteAttachmentPromise }

  TWriteAttachmentPromise = class(TPromise)
  strict private
    fDocument: TDocumentPath;
  private
    fIsConflict: Boolean;
    fAttachment: TAttachment;
  public
    constructor Create(aDocument: TDocumentPath; aAttachment: TAttachment);
    property IsConflict: Boolean read fIsConflict;
    property Document: TDocumentPath read fDocument;
    property Kind: TAttachmentKind read fAttachment.Kind;
    property Descriptor: UTF8String read fAttachment.Descriptor;
    property Extension: UTF8String read fAttachment.Extension;
  end;

  { TDocumentPropertiesPromise }

  TDocumentPropertiesPromise = class(TPromise)
  strict private
    fProperties: TDocumentProperties;
    fDocument: TDocumentPath;
  private
    procedure LoadAnswer(aStream: TStream);
  public
    constructor Create(aDocument: TDocumentPath);
    destructor Destroy; override;
    property Properties: TDocumentProperties read fProperties;
    property Document: TDocumentPath read fDocument;
  end;

  { TDocumentSynopsisPromise }

  TDocumentSynopsisPromise = class(TPromise)
  strict private
    fSynopsis: UTF8String;
    fDocument: TDocumentPath;
  private
    procedure LoadAnswer(aText: UTF8String);
  public
    constructor Create(aDocument: TDocumentPath);
    property Synopsis: UTF8String read fSynopsis;
    property Document: TDocumentPath read fDocument;
  end;

  { TDocumentListPromise }

  TDocumentListPromise = class(TPromise)
  strict private
    fDocument: TDocumentPath;
  private
    fList: TDocumentInfoArray;
  public
    constructor Create(aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
    property List: TDocumentInfoArray read fList;
  end;

  { TAttachmentListPromise }

  TAttachmentListPromise = class(TPromise)
  strict private
    fDocument: TDocumentPath;
  private
    fList: TAttachmentArray;
  public
    constructor Create(aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
    property List: TAttachmentArray read fList;

  end;

  { TDocumentIsFolderPromise }

  TDocumentIsFolderPromise = class(TPromise)
  strict private
    fDocument: TDocumentPath;
  private
    fIsFolder: Boolean;
  public
    constructor Create(aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
    property IsFolder: Boolean read fIsFolder;
  end;

  TShiftDocumentPromise = class(TPromise)
  strict private
    fDocument: TDocumentPath;
  public
    constructor Create(aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
  end;

  { TRenameDocumentPromise }

  TRenameDocumentPromise = class(TPromise)
  strict private
    fOldDocument: TDocumentPath;
    fNewDocument: TDocumentPath;
  public
    constructor Create(aOldDocument: TDocumentPath; aNewDocument: TDocumentPath);
    property Old: TDocumentPath read fOldDocument;
    property New: TDocumentPath read fNewDocument;
  end;

  { TRequestEditingAttachmentPromise }

  TRequestEditingAttachmentPromise = class(TPromise)
  strict private
    fDocument: TDocumentPath;
  private
    fAttachment: TAttachment;
    fCancelled: Boolean;
  public
    constructor Create(aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
    property Attachment: TAttachment read fAttachment;
    property Cancelled: Boolean read fCancelled;
  end;

  { TStewProject }

  TStewProject = class
  strict private type

    { TProjectCheckAndCreateTask }

    TProjectCheckAndCreateTask = class(TQueuedTask)
    strict protected
      fPath: TFile;
      procedure ResolveCreateProject(aPath: TFile);
      function CreatePromise: TPromise; override;
      property Path: TFile read fPath;
    public
      constructor Enqueue(aPath: Tfile);
    end;

    { TProjectCheckAtPathTask }

    TProjectCheckAtPathTask = class(TProjectCheckAndCreateTask)
    strict private
      procedure FileExists(Sender: TPromise);
    strict protected
      procedure DoTask; override;
    end;

    { TProjectCheckInParentTask }

    TProjectCheckInParentTask = class(TProjectCheckAndCreateTask)
    strict private
      procedure FileExistsInParent(Sender: TPromise);
      procedure FileExists(Sender: TPromise);
    strict protected
      procedure DoTask; override;
    end;

    { TWriteProjectPropertiesTask }

    TWriteProjectPropertiesTask = class(TDeferredTask)
    strict protected
      procedure HandleError(Input: TPromise; Error: TPromiseError); override;
      procedure DoTask({%H-}Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aPromise: TFileWritePromise);
    end;

    { TReadProjectPropertiesTask }

    TReadProjectPropertiesTask = class(TDeferredTask)
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aPromise: TFileReadPromise);
    end;

    { TWriteAttachmentTask }

    TWriteAttachmentTask = class(TDeferredTask)
    strict private
      fDocument: TDocumentPath;
      fAttachment: TAttachment;
    strict protected
      procedure HandleError(Input: TPromise; Error: TPromiseError); override;
      procedure DoTask({%H-}Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aDocument: TDocumentPath; aAttachment: TAttachment; aPromise: TFileWritePromise);
      property Document: TDocumentPath read fDocument;
      property Kind: TAttachmentKind read fAttachment.Kind;
      property Descriptor: UTF8String read fAttachment.Descriptor;
      property Extension: UTF8String read fAttachment.Extension;
    end;

    { TReadDocumentPropertiesTask }

    TReadDocumentPropertiesTask = class(TDeferredTask)
    strict private
      fDocument: TDocumentPath;
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aDocument: TDocumentPath; aPromise: TFileReadPromise);
    end;

    { TReadDocumentSynopsisTask }

    TReadDocumentSynopsisTask = class(TDeferredTask)
    strict private
      fDocument: TDocumentPath;
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aDocument: TDocumentPath; aPromise: TFileReadPromise);
    end;

    { TListDocumentsInFolderTask }

    TListDocumentsInFolderTask = class(TDeferredTask)
    strict private
      fProject: TStewProject;
      fDocument: TDocumentPath;
      fFiles: TFileInfoArray;
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
      procedure PropertiesReceived(Sender: TPromise);
    public
      constructor Defer(aDocument: TDocumentPath; aProject: TStewProject; aPromise: TFileListPromise);
    end;

    { TListAttachmentsForDocumentTask }

    TListAttachmentsForDocumentTask = class(TDeferredTask)
    strict private
      fProject: TStewProject;
      fDocument: TDocumentPath;
      fFiles: TFileInfoArray;
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aDocument: TDocumentPath; aProject: TStewProject; aPromise: TFileListPromise);
    end;

    { TDocumentListBuilder }

    TDocumentListBuilder = class(TObject)
    strict private
      fIndex: TStringList;
      function SortDocuments(List: TStringList; Index1, Index2: Integer): Integer;
    public
      constructor Create;
      function Build(aProperties: TDocumentProperties;
        aBasePath: TFile; aFiles: TFileInfoArray; aShadows: TDocumentInfoArray
  ): TDocumentInfoArray;
      destructor Destroy; override;
    end;

    { TIsDocumentAFolderTask }

    TIsDocumentAFolderTask = class(TDeferredTask)
    strict private
      fDocument: TDocumentPath;
      fProject: TStewProject;
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aDocument: TDocumentPath; aProject: TStewProject; aPromise: TFileExistencePromise);
    end;

    { TShiftDocumentTask }

    TShiftDocumentTask = class(TDeferredTask)
    strict private
      fProject: TStewProject;
      fDocument: TDocumentPath;
      fFiles: TFileInfoArray;
      fShiftDeltaOrIndex: Integer;
      fRelativeTo: TDocumentPath;
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
      procedure PropertiesReceived(Sender: TPromise);
      procedure PropertiesWritten(Sender: TPromise);
    public
      constructor Defer(aDocument: TDocumentPath; aNewIndexOrDelta: Integer; aRelativeTo: TDocumentPath; aProject: TStewProject; aPromise: TFileListPromise);
    end;

    { TRenameDocumentTask }

    TRenameDocumentTask = class(TDeferredTask)
    strict private
      fProject: TStewProject;
      fOldDocument: TDocumentPath;
      fNewDocument: TDocumentPath;
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
      procedure FilesRenamed(Sender: TPromise);
    public
      constructor Defer(aOld: TDocumentPath; aNew: TDocumentPath; aProject: TStewProject; aInputPromise: TFileListPromise);
    end;

    { TRequestEditAttachmentTask }

    TRequestEditAttachmentTask = class(TDeferredTask)
      procedure NewAttachment_ParentListed(Sender: TPromise);
    strict private
      fProject: TStewProject;
      fDocument: TDocumentPath;
      fAttachment: TAttachmentKind;
      fCandidates: TFileArray;
      fNewFile: TFile;
    strict protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
      procedure GetDefaultDescriptorAndExtension(aProps: TProjectProperties; out aDescriptor: UTF8String; out aExtension: UTF8String);
      procedure NewAttachment_PropertiesRead(Sender: TPromise);
      procedure NewAttachment_TemplatesListed(Sender: TPromise);
      procedure NewAttachment_FileCreated(Sender: TPromise);
      procedure TooManyAttachment_PropertiesRead(Sender: TPromise);
    public
      constructor Defer(aDocument: TDocumentPath; aAttachment: TAttachmentKind; aProject: TStewProject; aInputPromise: TFileListPromise);
    end;

  strict private
    fDisk: TFile;
    fCache: TFileSystemCache;
    fShadows: TShadowCache;
    fObservers: TProjectObserverList;
    FOnChooseTemplate: TAttachmentChoiceEvent;
    FOnConfirmNewAttachment: TAttachmentConfirmationEvent;
    fOnChooseAttachment: TAttachmentChoiceEvent;

    procedure ReportEvent(aEvent: TProjectEvent);
    procedure CacheActivity(Sender: TPromise);
    procedure CacheError(Sender: TPromise; aError: TPromiseError);
    procedure ProjectPropertiesDataReceived(Sender: TPromise);
    procedure ProjectPropertiesReadError(Sender: TPromise; aError: TPromiseError);
    procedure ProjectPropertiesWriteError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentListError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentListReceived(Sender: TPromise);
    procedure AttachmentListReceived(Sender: TPromise);
    procedure AttachmentListError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentPropertiesDataReceived(Sender: TPromise);
    procedure DocumentPropertiesReadError(Sender: TPromise;
      aError: TPromiseError);
    procedure AttachmentWriteError(Sender: TPromise;
      aError: TPromiseError);
    procedure DocumentSynopsisDataReceived(Sender: TPromise);
    procedure DocumentSynopsisReadError(Sender: TPromise; aError: TPromiseError
      );
    procedure DocumentIsFolderError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentRenamingError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentShifted(Sender: TPromise);
    procedure DocumentShiftError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentsRenamed(Sender: TPromise);
    procedure DocumentEditing(Sender: TPromise);
    procedure DocumentEditingError(Sender: TPromise; aError: TPromiseError);
  public

    property OnConfirmNewAttachment: TAttachmentConfirmationEvent read FOnConfirmNewAttachment write FOnConfirmNewAttachment;
    property OnChooseTemplate: TAttachmentChoiceEvent read FOnChooseTemplate write FOnChooseTemplate;
    property OnChooseAttachment: TAttachmentChoiceEvent read fOnChooseAttachment write fOnChooseAttachment;
  public
    // NOTE: You *could* use this to create a project, or you could use one
    // of the Check* class functions, which will verify whether the project
    // exists before creating. Use this after you find that the project doesn't
    // exist, if you still want to create the project at that location anyway.
    constructor Create(const Path: TFile);
    destructor Destroy; override;
    property DiskPath: TFile read fDisk;
    function GetProjectName: String;

    // A "Shadow" is a document that doesn't have any files to back it up.
    // This can be used to create new documents before they are saved,
    // and have them show up in the listings, or to create magic documents
    // which might have a special role and need to be created with every
    // project, such as "Trash". The shadow remains even after the files
    // might be actually created on disk, so it is up to the user to
    // delete the shadow once files have been created, such as when
    // saving properties.
    procedure CreateShadow(aDocument: TDocumentPath);
    // Returns true if the document is in the shadow list.
    // NOTE: Sometimes, a document may return true here, but show up as false
    // in the TDocumentInfo.IsShadow received from ListDocuments. In that case,
    // the document may still be shadowed, but because it has actual files backing
    // it up, it shouldn't be treated as a shadow (and it can be safely deleted
    // using DeleteShadow).
    function IsShadow(aDocument: TDocumentPath): Boolean;
    // Removes the "shadow" from the list of documents.
    procedure DeleteShadow(aDocument: TDocumentPath);

    function IsDocumentAFolder(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TDocumentIsFolderPromise;
    function ListDocumentsInFolder(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TDocumentListPromise;
    function ListAttachmentsForDocument(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TAttachmentListPromise;
    function ReadDocumentSynopsis(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TDocumentSynopsisPromise;
    function WriteDocumentSynopsis(aDocument: TDocumentPath; aSynopsis: UTF8String; aBackup: Boolean = false): TWriteAttachmentPromise;
    function ReadDocumentProperties(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TDocumentPropertiesPromise;
    function WriteDocumentProperties(aDocument: TDocumentPath; aProperties: TDocumentProperties; aBackup: Boolean = false): TWriteAttachmentPromise;
    function ReadProjectProperties(aForceRefresh: Boolean = false): TProjectPropertiesPromise;
    function WriteProjectProperties(aProperties: TProjectProperties; aBackup: Boolean = false): TWriteProjectPropertiesPromise;
    function ShiftDocumentBy(aDocument: TDocumentPath; aDelta: Integer): TShiftDocumentPromise;
    function ShiftDocumentUp(aDocument: TDocumentPath): TShiftDocumentPromise;
    function ShiftDocumentDown(aDocument: TDocumentPath): TShiftDocumentPromise;
    function ShiftDocumentTo(aDocument: TDocumentPath; aIndex: Integer): TShiftDocumentPromise;
    function ShiftDocumentRelativeTo(aDocument: TDocumentPath; aRelative: TDocumentPath; aDelta: Integer): TShiftDocumentPromise;
    function RenameDocument(aDocument: TDocumentPath; aNewDocument: TDocumentPath): TRenameDocumentPromise;
    function EditDocument(aDocument: TDocumentPath): TRequestEditingAttachmentPromise;
    function EditDocumentNotes(aDocument: TDocumentPath): TRequestEditingAttachmentPromise;
    function EditDocumentThumbnail(aDocument: TDocumentPath): TRequestEditingAttachmentPromise;
    function EditDocumentAttachment(aDocument: TDocumentPath; aAttachment: TAttachmentKind): TRequestEditingAttachmentPromise;

    // Important maintenance functions for working with the projects themselves.
    procedure AddObserver(aObserver: TProjectObserver);
    procedure RemoveObserver(aObserver: TProjectObserver);
    class function CheckExistenceAndCreate(aPath: TFile): TProjectPromise;
    class function CheckExistenceInParentAndCreate(aPath: TFile): TProjectPromise;

    // utility functions used internally, but here for your pleasure since they
    // don't change anything in the project state.
    function BuildAndSortDocumentList(aParent: TDocumentPath;
      aProperties: TDocumentProperties; aFiles: TFileInfoArray): TDocumentInfoArray;
    function DoConfirmNewAttachment(aDocument: TDocumentPath; aKind: TAttachmentKind): Boolean;
    function DoChooseTemplate(aDocument: TDocumentPath; aKind: TAttachmentKind; aList: TTemplateArray; out aChosen: TTemplate): Boolean;
    function DoChooseFileForAttachment(aDocument: TDocumentPath; aKind: TAttachmentKind; aChoices: TFileArray; out aChosen: TFile): Boolean;
    class function GetProjectPropertiesPath(aFile: TFile): TFile;
    class function GetDocumentPropertiesPath(aProjectPath: TFile; aDocument: TDocumentPath): TFile;
    class function GetDocumentSynopsisPath(aProjectPath: TFile; aDocument: TDocumentPath): TFile;
    class function GetDocumentFolderPath(aProjectPath: TFile; aDocument: TDocumentPath): TFile;
    const
      ProjectPropertiesDescriptor = 'stew';
      DocumentPropertiesDescriptor = 'properties';
      PropertiesExtension = 'json';
      DefaultDocumentPrimaryExtension = 'txt';
      DocumentSynopsisDescriptor = 'synopsis';
      DocumentSynopsisExtension = 'txt';
      DocumentNotesDescriptor = 'notes';
      DefaultDocumentNotesExtension = 'txt';
      DocumentThumbnailDescriptor = 'thumbnail';
      DefaultDocumentThumbnailExtension = 'png';
      DocumentBackupDescriptorPrefix = 'backup-';
  end;


  function ExcludeLeadingSlash(Const Path: UTF8String): UTF8String;
  function IncludeLeadingSlash(Const Path : UTF8String) : UTF8String;
  function IncludeTrailingSlash(Const Path : UTF8String) : UTF8String;
  function ExcludeTrailingSlash(Const Path: UTF8String): UTF8String;

  function IsNameTroublesome(const aName: UTF8String): Boolean;

  const
    AlwaysForbiddenNameCharacters: set of char = [#0..#$1F,#$7F,'<','>',':','"','/','\','|','?','*','%','[',']','~','{','}',';'];
    WhitespaceCharacters: set of char = [' ',#$A0];//,#$1680,#$180e,#$2000..#$200A,#$2028,#$2029,#$202F,#$3000]


implementation

uses
  sys_json, sys_log;

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

function IsNameTroublesome(const aName: UTF8String): Boolean;
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


function ExcludeLeadingSlash(const Path: UTF8String): UTF8String;
Var
  L : Integer;
begin
  Result:=Path;
  L:=Length(Result);
  If (L>0) and (Result[1] = '/') then
    Delete(Result,1,1);
end;

{ TAttachmentListPromise }

constructor TAttachmentListPromise.Create(aDocument: TDocumentPath);
begin
  inherited Create;
  fDocument := aDocument;
end;

{ TAttachmentListDataReceivedEvent }

constructor TAttachmentListDataReceivedEvent.Create(aDocument: TDocumentPath;
  aList: TAttachmentArray);
begin
  inherited Create(paAttachmentListDataReceived,aDocument);
  fList := aList;
end;

function TAttachmentListDataReceivedEvent.GetDescription: UTF8String;
var
  i: Integer;
begin
  Result:=inherited GetDescription;
  result := result + ' [';
  for i := 0 to Length(fList) - 1 do
  begin
    if i > 0 then
      result := result + ',';
    result := result + ' ' + TAttachment.AttachmentKindStrings[fList[i].Kind] +
                       ' "_' + fList[i].Descriptor +
                       '.' + fList[i].Extension + '"';
  end;
  result := result + ']';
end;

{ TStewProject.TListAttachmentsForDocumentTask }

procedure TStewProject.TListAttachmentsForDocumentTask.DoTask(Input: TPromise);
var
  lKind: TAttachmentKind;
  lFiles: TFileInfoArray;
  lAttachments: TAttachmentArray;
  l: Longint;
  i: Longint;
begin
  try
    lFiles := (Input as TFileListPromise).FilesInfo;
    l := Length(lFiles);
    SetLength(lAttachments,l);
    for i := 0 to l - 1 do
    begin
      if fDocument = TDocumentPath.FromFile(fProject.fDisk,lFiles[i].Item) then
      begin
        lKind := atUnknown;
        case lFiles[i].Item.Descriptor of
          TStewProject.DocumentPropertiesDescriptor:
             if lFiles[i].Item.Extension = TStewProject.PropertiesExtension then
                lKind := atProperties;
          TStewProject.DocumentSynopsisDescriptor:
             if lFiles[i].Item.Extension = TStewProject.DocumentSynopsisExtension then
                lKind := atSynopsis;
          TStewProject.DocumentNotesDescriptor:
             lKind := atNotes;
          TStewProject.DocumentThumbnailDescriptor:
             lKind := atThumbnail;
          '':
          begin
             if lFiles[i].IsFolder and (lFiles[i].Item.Extension = '') then
                lKind := atFolder
             else
                lKind := atPrimary;
          end
        else
          if Pos(TStewProject.DocumentBackupDescriptorPrefix,lFiles[i].Item.Descriptor) = 1 then
             lKind := atBackup;
        end;
        lAttachments[i] := TAttachment.Make(lKind,
                                           lFiles[i].Item.Descriptor,
                                           lFiles[i].Item.Extension);

      end;
    end;
    (Promise as TAttachmentListPromise).fList := lAttachments;
    Resolve;

  except
    on E: Exception do
    begin
      LogException('TStewProject.TListAttachmentsForDocumentTask.DoTask',E);
      Reject(E.Message);
    end;
  end;
end;

function TStewProject.TListAttachmentsForDocumentTask.CreatePromise: TPromise;
begin
  Result := TAttachmentListPromise.Create(fDocument);

end;

constructor TStewProject.TListAttachmentsForDocumentTask.Defer(
  aDocument: TDocumentPath; aProject: TStewProject; aPromise: TFileListPromise);
begin
  fDocument := aDocument;
  fProject := aProject;
  inherited Defer(aPromise);
end;

{ TDocumentInfo }

class function TDocumentInfo.Make(aDocument: TDocumentPath; aIsFolder: Boolean;
  aIsShadow: Boolean): TDocumentInfo;
begin
  result.Document := aDocument;
  result.IsFolder := aIsFolder;
  result.IsShadow := aIsShadow;
end;

{ TAttachmentEditingEvent }

constructor TAttachmentEditingEvent.Create(aAction: TProjectEventKind;
  aDocument: TDocumentPath; aAttachment: TAttachment; aCancelled: Boolean);
begin
  inherited Create(aAction,aDocument,aAttachment);
  fCancelled := aCancelled;
end;

function TAttachmentEditingEvent.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription;
  if fCancelled then
    result := result + ' <CANCELLED>';
end;

class operator TDocumentPath.=(a: TDocumentPath; b: TDocumentPath): Boolean;
begin
  result := CompareText(a.ID,b.ID) = 0;
end;

class function TDocumentPath.FromFile(aBase: TFile; aFile: TFile
  ): TDocumentPath;
var
  lSplit: TStringArray;
  i: Integer;
  lPacket: UTF8String;
begin
  if aBase.Contains(aFile) then
  begin
    // We want to be able to handle documents that are contained within
    // attachments, for future compatbility, so we are not looking at just
    // the packetname for the rest of the path, but we are for the rest,
    // so first we 'split' the directory after the base path.

    lSplit := aFile.Directory.SplitPath(aBase);
    result := TDocumentPath.Root;
    for i := 0 to length(lSplit) - 1 do
    begin
      result := Result.GetContainedDocument(lSplit[i]);
    end;
    lPacket := aFile.PacketName;
    if lPacket <> '' then
      result := result.GetContainedDocument(aFile.PacketName);
  end
  else if aBase = aFile then
    result := TDocumentPath.Root
  else
    result := TDocumentPath.Null;
end;

function SimpleIndexOf(List: TStrings; aText: String): Integer; inline;
begin
  // TStringList.IndexOf has a little bit of overhead that I don't want
  // and returns -1 for not found, when I want the final value.
  result:=0;
  While (result < List.Count) and (CompareText(list[Result],aText) <> 0) do
    result:=result+1;
end;

{ TRequestEditingAttachmentPromise }

constructor TRequestEditingAttachmentPromise.Create(aDocument: TDocumentPath);
begin
  inherited Create;
  fDocument := aDocument;
end;

{ TStewProject.TRequestEditAttachmentTask }

procedure TStewProject.TRequestEditAttachmentTask.NewAttachment_PropertiesRead(
  Sender: TPromise);
var
  lDescriptor: UTF8String;
  lExtension: UTF8String;
begin
  try
    GetDefaultDescriptorAndExtension((Sender as TProjectPropertiesPromise).Properties,lDescriptor,lExtension);
    fNewFile := fDocument.ToFile(fProject.fDisk,lDescriptor,lExtension);
    fNewFile.ListTemplatesFor.After(@NewAttachment_TemplatesListed,@SubPromiseRejected);
  except
    on E: Exception do
    begin
      LogException('TStewProject.TRequestEditAttachmentTask.NewAttachment_PropertiesRead',E);
      Reject(E.Message);
    end;
  end;
end;

procedure TStewProject.TRequestEditAttachmentTask.NewAttachment_TemplatesListed(
  Sender: TPromise);
var
  lTemplate: TTemplate;
  lData: TTemplateArray;
begin
  try
    lData := (Sender as TFileListTemplatesPromise).Templates;
    if Length(lData) = 0 then
    begin
      // create a simple, basic, blank file.
      // this is not cached, because editing is not done through cache.
       fNewFile.Write('').After(@NewAttachment_FileCreated,@SubPromiseRejected);
    end
    else
    begin
      if Length(lData) > 1 then
      begin
        if not fProject.DoChooseTemplate(fDocument,fAttachment,lData,lTemplate) then
        begin
          (Promise as TRequestEditingAttachmentPromise).fAttachment := TAttachment.FromFile(fProject.fDisk,fNewFile);
          (Promise as TRequestEditingAttachmentPromise).fCancelled := true;
          Resolve;
          Exit;
        end;
      end
      else
        lTemplate := lData[0];
      fNewFile.CreateFromTemplate(lTemplate).After(@NewAttachment_FileCreated,@SubPromiseRejected);
    end;

  except
    on E: Exception do
    begin
      LogException('TStewProject.TRequestEditAttachmentTask.NewAttachment_TemplatesListed',E);
      Reject(E.Message);
    end;
  end;

end;

procedure TStewProject.TRequestEditAttachmentTask.NewAttachment_FileCreated(
  Sender: TPromise);
begin
  fNewFile.OpenInEditor;
  (Promise as TRequestEditingAttachmentPromise).fAttachment := TAttachment.FromFile(fProject.fDisk,fNewFile);
  (Promise as TRequestEditingAttachmentPromise).fCancelled := false;
  // need to list the attachments in parent to make sure the new attachment is cached.
  fProject.ListAttachmentsForDocument(fDocument,true).After(@NewAttachment_ParentListed,@SubPromiseRejected);
end;

procedure TStewProject.TRequestEditAttachmentTask.TooManyAttachment_PropertiesRead
  (Sender: TPromise);
var
  lDescriptor: UTF8String;
  lExtension: UTF8String;
  lCandidatesLength: Integer;
  lFilteredLength: Integer;
  lFiltered: TFileArray;
  lChosenFile: TFile;
  i: Integer;
begin
  try
    GetDefaultDescriptorAndExtension((Sender as TProjectPropertiesPromise).Properties,lDescriptor,lExtension);

    lCandidatesLength := Length(fCandidates);
    lFilteredLength := 0;
    SetLength(lFiltered,0);
    for i := 0 to lCandidatesLength - 1 do
    begin
      if (fCandidates[i].Extension = lExtension) and
         (fCandidates[i].Descriptor = lDescriptor) then
      begin
        SetLength(lFiltered,lFilteredLength + 1);
        lFiltered[lFilteredLength] := fCandidates[i];
        inc(lFilteredLength);
      end;

    end;

    if lFilteredLength = 1 then
    begin
      lChosenFile := lFiltered[0];
      (Promise as TRequestEditingAttachmentPromise).fAttachment :=
               TAttachment.Make(fAttachment,
                                lChosenFile.Descriptor,
                                lChosenFile.Extension);
      lChosenFile.OpenInEditor;
      Resolve;
    end
    else
    begin
      if lFilteredLength = 0 then
        lFiltered := fCandidates;
      if fProject.DoChooseFileForAttachment(fDocument,fAttachment,lFiltered,lChosenFile) then
      begin
        (Promise as TRequestEditingAttachmentPromise).fAttachment :=
                 TAttachment.Make(fAttachment,
                                  lChosenFile.Descriptor,
                                  lChosenFile.Extension);
        lChosenFile.OpenInEditor;
        Resolve;
      end
      else
      begin
        // all done, and since this was a user cancelling event, it's
        // not an error
        (Promise as TRequestEditingAttachmentPromise).fAttachment := TAttachment.MakeUnknown('','');
        (Promise as TRequestEditingAttachmentPromise).fCancelled := true;
        Resolve;
      end;

    end;

  except
    on E: Exception do
    begin
      LogException('TStewProject.TRequestEditAttachmentTask.TooManyAttachment_PropertiesRead',E);
      Reject(E.Message);
    end;
  end;
end;

procedure TStewProject.TRequestEditAttachmentTask.NewAttachment_ParentListed(
  Sender: TPromise);
begin
  // don't need to actually do anything. The action was simply meant to cache
  // the new document.
  Resolve;
end;

procedure TStewProject.TRequestEditAttachmentTask.DoTask(Input: TPromise);
var
  i: Integer;
  lListedFiles: TFileArray;
  lListedLength: Integer;
  lCandidateLength: Integer;
  lChosenFile: TFile;
begin
  try

    // get a list of 'candidate' files from the listing...
    lListedFiles := (Input as TFileListPromise).GetJustFiles;
    lListedLength := Length(lListedFiles);
    lCandidateLength := 0;
    SetLength(fCandidates,0);
    for i := 0 to lListedLength - 1 do
    begin
      if (TDocumentPath.FromFile(fProject.fDisk,lListedFiles[i]) = fDocument) and
         (TAttachment.FromFile(fProject.fDisk,lListedFiles[i]).Kind = fAttachment) then
      begin
        SetLength(fCandidates,lCandidateLength + 1);
        fCandidates[lCandidateLength] := lListedFiles[i];
        inc(lCandidateLength);
      end;
    end;

    // we have a list, now figure out which one to edit.
    case lCandidateLength of
      0:
      begin
        if fProject.DoConfirmNewAttachment(fDocument,fAttachment) then
        begin
          // Need to create the new attachment, and for that we need to
          // know what the default is...
          case fAttachment of
            atNotes, atThumbnail, atPrimary:
              fProject.ReadProjectProperties.After(@NewAttachment_PropertiesRead,@SubPromiseRejected);
          else
            // we shouldn't really have gotten this far...
            Reject('Can''t edit this type of attachment: ' + TAttachment.AttachmentKindStrings[fAttachment]);
          end;
        end
        else
        begin
          // all done, and since this was a user cancelling event, it's
          // not an error
          (Promise as TRequestEditingAttachmentPromise).fAttachment := TAttachment.MakeUnknown('','');
          (Promise as TRequestEditingAttachmentPromise).fCancelled := true;
          Resolve;
        end;
      end;
      1:
      begin
        lChosenFile := fCandidates[0];
        (Promise as TRequestEditingAttachmentPromise).fAttachment :=
                 TAttachment.Make(fAttachment,
                                  lChosenFile.Descriptor,
                                  lChosenFile.Extension);
        lChosenFile.OpenInEditor;
        Resolve;
      end;
    else
      fProject.ReadProjectProperties.After(@TooManyAttachment_PropertiesRead,@SubPromiseRejected);
    end;
  except
    on E: Exception do
    begin
       LogException('TStewProject.TRequestEditAttachmentTask.DoTask',E);
       Reject(E.Message);
    end;
  end;

end;

function TStewProject.TRequestEditAttachmentTask.CreatePromise: TPromise;
begin
  result := TRequestEditingAttachmentPromise.Create(fDocument);
end;

procedure TStewProject.TRequestEditAttachmentTask.GetDefaultDescriptorAndExtension
  (aProps: TProjectProperties; out aDescriptor: UTF8String; out
  aExtension: UTF8String);
begin
  case fAttachment of
    atPrimary:
    begin
      aDescriptor := '';
      aExtension := aProps.DefaultDocExtension;
      if aExtension = '' then
        aExtension := TStewProject.DefaultDocumentPrimaryExtension;
    end;
    atNotes:
    begin
      aDescriptor := TStewProject.DocumentNotesDescriptor;
      aExtension := aProps.DefaultNotesExtension;
      if aExtension = '' then
        aExtension := TStewProject.DefaultDocumentNotesExtension;
    end;
    atThumbnail:
    begin
      aDescriptor := TStewProject.DocumentThumbnailDescriptor;
      aExtension := aProps.DefaultThumbnailExtension;
      if aExtension = '' then
        aExtension := TStewProject.DefaultDocumentThumbnailExtension;
    end;
  else
    raise Exception.Create('Invalid new attachment type: ' + TAttachment.AttachmentKindStrings[fAttachment]);
  end;
end;

constructor TStewProject.TRequestEditAttachmentTask.Defer(aDocument: TDocumentPath;
  aAttachment: TAttachmentKind; aProject: TStewProject;
  aInputPromise: TFileListPromise);
begin
  fDocument := aDocument;
  fAttachment := aAttachment;
  fProject := aProject;
  inherited Defer(aInputPromise);

end;

{ TRenameDocumentPromise }

constructor TRenameDocumentPromise.Create(aOldDocument: TDocumentPath;
  aNewDocument: TDocumentPath);
begin
  inherited Create;
  fOldDocument := aOldDocument;
  fNewDocument := aNewDocument;
end;

{ TStewProject.TRenameDocumentTask }

procedure TStewProject.TRenameDocumentTask.FilesRenamed(Sender: TPromise);
begin
  Resolve;
end;

procedure TStewProject.TRenameDocumentTask.DoTask(Input: TPromise);
var
  i: Integer;
  lListedFiles: TFileArray;
  lListedLength: Integer;
  lRenameLength: Integer;
  lOldFiles: TFileArray;
  lNewFiles: TFileArray;
begin
  lListedFiles := (Input as TFileListPromise).GetJustFiles;
  lListedLength := Length(lListedFiles);
  lRenameLength := 0;
  SetLength(lNewFiles,0);
  for i := 0 to lListedLength - 1 do
  begin
    if TDocumentPath.FromFile(fProject.fDisk,lListedFiles[i]) = fOldDocument then
    begin
      SetLength(lOldFiles,lRenameLength + 1);
      SetLength(lNewFiles,lRenameLength + 1);
      lOldFiles[lRenameLength] := lListedFiles[i];
      lNewFiles[lRenameLength] :=
          fNewDocument.ToFile(fProject.fDisk,
                              lListedFiles[i].Descriptor,
                              lListedFiles[i].Extension);
      inc(lRenameLength);
    end;
  end;
  fProject.fCache.RenameFiles(lOldFiles,lNewFiles).After(@FilesRenamed,@SubPromiseRejected);
end;

function TStewProject.TRenameDocumentTask.CreatePromise: TPromise;
begin
  result := TRenameDocumentPromise.Create(fOldDocument,fNewDocument);
end;

constructor TStewProject.TRenameDocumentTask.Defer(aOld: TDocumentPath;
  aNew: TDocumentPath; aProject: TStewProject; aInputPromise: TFileListPromise);
begin
  fOldDocument := aOld;
  fNewDocument := aNew;
  fProject := aProject;
  inherited Defer(aInputPromise);
end;

{ TShiftDocumentPromise }

constructor TShiftDocumentPromise.Create(aDocument: TDocumentPath);
begin
  inherited Create;
  fDocument := aDocument;
end;

{ TStewProject.TShiftDocumentTask }

procedure TStewProject.TShiftDocumentTask.PropertiesWritten(Sender: TPromise);
begin
  Resolve;
end;

procedure TStewProject.TShiftDocumentTask.DoTask(Input: TPromise);
var
  lReadProperties: TDocumentPropertiesPromise;
begin
  fFiles := (Input as TFileListPromise).FilesInfo;
  // we need the properties as well...
  lReadProperties := fProject.ReadDocumentProperties(fDocument.Container,false);
  lReadProperties.After(@PropertiesReceived,@SubPromiseRejected);
end;

function TStewProject.TShiftDocumentTask.CreatePromise: TPromise;
begin
  result := TShiftDocumentPromise.Create(fDocument);
end;

procedure TStewProject.TShiftDocumentTask.PropertiesReceived(Sender: TPromise
  );
var
  lDirectory: TDocumentPath;
  lProps: TDocumentProperties;
  lDocuments: TDocumentInfoArray;
  lNewDocuments: TDocumentInfoArray;
  l: Integer;
  i: Integer;
  lIndex: Integer;
  lNewIndex: Integer;
begin
  try
    lProps := (Sender as TDocumentPropertiesPromise).Properties;
    lDirectory := fDocument.Container;
    lDocuments := fProject.BuildAndSortDocumentList(lDirectory,
                          lProps,
                          fFiles);
    l := Length(lDocuments);
    SetLength(lNewDocuments,l);
    // first, we have to find the current index... Okay, it's kind
    // of annoying that we have to loop twice, but I'm not certain that
    // there's a better way.
    lIndex := -1;
    lNewIndex := -1;
    for i := 0 to l - 1 do
    begin
      if lDocuments[i].Document = fDocument then
      begin
        lIndex := i;
        if (fRelativeTo = TDocumentPath.Null) or (lNewIndex > -1) then
           break;
      end;
      if (fRelativeTo <> TDocumentPath.Null) and (lDocuments[i].Document = fRelativeTo) then
      begin
        lNewIndex := i;
        if lIndex > -1 then
          break;
      end;
    end;


    if (lIndex <> -1) and ((fRelativeTo = TDocumentPath.Null) or (lNewIndex <> -1)) then
    begin
      // we have to determine the new index as well..
      if fRelativeTo <> TDocumentPath.Null then
      begin
        lNewIndex := lNewIndex + fShiftDeltaOrIndex;
      end
      else
      begin
        lNewIndex := fShiftDeltaOrIndex;
      end;
      if lNewIndex < 0 then
        lNewIndex := 0
      else if lNewIndex > (l - 1) then
        lNewIndex := l - 1;
      if lNewIndex = lIndex then
      begin
        // we're actually done, it's annoying that we went through
        // all this work to get here, but... just leave.
        Resolve;
        exit;
      end;

      if lNewIndex < lIndex then
      begin
        for i := 0 to lNewIndex - 1 do
          lNewDocuments[i] := lDocuments[i];
        lNewDocuments[lNewIndex] := lDocuments[lIndex];
        for i := lNewIndex + 1 to lIndex do
          lNewDocuments[i] := lDocuments[i - 1];
        for i := lIndex + 1 to l - 1 do
          lNewDocuments[i] := lDocuments[i];
      end
      else
      begin
        for i := 0 to lIndex - 1 do
          lNewDocuments[i] := lDocuments[i];
        for i := lIndex to lNewIndex - 1 do
          lNewDocuments[i] := lDocuments[i + 1];
        lNewDocuments[lNewIndex] := lDocuments[lIndex];
        for i := lNewIndex + 1 to l - 1 do
          lNewDocuments[i] := lDocuments[i];
      end;

      // clear the props...
      lProps.Index.Length := 0;
      for i := 0 to l - 1 do
      begin
        lProps.Index.Put(i,lNewDocuments[i].Document.Name);
      end;
      fProject.WriteDocumentProperties(lDirectory,lProps,false).After(@PropertiesWritten,@SubPromiseRejected);
    end
    else
    // else, the document doesn't exist yet, so it doesn't make any sense to
    // add it.... right? But, we do need to resolve...
      Resolve;
  except
    on E: Exception do
    begin
      LogException('TStewProject.TShiftDocumentTask.PropertiesReceived',E);
       Reject(E.Message);
    end;
  end;
end;

constructor TStewProject.TShiftDocumentTask.Defer(aDocument: TDocumentPath;
  aNewIndexOrDelta: Integer; aRelativeTo: TDocumentPath;
  aProject: TStewProject; aPromise: TFileListPromise);
begin
  fDocument := aDocument;
  fShiftDeltaOrIndex := aNewIndexOrDelta;
  fRelativeTo := aRelativeTo;
  fProject := aProject;
  inherited Defer(aPromise);
end;

{ TStewProject.TDocumentListBuilder }


function TStewProject.TDocumentListBuilder.SortDocuments(List: TStringList;
  Index1, Index2: Integer): Integer;
var
  s1: String;
  s2: String;
  i1: Integer;
  i2: integer;
begin
  s1 := TDocumentPath.FromString(List[Index1]).Name;
  s2 := TDocumentPath.FromString(List[Index2]).Name;
  i1 := SimpleIndexOf(findex,s1);
  i2 := SimpleIndexOf(findex,s2);
  if (i1 = i2) then
    result := CompareText(s1,s2)
  else
    result := i1 - i2;
end;

constructor TStewProject.TDocumentListBuilder.Create;
begin
  inherited Create;
  fIndex := TStringList.Create;
end;

function TStewProject.TDocumentListBuilder.Build(aProperties: TDocumentProperties;
  aBasePath: TFile; aFiles: TFileInfoArray; aShadows: TDocumentInfoArray): TDocumentInfoArray;
var
  lList: TEZSortStringList;
  lFolders: TStringList;
  lShadows: TStringList;
  lInfo: TDocumentInfo;
  lDocument: TDocumentPath;
  lIsFolder: Boolean;
  l: Integer;
  i: Integer;
begin
  // FUTURE: I should be able to do this in less steps, for example
  // without translating to strings, and without keeping track of
  // folders separately. But that will require a data structure
  // that has a custom sort procedure attached and also prevents
  // adding documents which already exist.
  // to me to write
  // my own sorting procedure, at the very least.
  SetLength(Result,0);
  fIndex.Clear;
  l := aProperties.Index.Length;
  for i := 0 to l - 1 do
  begin
    fIndex.Add(aProperties.Index.Get(i).AsString);
  end;


  lList := TEZSortStringList.Create;
  try
    // start out with them sorted, so we can keep the documents unique
    // and make searching easier.
    lList.Sorted := true;
    // this doesn't do anything unless the string list is sorted, as we did just above
    lList.Duplicates:=dupIgnore;
    // we are counting items with different cases as the same, because they'll
    // be the same on some file systems anyway.
    lList.CaseSensitive:=false;

    // track the folders, since we can't just put that data into the list.
    // (Well, we could using AddObject, but I'm trying to get away from
    // casting numbers to TObject.)
    lFolders := TStringList.Create;
    try
      // same things...
      lFolders.Sorted := true;
      lFolders.Duplicates:=dupIgnore;
      lFolders.CaseSensitive:= false;

      // And, we need to track the ones entered as 'shadows', so
      // we can put that data in the info as well.
      lShadows := TStringList.Create;

      try
        lShadows.Sorted := true;
        lShadows.Duplicates:=dupIgnore;
        lShadows.CaseSensitive := false;

        // now, start adding the files as documents...
        l := Length(aFiles);
        for i := 0 to l - 1 do
        begin
          lDocument := TDocumentPath.FromFile(aBasePath,aFiles[i].Item);
          // hide the documents made from folder attachments such
          // as _stew.json and _properties.json in the root.
          if lDocument.Name <> '' then
          begin
            lIsFolder := aFiles[i].IsFolder and
               (aFiles[i].Item.Descriptor = '') and
               (aFiles[i].Item.Extension = '');
            lList.Add(lDocument.ID);
            if lIsFolder then
              lFolders.Add(lDocument.ID);
          end;
        end;

        // also add the shadows so they show up in the list.
        l := Length(aShadows);
        for i := 0 to l - 1 do
        begin
          lDocument := aShadows[i].Document;
          lIsFolder := aShadows[i].IsFolder;
          // Don't add to the 'shadows' list unless it's not already
          // found. I don't want it showing up as a "shadow" if there
          // is actually files backing it up.
          if lList.IndexOf(lDocument.ID) = -1 then
          begin
            lShadows.Add(lDocument.ID);
            lList.Add(lDocument.ID);
            if lIsFolder then
              lFolders.Add(lDocument.ID);
          end
          else if lIsFolder and (lFolders.IndexOf(lDocument.ID) = -1) then
            // even if the document is not technically a shadow, it might
            // be a folder as a shadow, so we need to add that as well.
            lFolders.Add(lDocument.ID);
        end;

        // we should just have an item for each document now. Now,
        // we have to change the sorting to sort by index.
        lList.Sorted := false;
        lList.OnEZSort:=@SortDocuments;
        lList.EZSort;
        l := lList.Count;
        SetLength(Result,l);
        for i := 0 to l - 1 do
        begin
          lInfo.Document := TDocumentPath.FromString(lList[i]);
          lInfo.IsFolder := lFolders.IndexOf(lList[i]) > -1;
          lInfo.IsShadow := lShadows.IndexOf(lList[i]) > -1;
          Result[i] := lInfo;
        end;
      finally

        lShadows.Free;
      end;

    finally
      lFolders.Free;
    end;
  finally
    lList.Free;
  end;

end;

destructor TStewProject.TDocumentListBuilder.Destroy;
begin
  FreeAndNil(fIndex);
  inherited Destroy;
end;

{ TShadowCache }

function TShadowCache.CreateShadow(aDocument: TDocumentPath): TShadowDocument;
begin
  result := CreatePath(aDocument.ID);
end;

procedure TShadowCache.DeleteShadow(aDocument: TDocumentPath);
begin
  DeletePath(aDocument.ID);
end;

function TShadowCache.HasShadow(aDocument: TDocumentPath): Boolean;
var
  lPath: TShadowDocument;
begin
  lPath := GetPath(aDocument.ID);
  result := (lPath <> nil) and lPath.IsShadow;
end;

function TShadowCache.IsFolder(aDocument: TDocumentPath): Boolean;
var
  lShadow: TShadowDocument;
begin
  lShadow := GetPath(aDocument.ID);
  result := (lShadow <> nil) and lShadow.IsFolder;
end;

function TShadowCache.GetContents(aDocument: TDocumentPath): TDocumentInfoArray;
var
  lShadow: TShadowDocument;
  lChild: TShadowDocument;
  lChildName: UTF8String;
  lInfo: TDocumentInfo;
  l: Integer;
  i: Integer;
begin
  lShadow := GetPath(aDocument.ID);
  if lShadow <> nil then
  begin
    l := lShadow.fContents.Count;
    SetLength(Result,l);
    for i := 0 to l - 1 do
    begin
      lChildName := lShadow.fContents.NameOfIndex(i);
      lChild := lShadow.fContents[i] as TShadowDocument;
      lInfo.Document := aDocument.GetContainedDocument(lChildName);
      lInfo.IsFolder := lChild.IsFolder;
      lInfo.IsShadow := true;
      Result[i] := lInfo;
    end;

  end;
end;

{ TShadowDocument }

function TShadowDocument.CreatePath(aPath: UTF8String): TShadowDocument;
var
  lChildName: UTF8String;
  lChildPath: UTF8String;
  i: Integer;
  lChildShadow: TShadowDocument;
begin
  SplitPath(aPath,lChildName,lChildPath);
  i := fContents.FindIndexOf(lChildName);
  if i = -1 then
  begin
    lChildShadow := TShadowDocument.Create(lChildPath = '');
    fContents.Add(lChildName,lChildShadow);
  end
  else
    lChildShadow := fContents.Items[i] as TShadowDocument;
  if lChildPath = '' then
    result := lChildShadow
  else
    result := lChildShadow.CreatePath(lChildPath);
end;

function TShadowDocument.GetIsFolder: Boolean;
begin
  result := fContents.Count > 0;
end;

function TShadowDocument.GetPath(aPath: UTF8String): TShadowDocument;
var
  lChildName: UTF8String;
  lChildPath: UTF8String;
  i: Integer;
  lChildShadow: TShadowDocument;
begin
  SplitPath(aPath,lChildName,lChildPath);
  i := fContents.FindIndexOf(lChildName);
  if i = -1 then
  begin
    result := nil;
  end
  else
  begin
    lChildShadow := fContents.Items[i] as TShadowDocument;
    if lChildPath = '' then
      result := lChildShadow
    else
      result := lChildShadow.GetPath(lChildPath);
  end;
end;

procedure TShadowDocument.DeletePath(aPath: UTF8String);
var
  lChildName: UTF8String;
  lChildPath: UTF8String;
  i: Integer;
  lChildShadow: TShadowDocument;
begin
  SplitPath(aPath,lChildName,lChildPath);
  i := fContents.FindIndexOf(lChildName);
  if i > -1 then
  begin
    lChildShadow := fContents.Items[i] as TShadowDocument;
    if lChildPath = '' then
    begin
      fContents.Delete(i);
    end
    else
    begin
      lChildShadow.DeletePath(lChildPath);
      if not lChildShadow.IsFolder then
        fContents.Delete(i);
    end;
  end;
end;

procedure TShadowDocument.SplitPath(aPath: UTF8String; out
  aChildName: UTF8String; out aChildPath: UTF8String);
var
  l: Integer;
  i: Integer;
begin
  l := Length(aPath);
  i := 1;
  while i <= l do
  begin
    if aPath[i] = '/' then
      break;
    inc(i);
  end;
  aChildName:=Copy(aPath,1,i - 1);
  aChildPath:=Copy(aPath,i + 1,MaxInt);
end;

constructor TShadowDocument.Create(aIsShadow: Boolean);
begin
  inherited Create;
  fContents := TFPHashObjectList.Create(true);
  fIsShadow := aIsShadow;
end;

destructor TShadowDocument.Destroy;
begin
  FreeAndNil(fContents);
  inherited Destroy;
end;

{ TDocumentIsFolderPromise }

constructor TDocumentIsFolderPromise.Create(aDocument: TDocumentPath);
begin
  inherited Create;
  fDocument := aDocument;
end;

{ TStewProject.TIsDocumentAFolderTask }

procedure TStewProject.TIsDocumentAFolderTask.DoTask(Input: TPromise);
begin
  (Promise as TDocumentIsFolderPromise).fIsFolder := (Input as TFileExistencePromise).IsFolder or
                                                     fProject.fShadows.IsFolder(fDocument);
  Resolve;
end;

function TStewProject.TIsDocumentAFolderTask.CreatePromise: TPromise;
begin
  result := TDocumentIsFolderPromise.Create(fDocument);
end;

constructor TStewProject.TIsDocumentAFolderTask.Defer(aDocument: TDocumentPath;
  aProject: TStewProject; aPromise: TFileExistencePromise);
begin
  fDocument := aDocument;
  fProject := aProject;
  inherited Defer(aPromise);
end;

{ TDocumentSynopsisPromise }

procedure TDocumentSynopsisPromise.LoadAnswer(aText: UTF8String);
begin
  fSynopsis := aText;
end;

constructor TDocumentSynopsisPromise.Create(aDocument: TDocumentPath);
begin
  inherited Create;
  fDocument := aDocument;
end;

{ TStewProject.TReadDocumentSynopsisTask }

procedure TStewProject.TReadDocumentSynopsisTask.DoTask(Input: TPromise);
begin
  if (Input as TFileReadPromise).Exists and not (Input as TFileReadPromise).IsFolder then
     (Promise as TDocumentSynopsisPromise).LoadAnswer((Input as TFileReadPromise).ReadString)
  else
     (Promise as TDocumentSynopsisPromise).LoadAnswer('');
  Resolve;
end;

function TStewProject.TReadDocumentSynopsisTask.CreatePromise: TPromise;
begin
  result := TDocumentSynopsisPromise.Create(fDocument);

end;

constructor TStewProject.TReadDocumentSynopsisTask.Defer(
  aDocument: TDocumentPath; aPromise: TFileReadPromise);
begin
  fDocument := aDocument;
  inherited Defer(aPromise);

end;

{ TDocumentListPromise }

constructor TDocumentListPromise.Create(aDocument: TDocumentPath);
begin
  inherited Create;
  fDocument := aDocument;
end;

{ TStewProject.TListDocumentsInFolderTask }

procedure TStewProject.TListDocumentsInFolderTask.PropertiesReceived(
  Sender: TPromise);
var
  lDocuments: TDocumentInfoArray;
begin
  try
    lDocuments := fProject.BuildAndSortDocumentList(fDocument,
                          (Sender as TDocumentPropertiesPromise).Properties,
                          fFiles);

    (Promise as TDocumentListPromise).fList := lDocuments;
    Resolve;

  except
    on E: Exception do
    begin
      LogException('TStewProject.TListDocumentsInFolderTask.PropertiesReceived',E);
      Reject(E.Message);
    end;
  end;
end;

procedure TStewProject.TListDocumentsInFolderTask.DoTask(Input: TPromise);
var
  lReadProperties: TDocumentPropertiesPromise;
begin
  fFiles := (Input as TFileListPromise).FilesInfo;
  // we need the properties as well...
  lReadProperties := fProject.ReadDocumentProperties(fDocument,false);
  lReadProperties.After(@PropertiesReceived,@SubPromiseRejected);
end;

function TStewProject.TListDocumentsInFolderTask.CreatePromise: TPromise;
begin
  Result := TDocumentListPromise.Create(fDocument);
end;

constructor TStewProject.TListDocumentsInFolderTask.Defer(
  aDocument: TDocumentPath; aProject: TStewProject; aPromise: TFileListPromise);
begin
  fDocument := aDocument;
  fProject := aProject;
  inherited Defer(aPromise);
end;

{ TWriteAttachmentPromise }

constructor TWriteAttachmentPromise.Create(aDocument: TDocumentPath;
  aAttachment: TAttachment);
begin
  inherited Create;
  fDocument := aDocument;
  fAttachment := aAttachment;
end;

{ TStewProject.TWriteProjectPropertiesTask }

procedure TStewProject.TWriteProjectPropertiesTask.HandleError(Input: TPromise;
  Error: TPromiseError);
begin
  inherited HandleError(Input, Error);
  (Promise as TWriteProjectPropertiesPromise).fIsConflict := (Input as TFileWritePromise).IsConflict;
end;

procedure TStewProject.TWriteProjectPropertiesTask.DoTask(Input: TPromise);
begin
  Resolve;
end;

function TStewProject.TWriteProjectPropertiesTask.CreatePromise: TPromise;
begin
  result := TWriteProjectPropertiesPromise.Create;
end;

constructor TStewProject.TWriteProjectPropertiesTask.Defer(
  aPromise: TFileWritePromise);
begin
  inherited Defer(aPromise);
end;

{ TDocumentPropertiesPromise }

procedure TDocumentPropertiesPromise.LoadAnswer(aStream: TStream);
begin
  if aStream <> nil then
    fProperties := FromJSON(TDocumentProperties,aStream) as TDocumentProperties
  else
    fProperties := TDocumentProperties.Create;
end;

constructor TDocumentPropertiesPromise.Create(aDocument: TDocumentPath);
begin
  inherited Create;
  fDocument := aDocument;
end;

destructor TDocumentPropertiesPromise.Destroy;
begin
  FreeAndNil(fProperties);
  inherited Destroy;
end;

{ TStewProject.TReadDocumentPropertiesTask }

procedure TStewProject.TReadDocumentPropertiesTask.DoTask(Input: TPromise);
begin
  if (Input as TFileReadPromise).Exists and not (Input as TFileReadPromise).IsFolder then
     (Promise as TDocumentPropertiesPromise).LoadAnswer((Input as TFileReadPromise).Data)
  else
     (Promise as TDocumentPropertiesPromise).LoadAnswer(nil);
  Resolve;
end;

function TStewProject.TReadDocumentPropertiesTask.CreatePromise: TPromise;
begin
  result := TDocumentPropertiesPromise.Create(fDocument);
end;

constructor TStewProject.TReadDocumentPropertiesTask.Defer(
  aDocument: TDocumentPath; aPromise: TFileReadPromise);
begin
  fDocument := aDocument;
  inherited Defer(aPromise);
end;

{ TStewProject.TWriteAttachmentTask }

procedure TStewProject.TWriteAttachmentTask.HandleError(Input: TPromise;
  Error: TPromiseError);
begin
  inherited HandleError(Input, Error);
  (Promise as TWriteAttachmentPromise).fIsConflict := (Input as TFileWritePromise).IsConflict;
end;

procedure TStewProject.TWriteAttachmentTask.DoTask(Input: TPromise);
begin
  Resolve;
end;

function TStewProject.TWriteAttachmentTask.CreatePromise: TPromise;
begin
  result := TWriteAttachmentPromise.Create(fDocument,fAttachment);
end;

constructor TStewProject.TWriteAttachmentTask.Defer(aDocument: TDocumentPath;
  aAttachment: TAttachment; aPromise: TFileWritePromise);
begin
  fDocument := aDocument;
  fAttachment := aAttachment;
  inherited Defer(aPromise);
end;

{ TAttachment }

class function TAttachment.MakeUnknown(aDescriptor: UTF8String;
  aExtension: UTF8String): TAttachment;
begin
  result.Kind:=atUnknown;
  result.Extension := aExtension;
  result.Descriptor := aDescriptor;
end;

class function TAttachment.MakeDirectory: TAttachment;
begin
  result.Kind:= atFolder;
  Result.Extension := '';
  Result.Descriptor:= '';
end;

class function TAttachment.MakePrimary(aExtension: UTF8String): TAttachment;
begin
  result.Kind:=atPrimary;
  Result.Extension := aExtension;
  result.Descriptor := '';
end;

class function TAttachment.MakeProperties: TAttachment;
begin
  result.Kind := atProperties;
  Result.Descriptor:=TStewProject.DocumentPropertiesDescriptor;
  Result.Extension := TStewProject.PropertiesExtension;
end;

class function TAttachment.MakeNotes(aExtension: UTF8String): TAttachment;
begin
  result.Kind:=atNotes;
  Result.Extension := aExtension;
  result.Descriptor := TStewProject.DocumentNotesDescriptor;
end;

class function TAttachment.MakeThumbnail(aExtension: UTF8String): TAttachment;
begin
  result.Kind:=atThumbnail;
  Result.Extension := aExtension;
  result.Descriptor := TStewProject.DocumentThumbnailDescriptor;
end;

class function TAttachment.MakeSynopsis: TAttachment;
begin
  result.Kind:=atSynopsis;
  Result.Extension := TStewProject.DocumentSynopsisExtension;
  result.Descriptor := TStewProject.DocumentSynopsisDescriptor;

end;

class function TAttachment.MakeBackup(aDescriptor: UTF8String;
  aExtension: UTF8String): TAttachment;
begin
  result.Kind:=atBackup;
  Result.Extension := aExtension;
  result.Descriptor := aDescriptor;

end;

class function TAttachment.Make(aKind: TAttachmentKind;
  aDescriptor: UTF8String; aExtension: UTF8String): TAttachment;
begin
  result.Kind := aKind;
  result.Descriptor:= aDescriptor;
  result.Extension:= aExtension;
end;

class function TAttachment.FromFile(aBase: TFile; aFile: TFile): TAttachment;
begin
  result := TAttachment.MakeUnknown('','');
  if aBase.Contains(aFile) then
  begin
    begin
      result.Extension := aFile.Extension;
      result.Descriptor := aFile.Descriptor;
      case result.Descriptor of
        '':
          if Result.Extension = '' then
            result.Kind:= atFolder
          else
            result.Kind := atPrimary;
        TStewProject.DocumentSynopsisDescriptor:
          if result.Extension = TStewProject.DocumentSynopsisExtension then
            result.Kind := atSynopsis;
        TStewProject.DocumentPropertiesDescriptor:
          if result.Extension = TStewProject.PropertiesExtension then
            result.Kind := atProperties;
        TStewProject.DocumentNotesDescriptor:
          result.Kind := atNotes;
        TStewProject.DocumentThumbnailDescriptor:
          result.Kind := atThumbnail;
      else
        if Pos(TStewProject.DocumentBackupDescriptorPrefix,aFile.Descriptor) = 1 then
          result.Kind := atBackup;
      end;
    end;
  end
  else if aBase = aFile then
     result.Kind := atFolder
  else
  // else we're actually not at all interested in this file...
end;

{ TAttachmentError }

constructor TAttachmentError.Create(aAction: TProjectEventKind;
  aDocument: TDocumentPath; aAttachment: TAttachment; aError: TPromiseError);
begin
  inherited Create(aAction,aDocument,aAttachment);
  fError := aError;
end;

function TAttachmentError.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription + ' "' + fError + '"';
end;

{ TDocumentSynopsisDataReceived }

constructor TDocumentSynopsisDataReceived.Create(aDocument: TDocumentPath;
  aSynopsis: UTF8String);
begin
  inherited Create(paAttachmentDataReceived,aDocument,TAttachment.MakeSynopsis);
  fSynopsis := aSynopsis;
end;

{ TDocumentPropertiesDataReceived }

constructor TDocumentPropertiesDataReceivedEvent.Create(aDocument: TDocumentPath; aProperties: TDocumentProperties);
begin
  inherited Create(paAttachmentDataReceived,aDocument,TAttachment.MakeProperties);
  fProperties := aProperties;
end;

{ TAttachmentEvent }

constructor TAttachmentEvent.Create(aAction: TProjectEventKind;
  aDocument: TDocumentPath; aAttachment: TAttachment);
begin
  inherited Create(aAction,aDocument);
  fAttachment := aAttachment;
end;

function TAttachmentEvent.AttachmentName: UTF8String;
begin
  result := TAttachment.AttachmentKindStrings[fAttachment.Kind];
end;

function TAttachmentEvent.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription + ' <' + TAttachment.AttachmentKindStrings[fAttachment.Kind] + '> "_' + fAttachment.Descriptor + '.' + fAttachment.Extension + '"';
end;

{ TDocumentError }

constructor TDocumentError.Create(aAction: TProjectEventKind;
  aDocument: TDocumentPath; aError: TPromiseError);
begin
  inherited Create(aAction,aDocument);
  fError := aError;
end;

function TDocumentError.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription + ' "' + fError + '"';
end;

{ TDocumentListedEvent }

constructor TDocumentListDataReceivedEvent.Create(aDocument: TDocumentPath;
  aList: TDocumentInfoArray);
begin
  inherited Create(paDocumentListDataReceived,aDocument);
  fList := aList;

end;

function TDocumentListDataReceivedEvent.GetDescription: UTF8String;
var
  i: Integer;
begin
  Result:=inherited GetDescription;
  result := result + ' [';
  for i := 0 to Length(fList) - 1 do
  begin
    if i > 0 then
      result := result + ',';
    if not (fList[i].Document = TDocumentPath.Null) then
       result := result + ' "' + fList[i].Document.ID + '"'
    else
       result := result + ' <BLANK DOCUMENT PATH>';
    if fList[i].IsFolder then
       result := result + ' <FOLDER>';
    if fList[i].IsShadow then
       result := result + ' <SHADOW>';
  end;
  result := result + ']';
end;

{ TDocumentRenameErrorEvent }

constructor TDocumentRenameErrorEvent.Create(aAction: TProjectEventKind;
  aDocument: TDocumentPath; aNewDocument: TDocumentPath; aError: TPromiseError);
begin
  inherited Create(aAction,aDocument,aNewDocument);
  fError := aError;
end;

function TDocumentRenameErrorEvent.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription + ' "' + fError + '"';
end;

{ TDocumentRenameEvent }

constructor TDocumentRenameEvent.Create(aAction: TProjectEventKind;
  aDocument: TDocumentPath; aNewDocument: TDocumentPath);
begin
  inherited Create(aAction,aDocument);
  fNewDocument := aNewDocument;
end;

function TDocumentRenameEvent.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription;
  if not (fNewDocument = TDocumentPath.Null) then
    result := result + ' TO: ' + fNewDocument.ID
  else
     result := result + ' TO: <BLANK DOCUMENT PATH>';
end;

{ TDocumentEvent }

constructor TDocumentEvent.Create(aAction: TProjectEventKind;
  aDocument: TDocumentPath);
begin
  inherited Create(aAction);
  fDocument := aDocument;
end;

function TDocumentEvent.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription;
  if not (fDocument = TDocumentPath.Null) then
    result := result + ' "' + fDocument.ID + '"'
  else
     result := result + ' <BLANK DOCUMENT PATH>';
end;

{ TProjectError }

constructor TProjectError.Create(aAction: TProjectEventKind; aError: TPromiseError
  );
begin
  inherited Create(aAction);
  fError := aError;
end;

function TProjectError.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription + ' "' + fError + '"';
end;

{ TProjectPropertiesDataReceivedEvent }

constructor TProjectPropertiesDataReceivedEvent.Create(
  aProperties: TProjectProperties);
begin
  inherited Create(paProjectPropertiesDataReceived);
  fProperties := aProperties;
end;

{ TProjectEvent }

constructor TProjectEvent.Create(aAction: TProjectEventKind);
begin
  inherited Create;
  fAction := aAction;
  fIsError := aAction in
  [paProjectPropertiesFileLoadingFailed,
  paProjectPropertiesFileSavingFailed,
  paDocumentsFileListingFailed,
  paDocumentFileCheckingFailed,
  paAttachmentFileLoadingFailed,
  paAttachmentFileSavingFailed,
  paDocumentFileRenameFailed,
  paProjectPropertiesLoadingFailed,
  paProjectPropertiesSavingFailed,
  paProjectPropertiesSaveConflictOccurred,
  paDocumentsListingFailed,
  paAttachmentListingFailed,
  paDocumentFolderCheckFailed,
  paDocumentShiftFailed,
  paDocumentRenamingFailed,
  paAttachmentLoadingFailed,
  paAttachmentSavingFailed,
  paAttachmentSaveConflictOccurred,
  paAttachmentEditingFailed,
  paUnexpectedProjectError,
  paUnexpectedDocumentError,
  paUnexpectedAttachmentError]
end;

function TProjectEvent.GetDescription: UTF8String;
begin
  result := '<' + EventKindStrings[fAction] + '>';
  if fIsError then
    result := '<ERROR> ' + result;
end;

{ TProjectPropertiesPromise }

destructor TProjectPropertiesPromise.Destroy;
begin
  FreeAndNil(fProperties);
  inherited Destroy;
end;

procedure TProjectPropertiesPromise.LoadAnswer(aStream: TStream);
begin
  if aStream <> nil then
    fProperties := FromJSON(TProjectProperties,aStream) as TProjectProperties
  else
    fProperties := TProjectProperties.Create;
end;

{ TStewProject.TReadProjectPropertiesTask }

procedure TStewProject.TReadProjectPropertiesTask.DoTask(Input: TPromise);
begin
  if (Input as TFileReadPromise).Exists and not (Input as TFileReadPromise).IsFolder then
     (Promise as TProjectPropertiesPromise).LoadAnswer((Input as TFileReadPromise).Data)
  else
     (Promise as TProjectPropertiesPromise).LoadAnswer(nil);
  Resolve;
end;

function TStewProject.TReadProjectPropertiesTask.CreatePromise: TPromise;
begin
  result := TProjectPropertiesPromise.Create;
end;

constructor TStewProject.TReadProjectPropertiesTask.Defer(aPromise: TFileReadPromise);
begin
  inherited Defer(aPromise);
end;

{ TProjectOpenTask }

procedure TStewProject.TProjectCheckAndCreateTask.ResolveCreateProject(aPath: TFile);
var
  lProject: TStewProject;
begin
  lProject := TStewProject.Create(aPath);
  (Promise as TProjectPromise).SetAnswer(lProject);
  Resolve;
end;

function TStewProject.TProjectCheckAndCreateTask.CreatePromise: TPromise;
begin
  result := TProjectPromise.Create(fPath);
end;

constructor TStewProject.TProjectCheckAndCreateTask.Enqueue(aPath: Tfile);
begin
  fPath := aPath;
  inherited Enqueue;
end;


{ TProjectOpenInParentTask }

procedure TStewProject.TProjectCheckInParentTask.FileExistsInParent(Sender: TPromise);
begin
  (Promise as TProjectPromise).SetAnswer((Sender as TProjectPromise).Project);
  Resolve;
end;

procedure TStewProject.TProjectCheckInParentTask.FileExists(Sender: TPromise);
var
  lParent: TFile;
begin
  if ((Sender as TFileExistencePromise).Exists) then
  begin
    ResolveCreateProject(Path);
  end
  else
  begin
    lParent := Path.Directory;
    if lParent = Path then
    begin
      Resolve;
    end
    else
    begin
      TProjectCheckInParentTask.Enqueue(lParent).After(@FileExistsInParent,@SubPromiseRejected);
    end;
  end;
end;

procedure TStewProject.TProjectCheckInParentTask.DoTask;
begin
  GetProjectPropertiesPath(Path).CheckExistence.After(@FileExists,@SubPromiseRejected);
end;

{ TProjectPromise }

constructor TProjectPromise.Create(aPath: TFile);
begin
  fPath := aPath;
  inherited Create;
end;

procedure TProjectPromise.SetAnswer(aProject: TStewProject);
begin
  fProject := aProject;
end;

{ TProjectOpenAtPathTask }

procedure TStewProject.TProjectCheckAtPathTask.FileExists(Sender: TPromise);
begin
  if ((Sender as TFileExistencePromise).Exists) then
  begin
    ResolveCreateProject(Path);
  end
  else
  begin
    Resolve;
  end;
end;

procedure TStewProject.TProjectCheckAtPathTask.DoTask;
begin
  GetProjectPropertiesPath(Path).CheckExistence.After(@FileExists,@SubPromiseRejected);
end;

{ TDocumentPath }

function TDocumentPath.GetContainer: TDocumentPath;
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

function TDocumentPath.GetIsNull: Boolean;
begin
  result := fID = '';
end;

function TDocumentPath.GetIsSystem: Boolean;
begin
  if Length(fID) > 0 then
    result := fID[1] = ':';
end;

function TDocumentPath.GetName: UTF8String;
var
  i : longint;
begin
  I := Length(fID);
  while (I > 0) and not (fID[I] = '/') do
    Dec(I);
  Result := Copy(fID, I + 1, MaxInt);
end;

function TDocumentPath.GetContainedDocument(aName: UTF8String): TDocumentPath;
begin
  // FUTURE: I'm not certain this is where I want to do this. What happens
  // if the file on the disk already has a troublesome name? It might be
  // better to put this in the UI itself.
  if IsNameTroublesome(aName) then
     raise Exception.Create('Document name "' + aName + '" isn''t going to work.');
  result.fID := IncludeTrailingSlash(fID) + aName;
end;

function TDocumentPath.Contains(aChild: TDocumentPath): Boolean;
var
  aChildContainer: TDocumentPath;
begin
  aChildContainer := aChild.Container;
  result := (aChildContainer = Self) or // child's parent is this guy
            ((aChildContainer <> TDocumentPath.Root) and // child's parent is not root
            Contains(aChildContainer)); // this guy is a parent of the child's parent
end;

function TDocumentPath.Split: TStringArray;
var
  idLen: Integer;
  resultLen: Integer;
  cur: Integer;
  last: Integer;
begin
  if fID = '' then
     raise Exception.Create('Can''t split the null path');
  idLen := Length(fID);
  resultLen := 0;
  SetLength(Result,0);
  // skip the starting slash...
  cur := 2;
  last := cur;
  while (cur <= idLen) do
  begin
    if fID[cur] = '/' then
    begin
      SetLength(Result,resultLen + 1);
      Result[resultLen] := Copy(fID, last, cur - last);
      inc(resultLen);
      last := cur + 1;
    end;
    Inc(cur);
  end;
  if cur > last then
  begin
    SetLength(Result,resultLen + 1);
    Result[resultLen] := Copy(fID, last, cur - last);
  end;
end;

function TDocumentPath.ToFile(aBase: TFile; aDescriptor: UTF8String;
  aExtension: UTF8String): TFile;
var
  lSplit: TStringArray;
  i: Integer;
begin
  if Self = TDocumentPath.Root then
  begin
    // Root attachments are files *inside* the folder

    if (aDescriptor = '') and (aExtension = '') then
    // the only 'attachment' on the root that isn't contained inside the
    // directory is the directory itself.
      result := aBase
    else
      result := aBase.GetContainedFile('',aDescriptor,aExtension,true)
  end
  else if Self = TDocumentPath.Null then
  begin
    raise Exception.Create('Can''t determine file for null document path')
  end
  else
  begin
    lSplit := Split;
    result := aBase;
    for i := 0 to Length(lSplit) - 1 do
    begin
      result := Result.GetContainedFile(lSplit[i]);
    end;
    // Can't just switch the descriptor and extension, because we need to dashify and dotify it.
    Result := Result.Directory.GetContainedFile(Result.Name,aDescriptor,aExtension,true);
  end;
end;

class function TDocumentPath.FromString(const aPath: UTF8String): TDocumentPath;
begin
  result.fID := IncludeLeadingSlash(ExcludeTrailingSlash(aPath));
end;

class function TDocumentPath.GetSystemDocument(aName: UTF8String): TDocumentPath;
begin
  result.fID := ':' + aName;
end;

{ TStewProject }

procedure TStewProject.AttachmentListReceived(Sender: TPromise);
begin
  ReportEvent(TAttachmentListDataReceivedEvent.Create(
              (Sender as TAttachmentListPromise).Document,
              (Sender as TAttachmentListPromise).List));
end;

procedure TStewProject.AttachmentListError(Sender: TPromise;
  aError: TPromiseError);
begin
  ReportEvent(TDocumentError.Create(paAttachmentListingFailed,
                                    (Sender as TAttachmentListPromise).Document,
                                    aError));
end;

procedure TStewProject.ReportEvent(aEvent: TProjectEvent);
var
  i: Integer;
begin
  try
    for i := 0 to fObservers.Count - 1 do
       fObservers[i](Self,aEvent);
  finally
    aEvent.Free;
  end;
end;

procedure TStewProject.CacheActivity(Sender: TPromise);
var
  lDocument: TDocumentPath;
  lAttachment: TAttachment;

  procedure ReportRead(Sender: TFileReadPromise); inline;
  begin
    if Sender.Path = GetProjectPropertiesPath(fDisk) then
    begin
        if Sender.State = psIncomplete then
           ReportEvent(TProjectEvent.Create(paLoadingProjectPropertiesFile))
        else
           ReportEvent(TProjectEvent.Create(paProjectPropertiesFileLoaded));
    end
    else
    begin
      lDocument := TDocumentPath.FromFile(fDisk,Sender.Path);
      lAttachment := TAttachment.FromFile(fDisk,Sender.Path);
      if Sender.State = psIncomplete then
         ReportEvent(TAttachmentEvent.Create(paLoadingAttachmentFile,lDocument,lAttachment))
      else
      begin
        // clear the shadow...
          if Sender.Exists then
             DeleteShadow(lDocument);
         ReportEvent(TAttachmentEvent.Create(paAttachmentFileLoaded,lDocument,lAttachment));
      end;
    end;
  end;

  procedure ReportWrite(Sender: TFileWritePromise); inline;
  begin
    if Sender.Path = GetProjectPropertiesPath(fDisk)  then
    begin
        if Sender.State = psIncomplete then
           ReportEvent(TProjectEvent.Create(paSavingProjectPropertiesFile))
        else
           ReportEvent(TProjectEvent.Create(paProjectPropertiesFileSaved));
    end
    else
    begin
      lAttachment := TAttachment.FromFile(fDisk,Sender.Path);
      lDocument := TDocumentPath.FromFile(fDisk,Sender.Path);
      if Sender.State = psIncomplete then
         ReportEvent(TAttachmentEvent.Create(paSavingAttachmentFile,lDocument,lAttachment))
      else
      begin
        // clear the shadow...
        DeleteShadow(lDocument);
        ReportEvent(TAttachmentEvent.Create(paAttachmentFileSaved,lDocument,lAttachment));
      end;
    end;
  end;

  procedure ReportList(Sender: TFileListPromise); inline;
  begin
   if not (Sender.Path = GetProjectPropertiesPath(fDisk)) then
    begin
      lDocument := TDocumentPath.FromFile(fDisk,Sender.Path);
      if Sender.State = psIncomplete then
         ReportEvent(TDocumentEvent.Create(paListingDocumentFiles,lDocument))
      else
      begin
        if Sender.Exists then
           DeleteShadow(lDocument);
         ReportEvent(TDocumentEvent.Create(paDocumentFilesListed,lDocument));
      end;
    end;
    // else, its an attempt to "list" the directory contents of a file. I'm
    // not worried about this.
  end;

  procedure ReportCheck(Sender: TFileExistencePromise); inline;
  begin
   if ((fDisk = Sender.Path) or fDisk.Contains(Sender.Path)) then
    begin
      lDocument := TDocumentPath.FromFile(fDisk,Sender.Path);
      if Sender.State = psIncomplete then
         ReportEvent(TDocumentEvent.Create(paCheckingDocumentFile,lDocument))
      else
      begin
        if Sender.Exists then
           DeleteShadow(lDocument);
         ReportEvent(TDocumentEvent.Create(paDocumentFileChecked,lDocument));
      end;
    end;
    // else, its an attempt to "list" the directory contents of a file. I'm
    // not worried about this.

  end;

begin
  if Sender is TFileReadPromise then
  begin
    ReportRead(Sender as TFileReadPromise);
  end
  else if Sender is TFileWritePromise then
  begin
    ReportWrite(Sender as TFileWritePromise);
  end
  else if Sender is TFileListPromise then
  begin
    ReportList(Sender as TFileListPromise);
  end
  else if Sender is TFileExistencePromise then
  begin
    ReportCheck(Sender as TFileExistencePromise)
  end
  else
    // we're not interested in renaming events,
    // which are handled by the specific renaming
    // promise, because we need the names of the documents being
    // renamed.
    Exit;


end;

procedure TStewProject.CacheError(Sender: TPromise; aError: TPromiseError);
var
  lDocument: TDocumentPath;
  lAttachment: TAttachment;

  procedure ReportRead(Sender: TFileReadPromise); inline;
  begin
    if Sender.Path = GetProjectPropertiesPath(fDisk) then
    begin
      ReportEvent(TProjectError.Create(paProjectPropertiesFileLoadingFailed,aError));
    end
    else
    begin
      lDocument := TDocumentPath.FromFile(fDisk,Sender.Path);
      lAttachment := TAttachment.FromFile(fDisk,Sender.Path);
      ReportEvent(TAttachmentError.Create(paAttachmentFileLoadingFailed,lDocument,lAttachment,aError));
    end;
  end;

  procedure ReportWrite(Sender: TFileWritePromise); inline;
  begin
    if Sender.Path = GetProjectPropertiesPath(fDisk)  then
    begin
      ReportEvent(TProjectError.Create(paProjectPropertiesFileSavingFailed,aError));
    end
    else
    begin
      lAttachment := TAttachment.FromFile(fDisk,Sender.Path);
      lDocument := TDocumentPath.FromFile(fDisk,Sender.Path);
      ReportEvent(TAttachmentError.Create(paAttachmentFileSavingFailed,lDocument,lAttachment,aError));
    end;
  end;

  procedure ReportList(Sender: TFileListPromise); inline;
  begin
    if not (Sender.Path = GetProjectPropertiesPath(fDisk)) then
    begin
      lDocument := TDocumentPath.FromFile(fDisk,Sender.Path);
      ReportEvent(TDocumentError.Create(paDocumentsFileListingFailed,lDocument,aError));
    end
    else
      ReportEvent(TProjectError.Create(paUnexpectedProjectError,'Error attempting to list contents of project properties: ' + aError));
  end;

  procedure ReportCheck(Sender: TFileExistencePromise); inline;
  begin
    if ((fDisk = Sender.Path) or fDisk.Contains(Sender.Path)) then
    begin
      lDocument := TDocumentPath.FromFile(fDisk,Sender.Path);
      ReportEvent(TDocumentError.Create(paDocumentFileCheckingFailed,lDocument,aError));
    end
    else
      ReportEvent(TProjectError.Create(paUnexpectedProjectError,'Error attempting to check the existence of a file outside the project: ' + aError));
  end;

begin
  if Sender is TFileReadPromise then
  begin
    ReportRead(Sender as TFileReadPromise);
  end
  else if Sender is TFileWritePromise then
  begin
    ReportWrite(Sender as TFileWritePromise);
  end
  else if Sender is TFileListPromise then
  begin
    ReportList(Sender as TFileListPromise);
  end
  else if Sender is TFileExistencePromise then
  begin
    ReportCheck(Sender as TFileExistencePromise)
  end
  else
    ReportEvent(TProjectError.Create(paUnexpectedProjectError,'Unexpected error from file cache: ' + aError));

end;

procedure TStewProject.ProjectPropertiesDataReceived(Sender: TPromise);
begin
  ReportEvent(TProjectPropertiesDataReceivedEvent.Create((Sender as TProjectPropertiesPromise).Properties));
end;

procedure TStewProject.ProjectPropertiesReadError(Sender: TPromise;
  aError: TPromiseError);
begin
  ReportEvent(TProjectError.Create(paProjectPropertiesLoadingFailed,aError));
end;

procedure TStewProject.ProjectPropertiesWriteError(Sender: TPromise;
  aError: TPromiseError);
begin
  if (Sender as TWriteProjectPropertiesPromise).IsConflict then
     ReportEvent(TProjectEvent.Create(paProjectPropertiesSaveConflictOccurred))
  else
     ReportEvent(TProjectError.Create(paProjectPropertiesSavingFailed,aError));
end;

procedure TStewProject.DocumentListError(Sender: TPromise; aError: TPromiseError
  );
begin
  ReportEvent(TDocumentError.Create(paDocumentsListingFailed,
                                    (Sender as TDocumentListPromise).Document,
                                    aError));
end;

procedure TStewProject.DocumentListReceived(Sender: TPromise);
begin
  ReportEvent(TDocumentListDataReceivedEvent.Create(
              (Sender as TDocumentListPromise).Document,
              (Sender as TDocumentListPromise).List));
end;

procedure TStewProject.DocumentPropertiesDataReceived(Sender: TPromise);
begin
  ReportEvent(TDocumentPropertiesDataReceivedEvent.Create(
                (Sender as TDocumentPropertiesPromise).Document,
                (Sender as TDocumentPropertiesPromise).Properties));
end;

procedure TStewProject.DocumentPropertiesReadError(Sender: TPromise;
  aError: TPromiseError);
begin
  ReportEvent(TAttachmentError.Create(paAttachmentLoadingFailed,
                                      (Sender as TDocumentPropertiesPromise).Document,
                                      TAttachment.MakeProperties,
                                      aError));
end;

procedure TStewProject.AttachmentWriteError(Sender: TPromise;
  aError: TPromiseError);
begin
  if (Sender as TWriteAttachmentPromise).IsConflict then
     ReportEvent(TAttachmentEvent.Create(paAttachmentSaveConflictOccurred,
                                         (Sender as TWriteAttachmentPromise).Document,
                                         (Sender as TWriteAttachmentPromise).fAttachment))
  else
     ReportEvent(TAttachmentError.Create(paAttachmentSavingFailed,
                                         (Sender as TWriteAttachmentPromise).Document,
                                         (Sender as TWriteAttachmentPromise).fAttachment,
                                         aError));
end;

procedure TStewProject.DocumentSynopsisDataReceived(Sender: TPromise);
begin
  ReportEvent(TDocumentSynopsisDataReceived.Create(
                (Sender as TDocumentSynopsisPromise).Document,
                (Sender as TDocumentSynopsisPromise).Synopsis));
end;

procedure TStewProject.DocumentSynopsisReadError(Sender: TPromise;
  aError: TPromiseError);
begin
  ReportEvent(TAttachmentError.Create(paAttachmentLoadingFailed,
                                      (Sender as TDocumentSynopsisPromise).Document,
                                      TAttachment.MakeSynopsis,
                                      aError));
end;

procedure TStewProject.DocumentIsFolderError(Sender: TPromise;
  aError: TPromiseError);
begin
  ReportEvent(TDocumentError.Create(paDocumentFolderCheckFailed,
                                    (Sender as TDocumentIsFolderPromise).Document,
                                    aError));
end;

procedure TStewProject.DocumentRenamingError(Sender: TPromise;
  aError: TPromiseError);
begin
  ReportEvent(TDocumentRenameErrorEvent.Create(paDocumentRenamingFailed,
                                          (Sender as TRenameDocumentPromise).Old,
                                          (Sender as TRenameDocumentPromise).New,
                                          aError));
end;

procedure TStewProject.DocumentShifted(Sender: TPromise);
begin
  ReportEvent(TDocumentEvent.Create(paDocumentShifted,
                                    (Sender as TShiftDocumentPromise).Document));
end;

procedure TStewProject.DocumentShiftError(Sender: TPromise;
  aError: TPromiseError);
begin
  ReportEvent(TDocumentError.Create(paDocumentShiftFailed,
                                    (Sender as TShiftDocumentPromise).Document,
                                    aError));
end;

procedure TStewProject.DocumentsRenamed(Sender: TPromise);
begin
  ReportEvent(TDocumentRenameEvent.Create(paDocumentRenamed,
                                          (Sender as TRenameDocumentPromise).Old,
                                          (Sender as TRenameDocumentPromise).New));
end;

procedure TStewProject.DocumentEditing(Sender: TPromise);
begin
  ReportEvent(TAttachmentEditingEvent.Create(paEditingAttachment,
                                      (Sender as TRequestEditingAttachmentPromise).Document,
                                      (Sender as TRequestEditingAttachmentPromise).Attachment,
                                      (Sender as TRequestEditingAttachmentPromise).Cancelled));
end;

procedure TStewProject.DocumentEditingError(Sender: TPromise;
  aError: TPromiseError);
begin
  ReportEvent(TAttachmentError.Create(paAttachmentEditingFailed,
                                      (Sender as TRequestEditingAttachmentPromise).Document,
                                      (Sender as TRequestEditingAttachmentPromise).Attachment,
                                      aError));

end;

constructor TStewProject.Create(const Path: TFile);
begin
  inherited Create;
  fDisk := Path;
  fCache := TFileSystemCache.Create(fDisk.System);
  fCache.OnActivity:=@CacheActivity;
  fCache.OnError:=@CacheError;
  fShadows := TShadowCache.Create(false);
  // fObservers := TProjectObserverList.Create;
  fObservers.Init;

end;

destructor TStewProject.Destroy;
begin
  FreeAndNil(fShadows);
  FreeAndNil(fCache);
  // FreeAndNil(fObservers);
  fObservers.Clear;
  inherited Destroy;
end;

function TStewProject.GetProjectName: String;
begin
  result := fDisk.PacketName;
end;

procedure TStewProject.CreateShadow(aDocument: TDocumentPath);
begin
  fShadows.CreateShadow(aDocument);
  ReportEvent(TDocumentEvent.Create(paShadowCreated,aDocument));
end;

function TStewProject.IsShadow(aDocument: TDocumentPath): Boolean;
begin
  result := fShadows.HasShadow(aDocument);
end;

procedure TStewProject.DeleteShadow(aDocument: TDocumentPath);
begin
  fShadows.DeleteShadow(aDocument);
  ReportEvent(TDocumentEvent.Create(paShadowUncreated,aDocument));
end;

function TStewProject.IsDocumentAFolder(aDocument: TDocumentPath;
  aForceRefresh: Boolean): TDocumentIsFolderPromise;
var
  lFolderFile: TFile;
  lCheckFile: TFileExistencePromise;
begin
  lFolderFile := GetDocumentFolderPath(fDisk,aDocument);
  if aForceRefresh then
    fCache.Uncache(lFolderFile,true);
  lCheckFile := fCache.CheckExistence(lFolderFile);
  result := TIsDocumentAFolderTask.Defer(aDocument,Self,lCheckFile).Promise as TDocumentIsFolderPromise;
  result.Catch(@DocumentIsFolderError);
end;

function TStewProject.ListDocumentsInFolder(aDocument: TDocumentPath;
  aForceRefresh: Boolean): TDocumentListPromise;
var
  lFolderFile: TFile;
  lListFiles: TFileListPromise;
begin
  lFolderFile := GetDocumentFolderPath(fDisk,aDocument);
  if aForceRefresh then
    fCache.Uncache(lFolderFile,true);
  lListFiles := fCache.ListFiles(lFolderFile);
  result := TListDocumentsInFolderTask.Defer(aDocument,Self,lListFiles).Promise as TDocumentListPromise;
  result.After(@DocumentListReceived,@DocumentListError);

end;

function TStewProject.ListAttachmentsForDocument(aDocument: TDocumentPath;
  aForceRefresh: Boolean): TAttachmentListPromise;
var
  lFolderFile: TFile;
  lListFiles: TFileListPromise;
begin
  lFolderFile := GetDocumentFolderPath(fDisk,aDocument.Container);
  if aForceRefresh then
    fCache.Uncache(lFolderFile);
  lListFiles := fCache.ListFiles(lFolderFile);
  result := TListAttachmentsForDocumentTask.Defer(aDocument,Self,lListFiles).Promise as TAttachmentListPromise;
  result.After(@AttachmentListReceived,@AttachmentListError);
end;

function TStewProject.ReadDocumentSynopsis(aDocument: TDocumentPath;
  aForceRefresh: Boolean): TDocumentSynopsisPromise;
var
  lSynFile: TFile;
  lReadFile: TFileReadPromise;
begin
  lSynFile := GetDocumentSynopsisPath(fDisk,aDocument);
  if aForceRefresh then
    fCache.Uncache(lSynFile);
  lReadFile := fCache.ReadFile(lSynFile);
  result := TReadDocumentSynopsisTask.Defer(aDocument,lReadFile).Promise as TDocumentSynopsisPromise;
  // we need to report the data that was received, and if an error occurred.
  result.After(@DocumentSynopsisDataReceived,@DocumentSynopsisReadError);
end;

function TStewProject.WriteDocumentSynopsis(aDocument: TDocumentPath;
  aSynopsis: UTF8String; aBackup: Boolean): TWriteAttachmentPromise;
var
  lWriteFile: TFileWritePromise;
  lOptions: TFileCacheWriteOptions;
begin
  if aBackup then
    lOptions := [fcwoCreateBackupOfOriginal]
  else
    lOptions := [];
  lWriteFile := fCache.WriteFile(GetDocumentSynopsisPath(fDisk,aDocument),lOptions);
  lWriteFile.WriteString(aSynopsis);
  result := TWriteAttachmentTask.Defer(aDocument,
                                       TAttachment.MakeSynopsis,
                                       lWriteFile).Promise as TWriteAttachmentPromise;
  // we only need to catch to make sure the error is reported.
  result.Catch(@AttachmentWriteError);
end;

function TStewProject.ReadDocumentProperties(aDocument: TDocumentPath;
  aForceRefresh: Boolean): TDocumentPropertiesPromise;
var
  lPropFile: TFile;
  lReadFile: TFileReadPromise;
begin
  lPropFile := GetDocumentPropertiesPath(fDisk,aDocument);
  if aForceRefresh then
    fCache.Uncache(lPropFile);
  lReadFile := fCache.ReadFile(lPropFile);
  result := TReadDocumentPropertiesTask.Defer(aDocument,lReadFile).Promise as TDocumentPropertiesPromise;
  // we need to report the data that was received, and if an error occurred.
  result.After(@DocumentPropertiesDataReceived,@DocumentPropertiesReadError);
end;

function TStewProject.WriteDocumentProperties(aDocument: TDocumentPath;
  aProperties: TDocumentProperties; aBackup: Boolean): TWriteAttachmentPromise;
var
  lWriteFile: TFileWritePromise;
  lOptions: TFileCacheWriteOptions;
begin
  if aBackup then
    lOptions := [fcwoCreateBackupOfOriginal]
  else
    lOptions := [];
  lWriteFile := fCache.WriteFile(GetDocumentPropertiesPath(fDisk,aDocument),lOptions);
  ToJSON(aProperties,lWriteFile.Data,'  ');
  result := TWriteAttachmentTask.Defer(aDocument,
                                       TAttachment.MakeProperties,
                                       lWriteFile).Promise as TWriteAttachmentPromise;
  // we only need to catch to make sure the error is reported.
  result.Catch(@AttachmentWriteError);
end;

function TStewProject.ReadProjectProperties(aForceRefresh: Boolean
  ): TProjectPropertiesPromise;
var
  lPropFile: TFile;
  lReadFile: TFileReadPromise;
begin
  lPropFile := GetProjectPropertiesPath(fDisk);
  if aForceRefresh then
    fCache.Uncache(lPropFile);
  lReadFile := fCache.ReadFile(lPropFile);
  result := TReadProjectPropertiesTask.Defer(lReadFile).Promise as TProjectPropertiesPromise;
  // we need to report the data that was received, and if an error occurred.
  result.After(@ProjectPropertiesDataReceived,@ProjectPropertiesReadError);
end;

function TStewProject.WriteProjectProperties(aProperties: TProjectProperties;
  aBackup: Boolean): TWriteProjectPropertiesPromise;
var
  lWriteFile: TFileWritePromise;
  lOptions: TFileCacheWriteOptions;
begin
  if aBackup then
     lOptions := [fcwoCreateBackupOfOriginal]
  else
    lOptions := [];
  lWriteFile := fCache.WriteFile(GetProjectPropertiesPath(fDisk),lOptions);
  ToJSON(aProperties,lWriteFile.Data,'  ');
  result := TWriteProjectPropertiesTask.Defer(lWriteFile).Promise as TWriteProjectPropertiesPromise;
  // we only need to catch to make sure the error is reported.
  result.Catch(@ProjectPropertiesWriteError);
end;

function TStewProject.ShiftDocumentBy(aDocument: TDocumentPath; aDelta: Integer
  ): TShiftDocumentPromise;
begin
  if aDelta = 0 then
    raise Exception.Create('Delta can''t be 0');
  result := ShiftDocumentRelativeTo(aDocument,aDocument,aDelta);
end;

function TStewProject.ShiftDocumentUp(aDocument: TDocumentPath
  ): TShiftDocumentPromise;
begin
  result := ShiftDocumentBy(aDocument,-1);
end;

function TStewProject.ShiftDocumentDown(aDocument: TDocumentPath
  ): TShiftDocumentPromise;
begin
  result := ShiftDocumentBy(aDocument,1);
end;

function TStewProject.ShiftDocumentTo(aDocument: TDocumentPath; aIndex: Integer
  ): TShiftDocumentPromise;
begin
  result := ShiftDocumentRelativeTo(aDocument,TDocumentPath.Null,aIndex);
end;

function TStewProject.ShiftDocumentRelativeTo(aDocument: TDocumentPath;
  aRelative: TDocumentPath; aDelta: Integer): TShiftDocumentPromise;
var
  lFolderFile: TFile;
  lListFiles: TFileListPromise;
begin
  if aDocument = TDocumentPath.Root then
    raise Exception.Create('Can''t shift the document root');
  lFolderFile := GetDocumentFolderPath(fDisk,aDocument.Container);
  lListFiles := fCache.ListFiles(lFolderFile);
  result := TShiftDocumentTask.Defer(aDocument,aDelta,aRelative,Self,lListFiles).Promise as TShiftDocumentPromise;
  result.After(@DocumentShifted,@DocumentShiftError);
end;

function TStewProject.RenameDocument(aDocument: TDocumentPath;
  aNewDocument: TDocumentPath): TRenameDocumentPromise;
var
  lListFiles: TFileListPromise;
  lDocumentFile: TFile;
begin
  if aDocument = TDocumentPath.Root then
    raise Exception.Create('Can''t rename the document root');
  if aNewDocument = TDocumentPath.Root then
    raise Exception.Create('Can''t rename a document to root');
  ReportEvent(TDocumentRenameEvent.Create(paRenamingDocument,aDocument,aNewDocument));
  lDocumentFile := GetDocumentFolderPath(fDisk,aDocument.Container);
  // uncache the document recursively so we make sure we get *all* of the correct files.
  // I don't want to risk their being an extra file lying around that didn't get
  // renamed.
  fCache.Uncache(lDocumentFile,true);
  lListFiles := fCache.ListFiles(lDocumentFile);
  result := TRenameDocumentTask.Defer(aDocument,aNewDocument,Self,lListFiles).Promise as TRenameDocumentPromise;
  result.After(@DocumentsRenamed,@DocumentRenamingError);
end;

function TStewProject.EditDocument(aDocument: TDocumentPath
  ): TRequestEditingAttachmentPromise;
begin
  result := EditDocumentAttachment(aDocument,atPrimary);
end;

function TStewProject.EditDocumentNotes(aDocument: TDocumentPath
  ): TRequestEditingAttachmentPromise;
begin
  result := EditDocumentAttachment(aDocument,atNotes);
end;

function TStewProject.EditDocumentThumbnail(aDocument: TDocumentPath
  ): TRequestEditingAttachmentPromise;
begin
  result := EditDocumentAttachment(aDocument,atThumbnail);
end;

function TStewProject.EditDocumentAttachment(aDocument: TDocumentPath;
  aAttachment: TAttachmentKind): TRequestEditingAttachmentPromise;
var
  lListFiles: TFileListPromise;
begin
  lListFiles := fCache.ListFiles(GetDocumentFolderPath(fDisk,aDocument.Container));
  result := TRequestEditAttachmentTask.Defer(aDocument,aAttachment,Self,lListFiles).Promise as TRequestEditingAttachmentPromise;
  result.After(@DocumentEditing,@DocumentEditingError);
end;

procedure TStewProject.AddObserver(aObserver: TProjectObserver);
begin
  fObservers.Add(aObserver);
end;

procedure TStewProject.RemoveObserver(aObserver: TProjectObserver);
begin
  fObservers.Remove(aObserver);
end;

class function TStewProject.CheckExistenceAndCreate(aPath: TFile): TProjectPromise;
begin
  result := TProjectCheckAtPathTask.Enqueue(aPath).Promise as TProjectPromise;
end;

class function TStewProject.CheckExistenceInParentAndCreate(aPath: TFile): TProjectPromise;
begin
  result := TProjectCheckInParentTask.Enqueue(aPath).Promise as TProjectPromise;
end;

function TStewProject.BuildAndSortDocumentList(
  aParent: TDocumentPath;
  aProperties: TDocumentProperties; aFiles: TFileInfoArray
  ): TDocumentInfoArray;
var
  lBuilder: TDocumentListBuilder;
begin
  lBuilder := TDocumentListBuilder.Create;
  try
    result := lBuilder.Build(aProperties,fDisk,aFiles,fShadows.GetContents(aParent));
  finally
    lBuilder.Free;
  end;

end;

function TStewProject.DoConfirmNewAttachment(aDocument: TDocumentPath;
  aKind: TAttachmentKind): Boolean;
begin
  if FOnConfirmNewAttachment <> nil then
    FOnConfirmNewAttachment(Self,aDocument,TAttachment.AttachmentKindStrings[aKind],Result)
  else
    raise Exception.Create('User has no way to confirm new attachment');
end;

function TStewProject.DoChooseTemplate(aDocument: TDocumentPath;
  aKind: TAttachmentKind; aList: TTemplateArray; out aChosen: TTemplate
  ): Boolean;
var
  l: Integer;
  i: Integer;
  lNames: TStringArray;
begin
  if fOnChooseTemplate <> nil then
  begin
    l := Length(aList);
    SetLength(lNames,l);
    for i := 0 to l - 1 do
    begin
      lNames[i] := aList[i].Name;
    end;
    fOnChooseTemplate(Self,aDocument,TAttachment.AttachmentKindStrings[aKind],lNames,i,Result);
    if result then
    begin
      if (i >= 0) and (i < l) then
        aChosen := aList[i]
      else
        raise Exception.Create('Invalid choice for template');
    end;

  end
  else
     raise Exception.Create('User has no way to choose templates');
end;

function TStewProject.DoChooseFileForAttachment(aDocument: TDocumentPath;
  aKind: TAttachmentKind; aChoices: TFileArray; out aChosen: TFile): Boolean;
var
  l: Integer;
  i: Integer;
  lNames: TStringArray;
begin
  if fOnChooseAttachment <> nil then
  begin
    l := Length(aChoices);
    SetLength(lNames,l);
    for i := 0 to l - 1 do
    begin
      lNames[i] := aChoices[i].Name;
    end;
    fOnChooseAttachment(Self,aDocument,TAttachment.AttachmentKindStrings[aKind],lNames,i,Result);
    if result then
    begin
      if (i >= 0) and (i < l) then
        aChosen := aChoices[i]
      else
        raise Exception.Create('Invalid choice for attachment file');
    end;

  end
  else
     raise Exception.Create('User has no way to choose from multiple attachments');
end;

class function TStewProject.GetProjectPropertiesPath(aFile: TFile): TFile;
begin
  result := aFile.GetContainedFile('',ProjectPropertiesDescriptor,PropertiesExtension,false);
end;

class function TStewProject.GetDocumentPropertiesPath(aProjectPath: TFile;
  aDocument: TDocumentPath): TFile;
begin
  result := aDocument.ToFile(aProjectPath,DocumentPropertiesDescriptor,PropertiesExtension);
end;

class function TStewProject.GetDocumentSynopsisPath(aProjectPath: TFile;
  aDocument: TDocumentPath): TFile;
begin
  result := aDocument.ToFile(aProjectPath,DocumentSynopsisDescriptor,DocumentSynopsisExtension);
end;

class function TStewProject.GetDocumentFolderPath(aProjectPath: TFile;
  aDocument: TDocumentPath): TFile;
begin
  result := aDocument.ToFile(aProjectPath,'','');
end;

end.

