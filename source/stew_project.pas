unit stew_project;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, sys_file, sys_async, stew_properties, stew_types, contnrs, sys_filecache, fgl;

{
TODO: Next: Implement project properties, then test that, then implement document
properties, then test that, then convert the GUI into using them both (since they both
have an effect on the JSON Editor GUI control). Then, work on the other actions.
see test_stew_project.

TODO: Something I might add later, if I feel it's necessary. Loading should have a refresh mode, I think I've put this elsewhere:
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

{

TODO: Events: Instead of having a separate event for each type of thing, create
a list that observers can add and remove themselves from. These are
method pointers, the method signature looking something like:

TProjectObserver = procedure(Sender: TProject; Event: TProjectEvent) of object;

Most of the events are emitted when the OnActivity and OnError events occur in the
file system cache. The promises returned are processed to determine the action
and the file it occurs on, and this is converted to document/attachment info
based on name, descriptor and extension for creating the event.

There are a few events which are emitted by the project itself based on other
evidence, because the events aren't knowable to the file cache.

The TProjectEvent object will have a flag indicating the type, for use in
case statements, but the data it contains will depend on the class, although
the same class is used for various types.

It will also have a boolean flag 'IsError', since this is an important distinction
that might need to be filtered out.

The classes of events are:

TProjectEvent: This is a simple notification, no parameters are necessary because
the only data of importance is the project itself.
=====================
ProjectPropertiesLoading: - triggered when a call is made to load the project
                            properties. UI can disable or indicate progress.
ProjectPropertiesSaving: - triggered when a call is made to save the properties.
                           UI can indicate progress.
ProjectPropertiesSaved: - triggered when a property save is finished. UI can indicate
                          progress, or trigger a reload to get the new data.
ProjectPropertiesSaveConflict - triggered if there is a conflict while saving properties.
                          While this is technically an error, it requires no error
                          message, so it is still a basic notification.

TProjectPropertiesLoaded (TProjectEvent):
Adds the project properties that were loaded, for reference
==============================
ProjectPropertiesLoaded: - triggered when a property load is
                           finished. UI can indicate progress, update controls or
                           check for data conflicts.

TProjectError (TProjectEvent):
These notifications will contain an error message. These are mostly useful for
reporting errors, but occasionally UI needs to be enabled to indicate completion
of activity.
==========================
ProjectPropertiesLoadError - triggered if an error occurrs while loading properties.
ProjectPropertiesSaveError - triggered if an error ocurrs while saving properties.

TDocumentEvent (TProjectEvent):
Contains a Document property which indicates the TDocumentPath this is relevant to.
=======================
DocumentListing - matching opposite is a TDocumentListedEvent.
DocumentCreated
DocumentDisappeared - might occur if, on a listing, the document suddenly doesn't
exist anymore, the editor can convert the document to "new" instead. This is
going to be different from a potential DocumentDeleted, which would and should be
cancellable. This is actually an error.

TDocumentRenameEvent (TDocumentEvent)
contains a second document property to indicate what the rename was too
=======================
DocumentRenaming
DocumentRenamed

TDocumentListedEvent (TDocumentEvent)
contains a list of files returned from listing the event
=======================
DocumentListed

TDocumentError (TDocumentEvent)
Adds an Error message.
=============================
DocumentListError

TDocumentRenameError (TDocumentRenameEvent)
==============================
DocumentRenameError

TAttachmentEvent (TDocumentEvent)
Adds an 'Attachment' property, which is an enum indicating which attachments
are involved here. Also an extension property in case there are "multiple" extensions.
=========================
AttachmentLoading
AttachmentSaving
AttachmentSaved
AttachmentDisappeared (See DocumentDisappeared)
AttachmentSaveConflict
AttachmentEditing - This doesn't have a corresponding AttachmentEdited because
we have no universal way of knowing when a document is done being edited. Some
processes return after launching a separate editor for the editor window, some
processes don't lock files while they're being edited, or lock them in some
non-standard way that only they know about.

TAttachmentLoaded (TAttachmentEvent):
-- Actually a different type depending on the type of attachment
========================
AttachmentLoaded

TAttachmentError (TAttachmentEvent)
Adds an error message on.
===============================
AttachmentLoadError
AttachmentSaveError

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
    class function FromString(const aPath: UTF8String): TDocumentPath; static;
    const Root: TDocumentPath = ( fID: '/');
    const Null: TDocumentPath = ( fID: '');
    class function GetSystemDocument(aName: UTF8String): TDocumentPath; static;
  end;

  TDocumentArray = array of TDocumentPath;

  TDocumentInfo = record
    Document: TDocumentPath;
    IsFolder: Boolean;
  end;

  TDocumentInfoArray = array of TDocumentInfo;

  // TODO: There are two "Loaded" events: *Loaded/*Listed and *DataReceived
  // the first is only called when the cache reports an actual file read (as opposed
  // to just reading from the data cache). It does not contain any of the data in
  // the event, it is only a notification for bystanders to know that a task
  // has been finished.
  // the second is called whenever a call to load the data is completed on the
  // project, whether the actual file cache had to be read or not. It is meant
  // as a way for some observer who is passively interested in the data to be
  // able to view the actual data. While it does mean that these observers get
  // a lot of data passed to them that they didn't necessarily request, it
  // does mean that they don't have to call load again to get the data after being
  // notified that it has been loaded. The alternative is to convert the file
  // system info into data twice -- once for the promise and once for the loaded
  // event.
  // TODO: There are two "Failed" events: *File*Failed and **Failed.
  // This is mainly a feature to ensure that no errors are missed. The first
  // one happens when there are errors in the file cache itself. The second includes
  // errors such as parsing data, indicates saving conflicts, etc. and are handled by
  // the initial promises themselves. UI error reporting *should* be done for the second
  // one only, as it is global, but the other might be important to watch for as well.
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
     paDocumentFolderCheckFailed,
     paDocumentCreated, // TODO: I'm not doing this... am I?
     paDocumentShifted,
     paDocumentShiftFailed,
     paDocumentDisappeared,
     paRenamingDocument,
     paDocumentRenamed,
     paDocumentRenamingFailed,
     paAttachmentDataReceived,
     paAttachmentLoadingFailed,
     paAttachmentSavingFailed,
     paAttachmentSaveConflictOccurred,
     paAttachmentDisappeared,
     paEditingAttachment);

  { TProjectEvent }

  TProjectEvent = class(TObject)
  private
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
     'DocumentFolderCheckFailed',
     'DocumentCreated',
     'DocumentShifted',
     'DocumentShiftFailed',
     'DocumentDisappeared',
     'RenamingDocument',
     'DocumentRenamed',
     'DocumentRenamingFailed',
     'AttachmentDataReceived',
     'AttachmentLoadingFailed',
     'AttachmentSavingFailed',
     'AttachmentSaveConflictOccurred',
     'AttachmentDisappeared',
     'EditingAttachment');

  end;



  TStewProject = class;

  TProjectObserver = procedure(Sender: TStewProject; Event: TProjectEvent) of object;
  TProjectObserverList = specialize TFPGList<TProjectObserver>;

  { TProjectPropertiesDataReceivedEvent }

  TProjectPropertiesDataReceivedEvent = class(TProjectEvent)
  private
    fProperties: TProjectProperties2;
  public
    constructor Create(aProperties: TProjectProperties2);
    property Properties: TProjectProperties2 read fProperties;
  end;

  { TProjectError }

  TProjectError = class(TProjectEvent)
  private
    fError: TPromiseError;
  public
    constructor Create(aAction: TProjectEventKind; aError: TPromiseError);
    function GetDescription: UTF8String; override;
  end;

  { TDocumentEvent }

  TDocumentEvent = class(TProjectEvent)
  private
    fDocument: TDocumentPath;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentListedEvent }

  { TDocumentListDataReceivedEvent }

  TDocumentListDataReceivedEvent = class(TDocumentEvent)
  private
    fList: TDocumentInfoArray;
  public
    constructor Create(aDocument: TDocumentPath; aList: TDocumentInfoArray);
    property List: TDocumentInfoArray read FList;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentError }

  TDocumentError = class(TDocumentEvent)
  private
    fError: TPromiseError;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aError: TPromiseError);
    property Error: TPromiseError read fError;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentRenameEvent }

  TDocumentRenameEvent = class(TDocumentEvent)
  private
    fNewDocument: TDocumentPath;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
      aNewDocument: TDocumentPath);
    property NewDocument: TDocumentPath read fNewDocument;
    function GetDescription: UTF8String; override;
  end;

  { TDocumentRenameErrorEvent }

  TDocumentRenameErrorEvent = class(TDocumentRenameEvent)
  private
    fError: TPromiseError;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aNewDocument: TDocumentPath; aError: TPromiseError);
    property Error: TPromiseError read fError;
    function GetDescription: UTF8String; override;
  end;

  // TODO: What else?
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
  end;

  { TAttachmentEvent }

  TAttachmentEvent = class(TDocumentEvent)
  private
    fAttachment: TAttachment;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aAttachment: TAttachment);
    property Attachment: TAttachmentKind read fAttachment.Kind;
    property Extension: UTF8String read fAttachment.Extension;
    property Descriptor: UTF8String read fAttachment.Descriptor;
    function GetDescription: UTF8String; override;
    const
      AttachmentKindStrings: array[TAttachmentKind] of UTF8String =
    ('unknown',
     'primary',
     'folder',
     'properties',
     'notes',
     'thumbnail',
     'synopsis',
     'backup'
    );
  end;

  { TDocumentPropertiesDataReceived }

  { TDocumentPropertiesDataReceivedEvent }

  TDocumentPropertiesDataReceivedEvent = class(TAttachmentEvent)
  private
    fProperties: TDocumentProperties2;
  public
    constructor Create(aDocument: TDocumentPath; aProperties: TDocumentProperties2);
    property Properties: TDocumentProperties2 read fProperties;
  end;

  { TDocumentSynopsisDataReceived }

  TDocumentSynopsisDataReceived = class(TAttachmentEvent)
  private
    fSynopsis: UTF8String;
  public
    constructor Create(aDocument: TDocumentPath; aSynopsis: UTF8String);
    property Synopsis: UTF8String read fSynopsis;
  end;

  { TAttachmentError }

  TAttachmentError = class(TAttachmentEvent)
  private
    fError: TPromiseError;
  public
    constructor Create(aAction: TProjectEventKind; aDocument: TDocumentPath;
       aAttachment: TAttachment; aError: TPromiseError);
    property Error: TPromiseError read fError;
    function GetDescription: UTF8String; override;
  end;

  // TODO: These are old events, which are going to be gone when I've got things figured out.
  TDocumentNotifyEvent = procedure(Sender: TObject; Document: TDocumentPath) of object;
  TDocumentExceptionEvent = procedure(Sender: TObject; Document: TDocumentPath; Error: String) of object;
  TAttachmentNotifyEvent = procedure(Sender: TObject; Document: TDocumentPath; AttachmentName: String) of object;
  TAttachmentExceptionEvent = procedure(Sender: TObject; Document: TDocumentPath; AttachmentName: String; Error: String) of object;
  TAttachmentConfirmationEvent = procedure(Sender: TObject; Document: TDocumentPath; AttachmentName: String; out Answer: Boolean) of object;
  TAttachmentChoiceEvent = procedure(Sender: TObject; Document: TDocumentPath; AttachmentName: String; aChoices: TStringArray; var Answer: String; out Accepted: Boolean) of object;

  TOrderDocumentPosition = (odpBefore,odpAfter);

  {TODO: Old cache stuff starts here. Get rid of it once I've gotten the new stuff in.}

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
    procedure FileSaveConflictCheck(aSender: TPromise; aData: TPromiseError);
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

  { TDocumentMetadata }
  // FUTURE: Currently, there's no way to *release* the cache, mostly because
  // I don't have any way for the project to know when cached data is no longer
  // needed (properties are needed by at least two UI areas: listing documents and
  // editing the properties). If this program gets unwieldy with large projects that
  // remain open for a long time, that would be the place to look.
  // -- I could just, periodically, remove any properties that are not marked
  // as modified. But it would be nicer to remove only those properties which
  // are not being modified in a tab.
  // -- switching to a request/response system like a browser would solve this
  // as well: if the data is in cache, return it, if not, load it, and don't
  // keep pointers to the metadata objects either, just use the data and assume
  // it may be deleted later.
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
    fID: TDocumentPath;
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
      aID: TDocumentPath; aIsRoot: Boolean);
    destructor Destroy; override;
    property Properties: TDocumentProperties read fProperties;
    // This isn't a true recursive. It is more of a recursive refresh that doesn't
    // cause new documents to appear. It will list the current document, yes.
    // But it will only cause children docs to list themselves if they have already
    // been listed.
    procedure ListDocuments(Recursive: Boolean);
    property ListingState: TListingState read fListingState;
    function GetContents: TDocumentArray;
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


  {TODO: New cache stuff starts here.}

  { TCachedItem }

  TCachedData = class(TObject)
  private
    fCreated: TDateTime;
  public
    constructor Create;
    property Created: TDateTime read fCreated;
  end;

  { TCachedFileData }

  TCachedFileData = class(TCachedData)
  private
    fPath: TFile;
  public
    constructor Create(const aPath: TFile);
    property Path: TFile read fPath;
  end;

  TAttachmentArray = array of TCachedFileData;

  { TCachedDocument }

  TCachedDocument = class(TCachedData)
  private
    fAttachments: TFPHashObjectList;
    fName: UTF8String;
  public
    constructor Create(const aName: UTF8String);
    destructor Destroy; override;
    function GetAttachment(const aDescriptor: UTF8String; aExtension: UTF8String): TCachedFileData;
    procedure PutAttachment(const aDescriptor: UTF8String; aExtension: UTF8String; aValue: TCachedFileData);
    function ListAttachments(const aDescriptor: UTF8String): TAttachmentArray;
    property Attachments[const aDescriptor: UTF8String; aExtension: UTF8String]: TCachedFileData read GetAttachment write PutAttachment; default;
    property Name: UTF8String read fName;
  end;

  TCachedDocumentArray = array of TCachedDocument;

  { TCachedDirectory }

  TCachedDirectory = class(TCachedFileData)
  private
    fDocuments: TFPHashObjectList;
  public
    constructor Create(const aPath: TFile);
    destructor Destroy; override;
    function GetDocument(const aName: UTF8String): TCachedDocument;
    procedure PutDocument(const aName: UTF8String; aValue: TCachedDocument);
    function ListDocumentNames: TStringArray;
    function ListDocuments: TCachedDocumentArray;
    property Documents[const aName: UTF8String]: TCachedDocument read GetDocument write PutDocument; default;
  end;

  { TCachedFilePromise }

  TCachedFilePromise = class(TCachedFileData)
  private
    fPromise: TPromise;
  public
    constructor Create(const aPath: TFile; aPromise: TPromise);
    property Promise: TPromise read fPromise;
  end;

  { TCachedProjectData }

  TCachedProjectData = class(TCachedData)
  private
    // Basically, I'm wrapping a directory and an attachment list, since the
    // project root keeps it's attachments alongside it's document contents, so
    // it would be incorrect to call the directory itself an attachment.
    // This is a sort of kludge, but I don't want to rewrite the code for
    // these objects. A mixin would be better, but at least I only need one
    // of these per application. I could use interfaces and implement them
    // with internal objects, but this shouldn't have to be an interface
    // because I don't care about type-compatibility with it.
    fRootDirectory: TCachedDirectory;
    fProjectAttachments: TCachedDocument;
    function GetName: UTF8String;
    function GetPath: TFile;
  public
    constructor Create(const aPath: TFile; const aName: UTF8String);
    destructor Destroy; override;
    function GetDocument(const aName: UTF8String): TCachedDocument;
    procedure PutDocument(const aName: UTF8String; aValue: TCachedDocument);
    function ListDocumentNames: TStringArray;
    function ListDocuments: TCachedDocumentArray;
    property Documents[const aName: UTF8String]: TCachedDocument read GetDocument write PutDocument; default;
    function GetAttachment(const aDescriptor: UTF8String; aExtension: UTF8String): TCachedFileData;
    procedure PutAttachment(const aDescriptor: UTF8String; aExtension: UTF8String; aValue: TCachedFileData);
    function ListAttachments(const aDescriptor: UTF8String): TAttachmentArray;
    property Attachments[const aDescriptor: UTF8String; aExtension: UTF8String]: TCachedFileData read GetAttachment write PutAttachment;
    property Name: UTF8String read GetName;
    property Path: TFile read GetPath;
  end;

  { TCachedAttachmentData }

  generic TCachedAttachmentData<DataType> = class(TCachedFileData)
  private
    fData: DataType;
  public
    constructor Create(const aPath: TFile; aProperties: DataType);
    property Data: DataType read fData;
  end;

  { TCachedAttachmentObject }

  generic TCachedAttachmentObject<DataType> = class(specialize TCachedAttachmentData<DataType>)
  public
    // We own the data, so we have to destroy it. Since the superior generic can
    // allow primitives, we have to override and create a new generic.
    destructor Destroy; override;
  end;


  { TCachedDocumentProperties }

  TCachedDocumentProperties = specialize TCachedAttachmentObject<TDocumentProperties2>;

  { TCachedProjectProperties }

  TCachedProjectProperties = specialize TCachedAttachmentObject<TProjectProperties2>;

  TCachedSynopsis = specialize TCachedAttachmentData<UTF8String>;


  { TShadowDocument }

  TShadowDocument = class
  private
    fContents: TFPHashObjectList;
  protected
    function CreatePath(aPath: UTF8String): TShadowDocument;
    function GetIsFolder: Boolean;
    function GetPath(aPath: UTF8String): TShadowDocument;
    procedure DeletePath(aPath: UTF8String);
    procedure SplitPath(aPath: UTF8String; out aChildName: UTF8String; out aChildPath: UTF8String);
  public
    constructor Create;
    destructor Destroy; override;
    property IsFolder: Boolean read GetIsFolder;
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
  private
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
  private
    fProperties: TProjectProperties2;
    procedure LoadAnswer(aStream: TStream);
  public
    destructor Destroy; override;
    property Properties: TProjectProperties2 read fProperties;
  end;

  TWriteProjectPropertiesPromise = class(TPromise)
  private
    fIsConflict: Boolean;
  public
    property IsConflict: Boolean read fIsConflict;
  end;

  { TWriteAttachmentPromise }

  TWriteAttachmentPromise = class(TPromise)
  private
    fIsConflict: Boolean;
    fDocument: TDocumentPath;
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
  private
    fProperties: TDocumentProperties2;
    fDocument: TDocumentPath;
    procedure LoadAnswer(aStream: TStream);
  public
    constructor Create(aDocument: TDocumentPath);
    destructor Destroy; override;
    property Properties: TDocumentProperties2 read fProperties;
    property Document: TDocumentPath read fDocument;
  end;

  { TDocumentSynopsisPromise }

  TDocumentSynopsisPromise = class(TPromise)
  private
    fSynopsis: UTF8String;
    fDocument: TDocumentPath;
    procedure LoadAnswer(aText: UTF8String);
  public
    constructor Create(aDocument: TDocumentPath);
    property Synopsis: UTF8String read fSynopsis;
    property Document: TDocumentPath read fDocument;
  end;

  { TDocumentListPromise }

  TDocumentListPromise = class(TPromise)
  private
    fDocument: TDocumentPath;
    fList: TDocumentInfoArray;
  public
    constructor Create(aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
    property List: TDocumentInfoArray read fList;
  end;

  { TDocumentIsFolderPromise }

  TDocumentIsFolderPromise = class(TPromise)
  private
    fDocument: TDocumentPath;
    fIsFolder: Boolean;
  public
    constructor Create(aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
    property IsFolder: Boolean read fIsFolder;
  end;

  { TShiftDocumentPromise }

  TShiftDocumentPromise = class(TPromise)
  private
    fDocument: TDocumentPath;
  public
    constructor Create(aDocument: TDocumentPath);
    property Document: TDocumentPath read fDocument;
  end;

  { TRenameDocumentPromise }

  TRenameDocumentPromise = class(TPromise)
  private
    fOldDocument: TDocumentPath;
    fNewDocument: TDocumentPath;
  public
    constructor Create(aOldDocument: TDocumentPath; aNewDocument: TDocumentPath);
    property Old: TDocumentPath read fOldDocument;
    property New: TDocumentPath read fNewDocument;
  end;

  { TStewProject }

  TStewProject = class
    procedure DocumentRenamingError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentShifted(Sender: TPromise);
    procedure DocumentShiftError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentsRenamed(Sender: TPromise);
  strict private type

    // this allows me access to the protected events on Project Properties,
    // but avoids making those events accessible from outside of the project,
    // which I don't want.
    // TODO: Get rid of this eventually.
    TProtectedProjectProperties = class(TProjectProperties)
    end;

    { TProjectOpenTask }

    TProjectOpenTask = class(TQueuedTask)
    protected
      fPath: TFile;
    protected
      procedure ProjectOpened(Sender: TPromise);
      procedure ResolveCreateProject(aPath: TFile);
      function CreatePromise: TPromise; override;
      property Path: TFile read fPath;
    public
      constructor Enqueue(aPath: Tfile);
    end;

    { TProjectOpenAtPathTask }

    TProjectOpenAtPathTask = class(TProjectOpenTask)
    private
      procedure FileExists(Sender: TPromise);
    protected
      procedure DoTask; override;
    end;

    { TProjectOpenInParentTask }

    TProjectOpenInParentTask = class(TProjectOpenTask)
    private
      procedure FileExistsInParent(Sender: TPromise);
      procedure FileExists(Sender: TPromise);
    protected
      procedure DoTask; override;
    end;

    { TProjectCreateNewTask }

    TProjectCreateNewTask = class(TProjectOpenTask)
    protected
      procedure DoTask; override;
    end;

    { TWriteProjectPropertiesTask }

    TWriteProjectPropertiesTask = class(TDeferredTask2)
    protected
      procedure HandleError(Input: TPromise; Error: TPromiseError); override;
      procedure DoTask({%H-}Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aPromise: TFileWritePromise);
    end;

    { TReadProjectPropertiesTask }

    TReadProjectPropertiesTask = class(TDeferredTask2)
    protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aPromise: TFileReadPromise);
    end;

    { TWriteAttachmentTask }

    TWriteAttachmentTask = class(TDeferredTask2)
    private
      fDocument: TDocumentPath;
      fAttachment: TAttachment;
    protected
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

    TReadDocumentPropertiesTask = class(TDeferredTask2)
    private
      fDocument: TDocumentPath;
    protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aDocument: TDocumentPath; aPromise: TFileReadPromise);
    end;

    { TReadDocumentSynopsisTask }

    TReadDocumentSynopsisTask = class(TDeferredTask2)
    private
      fDocument: TDocumentPath;
    protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aDocument: TDocumentPath; aPromise: TFileReadPromise);
    end;

    { TListDocumentsInFolderTask }

    TListDocumentsInFolderTask = class(TDeferredTask2)
    private
      fProject: TStewProject;
      fDocument: TDocumentPath;
      fFiles: TFileInfoArray;
    protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
      procedure PropertiesReceived(Sender: TPromise);
    public
      constructor Defer(aDocument: TDocumentPath; aProject: TStewProject; aPromise: TFileListPromise);
    end;

    { TDocumentListBuilder }

    TDocumentListBuilder = class(TObject)
    private
      fIndex: TStringList;
      function SortDocuments(List: TStringList; Index1, Index2: Integer): Integer;
    public
      constructor Create;
      function Build(aProperties: TDocumentProperties2;
        aBasePath: TFile; aFiles: TFileInfoArray; aShadows: TDocumentInfoArray
  ): TDocumentInfoArray;
      destructor Destroy; override;
    end;

    { TIsDocumentAFolderTask }

    TIsDocumentAFolderTask = class(TDeferredTask2)
    private
      fDocument: TDocumentPath;
      fProject: TStewProject;
    protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aDocument: TDocumentPath; aProject: TStewProject; aPromise: TFileExistencePromise);
    end;

    { TShiftDocumentByTask }

    TShiftDocumentByTask = class(TDeferredTask2)
    private
      fProject: TStewProject;
      fDocument: TDocumentPath;
      fFiles: TFileInfoArray;
      fDelta: Integer;
    protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
      procedure PropertiesReceived(Sender: TPromise);
      procedure PropertiesWritten(Sender: TPromise);
    public
      constructor Defer(aDocument: TDocumentPath; aProject: TStewProject; aDelta: Integer; aPromise: TFileListPromise);
    end;

    { TRenameDocumentTask }

    TRenameDocumentTask = class(TDeferredTask2)
      procedure FilesRenamed(Sender: TPromise);
    private
      fProject: TStewProject;
      fOldDocument: TDocumentPath;
      fNewDocument: TDocumentPath;
    protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aOld: TDocumentPath; aNew: TDocumentPath; aProject: TStewProject; aInputPromise: TFileListPromise);
    end;

  private
    fDisk: TFile;
    fCache: TFileSystemCache;
    fShadows: TShadowCache;
    fObservers: TProjectObserverList;

    // TODO: A lot of the rest are going away once everything's moved over
    // to updated promises and jsvalues.

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
    // TODO: New stuff... A bunch of the stuff above is deprecated.
    procedure ReportEvent(aEvent: TProjectEvent);
    procedure CacheActivity(Sender: TPromise);
    procedure CacheError(Sender: TPromise; aError: TPromiseError);
    procedure ProjectPropertiesDataReceived(Sender: TPromise);
    procedure ProjectPropertiesReadError(Sender: TPromise; aError: TPromiseError);
    procedure ProjectPropertiesWriteError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentListError(Sender: TPromise; aError: TPromiseError);
    procedure DocumentListReceived(Sender: TPromise);
    procedure DocumentPropertiesDataReceived(Sender: TPromise);
    procedure DocumentPropertiesReadError(Sender: TPromise;
      aError: TPromiseError);
    procedure AttachmentWriteError(Sender: TPromise;
      aError: TPromiseError);
    procedure DocumentSynopsisDataReceived(Sender: TPromise);
    procedure DocumentSynopsisReadError(Sender: TPromise; aError: TPromiseError
      );
    procedure DocumentIsFolderError(Sender: TPromise; aError: TPromiseError);
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
    // TODO: ConfirmNewAttachment and ChooseTemplate are not meant
    // for the observers to see, they are meant for attaching the UI to.
    property OnConfirmNewAttachment: TAttachmentConfirmationEvent read FOnConfirmNewAttachment write FOnConfirmNewAttachment;
    property OnChooseTemplate: TAttachmentChoiceEvent read FOnChooseTemplate write FOnChooseTemplate;
  public
    constructor Create;
    destructor Destroy; override;
    property DiskPath: TFile read fDisk;
    function GetDocument(const aDocumentID: TDocumentPath): TDocumentMetadata;
    property IsOpened: Boolean read GetIsOpened;
    function GetProjectName: String;
    property Properties: TProjectProperties read GetProperties;

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
    function IsShadow(aDocument: TDocumentPath): Boolean;
    // Removes the "shadow" from the list of documents.
    procedure DeleteShadow(aDocument: TDocumentPath);

    function IsDocumentAFolder(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TDocumentIsFolderPromise;
    function ListDocumentsInFolder(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TDocumentListPromise;
    function ReadDocumentSynopsis(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TDocumentSynopsisPromise;
    function WriteDocumentSynopsis(aDocument: TDocumentPath; aSynopsis: UTF8String): TWriteAttachmentPromise;
    function ReadDocumentProperties(aDocument: TDocumentPath; aForceRefresh: Boolean = false): TDocumentPropertiesPromise;
    function WriteDocumentProperties(aDocument: TDocumentPath; aProperties: TDocumentProperties2): TWriteAttachmentPromise;
    function ReadProjectProperties(aForceRefresh: Boolean = false): TProjectPropertiesPromise;
    function WriteProjectProperties(aProperties: TProjectProperties2
      ): TWriteProjectPropertiesPromise;
    function ShiftDocumentBy(aDocument: TDocumentPath; aDelta: Integer): TShiftDocumentPromise;
    function ShiftDocumentUp(aDocument: TDocumentPath): TShiftDocumentPromise;
    function ShiftDocumentDown(aDocument: TDocumentPath): TShiftDocumentPromise;
    function RenameDocument(aDocument: TDocumentPath; aNewDocument: TDocumentPath): TRenameDocumentPromise;


    // Important maintenance functions for working with the projects themselves.
    procedure AddObserver(aObserver: TProjectObserver);
    procedure RemoveObserver(aObserver: TProjectObserver);
    class function Open(aPath: TFile): TProjectPromise;
    class function OpenInParent(aPath: TFile): TProjectPromise;
    class function CreateNew(aPath: TFile): TProjectPromise;
    // utility functions used internally, but here for your pleasure since they
    // don't change anything in the project state.
    function BuildAndSortDocumentList(aParent: TDocumentPath;
      aProperties: TDocumentProperties2; aFiles: TFileInfoArray
  ): TDocumentInfoArray;
    class function TranslateFileToDocument(aBasePath: TFile; aFile: TFile): TDocumentPath;
    class function TranslateFileToAttachment(aBasePath: TFile; aFile: TFile): TAttachment;
    class function TranslateDocumentAttachmentToFile(aBasePath: TFile; aDocument: TDocumentPath; aDescriptor: UTF8String; aExtension: UTF8String): TFile;
    class function GetProjectPropertiesPath(aFile: TFile): TFile;
    class function GetDocumentPropertiesPath(aProjectPath: TFile; aDocument: TDocumentPath): TFile;
    class function GetDocumentSynopsisPath(aProjectPath: TFile; aDocument: TDocumentPath): TFile;
    class function GetDocumentFolderPath(aProjectPath: TFile; aDocument: TDocumentPath): TFile;
    const
      ProjectPropertiesDescriptor = 'stew';
      DocumentPropertiesDescriptor = 'properties';
      PropertiesExtension = 'json';
      DocumentSynopsisDescriptor = 'synopsis';
      DocumentSynopsisExtension = 'txt';
      DocumentNotesDescriptor = 'notes';
      DocumentThumbnailDescriptor = 'thumbnail';
      DocumentBackupDescriptorPrefix = 'backup-';
  end;


  function ExcludeLeadingSlash(Const Path: UTF8String): UTF8String;
  function IncludeLeadingSlash(Const Path : UTF8String) : UTF8String;
  function IncludeTrailingSlash(Const Path : UTF8String) : UTF8String;
  function ExcludeTrailingSlash(Const Path: UTF8String): UTF8String;

  operator = (a: TDocumentPath; b: TDocumentPath): Boolean;

  const
    AlwaysForbiddenNameCharacters: set of char = [#0..#$1F,#$7F,'<','>',':','"','/','\','|','?','*','%','[',']','~','{','}',';'];
    WhitespaceCharacters: set of char = [' ',#$A0];//,#$1680,#$180e,#$2000..#$200A,#$2028,#$2029,#$202F,#$3000]


implementation

uses
  sys_json;

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

operator=(a: TDocumentPath; b: TDocumentPath): Boolean;
begin
  result := CompareText(a.ID,b.ID) = 0;
end;

function SimpleIndexOf(List: TStrings; aText: String): Integer; inline;
begin
  // TStringList.IndexOf has a little bit of overhead that I don't want
  // and returns -1 for not found, when I want the final value.
  result:=0;
  While (result < List.Count) and (CompareText(list[Result],aText) <> 0) do
    result:=result+1;
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
    if fProject.TranslateFileToDocument(fProject.fDisk,lListedFiles[i]) = fOldDocument then
    begin
      SetLength(lOldFiles,lRenameLength + 1);
      SetLength(lNewFiles,lRenameLength + 1);
      lOldFiles[lRenameLength] := lListedFiles[i];
      lNewFiles[lRenameLength] :=
          TranslateDocumentAttachmentToFile(fProject.fDisk,
                                            fNewDocument,
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

{ TStewProject.TShiftDocumentByTask }

procedure TStewProject.TShiftDocumentByTask.PropertiesWritten(Sender: TPromise);
begin
  Resolve;
end;

procedure TStewProject.TShiftDocumentByTask.DoTask(Input: TPromise);
var
  lReadProperties: TDocumentPropertiesPromise;
begin
  fFiles := (Input as TFileListPromise).FilesInfo;
  // we need the properties as well...
  lReadProperties := fProject.ReadDocumentProperties(fDocument.Container,false);
  lReadProperties.After(@PropertiesReceived,@SubPromiseRejected);
end;

function TStewProject.TShiftDocumentByTask.CreatePromise: TPromise;
begin
  result := TShiftDocumentPromise.Create(fDocument);
end;

procedure TStewProject.TShiftDocumentByTask.PropertiesReceived(Sender: TPromise
  );
var
  lDirectory: TDocumentPath;
  lProps: TDocumentProperties2;
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
    for i := 0 to l - 1 do
    begin
      if lDocuments[i].Document = fDocument then
      begin
        lIndex := i;
        break;
      end;
    end;

    if lIndex <> -1 then
    begin
      lNewIndex := lIndex + fDelta;
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
      fProject.WriteDocumentProperties(lDirectory,lProps).After(@PropertiesWritten,@SubPromiseRejected);
    end
    else
    // else, the document doesn't exist yet, so it doesn't make any sense to
    // add it.... right? But, we do need to resolve...
      Resolve;
  except on E: Exception do
    Reject(E.Message);
  end;
end;

constructor TStewProject.TShiftDocumentByTask.Defer(aDocument: TDocumentPath;
  aProject: TStewProject; aDelta: Integer; aPromise: TFileListPromise);
begin
  fDocument := aDocument;
  fProject := aProject;
  fDelta := aDelta;
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

function TStewProject.TDocumentListBuilder.Build(aProperties: TDocumentProperties2;
  aBasePath: TFile; aFiles: TFileInfoArray; aShadows: TDocumentInfoArray): TDocumentInfoArray;
var
  lList: TEZSortStringList;
  lFolders: TStringList;
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


      // now, start adding the files as documents...
      l := Length(aFiles);
      for i := 0 to l - 1 do
      begin
        lDocument := TranslateFileToDocument(aBasePath,aFiles[i].Item);
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
        lList.Add(lDocument.ID);
        if lIsFolder then
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
        Result[i] := lInfo;
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
begin
  result := GetPath(aDocument.ID) <> nil;
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
    // TODO: Do we need to keep the name on the object?
    lChildShadow := TShadowDocument.Create;
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

constructor TShadowDocument.Create;
begin
  inherited Create;
  fContents := TFPHashObjectList.Create(true);
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

  except on E: Exception do
    Reject(E.Message);
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
    fProperties := FromJSON(TDocumentProperties2,aStream) as TDocumentProperties2
  else
    fProperties := TDocumentProperties2.Create;
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

constructor TDocumentPropertiesDataReceivedEvent.Create(aDocument: TDocumentPath; aProperties: TDocumentProperties2);
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

function TAttachmentEvent.GetDescription: UTF8String;
begin
  Result:=inherited GetDescription + ' <' + AttachmentKindStrings[fAttachment.Kind] + '> "_' + fAttachment.Descriptor + '.' + fAttachment.Extension + '"';
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
  aProperties: TProjectProperties2);
begin
  inherited Create(paProjectPropertiesDataReceived);
  fProperties := aProperties;
end;

{ TProjectEvent }

constructor TProjectEvent.Create(aAction: TProjectEventKind);
begin
  inherited Create;
  fAction := aAction;
  fIsError := aAction in [paProjectPropertiesLoadingFailed, paProjectPropertiesFileLoadingFailed,
                    paProjectPropertiesSavingFailed, paProjectPropertiesSaveConflictOccurred, paProjectPropertiesFileSavingFailed,
                    paDocumentsListingFailed, paDocumentsFileListingFailed,
                    paDocumentDisappeared,
                    paDocumentShiftFailed,
                    paDocumentRenamingFailed,
                    paAttachmentLoadingFailed, paAttachmentFileLoadingFailed,
                    paAttachmentSavingFailed, paAttachmentSaveConflictOccurred, paAttachmentFileSavingFailed,
                    paAttachmentDisappeared];
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
    fProperties := FromJSON(TProjectProperties2,aStream) as TProjectProperties2
  else
    fProperties := TProjectProperties2.Create;
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

procedure TStewProject.TProjectOpenTask.ProjectOpened(Sender: TPromise);
begin
  Resolve;
end;

procedure TStewProject.TProjectOpenTask.ResolveCreateProject(aPath: TFile);
var
  lProject: TStewProject;
begin
  lProject := TStewProject.Create(aPath);
  (Promise as TProjectPromise).SetAnswer(lProject);
  // TODO: Once we move to the new system there really is no need
  // to call this anymore.
  lProject.DoOpened.After(@ProjectOpened,@SubPromiseRejected);
end;

function TStewProject.TProjectOpenTask.CreatePromise: TPromise;
begin
  result := TProjectPromise.Create(fPath);
end;

constructor TStewProject.TProjectOpenTask.Enqueue(aPath: Tfile);
begin
  fPath := aPath;
  inherited Enqueue;
end;

{ TCachedAttachmentObject }

destructor TCachedAttachmentObject.Destroy;
begin
  FreeAndNil(fData);
  inherited Destroy;
end;

{ TCachedAttachmentData }

constructor TCachedAttachmentData.Create(const aPath: TFile;
  aProperties: DataType);
begin
  inherited Create(aPath);
  fData := aProperties;
end;

{ TCachedFilePromise }

constructor TCachedFilePromise.Create(const aPath: TFile; aPromise: TPromise);
begin
  inherited Create(aPath);
  fPromise := aPromise;
end;

{ TCachedProjectData }

function TCachedProjectData.GetName: UTF8String;
begin
  result := fProjectAttachments.Name;
end;

function TCachedProjectData.GetPath: TFile;
begin
  result := fRootDirectory.Path;
end;

constructor TCachedProjectData.Create(const aPath: TFile; const aName: UTF8String);
begin
  inherited Create;
  fRootDirectory := TCachedDirectory.Create(aPath);
  fProjectAttachments := TCachedDocument.Create(aName);
end;

destructor TCachedProjectData.Destroy;
begin
  FreeAndNil(fProjectAttachments);
  FreeAndNil(fRootDirectory);
  inherited Destroy;
end;

function TCachedProjectData.GetDocument(const aName: UTF8String
  ): TCachedDocument;
begin
  result := fRootDirectory.GetDocument(aName);
end;

procedure TCachedProjectData.PutDocument(const aName: UTF8String;
  aValue: TCachedDocument);
begin
  fRootDirectory.PutDocument(aName,aValue);
end;

function TCachedProjectData.ListDocumentNames: TStringArray;
begin
  result := fRootDirectory.ListDocumentNames;
end;

function TCachedProjectData.ListDocuments: TCachedDocumentArray;
begin
  result := fRootDirectory.ListDocuments;
end;

function TCachedProjectData.GetAttachment(const aDescriptor: UTF8String;
  aExtension: UTF8String): TCachedFileData;
begin
  result := fProjectAttachments.GetAttachment(aDescriptor,aExtension);
end;

procedure TCachedProjectData.PutAttachment(const aDescriptor: UTF8String;
  aExtension: UTF8String; aValue: TCachedFileData);
begin
  fProjectAttachments.PutAttachment(aDescriptor,aExtension,aValue);
end;

function TCachedProjectData.ListAttachments(const aDescriptor: UTF8String
  ): TAttachmentArray;
begin
  result := fProjectAttachments.ListAttachments(aDescriptor);
end;

{ TCachedDirectory }

constructor TCachedDirectory.Create(const aPath: TFile);
begin
  inherited Create(aPath);
  fDocuments := TFPHashObjectList.Create(true);
end;

destructor TCachedDirectory.Destroy;
begin
  FreeAndNil(fDocuments);
  inherited Destroy;
end;

function TCachedDirectory.GetDocument(const aName: UTF8String): TCachedDocument;
begin
  result := fDocuments.Find(aName) as TCachedDocument;
end;

procedure TCachedDirectory.PutDocument(const aName: UTF8String;
  aValue: TCachedDocument);
begin
  fDocuments.Add(aName,aValue);
end;

function TCachedDirectory.ListDocumentNames: TStringArray;
var
  i: Integer;
begin
  SetLength(result,fDocuments.Count);
  for i := 0 to fDocuments.Count - 1 do
  begin
    result[i] := fDocuments.NameOfIndex(i);
  end;
end;

function TCachedDirectory.ListDocuments: TCachedDocumentArray;
var
  i: Integer;
begin
  SetLength(result,fDocuments.Count);
  for i := 0 to fDocuments.Count - 1 do
  begin
    result[i] := fDocuments.Items[i] as TCachedDocument;
  end;
end;

{ TCachedFileData }

constructor TCachedFileData.Create(const aPath: TFile);
begin
  inherited Create;
  fPath := aPath;
end;

{ TCachedDocument }

constructor TCachedDocument.Create(const aName: UTF8String);
begin
  inherited Create;
  fName := aName;
  fAttachments := TFPHashObjectList.Create(true);
end;

destructor TCachedDocument.Destroy;
begin
  FreeAndNil(fAttachments);
  inherited Destroy;
end;

function TCachedDocument.GetAttachment(const aDescriptor: UTF8String;
  aExtension: UTF8String): TCachedFileData;
begin
  result := fAttachments.Find(BuildFileName('',aDescriptor,aExtension,true)) as TCachedFileData;
end;

procedure TCachedDocument.PutAttachment(const aDescriptor: UTF8String;
  aExtension: UTF8String; aValue: TCachedFileData);
begin
  fAttachments.Add(BuildFileName('',aDescriptor,aExtension,true),aValue);
end;

function TCachedDocument.ListAttachments(const aDescriptor: UTF8String
  ): TAttachmentArray;
var
  l: Integer;
  i: Integer;
begin
  l := 0;
  SetLength(Result,l);
  for i := 0 to fAttachments.Count - 1 do
  begin
    if ExcludeDescriptorDelimiter(ExtractFileDescriptor(fAttachments.NameOfIndex(i))) = aDescriptor then
    begin
      l := l + 1;
      SetLength(Result,l);
      Result[l - 1] := fAttachments[i] as TCachedFileData;
    end;
  end;


end;

{ TCachedItem }

constructor TCachedData.Create;
begin
  inherited Create;
  fCreated := Now;
end;

{ TProjectCreateNewTask }

procedure TStewProject.TProjectCreateNewTask.DoTask;
begin
  ResolveCreateProject(Path);
end;

{ TProjectOpenInParentTask }

procedure TStewProject.TProjectOpenInParentTask.FileExistsInParent(Sender: TPromise);
begin
  (Promise as TProjectPromise).SetAnswer((Sender as TProjectPromise).Project);
  Resolve;
end;

procedure TStewProject.TProjectOpenInParentTask.FileExists(Sender: TPromise);
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
      TProjectOpenInParentTask.Enqueue(lParent).After(@FileExistsInParent,@SubPromiseRejected);
    end;
  end;
end;

procedure TStewProject.TProjectOpenInParentTask.DoTask;
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

procedure TStewProject.TProjectOpenAtPathTask.FileExists(Sender: TPromise);
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

procedure TStewProject.TProjectOpenAtPathTask.DoTask;
begin
  // TODO: Once we move over to the new Properties format, just
  // make this a 'read', and pass the data onto the project constructor.
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

class function TDocumentPath.FromString(const aPath: UTF8String): TDocumentPath;
begin
  result.fID := IncludeLeadingSlash(ExcludeTrailingSlash(aPath));
end;

class function TDocumentPath.GetSystemDocument(aName: UTF8String): TDocumentPath;
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
  FileLoaded((aSender as TFileReadPromise).ReadString,(aSender as TFileReadPromise).Age);

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
  aData: TPromiseError);
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
     GetDefaultFile.Write('').After(@EditableFileWritten,@FileLoadFailed)
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
       GetDefaultFile.WithDifferentExtension('txt').Write('').After(@EditableFileWritten,@FileLoadFailed)
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
        aCandidates[0].Read.After(@FileLoaded,@FileLoadFailed);
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
      aCandidates[0].Write(lOptions,fFileAge,fContents).After(@FileSaved,@FileSaveConflictCheck);
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
  FilesListed((aSender as TFileListPromise).GetJustFiles,false);
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

function TDocumentMetadata.SortDocuments(List: TStringList; Index1,
  Index2: Integer): Integer;
var
  s1: String;
  s2: String;
  i1: Integer;
  i2: integer;
  index: TStrings;
begin
  s1 := TDocumentPath.FromString(List[Index1]).Name;
  s2 := TDocumentPath.FromString(List[Index2]).Name;
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
  FilesListed((aSender as TFileListPromise).GetJustFiles,true);
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
  aDiskPath: TFile; aID: TDocumentPath; aIsRoot: Boolean);
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

function TDocumentMetadata.GetContents: TDocumentArray;
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
      result[i] := TDocumentPath.FromString(list[i]);
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
  if fID <> TDocumentPath.Root then
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
  aList: TDocumentArray;
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
  if not (fID = TDocumentPath.Root) then
  begin
    // Add the 'directory' name to the files.
    AddFile(fDisk);
    aParent := fProject.GetDocument(fID.Container);
    if aParent.IsNew and not (aParent.fID = TDocumentPath.Root) then
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
    fMetadataCache := TDocumentMetadata.Create(Self,fDisk,TDocumentPath.Root,true);
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
      lDocument := TranslateFileToDocument(fDisk,Sender.Path);
      lAttachment := TranslateFileToAttachment(fDisk,Sender.Path);
      if Sender.State = psIncomplete then
         ReportEvent(TAttachmentEvent.Create(paLoadingAttachmentFile,lDocument,lAttachment))
      else
         ReportEvent(TAttachmentEvent.Create(paAttachmentFileLoaded,lDocument,lAttachment))
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
      lAttachment := TranslateFileToAttachment(fDisk,Sender.Path);
      lDocument := TranslateFileToDocument(fDisk,Sender.Path);
      if Sender.State = psIncomplete then
         ReportEvent(TAttachmentEvent.Create(paSavingAttachmentFile,lDocument,lAttachment))
      else
         ReportEvent(TAttachmentEvent.Create(paAttachmentFileSaved,lDocument,lAttachment))
    end;
  end;

  procedure ReportList(Sender: TFileListPromise); inline;
  begin
   if not (Sender.Path = GetProjectPropertiesPath(fDisk)) then
    begin
      lAttachment := TranslateFileToAttachment(fDisk,Sender.Path);
      lDocument := TranslateFileToDocument(fDisk,Sender.Path);
      if Sender.State = psIncomplete then
         ReportEvent(TDocumentEvent.Create(paListingDocumentFiles,lDocument))
      else
         ReportEvent(TDocumentEvent.Create(paDocumentFilesListed,lDocument))
    end;
    // else, its an attempt to "list" the directory contents of a file. I'm
    // not worried about this.
  end;

  procedure ReportCheck(Sender: TFileExistencePromise); inline;
  begin
   if ((fDisk = Sender.Path) or fDisk.Contains(Sender.Path)) then
    begin
      lDocument := TranslateFileToDocument(fDisk,Sender.Path);
      if Sender.State = psIncomplete then
         ReportEvent(TDocumentEvent.Create(paCheckingDocumentFile,lDocument))
      else
         ReportEvent(TDocumentEvent.Create(paDocumentFileChecked,lDocument))
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
// TODO: I'm potentially missing a bunch of errors here by using conditionals
// before reporting. I should be *reporting* these.
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
      lDocument := TranslateFileToDocument(fDisk,Sender.Path);
      lAttachment := TranslateFileToAttachment(fDisk,Sender.Path);
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
      lAttachment := TranslateFileToAttachment(fDisk,Sender.Path);
      lDocument := TranslateFileToDocument(fDisk,Sender.Path);
      ReportEvent(TAttachmentError.Create(paAttachmentFileSavingFailed,lDocument,lAttachment,aError));
    end;
  end;

  procedure ReportList(Sender: TFileListPromise); inline;
  begin
   if not (Sender.Path = GetProjectPropertiesPath(fDisk)) then
    begin
      lDocument := TranslateFileToDocument(fDisk,Sender.Path);
      ReportEvent(TDocumentError.Create(paDocumentsFileListingFailed,lDocument,aError));
    end;
    // else, its an attempt to "list" the directory contents of a file. I'm
    // not worried about this.
  end;

  procedure ReportCheck(Sender: TFileExistencePromise); inline;
  begin
    if ((fDisk = Sender.Path) or fDisk.Contains(Sender.Path)) then
    begin
      lDocument := TranslateFileToDocument(fDisk,Sender.Path);
      ReportEvent(TDocumentError.Create(paDocumentFileCheckingFailed,lDocument,aError));
    end;
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
    // renaming events are handled by the specific renaming
    // promise, because we need the names of the documents being
    // renamed.
    Exit;

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
  ReportEvent(TDocumentRenameErrorEvent.Create(paDocumentRenamed,
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
  inherited Create;
  fDisk := Path;
  fCache := TFileSystemCache.Create(fDisk.System);
  fCache.OnActivity:=@CacheActivity;
  fCache.OnError:=@CacheError;
  fShadows := TShadowCache.Create;
  fObservers := TProjectObserverList.Create;
  fMetadataCache := nil; // created on open.
  fProperties := nil;
end;

constructor TStewProject.Create;
begin
  raise Exception.Create('Please use one of the static functions ("Open...") to open a new project');
end;

destructor TStewProject.Destroy;
begin
  FreeAndNil(fShadows);
  FreeAndNil(fCache);
  FreeAndNil(fObservers);
  FreeAndNil(fMetadataCache);
  if fProperties <> nil then
    fProperties.Free;
  inherited Destroy;
end;

function TStewProject.GetDocument(const aDocumentID: TDocumentPath
  ): TDocumentMetadata;
begin
  if fMetadataCache = nil then
     raise Exception.Create('Please open project first');
  if aDocumentID.IsNull then
     raise Exception.Create('Can''t retrieve a null document');
  if aDocumentID = TDocumentPath.Root then
    result := fMetadataCache
  else
     result := fMetadataCache.PutPath(aDocumentID.ID);
end;

function TStewProject.GetProjectName: String;
begin
  result := fDisk.PacketName;
end;

procedure TStewProject.CreateShadow(aDocument: TDocumentPath);
begin
  fShadows.CreateShadow(aDocument);
end;

function TStewProject.IsShadow(aDocument: TDocumentPath): Boolean;
begin
  result := fShadows.HasShadow(aDocument);
end;

procedure TStewProject.DeleteShadow(aDocument: TDocumentPath);
begin
  fShadows.DeleteShadow(aDocument);
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
  aSynopsis: UTF8String): TWriteAttachmentPromise;
var
  lWriteFile: TFileWritePromise;
begin
  lWriteFile := fCache.WriteFile(GetDocumentSynopsisPath(fDisk,aDocument));
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
  aProperties: TDocumentProperties2): TWriteAttachmentPromise;
var
  lWriteFile: TFileWritePromise;
begin
  lWriteFile := fCache.WriteFile(GetDocumentPropertiesPath(fDisk,aDocument));
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

function TStewProject.WriteProjectProperties(aProperties: TProjectProperties2
  ): TWriteProjectPropertiesPromise;
var
  lWriteFile: TFileWritePromise;
begin
  lWriteFile := fCache.WriteFile(GetProjectPropertiesPath(fDisk));
  ToJSON(aProperties,lWriteFile.Data,'  ');
  result := TWriteProjectPropertiesTask.Defer(lWriteFile).Promise as TWriteProjectPropertiesPromise;
  // we only need to catch to make sure the error is reported.
  result.Catch(@ProjectPropertiesWriteError);
end;

function TStewProject.ShiftDocumentBy(aDocument: TDocumentPath; aDelta: Integer
  ): TShiftDocumentPromise;
var
  lFolderFile: TFile;
  lListFiles: TFileListPromise;
begin
  if aDocument = TDocumentPath.Root then
    raise Exception.Create('Can''t shift the document root');
  if aDelta = 0 then
    raise Exception.Create('Delta can''t be 0');
  lFolderFile := GetDocumentFolderPath(fDisk,aDocument.Container);
  lListFiles := fCache.ListFiles(lFolderFile);
  result := TShiftDocumentByTask.Defer(aDocument,Self,aDelta,lListFiles).Promise as TShiftDocumentPromise;
  result.After(@DocumentShifted,@DocumentShiftError);
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

function TStewProject.RenameDocument(aDocument: TDocumentPath;
  aNewDocument: TDocumentPath): TRenameDocumentPromise;
var
  lListFiles: TFileListPromise;
begin
  if aDocument = TDocumentPath.Root then
    raise Exception.Create('Can''t shift the document root');
  if aNewDocument = TDocumentPath.Root then
    raise Exception.Create('Can''t rename a document to root');
  lListFiles := fCache.ListFiles(GetDocumentFolderPath(fDisk,aDocument.Container));
  result := TRenameDocumentTask.Defer(aDocument,aNewDocument,Self,lListFiles).Promise as TRenameDocumentPromise;
  result.After(@DocumentsRenamed,@DocumentRenamingError);
end;

procedure TStewProject.AddObserver(aObserver: TProjectObserver);
begin
  fObservers.Add(aObserver);
end;

procedure TStewProject.RemoveObserver(aObserver: TProjectObserver);
begin
  fObservers.Remove(aObserver);
end;

class function TStewProject.Open(aPath: TFile): TProjectPromise;
begin
  result := TProjectOpenAtPathTask.Enqueue(aPath).Promise as TProjectPromise;
end;

class function TStewProject.OpenInParent(aPath: TFile): TProjectPromise;
begin
  result := TProjectOpenInParentTask.Enqueue(aPath).Promise as TProjectPromise;
end;

class function TStewProject.CreateNew(aPath: TFile): TProjectPromise;
begin
  result := TProjectCreateNewTask.Enqueue(aPath).Promise as TProjectPromise;
end;

function TStewProject.BuildAndSortDocumentList(
  aParent: TDocumentPath;
  aProperties: TDocumentProperties2; aFiles: TFileInfoArray
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

class function TStewProject.GetProjectPropertiesPath(aFile: TFile): TFile;
begin
  result := aFile.GetContainedFile('',ProjectPropertiesDescriptor,PropertiesExtension,false);
end;

class function TStewProject.GetDocumentPropertiesPath(aProjectPath: TFile;
  aDocument: TDocumentPath): TFile;
begin
  result := TranslateDocumentAttachmentToFile(aProjectPath,aDocument,DocumentPropertiesDescriptor,PropertiesExtension);
end;

class function TStewProject.GetDocumentSynopsisPath(aProjectPath: TFile;
  aDocument: TDocumentPath): TFile;
begin
  result := TranslateDocumentAttachmentToFile(aProjectPath,aDocument,DocumentSynopsisDescriptor,DocumentSynopsisExtension);
end;

class function TStewProject.GetDocumentFolderPath(aProjectPath: TFile;
  aDocument: TDocumentPath): TFile;
begin
  result := TranslateDocumentAttachmentToFile(aProjectPath,aDocument,'','');
end;

class function TStewProject.TranslateFileToDocument(aBasePath: TFile;
  aFile: TFile): TDocumentPath;
var
  lSplit: TStringArray;
  i: Integer;
begin
  if aBasePath.Contains(aFile) then
  begin
    // We want to be able to handle documents that are contained within
    // attachments, for future compatbility, so we are not looking at just
    // the packetname for the rest of the path, but we are for here,
    // so the check is against a file without descriptor and extension.
    lSplit := aFile.WithDifferentDescriptorAndExtension('','').SplitPath(aBasePath);
    result := TDocumentPath.Root;
    for i := 0 to length(lSplit) - 1 do
    begin
      result := Result.GetContainedDocument(lSplit[i]);
    end;
  end
  else if aBasePath = aFile then
    result := TDocumentPath.Root
  else
    result := TDocumentPath.Null;

end;

class function TStewProject.TranslateFileToAttachment(aBasePath: TFile;
  aFile: TFile): TAttachment;
begin
  result := TAttachment.MakeUnknown('','');
  if aBasePath.Contains(aFile) then
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
        DocumentSynopsisDescriptor:
          if result.Extension = DocumentSynopsisExtension then
            result.Kind := atSynopsis;
        DocumentPropertiesDescriptor:
          if result.Extension = PropertiesExtension then
            result.Kind := atProperties;
        DocumentNotesDescriptor:
          result.Kind := atNotes;
        DocumentThumbnailDescriptor:
          result.Kind := atThumbnail;
      else
        if Pos(DocumentBackupDescriptorPrefix,aFile.Descriptor) = 1 then
          result.Kind := atBackup;
      end;
    end;
  end
  else if aBasePath = aFile then
     result.Kind := atFolder
  else
  // else we're actually not at all interested in this file...
end;

class function TStewProject.TranslateDocumentAttachmentToFile(aBasePath: TFile;
  aDocument: TDocumentPath; aDescriptor: UTF8String; aExtension: UTF8String
  ): TFile;
var
  lSplit: TStringArray;
  i: Integer;
begin
  if aDocument = TDocumentPath.Root then
  begin
    if (aDescriptor = '') and (aExtension = '') then
    // the only 'attachment' on the root that isn't contained inside the
    // directory is the directory itself.
      result := aBasePath
    else
      result := aBasePath.GetContainedFile('',aDescriptor,aExtension,true)
  end
  else if aDocument = TDocumentPath.Null then
  begin
    raise Exception.Create('Can''t determine file for null document path')
  end
  else
  begin
    lSplit := aDocument.Split;
    result := aBasePath;
    for i := 0 to Length(lSplit) - 1 do
    begin
      result := Result.GetContainedFile(lSplit[i]);
    end;
    result := Result.WithDifferentDescriptorAndExtension(aDescriptor,aExtension);
  end;

end;

end.

