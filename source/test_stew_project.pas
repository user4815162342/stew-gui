unit test_stew_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry;

type
  TProjectSpec = class(TTestSpec)

  end;

{
TODO: Like I did with the JSValue stuff. Consider just writing the new project
object and writing tests against that. It will save me a lot of work.
Or, rather, start rewriting the existing TProject one bit at a time, to fit with the
new testing.

TODO: Write tests for all of this:

* Various scenarios for opening a project
  - project exists at directory
  - project exists in parent directory, open there instead.
  - project not found, so create a new project at initial directory.
* List all documents in the root of the project (make sure they're in the correct order)
* List some subdocuments
* Refresh a list without listing all subdocuments.
* Open up the project properties and check values
* Make changes to project properties and make sure they get written to disk correctly.
* Open up the properties and other data for a couple of different documents
* Make changes to properties and other data, and make sure they are sent
to disk appropriately (reload and re-read)
* Create a new document inside another document
* Lock some documents, and unlock.
* Re-order a document and make sure it shows up in the appropriate place in the list
  - One that's not in the index to before one that's in the index
  - One that's in the index to before one that's in the index
  - One that's not in the index to before one that's not in a index
* Rename a document
* Move a document from one folder to another
  - move to be a sibling of it's parent
  - move to be inside another folder that's a sibling of it's parent
  - move inside a sibling


TStewProject:
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
  constructor Create(const Path: TFile);
  destructor Destroy; override;
  property DiskPath: TFile read fDisk;
  function GetDocument(const aDocumentID: TDocumentID): TDocumentMetadata;
  procedure OpenAtPath(aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
  procedure OpenInParentDirectory(aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
  procedure OpenNewAtPath;
  property IsOpened: Boolean read GetIsOpened;
  function GetProjectName: String;
  property Properties: TProjectProperties read GetProperties;
end;

TDocumentMetadata:
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

TODO: Once the tests are done, start converting Project over to new code standards:
- Properties need to be the JSValue stuff
- Async stuff needs to be promises.
- Stop dealing with all of these events and convert to a browser style, also hiding the cache.
  - Instead of events, the project broadcasts changes which can be responded to.
    The broadcasts might include the actual promises so you can attach 'after' to
    them.
  - Instead of 'locking' documents before rename or move, a broadcast will be
    sent out asking if the document can be moved. The user can then be asked
    if that's okay. If anyone says "no" then it will fail.
  - Instead of searching for the cache, etc. All calls to get data from the project
    will return promises. The code will only deal with 'DocumentIDs', and call
    methods on the *Project* like "GetSynopsis", etc. The project will check it's
    cache, and if it's not found will go and get the data. There would also be
    parameters for "forcing" an update: don't even check file system if in cache;
    check file age and update if changed; update whether the file has changed or
    is in cache or not.

}

implementation

end.

