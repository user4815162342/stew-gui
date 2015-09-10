unit test_stew_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_file, sys_async, stew_project;

{
TODO:
- Project Properties almost done:
  - I need to use the FileAge thing... perhaps when saving or loading
    I can look for it in the cache.
  - Need to "force refresh" on opening project properties and other things
  - Need to allow cancelling of saving project properties (and refreshing for that matter)
  - Need some events to indicate what's going on.
- Need Document Lists (starting with the root level) read
- Need Document Properties read/write
- Need Document Synopsis read/write
- Need Edit Document Attachment
- Need Create Document
  - also make sure it shows up in lists after created.
- Need *Shift* Document up and down
- Need Move Document between folders.

TODO: I'm slowly converting the TStewProject over to the way I want it to be,
and testing the new stuff as I do so (this is better than writing the tests and
then rewriting them for the new things). As I complete each section, write a
test, once it's working, go back to the GUI and refactor to use the changes.
Then commit.

Overall changes, new code standards:
- Properties need to be the JSValue stuff, and are loaded by Metadata objects.
- All async stuff needs to be promises (except a few small things in the GUI layer).
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
    cache, and if it's not found will go and get the data.
  - There would also be parameters for "forcing" an update on most requests for
    data:
    - don't even check file system if in cache;
    - check file age and update cache if changed;
    - update whether the file has changed or is in cache or not.
- broadcast events:
  - In general, we don't need a broadcast event for a load. The objects that
  need the data will load it when they need it (and if it's already there,
  it will be loaded from the cache) and they will get promises. The only
  broadcast events we need are if the data somehow changes (or wants to change),
  or if there is some sort of error in filing (although this is only needed by
  the mainform, so that may remain a separate event). So, only broadcast when
  data is written, or some sort of check is made and the disk file has changed,
  so therefore the data needs to be reloaded.

Sections:

* Project Properties
 - Test Open up the project properties and check values
 - Test Make changes to project properties and make sure they get written to disk correctly.
* Document Properties
- Test Open up some document properties and check values
- Test Make changes to document properties and make sure they get written to disk correctly.
* Create Document
* Document listing:
  - Test listing,
  - Test listing of subdirectories
  - Test 'refresh' of a listing so that it only relists what is already cached.
* Re-order a document and make sure it shows up in the appropriate place in the list
  - One that's not in the index to before one that's in the index
  - One that's in the index to before one that's in the index
  - One that's not in the index to before one that's not in a index
* Rename a document
* Move a document from one folder to another
  - move to be a sibling of it's parent
  - move to be inside another folder that's a sibling of it's parent
  - move inside a sibling
* Lock Documents? Or, instead, allowing denial of document renaming and moving
  and other metadata something listening to a specific document might want.


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

}

type

  { TProjectSpec }

  TProjectSpec = class(TTestSpec)
    procedure Project_Properties_3(Sender: TPromise);
    procedure Project_Properties_4(Sender: TPromise);
  private
    fTempDir: String;
    fTestRootDir: TFile;
    fProject: TStewProject;
    procedure Open_Project_1(Sender: TPromise);
    procedure Open_Project_2(Sender: TPromise);
    procedure Open_Project_3(Sender: TPromise);
    procedure PromiseFailed(Sender: TPromise; aError: TPromiseException);
    procedure Project_Properties_1(Sender: TPromise);
    procedure Project_Properties_2(Sender: TPromise);
  public
    procedure SetupTest; override;
    procedure CleanupTest; override;
  published
    procedure Test_Open_Project;
    procedure Test_Project_Properties;
  end;


implementation

uses
  gui_async, FileUtil, sys_localfile, stew_properties, Graphics;

{ TProjectSpec }

procedure TProjectSpec.PromiseFailed(Sender: TPromise; aError: TPromiseException
  );
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  FailAsync(Sender.Tag,aError);
end;

procedure TProjectSpec.Open_Project_2(Sender: TPromise);
var
  lProject: TStewProject;
begin
  lProject := (Sender as TProjectPromise).Project;
  try
    Assert(lProject <> nil,'Project should have been opened in parent');
    Assert(lProject.DiskPath = fTestRootDir,'Open in parent should have been opened at the correct path');
  finally
    lProject.Free;
  end;
  TStewProject.CreateNew(LocalFile(GetTempFileName('',''))).After(@Open_Project_3,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Open_Project_3(Sender: TPromise);
var
  lProject: TStewProject;
begin
  lProject := (Sender as TProjectPromise).Project;
  try
    Assert(lProject <> nil,'New project should have been opened');
    Assert(lProject.DiskPath <> fTestRootDir,'New project should have been opened at the correct path');
  finally
    lProject.Free;
  end;
  EndAsync(Sender.Tag);
end;

procedure TProjectSpec.Project_Properties_1(Sender: TPromise);
begin
  fProject := (Sender as TProjectPromise).Project;
  fProject.ReadProjectProperties.After(@Project_Properties_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Project_Properties_2(Sender: TPromise);
var
  lProps: TProjectProperties2;
begin
  lProps := (Sender as TProjectPropertiesPromise).Properties;
  // yes, these are taken directly from the test_stew_properties.
  Assert(Length(lProps.Categories.keys) > 0,'Combination of parsing and GetPath should have gotten some data');
  Assert((lProps.Categories.Get('Chapter') as TCategoryDefinition2).PublishTitleLevel = 0,'Project category definitions should work');
  Assert(lProps.Categories.hasOwnProperty('Scene'),'Project categories should work');
  Assert(lProps.DefaultCategory = 'Chapter','Project default category should work');
  Assert(lProps.DefaultDocExtension = 'txt','Project default doc extension should work');
  Assert(lProps.DefaultNotesExtension = 'txt','Project default notes extension should work');
  Assert(lProps.DefaultStatus = 'Unwritten','Project default status should work');
  Assert(lProps.DefaultThumbnailExtension = 'png','Project default thumbnail extension should work');
  Assert((lProps.Statuses.Get('Unwritten') as TStatusDefinition2).Color = clRed,'Project statuses should work');
  lProps.DefaultDocExtension := '.doc';
  Assert(lProps.DefaultDocExtension = 'doc','Default doc extension should trim off the "." when assigning');

  // now, test changing some things and writing it out.
  lProps.DefaultCategory := 'Tome';
  fProject.WriteProjectProperties(lProps).After(@Project_Properties_3,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Project_Properties_3(Sender: TPromise);
begin
  // re-read it to see if the data changed.
  fProject.ReadProjectProperties.After(@Project_Properties_4,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Project_Properties_4(Sender: TPromise);
begin
  Assert((Sender as TProjectPropertiesPromise).Properties.DefaultCategory = 'Tome','Changes should have been saved');
  EndAsync(Sender.Tag);
end;

procedure TProjectSpec.Open_Project_1(Sender: TPromise);
var
  lProject: TStewProject;
begin
  lProject := (Sender as TProjectPromise).Project;
  try
    Assert(lProject <> nil,'Project should have been opened');
    Assert(lProject.DiskPath = fTestRootDir,'Project should have been opened at the correct path');
  finally
    lProject.Free;
  end;
  TStewProject.OpenInParent(fTestRootDir.GetContainedFile('Notes')).After(@Open_Project_2,@PromiseFailed).Tag := Sender.Tag;

end;

procedure TProjectSpec.SetupTest;
begin
  inherited SetupTest;
  SetAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
  fTempDir := GetTempFileName('','');
  CopyDirTree('../test-data/story/',IncludeTrailingPathDelimiter(fTempDir));
  fTestRootDir := LocalFile(fTempDir);
end;

procedure TProjectSpec.CleanupTest;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  DeleteDirectory(fTempDir,false);
  RemoveAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
  inherited CleanupTest;
end;

procedure TProjectSpec.Test_Open_Project;
begin
  TStewProject.Open(fTestRootDir).After(@Open_Project_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_Project_Properties;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.Open(fTestRootDir).After(@Project_Properties_1,@PromiseFailed).Tag := BeginAsync;
end;

end.

