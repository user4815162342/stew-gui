unit test_stew_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_file, sys_async, stew_project;

{
TODO:
- As we go through, make sure we're supporting the non-file events with
  each thing.
- Need Document Lists (starting with the root level) read
  - make sure this is sorted according to the index of the properties...
- Need Document Synopsis read/write
- Need Document IsDirectory (see below)
  - include a test where we list documents and then check for this value
    on each one.
- Need Edit Document Attachment
  - not absolutely sure we need to test this, although I suppose we're testing
    it at another point where we're getting the xdg-open error.
- Need Create Document
  - also make sure it shows up in lists after created.
- Need *Shift* Document up and down
  - and, again, test the listing after...
- Need Move Document between folders.
  - I don't *really* need to prevent a move if something's being edited when a
    request is made to move. I should be able to just automatically change the
    documentpath to the new one. However, if I'm renaming *over* another file,
    that might be a problem, but I don't think I should allow that.

TODO: An Idea to help out with the project tree building: Part of the
file existence check, and the listing, involves calls to DirectoryExists.
It shouldn't take much more to add a 'IsDirectory' property onto the
Existence check in the file system cache. I can then easily report, with
some quick check existence stuff, whether a given document has a directory
attachment, and therefore be able to easily mark it as expandable in the
project tree view, without having to guess. It does require doing a check
existence on every document, but that's something that might be useful
anyway.
-- Basically, all this does is add a IsDirectory(Document) method, task
and promise to the Project, and a little bit of infrastructure in the
CheckExistence file routines to get this information.

TODO: While we're at that, when we do a file list, the cache should automatically
store the fact that each of those files exist. If we're doing a directory checking,
we can either return a list of records and statuses, or just return two sets of
lists: one for file and one for folders.



TODO: Once TStewProject is ready and tested, start deprecating the old stuff,
and recompile the GUI. Use the deprecated hints to help figure out where things
need to be fixed.

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

  TProjectEventArray = array of String;
  { TProjectSpec }

  TProjectSpec = class(TTestSpec)
    procedure Document_Properties_1(Sender: TPromise);
    procedure Document_Properties_2(Sender: TPromise);
    procedure Document_Properties_3(Sender: TPromise);
    procedure Document_Properties_4(Sender: TPromise);
  private
    fTempDir: String;
    fTestRootDir: TFile;
    fProject: TStewProject;
    fProjectEvents: TProjectEventArray;
    procedure ClearProjectEvents;
    function VerifyProjectEvents(aExpected: array of String; aTestID: Integer): Boolean;
    procedure ObserveProject(Sender: TStewProject; Event: TProjectEvent);
    procedure Open_Project_1(Sender: TPromise);
    procedure Open_Project_2(Sender: TPromise);
    procedure Open_Project_3(Sender: TPromise);
    procedure PromiseFailed(Sender: TPromise; aError: TPromiseError);
    procedure Project_Properties_1(Sender: TPromise);
    procedure Project_Properties_2(Sender: TPromise);
    procedure Project_Properties_3(Sender: TPromise);
    procedure Project_Properties_4(Sender: TPromise);
  public
    procedure SetupTest; override;
    procedure CleanupTest; override;
  published
    procedure Test_Open_Project;
    procedure Test_Project_Properties;
    procedure Test_Document_Properties;
  end;


implementation

uses
  gui_async, FileUtil, sys_localfile, stew_properties, Graphics;

{ TProjectSpec }

procedure TProjectSpec.PromiseFailed(Sender: TPromise; aError: TPromiseError
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
    AssertAsync(lProject <> nil,'Project should have been opened in parent',Sender.Tag);
    AssertAsync(lProject.DiskPath = fTestRootDir,'Open in parent should have been opened at the correct path',Sender.Tag);
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
    AssertAsync(lProject <> nil,'New project should have been opened',Sender.Tag);
    AssertAsync(lProject.DiskPath <> fTestRootDir,'New project should have been opened at the correct path',Sender.Tag);
  finally
    lProject.Free;
  end;
  EndAsync(Sender.Tag);
end;

procedure TProjectSpec.Project_Properties_1(Sender: TPromise);
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fProject.AddObserver(@ObserveProject);
  fProject.ReadProjectProperties.After(@Project_Properties_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Project_Properties_2(Sender: TPromise);
var
  lProps: TProjectProperties2;
begin
  lProps := (Sender as TProjectPropertiesPromise).Properties;
  // yes, these are taken directly from the test_stew_properties.
  AssertAsync(Length(lProps.Categories.keys) > 0,'Combination of parsing and GetPath should have gotten some data',Sender.Tag);
  AssertAsync((lProps.Categories.Get('Chapter') as TCategoryDefinition2).PublishTitleLevel = 0,'Project category definitions should work',Sender.Tag);
  AssertAsync(lProps.Categories.hasOwnProperty('Scene'),'Project categories should work',Sender.Tag);
  AssertAsync(lProps.DefaultCategory = 'Chapter','Project default category should work',Sender.Tag);
  AssertAsync(lProps.DefaultDocExtension = 'txt','Project default doc extension should work',Sender.Tag);
  AssertAsync(lProps.DefaultNotesExtension = 'txt','Project default notes extension should work',Sender.Tag);
  AssertAsync(lProps.DefaultStatus = 'Unwritten','Project default status should work',Sender.Tag);
  AssertAsync(lProps.DefaultThumbnailExtension = 'png','Project default thumbnail extension should work',Sender.Tag);
  AssertAsync((lProps.Statuses.Get('Unwritten') as TStatusDefinition2).Color = clRed,'Project statuses should work',Sender.Tag);
  lProps.DefaultDocExtension := '.doc';
  AssertAsync(lProps.DefaultDocExtension = 'doc','Default doc extension should trim off the "." when assigning',Sender.Tag);

  // now, test changing some things and writing it out.
  lProps.DefaultCategory := 'Tome';
  fProject.WriteProjectProperties(lProps).After(@Project_Properties_3,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_Properties_1(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fProject.AddObserver(@ObserveProject);
  lDocument := TDocumentPath.Root.GetContainedDocument('Chapter 1');
  fProject.ReadDocumentProperties(lDocument).After(@Document_Properties_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_Properties_2(Sender: TPromise);
var
  lProps: TDocumentProperties2;
  lDocument: TDocumentPath;
begin
  lProps := (Sender as TDocumentPropertiesPromise).Properties;
  lDocument := (Sender as TDocumentPropertiesPromise).Document;
  AssertAsync(lProps.Category = 'Chapter','Category should return correct value',Sender.Tag);
  AssertAsync(lProps.Status = 'Unwritten','Status should return correct value',Sender.Tag);
  AssertAsync(lProps.Title = 'The Cottage','Title should return correct value',Sender.Tag);
  AssertAsync(lProps.Publish = false,'Publish should return correct value',Sender.Tag);
  AssertAsync(lProps.Index.Length = 0,'Index should return correct value',Sender.Tag);
  AssertAsync(lProps.User.Get('place').AsString = 'Jen''s Lakeside Cottage','User properties should return correct values',Sender.Tag);

  lProps.Title := 'The Cottage Not in the Woods';
  fProject.WriteDocumentProperties(lDocument,lProps).After(@Document_Properties_3,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_Properties_3(Sender: TPromise);
begin
  // re-read it to see if the data changed.
  fProject.ReadDocumentProperties((Sender as TWriteAttachmentPromise).Document).After(@Document_Properties_4,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_Properties_4(Sender: TPromise);
begin
  AssertAsync((Sender as TDocumentPropertiesPromise).Properties.Title = 'The Cottage Not in the Woods','Changes should have been saved',Sender.Tag);
  if not VerifyProjectEvents(['<LoadingAttachmentFile> "/Chapter 1" <properties> "_properties.json"',
                              '<AttachmentFileLoaded> "/Chapter 1" <properties> "_properties.json"',
                              '<AttachmentDataReceived> "/Chapter 1" <properties> "_properties.json"',
                              '<SavingAttachmentFile> "/Chapter 1" <properties> "_properties.json"',
                              '<AttachmentFileSaved> "/Chapter 1" <properties> "_properties.json"',
                              '<AttachmentDataReceived> "/Chapter 1" <properties> "_properties.json"'],Sender.Tag) then
    Exit;
  EndAsync(Sender.Tag);
end;

procedure TProjectSpec.ClearProjectEvents;
begin
  SetLength(fProjectEvents,0);
end;

function TProjectSpec.VerifyProjectEvents(aExpected: array of String;
  aTestID: Integer): Boolean;
var
  i: Integer;
begin
  result := true;
  for i := 0 to Length(aExpected) - 1 do
  begin
    if i >= Length(fProjectEvents) then
    begin
      FailAsync(aTestID,'Not enough events reported');
      result := false;
      Exit;
    end;
    if fProjectEvents[i] <> aExpected[i] then
    begin
      FailAsync(aTestID,'Expected event: "' + aExpected[i] + '" received: "' + fProjectEvents[i] + '"');
      result := false;
      Exit;
    end;
  end;
  if i < Length(fProjectEvents) - 1 then
  begin
    FailAsync(aTestID,'More events were observed than expected');
    result := false;
  end;

end;

procedure TProjectSpec.ObserveProject(Sender: TStewProject; Event: TProjectEvent);
var
  l: Integer;
begin
  l := Length(fProjectEvents);
  SetLength(fProjectEvents,l + 1);
  fProjectEvents[l] := Event.GetDescription;
  Report(fProjectEvents[l]);
end;

procedure TProjectSpec.Project_Properties_3(Sender: TPromise);
begin
  // re-read it to see if the data changed.
  fProject.ReadProjectProperties.After(@Project_Properties_4,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Project_Properties_4(Sender: TPromise);
begin
  AssertAsync((Sender as TProjectPropertiesPromise).Properties.DefaultCategory = 'Tome','Changes should have been saved',Sender.Tag);
  if not VerifyProjectEvents(['<LoadingProjectPropertiesFile>',
                              '<ProjectPropertiesFileLoaded>',
                              '<ProjectPropertiesDataReceived>',
                              '<SavingProjectPropertiesFile>',
                              '<ProjectPropertiesFileSaved>',
                              '<ProjectPropertiesDataReceived>'],Sender.Tag) then
    Exit;
  EndAsync(Sender.Tag);
end;

procedure TProjectSpec.Open_Project_1(Sender: TPromise);
var
  lProject: TStewProject;
begin
  lProject := (Sender as TProjectPromise).Project;
  try
    AssertAsync(lProject <> nil,'Project should have been opened',Sender.Tag);
    AssertAsync(lProject.DiskPath = fTestRootDir,'Project should have been opened at the correct path',Sender.Tag);
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

procedure TProjectSpec.Test_Document_Properties;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.Open(fTestRootDir).After(@Document_Properties_1,@PromiseFailed).Tag := BeginAsync;
end;

end.

