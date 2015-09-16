unit test_stew_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_file, sys_async, stew_project;

{
TODO:
- As we go through, make sure we're supporting the non-file events with
  each thing.
- TODO: Make sure all of the 'events' are being notified, or get rid of them.
- Need Edit Document Attachment
  - not absolutely sure we need to test this, although I suppose we're testing
    it at another point where we're getting the xdg-open error.

TODO: In the Async code: It would be nice to have some events that get
passed to the application when tasks are queued. This will allow us to
show a busy signal on the app. Also, another function should force the
mainform to wait until the tasks  are complete before it closes. This does
have the issue of make the application not want to close (which can be
a problem if some operation is hanging), so maybe have a timeout where
the application automatically closes.

TODO: Also in Async code, make sure there's a try...except around running
the code, and allow the application to report any uncaught errors.

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
    procedure Document_Properties_1(Sender: TPromise);
    procedure Document_Properties_2(Sender: TPromise);
    procedure Document_Properties_3(Sender: TPromise);
    procedure Document_Properties_4(Sender: TPromise);
    procedure Document_List_1(Sender: TPromise);
    procedure Document_List_2(Sender: TPromise);
    procedure Document_Syn_1(Sender: TPromise);
    procedure Document_Syn_2(Sender: TPromise);
    procedure Document_Syn_3(Sender: TPromise);
    procedure Document_Syn_4(Sender: TPromise);
    procedure Document_IsFolder_1(Sender: TPromise);
    procedure Document_IsFolder_2(Sender: TPromise);
    procedure Rename_1(Sender: TPromise);
    procedure Rename_2(Sender: TPromise);
    procedure Shadow_1(Sender: TPromise);
    procedure Shadow_2(Sender: TPromise);
    procedure Shadow_3(Sender: TPromise);
    procedure Shift_1(Sender: TPromise);
    procedure Shift_2(Sender: TPromise);
    procedure Shift_3(Sender: TPromise);
    procedure Rename_3(Sender: TPromise);
    procedure Rename_4(Sender: TPromise);
    procedure Rename_5(Sender: TPromise);
    procedure Rename_6(Sender: TPromise);
  public
    procedure SetupTest; override;
    procedure CleanupTest; override;
  published
    // TODO: We have to enforce the *order* that these tests happen in,
    // or do the file stuff before and after each test.
    procedure Test_DocumentPath;
    procedure Test_Open_Project;
    procedure Test_Project_Properties;
    procedure Test_Document_Properties;
    procedure Test_Document_List;
    procedure Test_Document_Synopsis;
    procedure Test_Document_IsFolder;
    procedure Test_Shadows;
    procedure Test_Shift;
    procedure Test_Rename;
  end;


implementation

uses
  gui_async, FileUtil, sys_localfile, stew_properties, Graphics, stew_types;

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
    if not AssertAsync(lProject <> nil,'Project should have been opened in parent',Sender.Tag) then Exit;;
    if not AssertAsync(lProject.DiskPath = fTestRootDir,'Open in parent should have been opened at the correct path',Sender.Tag) then Exit;
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
    if not AssertAsync(lProject <> nil,'New project should have been opened',Sender.Tag) then Exit;
    if not AssertAsync(lProject.DiskPath <> fTestRootDir,'New project should have been opened at the correct path',Sender.Tag) then Exit;
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
  if not AssertAsync(lProps.DefaultCategory = 'Chapter','Project default category should work',Sender.Tag) then Exit;
  if not AssertAsync(lProps.DefaultDocExtension = 'txt','Project default doc extension should work',Sender.Tag) then Exit;
  if not AssertAsync(lProps.DefaultNotesExtension = 'txt','Project default notes extension should work',Sender.Tag) then Exit;
  if not AssertAsync(lProps.DefaultStatus = 'Unwritten','Project default status should work',Sender.Tag) then Exit;
  if not AssertAsync(lProps.DefaultThumbnailExtension = 'png','Project default thumbnail extension should work',Sender.Tag) then Exit;
  AssertAsync((lProps.Statuses.Get('Unwritten') as TStatusDefinition2).Color = clRed,'Project statuses should work',Sender.Tag);
  lProps.DefaultDocExtension := '.doc';
  if not AssertAsync(lProps.DefaultDocExtension = 'doc','Default doc extension should trim off the "." when assigning',Sender.Tag) then Exit;

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
  if not AssertAsync(lProps.Category = 'Chapter','Category should return correct value',Sender.Tag) then Exit;
  if not AssertAsync(lProps.Status = 'Unwritten','Status should return correct value',Sender.Tag) then Exit;
  if not AssertAsync(lProps.Title = 'The Cottage','Title should return correct value',Sender.Tag) then Exit;
  if not AssertAsync(lProps.Publish = false,'Publish should return correct value',Sender.Tag) then Exit;
  if not AssertAsync(lProps.Index.Length = 0,'Index should return correct value',Sender.Tag) then Exit;
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

procedure TProjectSpec.Document_List_1(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fProject.AddObserver(@ObserveProject);
  lDocument := TDocumentPath.Root;
  fProject.ListDocumentsInFolder(lDocument).After(@Document_List_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_List_2(Sender: TPromise);
var
  lDocuments: TDocumentInfoArray;
  l: Integer;
begin
  lDocuments := (Sender as TDocumentListPromise).List;
  l := Length(lDocuments);
  if not AssertAsync(l = 7,'Document list should return 7 items',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[0].Document.Name = 'Chapter 1','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[1].Document.Name = 'Chapter 2','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(not lDocuments[1].IsFolder,'Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[2].Document.Name = 'Chapter 3','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[3].Document.Name = 'Chapter 4','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[4].Document.Name = 'Chapter 5','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[5].Document.Name = 'Epilogue','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[6].Document.Name = 'Notes','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[6].IsFolder,'Document list should return and be correctly sorted',Sender.Tag) then Exit;
  VerifyProjectEvents([
'<ListingDocumentFiles> "/"',
'<DocumentFilesListed> "/"',
'<LoadingAttachmentFile> "/" <properties> "_properties.json"',
'<AttachmentFileLoaded> "/" <properties> "_properties.json"',
'<AttachmentDataReceived> "/" <properties> "_properties.json"',
'<DocumentListDataReceived> "/" [ "/Chapter 1", "/Chapter 2", "/Chapter 3", "/Chapter 4", "/Chapter 5", "/Epilogue", "/Notes" <FOLDER>]'
  ],Sender.Tag);
  EndAsync(Sender.Tag);

end;

procedure TProjectSpec.Document_Syn_1(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fProject.AddObserver(@ObserveProject);
  lDocument := TDocumentPath.Root.GetContainedDocument('Chapter 1');
  fProject.ReadDocumentSynopsis(lDocument).After(@Document_Syn_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_Syn_2(Sender: TPromise);
var
  lSyn: UTF8String;
  lDocument: TDocumentPath;
begin
  lSyn := (Sender as TDocumentSynopsisPromise).Synopsis;
  lDocument := (Sender as TDocumentSynopsisPromise).Document;
  if not AssertAsync(lSyn = 'Jack confesses to his friends that he''s actually a werewolf. While Jen and Joe are completely surprised, Jane responds to the news in a rather unexpected manner.','Synopsis must be read correctly',Sender.Tag) then Exit;
  lSyn := 'Jack confesses. Jen and Joe are suprised. Jane responds.';
  fProject.WriteDocumentSynopsis(lDocument,lSyn).After(@Document_Syn_3,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_Syn_3(Sender: TPromise);
begin
  // re-read it to see if the data changed.
  fProject.ReadDocumentSynopsis((Sender as TWriteAttachmentPromise).Document).After(@Document_Syn_4,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_Syn_4(Sender: TPromise);
var
  lSyn: UTF8String;
begin
  lSyn := (Sender as TDocumentSynopsisPromise).Synopsis;
  if not AssertAsync(lSyn = 'Jack confesses. Jen and Joe are suprised. Jane responds.','Changes should have been saved',Sender.Tag) then Exit;
  if not VerifyProjectEvents(['<LoadingAttachmentFile> "/Chapter 1" <synopsis> "_synopsis.txt"',
                              '<AttachmentFileLoaded> "/Chapter 1" <synopsis> "_synopsis.txt"',
                              '<AttachmentDataReceived> "/Chapter 1" <synopsis> "_synopsis.txt"',
                              '<SavingAttachmentFile> "/Chapter 1" <synopsis> "_synopsis.txt"',
                              '<AttachmentFileSaved> "/Chapter 1" <synopsis> "_synopsis.txt"',
                              '<AttachmentDataReceived> "/Chapter 1" <synopsis> "_synopsis.txt"'],Sender.Tag) then
    Exit;
  EndAsync(Sender.Tag);
end;

procedure TProjectSpec.Document_IsFolder_1(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fProject.AddObserver(@ObserveProject);
  lDocument := TDocumentPath.Root.GetContainedDocument('Notes');
  fProject.IsDocumentAFolder(lDocument).After(@Document_IsFolder_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Document_IsFolder_2(Sender: TPromise);
begin
  AssertAsync((Sender as TDocumentIsFolderPromise).IsFolder,'Document should be a folder',Sender.Tag);
  VerifyProjectEvents([
'<CheckingDocumentFile> "/Notes"',
'<DocumentFileChecked> "/Notes"'
  ],Sender.Tag);
  EndAsync(Sender.Tag);

end;

procedure TProjectSpec.Rename_1(Sender: TPromise);
var
  lOld: TDocumentPath;
  lNew: TDocumentPath;
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fProject.AddObserver(@ObserveProject);
  lOld := TDocumentPath.Root.GetContainedDocument('Epilogue');
  lNew := TDocumentPath.Root.GetContainedDocument('Chapter 6');
  fProject.RenameDocument(lOld,lNew).After(@Rename_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Rename_2(Sender: TPromise);
begin
  fProject.ListDocumentsInFolder(TDocumentPath.Root).After(@Rename_3,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Shadow_1(Sender: TPromise);
var
  lDocument: TDocumentPath;
  lShadow: TDocumentPath;
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fProject.AddObserver(@ObserveProject);
  lDocument := TDocumentPath.Root.GetContainedDocument('Chapter 1');
  lShadow := lDocument.GetContainedDocument('Notes');
  fProject.CreateShadow(lShadow);
  fProject.IsDocumentAFolder(lDocument).After(@Shadow_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Shadow_2(Sender: TPromise);
begin
  if not AssertAsync((Sender as TDocumentIsFolderPromise).IsFolder,'Document should have been a folder containing a shadow',Sender.Tag) then
    Exit;
  fProject.ListDocumentsInFolder((Sender as TDocumentIsFolderPromise).Document).After(@Shadow_3,@PromiseFailed).Tag := Sender.Tag;

end;

procedure TProjectSpec.Shadow_3(Sender: TPromise);
begin
  if not AssertAsync(Length((Sender as TDocumentListPromise).List) = 1,'Shadow should show up in list',Sender.Tag) then
    Exit;
  if not AssertAsync((Sender as TDocumentListPromise).List[0].Document.Name = 'Notes','Shadow should show up in list',Sender.Tag) then
    Exit;
  if not VerifyProjectEvents([
    '<ShadowCreated> "/Chapter 1/Notes"',
    '<CheckingDocumentFile> "/Chapter 1"',
    '<DocumentFileChecked> "/Chapter 1"',
    '<ListingDocumentFiles> "/Chapter 1"',
    '<DocumentFilesListed> "/Chapter 1"',
    '<LoadingAttachmentFile> "/Chapter 1" <properties> "_properties.json"',
    '<AttachmentFileLoaded> "/Chapter 1" <properties> "_properties.json"',
    '<AttachmentDataReceived> "/Chapter 1" <properties> "_properties.json"',
    '<DocumentListDataReceived> "/Chapter 1" [ "/Chapter 1/Notes"]'
  ],Sender.Tag) then
    Exit;
  EndAsync(Sender.Tag);
end;

procedure TProjectSpec.Shift_1(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fProject.AddObserver(@ObserveProject);
  lDocument := TDocumentPath.Root.GetContainedDocument('Notes');
  fProject.ShiftDocumentBy(lDocument,-2).After(@Shift_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Shift_2(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  lDocument := TDocumentPath.Root;
  fProject.ListDocumentsInFolder(lDocument).After(@Shift_3,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Shift_3(Sender: TPromise);
var
  lDocuments: TDocumentInfoArray;
  l: Integer;
begin
  lDocuments := (Sender as TDocumentListPromise).List;
  l := Length(lDocuments);
  if not AssertAsync(l = 7,'Document list should return 7 items',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[0].Document.Name = 'Chapter 1','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[1].Document.Name = 'Chapter 2','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(not lDocuments[1].IsFolder,'Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[2].Document.Name = 'Chapter 3','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[3].Document.Name = 'Chapter 4','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[4].Document.Name = 'Notes','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[4].IsFolder,'Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[5].Document.Name = 'Chapter 5','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[6].Document.Name = 'Epilogue','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  VerifyProjectEvents([
'<ListingDocumentFiles> "/"',
'<DocumentFilesListed> "/"',
'<LoadingAttachmentFile> "/" <properties> "_properties.json"',
'<AttachmentFileLoaded> "/" <properties> "_properties.json"',
'<AttachmentDataReceived> "/" <properties> "_properties.json"',
'<SavingAttachmentFile> "/" <properties> "_properties.json"',
'<AttachmentFileSaved> "/" <properties> "_properties.json"',
'<DocumentShifted> "/Notes"',
'<AttachmentDataReceived> "/" <properties> "_properties.json"',
'<DocumentListDataReceived> "/" [ "/Chapter 1", "/Chapter 2", "/Chapter 3", "/Chapter 4", "/Notes" <FOLDER>, "/Chapter 5", "/Epilogue"]'
  ],Sender.Tag);
  EndAsync(Sender.Tag);


end;

procedure TProjectSpec.Rename_3(Sender: TPromise);
var
  lDocuments: TDocumentInfoArray;
  l: Integer;
begin
  lDocuments := (Sender as TDocumentListPromise).List;
  l := Length(lDocuments);
  if not AssertAsync(l = 7,'Document list should return 7 items',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[0].Document.Name = 'Chapter 1','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[1].Document.Name = 'Chapter 2','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(not lDocuments[1].IsFolder,'Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[2].Document.Name = 'Chapter 3','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[3].Document.Name = 'Chapter 4','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[4].Document.Name = 'Notes','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[4].IsFolder,'Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[5].Document.Name = 'Chapter 5','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[6].Document.Name = 'Chapter 6','Document renme should have worked',Sender.Tag) then Exit;
  // Now, move Chapter 2 into Notes
  fProject.RenameDocument(TDocumentPath.Root.GetContainedDocument('Chapter 6'),
                          TDocumentPath.Root.GetContainedDocument('Notes').GetContainedDocument('Chapter 6')).
                          After(@Rename_4,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Rename_4(Sender: TPromise);
begin
  fProject.ListDocumentsInFolder(TDocumentPath.Root).After(@Rename_5,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Rename_5(Sender: TPromise);
var
  lDocuments: TDocumentInfoArray;
  l: Integer;
begin
  lDocuments := (Sender as TDocumentListPromise).List;
  l := Length(lDocuments);
  if not AssertAsync(l = 6,'Document list should return 6 items',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[0].Document.Name = 'Chapter 1','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[1].Document.Name = 'Chapter 2','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(not lDocuments[1].IsFolder,'Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[2].Document.Name = 'Chapter 3','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[3].Document.Name = 'Chapter 4','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[4].Document.Name = 'Notes','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[4].IsFolder,'Document list should return and be correctly sorted',Sender.Tag) then Exit;
  if not AssertAsync(lDocuments[5].Document.Name = 'Chapter 5','Document list should return and be correctly sorted',Sender.Tag) then Exit;
  // Now, check if it's in Notes.
  fProject.ListDocumentsInFolder(TDocumentPath.Root.GetContainedDocument('Notes')).
                          After(@Rename_6,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Rename_6(Sender: TPromise);
var
  lDocuments: TDocumentInfoArray;
  l: Integer;
  lName: String;
begin
  lDocuments := (Sender as TDocumentListPromise).List;
  l := Length(lDocuments);
  if not AssertAsync(l = 2,'Document list should return 2 items',Sender.Tag) then Exit;
  lName := lDocuments[0].Document.Name;
  if not AssertAsync((lName = 'Chapter 6') or (lName = 'Characters'),'Document should only containe Chapter 6 or Characters',Sender.Tag) then Exit;
  lName := lDocuments[1].Document.Name;
  if not AssertAsync((lName = 'Chapter 6') or (lName = 'Characters'),'Document should only containe Chapter 6 or Characters',Sender.Tag) then Exit;
  // TODO: Verify events...
  if not VerifyProjectEvents([
         '<RenamingDocument> "/Epilogue" TO: /Chapter 6',
         '<ListingDocumentFiles> "/"',
         '<DocumentFilesListed> "/"',
         '<DocumentRenamed> "/Epilogue" TO: /Chapter 6',
         '<ListingDocumentFiles> "/"',
         '<DocumentFilesListed> "/"',
         '<LoadingAttachmentFile> "/" <properties> "_properties.json"',
         '<AttachmentFileLoaded> "/" <properties> "_properties.json"',
         '<AttachmentDataReceived> "/" <properties> "_properties.json"',
         '<DocumentListDataReceived> "/" [ "/Chapter 1", "/Chapter 2", "/Chapter 3", "/Chapter 4", "/Notes" <FOLDER>, "/Chapter 5", "/Chapter 6"]',
         '<RenamingDocument> "/Chapter 6" TO: /Notes/Chapter 6',
         '<DocumentRenamed> "/Chapter 6" TO: /Notes/Chapter 6',
         '<ListingDocumentFiles> "/"',
         '<DocumentFilesListed> "/"',
         '<AttachmentDataReceived> "/" <properties> "_properties.json"',
         '<DocumentListDataReceived> "/" [ "/Chapter 1", "/Chapter 2", "/Chapter 3", "/Chapter 4", "/Notes" <FOLDER>, "/Chapter 5"]',
         '<ListingDocumentFiles> "/Notes"',
         '<DocumentFilesListed> "/Notes"',
         '<LoadingAttachmentFile> "/Notes" <properties> "_properties.json"',
         '<AttachmentFileLoaded> "/Notes" <properties> "_properties.json"',
         '<AttachmentDataReceived> "/Notes" <properties> "_properties.json"',
         '<DocumentListDataReceived> "/Notes" [ "/Notes/Chapter 6", "/Notes/Characters" <FOLDER>]'
     ],Sender.Tag) then Exit;
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
    if not AssertAsync(lProject <> nil,'Project should have been opened',Sender.Tag) then Exit;
    if not AssertAsync(lProject.DiskPath = fTestRootDir,'Project should have been opened at the correct path',Sender.Tag) then Exit;
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

procedure TProjectSpec.Test_DocumentPath;
var
  lPath: TDocumentPath;
  lSplit: TStringArray;
begin
  lPath := TDocumentPath.Root;
  lSplit := lPath.Split;
  Assert(Length(lSplit) = 0,'Split of root should be 0');
  lPath := TDocumentPath.Root.GetContainedDocument('Foo');
  lSplit := lPath.Split;
  Assert(Length(lSplit) = 1,'Split of single path should be 1');
  Assert(lSplit[0] = 'Foo','Split should work right [1]');
  lPath := TDocumentPath.Root.GetContainedDocument('Foo').GetContainedDocument('Bar');
  lSplit := lPath.Split;
  Assert(Length(lSplit) = 2,'Split should work right [2]');
  Assert(lSplit[0] = 'Foo','Split should work right [3]');
  Assert(lSplit[1] = 'Bar','Split should work right [4]');
  lPath := TDocumentPath.Root.GetContainedDocument('Foo').GetContainedDocument('Bar').GetContainedDocument('Fooze');
  lSplit := lPath.Split;
  Assert(Length(lSplit) = 3,'Split should work right [5]');
  Assert(lSplit[0] = 'Foo','Split should work right [6]');
  Assert(lSplit[1] = 'Bar','Split should work right [7]');
  Assert(lSplit[2] = 'Fooze','Split should work right [8]');

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

procedure TProjectSpec.Test_Document_List;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.Open(fTestRootDir).After(@Document_List_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_Document_Synopsis;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.Open(fTestRootDir).After(@Document_Syn_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_Document_IsFolder;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.Open(fTestRootDir).After(@Document_IsFolder_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_Shadows;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.Open(fTestRootDir).After(@Shadow_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_Shift;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.Open(fTestRootDir).After(@Shift_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_Rename;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.Open(fTestRootDir).After(@Rename_1,@PromiseFailed).Tag := BeginAsync;
end;

end.

