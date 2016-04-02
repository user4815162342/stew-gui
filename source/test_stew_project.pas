unit test_stew_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_file, sys_async, stew_project, sys_types;

type

  { TTestObserver }

  TTestObserver = class
  private
    fProject: TStewProject;
  public
    Flag: Boolean;
    constructor Create(aProject: TStewProject);
    procedure ObserveProject(Sender: TStewProject; {%H-}Event: TProjectEvent);
    procedure StopObserving;
    destructor Destroy; override;
  end;

  TProjectEventArray = array of String;
  { TProjectSpec }

  TProjectSpec = class(TTestSpec)
    procedure EditChooseAttachment(Sender: TObject; Document: TDocumentPath;
      AttachmentName: String; aChoices: TStringArray; var Answer: Integer; out
      Accepted: Boolean);
    procedure Edit_5(Sender: TPromise);
    procedure Edit_6(Sender: TPromise);
    procedure Edit_7(Sender: TPromise);
    procedure RemoveObservers_1(Sender: TPromise);
    procedure RemoveObservers_2(Sender: TPromise);
  private
    fTempDir: String;
    fTestRootDir: TFile;
    fProject: TStewProject;
    fProjectEvents: TProjectEventArray;
    fEditorCalledFlag: Boolean;
    fNewAttachmentConfirmedFlag: Boolean;
    fTemplateChosenFlag: Boolean;
    fAttachmentChosenFlag: Boolean;
    fObserverA: TTestObserver;
    fObserverB: TTestObserver;
    fObserverC: TTestObserver;
    procedure ClearProjectEvents;
    function VerifyProjectEvents(aExpected: array of String; aTestID: Integer): Boolean;
    procedure ObserveProject(Sender: TStewProject; Event: TProjectEvent);
    procedure Open_Project_1(Sender: TPromise);
    procedure Open_Project_2(Sender: TPromise);
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
    procedure Edit_1(Sender: TPromise);
    procedure Edit_2(Sender: TPromise);
    procedure MockEditor(aFile: TFile; out aEdited: Boolean);
    procedure EditConfirmNewAttachment(Sender: TObject;
      Document: TDocumentPath; AttachmentName: String; out Answer: Boolean);
    procedure Edit_3(Sender: TPromise);
    procedure EditChooseTemplate(Sender: TObject; Document: TDocumentPath;
      AttachmentName: String; aChoices: TStringArray; var Answer: Integer; out
      Accepted: Boolean);

    procedure Edit_4(Sender: TPromise);
  public
    procedure SetupTest; override;
    procedure CleanupTest; override;
  published
    procedure Test_01_DocumentPath;
    procedure Test_02_Open_Project;
    procedure Test_03_Project_Properties;
    procedure Test_04_Document_Properties;
    procedure Test_05_Document_List;
    procedure Test_06_Document_Synopsis;
    procedure Test_07_Document_IsFolder;
    procedure Test_08_Shadows;
    procedure Test_09_Shift;
    procedure Test_10_Rename;
    procedure Test_11_Edit;
    procedure Test_12_RemoveObservers;
  end;


implementation

uses
  gui_async, FileUtil, sys_localfile, stew_properties, Graphics, sys_os;

{ TTestObserver }

procedure TTestObserver.ObserveProject(Sender: TStewProject;
  Event: TProjectEvent);
begin
  Flag := true;
end;

constructor TTestObserver.Create(aProject: TStewProject);
begin
  inherited Create;
  Flag := false;
  fProject := aProject;
  fProject.AddObserver(@ObserveProject);
end;

procedure TTestObserver.StopObserving;
begin
   fProject.RemoveObserver(@ObserveProject);

end;

destructor TTestObserver.Destroy;
begin
   StopObserving;
  inherited Destroy;
end;

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
  lNew: TFile;
begin
  lProject := (Sender as TProjectPromise).Project;
  try
    if not AssertAsync(lProject <> nil,'Project should have been opened in parent',Sender.Tag) then Exit;;
    if not AssertAsync(lProject.DiskPath = fTestRootDir,'Open in parent should have been opened at the correct path',Sender.Tag) then Exit;
  finally
    lProject.Free;
  end;

  lNew := LocalFile(CreateTemporaryDirectory);
  lProject := TStewProject.Create(lNew);
  try
    if not AssertAsync(lProject.DiskPath = lNew,'New project should have been opened at the correct path',Sender.Tag) then Exit;
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
  lProps: TProjectProperties;
begin
  lProps := (Sender as TProjectPropertiesPromise).Properties;
  // yes, these are taken directly from the test_stew_properties.
  AssertAsync(Length(lProps.Categories.keys) > 0,'Combination of parsing and GetPath should have gotten some data',Sender.Tag);
  AssertAsync((lProps.Categories.Get('Chapter') as TCategoryDefinition).PublishTitleLevel = 0,'Project category definitions should work',Sender.Tag);
  AssertAsync(lProps.Categories.hasOwnProperty('Scene'),'Project categories should work',Sender.Tag);
  if not AssertAsync(lProps.DefaultCategory = 'Chapter','Project default category should work',Sender.Tag) then Exit;
  if not AssertAsync(lProps.DefaultDocExtension = 'txt','Project default doc extension should work',Sender.Tag) then Exit;
  // (sic) I want the default notes extension to be weird, for template testing later.
  if not AssertAsync(lProps.DefaultNotesExtension = 'tst','Project default notes extension should work',Sender.Tag) then Exit;
  if not AssertAsync(lProps.DefaultStatus = 'Unwritten','Project default status should work',Sender.Tag) then Exit;
  if not AssertAsync(lProps.DefaultThumbnailExtension = 'png','Project default thumbnail extension should work',Sender.Tag) then Exit;
  AssertAsync((lProps.Statuses.Get('Unwritten') as TStatusDefinition).Color = clRed,'Project statuses should work',Sender.Tag);
  lProps.DefaultDocExtension := '.doc';
  if not AssertAsync(lProps.DefaultDocExtension = 'doc','Default doc extension should trim off the "." when assigning',Sender.Tag) then Exit;
  // change it back to 'txt' since I need it for another test.
  lProps.DefaultDocExtension:='.txt';
  if not AssertAsync(lProps.DefaultDocExtension = 'txt','Default doc extension should trim off the "." when assigning',Sender.Tag) then Exit;

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
  lProps: TDocumentProperties;
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
  if not VerifyProjectEvents(['<LoadingAttachmentFile> "/Chapter 1" <property file> "_properties.json"',
                              '<AttachmentFileLoaded> "/Chapter 1" <property file> "_properties.json"',
                              '<AttachmentDataReceived> "/Chapter 1" <property file> "_properties.json"',
                              '<SavingAttachmentFile> "/Chapter 1" <property file> "_properties.json"',
                              '<AttachmentFileSaved> "/Chapter 1" <property file> "_properties.json"',
                              '<AttachmentDataReceived> "/Chapter 1" <property file> "_properties.json"'],Sender.Tag) then
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
'<LoadingAttachmentFile> "/" <property file> "_properties.json"',
'<AttachmentFileLoaded> "/" <property file> "_properties.json"',
'<AttachmentDataReceived> "/" <property file> "_properties.json"',
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
    '<LoadingAttachmentFile> "/Chapter 1" <property file> "_properties.json"',
    '<AttachmentFileLoaded> "/Chapter 1" <property file> "_properties.json"',
    '<AttachmentDataReceived> "/Chapter 1" <property file> "_properties.json"',
    '<DocumentListDataReceived> "/Chapter 1" [ "/Chapter 1/Notes" <SHADOW>]'
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
'<LoadingAttachmentFile> "/" <property file> "_properties.json"',
'<AttachmentFileLoaded> "/" <property file> "_properties.json"',
'<AttachmentDataReceived> "/" <property file> "_properties.json"',
'<SavingAttachmentFile> "/" <property file> "_properties.json"',
'<AttachmentFileSaved> "/" <property file> "_properties.json"',
'<DocumentShifted> "/Notes"',
'<AttachmentDataReceived> "/" <property file> "_properties.json"',
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
  if not VerifyProjectEvents([
         '<RenamingDocument> "/Epilogue" TO: /Chapter 6',
         '<ListingDocumentFiles> "/"',
         '<DocumentFilesListed> "/"',
         '<DocumentRenamed> "/Epilogue" TO: /Chapter 6',
         '<ListingDocumentFiles> "/"',
         '<DocumentFilesListed> "/"',
         '<LoadingAttachmentFile> "/" <property file> "_properties.json"',
         '<AttachmentFileLoaded> "/" <property file> "_properties.json"',
         '<AttachmentDataReceived> "/" <property file> "_properties.json"',
         '<DocumentListDataReceived> "/" [ "/Chapter 1", "/Chapter 2", "/Chapter 3", "/Chapter 4", "/Notes" <FOLDER>, "/Chapter 5", "/Chapter 6"]',
         '<RenamingDocument> "/Chapter 6" TO: /Notes/Chapter 6',
         '<ListingDocumentFiles> "/"',
         '<DocumentFilesListed> "/"',
         '<DocumentRenamed> "/Chapter 6" TO: /Notes/Chapter 6',
         '<ListingDocumentFiles> "/"',
         '<DocumentFilesListed> "/"',
         '<LoadingAttachmentFile> "/" <property file> "_properties.json"',
         '<AttachmentFileLoaded> "/" <property file> "_properties.json"',
         '<AttachmentDataReceived> "/" <property file> "_properties.json"',
         '<DocumentListDataReceived> "/" [ "/Chapter 1", "/Chapter 2", "/Chapter 3", "/Chapter 4", "/Notes" <FOLDER>, "/Chapter 5"]',
         '<ListingDocumentFiles> "/Notes"',
         '<DocumentFilesListed> "/Notes"',
         '<LoadingAttachmentFile> "/Notes" <property file> "_properties.json"',
         '<AttachmentFileLoaded> "/Notes" <property file> "_properties.json"',
         '<AttachmentDataReceived> "/Notes" <property file> "_properties.json"',
         '<DocumentListDataReceived> "/Notes" [ "/Notes/Chapter 6", "/Notes/Characters" <FOLDER>]'
     ],Sender.Tag) then Exit;
  EndAsync(Sender.Tag);
end;

procedure TProjectSpec.Edit_1(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fEditorCalledFlag := false;
  fProject.AddObserver(@ObserveProject);
  fProject.OnConfirmNewAttachment:=@EditConfirmNewAttachment;
  fProject.OnChooseTemplate:=@EditChooseTemplate;
  fProject.OnChooseAttachment:=@EditChooseAttachment;
  // This one should have *just* one existing file available,
  // so it should work...
  lDocument := TDocumentPath.Root.GetContainedDocument('Chapter 1');
  fProject.EditDocument(lDocument).After(@Edit_2,@PromiseFailed).Tag := Sender.Tag;

end;

procedure TProjectSpec.Edit_2(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  if not AssertAsync(fEditorCalledFlag,'Editor was not called',Sender.Tag) then Exit;
  fEditorCalledFlag := false;
  fNewAttachmentConfirmedFlag:=false;
  fTemplateChosenFlag:=false;
  lDocument := TDocumentPath.Root.GetContainedDocument('Chapter 2');
  fProject.EditDocument(lDocument).After(@Edit_3,@PromiseFailed).Tag := Sender.Tag;

end;

procedure TProjectSpec.MockEditor(aFile: TFile; out aEdited: Boolean);
begin
  Report('Editor requested for ' + aFile.ID);
  fEditorCalledFlag := true;
  aEdited := true;
end;

procedure TProjectSpec.EditConfirmNewAttachment(Sender: TObject;
  Document: TDocumentPath; AttachmentName: String; out Answer: Boolean);
begin
  // always confirm...
  Report('Confirming new attachment for ' + Document.ID + ': ' + AttachmentName);
  Answer := true;
  fNewAttachmentConfirmedFlag := true;
end;

procedure TProjectSpec.Edit_3(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  if not AssertAsync(fEditorCalledFlag,'Editor was not called',Sender.Tag) then Exit;
  fEditorCalledFlag := false;
  if not AssertAsync(fNewAttachmentConfirmedFlag,'New attachment confirmation was not called',Sender.Tag) then Exit;
  fNewAttachmentConfirmedFlag := false;
  if not AssertAsync(not fTemplateChosenFlag,'Template choice should not have been presented',Sender.Tag) then Exit;
  fTemplateChosenFlag:=false;
  lDocument := TDocumentPath.Root.GetContainedDocument('Chapter 2');
  fProject.EditDocumentThumbnail(lDocument).After(@Edit_4,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.EditChooseTemplate(Sender: TObject;
  Document: TDocumentPath; AttachmentName: String; aChoices: TStringArray;
  var Answer: Integer; out Accepted: Boolean);
var
  i: Integer;
  lMessage: String;
begin
  // always confirm...
  lMessage := 'Choosing template for ' + Document.ID + ': ' + AttachmentName + ' from: [';
  for i := 0 to Length(aChoices) - 1 do
  begin
    if i > 0 then
      lMessage := lMessage + ', ';
    lMessage := lMessage + aChoices[i];
  end;
  lMessage := lMessage + ']';
  Report(lMessage);
  Answer := 0;
  Accepted := true;
  fTemplateChosenFlag := true;
end;

procedure TProjectSpec.Edit_4(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  if not AssertAsync(fEditorCalledFlag,'Editor was not called',Sender.Tag) then Exit;
  fEditorCalledFlag := false;
  if not AssertAsync(fNewAttachmentConfirmedFlag,'New attachment confirmation was not called',Sender.Tag) then Exit;
  fNewAttachmentConfirmedFlag := false;
  if not AssertAsync(fTemplateChosenFlag,'Template choice was not presented',Sender.Tag) then Exit;
  fTemplateChosenFlag:=false;
  lDocument := TDocumentPath.Root.GetContainedDocument('Chapter 2');
  fProject.EditDocumentNotes(lDocument).After(@Edit_5,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.EditChooseAttachment(Sender: TObject;
  Document: TDocumentPath; AttachmentName: String; aChoices: TStringArray;
  var Answer: Integer; out Accepted: Boolean);
var
  i: Integer;
  lMessage: String;
begin
  // always confirm...
  lMessage := 'Choosing attachment for ' + Document.ID + ': ' + AttachmentName + ' from: [';
  for i := 0 to Length(aChoices) - 1 do
  begin
    if i > 0 then
      lMessage := lMessage + ', ';
    lMessage := lMessage + aChoices[i];
  end;
  lMessage := lMessage + ']';
  Report(lMessage);
  Answer := 0;
  Accepted := true;
  fAttachmentChosenFlag := true;
end;

procedure TProjectSpec.Edit_5(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  if not AssertAsync(fEditorCalledFlag,'Editor was not called',Sender.Tag) then Exit;
  fEditorCalledFlag := false;
  if not AssertAsync(fNewAttachmentConfirmedFlag,'New attachment confirmation was not called',Sender.Tag) then Exit;
  fNewAttachmentConfirmedFlag := false;
  if not AssertAsync(not fTemplateChosenFlag,'Template choice should not have been presented',Sender.Tag) then Exit;
  fTemplateChosenFlag:=false;
  fAttachmentChosenFlag := false;
  lDocument := TDocumentPath.Root.GetContainedDocument('Notes').GetContainedDocument('Characters').GetContainedDocument('Jack');
  fProject.EditDocument(lDocument).After(@Edit_6,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.Edit_6(Sender: TPromise);
var
  lDocument: TDocumentPath;
begin
  if not AssertAsync(fEditorCalledFlag,'Editor was not called',Sender.Tag) then Exit;
  fEditorCalledFlag := false;
  if not AssertAsync(not fNewAttachmentConfirmedFlag,'New attachment confirmation should not have been called',Sender.Tag) then Exit;
  fNewAttachmentConfirmedFlag := false;
  if not AssertAsync(not fTemplateChosenFlag,'Template choice should not have been presented',Sender.Tag) then Exit;
  fTemplateChosenFlag:=false;
  if not AssertAsync(fAttachmentChosenFlag,'Attachment choice should have been presented',Sender.Tag) then Exit;
  fAttachmentChosenFlag := false;
  lDocument := TDocumentPath.Root.GetContainedDocument('Notes').GetContainedDocument('Characters').GetContainedDocument('Jack');
  fProject.EditDocumentNotes(lDocument).After(@Edit_7,@PromiseFailed).Tag := Sender.Tag;

end;

procedure TProjectSpec.Edit_7(Sender: TPromise);
begin
  if not AssertAsync(fEditorCalledFlag,'Editor was not called',Sender.Tag) then Exit;
  fEditorCalledFlag := false;
  if not AssertAsync(not fNewAttachmentConfirmedFlag,'New attachment confirmation should not have been called',Sender.Tag) then Exit;
  fNewAttachmentConfirmedFlag := false;
  if not AssertAsync(not fTemplateChosenFlag,'Template choice should not have been presented',Sender.Tag) then Exit;
  fTemplateChosenFlag:=false;
  if not AssertAsync(not fAttachmentChosenFlag,'Attachment choice should not have been presented',Sender.Tag) then Exit;
  fAttachmentChosenFlag := false;
  if not VerifyProjectEvents([
          '<ListingDocumentFiles> "/"',
          '<DocumentFilesListed> "/"',
          '<EditingAttachment> "/Chapter 1" <primary> "_.txt"',
          '<LoadingProjectPropertiesFile>',
          '<ProjectPropertiesFileLoaded>',
          '<ProjectPropertiesDataReceived>',
          '<EditingAttachment> "/Chapter 2" <primary> "_.txt"',
          '<ProjectPropertiesDataReceived>',
          '<EditingAttachment> "/Chapter 2" <thumbnail> "_thumbnail.png"',
          '<ProjectPropertiesDataReceived>',
          '<EditingAttachment> "/Chapter 2" <note> "_notes.tst"',
          '<ListingDocumentFiles> "/Notes/Characters"',
          '<DocumentFilesListed> "/Notes/Characters"',
          '<ProjectPropertiesDataReceived>',
          '<EditingAttachment> "/Notes/Characters/Jack" <primary> "_.doc"',
          '<ProjectPropertiesDataReceived>',
          '<EditingAttachment> "/Notes/Characters/Jack" <note> "_notes.tst"'
  ],Sender.Tag) then Exit;
  EndAsync(Sender.Tag);

end;

procedure TProjectSpec.RemoveObservers_1(Sender: TPromise);
begin
  // So, these observers basically set a flag to true when the project
  // emits an event to observe, this is a way we can be sure that
  // the observation occurred.
  // Now, before we add the third observer, we are removing the second.
  // If everything goes right, then after the observe, the result should
  // be changed on A and C, but not B, because that one isn't observing.
  fProject := (Sender as TProjectPromise).Project;
  ClearProjectEvents;
  fObserverA := TTestObserver.Create(fProject);
  fObserverB := TTestObserver.Create(fProject);
  fObserverB.StopObserving;
  fObserverC := TTestObserver.Create(fProject);
  // This is where the problem was. Apparently, when two method pointers are compared,
  // only the code is compared, not the data.
  // http://stackoverflow.com/a/1027201/300213
  //Assert(@fObserverA.ObserveProject <> @fObserverB.ObserveProject,'Two method pointers should not be equal');
  fProject.ReadProjectProperties.After(@RemoveObservers_2,@PromiseFailed).Tag := Sender.Tag;
end;

procedure TProjectSpec.RemoveObservers_2(Sender: TPromise);
begin
  if not AssertAsync((fObserverA.Flag) and
              (not fObserverB.Flag) and
              (fObserverC.Flag),'When removing observers, the correct observer must be removed',Sender.Tag) then
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
    if not AssertAsync(lProject <> nil,'Project should have been opened',Sender.Tag) then Exit;
    if not AssertAsync(lProject.DiskPath = fTestRootDir,'Project should have been opened at the correct path',Sender.Tag) then Exit;
  finally
    lProject.Free;
  end;
  TStewProject.CheckExistenceInParentAndCreate(fTestRootDir.GetContainedFile('Notes')).After(@Open_Project_2,@PromiseFailed).Tag := Sender.Tag;

end;

procedure TProjectSpec.SetupTest;
begin
  inherited SetupTest;
  SetAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
  fTempDir := CopyTemporaryFileData('../test-data/story/');
  fTestRootDir := LocalFile(fTempDir);
end;

procedure TProjectSpec.CleanupTest;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  RemoveAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
  TOperatingSystemInterface.RemoveInternalEditor(@MockEditor);
  TOperatingSystemInterface.RemoveTemplateFolder(LocalFile('../test-data/templates'));
  TOperatingSystemInterface.UseSystemTemplates:=true;
  inherited CleanupTest;
end;

procedure TProjectSpec.Test_01_DocumentPath;
var
  lSplit: TStringArray;
  lDocument: TDocumentPath;
  lFile: TFile;
begin
  lDocument := TDocumentPath.Root;
  lSplit := lDocument.Split;
  Assert(Length(lSplit) = 0,'Split of root should be 0');
  lDocument := TDocumentPath.Root.GetContainedDocument('Foo');
  lSplit := lDocument.Split;
  Assert(Length(lSplit) = 1,'Split of single path should be 1');
  Assert(lSplit[0] = 'Foo','Split should work right [1]');
  lDocument := TDocumentPath.Root.GetContainedDocument('Foo').GetContainedDocument('Bar');
  lSplit := lDocument.Split;
  Assert(Length(lSplit) = 2,'Split should work right [2]');
  Assert(lSplit[0] = 'Foo','Split should work right [3]');
  Assert(lSplit[1] = 'Bar','Split should work right [4]');
  lDocument := TDocumentPath.Root.GetContainedDocument('Foo').GetContainedDocument('Bar').GetContainedDocument('Fooze');
  lSplit := lDocument.Split;
  Assert(Length(lSplit) = 3,'Split should work right [5]');
  Assert(lSplit[0] = 'Foo','Split should work right [6]');
  Assert(lSplit[1] = 'Bar','Split should work right [7]');
  Assert(lSplit[2] = 'Fooze','Split should work right [8]');

  lDocument := TDocumentPath.Root;
  lFile := lDocument.ToFile(fTestRootDir,'','');
  Assert(lFile = fTestRootDir,'No extension or descriptor on a root document should return the base path.');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [1]');
  lFile := lDocument.ToFile(fTestRootDir,'stew','');
  Assert(lFile = fTestRootDir.GetContainedFile('_stew'),'A single descriptor on a root document should return a file inside the base path.');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [2]');
  lFile := lDocument.ToFile(fTestRootDir,'','txt');
  Assert(lFile = fTestRootDir.GetContainedFile('.txt'),'A single extension on a root document should return a file inside the base path.');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [3]');
  lFile := lDocument.ToFile(fTestRootDir,'stew','txt');
  Assert(lFile = fTestRootDir.GetContainedFile('_stew.txt'),'A descriptor and extension on a root document should return a file inside the base path.');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [4]');

  lDocument := lDocument.GetContainedDocument('Test');
  lFile := lDocument.ToFile(fTestRootDir,'','');
  Assert(lFile = fTestRootDir.GetContainedFile('Test'),'No extension or descriptor on a document should return a file in the base path.');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [5]');
  lFile := lDocument.ToFile(fTestRootDir,'stew','');
  Assert(lFile = fTestRootDir.GetContainedFile('Test_stew'),'A single descriptor on a document should return a file inside the base path.');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [6]');
  lFile := lDocument.ToFile(fTestRootDir,'','txt');
  Assert(lFile = fTestRootDir.GetContainedFile('Test.txt'),'A single extension on a document should return a file inside the base path.');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [7]');
  lFile := lDocument.ToFile(fTestRootDir,'stew','txt');
  Assert(lFile = fTestRootDir.GetContainedFile('Test_stew.txt'),'A descriptor and extension on a document should return a file inside the base path.');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [8]');

  lDocument := TDocumentPath.Root.GetContainedDocument('II. Test');
  lFile := lDocument.ToFile(fTestRootDir,'','');
  Assert(lFile = fTestRootDir.GetContainedFile('II. Test.'),'Periods in filenames should work [1]');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [9]');
  lFile := lDocument.ToFile(fTestRootDir,'stew','');
  Assert(lFile = fTestRootDir.GetContainedFile('II. Test_stew.'),'Periods in filenames should work [2]');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [10]');
  lFile := lDocument.ToFile(fTestRootDir,'','txt');
  Assert(lFile = fTestRootDir.GetContainedFile('II. Test.txt'),'Periods in filenames should work [3]');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [11]');
  lFile := lDocument.ToFile(fTestRootDir,'stew','txt');
  Assert(lFile = fTestRootDir.GetContainedFile('II. Test_stew.txt'),'Periods in filenames should work [4]');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [12]');

  lDocument := TDocumentPath.Root.GetContainedDocument('II_ Test');
  lFile := lDocument.ToFile(fTestRootDir,'','');
  Assert(lFile = fTestRootDir.GetContainedFile('II_ Test_'),'Underscores in filenames should work [1]');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [13]');
  lFile := lDocument.ToFile(fTestRootDir,'stew','');
  Assert(lFile = fTestRootDir.GetContainedFile('II_ Test_stew'),'Underscores in filenames should work [2]');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [14]');
  lFile := lDocument.ToFile(fTestRootDir,'','txt');
  Assert(lFile = fTestRootDir.GetContainedFile('II_ Test_.txt'),'Underscores in filenames should work [3]');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [15]');
  lFile := lDocument.ToFile(fTestRootDir,'stew','txt');
  Assert(lFile = fTestRootDir.GetContainedFile('II_ Test_stew.txt'),'Underscores in filenames should work [4]');
  Assert(TDocumentPath.FromFile(fTestRootDir,lFile) = lDocument,'Translating back should work right [16]');

  // NOTE: There's a possibility where we've got a file that's been added in, but contains underscores. For
  // example: Text_Summary.txt. This is going to show up as a document called Text, and
  // that "_Summary" attachment will not be seen in the UI. Trying to fix this issue
  // is out of scope at this point.
end;

procedure TProjectSpec.Test_02_Open_Project;
begin
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Open_Project_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_03_Project_Properties;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Project_Properties_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_04_Document_Properties;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Document_Properties_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_05_Document_List;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Document_List_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_06_Document_Synopsis;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Document_Syn_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_07_Document_IsFolder;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Document_IsFolder_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_08_Shadows;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Shadow_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_09_Shift;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Shift_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_10_Rename;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Rename_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_11_Edit;
begin
  if fProject <> nil then
    FreeAndNil(fProject);
  // I don't want it attempting to call xdg-open and getting the dialog again
  // to mess up the test. This mechanism was tested in test_sys_file anyway,
  // so I can mock it.
  TOperatingSystemInterface.AddInternalEditor(@MockEditor);
  // I also want to control the templates more appropriately, again that
  // was tested in test_sys_file, so it shouldn't be a problem.
  TOperatingSystemInterface.UseSystemTemplates := false;
  TOperatingSystemInterface.AddTemplateFolder(LocalFile('../test-data/templates'));
  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@Edit_1,@PromiseFailed).Tag := BeginAsync;
end;

procedure TProjectSpec.Test_12_RemoveObservers;
begin
  if fProject <> nil then
     FreeAndNil(fProject);

  TStewProject.CheckExistenceAndCreate(fTestRootDir).After(@RemoveObservers_1,@PromiseFailed).Tag := BeginAsync;

end;

end.

