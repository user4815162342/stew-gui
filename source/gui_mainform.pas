unit gui_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, ComCtrls, stew_project, fgl, gui_config, contnrs,
  simpleipc, gui_editorframe, stew_types, sys_async, sys_file, sys_os;

type

  TMainFormAction = (mfaProjectPropertiesLoading,
                     mfaProjectPropertiesLoaded,
                     mfaProjectPropertiesSaving,
                     mfaProjectPropertiesSaved,
                     mfaDocumentsListed,
                     mfaDocumentPropertiesLoading,
                     mfaDocumentPropertiesLoaded,
                     mfaDocumentPropertiesSaving,
                     mfaDocumentPropertiesSaved,
                     mfaDocumentSynopsisLoaded,
                     mfaDocumentSynopsisLoading,
                     mfaDocumentSynopsisSaving,
                     mfaDocumentSynopsisSaved,
                     mfaDocumentSynopsisSaveConflicted,
                     mfaDocumentChanged,
                     mfaDocumentCreated);
  TMainFormObserverHandler = procedure(aAction: TMainFormAction; aDocument: TDocumentID) of object;
  TMainFormObserverList = specialize TFPGList<TMainFormObserverHandler>;

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    RecentProjectsMenuItem: TMenuItem;
    OpenProjectMenuItem: TMenuItem;
    NewProjectMenuItem: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    PreferencesMenuItem: TMenuItem;
    ProjectSettingsMenuItem: TMenuItem;
    RefreshProjectMenuItem: TMenuItem;
    ProjectMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    OpenProjectDialog: TSelectDirectoryDialog;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure DoChooseNewAttachmentTemplate(Sender: TObject;
      {%H-}Document: TDocumentID; AttachmentName: String; aChoices: TStringArray;
      var Answer: String; out Accepted: Boolean);
    procedure DoConfirmNewAttachment(Sender: TObject; {%H-}Document: TDocumentID;
      AttachmentName: String; out Answer: Boolean);
    procedure DocumentAttachmentLoading(Sender: TObject; Document: TDocumentID;
      AttachmentName: String);
    procedure DocumentAttachmentSaveConflicted(Sender: TObject;
      Document: TDocumentID; AttachmentName: String);
    procedure DocumentAttachmentSaved(Sender: TObject; Document: TDocumentID;
      AttachmentName: String);
    procedure DocumentAttachmentSaving(Sender: TObject; Document: TDocumentID;
      AttachmentName: String);
    procedure DocumentChanged(Sender: TObject; Document: TDocumentID);
    procedure DocumentCreated(Sender: TObject; Document: TDocumentID);
    procedure DocumentListError(Sender: TObject; Document: TDocumentID;
      Error: String);
    procedure DocumentPropertiesLoading(Sender: TObject; Document: TDocumentID);
    procedure DocumentPropertiesSaving(Sender: TObject; Document: TDocumentID);
    procedure DocumentRenameFailed(Sender: TObject; Document: TDocumentID;
      Error: String);
    procedure DocumentsListed(Sender: TObject; Document: TDocumentID);
    procedure DocumentAttachmentLoaded(Sender: TObject; Document: TDocumentID;
      Attachment: String);
    procedure DocumentAttachmentError(Sender: TObject; Document: TDocumentID;
      Attachment: String; Error: String);
    procedure DocumentTabCloseRequested(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewProjectMenuItemClick(Sender: TObject);
    procedure OpenProjectMenuItemClick(Sender: TObject);
    procedure PreferencesMenuItemClick(Sender: TObject);
    procedure ProjectPropertiesLoading(Sender: TObject);
    procedure ProjectPropertiesSaving(Sender: TObject);
    procedure ProjectSettingsMenuItemClick(Sender: TObject);
    procedure RefreshProjectMenuItemClick(Sender: TObject);
  strict private type
    TProtectedStewProject = class(TStewProject)
    end;

  private
    { private declarations }
    fProject: TProtectedStewProject;
    fConfig: TStewApplicationConfig;
    fObservers: TMainFormObserverList;
    fFrames: array[TAlign] of TControl;
    fSplitters: array[TAlign] of TControl;
    fDocumentPane: TAlign;
    // FUTURE: Should be a hash list, so we can look things up by ID.
    fOpenDocuments: TObjectList;
    procedure DocumentPropertiesError(Sender: TObject; Document: TDocumentID;
      Error: String);
    procedure DocumentPropertiesLoaded(Sender: TObject; Document: TDocumentID);
    procedure DocumentPropertiesSaveConflicted(Sender: TObject;
      Document: TDocumentID);
    procedure DocumentPropertiesSaved(Sender: TObject; Document: TDocumentID);
    function GetProject: TStewProject;
    procedure ProjectLoadFailed(E: String);
    procedure ProjectOpened(Sender: TObject);
    procedure ProjectPropertiesError(Sender: TObject; aError: String);
    procedure ProjectPropertiesLoaded(Sender: TObject);
    procedure ProjectPropertiesSaveConflicted(Sender: TObject);
    procedure ProjectPropertiesSaved(Sender: TObject);
  protected
    procedure StartupCheckProject({%H-}Data: PtrInt);
    procedure StartupIfProjectExists(aValue: Boolean);
    procedure StartupIfProjectParentDirectoryExists(aValue: Boolean);
    procedure NotifyObservers(aAction: TMainFormAction; aDocument: TDocumentID);
    procedure ReadUISettings;
    procedure WriteUISettings;
    procedure LayoutFrames;
  public
    { public declarations }
    property Project: TStewProject read GetProject;
    function OpenDocument(aDocument: TDocumentID): TEditorFrame;
    procedure OpenPreferences;
    procedure OpenProjectSettings;
    procedure Observe(aObserver: TMainFormObserverHandler);
    procedure Unobserve(aObserver: TMainFormObserverHandler);
    procedure RequestTabClose(aFrame: TEditorFrame);
    // NOTE: I've made these public to make it easier to 'extend' the application,
    // but these layout functions really should only be used during app initialization
    // for now.
    function LayoutFrame(aFrameClass: TControlClass; aLocation: TAlign): TControl;
    function LayoutTabs(aLocation: TAlign): TControl;
    procedure LayoutDocumentPane(aLocation: TAlign);
  end;

  { TMRUMenuItem }

  TMRUMenuItem = class(TMenuItem)
    procedure MRUProjectClicked(Sender: TObject);
  private
    fProjectPath: TFile;
  public
    constructor Create(TheOwner: TComponent); override;
    property ProjectPath: TFile read fProjectPath write fProjectPath;
  end;

  { TAsyncCallback }

  TAsyncCallback = class
    procedure Callback({%H-}Data: PtrInt);
  private
    fCallback: TDeferredCallback;
  public
    constructor Create(aCallback: TDeferredCallback);
    procedure Enqueue;
  end;

  procedure QueueAsyncCall(aCallback: TDeferredCallback);
  procedure RunNewStewInstance(const aProjectPath: TFile);
  procedure RunNewStewInstanceWithPrompt;

var
  MainForm: TMainForm;

const
  PromptForProjectArgument = '--prompt-for-project';
  VersionArgument = '--version';

implementation

uses
  gui_projectmanager, gui_documenteditor, gui_preferenceseditor, gui_projectsettingseditor, LCLProc, gui_about, gui_listdialog, sys_localfile;

{$R *.lfm}

// using ':' at the beginning because that should not be a valid filename.
const PreferencesDocumentID: UTF8String = 'preferences';
const ProjectSettingsDocumentID: UTF8String = 'project settings';

procedure QueueAsyncCall(aCallback: TDeferredCallback);
begin
  TAsyncCallback.Create(aCallback).Enqueue;
end;

procedure RunNewStewInstance(const aProjectPath: TFile);
begin
  try
    RunDetachedProcess(Application.ExeName,[aProjectPath.ID]);

  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure RunNewStewInstanceWithPrompt;
begin
  try
     RunDetachedProcess(Application.ExeName,[PromptForProjectArgument]);
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

{ TMRUMenuItem }

procedure TMRUMenuItem.MRUProjectClicked(Sender: TObject);
begin
  RunNewStewInstance(ProjectPath);
end;

constructor TMRUMenuItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnClick:=@MRUProjectClicked;
end;

{ TAsyncCallback }

procedure TAsyncCallback.Callback(Data: PtrInt);
begin
  fCallback;
  Free;
end;

constructor TAsyncCallback.Create(aCallback: TDeferredCallback);
begin
  inherited Create;
  fCallback := aCallback;
end;

procedure TAsyncCallback.Enqueue;
begin
  // Notice that I'm sending 0 as the data parameter to the Deferred call. Rather
  // then dealing with pointers, I've already got a pointer to the object in the
  // method, so I can store the data on there.
  Application.QueueAsyncCall(@Callback,0);
end;

{ TMainForm }

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.DoChooseNewAttachmentTemplate(Sender: TObject;
  Document: TDocumentID; AttachmentName: String; aChoices: TStringArray;
  var Answer: String; out Accepted: Boolean);
begin
  Accepted := ChoiceQuery('There are multiple templates available for your new ' + AttachmentName + ' file.' + LineEnding +
                          'Please specify which one you would like to use:', aChoices,Answer);
end;

procedure TMainForm.DoConfirmNewAttachment(Sender: TObject;
  Document: TDocumentID; AttachmentName: String; out Answer: Boolean);
begin
  Answer :=
    MessageDlg('The ' + AttachmentName + ' file for this document does not exist.' + LineEnding +
               'Do you wish to create a new file?',mtConfirmation,mbYesNo,0) = mrYes;
end;

procedure TMainForm.DocumentAttachmentLoading(Sender: TObject;
  Document: TDocumentID; AttachmentName: String);
begin
  if AttachmentName = 'Synopsis' then
  begin
    NotifyObservers(mfaDocumentSynopsisLoading,Document);
  end;

end;

procedure TMainForm.DocumentAttachmentSaveConflicted(Sender: TObject;
  Document: TDocumentID; AttachmentName: String);
begin
  if AttachmentName = 'Synopsis' then
  begin
    NotifyObservers(mfaDocumentSynopsisSaveConflicted,Document);
  end;

end;

procedure TMainForm.DocumentAttachmentSaved(Sender: TObject;
  Document: TDocumentID; AttachmentName: String);
begin
  if AttachmentName = 'Synopsis' then
  begin
    NotifyObservers(mfaDocumentSynopsisSaved,Document);
  end;

end;

procedure TMainForm.DocumentAttachmentSaving(Sender: TObject;
  Document: TDocumentID; AttachmentName: String);
begin
  if AttachmentName = 'Synopsis' then
  begin
    NotifyObservers(mfaDocumentSynopsisSaving,Document);
  end;

end;

procedure TMainForm.DocumentChanged(Sender: TObject; Document: TDocumentID);
begin
  NotifyObservers(mfaDocumentChanged,Document);
end;

procedure TMainForm.DocumentCreated(Sender: TObject; Document: TDocumentID);
begin
  NotifyObservers(mfaDocumentCreated,Document);
  OpenDocument(Document);
end;

procedure TMainForm.DocumentListError(Sender: TObject; Document: TDocumentID;
  Error: String);
begin
  ShowMessage('An error occurred while listing documents.' + LineEnding +
              'The parent document''s ID was "' + Document.ID + '".' + LineEnding +
              Error + LineEnding +
              'You may want to restart the program, or wait and try your task again later');

end;

procedure TMainForm.DocumentPropertiesLoading(Sender: TObject;
  Document: TDocumentID);
begin
  NotifyObservers(mfaDocumentPropertiesLoading,Document);
end;

procedure TMainForm.DocumentPropertiesSaving(Sender: TObject;
  Document: TDocumentID);
begin
  NotifyObservers(mfaDocumentPropertiesSaving,Document);

end;

procedure TMainForm.DocumentRenameFailed(Sender: TObject;
  Document: TDocumentID; Error: String);
begin
  ShowMessage('An error occurred while renaming a document properties.' + LineEnding +
              'The rename may be incomplete.' + LineEnding +
              'The document''s ID was ' + Document.ID + '.' + LineEnding +
              Error + LineEnding +
              'You may want to restart the program, or wait and try your task again later');
end;

procedure TMainForm.DocumentsListed(Sender: TObject; Document: TDocumentID);
begin
  NotifyObservers(mfaDocumentsListed,Document);
end;

procedure TMainForm.DocumentAttachmentLoaded(Sender: TObject;
  Document: TDocumentID; Attachment: String);
begin
  if Attachment = 'Synopsis' then
  begin
     NotifyObservers(mfaDocumentSynopsisLoaded,Document);
  end;
end;

procedure TMainForm.DocumentAttachmentError(Sender: TObject;
  Document: TDocumentID; Attachment: String; Error: String);
begin
  ShowMessage('An error occurred while saving or loading an attachment.' + LineEnding +
              'The document''s ID was ' + Document.ID + '.' + LineEnding +
              'The attachment type was ' + Attachment + '.' + LineEnding +
              Error + LineEnding +
              'You may want to restart the program, or wait and try your task again later');
end;

procedure TMainForm.DocumentPropertiesError(Sender: TObject;
  Document: TDocumentID; Error: String);
begin
  ShowMessage('An error occurred while saving or loading the document properties.' + LineEnding +
              'The document''s ID was ' + Document.ID + '.' + LineEnding +
              Error + LineEnding +
              'You may want to restart the program, or wait and try your task again later');
end;

procedure TMainForm.DocumentPropertiesLoaded(Sender: TObject;
  Document: TDocumentID);
begin
  NotifyObservers(mfaDocumentPropertiesLoaded,Document);
end;

procedure TMainForm.DocumentPropertiesSaveConflicted(Sender: TObject;
  Document: TDocumentID);
begin
   if MessageDlg('The document properties file has changed on the disk since the last time it was loaded.' + LineEnding +
             'Document ID: ' + Document.ID + LineEnding +
             'Would you like to overwrite it''s contents?',mtWarning,mbYesNo,0) = mrYes then
   begin
     fProject.GetDocument(Document).Properties.Save(true);
   end;
end;

procedure TMainForm.DocumentPropertiesSaved(Sender: TObject;
  Document: TDocumentID);
begin
  NotifyObservers(mfaDocumentPropertiesSaved,Document);
end;

type

  { TCloseTab }

  TCloseTab = class(TDeferredCall)
  private
    fTab: TTabSheet;
  protected
    procedure DoCallback; override;
  public
    constructor Create(aTab: TTabSheet);
  end;

{ TCloseTab }

procedure TCloseTab.DoCallback;
begin
  fTab.Free;
end;

constructor TCloseTab.Create(aTab: TTabSheet);
begin
  fTab := aTab;
end;

procedure TMainForm.DocumentTabCloseRequested(Sender: TObject);
var
  Tab: TTabSheet;
  Frame: TEditorFrame;
  CanClose: Boolean;
begin
  CanClose := true;
  if (Sender is TTabSheet) then
  begin
    Tab := Sender as TTabSheet;
    if (Tab.ComponentCount > 0) then
      if (Tab.Components[0] is TEditorFrame) then
        Frame := (Tab.Components[0] as TEditorFrame);
  end
  else if (Sender is TEditorFrame) then
  begin
    Frame := Sender as TEditorFrame;
    if (Frame.Parent is TTabSheet) then
       Tab := Frame.Parent as TTabSheet;
  end;

  if Tab <> nil then
  begin
    CanClose := true;
    if (Frame <> nil) then
    begin
      CanClose := Frame.CloseQuery;

      if CanClose then
      // remove from the list, so it doesn't get found again,
      // and so the open document doesn't save. There's a small chance
      // for exception later, but this should still happen.
         fOpenDocuments.Remove(Frame);
    end;
    if CanClose then
    begin
      // delay destroying the tab until later, in case there are
      // some messages that still have to be processed.
      TCloseTab.Create(Tab).Enqueue;
    end;
  end;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // load the configuration first, so we don't overwrite the stuff I'm not
  // interested in overwriting (such as MRU Projects) which were added by
  // another instance.
  fConfig.Load;
  WriteUISettings;
  fConfig.Save;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Frame: TEditorFrame;
  i: Integer;
begin
  for i := 0 to fOpenDocuments.Count - 1 do
  begin
    Frame := fOpenDocuments[i] as TEditorFrame;
    if Frame <> nil then
    begin
      if not Frame.CloseQuery then
      begin
        Frame.SetFocus;
        CanClose := false;
        break;
      end;
    end;
  end;

end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  openParam: String;
  stewFolder: TFile;
begin

  fOpenDocuments := TObjectList.create(false);

  openParam := '';
  stewFolder := LocalFile('');
  fProject := nil;

  fConfig := TStewApplicationConfig.Create;
  try
     fConfig.Load;
  except
    on E: Exception do
      ShowMessage('The settings file could not be loaded for some reason.' + LineEnding +
                  'The Error Message Was: ' + E.Message + LineEnding +
                  'Default settings will be used, and the current settings will be overwritten.');
  end;

  LayoutFrames;

  // start loading a project, or asking for one...
  Enabled := false;
  if (Application.ParamCount > 0) then
  begin
    openParam := Application.Params[1];
    if openParam <> PromptForProjectArgument then
    begin
      stewFolder := LocalFile(openParam);
    end;
    // else, this automatically causes the code below to not create
    // a project with that as the property.
  end
  else
  begin
    stewFolder := fConfig.MRUProject;
  end;

  if (stewFolder.ID <> '') then
  begin
    fProject := TProtectedStewProject.Create(stewFolder);
  end;
  Application.QueueAsyncCall(@StartupCheckProject,0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if fProject <> nil then
  begin
    FreeAndNil(fProject);
  end;
  FreeAndNil(fConfig);
  FreeAndNil(fOpenDocuments);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // Must be done in form show in order to remember the heights and widths
  // of the wsNormal state. (I mean, if you maximize it, it will remember
  // how it was set in the previous session when restoring it to normal).
  ReadUISettings;

end;

procedure TMainForm.NewProjectMenuItemClick(Sender: TObject);
begin
  RunNewStewInstanceWithPrompt;
end;

procedure TMainForm.OpenProjectMenuItemClick(Sender: TObject);
begin
  if OpenProjectDialog.Execute then
  begin
    RunNewStewInstance(LocalFile(OpenProjectDialog.FileName));
  end;
end;

function TMainForm.GetProject: TStewProject;
begin
  result := fProject;
end;

procedure TMainForm.StartupIfProjectParentDirectoryExists(aValue: Boolean);
begin
  if not aValue then
  begin
    if MessageDlg('No stew project could be found.' + LineEnding +
               'Would you like to create one at: ' + fProject.DiskPath.ID + '?',mtConfirmation,mbYesNo,0) =
       mrYes then
    begin;
      fProject.OpenNewAtPath;

    end
    else
      Close;
  end
  else
  begin

    ShowMessage('Found a project at: ' + fProject.DiskPath.ID);
  end;

end;

procedure TMainForm.PreferencesMenuItemClick(Sender: TObject);
begin
  OpenPreferences;
end;

procedure TMainForm.ProjectPropertiesLoading(Sender: TObject);
begin
  NotifyObservers(mfaProjectPropertiesLoading,TDocumentID.Null);
end;

procedure TMainForm.ProjectPropertiesSaving(Sender: TObject);
begin
  NotifyObservers(mfaProjectPropertiesSaving,TDocumentID.Null);

end;

procedure TMainForm.ProjectLoadFailed(E: String);
begin
  ShowMessage('The project couldn''t be loaded.' + LineEnding +
              'The error message was: ' + E + LineEnding +
              'This program will close.');
  Close;
end;

procedure TMainForm.ProjectOpened(Sender: TObject);
var
  i: Integer;
  item: TMRUMenuItem;
  mru: TFile;
begin
  // FUTURE: Someday, will have to store the system type as well.
  fConfig.MRUProject := fProject.DiskPath;
  // save the configuration, so that the MRU Project becomes available
  // if we open up another project in the midst of this.
  fConfig.Save;
  Self.Caption := Application.Title + ' - ' + fProject.GetProjectName;
  Enabled := true;
  with fProject.GetDocument(TDocumentID.Root) do
  begin
    Properties.Load;
    ListDocuments(false);
  end;

  // put the recent projects in the MRU menu, but not this current one...
  RecentProjectsMenuItem.Clear;
  for i := 0 to fConfig.mruProjects.Count - 1 do
  begin
    mru := LocalFile(fConfig.mruProjects[i]);
    if mru <> fProject.DiskPath then
    begin
      item := TMRUMenuItem.Create(RecentProjectsMenuItem);
      item.ProjectPath := LocalFile(fConfig.mruProjects[i]);
      item.Caption := item.ProjectPath.PacketName;
      RecentProjectsMenuItem.Add(item);
    end;
  end;
  RecentProjectsMenuItem.Enabled := (RecentProjectsMenuItem.Count > 0);

end;

procedure TMainForm.ProjectPropertiesError(Sender: TObject; aError: String);
begin
  ShowMessage('An error occurred while saving or loading the project properties.' + LineEnding +
              aError + LineEnding +
              'You may want to restart the program, or wait and try your task again later');
end;

procedure TMainForm.ProjectPropertiesLoaded(Sender: TObject);
begin
  Enabled := true;
  NotifyObservers(mfaProjectPropertiesLoaded,TDocumentID.Null);
end;

procedure TMainForm.ProjectPropertiesSaveConflicted(Sender: TObject);
begin
  if MessageDlg('The properties file has changed on the disk since the last time it was loaded.' + LineEnding +
             'Would you like to overwrite it''s contents?',mtWarning,mbYesNo,0) = mrYes then
   begin
     fProject.Properties.Save(true);
   end;
end;

procedure TMainForm.ProjectPropertiesSaved(Sender: TObject);
begin
  NotifyObservers(mfaProjectPropertiesSaved,TDocumentID.Null);
end;

procedure TMainForm.ProjectSettingsMenuItemClick(Sender: TObject);
begin
  OpenProjectSettings;
end;

procedure TMainForm.RefreshProjectMenuItemClick(Sender: TObject);
begin
  Project.GetDocument(TDocumentID.Root).ListDocuments(true);
end;

procedure TMainForm.StartupCheckProject(Data: PtrInt);
begin
  if fProject = nil then
  begin
    if OpenProjectDialog.Execute then
    begin

      fProject := TProtectedStewProject.Create(LocalFile(OpenProjectDialog.FileName));

    end
    else
    begin
      Close;
      Exit;
    end;
  end;

  fProject.OnOpened:=@ProjectOpened;
  fProject.OnPropertiesError:=@ProjectPropertiesError;
  fProject.OnPropertiesLoaded:=@ProjectPropertiesLoaded;
  fProject.OnPropertiesSaveConflicted:=@ProjectPropertiesSaveConflicted;
  fProject.OnPropertiesSaved:=@ProjectPropertiesSaved;
  fProject.OnPropertiesLoading:=@ProjectPropertiesLoading;
  fProject.OnPropertiesSaving:=@ProjectPropertiesSaving;
  fProject.OnDocumentPropertiesError:=@DocumentPropertiesError;
  fProject.OnDocumentPropertiesLoaded:=@DocumentPropertiesLoaded;
  fProject.OnDocumentPropertiesSaveConflicted:=@DocumentPropertiesSaveConflicted;
  fProject.OnDocumentPropertiesSaved:=@DocumentPropertiesSaved;
  fProject.OnDocumentPropertiesSaving:=@DocumentPropertiesSaving;
  fProject.OnDocumentPropertiesLoading:=@DocumentPropertiesLoading;
  fProject.OnDocumentAttachmentLoaded:=@DocumentAttachmentLoaded;
  fProject.OnDocumentAttachmentError:=@DocumentAttachmentError;
  fProject.OnDocumentAttachmentLoading:=@DocumentAttachmentLoading;
  fProject.OnDocumentAttachmentSaving:=@DocumentAttachmentSaving;
  fProject.OnDocumentAttachmentSaved:=@DocumentAttachmentSaved;
  fProject.OnDocumentAttachmentSaveConflicted:=@DocumentAttachmentSaveConflicted;
  fProject.OnDocumentsListed:=@DocumentsListed;
  fProject.OnDocumentListError:=@DocumentListError;
  fProject.OnDocumentCreated:=@DocumentCreated;
  fProject.OnDocumentChanged:=@DocumentChanged;
  fProject.OnDocumentRenameFailed:=@DocumentRenameFailed;
  fProject.OnConfirmNewAttachment:=@DoConfirmNewAttachment;
  fProject.OnChooseTemplate:=@DoChooseNewAttachmentTemplate;
  fProject.OpenAtPath(@StartupIfProjectExists,@ProjectLoadFailed);

end;

procedure TMainForm.StartupIfProjectExists(aValue: Boolean);
begin
  if not aValue then
  begin
    // NMS: I decided that this should be done automatically, but I'll leave this
    //      stuff here in case someone convinces me otherwise.
    //if MessageDlg('There is no stew project at: ' + fProject.DiskPath.ID + LineEnding +
    //           'Would you like to search for one in parent directories?' + LineEnding +
    //           'You can create a new one if none are found.',mtConfirmation,mbYesNo,0) =
    //   mrYes then
    //begin;
      fProject.OpenInParentDirectory(@StartupIfProjectParentDirectoryExists,@ProjectLoadFailed);

    //end
    //else
    //  Close;
  end

end;

procedure TMainForm.NotifyObservers(aAction: TMainFormAction; aDocument: TDocumentID);
var
  i: Integer;
begin
  if fObservers <> nil then
  begin
    for i := fObservers.Count - 1 downto 0 do
    begin
      fObservers[i](aAction,aDocument);
    end;
  end;
end;

procedure TMainForm.ReadUISettings;
begin
  Height := fConfig.MainWindow.height;
  Width := fConfig.MainWindow.width;
  if fConfig.MainWindow.maximized then
  begin
    WindowState := wsMaximized;
  end;
  if fFrames[alLeft] <> nil then
    fFrames[alLeft].Width := fConfig.MainWindow.leftPaneWidth;
  if fFrames[alRight] <> nil then
    fFrames[alRight].Width := fConfig.MainWindow.rightPaneWidth;
  if fFrames[alTop] <> nil then
    fFrames[alTop].Height := fConfig.MainWindow.topPaneHeight;
  if fFrames[alBottom] <> nil then
    fFrames[alBottom].Height := fConfig.MainWindow.bottomPaneHeight;

end;

procedure TMainForm.WriteUISettings;
begin
  // FUTURE: Perhaps I should "load" the current settings, so I don't
  // override stuff which I didn't touch. For example: If another
  // instance opens while this one is open, it's going to mess with
  // the MRUProjects, and these will be lost.
  if WindowState = wsMaximized then
  begin
    fConfig.MainWindow.Maximized := true;
    // don't set the height and width, so they're remembered from last time.
  end
  else
  begin
    fConfig.MainWindow.Maximized := false;
    fConfig.MainWindow.Height := Height;
    fConfig.MainWindow.Width := Width;
  end;

  if fFrames[alLeft] <> nil then
    fConfig.MainWindow.LeftPaneWidth := fFrames[alLeft].Width;
  if fFrames[alRight] <> nil then
    fConfig.MainWindow.RightPaneWidth := fFrames[alRight].Width;
  if fFrames[alTop] <> nil then
    fConfig.MainWindow.TopPaneHeight := fFrames[alTop].Height;
  if fFrames[alBottom] <> nil then
    fConfig.MainWindow.BottomPaneHeight := fFrames[alBottom].Height;


end;

procedure TMainForm.LayoutFrames;
begin
  // Layout and docking stuff...
  LayoutFrame(TProjectManager,alLeft);
  // lay out the document pane
  LayoutDocumentPane(alClient);

end;

function TMainForm.OpenDocument(aDocument: TDocumentID): TEditorFrame;
var
  EditorClass: TEditorFrameClass;
  i: Integer;
begin
  if fDocumentPane = alNone then
  begin
    raise Exception.Create('There''s no place to open documents yet.');
  end;

  Result := nil;
  // FUTURE: Again, if fOpenDocuments were a hash, this would be easier.
  for i := 0 to fOpenDocuments.Count - 1 do
  begin
    if (fOpenDocuments[i] is TEditorFrame) then
    begin
      Result := fOpenDocuments[i] as TEditorFrame;
      if Result.Document = aDocument then
         break
      else
         Result := nil;
    end;
  end;

  if Result = nil then
  begin
    // When I use a case statement here, it says Constant and CASE types do not match,
    // even though these *are* constants.
    // FUTURE: Some sort of 'class registry' might be useful here.
    if aDocument = TDocumentID.GetSystemDocument(PreferencesDocumentID) then
      EditorClass := TApplicationPreferencesEditor
    else if aDocument = TDocumentID.GetSystemDocument(ProjectSettingsDocumentID) then
      EditorClass := TProjectSettingsEditor
    else
       EditorClass := TDocumentEditor;
    Result := (LayoutFrame(EditorClass,fDocumentPane) as TEditorFrame);
    Result.Document := aDocument;
    fOpenDocuments.Add(Result);
  end;
  Result.Show;
end;

procedure TMainForm.OpenPreferences;
begin
  OpenDocument(TDocumentID.GetSystemDocument(PreferencesDocumentID));
end;

procedure TMainForm.OpenProjectSettings;
begin
  OpenDocument(TDocumentID.GetSystemDocument(ProjectSettingsDocumentID));
end;

procedure TMainForm.Observe(aObserver: TMainFormObserverHandler);
begin
  if fObservers = nil then
  begin
    fObservers := TMainFormObserverList.Create;
  end;
  fObservers.Add(aObserver);

end;

procedure TMainForm.Unobserve(aObserver: TMainFormObserverHandler);
begin
  if fObservers <> nil then
  begin
    fObservers.Remove(aObserver);
    if fObservers.Count = 0 then
    begin
      fObservers.Free;
      fObservers := nil;
    end;
  end;
end;

procedure TMainForm.RequestTabClose(aFrame: TEditorFrame);
begin
  MainForm.DocumentTabCloseRequested(aFrame);
end;

function TMainForm.LayoutFrame(aFrameClass: TControlClass; aLocation: TAlign
  ): TControl;
var
  NewTab: TTabSheet;
begin
  if aLocation in [alNone,alCustom] then
  begin
    raise Exception.Create('Controls must be laid out left, right, top, bottom or client');
  end;
  if fFrames[aLocation] <> nil then
  begin
    if fFrames[aLocation] is TPageControl then
    begin
      NewTab := TPageControl(fFrames[aLocation]).AddTabSheet;
      result := aFrameClass.Create(NewTab);
      NewTab.Caption := result.Caption;
      result.Parent := NewTab;
      result.Align := alClient;
    end
    else
    begin
      raise Exception.Create('A control is already laid out there.');
    end;
  end
  else
  begin
    result := aFrameClass.Create(Self);
    fFrames[aLocation] := result;
    fSplitters[aLocation] := TSplitter.Create(Self);
    fSplitters[aLocation].Align := aLocation;
    fSplitters[aLocation].Parent := Self;
    result.Parent := Self;
    result.Align := aLocation;
  end;

  {- if there is already a control at the specified location, and it is not a PageControl,
  an error will be raised. If there is a pagecontrol, the item will be added to
  the pagecontrol with it's "Caption" as the page name.
  - Also plugs the control into the form so that it can receive project information.}

end;

function TMainForm.LayoutTabs(aLocation: TAlign): TControl;
begin
  result := LayoutFrame(TPageControl,aLocation);
end;

procedure TMainForm.LayoutDocumentPane(aLocation: TAlign);
var
  aDocumentPane: TPageControl;
begin
  if fDocumentPane = alNone then
  begin
    fDocumentPane := aLocation;
    aDocumentPane := LayoutTabs(alClient) as TPageControl;
    aDocumentPane.Options:= [nboShowCloseButtons];
    aDocumentPane.OnCloseTabClicked:=@DocumentTabCloseRequested;
  end
  else
    raise Exception.Create('Document pane is already set');

end;

initialization

  SetAsyncCallQueuer(@QueueAsyncCall);

end.

