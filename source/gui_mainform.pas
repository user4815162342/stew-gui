unit gui_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, ComCtrls, StdCtrls, stew_project, fgl, gui_config, contnrs,
  simpleipc, gui_editorframe, sys_types, sys_async, sys_file, sys_os,
  stew_properties;

type
  {TODO: I really need to go through all of this code and change every procedure
  parameter to const when possible.

  The following regex should find all parameters with just a name and a type.
  I'm not sure what kind of false positives we might get.
    // find: ((function|procedure)[^;]+[(;]\s*)([^ :]+\s*:\s*[^ ;)]+)
    // replace with: $1const $3

    You will have to go through again after each pass, as the final variables
    will be fixed first.
  }


  {
  Glyphs needed:
  - Main Menu:
    - New Project
    - Open Project
    - Preferences
    - Exit
    - Refresh Project
    - Project Settings
  - Project ManagerToolbar:
    - New Sibling
    - New Child
    - Change Name
    - Move Up
    - Move Down
    - Delete
  - Editor Frame Toolbar:
    - Close
  - Document Editor Toolbar:
    - Save
    - Refresh
    - Edit Primary
    - Edit Notes
  - Project Settings Toolbar:
    - Save
    - Refresh
    - Edit Notes
  - JSON editor Toolbar:
    - New Prop List
    - New List
    - New Yes/No
    - New Number
    - New String
    - Delete Item

  - TODO: Eventually, a bunch of glyphs to make available for the categories.
  }


  TMainForm = class;
  TMainFormAction = (mfaProjectOpened, mfaProjectClosed, mfaDocumentOpened, mfaDocumentClosed);
  TMainFormObserverHandler = procedure(Sender: TMainForm; Action: TMainFormAction; aDocument: TDocumentPath) of object;
  TMainFormObserverList = specialize TFPGList<TMainFormObserverHandler>;

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationImages: TImageList;
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
    CloseTimeoutTimer: TTimer;
    MainStatus: TStatusBar;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure CloseTimeoutTimerTimer(Sender: TObject);
    procedure DocumentTabCloseRequested(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MainStatusMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      {%H-}Y: Integer);
    procedure NewProjectMenuItemClick(Sender: TObject);
    procedure OpenProjectMenuItemClick(Sender: TObject);
    procedure PreferencesMenuItemClick(Sender: TObject);
    procedure ProjectSettingsMenuItemClick(Sender: TObject);
    procedure RefreshProjectMenuItemClick(Sender: TObject);
  private
    { private declarations }
    fProject: TStewProject;
    fConfig: IStewApplicationConfig;
    fObservers: TMainFormObserverList;
    fFrames: array[TAlign] of TControl;
    fSplitters: array[TAlign] of TControl;
    fDocumentPane: TAlign;
    // FUTURE: Should be a hash list, so we can look things up by ID.
    fOpenDocuments: TObjectList;
    fGoalHints: TStringArray;
    function GetProject: TStewProject;
    procedure DoChooseAttachment(Sender: TObject; Document: TDocumentPath;
      AttachmentName: String; aChoices: TStringArray; var Answer: Integer; out
      Accepted: Boolean);
    procedure DoChooseNewAttachmentTemplate(Sender: TObject;
      {%H-}Document: TDocumentPath; AttachmentName: String; aChoices: TStringArray;
      var Answer: Integer; out Accepted: Boolean);
    procedure DoConfirmNewAttachment(Sender: TObject; {%H-}Document: TDocumentPath;
      AttachmentName: String; out Answer: Boolean);
    procedure ObserveProject(Sender: TStewProject; Event: TProjectEvent);
    procedure QueueStateChanged(aActive: Boolean);
    procedure ShowProjectError(Event: TProjectEvent);
    procedure OpenProject(aPath: TFile);
    procedure InitializeProject;
    procedure AfterProjectOpen(Sender: TPromise);
    procedure AfterProjectOpenInParent(Sender: TPromise);
    procedure ProjectOpenFailed(Sender: TPromise; Error: TPromiseError);
    procedure StartupAskForProject({%H-}Data: PtrInt);
    procedure NotifyObservers(aAction: TMainFormAction; aDocument: TDocumentPath);
    procedure ReadUISettings;
    procedure WriteUISettings;
    procedure LayoutFrames;
    function FindFrameForDocument(aDocument: TDocumentPath): TEditorFrame;
    function MessageDialog(const Title: String; const Message: String; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; ButtonCaptions: Array of String): Integer;
    procedure UpdateStatus(aProps: IProjectProperties);
    procedure ResizeStatusPanels;
    procedure SetupGlyphs;
  public
    { public declarations }
    property Project: TStewProject read GetProject;
    function OpenDocument(aDocument: TDocumentPath): TEditorFrame;
    procedure OpenPreferences;
    procedure OpenProjectSettings;
    procedure Observe(aObserver: TMainFormObserverHandler);
    procedure Unobserve(aObserver: TMainFormObserverHandler);
    function IsDocumentOpen(aDocument: TDocumentPath): Boolean;
    procedure RequestTabClose(aFrame: TEditorFrame);
    // Creates a Message Dialog according to our UI standards, which includes:
    // - using the form's title as the dialog caption
    // - allowing custom captions on the buttons, which gives the user a better
    // idea about what they are choosing.
    function MessageDialog(const Message: String; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; ButtonCaptions: Array of String): Integer;
    procedure ShowMessage(const Message: String; DlgType: TMsgDlgType;
      AcceptCaption: String);
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

  procedure RunNewStewInstance(const aProjectPath: TFile);
  procedure RunNewStewInstanceWithPrompt;

  function InitializeLog(const aName: String; aOverwrite: Boolean): UTF8String;

var
  MainForm: TMainForm;

const
  PromptForProjectArgument = '--prompt-for-project';
  VersionArgument = '--version';

implementation

uses
  gui_projectmanager, gui_documenteditor, gui_preferenceseditor, gui_projectsettingseditor, LCLProc, gui_listdialog, sys_localfile, gui_async, sys_versionsupport, math, sys_log, gui_glyphs;

{$R *.lfm}

// using ':' at the beginning because that should not be a valid filename.
const PreferencesDocumentID: UTF8String = 'preferences';
const ProjectSettingsDocumentID: UTF8String = 'project settings';

procedure RunNewStewInstance(const aProjectPath: TFile);
begin
  try
    TOperatingSystemInterface.RunDetachedProcess(Application.ExeName,[aProjectPath.ID]);

  except
    on E: Exception do
    begin
      LogException('gui_mainform.RunNewStewInstance',E);
      ShowException(E,ExceptAddr);
    end;
  end;
end;

procedure RunNewStewInstanceWithPrompt;
begin
  try
     TOperatingSystemInterface.RunDetachedProcess(Application.ExeName,[PromptForProjectArgument]);
  except
    on E: Exception do
    begin
      LogException('gui_mainform.RunNewStewInstanceWithPrompt',E);
      ShowException(E,ExceptAddr);
    end;
  end;
end;

function InitializeLog(const aName: String; aOverwrite: Boolean): UTF8String;
var
  lDir: UTF8String;
begin
  lDir := GetAppConfigDir(false) + 'logs' + DirectorySeparator;
  result := lDir + aName;
  ForceDirectoriesUTF8(lDir);
  if aOverwrite and FileExistsUTF8(result) then
  begin
     DeleteFileUTF8(result);
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

{ TMainForm }

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
var
  lAboutText: TAboutText;
begin
  lAboutText := GetAboutText(Application);
  MessageDialog(lAboutText.Title,lAboutText.Copyright + LineEnding +
                               lAboutText.Description + LineEnding +
                               lAboutText.BuildInfo,mtCustom,[mbOK],['Thanks for Using This Program!']);
end;

procedure TMainForm.CloseTimeoutTimerTimer(Sender: TObject);
const
  cMethod: String = 'TMainForm.CloseTimeoutTimerTimer';
begin
  LogAction(cMethod,'');
  // the tag is used to tell the mainform that this is a timeout, and
  // it's okay to close...
  CloseTimeoutTimer.Tag := 1;
  Close;
end;

procedure TMainForm.DoChooseAttachment(Sender: TObject;
  Document: TDocumentPath; AttachmentName: String; aChoices: TStringArray;
  var Answer: Integer; out Accepted: Boolean);
begin
  Accepted := ChoiceQuery('There are multiple ' + AttachmentName + ' files on the disk for the document ' + Document.Name + '.' + LineEnding +
                          'Please specify which one you would like to work with:', aChoices,Answer);
end;

procedure TMainForm.AfterProjectOpen(Sender: TPromise);
begin
  fProject := (Sender as TProjectPromise).Project;
  if fProject = nil then
     TStewProject.CheckExistenceInParentAndCreate((Sender as TProjectPromise).Path).After(@AfterProjectOpenInParent,@ProjectOpenFailed)
  else
     InitializeProject;
end;

procedure TMainForm.AfterProjectOpenInParent(Sender: TPromise);
var
  lPath: TFile;
begin
  fProject := (Sender as TProjectPromise).Project;
  lPath := (Sender as TProjectPromise).Path;
  if fProject = nil then
  begin
     if MessageDialog('No stew project could be found.' + LineEnding +
                'Would you like to create one at: ' + lPath.ID + '?',mtConfirmation,mbYesNo,['Create New Stew Project','Open a Different Project']) =
        mrYes then
     begin;
        // just create it...
        fProject := TStewProject.Create(lPath);
        InitializeProject;
     end
     else
     begin
       if OpenProjectDialog.Execute then
       begin
         OpenProject(LocalFile(OpenProjectDialog.FileName));

       end
       else
       begin
         Close;
         Exit;
       end;
     end;
  end
  else
  begin
     ShowMessage('Found a project at: ' + fProject.DiskPath.ID,mtConfirmation,'Okay, Open It');
     InitializeProject;
  end;

end;

procedure TMainForm.ProjectOpenFailed(Sender: TPromise; Error: TPromiseError);
begin
  ShowMessage('The project couldn''t be loaded.' + LineEnding +
              'The error message was: ' + Error + LineEnding +
              'This program will close.',mtError,'Sigh');
  Close;
end;

procedure TMainForm.DoChooseNewAttachmentTemplate(Sender: TObject;
  Document: TDocumentPath; AttachmentName: String; aChoices: TStringArray;
  var Answer: Integer; out Accepted: Boolean);
begin
  Accepted := ChoiceQuery('There are multiple templates available for your new ' + AttachmentName + ' file.' + LineEnding +
                          'Please specify which one you would like to use:', aChoices,Answer);
end;

procedure TMainForm.DoConfirmNewAttachment(Sender: TObject;
  Document: TDocumentPath; AttachmentName: String; out Answer: Boolean);
begin
  Answer :=
    MessageDialog('The ' + AttachmentName + ' file for this document does not exist.' + LineEnding +
               'Do you wish to create a new file?',mtConfirmation,mbYesNo,['Create the File','Cancel this Action']) = mrYes;
end;

type

  { TCloseTabTask }

  TCloseTabTask = class(TQueuedTask)
  private
    fTab: TTabSheet;
  protected
    procedure DoTask; override;
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue(aTab: TTabSheet);
  end;

{ TCloseTabTask }

procedure TCloseTabTask.DoTask;
begin
  fTab.Free;
  Resolve;
end;

function TCloseTabTask.CreatePromise: TPromise;
begin
  result := TPromise.Create;
end;

constructor TCloseTabTask.Enqueue(aTab: TTabSheet);
begin
  fTab := aTab;
  inherited Enqueue;
end;

procedure TMainForm.DocumentTabCloseRequested(Sender: TObject);
const
  cMethod: String = 'TMainForm.DocumentTabCloseRequested';
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
      LogAction(cMethod,Frame.Document.ID);
      CanClose := Frame.CloseQuery;

      if CanClose then
      begin
      // remove from the list, so it doesn't get found again,
      // and so the open document doesn't save. There's a small chance
      // for exception later, but this should still happen.
         fOpenDocuments.Remove(Frame);
         NotifyObservers(mfaDocumentClosed,Frame.Document);
      end;
    end;
    if CanClose then
    begin
      // delay destroying the tab until later, in case there are
      // some messages that still have to be processed.
      TCloseTabTask.Enqueue(Tab);
    end;
  end;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
const
  cMethod: String = 'TMainForm.ExitMenuItemClick';
begin
  LogAction(cMethod,'');
  Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
const
  cMethod: String = 'TMainForm.FormClose';
begin
  LogAction(cMethod,'');
  // load the configuration first, so we don't overwrite the stuff I'm not
  // interested in overwriting (such as MRU Projects) which were added by
  // another instance.
  fConfig.Load;
  WriteUISettings;
  fConfig.Save;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
const
  cMethod: String = 'TMainForm.FormCloseQuery';
var
  Frame: TEditorFrame;
  i: Integer;
begin
  LogAction(cMethod,'');
  for i := 0 to fOpenDocuments.Count - 1 do
  begin
    Frame := fOpenDocuments[i] as TEditorFrame;
    if Frame <> nil then
    begin
      if not Frame.CloseQuery then
      begin
        CanClose := false;
        break;
      end;
    end;
  end;
  // If the queue is active, then we don't want to close yet until
  // it's done. However, a timeout on this wait prevents the app
  // from staying open indefinitely if there's a task that simply
  // will never be done.
  if TPromiseMonitor.IsActive then
  begin
    // The tag is used by the timer to communicate whether this
    // close is caused by a timeout. If it's any value other than
    // 0, then the timer is triggering this close, and we should
    // allow the form to close even if the queue is active.
    if CloseTimeoutTimer.Tag = 0 then
    begin
      // Tell the window that the form can't close yet.
      CanClose := false;
      // Turn on the timeout timer.
      CloseTimeoutTimer.Enabled := true;
      // Disable the form to prevent further (accidental) User interaction
      // before the close.
      Enabled := false;
      Exit;
    end;
  end;

end;



procedure TMainForm.FormCreate(Sender: TObject);
var
  openParam: String;
  stewFolder: TFile;
begin
  SetupGlyphs;

  TPromiseMonitor.OnStateChanged:=@QueueStateChanged;

  fOpenDocuments := TObjectList.create(false);

  openParam := '';
  stewFolder := LocalFile('');
  fProject := nil;

  fConfig := TStewApplicationConfigObjects.NewStewApplicationConfig;
  try
     fConfig.Load;
  except
    on E: Exception do
    begin
      LogException('Loading Configuration in TMainForm',E);
      ShowMessage('The settings file could not be loaded for some reason.' + LineEnding +
                  'The Error Message Was: ' + E.Message + LineEnding +
                  'Default settings will be used, and the current settings will be overwritten.',mtError,'Sigh');
    end;
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
    stewFolder := fConfig.MRUProjects.MRUProject;
  end;

  if (stewFolder.ID <> '') then
  begin
    OpenProject(stewFolder);
  end
  else
    Application.QueueAsyncCall(@StartupAskForProject,0);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
const
  cMethod: String = 'TMainForm.FormDestroy';
begin
  LogAction(cMethod,'');
  TPromiseMonitor.OnStateChanged := nil;

  if fProject <> nil then
  begin
    NotifyObservers(mfaProjectClosed,TDocumentPath.Null);
    fProject.RemoveObserver(@ObserveProject);
    FreeAndNil(fProject);
  end;
  fConfig := nil;
  //FreeAndNil(fConfig);
  FreeAndNil(fOpenDocuments);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  // Don't log every resize...
  ResizeStatusPanels;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // Must be done in form show in order to remember the heights and widths
  // of the wsNormal state. (I mean, if you maximize it, it will remember
  // how it was set in the previous session when restoring it to normal).
  ReadUISettings;

end;

procedure TMainForm.MainStatusMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  i,border:integer;
begin
  // http://lists.lazarus.freepascal.org/pipermail/lazarus/2012-May/074105.html
  // Display a hint for the status panels to indicate how many days
  // we have left until the goal.
  i:=0;
  border:=0;
  for i := 0 to MainStatus.Panels.Count - 1 do
  begin
    border:=border+MainStatus.Panels[i].Width;
    if (X < border) then
       break;

  end;
  if (i < MainStatus.Panels.Count) then
  begin
    MainStatus.Hint:=fGoalHints[i];
    Application.CancelHint;
    Application.ActivateHint(mouse.CursorPos);
  end;
end;


procedure TMainForm.NewProjectMenuItemClick(Sender: TObject);
const
  cMethod: String = 'TMainForm.NewProjectMenuItemClick';
begin
  LogAction(cMethod,'');
  RunNewStewInstanceWithPrompt;
end;

procedure TMainForm.ObserveProject(Sender: TStewProject; Event: TProjectEvent);
begin
  if Event.IsError then
  begin
    ShowProjectError(Event)
  end
  else
  begin
    case Event.Action of
       paProjectPropertiesDataReceived:
         UpdateStatus((Event as TProjectPropertiesDataReceivedEvent).Properties);
    end;
  end;
end;

procedure TMainForm.QueueStateChanged(aActive: Boolean);
begin
  if aActive then
    Cursor := crHourGlass
  else
    Cursor := crDefault;

end;

procedure TMainForm.ShowProjectError(Event: TProjectEvent);
var
  lMessage: String;
begin
  lMessage := 'An error occurred';
  case Event.Action of
     paProjectPropertiesLoadingFailed, paProjectPropertiesSavingFailed:
       lMessage := lMessage + ' while loading or saving the project properties.';
     paDocumentsListingFailed:
       lMessage := lMessage + ' while listing the contents of a folder.';
     paAttachmentListingFailed:
       lMessage := lMessage + ' while listing attachments for a document.';
     paDocumentFolderCheckFailed:
       lMessage := lMessage + ' while checking a document.';
     paDocumentShiftFailed:
       lMessage := lMessage + ' while shifting a document up or down.';
     paDocumentRenamingFailed:
       lMessage := lMessage + ' while renaming a document.';
     paAttachmentLoadingFailed, paAttachmentSavingFailed:
       lMessage := lMessage + ' while loading or saving a ' + (Event as TAttachmentEvent).AttachmentName + '.';
     paAttachmentEditingFailed:
       lMessage := lMessage + ' while attempting to edit a ' + (Event as TAttachmentEvent).AttachmentName + '.';
     paUnexpectedProjectError:
       lMessage := lMessage + '.';
     paUnexpectedDocumentError:
       lMessage := lMessage + ' while working with a document.';
     paUnexpectedAttachmentError:
       lMessage := lMessage + ' while working with a ' + (Event as TAttachmentEvent).AttachmentName + '.';
     paProjectPropertiesSaveConflictOccurred:
       lMessage := 'The project properties file has changed on disk since the last time you opened it.';
     paAttachmentSaveConflictOccurred:
       lMessage := 'The ' + (Event as TAttachmentEvent).AttachmentName + ' file has changed on disk since the last time you opened it.';
  end;
  if Event is TDocumentError then
  begin
    lMessage := lMessage + LineEnding;
    lMessage := lMessage + 'The document''s ID was ' + (Event as TDocumentEvent).Document.ID + '.' + LineEnding;
    lMessage := lMessage + (Event as TDocumentError).Error + LineEnding;
  end
  else if Event is TAttachmentError then
  begin
    lMessage := lMessage + LineEnding;
    lMessage := lMessage + 'The document''s ID was ' + (Event as TDocumentEvent).Document.ID + '.' + LineEnding;
    lMessage := lMessage + (Event as TAttachmentError).Error + LineEnding;
  end
  else if Event is TProjectError then
  begin
    lMessage := lMessage + LineEnding;
    lMessage := lMessage + (Event as TProjectError).Error + LineEnding;
  end;
  ShowMessage(lMessage +
             'You may want to restart the program, or wait and try your task again later',mtError,'Sigh');
end;

procedure TMainForm.OpenProjectMenuItemClick(Sender: TObject);
const
  cMethod: String = 'TMainForm.OpenProjectMenuItemClick';
begin
  LogAction(cMethod,'');
  if OpenProjectDialog.Execute then
  begin
    RunNewStewInstance(LocalFile(OpenProjectDialog.FileName));
  end;
end;

function TMainForm.GetProject: TStewProject;
begin
  result := fProject;
end;

procedure TMainForm.PreferencesMenuItemClick(Sender: TObject);
const
  cMethod: String = 'TMainForm.PreferencesMenuItemClick';
begin
  LogAction(cMethod,'');
  OpenPreferences;
end;

procedure TMainForm.OpenProject(aPath: TFile);
begin
  TStewProject.CheckExistenceAndCreate(aPath).After(@AfterProjectOpen,@ProjectOpenFailed);

end;

procedure TMainForm.InitializeProject;
var
  i: Integer;
  item: TMRUMenuItem;
  mru: TFile;
begin

  fProject.AddObserver(@ObserveProject);
  // These are user interactions, and aren't handled by observers.
  fProject.OnConfirmNewAttachment:=@DoConfirmNewAttachment;
  fProject.OnChooseTemplate:=@DoChooseNewAttachmentTemplate;
  fProject.OnChooseAttachment:=@DoChooseAttachment;

  // FUTURE: Someday, will have to store the system type as well.
  fConfig.MRUProjects.MRUProject := fProject.DiskPath;
  // save the configuration, so that the MRU Project becomes available
  // if we open up another project in the midst of this.
  fConfig.Save;
  Self.Caption := Application.Title + ' - ' + fProject.GetProjectName;
  Enabled := true;

  // put the recent projects in the MRU menu, but not this current one...
  RecentProjectsMenuItem.Clear;
  for i := 0 to fConfig.mruProjects.Length - 1 do
  begin
    mru := fConfig.mruProjects.GetProject(i).Path;
    if mru <> fProject.DiskPath then
    begin
      item := TMRUMenuItem.Create(RecentProjectsMenuItem);
      item.ProjectPath := mru;
      item.Caption := item.ProjectPath.PacketName;
      RecentProjectsMenuItem.Add(item);
    end;
  end;
  RecentProjectsMenuItem.Enabled := (RecentProjectsMenuItem.Count > 0);

  NotifyObservers(mfaProjectOpened,TDocumentPath.Null);

end;

procedure TMainForm.ProjectSettingsMenuItemClick(Sender: TObject);
const
  cMethod: String = 'TMainForm.ProjectSettingsMenuItemClick';
begin
  LogAction(cMethod,'');
  OpenProjectSettings;
end;

procedure TMainForm.RefreshProjectMenuItemClick(Sender: TObject);
const
  cMethod: String = 'TMainForm.RefreshProjectMenuItemClick';
begin
  LogAction(cMethod,'');
  // This should cause a ListingDataReceived event (or something
  // like that) on the project, and the explorer can get that information.
  // Note that we're no longer doing recursive. Maybe that's something the
  // project explorer will end up doing on it's own.
  Project.ListDocumentsInFolder(TDocumentPath.Root,true);
end;

procedure TMainForm.StartupAskForProject(Data: PtrInt);
begin
  if OpenProjectDialog.Execute then
  begin
    OpenProject(LocalFile(OpenProjectDialog.FileName));

  end
  else
  begin
    Close;
    Exit;
  end;
end;

procedure TMainForm.NotifyObservers(aAction: TMainFormAction;
  aDocument: TDocumentPath);
var
  i: Integer;
begin
  if fObservers <> nil then
  begin
    for i := fObservers.Count - 1 downto 0 do
    begin
      fObservers[i](Self,aAction,aDocument);
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

function TMainForm.FindFrameForDocument(aDocument: TDocumentPath): TEditorFrame;
var
  i: Integer;
begin
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

end;

function TMainForm.OpenDocument(aDocument: TDocumentPath): TEditorFrame;
var
  EditorClass: TEditorFrameClass;
begin
  if fDocumentPane = alNone then
  begin
    raise Exception.Create('There''s no place to open documents yet.');
  end;

  Result := FindFrameForDocument(aDocument);

  if Result = nil then
  begin
    // When I use a case statement here, it says Constant and CASE types do not match,
    // even though these *are* constants.
    // FUTURE: Some sort of 'class registry' might be useful here.
    if aDocument = TDocumentPath.GetSystemDocument(PreferencesDocumentID) then
      EditorClass := TApplicationPreferencesEditor
    else if aDocument = TDocumentPath.GetSystemDocument(ProjectSettingsDocumentID) then
      EditorClass := TProjectSettingsEditor
    else
       EditorClass := TDocumentEditor;
    Result := (LayoutFrame(EditorClass,fDocumentPane) as TEditorFrame);
    Result.Document := aDocument;
    fOpenDocuments.Add(Result);
    NotifyObservers(mfaDocumentOpened,aDocument);
  end;
  Result.Show;
end;

procedure TMainForm.OpenPreferences;
begin
  OpenDocument(TDocumentPath.GetSystemDocument(PreferencesDocumentID));
end;

procedure TMainForm.OpenProjectSettings;
begin
  OpenDocument(TDocumentPath.GetSystemDocument(ProjectSettingsDocumentID));
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

function TMainForm.IsDocumentOpen(aDocument: TDocumentPath): Boolean;
begin
  result := FindFrameForDocument(aDocument) <> nil;
end;

procedure TMainForm.RequestTabClose(aFrame: TEditorFrame);
begin
  MainForm.DocumentTabCloseRequested(aFrame);
end;


type
  // This is used as a trick in order to access a protected method on the buttons
  // in message dialog. This is actually a case where that protected method, which
  // really doesn't make any changes to the object, doesn't need to be protected
  // in my opinion, so I'm willing to do this.
  TButtonAccess = class(TBitBtn);

function TMainForm.MessageDialog(const Title: String; const Message: String;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; ButtonCaptions: array of String
  ): Integer;
var
  lMsgdlg: TForm;
  i: Integer;
  lButton: TBitBtn;
  lCaptionindex: Integer;
  lLastButton: TBitBtn;
  lButtonWidth: Integer = 0;
  lButtonHeight: Integer = 0;
begin
  // inspired by: http://stackoverflow.com/a/18620157/300213
  // Except that was written for delphi, and lazarus seems to work differently...
  // Basically, we have to assign different captions after creating the
  // dialog itself. In the stack overflow comment, they just assign the captions.
  // However, in Lazarus, that does not change the button widths, and the captions
  // get cut off. So, I have to do that, as well as align the buttons appropriately.

  lMsgdlg := createMessageDialog(Message, DlgType, Buttons);
  try
     lMsgdlg.Caption := Title;
     lMsgdlg.BiDiMode := Self.BiDiMode;
     lCaptionindex := Length(ButtonCaptions) - 1;
     lLastButton := nil;

     // Basically, we're working backwards, because we need to justify
     // the buttons to the right (although if BiDiMode is right to left,
     // do I need to change that?), which means that I need to start with
     // the rightmost button (which is the last button added) and go on to
     // the left.
     for i := lMsgdlg.componentcount - 1 downto 0 Do
     begin
       if (lMsgdlg.components[i] is TCustomButton) then
       Begin
         lButton := TBitBtn(lMsgdlg.components[i]);
         if lCaptionindex >= 0 then
         begin
           // Yes, No
           lButton.Caption := ButtonCaptions[lCaptionindex];
           TButtonAccess(lButton).CalculatePreferredSize(lButtonWidth,lButtonHeight,true);
           lButton.Width := lButtonWidth + 10;
           if lLastButton <> nil then
           begin
             lButton.Left := (lLastButton.Left - 10) - lButton.Width;
           end
           else
           begin
             lButton.Left := (lMsgdlg.ClientWidth - 10) - lButton.Width;
           end;

           lLastButton := lButton;
         end;
         dec(lCaptionindex);
       end
     end;
     Result := lMsgdlg.Showmodal;

  finally
    lMsgdlg.Free;
  end;
end;

procedure TMainForm.UpdateStatus(aProps: IProjectProperties);

  procedure SetDeadlineHint(i: Integer; aDeadline: IDeadline);
  var
    lText: String;
  begin
    lText := IntToStr(trunc(aDeadline.Due) - trunc(Now)) + ' days left' + LineEnding +
             IntToStr(WorkingDaysDifference(aDeadline.Due,Now)) + ' working days left';
    fGoalHints[i] := lText;
  end;

  procedure AddDeadlinePanel(aDeadline: IDeadline; aAlign: TAlignment);
  var
    lPanel: TStatusPanel;
    lText: String;
  begin
    lText := aDeadLine.Name + ' due ' + DateTimeToRelativeEnglish(Now,aDeadLine.Due);
    lPanel := MainStatus.Panels.Add;
    lPanel.Text := lText;
    lPanel.Alignment:= aAlign;
  end;

var
  i: Integer;
  lList: IDeadlines;
begin
  SetLength(fGoalHints,aProps.Deadlines.Length);
  if aProps.Deadlines.Length > 0 then
  begin

     MainStatus.Visible := true;
     MainStatus.Panels.Clear;

     lList := aProps.Deadlines.Clone as IDeadlines;
     lList.Sort;
      for i := 0 to lList.Length - 1 do
      begin
        if i = (lList.Length - 1) then
           AddDeadlinePanel(lList[i],taRightJustify)
        else
           AddDeadlinePanel(lList[i],taRightJustify);
        SetDeadlineHint(i,lList[i]);
      end;


     ResizeStatusPanels;

  end
  else
  begin
    MainStatus.Panels.Clear;
    MainStatus.Visible := false;
  end;
end;

procedure TMainForm.ResizeStatusPanels;
const
  PADDING: Integer = 20;
var
  lCount: Integer;
  lWidth: Integer;
  lLastWidth: Integer;
  lAllWidth: Integer;
  lMaxWidth: Integer;
  i: Integer;
begin

  lCount := MainStatus.Panels.Count;
  if lCount > 0 then
  begin;
    lMaxWidth := MainStatus.Width;
    if (lCount > 1) and (MainStatus.Panels[lCount - 1].Alignment = taRightJustify) then
    begin
      lLastWidth := MainStatus.Canvas.TextWidth(MainStatus.Panels[lcount - 1].Text) + PADDING;
      MainStatus.Panels[lCount - 1].Width := lLastWidth;
      if lLastWidth > lMaxWidth then
         lLastWidth := lMaxWidth;
    end
    else
    begin
      lLastWidth := 0;
    end;
    lAllWidth := 0;
    for i := 0 to lCount - 1 do
    begin
      if (lLastWidth > 0) and (i = (lCount - 1)) then
         break;
      lWidth := MainStatus.Canvas.TextWidth(MainStatus.Panels[i].Text) + PADDING;
      if (lAllWidth + lLastWidth + lWidth) > lMaxWidth then
      begin
        lWidth := Max(0,lMaxWidth - (lAllWidth + lLastWidth));
      end;
      MainStatus.Panels[i].Width := lWidth;
      lAllWidth := lAllWidth + lWidth;
    end;

  end;

end;

procedure TMainForm.SetupGlyphs;
var
  lGlyph: TStewButtonGlyph;
  lBitmap: TCustomBitmap;
begin
  for lGlyph := Low(TStewButtonGlyph) to high(TStewButtonGlyph) do
  begin
    lBitmap := GetStewButtonIcon(lGlyph,ApplicationImages.Width);
    try
      ApplicationImages.Add(lBitmap,nil);
    finally
      lBitmap.Free;
    end;
  end;

  // Add items...
  NewProjectMenuItem.ImageIndex := ord(sbgNew);
  OpenProjectMenuItem.ImageIndex := ord(sbgOpen);
  PreferencesMenuItem.ImageIndex := ord(sbgPreferences);
  ExitMenuItem.ImageIndex := ord(sbgQuit);
  RefreshProjectMenuItem.ImageIndex := ord(sbgRefresh);
  ProjectSettingsMenuItem.ImageIndex := ord(sbgProperties);
  AboutMenuItem.ImageIndex := ord(sbgAbout);
end;

function TMainForm.MessageDialog(const Message: String; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; ButtonCaptions: array of String): Integer;
begin
  result := MessageDialog(Self.Caption,Message,DlgType,Buttons,ButtonCaptions);
end;

procedure TMainForm.ShowMessage(const Message: String; DlgType: TMsgDlgType; AcceptCaption: String);
begin
  MessageDialog(Message,DlgType,[mbOK],[AcceptCaption]);
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
  // set up logging...
  SetLogOutput(InitializeLog('application.log',true));
  LogHeader;
  // Set up async queue
  SetAsyncCallQueuer(@GUIQueueAsyncCall);

end.

