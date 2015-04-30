unit stewmainform;

{$mode objfpc}{$H+}

interface

{

TODO: To get this actually usable:
1. Next, work on the document properties and editors. The infrastructure
looks a lot like the project settings editor, of course.
2. Once I have document properties, I should be able to easily 'sort' the
documents based on the index property of the folder and root properties.
3. A button for opening documents and notes.

At that point, I can slowly start moving from the command line to the GUI.
The command line will probably never be completely deprecated, because the
scripting capabilities there are still quite useful.

TODO: FPC now has 'strict private' and 'strict protected' that make things
work the way private and protected are supposed to work (just being in the
same unit doesn't help). Convert everything to that for clearer documentation
on what fields are actually supposed to be private, and which ones are supposed
to be unit private.

TODO: Also, I can use nested classes for some of the 'deferred tasks' things,
also almost anywhere that I've got a class defined in implementation. This
makes for clearer documentation.

TODO: Working on rough up of interface.
- start laying out some components on the DocumentInspector, based on the stuff below.
(Now that I see what I've got, I have a lot more room, so maybe I don't need tabs on it,
but design it with them in mind anyway).

TODO: Then, push it up to Github for anyone who's interested to see what I'm doing.


TODO: When storing and opening project-dependent GUI states (open files, expanded folders),
these should go in the desktop's preferences directory for now. My reasoning is that
if it stays in the directory of the project, not only do I have to have one more
async file to worry about, but any "project sharing" that's going on will mean
other people find the tabs they have opened gone when they open it up after
someone else has opened it up. At some point in the future, I can figure out a
way to store it on a per-user basis, so that someone can have the same tabs open
no matter where they are, but I'm not really certain that's important right now
(that might be useful for a web app version, but not this).
- Anyway, the settings would best be stored in with the MRU data in the config
file. That way, they get cleaned out after a bunch of projects, and I don't have
to worry about trying to match up the filename with the path of the document.

TODO: Some interface ideas:
- Instead of the "filter" button, make it a combobox, plus add a "search" button next to it, and an "advanced" search button.
Although that's sometime in the future. Maybe get rid of the thing for now, until
I find a need for it. Something like this might work better as a search bar in
a toolbar across the top of the form.
- Application Settings (stewconfig) and Project Settings (_stew.json) should be
opened in the central pain, in a tab.
- Close, Edit, etc. buttons should be in a toolbar across the top of the document inspector form,
above all of it's contents. In theory, it should be possible to add buttons as
necessary.
- said toolbar could also include the "path" to the document.

TODO: Need a central repository of bitmaps and icons for buttons. I would like
to be able to retrieve these from the system, if possible. Otherwise, here
are some good icons:
/usr/share/lazarus/1.2.6/lcl/images
http://tango.freedesktop.org/Tango_Desktop_Project
and, of course, the silk icons are good.

Also, look at Dialogs.GetDialogIcon, which uses ThemeServices (What is that?)
and CreateBitmapFromLazarusResource if that doesn't work.

TODO: For the JSON Editors, look at the demo JSONViewer project at /usr/share/lazarus/1.2.6/tools/jsonviewer
then again, maybe not, because it doesn't do it quite the way I expected.


TODO: One of the possible frames is a "grid" frame which shows the contents of one
directory in the project only, with a bunch of it's standard properties, and
possibly the user property.

TODO: Document Frames include an optional "Content" and "Notes" tabsheet. If the
document type for these is a .txt or .rtf, then an editor can be shown. Otherwise,
the tabs for these will be replaced by buttons which open up the item in another
process. I'm not sure exactly how to do this, I don't think it's possible to add
a button, but perhaps it's possible to show a tab.
-- I'm thinking a button panel at the bottom of the tab, which shows up on every
tab (or is actually set up above the tabs). This button panel will include a
"close" button (when on Windows, which doesn't support close buttons on tabs),
and "Content" and "Notes" buttons, and maybe some other tools.

TODO: I've got something started. Next:
- Rough up the rest of the interface to make it look nice. Just keep controls disabled
until I finish each feature.

- A window with two panes:
  - Left Pane: "Explorer"
    - would be a treeview, only getting file system data when node is expanded.
    - A "filter" textbox and button at the top to show only content which matches certain filters.
    - A button would expand this whole left pane and turn it into a grid, showing
      properties of each document (user-customizable what properties are shown).
      -- Would need to switch to VirtualTreeView to get the columns
      - It's even possible that the properties could be "edited" in the grid.
  - Right Pane:
    - Document name, and possibly last modified time.
    - Synopsis: A text box allowing review/editing of synopsis.
    - Properties: A "Treeview" of properties for the currently selected document, with some editing capabilities.
    - Contents: A list of the contents of the file, if it's a folder.
    - Edit Buttons: Buttons to edit notes, edit primary, edit thumbnail, backup. Possibly more.
- Consider Making the UI layout dockable, so the user can drag things around. But,
instead of keeping this unlocked, make the docking locked by default, and then save
a few standard layouts to pick from in the view menu. This prevents accidental "weird"
docking bugs.

- The documents aren't showing up in order, which means I do have to load properties
right now to get those orders.
- remember previously open projects, and previously expanded nodes within a project.


TODO: Support some internal editors:
- RichTextEditor for RTF, possibly others once I can find support
- TextEditor for TXT
- SynEdit for Markdown, possibly some other stuff.

}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, ComCtrls, stewproject, fgl, stewconfig, contnrs, steweditorframe;

type

  // TODO: the project refresh thing isn't really something I need to broadcast.
  // It's really just meant for the project inspector to re-list items. Perhaps
  // it should be some sort of menu Item that gets automatically added by the
  // project inspector frame.
  // TODO: Make use of the properties loaded and saved events.
  TMainFormAction = (mfaProjectPropertiesLoaded,
                     mfaProjectPropertiesSaved,
                     mfaRootPropertiesLoaded,
                     mfaRootPropertiesSaved,
                     mfaDocumentsListed,
                     mfaDocumentPropertiesLoaded,
                     mfaDocumentPropertiesSaved);
  TMainFormObserverHandler = procedure(aAction: TMainFormAction; aDocument: TDocumentID) of object;
  TMainFormObserverList = specialize TFPGList<TMainFormObserverHandler>;

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    PreferencesMenuItem: TMenuItem;
    ProjectSettingsMenuItem: TMenuItem;
    RefreshProjectMenuItem: TMenuItem;
    ProjectMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    OpenProjectDialog: TSelectDirectoryDialog;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure DocumentListError(Sender: TObject; Document: TDocumentID;
      Error: String);
    procedure DocumentsListed(Sender: TObject; Document: TDocumentID);
    procedure DocumentTabCloseRequested(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PreferencesMenuItemClick(Sender: TObject);
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
    procedure RootProjectPropertiesError(Sender: TObject; aError: String);
    procedure RootProjectPropertiesLoaded(Sender: TObject);
    procedure RootProjectPropertiesSaveConflicted(Sender: TObject);
    procedure RootProjectPropertiesSaved(Sender: TObject);
  protected
    procedure StartupCheckProject({%H-}Data: PtrInt);
    procedure StartupIfProjectExists(aValue: Boolean);
    procedure StartupIfProjectParentDirectoryExists(aValue: Boolean);
    procedure NotifyObservers(aAction: TMainFormAction; aDocument: TDocumentID = '');
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

  { HStewFrameHelper }

var
  MainForm: TMainForm;

implementation

uses
  stewprojectinspector, stewdocumenteditor, stewasync, stewpreferenceseditor, stewprojectsettingseditor, LCLProc, stewabout;

{$R *.lfm}

// using ':' at the beginning because that should not be a valid filename.
const PreferencesDocumentID: TDocumentID = ':preferences';
const ProjectSettingsDocumentID: TDocumentID = ':project settings';

{ TMainForm }

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.DocumentListError(Sender: TObject; Document: TDocumentID;
  Error: String);
begin
  ShowMessage('An error occurred while listing documents.' + LineEnding +
              'The parent document''s ID was "' + Document + '".' + LineEnding +
              Error + LineEnding +
              'You may want to restart the program, or wait and try your task again later');

end;

procedure TMainForm.DocumentsListed(Sender: TObject; Document: TDocumentID);
begin
  NotifyObservers(mfaDocumentsListed,Document);
end;

procedure TMainForm.DocumentPropertiesError(Sender: TObject;
  Document: TDocumentID; Error: String);
begin
  ShowMessage('An error occurred while saving or loading the document properties.' + LineEnding +
              'The document''s ID was ' + Document + '.' + LineEnding +
              Error + LineEnding +
              'You may want to restart the program, or wait and try your task again later');
end;

procedure TMainForm.DocumentPropertiesLoaded(Sender: TObject;
  Document: TDocumentID);
begin
  Enabled := true;
  NotifyObservers(mfaDocumentPropertiesLoaded,Document);
end;

procedure TMainForm.DocumentPropertiesSaveConflicted(Sender: TObject;
  Document: TDocumentID);
begin
   if MessageDlg('The document properties file has changed on the disk since the last time it was loaded.' + LineEnding +
             'Document ID: ' + Document + LineEnding +
             'Would you like to overwrite it''s contents?',mtWarning,mbYesNo,0) = mrYes then
   begin
     fProject.GetDocumentProperties(Document).Save(true);
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
  WriteUISettings;
  try
    fConfig.Save;
  except
    // TODO:
    // Error occurred while saving settings, how to alert the user?
    // at this point, if I show a message, it messes with the destruction
    // of the form. I can't output to the console because of
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  stewFolder: String;
begin

  fOpenDocuments := TObjectList.create(false);

  stewFolder := '';
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
    stewFolder := Application.Params[1];
  end
  else
  begin
    stewFolder := fConfig.MRUProject;
  end;

  if stewFolder <> '' then
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

function TMainForm.GetProject: TStewProject;
begin
  result := fProject;
end;

procedure TMainForm.StartupIfProjectParentDirectoryExists(aValue: Boolean);
begin
  if not aValue then
  begin
    // TODO: Search the parents (use a method on the project, so *it* can
    // change the directory instead). If not found, then ask if the user
    // wants to create a new one (again, use a method on the project).
    if MessageDlg('No stew project could be found.' + LineEnding +
               'Would you like to create one at: ' + fProject.DiskPath + '?',mtConfirmation,mbOKCancel,0) =
       mrOK then
    begin;
      fProject.OpenNewAtPath;

    end
    else
      Close;
  end
  else
  begin
    ShowMessage('Found a project at: ' + fProject.DiskPath);

  end;

end;

procedure TMainForm.PreferencesMenuItemClick(Sender: TObject);
begin
  OpenPreferences;
end;

procedure TMainForm.ProjectLoadFailed(E: String);
begin
  ShowMessage('The project couldn''t be loaded.' + LineEnding +
              'The error message was: ' + E + LineEnding +
              'This program will close.');
  Close;
end;

procedure TMainForm.ProjectOpened(Sender: TObject);
begin
  fConfig.MRUProject := fProject.DiskPath;
  Self.Caption := Application.Title + ' - ' + fProject.GetProjectName;
  // But... we don't actually enable anything, not until we get a project properties loaded.
  fProject.ListDocuments(RootDocument);
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
  NotifyObservers(mfaProjectPropertiesLoaded);
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
  NotifyObservers(mfaProjectPropertiesSaved);
end;

procedure TMainForm.ProjectSettingsMenuItemClick(Sender: TObject);
begin
  OpenProjectSettings;
end;

procedure TMainForm.RefreshProjectMenuItemClick(Sender: TObject);
begin
  Project.ListDocuments(RootDocument);
end;

procedure TMainForm.RootProjectPropertiesError(Sender: TObject; aError: String);
begin
  ShowMessage('An error occurred while saving or loading the project root properties.' + LineEnding +
              aError + LineEnding +
              'You may want to restart the program, or wait and try your task again later');
end;

procedure TMainForm.RootProjectPropertiesLoaded(Sender: TObject);
begin
  NotifyObservers(mfaRootPropertiesLoaded);
end;

procedure TMainForm.RootProjectPropertiesSaveConflicted(Sender: TObject);
begin
  if MessageDlg('The root properties file has changed on the disk since the last time it was loaded.' + LineEnding +
             'Would you like to overwrite it''s contents?',mtWarning,mbYesNo,0) = mrYes then
   begin
     fProject.GetRootProperties.Save(true);
   end;
end;

procedure TMainForm.RootProjectPropertiesSaved(Sender: TObject);
begin
  NotifyObservers(mfaRootPropertiesSaved);
end;


procedure TMainForm.StartupCheckProject(Data: PtrInt);
begin
  if fProject = nil then
  begin
    if OpenProjectDialog.Execute then
    begin

      fProject := TProtectedStewProject.Create(OpenProjectDialog.FileName);

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
  fProject.OnRootPropertiesError:=@RootProjectPropertiesError;
  fProject.OnRootPropertiesLoaded:=@RootProjectPropertiesLoaded;
  fProject.OnRootPropertiesSaveConflicted:=@RootProjectPropertiesSaveConflicted;
  fProject.OnRootPropertiesSaved:=@RootProjectPropertiesSaved;
  fProject.OnDocumentPropertiesError:=@DocumentPropertiesError;
  fProject.OnDocumentPropertiesLoaded:=@DocumentPropertiesLoaded;
  fProject.OnDocumentPropertiesSaveConflicted:=@DocumentPropertiesSaveConflicted;
  fProject.OnDocumentPropertiesSaved:=@DocumentPropertiesSaved;
  fProject.OnDocumentsListed:=@DocumentsListed;
  fProject.OnDocumentListError:=@DocumentListError;
  fProject.OpenAtPath(@StartupIfProjectExists,@ProjectLoadFailed);

end;

procedure TMainForm.StartupIfProjectExists(aValue: Boolean);
begin
  if not aValue then
  begin
    // TODO: Search the parents (use a method on the project, so *it* can
    // change the directory instead). If not found, then ask if the user
    // wants to create a new one (again, use a method on the project).
    if MessageDlg('There is no stew project at: ' + fProject.DiskPath + LineEnding +
               'Would you like to search for one in parent directories?',mtConfirmation,mbOKCancel,0) =
       mrOK then
    begin;
      fProject.OpenInParentDirectory(@StartupIfProjectParentDirectoryExists,@ProjectLoadFailed);

    end
    else
      StartupIfProjectParentDirectoryExists(false);
  end

end;

procedure TMainForm.NotifyObservers(aAction: TMainFormAction; aDocument: TDocumentID = '');
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
  // TODO: This is where we set up the basic layout for the application.
  // Any changes to where things are laid out should occur here.
  LayoutFrame(TProjectInspectorFrame,alLeft);
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
    if aDocument = PreferencesDocumentID then
      EditorClass := TApplicationPreferencesEditor
    else if aDocument = ProjectSettingsDocumentID then
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
  OpenDocument(PreferencesDocumentID);
end;

procedure TMainForm.OpenProjectSettings;
begin
  OpenDocument(ProjectSettingsDocumentID);
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


end.

