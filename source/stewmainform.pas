unit stewmainform;

{$mode objfpc}{$H+}

interface

{

TODO: Working on rough up of interface.
- start laying out some components on the DocumentInspector, based on the stuff below.
(Now that I see what I've got, I have a lot more room, so maybe I don't need tabs on it,
but design it with them in mind anyway).

TODO: Then, push it up to Github for anyone who's interested to see what I'm doing.


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

  TMainFormAction = (mfaProjectRefresh);
  TMainFormObserverHandler = procedure(aAction: TMainFormAction) of object;
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
    procedure DocumentTabCloseRequested(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PreferencesMenuItemClick(Sender: TObject);
    procedure ProjectSettingsMenuItemClick(Sender: TObject);
    procedure RefreshProjectMenuItemClick(Sender: TObject);
  private
    { private declarations }
    fProject: TStewProject;
    fConfig: TStewApplicationConfig;
    fObservers: TMainFormObserverList;
    fFrames: array[TAlign] of TControl;
    fSplitters: array[TAlign] of TControl;
    fDocumentPane: TAlign;
    // FUTURE: Should be a hash list, so we can look things up by ID.
    fOpenDocuments: TObjectList;
  protected
    procedure StartupCheckProject({%H-}Data: PtrInt);
    procedure StartupProjectExists(aValue: Boolean);
    procedure NotifyObservers(aAction: TMainFormAction);
    procedure ReadUISettings;
    procedure WriteUISettings;
    procedure LayoutFrames;
  public
    { public declarations }
    property Project: TStewProject read fProject;
    function OpenDocument(aDocument: TDocumentID): TEditorFrame;
    procedure OpenPreferences;
    procedure OpenProjectSettings;
    procedure Observe(aObserver: TMainFormObserverHandler);
    procedure Unobserve(aObserver: TMainFormObserverHandler);
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
    fProject := TStewProject.Create(stewFolder);
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

procedure TMainForm.PreferencesMenuItemClick(Sender: TObject);
begin
  OpenPreferences;
end;

procedure TMainForm.ProjectSettingsMenuItemClick(Sender: TObject);
begin
  OpenProjectSettings;
end;

procedure TMainForm.RefreshProjectMenuItemClick(Sender: TObject);
begin
  NotifyObservers(mfaProjectRefresh);
end;

procedure TMainForm.StartupCheckProject(Data: PtrInt);
begin
  if fProject = nil then
  begin
    if OpenProjectDialog.Execute then
    begin

      fProject := TStewProject.Create(OpenProjectDialog.FileName);

    end
    else
    begin
      Close;
      Exit;
    end;
  end;

  fProject.Exists(@StartupProjectExists,@Application.ShowException);

end;

procedure TMainForm.StartupProjectExists(aValue: Boolean);
begin
  if not aValue then
  begin
    // TODO: Should ask if you want to create it.
    ShowMessage('Couldn''t find stew project at: ' + fProject.DiskPath);
    Close;
  end
  else
  begin
    Enabled := true;
    fConfig.MRUProject := fProject.DiskPath;
    Self.Caption := Application.Title + ' - ' + fProject.GetProjectName;

    NotifyObservers(mfaProjectRefresh);


  end;

end;

procedure TMainForm.NotifyObservers(aAction: TMainFormAction);
var
  i: Integer;
begin
  if fObservers <> nil then
  begin
    for i := fObservers.Count - 1 downto 0 do
    begin
      fObservers[i](aAction);
    end;
  end;
end;

procedure TMainForm.ReadUISettings;
begin
  Height := fConfig.MainWindow.GetHeight(Height);
  Width := fConfig.MainWindow.GetWidth(Width);
  if fConfig.MainWindow.GetMaximized(WindowState = wsMaximized) then
  begin
    WindowState := wsMaximized;
  end;
  if fFrames[alLeft] <> nil then
    fFrames[alLeft].Width := fConfig.MainWindow.GetLeftPaneWidth(fFrames[alLeft].Width);
  if fFrames[alRight] <> nil then
    fFrames[alRight].Width := fConfig.MainWindow.GetLeftPaneWidth(fFrames[alRight].Width);
  if fFrames[alTop] <> nil then
    fFrames[alTop].Height := fConfig.MainWindow.GetLeftPaneWidth(fFrames[alTop].Height);
  if fFrames[alBottom] <> nil then
    fFrames[alBottom].Height := fConfig.MainWindow.GetLeftPaneWidth(fFrames[alBottom].Height);

end;

procedure TMainForm.WriteUISettings;
begin
  if WindowState = wsMaximized then
  begin
    fConfig.MainWindow.SetMaximized(true);
    // don't set the height and width, so they're remembered from last time.
  end
  else
  begin
    fConfig.MainWindow.SetMaximized(false);
    fConfig.MainWindow.SetHeight(Height);
    fConfig.MainWindow.SetWidth(Width);
  end;

  if fFrames[alLeft] <> nil then
    fConfig.MainWindow.SetLeftPaneWidth(fFrames[alLeft].Width);
  if fFrames[alRight] <> nil then
    fConfig.MainWindow.SetLeftPaneWidth(fFrames[alRight].Width);
  if fFrames[alTop] <> nil then
    fConfig.MainWindow.SetLeftPaneWidth(fFrames[alTop].Height);
  if fFrames[alBottom] <> nil then
    fConfig.MainWindow.SetLeftPaneWidth(fFrames[alBottom].Height);


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

