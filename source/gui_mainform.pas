unit gui_mainform;

{$mode objfpc}{$H+}

interface

{

TODO: To get this actually usable and publish it on Github, at least.
- Update the CLI version to work with the new schema for status, so I can still
use that for more complex tasks.

At that point, I can slowly start moving from the command line to the GUI.
The command line will probably never be completely deprecated, because the
scripting capabilities there are still quite useful.

TODO: At this point, I think I can consider it "done for now". Just clean up
todo's and collect them all into a single file for use later. Then, check into
github.

TODO: I don't really want to create a single instance application, or a single instance
per project application -- I would rather be able to have multiple people connect
to the same project if necessary (although this is going to take some testing if I
ever get it connected to the internet). However, it might be nice to have a warning
if the local project file is already open on the same machine. I could use an IPC
server to do this, lazarus has TSimpleIPCServer/Client, I just have to figure out
the system to figure this out.

TODO: We also need to make use of the 'closeQuery' on the editors, check if values
are modified prior to saving.
-- Actually, *this* is where we want to check modified, not the property objects themselves,
because we've been saving them immediately after writing out the data. So, we
can pretty much get rid of the "SetModified" and "ClearModified" on that.

TODO: Don't forget to revisit the "Tasks" Idea, and all of the other thoughts
I had on the stew-cli that can still be transferred over.

TODO: I just realized that the temporary back up files are going to appear as attachments,
which could really make the whole data structure for the cache huge given enough
time. But then, I also need to revisit those backups once I'm certain they're
working.

TODO: Need to come up with a more dynamic, active layout system for Documents.
The layout should depend on the aspect ratio of the screen as well as the size,
somewhat like the way it's done with Bootstrap: - if there's enough width to
have controls next to each other, do it. Otherwise set them up vertically.

TODO: All MessageDlg's should use MainForm.Title as the caption.

TODO: Some sort of GUI testing framework would be nice, so I can do regression
testing to make sure that bugs don't reappear.

TODO: Need to test this with documents containing '_'. I might have to "fix"
certain names before applying them.

TODO: A search function would basically bring up a DocumentQuery Tab. You would
specify what document to start at, what properties to filter by (including user
props), and whether to search recursively. The document would show up as a grid
showing all of the documents, including the color coding and glyphs of the
project manager. The document ID would be something like :query?category=Chapter;status=Complete...

TODO: At some point, I need to go through and convert all string-based and
file-based functions to UTF8 equivalents.

TODO: Put into some sort of coding guidelines the following rules:
- Code should only be made protected or private if it would cause the object
to go into an unknown state if it was called with incorrect parameters.
- Similarly, all functions and types should be in the interface unless calling
them with incorrect parameters would put the application into an unknown state.

These two rules allow for easier code re-use, since you're not restricting functions
that might actually be useful elsewhere (such as a filename processing function
or something). I don't guarantee I've followed this everywhere, because I've
come up with this rule after a lot of the development was done.

TODO: At some point in the future, I might feel the need to "clear" the cache
every once in a while. This is a little complex, but not too bad, we just
have to avoid removing documents that are currently open (locking required
if we haven't put that in yet) or new. But, we also can't remove siblings
of said locked or new documents either, or parents of them, because we'll
lose the listing state of the parent. Actually, this requires us to lock
them based on expanded nodes in the project manager as well.

TODO: Eventually, rework the JSON persistence stuff to work like the attachments,
with our own JSON objects. The setup is very similar to fpjson, except that the
JSON objects and arrays, and possibly even the primitives, are "typed":
- A given json object will have all of the mechanisms to retrieve data directly
from it's hash as protected properties, so they aren't available publicly.
- the object will not necessarily be TPersistent, it will at least be compiled
with $M+ or whatever, to allow it to have published properties.
- The public versions of these properties will work with RTTI. If you are attempting
to assign a value, it will first check if it's a published property, and if it is
it will try to assign the value to that property (if it's read-only, it will create
an error, or maybe it will call an assign method, so maybe this is a TPersistent).
-- These published properties can either 1) validate and assign using the internal
protected mechanisms for assigning properties or 2) store the data in some other
process.
-- Even if a property isn't published, the object can still override the data
by overriding a DoCreateMember which is given the property name and returns the
actually constructed object that is needed. This is an alternative mechanism.
- for the array, I'm not sure if published stuff will work, I'll have to look
into that, but it might just be some virtual 'Add' or 'insert' methods that
would get overridden.
- The streamer will have methods like ReadObject(AObject: TJSONObject), ReadArray(AArray: TJSONArray), ReadString, etc.
In which you provide the object to apply the data to. This streamer will do the
following:
-- if it has primitive data to assign to a property or index, it will attempt to assign directly.
-- if it has an array or an object, it will assume the object is going to be read-only
and that the objects will be assigned directly. In this case, it will retrieve the
value from the parent object, then call ReadObject on itself.
-- The one thing it will be restricted to: It only knows how to read JSON Objects,
strings, numbers and booleans, not anything else like TPersistent, or TComponent
or anything like that.

- This way, all you'd have to do to create a JSON object that has typed properties
as well as storing arbitrary data, is to create a subclass of this, and add in
some published properties with your own backed private data (or even storing them
in the hash itself), and you can control what kinds of values go into it.


TODO: Test with an empty or new project again.

TODO: FPC now has 'strict private' and 'strict protected' that make things
work the way private and protected are supposed to work (just being in the
same unit doesn't help). Convert everything to that for clearer documentation
on what fields are actually supposed to be private, and which ones are supposed
to be unit private.

TODO: Also, I can use nested classes for some of the 'deferred tasks' things,
also almost anywhere that I've got a class defined in implementation. This
makes for clearer documentation.

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

TODO: "Note Stew":
- this would be an application very similar to Stew (which would become Story Stew).
In fact, it's similar enough that it could just be a different project that shares
a lot of the same UI, perhaps with just a few ifdefs, or perhaps an "application spec"
file that sets some global variables.
- The primary differences:
  - metadata consists only of the primary document and properties. There are no
    'notes', 'synopsis', etc. Because the primary document is the notes.
  - some of the properties are removed because they're unnecessary (published is
    out, but categories and statuses might still be available).
  - the document editor is replaced with a note editor, most of whose body is a
    RichTextEditor, or something like that (depending on how we store the notes,
    in fact the body might differ based on the extension).
    - It would also still have some properties, but these are less obtrusive, but
      would include a user data editor.
  - almost everything else should work just the same. So, it's a matter of:
    - having a different "type" for the metadata
    - having a different "type" for the main document interface.


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

TODO: Divide the units into three parts:
- UI (forms, frames and controls): prefix is ui
- Stew Data Structures: prefix is stew
- Platform Interface (basically just stewfile, stewasync, future stewshell): prefix is sys

}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, ComCtrls, stew_project, fgl, gui_config, contnrs,
  simpleipc, gui_editorframe, stew_types, sys_async, sys_file, sys_os;

type

  // TODO: It might be better to be able to register events for a specific action
  // and even document ID. Currently, each tab is a separate observer. If I add
  // in other objects which might observe, and I get to a point where there's
  // a lot, then cycling through the list could get expensive.
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
const PreferencesDocumentID: TDocumentID = ':preferences';
const ProjectSettingsDocumentID: TDocumentID = ':project settings';

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
    NotifyObservers(mfaDocumentSynopsisLoading,Document);

end;

procedure TMainForm.DocumentAttachmentSaveConflicted(Sender: TObject;
  Document: TDocumentID; AttachmentName: String);
begin
  if AttachmentName = 'Synopsis' then
    NotifyObservers(mfaDocumentSynopsisSaveConflicted,Document);

end;

procedure TMainForm.DocumentAttachmentSaved(Sender: TObject;
  Document: TDocumentID; AttachmentName: String);
begin
  if AttachmentName = 'Synopsis' then
    NotifyObservers(mfaDocumentSynopsisSaved,Document);

end;

procedure TMainForm.DocumentAttachmentSaving(Sender: TObject;
  Document: TDocumentID; AttachmentName: String);
begin
  if AttachmentName = 'Synopsis' then
    NotifyObservers(mfaDocumentSynopsisSaving,Document);

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
              'The parent document''s ID was "' + Document + '".' + LineEnding +
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
              'The document''s ID was ' + Document + '.' + LineEnding +
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
     NotifyObservers(mfaDocumentSynopsisLoaded,Document);
  // TODO: What other attachments;
end;

procedure TMainForm.DocumentAttachmentError(Sender: TObject;
  Document: TDocumentID; Attachment: String; Error: String);
begin
  ShowMessage('An error occurred while saving or loading an attachment.' + LineEnding +
              'The document''s ID was ' + Document + '.' + LineEnding +
              'The attachment type was ' + Attachment + '.' + LineEnding +
              Error + LineEnding +
              'You may want to restart the program, or wait and try your task again later');
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
  NotifyObservers(mfaDocumentPropertiesLoaded,Document);
end;

procedure TMainForm.DocumentPropertiesSaveConflicted(Sender: TObject;
  Document: TDocumentID);
begin
   if MessageDlg('The document properties file has changed on the disk since the last time it was loaded.' + LineEnding +
             'Document ID: ' + Document + LineEnding +
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
  try
    fConfig.Save;
  except
    // TODO:
    // Error occurred while saving settings, how to alert the user?
    // at this point, if I show a message, it messes with the destruction
    // of the form. I can't output to the console because of Windows.
  end;
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
      // TODO: We actually have to have the frames *use* this method now.
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

  if (stewFolder <> '') and (stewFolder <> PromptForProjectArgument) then
  begin
    fProject := TProtectedStewProject.Create(LocalFile(stewFolder));
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
  NotifyObservers(mfaProjectPropertiesLoading);
end;

procedure TMainForm.ProjectPropertiesSaving(Sender: TObject);
begin
  NotifyObservers(mfaProjectPropertiesSaving);

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
  fConfig.MRUProject := fProject.DiskPath.ID;
  // save the configuration, so that the MRU Project becomes available
  // if we open up another project in the midst of this.
  fConfig.Save;
  Self.Caption := Application.Title + ' - ' + fProject.GetProjectName;
  Enabled := true;
  with fProject.GetDocument(RootDocument) do
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
  Project.GetDocument(RootDocument).ListDocuments(true);
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
  // TODO: Perhaps I should "load" the current settings, so I don't
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
  // TODO: This is where we set up the basic layout for the application.
  // Any changes to where things are laid out should occur here.
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

initialization

  SetAsyncCallQueuer(@QueueAsyncCall);

end.

