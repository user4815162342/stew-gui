unit stewprojectsettingseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  Grids, steweditorframe, stewproperties, stewmainform, stewjsoneditor, stewproject;

// TODO: Need to enable/disable state based on the loading state of the
// project properties as well. At the very least, I can't save while already
// saving. Right now, it's not a problem, but in the future when saving is
// done in a separate thread against the internet, it will be a problem.
// The easiest way, of course, is to just disable to whole interface while
// some sort of saving is going on.
// - probably the best way is to have the MainForm handle events on the
// project, and broadcast the events through the fpoobserver stuff.
// - for part of this, instead of just handling loaded/failed, etc. There
// should be a FilingStateChanged event. There probably should be a 'new'
// state to check whether the file has been loaded or not before calling load,
// but this would also mean that we'd have to keep a 'failed' state when
// a load fails (when a saving fails, we can probably keep the old loaded
// state, however. But, we shouldn't be able to save again if the load
// failed).

// TODO: More properties that need to be handled.
//    - editors for certain file extensions (See Preferences Menu).
//    - defaultPublishExtension
//    - category definition publish properties (see stew-cli):
//      - publishTitle (whether the 'title' is published)
//      - publishTitleLevel
//      - publishTitlePrefix
//      - publishMarkerBefore
//      - publishMarkerAfter
//      - publishMarkerBetween
//    - "Type" of project:
//      - Notebook: In this project type, there is only a 'primary', no notes,
//        and no thumbnail, and no synopsis, and as many editors are set to
//        internal as possible. There is also no "publish" and "title" properties,
//        and the publishing stuff which might be needed in the stew file.
//      - Manuscript: In this project type, there are separate notes and things,
//        basically, everything that's always been in stew.
//    - Root Index (if allowed for editing in the documents, then it should be here too).
//    - Notes from Root properties.

type

  { TProjectSettingsEditor }

  TProjectSettingsEditor = class(TEditorFrame)
    CategoryDefinitionsGrid: TStringGrid;
    UserPropertiesLabel: TLabel;
    UserPropertiesPanel: TPanel;
    StatusDefinitionsGrid: TStringGrid;
    CategoryDefinitionsHeaderPanel: TPanel;
    StatusDefinitionsHeaderPanel: TPanel;
    CategoryDefinitionsLabel: TLabel;
    StatusDefinitionsLabel: TLabel;
    StatusDefinitionsPanel: TPanel;
    DefaultDocExtensionEdit: TEdit;
    DefaultThumbnailExtensionEdit: TEdit;
    DefaultNotesExtensionEdit: TEdit;
    DefaultDocExtensionLabel: TLabel;
    DefaultThumbnailExtensionLabel: TLabel;
    DefaultNotesExtensionLabel: TLabel;
    DefaultDocExtensionPanel: TPanel;
    DefaultThumbnailExtensionPanel: TPanel;
    DefaultNotesExtensionPanel: TPanel;
    GridsPanel: TPanel;
    CategoryDefinitionsPanel: TPanel;
    RefreshButton: TToolButton;
    SaveButton: TToolButton;
    EditNotesButton: TToolButton;
    procedure EditNotesButtonClick(Sender: TObject);
    procedure ObserveMainForm(aAction: TMainFormAction; {%H-}aDocument: TDocumentID);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    procedure SetupControls;
  private
    { private declarations }
    fUserPropertiesEditor: TJSONEditor;
    procedure ShowDataToUser;
    function WriteDataFromUser: Boolean;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs, Graphics, fpjson, stewpersist;

{$R *.lfm}

{ TProjectSettingsEditor }

procedure TProjectSettingsEditor.RefreshButtonClick(Sender: TObject);
begin
  // TODO: Test this.
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
       MainForm.Project.Properties.Load;
       // this should automatically call something on the main form
       // which will notify that the project has refreshed, and do so.
  end;
end;

procedure TProjectSettingsEditor.SaveButtonClick(Sender: TObject);
begin
  ShowMessage('Sorry, I can''t save these things yet');
  // TODO: We have to fix the statuses and categories before
  // we can save them. Right now status is a string list in the
  // stew cli, but a map like categories here. Statuses actually
  // need to maintain a workflow order, however, allowing for the
  // nextstatus command. Which means that I need them to be an array
  // of objects with unique values. I might as well do the same to
  // categories.
  // - In stew CLI, I need to change categories to be an array of objects
  // instead of a map. I need to be able to accept the old data as well,
  // however. Which means I need a "converter"
  // - In this GUI, I need to change both into TCollections after all.
  // That actually simplifies, to some extent, the serializing.

  // TODO: WriteDataFromUser;
end;

procedure TProjectSettingsEditor.SetupControls;
var
  canEdit: Boolean;
begin
  canEdit := (MainForm.Project <> nil) and
             (MainForm.Project.IsOpened) and
             (MainForm.Project.Properties.FilingState in [fsLoaded]);
  RefreshButton.Enabled := canEdit;
  SaveButton.Enabled := canEdit;
  EditNotesButton.Enabled := canEdit;
  DefaultDocExtensionLabel.Enabled := canEdit;
  DefaultDocExtensionEdit.Enabled := canEdit;
  DefaultNotesExtensionLabel.Enabled := canEdit;
  DefaultNotesExtensionEdit.Enabled := canEdit;
  DefaultThumbnailExtensionLabel.Enabled := canEdit;
  DefaultThumbnailExtensionEdit.Enabled := canEdit;
  CategoryDefinitionsGrid.Enabled := canEdit;
  CategoryDefinitionsLabel.Enabled := canEdit;
  StatusDefinitionsLabel.Enabled:=canEdit;
  StatusDefinitionsGrid.Enabled := canEdit;
  UserPropertiesLabel.Enabled := canEdit;
  fUserPropertiesEditor.Enabled := canEdit;

end;

procedure TProjectSettingsEditor.ObserveMainForm(aAction: TMainFormAction;
  aDocument: TDocumentID);
begin
  case aAction of
    mfaProjectPropertiesLoaded, mfaProjectPropertiesLoading, mfaProjectPropertiesSaved, mfaDocumentPropertiesSaving:
      ShowDataToUser;
  end;
end;

procedure TProjectSettingsEditor.EditNotesButtonClick(Sender: TObject);
begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
       MainForm.Project.EditDocumentNotes(RootDocument);
  end;

end;

function ColorToHex(x: TColor): String;
begin
  result := '#' +
     IntToHex(Red(x),2) +
     IntToHex(Green(x),2) +
     IntToHex(Blue(x),2);
end;

procedure TProjectSettingsEditor.ShowDataToUser;
var
  props: TProjectProperties;
  i: Integer;
  aName: String;
begin
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) and (MainForm.Project.Properties.FilingState in [fsLoaded]) then
  begin
    props := MainForm.Project.Properties;
    DefaultDocExtensionEdit.Text := props.defaultDocExtension;
    DefaultNotesExtensionEdit.Text := props.defaultNotesExtension;
    DefaultThumbnailExtensionEdit.Text := props.defaultThumbnailExtension;

    with CategoryDefinitionsGrid do
    begin
      Clear;
      for i := 0 to props.categories.NameCount - 1 do
      begin
        aName := props.categories.Names[i];
        RowCount := RowCount + 1;
        Cells[0,i] := aName;
        Cells[1,i] := IntToHex((props.categories[aName] as TKeywordDefinition).color,6);
        Cells[2,i] := BoolToStr(props.defaultCategory = aName,'Yes','No');
      end;
      AutoSizeColumns;
    end;

    with StatusDefinitionsGrid do
    begin
      Clear;
      for i := 0 to props.statuses.NameCount - 1 do
      begin
        aName := props.statuses.Names[i];
        RowCount := RowCount + 1;
        Cells[0,i] := aName;
        Cells[1,i] := IntToHex((props.statuses[aName] as TKeywordDefinition).color,6);
        Cells[2,i] := BoolToStr(props.defaultStatus = aName,'Yes','No');
      end;
      AutoSizeColumns;
    end;

    fUserPropertiesEditor.SetJSON(props.user);

  end;
  SetupControls;
end;

function TProjectSettingsEditor.WriteDataFromUser: Boolean;
var
  props: TProjectProperties;
  aUser: TJSONData;
begin
  result := false;
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    props := MainForm.Project.Properties;
    props.defaultDocExtension := DefaultDocExtensionEdit.Text;
    props.defaultNotesExtension:= DefaultNotesExtensionEdit.Text;
    props.defaultThumbnailExtension:=DefaultThumbnailExtensionEdit.Text;
    // TODO: Load Category Definitions and default category.
    // TODO: Load Status Definitions and default status.

    // I need to create and destroy this object because
    // it gets cloned when setting the property.
    aUser := fUserPropertiesEditor.CreateJSON;
    try
      props.user := aUser;
    finally
      aUser.Free;
    end;

    props.Save;
    result := true;

  end
end;

constructor TProjectSettingsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := 'Project Settings';
  fUserPropertiesEditor := TJSONEditor.Create(UserPropertiesPanel);
  fUserPropertiesEditor.Parent := UserPropertiesPanel;
  //fUserPropertiesEditor.Height := 192;
  fUserPropertiesEditor.Align := alClient;
  fUserPropertiesEditor.BorderSpacing.Top := 10;

  ShowDataToUser;
  MainForm.Observe(@ObserveMainForm);
end;

destructor TProjectSettingsEditor.Destroy;
begin
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

end.

