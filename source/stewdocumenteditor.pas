unit stewdocumenteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  stewproject, steweditorframe, stewjsoneditor, stewmainform;

type

{
TODO: Need to enable disable based on loading state of properties
and some other things, not just setting a value.

TODO: Need to load the synopses.

TODO: Possibly also need to have a contents grid which shows the
values of specific properties.

TODO: Keep the paradigms similar in projectsettingseditor.

TODO: Edit the following properties...
  - Directory Index? Although that might be managed in another way.
  - x Title
  - x Category
  - x Status
  - x Publish
  - References?
  - Tags?
  - x Notes
  - x Synopsis
  - TODO: Thumbnails -- should be done by clicking on the thumbnail image itself.
  - TODO: Backups

TODO: Thumbnail image should only show up if it's set.

TODO: Someday, I should revisit the layout... I'd like something more like a
flow-based layout.

FUTURE: The 'Notes' and 'Edit' buttons should have some way of indicating if
the file exists already. Or, we need to come up with a different way of doing that.
}

  { TDocumentEditor }

  TDocumentEditor = class(TEditorFrame)
    CategoryEdit: TComboBox;
    CategoryLabel: TLabel;
    CategoryPanel: TPanel;
    SynopsisEdit: TMemo;
    SynopsisLabel: TLabel;
    SynopsisPanel: TPanel;
    PublishEdit: TCheckBox;
    StatusEdit: TComboBox;
    StatusLabel: TLabel;
    StatusPanel: TPanel;
    TitleEdit: TEdit;
    HeaderPanel: TPanel;
    ThumbnailImage: TImage;
    HeaderFieldsPanel: TPanel;
    TitleLabel: TLabel;
    TitlePanel: TPanel;
    DocumentIDLabel: TLabel;
    SaveButton: TToolButton;
    RefreshButton: TToolButton;
    EditPrimaryButton: TToolButton;
    EditNotesButton: TToolButton;
    UserPropertiesLabel: TLabel;
    UserPropertiesPanel: TPanel;
    procedure EditNotesButtonClick(Sender: TObject);
    procedure EditPrimaryButtonClick(Sender: TObject);
    procedure ObserveMainForm(aAction: TMainFormAction; aDocument: TDocumentID);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    { private declarations }
  private
    procedure SetEditingEnabled(AValue: Boolean);
  protected
    fUserPropertiesEditor: TJSONEditor;
    procedure SetDocument(AValue: TDocumentID); override;
    procedure ShowDataToUser;
    procedure UpdateLookupLists;
    property EditingEnabled: Boolean write SetEditingEnabled;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs, stewproperties;

{$R *.lfm}

{ TDocumentEditor }

procedure TDocumentEditor.SaveButtonClick(Sender: TObject);
begin
  ShowMessage('Sorry, I can''t save these things yet');

  // TODO: WriteDataFromUser;

end;

procedure TDocumentEditor.SetEditingEnabled(AValue: Boolean);
begin
  // TODO: Instead of setting it to a specific state, we should set
  // the values enabled or disabled based on other states.
  RefreshButton.Enabled := AValue;
  SaveButton.Enabled := AValue;
  EditPrimaryButton.Enabled := AValue;
  EditNotesButton.Enabled := AValue;
  TitleLabel.Enabled := aValue;
  TitleEdit.Enabled := AValue;
  PublishEdit.Enabled := AValue;
  CategoryLabel.Enabled := AValue;
  CategoryEdit.Enabled := AValue;
  StatusLabel.Enabled := AValue;
  StatusEdit.Enabled:=AValue;
  // Synopsis is dependent on something else besides properties.
  // TODO: Need to load synopsis.
  SynopsisLabel.Enabled := false;
  SynopsisEdit.Enabled := false;
  UserPropertiesLabel.Enabled := AValue;
  fUserPropertiesEditor.Enabled := AValue;
end;

procedure TDocumentEditor.RefreshButtonClick(Sender: TObject);
begin
  // TODO: Test this.
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
       MainForm.Project.GetDocumentProperties(Document).Load;
       // this should automatically call something on the main form
       // which will notify that the project has refreshed, and do so.
  end;

end;

procedure TDocumentEditor.EditPrimaryButtonClick(Sender: TObject);
begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    MainForm.Project.EditDocumentPrimary(Document);
  end;
end;

procedure TDocumentEditor.ObserveMainForm(aAction: TMainFormAction;
  aDocument: TDocumentID);
begin
  case aAction of
    mfaDocumentPropertiesLoaded:
      if aDocument = Document then
         ShowDataToUser;
    mfaProjectPropertiesLoaded:
      UpdateLookupLists;
  end;
end;

procedure TDocumentEditor.EditNotesButtonClick(Sender: TObject);
begin
  // TODO: Test this.
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    MainForm.Project.EditDocumentNotes(Document);
  end;
end;

procedure TDocumentEditor.SetDocument(AValue: TDocumentID);
var
  aName: String;
begin
  if Document <> AValue then
  begin
    inherited SetDocument(aValue);
    DocumentIDLabel.Caption := AValue;
    // TODO: Make use of 'title'?
    aName := ExtractDocumentName(AValue);
    if (Parent <> nil) and (Parent is ttabsheet) then
    begin
      Parent.Caption := aName;
    end;
    UpdateLookupLists;
    ShowDataToUser;
  end;
end;

procedure TDocumentEditor.ShowDataToUser;
var
  props: TDocumentProperties;
begin
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    EditingEnabled := true;
    props := MainForm.Project.GetDocumentProperties(Document);
    TitleEdit.Text := props.title;
    PublishEdit.Checked := props.publish;
    CategoryEdit.Text := props.category;
    StatusEdit.Text := props.status;
    fUserPropertiesEditor.SetJSON(props.user);

  end
  else
    EditingEnabled := false;
end;

procedure TDocumentEditor.UpdateLookupLists;
var
  props: TProjectProperties;
  i: Integer;
begin
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    props := MainForm.Project.Properties;
    StatusEdit.Items.Clear;
    for i := 0 to props.statuses.NameCount - 1 do
      StatusEdit.Items.Add(props.statuses.Names[i]);
    CategoryEdit.Items.Clear;
    for i := 0 to props.categories.NameCount - 1 do
      CategoryEdit.Items.Add(props.categories.Names[i]);

  end
  else
    EditingEnabled := false;

end;

constructor TDocumentEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // force the LCL to create a unique name of it's own.
  Name := '';
  fUserPropertiesEditor := TJSONEditor.Create(UserPropertiesPanel);
  fUserPropertiesEditor.Parent := UserPropertiesPanel;
  //fUserPropertiesEditor.Height := 192;
  fUserPropertiesEditor.Align := alClient;
  fUserPropertiesEditor.BorderSpacing.Top := 10;

  MainForm.Observe(@ObserveMainForm);
end;

destructor TDocumentEditor.Destroy;
begin
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

end.

