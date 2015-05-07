unit stewdocumenteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  stewproject, steweditorframe, stewjsoneditor, stewmainform;

type

{
TODO: Need to be able to 'save'.

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
    procedure SetupControls;
  protected
    fUserPropertiesEditor: TJSONEditor;
    procedure SetDocument(AValue: TDocumentID); override;
    procedure ShowDataToUser;
    procedure ShowSynopsisToUser;
    procedure UpdateLookupLists;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs, stewproperties, stewpersist;

{$R *.lfm}

{ TDocumentEditor }

procedure TDocumentEditor.SaveButtonClick(Sender: TObject);
begin
  ShowMessage('Sorry, I can''t save these things yet');

  // TODO: WriteDataFromUser;

end;

procedure TDocumentEditor.SetupControls;
var
  canEditProps: Boolean;
  canEditSynopsis: Boolean;
  canEditAttachments: Boolean;
begin
  canEditProps := (MainForm.Project <> nil) and
                  (MainForm.Project.IsOpened) and
                  // properties depends on valid values from the project properties as well.
                  (MainForm.Project.Properties.FilingState in [fsLoaded]) and
                  (MainForm.Project.GetDocumentProperties(Document).FilingState in [fsLoaded]);
  canEditAttachments := (MainForm.Project <> nil) and
                  (MainForm.Project.IsOpened) and
                  (MainForm.Project.IsDocumentListed(Document));
  // Synopsis is dependent on something else besides properties.
  canEditSynopsis := (MainForm.Project <> nil) and
                  (MainForm.Project.IsOpened) and
                  (MainForm.Project.HasDocumentSynopsis(Document));

  RefreshButton.Enabled := canEditProps;
  SaveButton.Enabled := canEditProps;
  EditPrimaryButton.Enabled := canEditAttachments;
  EditNotesButton.Enabled := canEditAttachments;
  TitleLabel.Enabled := canEditProps;
  TitleEdit.Enabled := canEditProps;
  PublishEdit.Enabled := canEditProps;
  CategoryLabel.Enabled := canEditProps;
  CategoryEdit.Enabled := canEditProps;
  StatusLabel.Enabled := canEditProps;
  StatusEdit.Enabled := canEditProps;
  SynopsisLabel.Enabled := canEditSynopsis;
  SynopsisEdit.Enabled := canEditSynopsis;
  UserPropertiesLabel.Enabled := canEditProps;
  fUserPropertiesEditor.Enabled := canEditProps;

  UpdateLookupLists;
end;

procedure TDocumentEditor.RefreshButtonClick(Sender: TObject);
begin
  // TODO: Test this.
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
       MainForm.Project.GetDocumentProperties(Document).Load;
       MainForm.Project.LoadDocumentSynopsis(Document);
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
    mfaDocumentPropertiesLoaded, mfaDocumentPropertiesLoading, mfaDocumentPropertiesSaved, mfaDocumentPropertiesSaving:
      if aDocument = Document then
         ShowDataToUser;
    mfaDocumentSynopsisLoaded:
      if aDocument = Document then
      begin
         ShowSynopsisToUser;
         SetupControls;
      end;
    mfaProjectPropertiesLoaded, mfaProjectPropertiesLoading, mfaProjectPropertiesSaved, mfaProjectPropertiesSaving:
      SetupControls;
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
    if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
      MainForm.Project.LoadDocumentSynopsis(AValue);
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
    props := MainForm.Project.GetDocumentProperties(Document);
    TitleEdit.Text := props.title;
    PublishEdit.Checked := props.publish;
    CategoryEdit.Text := props.category;
    StatusEdit.Text := props.status;
    fUserPropertiesEditor.SetJSON(props.user);
    ShowSynopsisToUser;

  end;
  SetupControls;

end;

procedure TDocumentEditor.ShowSynopsisToUser;
begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    if MainForm.Project.HasDocumentSynopsis(Document) then
      SynopsisEdit.Lines.Text := MainForm.Project.GetDocumentSynopsis(Document);
  end;
end;

procedure TDocumentEditor.UpdateLookupLists;
var
  props: TProjectProperties;
  i: Integer;
begin
  StatusEdit.Items.Clear;
  CategoryEdit.Items.Clear;
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) and (MainForm.Project.Properties.FilingState in [fsLoaded]) then
  begin
    props := MainForm.Project.Properties;
    for i := 0 to props.statuses.NameCount - 1 do
      StatusEdit.Items.Add(props.statuses.Names[i]);
    for i := 0 to props.categories.NameCount - 1 do
      CategoryEdit.Items.Add(props.categories.Names[i]);

  end

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

