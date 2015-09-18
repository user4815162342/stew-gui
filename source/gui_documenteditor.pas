unit gui_documenteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  stew_project, gui_editorframe, gui_jsoneditor, gui_mainform;

type

{
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
    procedure ObserveMainForm(aAction: TMainFormAction; aDocument: TDocumentPath);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    { private declarations }
  private
    procedure SetupControls;
  protected
    fUserPropertiesEditor: TJSONEditor;
    procedure SetDocument(AValue: TDocumentPath); override;
    procedure ShowPropertiesToUser;
    procedure WriteDataFromUser;
    procedure ShowSynopsisToUser;
    procedure UpdateLookupLists;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function CloseQuery: Boolean; override;
  end;

implementation

uses
  Dialogs, stew_properties, sys_types, fpjson;

{$R *.lfm}

{ TDocumentEditor }

procedure TDocumentEditor.SaveButtonClick(Sender: TObject);
begin
  ShowMessage('The old version will be backed up in case this doesn''t work.');
  WriteDataFromUser;

end;

procedure TDocumentEditor.SetupControls;
var
  canEditProps: Boolean;
  canEditSynopsis: Boolean;
  canEditAttachments: Boolean;
  aData: TDocumentMetadata;
  aTabCaption: String;
begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
    aData := MainForm.Project.GetDocument(Document)
  else
    aData := nil;
  canEditProps := (aData <> nil) and
                  // properties depends on valid values from the project properties as well.
                  (MainForm.Project.Properties.FilingState in [fsLoaded]) and
                  (aData.Properties.FilingState in [fsLoaded]);
  canEditAttachments := (MainForm.Project <> nil) and
                  (MainForm.Project.IsOpened) and
                  (aData.AreAttachmentsListed);
  // Synopsis is dependent on something else besides properties.
  canEditSynopsis := canEditAttachments and
                  (aData.Synopsis.FilingState in [fsLoaded]);

  if canEditProps then
  begin
    if (Parent <> nil) and (Parent is TTabSheet) then
    begin
      aTabCaption := aData.Properties.title;
      if aTabCaption = '' then
         aTabCaption := Document.Name;
      Parent.Caption := aTabCaption;
    end;
  end;

  RefreshButton.Enabled := canEditProps;
  SaveButton.Enabled := canEditProps and canEditSynopsis;
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
var
  aData: TDocumentMetadata;
begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    aData := MainForm.Project.GetDocument(Document);
    aData.Properties.Load;
    aData.Synopsis.Load;
       // this should automatically call something on the main form
       // which will notify that the project has refreshed, and do so.
  end;

end;

procedure TDocumentEditor.EditPrimaryButtonClick(Sender: TObject);
begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    MainForm.Project.GetDocument(Document).Primary.OpenInEditor;
  end;
end;

procedure TDocumentEditor.ObserveMainForm(aAction: TMainFormAction;
  aDocument: TDocumentPath);
begin
  case aAction of
    mfaDocumentPropertiesLoaded, mfaDocumentPropertiesSaved:
      if aDocument = Document then
      begin
         ShowPropertiesToUser;
         SetupControls;
      end;
    mfaDocumentPropertiesLoading, mfaDocumentPropertiesSaving, mfaDocumentChanged:
      if aDocument = Document then
         SetupControls;
    mfaDocumentSynopsisLoaded, mfaDocumentSynopsisSaved:
      if aDocument = Document then
      begin
         ShowSynopsisToUser;
         SetupControls;
      end;
    mfaDocumentSynopsisLoading, mfaDocumentSynopsisSaving:
      if aDocument = Document then
        SetupControls;
    mfaDocumentSynopsisSaveConflicted:
      if aDocument = Document then
      begin
        if MessageDlg('The synopsis has changed on the disk since the last time it was loaded.' + LineEnding +
                  'Would you like to overwrite it''s contents?',mtWarning,mbYesNo,0) = mrYes then
        begin
          MainForm.Project.GetDocument(Document).Synopsis.Save(true);
        end;
      end;
    mfaProjectPropertiesLoaded, mfaProjectPropertiesLoading, mfaProjectPropertiesSaved, mfaProjectPropertiesSaving:
      SetupControls;
  end;
end;

procedure TDocumentEditor.EditNotesButtonClick(Sender: TObject);
begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    MainForm.Project.GetDocument(Document).Notes.OpenInEditor;
  end;
end;

procedure TDocumentEditor.SetDocument(AValue: TDocumentPath);
var
  aName: String;
begin
  // FUTURE: Should really consider what to do if we're assigning a null
  // document.
  if (Document <> AValue) then
  begin
    if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
    begin
      with MainForm.Project.GetDocument(AValue) do
      begin
        if IsLocked then
        begin
           ShowMessage('I can''t open that document. I''m busy doing something with it right now.');
           exit;
        end;

        Lock(Self);

        inherited SetDocument(aValue);
        DocumentIDLabel.Caption := AValue.ID;

        aName := GetName;
        if (Parent <> nil) and (Parent is TTabSheet) then
        begin
          Parent.Caption := aName;
        end;

        Properties.Load;
        Synopsis.Load;
      end;
    end;


  end;
end;

procedure TDocumentEditor.ShowPropertiesToUser;
var
  props: TDocumentProperties;
begin
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    props := MainForm.Project.GetDocument(Document).Properties;
    TitleEdit.Text := props.title;
    PublishEdit.Checked := props.publish;
    CategoryEdit.Text := props.category;
    StatusEdit.Text := props.status;
    fUserPropertiesEditor.SetJSON(props.user);

  end;

end;

procedure TDocumentEditor.WriteDataFromUser;
var
  doc: TDocumentMetadata;
  props: TDocumentProperties;
  aUser: TJSONData;
begin
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    doc := MainForm.Project.GetDocument(Document);
    props := doc.Properties;
    props.title := TitleEdit.Text;
    props.publish := PublishEdit.Checked;
    props.category := CategoryEdit.Text;
    props.status := StatusEdit.Text;

    // I need to create and destroy this object because
    // it gets cloned when setting the property.
    aUser := fUserPropertiesEditor.CreateJSON;
    try
      props.user := aUser;
    finally
      aUser.Free;
    end;

    props.Save;
    with doc.Synopsis do
    begin
      Contents := SynopsisEdit.Lines.Text;
      Save;
    end;


  end

end;

procedure TDocumentEditor.ShowSynopsisToUser;
var
  aMeta: TDocumentMetadata;
begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    aMeta := MainForm.Project.GetDocument(Document);
    if aMeta.Synopsis.FilingState in [fsLoaded] then
      SynopsisEdit.Lines.Text := aMeta.Synopsis.Contents;
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

function TDocumentEditor.CloseQuery: Boolean;
begin
  Result:=inherited CloseQuery;
  if Result and
     (MainForm.Project <> nil) and
     (MainForm.Project.IsOpened) and
     (not Document.IsNull) then
      MainForm.Project.GetDocument(Document).Unlock(Self);
end;

end.

