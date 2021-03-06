unit gui_documenteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  stew_project, gui_editorframe, gui_jsoneditor, stew_mainform, stew_properties, sys_async;

type

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
    procedure CategoryEditChange(Sender: TObject);
    procedure EditNotesButtonClick(Sender: TObject);
    procedure EditPrimaryButtonClick(Sender: TObject);
    procedure PublishEditClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure StatusEditChange(Sender: TObject);
  strict private
    fUserPropertiesEditor: TJSONEditor;
    fUIUpdateCount: Integer;
    fPropsAvailable: Boolean;
    fSynopsisAvailable: Boolean;
    fWriting: Boolean;
    fPublishModified: Boolean;
    fCategoryModified: Boolean;
    fStatusModified: Boolean;
    procedure ClearData;
    procedure ClearModified;
    function IsModified: Boolean;
    procedure ProjectPropertiesUpdated(aData: IProjectProperties);
    procedure DocumentRenamed(aOldDocument: TDocumentPath; aNewDocument: TDocumentPath);
    procedure PropertiesUpdated(aData: IDocumentProperties);
    procedure AttachmentsListed(aData: TAttachmentArray);
    procedure SynopsisUpdated(aData: UTF8String);
    procedure WriteData;
    procedure WriteData_Done;
    procedure BeginUIUpdate;
    procedure EndUIUpdate;
    procedure SetupControls;
    procedure ObserveMainForm(Sender: TMainForm; aAction: TMainFormAction;
      {%H-}aDocument: TDocumentPath);
    procedure ObserveProject(Sender: TStewProject; Event: TProjectEvent);
    procedure WriteData_PropertiesRead(Sender: TPromise);
    procedure WriteData_SynopsisWritten(Sender: TPromise);
    procedure WriteData_PropertiesWritten(Sender: TPromise);
  strict protected
    procedure SetDocument(AValue: TDocumentPath); override;
    function SetupGlyphs: Boolean; override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function CloseQuery: Boolean; override;
  end;

implementation

uses
  Dialogs, sys_types, sys_log, gui_glyphs, sys_dynval, sys_dynval_json;

{$R *.lfm}

{ TDocumentEditor }

procedure TDocumentEditor.SaveButtonClick(Sender: TObject);
const
  cMethod: String = 'TDocumentEditor.SaveButtonClick';
begin
  LogAction(cMethod,Document.ID);
  //MainForm.ShowMessage('The old version will be backed up in case this doesn''t work.',mtConfirmation,'Got it');
  WriteData;

end;

procedure TDocumentEditor.StatusEditChange(Sender: TObject);
begin
  fStatusModified := true;
end;

procedure TDocumentEditor.WriteData_PropertiesRead(Sender: TPromise);
var
  lProps: IDocumentProperties;
begin
  lProps := (Sender as TDocumentPropertiesPromise).Properties.Clone as IDocumentProperties;

  lProps.Title := TitleEdit.Text;
  lProps.Publish := PublishEdit.Checked;
  lProps.Category := CategoryEdit.Text;
  lProps.Status := StatusEdit.Text;

  // I need to create and destroy this object because
  // it gets cloned when setting the property.
  lProps.User := fUserPropertiesEditor.GetMap;

  if MainForm.Project <> nil then
  begin
     MainForm.MessageDialog('The old file will be backed up first.',TMsgDlgType.mtInformation,'Got it');
     MainForm.Project.WriteDocumentProperties(Document,lProps,true).After(@WriteData_PropertiesWritten)
  end
  else
  begin
    // This is really an error message...
    MainForm.MessageDialog('Properties can''t be saved, the project has closed.',mtError,'Sigh');
    ClearData;
  end;

end;

procedure TDocumentEditor.WriteData_SynopsisWritten(Sender: TPromise);
begin
  WriteData_Done;
end;

procedure TDocumentEditor.WriteData_PropertiesWritten(Sender: TPromise);
begin
  if SynopsisEdit.Modified then
  begin
     if MainForm.Project <> nil then
     begin
        MainForm.Project.WriteDocumentSynopsis(Document,SynopsisEdit.Lines.Text,false).After(@WriteData_SynopsisWritten)
     end
     else
     begin
       // This is really an error message...
       MainForm.MessageDialog('Synopsis can''t be saved, the project has closed.',mtError,'Sigh');
       ClearData;
     end;
  end
  else
  begin
    WriteData_Done;
  end;

end;

procedure TDocumentEditor.SetupControls;
var
  canEditProps: Boolean;
  canEditSynopsis: Boolean;
  canEditAttachments: Boolean;
  aTabCaption: String;
begin
  canEditProps := (MainForm.Project <> nil) and
                  fPropsAvailable and
                  (not fWriting) and
                  (fUIUpdateCount = 0);
  canEditSynopsis := (MainForm.Project <> nil) and
                     fSynopsisAvailable and
                     (not fWriting) and
                     (fUIUpdateCount = 0);
  // can't think of any reason you shouldn't be able to edit if everything else is available...
  canEditAttachments := (MainForm.Project <> nil) and
                        fPropsAvailable and
                        fSynopsisAvailable and
                        (not fWriting) and
                        (fUIUpdateCount = 0);

  if (Parent <> nil) and (Parent is TTabSheet) then
  begin
    if canEditProps then
    begin
      aTabCaption := TitleEdit.Text;
      if aTabCaption = '' then
         aTabCaption := Document.Name;
      Parent.Caption := aTabCaption;
    end
    else
    begin
      Parent.Caption:=Document.Name;
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

end;

procedure TDocumentEditor.RefreshButtonClick(Sender: TObject);
const
  cMethod: String = 'TDocumentEditor.RefreshButtonClick';
begin
  LogAction(cMethod,Document.ID);
  ClearModified;
  if MainForm.Project <> nil then
  begin
    MainForm.Project.ReadDocumentProperties(Document,true);
    MainForm.Project.ReadDocumentSynopsis(Document,true);
    MainForm.Project.ListAttachmentsForDocument(Document,true);
  end;
end;

procedure TDocumentEditor.EditPrimaryButtonClick(Sender: TObject);
const
  cMethod: String = 'TDocumentEditor.EditPrimaryButtonClick';
begin
  LogAction(cMethod,Document.ID);
  if MainForm.Project <> nil then
    MainForm.Project.EditDocument(Document);
end;

procedure TDocumentEditor.PublishEditClick(Sender: TObject);
begin
  fPublishModified := true;
end;


procedure TDocumentEditor.ObserveMainForm(Sender: TMainForm;
  aAction: TMainFormAction; aDocument: TDocumentPath);
begin
  case aAction of
    mfaProjectOpened:
    begin
      MainForm.Project.AddObserver(@ObserveProject);
      MainForm.Project.ReadProjectProperties;
    end;
    mfaProjectClosed:
    begin
      MainForm.Project.RemoveObserver(@ObserveProject);
      ClearData;
    end;
  end;

end;

procedure TDocumentEditor.ObserveProject(Sender: TStewProject;
  Event: TProjectEvent);
begin
  case Event.Action of
    paLoadingProjectPropertiesFile,
    paSavingProjectPropertiesFile:
       BeginUIUpdate;
    paListingDocumentFiles:
      BeginUIUpdate;
    paDocumentFilesListed,
    paDocumentsFileListingFailed:
      EndUIUpdate;
    paProjectPropertiesFileLoaded,
    paProjectPropertiesFileLoadingFailed,
    paProjectPropertiesFileSaved,
    paProjectPropertiesFileSavingFailed:
       EndUIUpdate;
    paLoadingAttachmentFile,
    paSavingAttachmentFile:
       if (Event as TAttachmentEvent).Document = Document then
          BeginUIUpdate;
    paAttachmentFileLoaded,
    paAttachmentFileLoadingFailed,
    paAttachmentFileSaved,
    paAttachmentFileSavingFailed:
       if (Event as TAttachmentEvent).Document = Document then
          EndUIUpdate;
    paProjectPropertiesDataReceived:
    // Need to update the combo boxes for statuses and categories.
      ProjectPropertiesUpdated((Event as TProjectPropertiesDataReceivedEvent).Properties);
    paAttachmentListDataReceived:
      if (Event as TAttachmentListDataReceivedEvent).Document = Document then
      begin
        AttachmentsListed((Event as TAttachmentListDataReceivedEvent).List);
      end;
    paShadowCreated,
    paShadowUncreated:
    // do nothing. But, someday there might be something we want to change in the UI here.
      ;
    paRenamingDocument:
      if (Event as TDocumentRenameEvent).Document = Document then
         BeginUIUpdate;
    paDocumentRenamingFailed:
       if (Event as TDocumentRenameEvent).Document = Document then
          EndUIUpdate;
    paDocumentRenamed:
       if (Event as TDocumentRenameEvent).Document = Document then
       begin
          EndUIUpdate;
          DocumentRenamed(Document,(Event as TDocumentRenameEvent).NewDocument);
       end;
    paAttachmentDataReceived:
       if (Event as TAttachmentEvent).Document = Document then
       begin
         case (Event as TAttachmentEvent).Attachment of
           atProperties:
             PropertiesUpdated((Event as TDocumentPropertiesDataReceivedEvent).Properties);
           atSynopsis:
             SynopsisUpdated((Event as TDocumentSynopsisDataReceived).Synopsis);
         end;
       end;
  end;
end;

procedure TDocumentEditor.EditNotesButtonClick(Sender: TObject);
const
  cMethod: String = 'TDocumentEditor.EditNotesButtonClick';
begin
  LogAction(cMethod,Document.ID);
  if MainForm.Project <> nil then
     MainForm.Project.EditDocumentNotes(Document);
end;

procedure TDocumentEditor.CategoryEditChange(Sender: TObject);
begin
  fCategoryModified := true;

end;

procedure TDocumentEditor.SetDocument(AValue: TDocumentPath);
begin
  // FUTURE: Should really consider what to do if we're assigning a null
  // document.
  if (Document <> AValue) then
  begin
    // I don't want to just *clear* the data. This might be changed
    // due to a rename, and I don't want to lose changes.
    inherited SetDocument(aValue);
    DocumentIDLabel.Caption := AValue.ID;

    // refresh what we've got...
    if MainForm.Project <> nil then
    begin
      MainForm.Project.ReadDocumentProperties(AValue);
      MainForm.Project.ReadDocumentSynopsis(AValue);
      MainForm.Project.ListAttachmentsForDocument(AValue);
    end;

    SetupControls;

  end;
end;

function TDocumentEditor.SetupGlyphs: Boolean;
begin
  Result:=inherited SetupGlyphs;
  if Result then
  begin
    SaveButton.ImageIndex := ord(sbgSave);
    RefreshButton.ImageIndex := ord(sbgRefresh);
    EditPrimaryButton.ImageIndex := ord(sbgEditPrimary);
    EditNotesButton.ImageIndex := ord(sbgEditNotes);

  end;
end;

procedure TDocumentEditor.ClearData;
begin
  fPropsAvailable := false;
  fSynopsisAvailable := false;
  TitleEdit.Text := '';
  PublishEdit.Checked := false;
  CategoryEdit.Text := '';
  StatusEdit.Text := '';
  SynopsisEdit.Lines.Clear;
  fUserPropertiesEditor.SetMap(nil);
  ClearModified;
  SetupControls;
end;

procedure TDocumentEditor.ClearModified;
begin
  TitleEdit.Modified := false;
  fPublishModified := false;
  fCategoryModified := false;
  fStatusModified := false;
  SynopsisEdit.Modified:=false;
  fUserPropertiesEditor.Modified := false;

end;

function TDocumentEditor.IsModified: Boolean;
begin
  result := TitleEdit.Modified or
            fPublishModified or
            fCategoryModified or
            fStatusModified or
            SynopsisEdit.Modified or
            fUserPropertiesEditor.Modified;
end;

procedure TDocumentEditor.ProjectPropertiesUpdated(aData: IProjectProperties);
var
  i: Integer;
  lKeys: TStringArray2;
begin

  StatusEdit.Items.Clear;
  CategoryEdit.Items.Clear;
  lKeys := aData.Statuses.keys;
  for i := 0 to lKeys.Count - 1 do
    StatusEdit.Items.Add(lKeys[i]);
  lKeys := aData.Categories.keys;
  for i := 0 to lKeys.Count - 1 do
    CategoryEdit.Items.Add(lKeys[i]);
end;

procedure TDocumentEditor.DocumentRenamed(aOldDocument: TDocumentPath;
  aNewDocument: TDocumentPath);
begin
  // should be able to just set the document to the new one, right?
  if aOldDocument = Document then
     Document := aNewDocument;
end;

procedure TDocumentEditor.PropertiesUpdated(aData: IDocumentProperties);
begin
  fPropsAvailable := true;
  if not TitleEdit.Modified then
  begin
    TitleEdit.Text := aData.Title;
    TitleEdit.Modified := false;
  end;
  if not fPublishModified then
  begin
    PublishEdit.Checked := aData.Publish;
    fPublishModified :=  false;
  end;
  if not fCategoryModified then
  begin
    CategoryEdit.Text := aData.Category;
    fCategoryModified := false;
  end;
  if not fStatusModified then
  begin
    StatusEdit.Text := aData.Status;
    fStatusModified := false;
  end;
  if not fUserPropertiesEditor.Modified then
  begin
    fUserPropertiesEditor.SetMap(aData.User);
    fUserPropertiesEditor.Modified := false;
  end;
  SetupControls;
end;

procedure TDocumentEditor.AttachmentsListed(aData: TAttachmentArray);
var
  i: Longint;
  l: Longint;
  lFoundPrimary: Boolean;
  lFoundNotes: Boolean;
begin
  // TODO: Also come up with a combobutton to edit the remaining attachments.
  // TODO: In fact, if there is more than one primary, or notes, the notes
  // button should be a dropdown button.
  // TODO: Eventually, the result will change a button image, not the caption.
  lFoundNotes := false;
  lFoundPrimary := false;
  l := Length(aData) - 1;
  for i := 0 to l do
  begin
    if (not lFoundPrimary) and (aData[i].Kind = atPrimary) then
    begin
       lFoundPrimary := true;
    end;
    if (not lFoundNotes) and (aData[i].Kind = atNotes) then
    begin
       lFoundNotes := true;
    end;
  end;
  if lFoundPrimary then
  begin
    EditPrimaryButton.ImageIndex := ord(sbgEditPrimary);
  end
  else
  begin
    EditPrimaryButton.ImageIndex := ord(sbgCreatePrimary);
  end;
  if lFoundNotes then
  begin
    EditNotesButton.ImageIndex := ord(sbgEditNotes);
  end
  else
  begin
    EditNotesButton.ImageIndex := ord(sbgCreateNotes);
  end;
end;

procedure TDocumentEditor.SynopsisUpdated(aData: UTF8String);
begin
  fSynopsisAvailable := true;
  if not SynopsisEdit.Modified then
  begin
     SynopsisEdit.Lines.Text := aData;
     SynopsisEdit.Modified := false;
  end;
  SetupControls;
end;

procedure TDocumentEditor.WriteData;
begin
  if MainForm.Project <> nil then
  begin
    fWriting := true;
    // retrieve the data first, then write it, so we have the most up-to-date
    // stuff, including any "unknowns".
    MainForm.Project.ReadDocumentProperties(Document).After(@WriteData_PropertiesRead);
    SetupControls;
  end;
end;

procedure TDocumentEditor.WriteData_Done;
begin
  ClearModified;
  fWriting := False;
  if MainForm.Project <> nil then
  begin
    MainForm.Project.ReadDocumentProperties(Document);
    MainForm.Project.ReadDocumentSynopsis(Document);
    MainForm.Project.ListAttachmentsForDocument(Document);
  end;
  SetupControls;

end;

procedure TDocumentEditor.BeginUIUpdate;
begin
  inc(fUIUpdateCount);
  if fUIUpdateCount = 1 then
  begin
    SetupControls;
  end;
end;

procedure TDocumentEditor.EndUIUpdate;
begin
  dec(fUIUpdateCount);
  if fUIUpdateCount = 0 then
  begin
    SetupControls;
  end;
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
  // if the project is already opened, then notify self to make sure
  // we hook up to it.
  if MainForm.Project <> nil then
     ObserveMainForm(MainForm,mfaProjectOpened,TDocumentPath.Null);
end;

destructor TDocumentEditor.Destroy;
begin
  if MainForm.Project <> nil then
     MainForm.Project.RemoveObserver(@ObserveProject);
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

function TDocumentEditor.CloseQuery: Boolean;
begin
  if IsModified then
  begin
    SetFocus;
    {
    NOTE: See gui_projectsettingseditor for notes on convention breakage here.
    }

    result := MainForm.ButtonDialog('You are about to lose your changes to "' + Document.Name + '".' + LineEnding +
               'Are you sure you want to close it?',mtWarning,mbYesNo,['Close Without Saving','Don''t Close']) = mrYes;
  end
  else
    result := true;
end;

end.

