unit stewdocumenteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  stewproject, steweditorframe, stewjsoneditor;

type

{
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
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    { private declarations }
  protected
    fUserPropertiesEditor: TJSONEditor;
    procedure SetDocument(AValue: TDocumentID); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  stewmainform;

{$R *.lfm}

{ TDocumentEditor }

procedure TDocumentEditor.SaveButtonClick(Sender: TObject);
begin
  // TODO:

end;

procedure TDocumentEditor.RefreshButtonClick(Sender: TObject);
begin
  // TODO:

end;

procedure TDocumentEditor.EditPrimaryButtonClick(Sender: TObject);
begin
  // TODO:
end;

procedure TDocumentEditor.EditNotesButtonClick(Sender: TObject);
begin
  // TODO:
end;

procedure TDocumentEditor.SetDocument(AValue: TDocumentID);
var
  aName: String;
begin
  if Document <> AValue then
  begin
    inherited SetDocument(aValue);
    DocumentIDLabel.Caption := AValue;
    aName := MainForm.Project.GetBaseName(AValue);
    if (Parent <> nil) and (Parent is ttabsheet) then
    begin
      Parent.Caption := aName;
    end;
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

end;

end.

