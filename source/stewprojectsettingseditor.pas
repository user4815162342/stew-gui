unit stewprojectsettingseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  Grids, steweditorframe, stewproperties, stewmainform, stewjsoneditor;

// TODO: There's a weird bug on refresh which I should look into, something
// in the extra properties thing.

// TODO: Clean up the todos in here and in the json editor and project properties,
// and move on to documents.

// TODO: More possible properties:
//    - editors for certain file extensions (See Preferences Menu).
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
    ToolButton1: TToolButton;
    procedure ObserveMainForm(aAction: TMainFormAction);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    procedure SetEditingEnabled(AValue: Boolean);
  private
    { private declarations }
    fUserPropertiesEditor: TJSONEditor;
    procedure UpdateDataBindings;
    property EditingEnabled: Boolean write SetEditingEnabled;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs, Graphics;

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
  ShowMessage('Saving project properties isn''t implemented yet.')
  // TODO: Save project properties...
  // TODO: We only really need this until I'm sure that automatic saving and
  // loading is working. Although, perhaps I don't want automatic saving?
end;

procedure TProjectSettingsEditor.SetEditingEnabled(AValue: Boolean);
begin
  // TODO: Make sure all data controls are enabled as appropriate.
  RefreshButton.Enabled := AValue;
  SaveButton.Enabled := AValue;
  DefaultDocExtensionLabel.Enabled := AValue;
  DefaultDocExtensionEdit.Enabled := AValue;
  DefaultNotesExtensionLabel.Enabled := AValue;
  DefaultNotesExtensionEdit.Enabled := AValue;
  DefaultThumbnailExtensionLabel.Enabled := AValue;
  DefaultThumbnailExtensionEdit.Enabled := AValue;
  CategoryDefinitionsGrid.Enabled := AValue;
  CategoryDefinitionsLabel.Enabled := AValue;
  StatusDefinitionsLabel.Enabled:=AValue;
  StatusDefinitionsGrid.Enabled := AValue;
  UserPropertiesLabel.Enabled := AValue;
  fUserPropertiesEditor.Enabled := AValue;
end;

procedure TProjectSettingsEditor.ObserveMainForm(aAction: TMainFormAction);
begin
  // TODO: What else?
  case aAction of
    mfaProjectRefresh:
      UpdateDataBindings;
  end;
end;

function ColorToHex(x: TColor): String;
begin
  result := '#' +
     IntToHex(Red(x),2) +
     IntToHex(Green(x),2) +
     IntToHex(Blue(x),2);
end;

procedure TProjectSettingsEditor.UpdateDataBindings;
var
  props: TProjectProperties;
  i: Integer;
  aName: String;
begin
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    EditingEnabled := true;
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

  end
  else
    EditingEnabled := false;
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

  UpdateDataBindings;
  MainForm.Observe(@ObserveMainForm);
end;

destructor TProjectSettingsEditor.Destroy;
begin
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

end.

