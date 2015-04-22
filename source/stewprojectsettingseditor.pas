unit stewprojectsettingseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  Grids, steweditorframe, stewproperties, stewmainform;
// TODO: Show Project Options. This is both the "stew" file and the project root properties.
//    The possible options include:
//    - category definitions, including publishing details.
//    - status definitions
//    - default file extensions (primary, note, thumbnail)
//    - editors for certain file extensions (See Preferences Menu).
//    - "Type" of project:
//      - Notebook: In this project type, there is only a 'primary', no notes,
//        and no thumbnail, and no synopsis, and as many editors are set to
//        internal as possible. There is also no "publish" and "title" properties,
//        and the publishing stuff which might be needed in the stew file.
//      - Manuscript: In this project type, there are separate notes and things,
//        basically, everything that's always been in stew.
//    - user settings (the stew-cli stuff allows user settings both in stew and in the project root)
//    - Root Index (if allowed for editing in the documents, then it should be here too).
//    - Notes

type

  { TProjectSettingsEditor }

  TProjectSettingsEditor = class(TEditorFrame)
    DefaultDocExtensionEdit: TEdit;
    DefaultThumbnailExtensionEdit: TEdit;
    DefaultNotesExtensionEdit: TEdit;
    DefaultDocExtensionLabel: TLabel;
    DefaultThumbnailExtensionLabel: TLabel;
    DefaultNotesExtensionLabel: TLabel;
    DefaultDocExtensionPanel: TPanel;
    DefaultThumbnailExtensionPanel: TPanel;
    DefaultNotesExtensionPanel: TPanel;
    CategoryDefinitionsPanel: TPanel;
    CategoryDefinitionsHeaderPanel: TPanel;
    CategoryDefinitionsLabel: TLabel;
    RefreshButton: TToolButton;
    SaveButton: TToolButton;
    CategoryDefinitionsGrid: TStringGrid;
    ToolButton1: TToolButton;
    procedure ObserveMainForm(aAction: TMainFormAction);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    procedure SetEditingEnabled(AValue: Boolean);
  private
    { private declarations }
    procedure UpdateDataBindings;
    property EditingEnabled: Boolean write SetEditingEnabled;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs;

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
end;

procedure TProjectSettingsEditor.ObserveMainForm(aAction: TMainFormAction);
begin
  // TODO: What else?
  case aAction of
    mfaProjectRefresh:
      UpdateDataBindings;
  end;
end;

procedure TProjectSettingsEditor.UpdateDataBindings;
var
  props: TProjectProperties;
begin
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    EditingEnabled := true;
    props := MainForm.Project.Properties;
    DefaultDocExtensionEdit.Text := props.defaultDocExtension;
    DefaultNotesExtensionEdit.Text := props.defaultNotesExtension;
    DefaultThumbnailExtensionEdit.Text := props.defaultThumbnailExtension;
    // TODO: Assign data in properties to the form controls.

  end
  else
    EditingEnabled := false;
end;

constructor TProjectSettingsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := 'Project Settings';
  UpdateDataBindings;
  MainForm.Observe(@ObserveMainForm);
end;

destructor TProjectSettingsEditor.Destroy;
begin
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

end.

