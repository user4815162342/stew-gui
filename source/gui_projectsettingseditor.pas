unit gui_projectsettingseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  Grids, Dialogs, gui_editorframe, stew_properties, gui_mainform,
  gui_jsoneditor, stew_project, graphics, Menus, ExtDlgs, sys_async;

{
TODO: Deadline Grid:
- After *that's* done, I need to know how many "working" days (two kinds,
  full days and light days), which means I need to track what days are working
  days in the application preferences (finally, something for that screen).
}

// FUTURE: More properties that need to be handled.
//    - editors for certain file extensions (Also put this in Application Preferences).
//    - defaultPublishExtension
//    - "Type" of project:
//      - Notebook: In this project type, there is only a 'primary', no notes,
//        and no thumbnail, and no synopsis, and as many editors are set to
//        internal as possible. There is also no "publish" and "title" properties,
//        and the publishing stuff which might be needed in the stew file.
//      - Manuscript: In this project type, there are separate notes and things,
//        basically, everything that's always been in stew.

type

  { TProjectSettingsEditor }

  TProjectSettingsEditor = class(TEditorFrame)
    AddDeadlineButton: TToolButton;
    AddStatusButton: TToolButton;
    CalendarDialog: TCalendarDialog;
    CategoryDefinitionsGrid: TStringGrid;
    DeadlinesGrid: TStringGrid;
    DeadlinesToolbar: TToolBar;
    DeadlinesHeaderPanel: TPanel;
    DeadlinesLabel: TLabel;
    CategoryDefinitionsToolbar: TToolBar;
    AddCategoryButton: TToolButton;
    CategoryColorChooser: TColorDialog;
    ColorMenu: TPopupMenu;
    CustomColorMenu: TMenuItem;
    DefaultDocExtensionEdit: TEdit;
    DefaultDocExtensionLabel: TLabel;
    DefaultDocExtensionPanel: TPanel;
    DefaultExtensionsPanel: TPanel;
    DefaultNotesExtensionEdit: TEdit;
    DefaultNotesExtensionLabel: TLabel;
    DefaultNotesExtensionPanel: TPanel;
    DefaultThumbnailExtensionEdit: TEdit;
    DefaultThumbnailExtensionLabel: TLabel;
    DefaultThumbnailExtensionPanel: TPanel;
    DeadlinesPanel: TPanel;
    DeleteDeadlineButton: TToolButton;
    PropertiesPanel: TPanel;
    StatusDefinitionsToolbar: TToolBar;
    DeleteCategoryButton: TToolButton;
    DeleteStatusButton: TToolButton;
    UserPropertiesLabel: TLabel;
    UserPropertiesPanel: TPanel;
    StatusDefinitionsGrid: TStringGrid;
    CategoryDefinitionsHeaderPanel: TPanel;
    StatusDefinitionsHeaderPanel: TPanel;
    CategoryDefinitionsLabel: TLabel;
    StatusDefinitionsLabel: TLabel;
    StatusDefinitionsPanel: TPanel;
    GridsPanel: TPanel;
    CategoryDefinitionsPanel: TPanel;
    RefreshButton: TToolButton;
    SaveButton: TToolButton;
    EditNotesButton: TToolButton;
    procedure AddCategoryButtonClick(Sender: TObject);
    procedure AddDeadlineButtonClick(Sender: TObject);
    procedure AddStatusButtonClick(Sender: TObject);
    procedure CategoryDefinitionsGridButtonClick(Sender: TObject; aCol,
      aRow: Integer);
    procedure CategoryDefinitionsGridCheckboxToggled(sender: TObject; aCol,
      aRow: Integer; aState: TCheckboxState);
    procedure CategoryDefinitionsGridDrawCell(Sender: TObject; aCol,
      aRow: Integer; aRect: TRect; {%H-}aState: TGridDrawState);
    procedure CategoryDefinitionsGridValidateEntry(sender: TObject; aCol,
      {%H-}aRow: Integer; const OldValue: string; var NewValue: String);
    procedure CustomColorMenuClick(Sender: TObject);
    procedure ColorMenuClick(Sender: TObject);
    procedure DeleteCategoryButtonClick(Sender: TObject);
    procedure DeleteDeadlineButtonClick(Sender: TObject);
    procedure DeleteStatusButtonClick(Sender: TObject);
    procedure EditNotesButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  strict private
    { private declarations }
    fUserPropertiesEditor: TJSONEditor;
    fUIUpdateCount: Integer;
    fDataAvailable: Boolean;
    fWriting: Boolean;
    procedure ClearData;
    procedure ClearModified;
    function IsModified: Boolean;
    procedure ShowData(aData: IProjectProperties);
    procedure AttachmentsListed(aData: TAttachmentArray);
    procedure WriteData;
    procedure BeginUIUpdate;
    procedure EndUIUpdate;
    procedure SetupControls;
    procedure SetupColorMenu;
    procedure WriteData_Read(Sender: TPromise);
    procedure WriteData_Written(Sender: TPromise);
    procedure ObserveMainForm(Sender: TMainForm; aAction: TMainFormAction;
      {%H-}aDocument: TDocumentPath);
    procedure ObserveProject(Sender: TStewProject; Event: TProjectEvent);
  strict protected
    function SetupGlyphs: Boolean; override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function CloseQuery: Boolean; override;
  end;

  function StrToColor(const aValue: String): TColor;
  function ColorToStr(const aValue: TColor): String;




implementation

uses
  sys_types, sys_json,
  sys_dynval, sys_dynval_json,
  clocale, { clocale is required in order to pull in the system locale settings, i.e. for displaying dates properly }
  sys_log,
  gui_glyphs;

type
  TDefColumnKind = (ckString, ckInteger, ckBoolean, ckRadio, ckColor, ckDate);

const
  CatNameCol: Integer = 0;
  CatColorCol: Integer = 1;
  CatDefaultCol: Integer = 2;
  CatPTitleCol: Integer = 3;
  CatPTitleLevelCol: Integer = 4;
  CatPTitlePrefixCol: Integer = 5;
  CatPMarkerBeforeCol: Integer = 6;
  CatPMarkerAfterCol: Integer = 7;
  CatPMarkerBetweenCol: Integer = 8;

  StatNameCol: Integer = 0;
  StatColorCol: Integer = 1;
  StatDefaultCol: Integer = 2;

  DeadlineNameCol: Integer = 0;
  DeadlineDueCol: Integer = 1;

  TrueValue: String = 'Yes';
  FalseValue: String = 'No';

function StrToColor(const aValue: String): TColor;
var
  x: Integer;
begin
  if TryStrToInt('$' + aValue,x) then
     result := TColor(x)
  else
    result := clDefault;
end;

function ColorToStr(const aValue: TColor): String;
begin
  result := IntToHex(aValue,8);

end;

{$R *.lfm}

{ TProjectSettingsEditor }

procedure TProjectSettingsEditor.RefreshButtonClick(Sender: TObject);
const
  cMethod: String = 'TProjectSettingsEditor.RefreshButtonClick';
begin
  LogAction(cMethod,Document.ID);
  if MainForm.Project <> nil then
  begin
    ClearModified;
    MainForm.Project.ReadProjectProperties(true);
    MainForm.Project.ListAttachmentsForDocument(TDocumentPath.Root,true);

  end;
end;

procedure TProjectSettingsEditor.SaveButtonClick(Sender: TObject);
const
  cMethod: String = 'TProjectSettingsEditor.SaveButtonClick';
begin
  LogAction(cMethod,Document.ID);
  //MainForm.ShowMessage('The old version will be backed up in case this doesn''t work.',mtConfirmation,'Got it');
  WriteData;
end;

procedure TProjectSettingsEditor.WriteData_Read(Sender: TPromise);
var
  lProps: IProjectProperties;
  lUser: IDynamicMap;
  i: Integer;
  j: Integer;
  lFound: Boolean;
  lLevel: Integer;
  lNames: TStringArray2;
  lCat: ICategoryDefinition;
  lStat: IStatusDefinition;
begin
  lProps := (Sender as TProjectPropertiesPromise).Properties.Clone as IProjectProperties;
  lProps.defaultDocExtension := DefaultDocExtensionEdit.Text;
  lProps.defaultNotesExtension:= DefaultNotesExtensionEdit.Text;
  lProps.defaultThumbnailExtension:=DefaultThumbnailExtensionEdit.Text;


  with CategoryDefinitionsGrid do
  begin
    // changes and new items
    // we're skipping the first row, which is the header row.
    for i := 1 to RowCount - 1 do
    begin
      lCat := lProps.categories.GetCategory(Cells[CatNameCol,i]);
      if lCat = nil then
      begin
        lCat := TPropertyObjects.NewCategoryDefinition;
        lProps.Categories.SetCategory(Cells[CatNameCol,i],lCat);
      end;
      lCat.color := StrToColor(Cells[CatColorCol,i]);
      lCat.publishTitle:=Cells[CatPTitleCol,i] = TrueValue;
      if TryStrToInt(Cells[CatPTitleLevelCol,i],lLevel) then
         lCat.publishTitleLevel := lLevel;
      lCat.publishTitlePrefix := Cells[CatPTitlePrefixCol,i];
      lCat.publishMarkerAfter:=Cells[CatPMarkerAfterCol,i] = TrueValue;
      lCat.publishMarkerBefore:=Cells[CatPMarkerBeforeCol,i] = TrueValue;
      lCat.publishMarkerBetween:=Cells[CatPMarkerBetweenCol,i] = TrueValue;

      if Cells[CatDefaultCol,i] = TrueValue then
        lProps.defaultCategory:=Cells[CatNameCol,i];
    end;

    // deletions
    lNames := lProps.Categories.keys;

    for i := 0 to lNames.Count - 1 do
    begin
      lFound := false;
      for j := 0 to RowCount - 1 do
      begin
        if Cells[CatNameCol,j] = lNames[i] then
        begin
          lFound := true;
          break;
        end;
      end;
      if not lFound then
         lProps.categories.Delete(lNames[i]);
    end;

  end;

  with StatusDefinitionsGrid do
  begin
    // changes and new items
    // we're skipping the first row, which is the header row.
    for i := 1 to RowCount - 1 do
    begin
      lStat := lProps.statuses.GetStatus(Cells[StatNameCol,i]);
      if lStat = nil then
      begin
        lStat := TPropertyObjects.NewStatusDefinition;
        lProps.Statuses.SetStatus(Cells[StatNameCol,i],lStat);
      end;
      lStat.color := StrToColor(Cells[StatColorCol,i]);
      if Cells[StatDefaultCol,i] = TrueValue then
        lProps.defaultStatus:=Cells[StatNameCol,i];
    end;

    // deletions
    lNames := lProps.statuses.keys;

    for i := 0 to lNames.Count - 1 do
    begin
      lFound := false;
      for j := 0 to RowCount - 1 do
      begin
        if Cells[StatNameCol,j] = lNames[i] then
        begin
          lFound := true;
          break;
        end;
      end;
      if not lFound then
         lProps.statuses.Delete(lNames[i]);
    end;

  end;

  with DeadlinesGrid do
  begin
    // changes and new items
    // we're skipping the first row, which is the header row.
    lProps.Deadlines.Length := 0;
    for i := 1 to RowCount - 1 do
    begin
      lProps.Deadlines.Add(Cells[DeadlineNameCol,i],StrToDate(Cells[DeadlineDueCol,i]));
    end;
  end;



  // I need to create and destroy this object because
  // it gets cloned when setting the property.
  lUser := fUserPropertiesEditor.GetMap;
  if lUser <> nil then
    lProps.User := lUser
  else
    lProps.User.Clear;

  if MainForm.Project <> nil then
  begin
    // TODO: Get rid of the backup once we're sure...
    MainForm.ShowMessage('The old file will be backed up first.',TMsgDlgType.mtInformation,'Got it');
    MainForm.Project.WriteProjectProperties(lProps,true).After(@WriteData_Written);
  end
  else
  begin
    // This is really an error message...
    MainForm.ShowMessage('Properties can''t be saved, the project has closed.',mtError,'Sigh');
    ClearData;
  end;


end;

procedure TProjectSettingsEditor.WriteData_Written(Sender: TPromise);
begin
  if MainForm.Project <> nil then
  begin
    ClearModified;
    fWriting := False;
    MainForm.Project.ReadProjectProperties;
    MainForm.Project.ListAttachmentsForDocument(TDocumentPath.Root);
    SetupControls;

  end;
end;

procedure TProjectSettingsEditor.SetupControls;
var
  canEdit: Boolean;
begin
  canEdit := fDataAvailable and (not fWriting) and (fUIUpdateCount = 0);
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
  AddCategoryButton.Enabled := canEdit;
  DeleteCategoryButton.Enabled := canEdit;
  StatusDefinitionsLabel.Enabled:=canEdit;
  StatusDefinitionsGrid.Enabled := canEdit;
  AddStatusButton.Enabled := canEdit;
  DeleteStatusButton.Enabled := canEdit;
  DeadlinesLabel.Enabled := canEdit;
  DeadlinesGrid.Enabled := canEdit;
  AddDeadlineButton.Enabled := canEdit;
  DeleteDeadlineButton.Enabled := canEdit;
  UserPropertiesLabel.Enabled := canEdit;
  fUserPropertiesEditor.Enabled := canEdit;

end;

procedure TProjectSettingsEditor.SetupColorMenu;
  procedure AddMenuItem(aName: String; aColor: TColor);
  var
    aItem: TMenuItem;
  begin
    aItem := TMenuItem.Create(ColorMenu);
    aItem.Caption := aName;
    aItem.Tag := aColor;
    aItem.OnClick := @ColorMenuClick;
    ColorMenu.Items.Insert(ColorMenu.Items.IndexOf(CustomColorMenu),aItem);
  end;
begin
  if ColorMenu.Items.Count = 1 then
  begin
    AddMenuItem('Blank',clDefault);
    AddMenuItem('Black',clBlack);
    AddMenuItem('White',clWhite);
    AddMenuItem('Red',clRed);
    AddMenuItem('Yellow',clYellow);
    AddMenuItem('Green',clGreen);
    AddMenuItem('Blue',clBlue);
    AddMenuItem('Purple',clPurple);
    AddMenuItem('Grey',clGray);
    // clDkGray constant is assigned as a synonym of clGray. I'm fairly certain this
    // is a bug, but this goes back to Delphi, so it won't ever be fixed.
    AddMenuItem('Dark Grey',TColor($404040));
    AddMenuItem('Maroon',clMaroon);
    AddMenuItem('Lime',clLime);
    AddMenuItem('Olive',clOlive);
    AddMenuItem('Teal',clTeal);
    AddMenuItem('Sky Blue',clSkyBlue);
    AddMenuItem('Navy',clNavy);
    AddMenuItem('Fuchsia',clFuchsia);
  end;
end;

procedure TProjectSettingsEditor.ObserveMainForm(Sender: TMainForm;
  aAction: TMainFormAction; aDocument: TDocumentPath);
begin
  case aAction of
    mfaProjectOpened:
    begin
      MainForm.Project.AddObserver(@ObserveProject);
      MainForm.Project.ReadProjectProperties;
      MainForm.Project.ListAttachmentsForDocument(TDocumentPath.Root);
    end;
    mfaProjectClosed:
    begin
      MainForm.Project.RemoveObserver(@ObserveProject);
      ClearData;
    end;
  end;
end;

procedure TProjectSettingsEditor.ObserveProject(Sender: TStewProject;
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
    paProjectPropertiesDataReceived:
       ShowData((Event as TProjectPropertiesDataReceivedEvent).Properties);
    paAttachmentListDataReceived:
      if (Event as TAttachmentListDataReceivedEvent).Document = TDocumentPath.Root then
      begin
        AttachmentsListed((Event as TAttachmentListDataReceivedEvent).List);
      end;
  end;
end;

function TProjectSettingsEditor.SetupGlyphs: Boolean;
begin
  Result:=inherited SetupGlyphs;
  if Result then
  begin
    RefreshButton.ImageIndex := ord(sbgRefresh);
    SaveButton.ImageIndex := ord(sbgSave);
    EditNotesButton.ImageIndex := ord(sbgEditNotes);
  end;
end;

procedure TProjectSettingsEditor.EditNotesButtonClick(Sender: TObject);
const
  cMethod: String = 'TProjectSettingsEditor.EditNotesButtonClick';
begin
  LogAction(cMethod,Document.ID);
  if MainForm.Project <> nil then
     MainForm.Project.EditDocumentNotes(TDocumentPath.Root);
end;

procedure TProjectSettingsEditor.AddCategoryButtonClick(Sender: TObject);
var
  i: Integer;
begin
  with CategoryDefinitionsGrid  do
  begin
    i := RowCount;
    RowCount := RowCount + 1;
    Cells[CatNameCol,i] := 'Category ' + IntToStr(i);
    Cells[CatColorCol,i] := ColorToStr(clDefault);
    Cells[CatDefaultCol,i] := FalseValue;
    CategoryDefinitionsGrid.Row := i;
  end;
end;

procedure TProjectSettingsEditor.AddDeadlineButtonClick(Sender: TObject);
var
  i: Integer;
begin
  with DeadlinesGrid  do
  begin
    i := RowCount;
    RowCount := RowCount + 1;
    Cells[DeadlineNameCol,i] := 'Deadline ' + IntToStr(i);
    Cells[DeadlineDueCol,i] := DateToStr(Now + 7);
    DeadlinesGrid.Row := i;
    AutoSizeColumns;
  end;
end;

procedure TProjectSettingsEditor.AddStatusButtonClick(Sender: TObject);
var
  i: Integer;
begin
  with StatusDefinitionsGrid  do
  begin
    i := RowCount;
    RowCount := RowCount + 1;
    Cells[StatNameCol,i] := 'Status ' + IntToStr(i);
    Cells[StatColorCol,i] := ColorToStr(clDefault);
    Cells[StatDefaultCol,i] := FalseValue;
    StatusDefinitionsGrid.Row := i;
  end;
end;

procedure TProjectSettingsEditor.CategoryDefinitionsGridButtonClick(
  Sender: TObject; aCol, aRow: Integer);
var
  pt: TPoint;
begin
  case ((Sender as TStringGrid).Columns[aCol].Tag) of
    ord(ckColor):
    if (not (Sender as TStringGrid).EditorMode) then
    begin
      (Sender as TStringGrid).Col := aCol;
      (Sender as TStringGrid).Row := aRow;
      SetupColorMenu;
      ColorMenu.PopupComponent := Sender as TStringGrid;
      with (Sender as TStringGrid).CellRect(aCol,aRow) do
      begin
        pt := (Sender as TStringGrid).ClientToScreen(Point(Left,Bottom));
      end;
      ColorMenu.PopUp(pt.X,pt.Y);
    end;
    ord(ckDate):
    begin
      CalendarDialog.Date:=StrToDate((Sender as TStringGrid).Cells[aCol,aRow]);
      if CalendarDialog.Execute then
         (Sender as TStringGrid).Cells[aCol,aRow] := DateToStr(CalendarDialog.Date);
    end;
  end;
end;

procedure TProjectSettingsEditor.CategoryDefinitionsGridCheckboxToggled(
  sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
var
  i: Integer;
  grid: TStringGrid;
begin
  grid := sender as TStringGrid;
  if grid.Columns[aCol].Tag = ord(ckRadio) then
  begin
    grid := Sender as TStringGrid;
    if aState = cbUnchecked then
    // don't allow them to 'uncheck' a default.
      grid.Cells[aCol,aRow] := TrueValue
    else if aState = cbChecked then
    begin
      // uncheck the other one.
      for i := 0 to grid.RowCount - 1 do
      begin
        if i <> aRow then
          grid.Cells[aCol,i] := FalseValue;
      end;

    end;
  end;
end;

procedure TProjectSettingsEditor.CategoryDefinitionsGridDrawCell(
  Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aNewRect: TRect;
  aColor: TColor;
begin
  if (aRow > 0) and ((Sender as TStringGrid).Columns[aCol].Tag = ord(ckColor)) then
  begin
    with (Sender as TStringGrid) do
    begin
      aColor := StrToColor((Sender as TStringGrid).Cells[aCol,aRow]);
      if aColor = clDefault then
         aColor := clWindow;
      Canvas.Brush.Color := aColor;
      aNewRect.Left := aRect.Left + 4;
      aNewRect.Right := aRect.Right - 4;
      aNewRect.Top := aRect.Top + 4;
      aNewRect.Bottom := aRect.Bottom - 4;
      Canvas.RoundRect(aNewRect,4,4);
      //Canvas.TextOut(aRect.Left + 2,aRect.Top + 2,  Cells[ACol, ARow]);
    end;
  end;
end;

procedure TProjectSettingsEditor.CategoryDefinitionsGridValidateEntry(
  sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);
var
  aValue: Integer;
begin
  if (Sender as TStringGrid).Columns[aCol].Tag = Ord(ckInteger) then
  begin
    if NewValue = '' then
       NewValue := '0'
    else if not TryStrToInt(NewValue,aValue) then
       NewValue := OldValue;


  end;
end;

procedure TProjectSettingsEditor.CustomColorMenuClick(Sender: TObject);
var
  grid: TStringGrid;
begin
  grid := ColorMenu.PopupComponent as TStringGrid;
  CategoryColorChooser.Color := StrToColor(grid.Cells[grid.Col,grid.Row]);
  if CategoryColorChooser.Execute then
    grid.Cells[grid.Col,grid.Row] := ColorToStr(CategoryColorChooser.Color);
end;

procedure TProjectSettingsEditor.ColorMenuClick(Sender: TObject);
var
  grid: TStringGrid;
  menuColor: TColor;
begin
  menuColor := (Sender as TMenuItem).Tag;
  grid := ColorMenu.PopupComponent as TStringGrid;
  grid.Cells[grid.Col,grid.Row] := ColorToStr(menuColor);
end;

procedure TProjectSettingsEditor.DeleteCategoryButtonClick(Sender: TObject);
var
  category: String;
begin
  if CategoryDefinitionsGrid.Row > 0 then
  begin
    category := CategoryDefinitionsGrid.Cells[CatNameCol,CategoryDefinitionsGrid.Row];
    if MessageDlg('Are you sure you want to delete the category "' + category + '"?',mtConfirmation,mbYesNo,0) = mrYes then
    begin
       CategoryDefinitionsGrid.DeleteRow(CategoryDefinitionsGrid.Row);
       // This doesn't seem to be being set...
       CategoryDefinitionsGrid.Modified := true;
    end;
  end;
end;

procedure TProjectSettingsEditor.DeleteDeadlineButtonClick(Sender: TObject);
var
  deadline: STring;
begin
  if DeadlinesGrid.Row > 0 then
  begin
    deadline := DeadlinesGrid.Cells[DeadlineNameCol,DeadlinesGrid.Row];
    if MessageDlg('Are you sure you want to delete the deadling "' + deadline + '"?',mtConfirmation,mbYesNo,0) = mrYes then
    begin
       DeadlinesGrid.DeleteRow(DeadlinesGrid.Row);
       // This doesn't seem to be being set...
       DeadlinesGrid.Modified := true;
    end;
  end;
end;

procedure TProjectSettingsEditor.DeleteStatusButtonClick(Sender: TObject);
var
  status: String;
begin
  if StatusDefinitionsGrid.Row > 0 then
  begin
    status := StatusDefinitionsGrid.Cells[StatNameCol,StatusDefinitionsGrid.Row];
    if MessageDlg('Are you sure you want to delete the status "' + status + '"?',mtConfirmation,mbYesNo,0) = mrYes then
    begin
       StatusDefinitionsGrid.DeleteRow(StatusDefinitionsGrid.Row);
       // This doesn't seem to be being set...
       StatusDefinitionsGrid.Modified := true;
    end;
  end;
end;

procedure TProjectSettingsEditor.ClearData;
begin
  fDataAvailable := false;
  DefaultDocExtensionEdit.Text := '';
  DefaultNotesExtensionEdit.Text := '';
  DefaultThumbnailExtensionEdit.Text := '';

  CategoryDefinitionsGrid.Clear;
  // add one in for the fixed column header.
  CategoryDefinitionsGrid.RowCount := 1;

  StatusDefinitionsGrid.Clear;
  StatusDefinitionsGrid.RowCount := 1;

  DeadlinesGrid.Clear;
  DeadlinesGrid.RowCount := 1;

  fUserPropertiesEditor.SetMap(nil);
  ClearModified;
  SetupControls;
end;

procedure TProjectSettingsEditor.ClearModified;
begin
  DefaultDocExtensionEdit.Modified := false;
  DefaultNotesExtensionEdit.Modified := false;
  DefaultThumbnailExtensionEdit.Modified := false;
  CategoryDefinitionsGrid.Modified := false;
  StatusDefinitionsGrid.Modified := false;
  DeadlinesGrid.Modified := false;
  fUserPropertiesEditor.Modified := false;

end;

function TProjectSettingsEditor.IsModified: Boolean;
begin
  result := DefaultDocExtensionEdit.Modified or
            DefaultNotesExtensionEdit.Modified or
            DefaultThumbnailExtensionEdit.Modified or
            CategoryDefinitionsGrid.Modified or
            StatusDefinitionsGrid.Modified or
            DeadlinesGrid.Modified or
            fUserPropertiesEditor.Modified;

end;

procedure TProjectSettingsEditor.ShowData(aData: IProjectProperties);
var
  i: Integer;
  j: Integer;
  lKeys: TStringArray2;
  lCat: ICategoryDefinition;
begin
  fDataAvailable := true;

  if not DefaultDocExtensionEdit.Modified then
  begin
     DefaultDocExtensionEdit.Text := aData.defaultDocExtension;
     DefaultDocExtensionEdit.Modified:=false;
  end;
  if not DefaultNotesExtensionEdit.Modified then
  begin
     DefaultNotesExtensionEdit.Text := aData.defaultNotesExtension;
     DefaultNotesExtensionEdit.Modified := false;
  end;
  if not DefaultThumbnailExtensionEdit.Modified then
  begin
     DefaultThumbnailExtensionEdit.Text := aData.defaultThumbnailExtension;
     DefaultThumbnailExtensionEdit.Modified := false;
  end;

  with CategoryDefinitionsGrid do
  begin
    if not Modified then
    begin
      Clear;
      // add one in for the fixed column header.
      RowCount := 1;
      lKeys := aData.Categories.keys;
      for i := 0 to lKeys.Count - 1 do
      begin
        j := RowCount;
        lCat := aData.Categories[lKeys[i]];
        RowCount := RowCount + 1;
        Cells[CatNameCol,j] := lKeys[i];
        Cells[CatColorCol,j] := ColorToStr(lCat.Color);
        Cells[CatDefaultCol,j] := BoolToStr(aData.defaultCategory = lKeys[i],TrueValue,FalseValue);
        Cells[CatPTitleCol,j] := BoolToStr(lCat.publishTitle,TrueValue,FalseValue);
        Cells[CatPTitleLevelCol,j] := IntToStr(lCat.publishTitleLevel);
        Cells[CatPTitlePrefixCol,j] := lCat.publishTitlePrefix;
        Cells[CatPMarkerBeforeCol,j] := BoolToStr(lCat.publishMarkerBefore,TrueValue,FalseValue);
        Cells[CatPMarkerAfterCol,j] := BoolToStr(lCat.publishMarkerAfter,TrueValue,FalseValue);
        Cells[CatPMarkerBetweenCol,j] := BoolToStr(lCat.publishMarkerBetween,TrueValue,FalseValue);

      end;
      AutoSizeColumns;
      Modified := false;
    end;
  end;

  with StatusDefinitionsGrid do
  begin
    if not Modified then
    begin
      Clear;
      // add one in for the fixed column header.
      RowCount := 1;
      lKeys := aData.Statuses.keys;
      for i := 0 to lKeys.Count - 1 do
      begin
        j := RowCount;
        RowCount := RowCount + 1;
        Cells[StatNameCol,j] := lKeys[i];
        Cells[StatColorCol,j] := ColorToStr(aData.Statuses[lKeys[i]].color);
        Cells[StatDefaultCol,j] := BoolToStr(aData.defaultStatus = lKeys[i],TrueValue,FalseValue);
      end;
      AutoSizeColumns;
      Modified := false;

    end;
  end;

  with DeadlinesGrid do
  begin
    if not Modified then
    begin
      Clear;
      // add one in for the fixed column header.
      RowCount := 1;
      for i := 0 to aData.Deadlines.Length - 1 do
      begin
        j := RowCount;
        RowCount := RowCount + 1;
        Cells[DeadlineNameCol,j] := aData.Deadlines[i].Name;
        Cells[DeadlineDueCol,j] := DateToStr(aData.Deadlines[i].Due);
      end;
      AutoSizeColumns;
      Modified := false;

    end;
  end;

  if not fUserPropertiesEditor.Modified then
  begin
     fUserPropertiesEditor.SetMap(aData.User);
     fUserPropertiesEditor.Modified := false;
  end;

  SetupControls;
end;

procedure TProjectSettingsEditor.AttachmentsListed(aData: TAttachmentArray);
var
  i: Longint;
  l: Longint;
  lFoundNotes: Boolean;
begin
  // TODO: Also come up with a combobutton to edit the remaining attachments.
  // TODO: In fact, if there is more than one primary, or notes, the notes
  // button should be a dropdown button.
  lFoundNotes := false;
  l := Length(aData) - 1;
  for i := 0 to l do
  begin
    if (not lFoundNotes) and (aData[i].Kind = atNotes) then
    begin
       lFoundNotes := true;
    end;
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

procedure TProjectSettingsEditor.WriteData;
begin
  if MainForm.Project <> nil then
  begin
    fWriting := true;
    // retrieve the data first, then write it, so we have the most up-to-date
    // stuff, including any "unknowns".
    MainForm.Project.ReadProjectProperties.After(@WriteData_Read);
    SetupControls;

  end;
end;

procedure TProjectSettingsEditor.BeginUIUpdate;
begin
  inc(fUIUpdateCount);
  if fUIUpdateCount = 1 then
  begin
    SetupControls;
  end;
end;

procedure TProjectSettingsEditor.EndUIUpdate;
begin
  dec(fUIUpdateCount);
  if fUIUpdateCount = 0 then
  begin
    SetupControls;
  end;
end;

constructor TProjectSettingsEditor.Create(TheOwner: TComponent);
  procedure AddColumn(aGrid: TStringGrid; aIndex: Integer; aCaption: String; aType: TDefColumnKind);
  begin
    with aGrid.Columns.Insert(aIndex) as TGridColumn do
    begin
      Tag := ord(aType);
      case aType of
        ckString, ckInteger:
          ButtonStyle:=cbsAuto;
        ckDate:
          ButtonStyle := cbsEllipsis;
        ckBoolean, ckRadio:
        begin
          ButtonStyle := cbsCheckboxColumn;
          ValueChecked:=TrueValue;
          ValueUnchecked:=FalseValue;
        end;
        ckColor:
          ButtonStyle := cbsButtonColumn;
      end;
      Title.Caption := aCaption;
    end;
  end;

begin
  inherited Create(TheOwner);
  Caption := 'Project Settings';
  fUserPropertiesEditor := TJSONEditor.Create(UserPropertiesPanel);
  fUserPropertiesEditor.Parent := UserPropertiesPanel;
  //fUserPropertiesEditor.Height := 192;
  fUserPropertiesEditor.Align := alClient;
  fUserPropertiesEditor.BorderSpacing.Top := 10;

  AddColumn(CategoryDefinitionsGrid,CatNameCol,'Name',ckString);
  AddColumn(CategoryDefinitionsGrid,CatColorCol,'Color',ckColor);
  AddColumn(CategoryDefinitionsGrid,CatDefaultCol,'Default',ckRadio);
  AddColumn(CategoryDefinitionsGrid,CatPTitleCol,'Pub Title',ckBoolean);
  AddColumn(CategoryDefinitionsGrid,CatPTitleLevelCol,'Pub Level',ckInteger);
  AddColumn(CategoryDefinitionsGrid,CatPTitlePrefixCol,'Pub Prefix',ckString);
  AddColumn(CategoryDefinitionsGrid,CatPMarkerBeforeCol,'Pub Mark Bef',ckBoolean);
  AddColumn(CategoryDefinitionsGrid,CatPMarkerAfterCol,'Pub Mark Aft',ckBoolean);
  AddColumn(CategoryDefinitionsGrid,CatPMarkerBetweenCol,'Pub Mark Betw',ckBoolean);
  CategoryDefinitionsGrid.FixedRows := 1;

  AddColumn(StatusDefinitionsGrid,StatNameCol,'Name',ckString);
  AddColumn(StatusDefinitionsGrid,StatColorCol,'Color',ckColor);
  AddColumn(StatusDefinitionsGrid,StatDefaultCol,'Default',ckRadio);
  StatusDefinitionsGrid.FixedRows := 1;

  AddColumn(DeadlinesGrid,DeadlineNameCol,'Name',ckString);
  // TODO: Need 'ckDate';
  AddColumn(DeadlinesGrid,DeadlineDueCol,'Due',ckDate);
  DeadlinesGrid.FixedRows := 1;

  MainForm.Observe(@ObserveMainForm);
  // if the project is already opened, then notify self to make sure
  // we hook up to it.
  if MainForm.Project <> nil then
     ObserveMainForm(MainForm,mfaProjectOpened,TDocumentPath.Null);
end;

destructor TProjectSettingsEditor.Destroy;
begin
  if MainForm.Project <> nil then
     MainForm.Project.RemoveObserver(@ObserveProject);
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

function TProjectSettingsEditor.CloseQuery: Boolean;
begin
  if IsModified then
  begin
    SetFocus;
    {
    NOTE: The convention is to ask "Do You want to Save?" with a Yes/No/Cancel.
    Where the 'Yes' causes the save and still closes the document.

    But, why? Just because it's always been done that way?

    Here's my theory: Most of the time, when I get this message, I'm going to
    hit cancel, because I realized that there was some stuff I had to do yet.
    Sure, occasionally I just want to close and save, but usually I want to
    check on what I've got and decide whether I want to change.

    Since two options are always better for UI than three, I'm just getting
    rid of the 'Yes' here and leaving the 'Cancel'.

    And yes, I'm going to confess that since my saving is not an instantaneous
    process, following the original conventions is going to take a bit of work,
    and it's possible I'm rationalizing to avoid that work:
    I have to return false here, but once the document is saved, then close the
    tab. And, this gets even more complicated when the closequery is initiated
    by an application close.

    To avoid the convention, I'm making sure to specify custom button captions
    for the message box.

    }

    result := MainForm.MessageDialog('You are about to lose your changes to the project settings.' + LineEnding +
               'Are you sure you want to close?',mtWarning,mbYesNo,['Close Without Saving','Don''t Close']) = mrYes;
  end
  else
    result := true;
end;

end.

