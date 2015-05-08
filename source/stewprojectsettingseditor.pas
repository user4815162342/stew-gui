unit stewprojectsettingseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  Grids, Dialogs, steweditorframe, stewproperties, stewmainform,
  stewjsoneditor, stewproject, graphics, Menus;

// TODO: I need to fix the stew cli to handle the statuses as a map instead of
// an array of strings.

// TODO: More properties that need to be handled.
//    - editors for certain file extensions (See Preferences Menu).
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
    AddStatusButton: TToolButton;
    CategoryDefinitionsGrid: TStringGrid;
    CategoryDefinitionsToolbar: TToolBar;
    AddCategoryButton: TToolButton;
    CategoryColorChooser: TColorDialog;
    ColorMenu: TPopupMenu;
    CustomColorMenu: TMenuItem;
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
    procedure AddCategoryButtonClick(Sender: TObject);
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
    procedure DeleteStatusButtonClick(Sender: TObject);
    procedure EditNotesButtonClick(Sender: TObject);
    procedure ObserveMainForm(aAction: TMainFormAction; {%H-}aDocument: TDocumentID);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    procedure SetupControls;
    function ColorFromCell(aGrid: TStringGrid; aCol: Integer; aRow: Integer): TColor;
    procedure ColorToCell(aGrid: TStringGrid; aCol: Integer; aRow: Integer; aColor: TColor);
    procedure SetupColorMenu;
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
  fpjson, stewtypes;

type
  TDefColumnKind = (ckString, ckInteger, ckBoolean, ckRadio, ckColor);

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

  TrueValue: String = 'Yes';
  FalseValue: String = 'No';

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
  ShowMessage('The old version will be backed up in case this doesn''t work.');
  WriteDataFromUser;
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

function TProjectSettingsEditor.ColorFromCell(aGrid: TStringGrid;
  aCol: Integer; aRow: Integer): TColor;
begin
  result := TColor(StrToInt('$' + aGrid.Cells[aCol,aRow]));
end;

procedure TProjectSettingsEditor.ColorToCell(aGrid: TStringGrid; aCol: Integer;
  aRow: Integer; aColor: TColor);
begin
  aGrid.Cells[aCol,aRow] := IntToHex(aColor,8);
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
    // is a bug.
    // TODO: Report bug.
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
       MainForm.Project.GetDocument(RootDocument).Notes.OpenInEditor;
  end;

end;

procedure TProjectSettingsEditor.AddCategoryButtonClick(Sender: TObject);
var
  i: Integer;
begin
  with CategoryDefinitionsGrid  do
  begin
    i := RowCount;
    RowCount := RowCount + 1;
    Cells[CatNameCol,i] := 'Category ' + IntToStr(i + 1);
    ColorToCell(CategoryDefinitionsGrid,CatColorCol,i,clDefault);
    Cells[CatDefaultCol,i] := FalseValue;
    CategoryDefinitionsGrid.Row := i;
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
    Cells[StatNameCol,i] := 'Status ' + IntToStr(i + 1);
    ColorToCell(StatusDefinitionsGrid,StatColorCol,i,clDefault);
    Cells[StatDefaultCol,i] := FalseValue;
    StatusDefinitionsGrid.Row := i;
  end;
end;

procedure TProjectSettingsEditor.CategoryDefinitionsGridButtonClick(
  Sender: TObject; aCol, aRow: Integer);
var
  pt: TPoint;
begin
  if (Sender as TStringGrid).Columns[aCol].Tag = ord(ckColor) then
  begin
    SetupColorMenu;
    ColorMenu.PopupComponent := Sender as TStringGrid;
    with (Sender as TStringGrid).CellRect(aCol,aRow) do
    begin
      pt := (Sender as TStringGrid).ClientToScreen(Point(Left,Bottom));
    end;
    ColorMenu.PopUp(pt.X,pt.Y);
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
      aColor := ColorFromCell(Sender as TStringGrid,aCol,aRow);
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
  CategoryColorChooser.Color := ColorFromCell(grid,grid.Col,grid.Row);
  if CategoryColorChooser.Execute then
    ColorToCell(grid,grid.Col,grid.Row,CategoryColorChooser.Color);
end;

procedure TProjectSettingsEditor.ColorMenuClick(Sender: TObject);
var
  grid: TStringGrid;
  menuColor: TColor;
begin
  menuColor := (Sender as TMenuItem).Tag;
  grid := ColorMenu.PopupComponent as TStringGrid;
  ColorToCell(grid,grid.Col,grid.Row,menuColor);
end;

procedure TProjectSettingsEditor.DeleteCategoryButtonClick(Sender: TObject);
var
  category: String;
begin
  category := CategoryDefinitionsGrid.Cells[CatNameCol,CategoryDefinitionsGrid.Row];
  if MessageDlg('Are you sure you want to delete the category "' + category + '"?',mtConfirmation,mbYesNo,0) = mrYes then
     CategoryDefinitionsGrid.DeleteRow(CategoryDefinitionsGrid.Row);
end;

procedure TProjectSettingsEditor.DeleteStatusButtonClick(Sender: TObject);
var
  status: String;
begin
  status := StatusDefinitionsGrid.Cells[StatNameCol,StatusDefinitionsGrid.Row];
  if MessageDlg('Are you sure you want to delete the status "' + status + '"?',mtConfirmation,mbYesNo,0) = mrYes then
     StatusDefinitionsGrid.DeleteRow(StatusDefinitionsGrid.Row);
end;

procedure TProjectSettingsEditor.ShowDataToUser;
var
  props: TProjectProperties;
  aCat: TCategoryDefinition;
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
        aCat := props.categories[aName] as TCategoryDefinition;
        RowCount := RowCount + 1;
        Cells[CatNameCol,i] := aName;
        ColorToCell(CategoryDefinitionsGrid,CatColorCol,i,aCat.color);
        Cells[CatDefaultCol,i] := BoolToStr(props.defaultCategory = aName,TrueValue,FalseValue);
        Cells[CatPTitleCol,i] := BoolToStr(aCat.publishTitle,TrueValue,FalseValue);
        Cells[CatPTitleLevelCol,i] := IntToStr(aCat.publishTitleLevel);
        Cells[CatPTitlePrefixCol,i] := aCat.publishTitlePrefix;
        Cells[CatPMarkerBeforeCol,i] := BoolToStr(aCat.publishMarkerBefore,TrueValue,FalseValue);
        Cells[CatPMarkerAfterCol,i] := BoolToStr(aCat.publishMarkerAfter,TrueValue,FalseValue);
        Cells[CatPMarkerBetweenCol,i] := BoolToStr(aCat.publishMarkerBetween,TrueValue,FalseValue);

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
        Cells[StatNameCol,i] := aName;
        ColorToCell(StatusDefinitionsGrid,StatColorCol,i,(props.statuses[aName] as TStatusDefintion).color);
        Cells[StatDefaultCol,i] := BoolToStr(props.defaultStatus = aName,TrueValue,FalseValue);
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
  i: Integer;
  j: Integer;
  aFound: Boolean;
  aLevel: Integer;
  aNames: TStringArray;
  aCat: TCategoryDefinition;
  aStat: TStatusDefintion;
begin
  result := false;
  props := nil;
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    props := MainForm.Project.Properties;
    props.defaultDocExtension := DefaultDocExtensionEdit.Text;
    props.defaultNotesExtension:= DefaultNotesExtensionEdit.Text;
    props.defaultThumbnailExtension:=DefaultThumbnailExtensionEdit.Text;


    with CategoryDefinitionsGrid do
    begin
      // changes and new items
      for i := 0 to RowCount - 1 do
      begin
        aCat := props.categories.Find(Cells[CatNameCol,i]) as TCategoryDefinition;
        if aCat = nil then
          aCat := props.categories.Add(Cells[CatNameCol,i]) as TCategoryDefinition;
        aCat.color := ColorFromCell(CategoryDefinitionsGrid,CatColorCol,i);
        aCat.publishTitle:=Cells[CatPTitleCol,i] = TrueValue;
        if TryStrToInt(Cells[CatPTitleLevelCol,i],aLevel) then
           aCat.publishTitleLevel := aLevel;
        aCat.publishTitlePrefix := Cells[CatPTitlePrefixCol,i];
        aCat.publishMarkerAfter:=Cells[CatPMarkerAfterCol,i] = TrueValue;
        aCat.publishMarkerBefore:=Cells[CatPMarkerBeforeCol,i] = TrueValue;
        aCat.publishMarkerBetween:=Cells[CatPMarkerBetweenCol,i] = TrueValue;

        if Cells[CatDefaultCol,i] = TrueValue then
          props.defaultCategory:=Cells[CatNameCol,i];
      end;

      // deletions
      aNames := props.categories.GetNameList;

      for i := 0 to Length(aNames) - 1 do
      begin
        aFound := false;
        for j := 0 to RowCount - 1 do
        begin
          if Cells[CatNameCol,j] = aNames[i] then
          begin
            aFound := true;
            break;
          end;
        end;
        if not aFound then
           props.categories.Delete(aNames[i]);
      end;

    end;

    with StatusDefinitionsGrid do
    begin
      // changes and new items
      for i := 0 to RowCount - 1 do
      begin
        aStat := props.statuses.Find(Cells[StatNameCol,i]) as TStatusDefintion;
        if aStat = nil then
          aStat := props.statuses.Add(Cells[StatNameCol,i]) as TStatusDefintion;
        aStat.color := ColorFromCell(StatusDefinitionsGrid,StatColorCol,i);
        if Cells[StatDefaultCol,i] = TrueValue then
          props.defaultStatus:=Cells[StatNameCol,i];
      end;

      // deletions
      aNames := props.statuses.GetNameList;

      for i := 0 to Length(aNames) - 1 do
      begin
        aFound := false;
        for j := 0 to RowCount - 1 do
        begin
          if Cells[StatNameCol,j] = aNames[i] then
          begin
            aFound := true;
            break;
          end;
        end;
        if not aFound then
           props.statuses.Delete(aNames[i]);
      end;

    end;



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
  procedure AddColumn(aGrid: TStringGrid; aIndex: Integer; aCaption: String; aType: TDefColumnKind);
  begin
    with aGrid.Columns.Insert(aIndex) as TGridColumn do
    begin
      Tag := ord(aType);
      case aType of
        ckString, ckInteger:
          ButtonStyle:=cbsAuto;
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

  ShowDataToUser;
  MainForm.Observe(@ObserveMainForm);
end;

destructor TProjectSettingsEditor.Destroy;
begin
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

end.

