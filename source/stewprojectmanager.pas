unit stewprojectmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, stewproject, stewfile, stewmainform, stewproperties, graphics;

type

  // TODO: May need a way to distinguish between creating a new sibling and a child document.

  { TProjectManager }

  TProjectManager = class(TFrame)
    DocumentGlyphs: TImageList;
    ProjectExplorer: TTreeView;
    ProjectToolbar: TToolBar;
    NewDocumentButton: TToolButton;
    RenameDocumentButton: TToolButton;
    DeleteDocumentButton: TToolButton;
    MoveDocumentButton: TToolButton;
    procedure DeleteDocumentButtonClick(Sender: TObject);
    procedure MoveDocumentButtonClick(Sender: TObject);
    procedure NewDocumentButtonClick(Sender: TObject);
    procedure ObserveMainForm(aAction: TMainFormAction; {%H-}aDocument: TDocumentID);
    procedure ProjectExplorerCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure ProjectExplorerCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ProjectExplorerDblClick(Sender: TObject);
    procedure ProjectExplorerExpanding(Sender: TObject; Node: TTreeNode;
      var {%H-}AllowExpansion: Boolean);
    procedure ProjectExplorerSelectionChanged(Sender: TObject);
    procedure RenameDocumentButtonClick(Sender: TObject);
  private type

    { TProjectInspectorNode }

    TProjectInspectorNode = class(TTreeNode)
    private
      FDocumentID: TDocumentID;
      fExpandOnList: Boolean;
      fIsNew: Boolean;
      FStatusColor: TColor;
      procedure SetDocumentID(AValue: TDocumentID);
      procedure SetStatusColor(AValue: TColor);
    public
      property DocumentID: TDocumentID read FDocumentID write SetDocumentID;
      property ExpandOnList: Boolean read fExpandOnList write fExpandOnList;
      property StatusColor: TColor read FStatusColor write SetStatusColor;
      property IsNew: Boolean read fIsNew write fIsNew;
      procedure SetStyleFromProjectProperties(aProps: TProjectProperties);
    end;
  private
    { private declarations }
    function GetProject: TStewProject;
    procedure ReloadNodeForDocument(aDocument: TDocumentID);
    procedure ReloadNode(aNode: TProjectInspectorNode; aProps: TProjectProperties);
    function GetTreeNodeForDocument(aDocument: TDocumentID): TProjectInspectorNode;
    procedure InitializeNode(aNode: TProjectInspectorNode; aDocument: TDocumentID;
      aProps: TProjectProperties);
    procedure UpdateCategoryGlyphs;
    procedure SetupControls;
  protected
    property Project: TStewProject read GetProject;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  dialogs, stewtypes;

{$R *.lfm}

{ TProjectInspectorNode }

procedure TProjectManager.TProjectInspectorNode.SetDocumentID(AValue: TDocumentID);
begin
  if FDocumentID <> AValue then
  begin
    FDocumentID := AValue;
    Text := ExtractDocumentName(AValue);
  end;
end;

procedure TProjectManager.TProjectInspectorNode.SetStatusColor(AValue: TColor);
begin
  if FStatusColor=AValue then Exit;
  FStatusColor:=AValue;
end;

procedure TProjectManager.TProjectInspectorNode.SetStyleFromProjectProperties(
  aProps: TProjectProperties);
var
  docData: TDocumentMetadata;
  docProps: TDocumentProperties;
  status: TKeywordDefinition;
  name: String;
begin
  if aProps = nil then
  begin
    if (MainForm.Project <> nil) and MainForm.Project.IsOpened then
      aProps := MainForm.Project.Properties
    else
    begin
      ImageIndex := 0;
      SelectedIndex := 0;
      StatusColor := clDefault;
      Exit;
    end;
  end;

  docData := MainForm.Project.GetDocument(DocumentID);
  docProps := docData.Properties;
  ImageIndex := aProps.categories.IndexOf(docProps.category) + 1;
  SelectedIndex:= ImageIndex;

  status := aProps.statuses.Find(docProps.status) as TKeywordDefinition;
  if status <> nil then
     StatusColor := status.color
  else
     StatusColor := clDefault;

  name := ExtractDocumentName(DocumentID);
  if docProps.title <> '' then
    Text := docProps.title + ' (' + name + ')'
  else
    Text := name;

  // If we are losing "newness", then it's possible that the parent is also
  // no longer "new", because a directory, at least, has been created.
  if fIsNew <> docData.IsNew then
  begin
    if fIsNew and (Parent <> nil) then
       (Parent as TProjectInspectorNode).SetStyleFromProjectProperties(aProps);
    fIsNew := docData.IsNew;
  end;
end;

{ TProjectManager }

procedure TProjectManager.ProjectExplorerExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  Document: TDocumentID;
  aDoc: TDocumentMetadata;
begin

    Document := (Node as TProjectInspectorNode).DocumentID;
    aDoc := Project.GetDocument(Document);
    if aDoc.ListingState = lsNotListed then
    begin
      Project.GetDocument(Document).ListDocuments(false);
      // also need to reload the properties to get the right sort order.
      (Node as TProjectInspectorNode).ExpandOnList := true;
    end;
end;

procedure TProjectManager.ProjectExplorerSelectionChanged(Sender: TObject);
var
  aNode: TProjectInspectorNode;
  aDoc: TDocumentMetadata;
begin
  SetupControls;
  // This is necessary so that I can create child nodes easily.
  aNode := ProjectExplorer.Selected as TProjectInspectorNode;
  if aNode <> nil then
  begin
    aDoc := MainForm.Project.GetDocument(aNode.DocumentID);
    if aDoc.ListingState = lsNotListed then
      aDoc.ListDocuments(false);
  end;
end;

procedure TProjectManager.RenameDocumentButtonClick(Sender: TObject);
begin
  ShowMessage('I can''t rename documents yet');
end;

procedure TProjectManager.ObserveMainForm(aAction: TMainFormAction;
  aDocument: TDocumentID);
begin
  case aAction of
    mfaDocumentsListed,mfaDocumentPropertiesLoaded,mfaDocumentPropertiesSaved:
    begin
      ReloadNodeForDocument(aDocument);
      SetupControls;
    end;
    mfaProjectPropertiesLoaded, mfaProjectPropertiesSaved:
    begin
      UpdateCategoryGlyphs;
      SetupControls;
    end;
  end;
end;

procedure TProjectManager.NewDocumentButtonClick(Sender: TObject);
var
  aNode: TProjectInspectorNode;
  aDocument: TDocumentID;
  aName: String;
  aParent: TDocumentMetadata;
begin
  aName := '';
  if InputQuery(MainForm.Caption,'What would you like to name the new document?',aName) then
  begin
    aNode := ProjectExplorer.Selected as TProjectInspectorNode;
    if aNode = nil then
      aDocument := RootDocument
    else
      aDocument := aNode.DocumentID;
    aParent := MainForm.Project.GetDocument(aDocument);
    if aParent.HasDocument(aName) then
       ShowMessage('A document named "' + aName + '" already exists here.')
    else
    begin
       aParent.CreateDocument(aName);
       if aNode <> nil then
         aNode.Expanded := true;
    end;
  end;
end;

procedure TProjectManager.DeleteDocumentButtonClick(Sender: TObject);
begin
  ShowMessage('I can''t delete documents yet');
end;

procedure TProjectManager.MoveDocumentButtonClick(Sender: TObject);
begin
  ShowMessage('I can''t move documents yet');
end;

procedure TProjectManager.ProjectExplorerCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TProjectInspectorNode;
end;

procedure TProjectManager.ProjectExplorerCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  // NOTE: In order for this to work, tvoThemedDraw must be turned off
  // in the TTreeView options.
   Sender.Canvas.Font.Color := (Node as TProjectInspectorNode).StatusColor;
   if (Node as TProjectInspectorNode).IsNew then
     Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsItalic]
   else
     Sender.Canvas.Font.Style := Sender.Canvas.Font.Style - [fsItalic];

   DefaultDraw := true;
end;

procedure TProjectManager.ProjectExplorerDblClick(Sender: TObject);
var
  Node: TTreeNode;
  Document: TDocumentID;
begin

  Node := ProjectExplorer.Selected;
  if (Node <> nil) then
  begin
    Document := (Node as TProjectInspectorNode).DocumentID;
    MainForm.OpenDocument(Document);

  end;

end;

function TProjectManager.GetProject: TStewProject;
begin
  result := MainForm.Project;
end;

procedure TProjectManager.ReloadNodeForDocument(aDocument: TDocumentID);
var
  aNode: TProjectInspectorNode;
begin
  if aDocument <> RootDocument then
  begin
     aNode := GetTreeNodeForDocument(aDocument);
     ReloadNode(aNode,nil);
  end
  else
     ReloadNode(nil,nil);
  // else, I don't care about this document because I haven't loaded it yet.
end;

procedure TProjectManager.ReloadNode(aNode: TProjectInspectorNode; aProps: TProjectProperties);
var
  aDoc: TDocumentMetadata;
  aList: TDocumentList;
  i: Integer;
  aChild: TProjectInspectorNode;
  aMatch: TProjectInspectorNode;
begin

  if aProps = nil then
  begin
    if (MainForm.Project <> nil) and MainForm.Project.IsOpened then
      aProps := MainForm.Project.Properties;
  end;

  ProjectExplorer.BeginUpdate;
  try
    if aNode <> nil then
    begin
      aNode.SetStyleFromProjectProperties(aProps);
      aDoc := MainForm.Project.GetDocument(aNode.DocumentID);
      aChild := aNode.GetFirstChild as TProjectInspectorNode;

    end
    else
    begin
      aDoc := MainForm.Project.GetDocument(RootDocument);
      aChild := ProjectExplorer.Items.GetFirstNode as TProjectInspectorNode;
    end;

    if aDoc.ListingState = lsListed then
    begin
      aList := aDoc.GetContents;

      for i := 0 to Length(aList) - 1 do
      begin
        // first, look for a match starting at the current node.
        aMatch := aChild; // if this is nil, then the next loop won't even run.
        while (aMatch <> nil) and (aMatch.DocumentID <> aList[i]) do
          aMatch := aMatch.GetNextSibling as TProjectInspectorNode;

        // did we find a match? BTW: If child was nil then we didn't find a match.
        if aMatch <> nil then
        begin
          // is the match the same as the child we were looking at?
          if aMatch <> aChild then
          begin
            // move it before the one we had before.
            aMatch.MoveTo(aChild,naInsert);
            // switch the current child to this moved one.
            aChild := aMatch;
          end;
          // else do nothing and go on to the next

          // reload the node from it's list and properties. This will
          // not go and get data it doesn't have, it will just update
          // the data it *does* have.
          ReloadNode(aChild,aProps);

        end
        else
        begin
          // we didn't find a match, so we need to create one.
          // create a new node that matches.
          if aChild <> nil then
             aChild := ProjectExplorer.Items.Insert(aChild,'') as TProjectInspectorNode
          else
             aChild := ProjectExplorer.Items.AddChild(aNode,'') as TProjectInspectorNode;
          // initialize it.
          InitializeNode(aChild,aList[i],aProps);
        end;

        // go on to the next sibling and the next item in the list.
        aChild := aChild.GetNextSibling as TProjectInspectorNode;

      end;

      // if achild is not nil then we reached the end of the list
      // and there are still some extra nodes left.
      while aChild <> nil do
      begin
        aMatch := aChild.GetNextSibling as TProjectInspectorNode;
        ProjectExplorer.Items.Delete(aChild);
        aChild := aMatch;
      end;

      if (aNode <> nil) then
      begin
        if (aNode.ExpandOnList) then
        begin
          aNode.ExpandOnList := false;
          if Length(aList) > 0 then
             aNode.Expanded := true
        end;
        if Length(aList) = 0 then
           aNode.HasChildren:=false;
      end;
    end;

  finally
    ProjectExplorer.EndUpdate;
  end;

end;

function TProjectManager.GetTreeNodeForDocument(aDocument: TDocumentID): TProjectInspectorNode;
begin
  result := ProjectExplorer.Items.GetFirstNode as TProjectInspectorNode;
  while result <> nil do
  begin
    if result.DocumentID = aDocument then
       Exit;
    if IsParentDocument(result.DocumentID,aDocument) then
       result := result.GetFirstChild as TProjectInspectorNode
    else
       result := result.GetNextSibling as TProjectInspectorNode;
  end;
  result := nil;
end;

procedure TProjectManager.InitializeNode(aNode: TProjectInspectorNode;
  aDocument: TDocumentID; aProps: TProjectProperties);
var
  docData: TDocumentMetadata;
  docProps: TDocumentProperties;
begin
  aNode.DocumentID:=aDocument;
  aNode.SetStyleFromProjectProperties(aProps);
  if (MainForm.Project <> nil) then
  begin
    docData := MainForm.Project.GetDocument(aDocument);
    if (docData.ListingState <> lsListed) then
       aNode.HasChildren := true;

    // we need property information to display it.
    docProps := docData.Properties;
    if docProps.FilingState = fsNotLoaded then
      docProps.Load;
  end;
end;

procedure TProjectManager.UpdateCategoryGlyphs;
var
  circleOffset: Integer;
  iconWidth: Integer;
  props: TProjectProperties;
  i: Integer;

  function AddBitmapForColor(aColor: TColor): Integer;
  var
    bm: TBitmap;
  begin
    bm := TBitmap.Create;
    try
      bm.Width := iconWidth;
      bm.Height := iconWidth;
      with bm.Canvas do
      begin
        // TODO: okay.. this is pretty ugly, a png would be better,
        // but it will do for now since I'm going to switch to glyphs
        // later anyway.
        Brush.Color := clWindow;
        Pen.Color := clWindow;
        FillRect(0,0,iconWidth,iconWidth);
        Brush.Color := aColor;
        Pen.Color := clWindowText;
        Pen.Width := 2;
        Ellipse(circleOffset,circleOffset,iconWidth - circleOffset,iconWidth - circleOffset);
      end;
      result := DocumentGlyphs.AddMasked(bm,clDefault);
    finally
      bm.Free;
    end;

  end;

begin
  if (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
  begin
    props := MainForm.Project.Properties;
    DocumentGlyphs.BeginUpdate;
    try
      DocumentGlyphs.Clear;
      iconWidth := DocumentGlyphs.Width;
      circleOffset := 2;
      // add the basic, default glyph.
      AddBitmapForColor(clDefault);
      for i := 0 to props.categories.NameCount - 1 do
        AddBitmapForColor((props.categories.Items[props.categories.Names[i]] as TKeywordDefinition).color);
    finally
      DocumentGlyphs.EndUpdate;
    end;
    ProjectExplorer.BeginUpdate;
    try
      for i := 0 to ProjectExplorer.Items.Count - 1 do
        (ProjectExplorer.Items[i] as TProjectInspectorNode).SetStyleFromProjectProperties(props);
    finally
      ProjectExplorer.EndUpdate;
    end;
  end;
end;

procedure TProjectManager.SetupControls;
var
  canManageNode: Boolean;
  canAddNode: Boolean;
begin
  canManageNode := ProjectExplorer.Selected <> nil;
  if ProjectExplorer.Selected = nil then
  begin
    canManageNode := false;
    canAddNode := true;
  end
  else
  begin
    canManageNode := true;
    canAddNode := MainForm.Project.GetDocument((ProjectExplorer.Selected as TProjectInspectorNode).DocumentID).ListingState = lsListed;
  end;
  RenameDocumentButton.Enabled := canManageNode;
  MoveDocumentButton.Enabled := canManageNode;
  DeleteDocumentButton.Enabled := canManageNode;
  NewDocumentButton.Enabled := canAddNode;
end;

constructor TProjectManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  MainForm.Observe(@ObserveMainForm);
  Text := 'Project';
  SetupControls;
end;

destructor TProjectManager.Destroy;
begin
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

end.

