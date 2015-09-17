unit gui_projectmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, stew_project, gui_mainform, stew_properties, graphics;

type

  { TProjectManager }

  TProjectManager = class(TFrame)
    DocumentGlyphs: TImageList;
    ProjectExplorer: TTreeView;
    ProjectToolbar: TToolBar;
    NewChildDocumentButton: TToolButton;
    RenameDocumentButton: TToolButton;
    DeleteDocumentButton: TToolButton;
    NewSiblingDocumentButton: TToolButton;
    MoveDocumentUpButton: TToolButton;
    MoveDocumentDownButton: TToolButton;
    procedure DeleteDocumentButtonClick(Sender: TObject);
    procedure MoveDocumentDownButtonClick(Sender: TObject);
    procedure MoveDocumentUpButtonClick(Sender: TObject);
    procedure NewChildDocumentButtonClick(Sender: TObject);
    procedure NewSiblingDocumentButtonClick(Sender: TObject);
    procedure ObserveMainForm(aAction: TMainFormAction; {%H-}aDocument: TDocumentPath);
    procedure ProjectExplorerCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure ProjectExplorerCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ProjectExplorerDblClick(Sender: TObject);
    procedure ProjectExplorerDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ProjectExplorerDragOver(Sender, Source: TObject; {%H-}X, {%H-}Y: Integer;
      {%H-}State: TDragState; var Accept: Boolean);
    procedure ProjectExplorerExpanding(Sender: TObject; Node: TTreeNode;
      var {%H-}AllowExpansion: Boolean);
    procedure ProjectExplorerSelectionChanged(Sender: TObject);
    procedure RenameDocumentButtonClick(Sender: TObject);
  private type

    { TProjectInspectorNode }

    TProjectInspectorNode = class(TTreeNode)
    private
      FDocumentID: TDocumentPath;
      fExpandOnList: Boolean;
      fIsLocked: Boolean;
      fIsNew: Boolean;
      FStatusColor: TColor;
      procedure SetDocumentID(AValue: TDocumentPath);
      procedure SetStatusColor(AValue: TColor);
    public
      property DocumentID: TDocumentPath read FDocumentID write SetDocumentID;
      property ExpandOnList: Boolean read fExpandOnList write fExpandOnList;
      property StatusColor: TColor read FStatusColor write SetStatusColor;
      property IsNew: Boolean read fIsNew write fIsNew;
      property IsLocked: Boolean read fIsLocked write fIsLocked;
      procedure SetStyleFromProjectProperties(aProps: TProjectProperties);
    end;
  private
    { private declarations }
    function GetProject: TStewProject;
    procedure ReloadNodeForDocument(aDocument: TDocumentPath);
    procedure ReloadNode(aNode: TProjectInspectorNode; aProps: TProjectProperties);
    function GetTreeNodeForDocument(aDocument: TDocumentPath): TProjectInspectorNode;
    procedure InitializeNode(aNode: TProjectInspectorNode; aDocument: TDocumentPath;
      aProps: TProjectProperties);
    procedure UpdateCategoryGlyphs;
    procedure SetupControls;
    procedure CreateNewDocument(aChild: Boolean);
  protected
    property Project: TStewProject read GetProject;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  dialogs, stew_types;

{$R *.lfm}

{ TProjectInspectorNode }

procedure TProjectManager.TProjectInspectorNode.SetDocumentID(AValue: TDocumentPath);
begin
  if FDocumentID <> AValue then
  begin
    FDocumentID := AValue;
    Text := AValue.Name;
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

  name := docData.GetName;
  if docProps.title <> '' then
    Text := docProps.title + ' (' + name + ')'
  else
    Text := name;

  if fIsNew <> docData.IsNew then
  begin
    fIsNew := docData.IsNew;
  end;

  if fIsLocked <> docData.IsLocked then
  begin
    fIsLocked := docData.IsLocked;
  end;
end;

{ TProjectManager }

procedure TProjectManager.ProjectExplorerExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  Document: TDocumentPath;
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
var
  aNode: TProjectInspectorNode;
  aDoc: TDocumentMetadata;
  aParent: TDocumentMetadata;
  aOldName: String;
  aNewName: String;
begin
  aNode := ProjectExplorer.Selected as TProjectInspectorNode;
  aDoc := MainForm.Project.GetDocument(aNode.DocumentID);
  if aDoc.IsLocked then
  begin
    ShowMessage('This document is open in a tab, to avoid problems with unsaved data, I can''t rename it until that tab is closed.');
    Exit;
  end;
  aOldName := aDoc.GetName;
  aNewName := aOldName;
  if InputQuery(MainForm.Caption,'Enter new name for document:',aNewName) then
  begin
    aParent := aDoc.GetParent;
    if aParent.IsTroublesome(aNewName) then
       ShowMessage('The name "' + aNewName + '" would cause problems as a filename on some computers.' + LineEnding +
                   'For example, the following characters are not allowed: <, >, :, ", /, \, |, ?, *, %, [, ], ~,{ ,}, ;' + LineEnding +
                   'Plus, spaces at the beginning or end of the name, a hyphen at the beginning, or two or more spaces together in the middle of a name.')
    else if aParent.HasDocument(aNewName) then
       ShowMessage('A document named "' + aNewName + '" already exists here.')
    else
       aParent.Rename(aOldName,aNewName);
  end;
end;

procedure TProjectManager.ObserveMainForm(aAction: TMainFormAction;
  aDocument: TDocumentPath);
begin
  case aAction of
    mfaDocumentsListed,mfaDocumentPropertiesLoaded,mfaDocumentPropertiesSaved,mfaDocumentChanged:
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

procedure TProjectManager.NewChildDocumentButtonClick(Sender: TObject);
begin
  CreateNewDocument(true);
end;

procedure TProjectManager.NewSiblingDocumentButtonClick(Sender: TObject);
begin
  CreateNewDocument(false);
end;

procedure TProjectManager.DeleteDocumentButtonClick(Sender: TObject);
begin
  ShowMessage('I can''t delete documents yet');
end;

procedure TProjectManager.MoveDocumentDownButtonClick(Sender: TObject);
var
  aNode: TProjectInspectorNode;
  aSiblingNode: TProjectInspectorNode;
  aDoc: TDocumentMetadata;
  aParent: TDocumentMetadata;
  aSibling: TDocumentMetadata;
begin
  aNode := ProjectExplorer.Selected as TProjectInspectorNode;
  aSiblingNode := aNode.GetNextSibling as TProjectInspectorNode;
  if aSiblingNode = nil then
    Exit;
  aDoc := MainForm.Project.GetDocument(aNode.DocumentID);
  aSibling := MainForm.Project.GetDocument(aSiblingNode.DocumentID);
  aParent := aDoc.GetParent;
  with aNode.GetPrevSibling do;
  if aParent.IsLocked then
     ShowMessage('The containing document is open in a tab. Please close that before you re-order it''s contents');

  aParent.OrderDocument(aDoc,odpAfter,aSibling);

end;

procedure TProjectManager.MoveDocumentUpButtonClick(Sender: TObject);
var
  aNode: TProjectInspectorNode;
  aSiblingNode: TProjectInspectorNode;
  aDoc: TDocumentMetadata;
  aParent: TDocumentMetadata;
  aSibling: TDocumentMetadata;
begin
  aNode := ProjectExplorer.Selected as TProjectInspectorNode;
  aSiblingNode := aNode.GetPrevSibling as TProjectInspectorNode;
  if aSiblingNode = nil then
    Exit;
  aDoc := MainForm.Project.GetDocument(aNode.DocumentID);
  aSibling := MainForm.Project.GetDocument(aSiblingNode.DocumentID);
  aParent := aDoc.GetParent;
  with aNode.GetPrevSibling do;
  if aParent.IsLocked then
     ShowMessage('The containing document is open in a tab. Please close that before you re-order it''s contents');

  aParent.OrderDocument(aDoc,odpBefore,aSibling);

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
  with Node as TProjectInspectorNode do
  begin
    Sender.Canvas.Font.Color := StatusColor;
    if IsNew then
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsItalic]
    else
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style - [fsItalic];

    if IsLocked then
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsBold]
    else
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style - [fsBold];

  end;

   DefaultDraw := true;
end;

procedure TProjectManager.ProjectExplorerDblClick(Sender: TObject);
var
  Node: TTreeNode;
  Document: TDocumentPath;
begin

  Node := ProjectExplorer.Selected;
  if (Node <> nil) then
  begin
    Document := (Node as TProjectInspectorNode).DocumentID;
    MainForm.OpenDocument(Document);

  end;

end;

procedure TProjectManager.ProjectExplorerDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  aDraggingNode: TProjectInspectorNode;
  aTargetNode: TProjectInspectorNode;
  aDraggingDocID: TDocumentPath;
  aTargetDocID: TDocumentPath;
  aTargetDoc: TDocumentMetadata;
  aDraggingDoc: TDocumentMetadata;
begin
  aTargetNode := ProjectExplorer.GetNodeAt(X,Y) as TProjectInspectorNode;
  if Source = Sender then
  begin
    aDraggingNode := ProjectExplorer.Selected as TProjectInspectorNode;
    if (aDraggingNode <> nil) and (aTargetNode <> aDraggingNode) and
       (MainForm.Project <> nil) and (MainForm.Project.IsOpened) then
    begin
      aDraggingDocID := aDraggingNode.DocumentID;
      if aTargetNode <> nil then
        aTargetDocID := aTargetNode.DocumentID
      else
        aTargetDocID := TDocumentPath.Root;
      aTargetDoc := MainForm.Project.GetDocument(aTargetDocID);
      aDraggingDoc:= MainForm.Project.GetDocument(aDraggingDocID);
      if aTargetDoc.ListingState <> lsListed then
        Exit;
      if aTargetDoc.HasDocument(aDraggingDoc.GetName) then
      begin
        ShowMessage('A document named "' + aDraggingDoc.GetName + '" already exists there.');
        Exit;
      end;
      aTargetDoc.MoveDocToHere(aDraggingDoc);
    end
  end;
end;

procedure TProjectManager.ProjectExplorerDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  aTargetNode: TProjectInspectorNode;
  aDraggingNode: TProjectInspectorNode;
  aTargetDoc: TDocumentMetadata;
begin
  Accept := false;
  if Source = Sender then
  begin
    aTargetNode := ProjectExplorer.GetNodeAt(X,Y) as TProjectInspectorNode;
    aDraggingNode := ProjectExplorer.Selected as TProjectInspectorNode;
    if aDraggingNode <> nil then
    begin
      if aTargetNode <> nil then
      begin
        if aTargetNode.HasAsParent(aDraggingNode) then
           Exit;
        if aTargetNode = aDraggingNode.Parent then
           Exit;
        aTargetDoc := MainForm.Project.GetDocument(aTargetNode.DocumentID)
      end
      else
        aTargetDoc := MainForm.Project.GetDocument(TDocumentPath.Root);

      if aTargetDoc.ListingState = lsNotListed then
        // list documents so we can retry very soon.
        aTargetDoc.ListDocuments(false);
      Accept := aTargetDoc.ListingState = lsListed;
    end;
  end;
end;

function TProjectManager.GetProject: TStewProject;
begin
  result := MainForm.Project;
end;

procedure TProjectManager.ReloadNodeForDocument(aDocument: TDocumentPath);
var
  aNode: TProjectInspectorNode;
begin
  if aDocument <> TDocumentPath.Root then
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
  aList: TDocumentArray;
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
      aDoc := MainForm.Project.GetDocument(TDocumentPath.Root);
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

function TProjectManager.GetTreeNodeForDocument(aDocument: TDocumentPath): TProjectInspectorNode;
begin
  result := ProjectExplorer.Items.GetFirstNode as TProjectInspectorNode;
  while result <> nil do
  begin
    if result.DocumentID = aDocument then
       Exit;
    if result.DocumentID.Contains(aDocument) then
       result := result.GetFirstChild as TProjectInspectorNode
    else
       result := result.GetNextSibling as TProjectInspectorNode;
  end;
  result := nil;
end;

procedure TProjectManager.InitializeNode(aNode: TProjectInspectorNode;
  aDocument: TDocumentPath; aProps: TProjectProperties);
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
const
  shadowOffset: Integer = 2;
  dogEarWidthMultiple: Integer = 3;
  pageWidthAspect: Double = (8.5/11);
var
  iconWidth: Integer;
  props: TProjectProperties;
  i: Integer;
  iconA: TPoint;
  iconB: TPoint;
  iconC: TPoint;
  iconD: TPoint;
  iconE: TPoint;
  iconF: TPoint;
  shadowA: TPoint;
  shadowB: TPoint;
  shadowC: TPoint;
  shadowE: TPoint;
  shadowF: TPoint;

  procedure CalculateGlyphPoints;
  var
    pageWidth: Integer;
    pageHeight: Integer;
    margin: Integer;
    dogEarWidth: Integer;
  begin
    {
       . . A X X X X X X X X X X B . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - - - - - X . .
       . . X - - - - - - D X X X C . .
       . . X - - - - - - X X X X . . .
       . . X - - - - - - X X X . . . .
       . . X - - - - - - X X . . . . .
       . . F X X X X X X E . . . z . .
    }
    pageHeight := (iconWidth - 1) - shadowOffset;
    pageWidth := trunc(pageHeight  * pageWidthAspect);
    margin := trunc((iconWidth - pageWidth) / 2);
    dogEarWidth := trunc(pageWidth / dogEarWidthMultiple);
    iconA := Point(margin,0);
    iconB := Point(pageWidth + margin,0);
    iconF := Point(margin,pageHeight);
    iconE := Point((pageWidth + margin) - dogEarWidth,pageHeight);
    iconC := Point(pageWidth + margin,pageHeight - dogEarWidth);
    iconD := Point(iconE.X,iconC.Y);
    shadowA := Point(iconA.X + shadowOffset,iconA.Y + shadowOffset);
    shadowB := Point(iconB.X + shadowOffset,iconB.Y + shadowOffset);
    shadowC := Point(iconC.X + shadowOffset,iconC.Y + shadowOffset);
    shadowE := Point(iconE.X + shadowOffset,iconE.Y + shadowOffset);
    shadowF := Point(iconF.X + shadowOffset,iconF.Y + shadowOffset);

  end;


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
        Brush.Color := clWindow;
        Pen.Color := clWindow;
        FillRect(0,0,iconWidth,iconWidth);
        // now, start drawing the glyph
        Pen.Width := 1;

        // the shadow
        Brush.Color := clBtnShadow;
        Pen.Color := clBtnShadow;
        Polygon([shadowA,shadowB,shadowC,shadowE,shadowF,shadowA]);

        // the actual page
        Brush.Color := aColor;
        Pen.Color := clWindowText;
        Polygon([iconA,iconB,iconC,iconD,iconE,iconF,iconA]);
        // and the little triangle
        Brush.Color := clWindowText;
        Polygon([iconD,iconC,iconE,iconD]);
        //Ellipse(circleOffset,circleOffset,iconWidth - circleOffset,iconWidth - circleOffset);
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
      CalculateGlyphPoints;
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
  canAddChildNode: Boolean;
  canAddSibNode: Boolean;
  canMoveDown: Boolean;
  canMoveUp: Boolean;
begin
  canManageNode := ProjectExplorer.Selected <> nil;
  if ProjectExplorer.Selected = nil then
  begin
    canManageNode := false;
    canAddChildNode := true;
    canAddSibNode := false;
    canMoveDown := false;
    canMoveUp := false;
  end
  else
  begin
    canManageNode := true;
    canAddChildNode := MainForm.Project.GetDocument((ProjectExplorer.Selected as TProjectInspectorNode).DocumentID).ListingState = lsListed;
    // if we're adding a sibling node, then the document is probably already
    // listed.
    canAddSibNode := true;
    canMoveDown:= ProjectExplorer.Selected.GetNextSibling <> nil;
    canMoveUp := ProjectExplorer.Selected.GetPrevSibling <> nil;
  end;
  RenameDocumentButton.Enabled := canManageNode;
  DeleteDocumentButton.Enabled := canManageNode;
  NewChildDocumentButton.Enabled := canAddChildNode;
  NewSiblingDocumentButton.Enabled:= canAddSibNode;
  MoveDocumentDownButton.Enabled:=canMoveDown;
  MoveDocumentUpButton.Enabled := canMoveUp;

end;

procedure TProjectManager.CreateNewDocument(aChild: Boolean);
var
  aNode: TProjectInspectorNode;
  aDocument: TDocumentPath;
  aName: String;
  aParent: TDocumentMetadata;
begin
  aName := '';
  if InputQuery(MainForm.Caption,'What would you like to name the new document?',aName) then
  begin
    aNode := ProjectExplorer.Selected as TProjectInspectorNode;
    if aNode = nil then
    begin
      if not aChild then
        raise Exception.Create('Can''t add a sibling document without a selected node');
      aDocument := TDocumentPath.Root;
    end
    else
      aDocument := aNode.DocumentID;
    if aChild then
       aParent := MainForm.Project.GetDocument(aDocument)
    else
    begin
       aParent := MainForm.Project.GetDocument(aDocument).GetParent;
       // the 'aDocument' should never be root, so the above should
       // always return *something*.
    end;
    if aParent.IsTroublesome(aName) then
       ShowMessage('The name "' + aName + '" would cause problems as a filename on some computers.' + LineEnding +
                   'For example, the following characters are not allowed: <, >, :, ", /, \, |, ?, *, %, [, ], ~,{ ,}, ;' + LineEnding +
                   'Plus, spaces at the beginning or end of the name, a hyphen at the beginning, or two or more spaces together in the middle of a name.')
    else if aParent.HasDocument(aName) then
       ShowMessage('A document named "' + aName + '" already exists here.')
    else
    begin
       aParent.CreateDocument(aName);
       if aChild and (aNode <> nil) then
         aNode.Expanded := true;
    end;
  end;
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

