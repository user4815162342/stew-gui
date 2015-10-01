unit gui_projectmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, stew_project, gui_mainform, stew_properties, graphics, fgl, sys_async;

type

{ FUTURE:
  - When node is selected, the status color disappears.
    - Attempting to override the brush color when a node is selected
      doesn't change the selection color.
    - Attempting to change SelectionColor with each SelectedChange causes
      all nodes below that node to suddenly have that background color.
}


  TKeywordColorMap = specialize TFPGMap<UTF8String,TColor>;
  TColorNumberMap = specialize TFPGMap<TColor,Integer>;

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
      fExpandedFirstTime: Boolean;
      fExpandOnList: Boolean;
      fIsOpen: Boolean;
      fIsNew: Boolean;
      FStatusColor: TColor;
      fStatus: UTF8String;
      fCategory: UTF8String;
      procedure SetDocumentID(AValue: TDocumentPath);
      procedure SetStatusColor(AValue: TColor);
    public
      property DocumentID: TDocumentPath read FDocumentID write SetDocumentID;
      property ExpandedFirstTime: Boolean read fExpandedFirstTime write fExpandedFirstTime;
      property ExpandOnList: Boolean read fExpandOnList write fExpandOnList;
      property StatusColor: TColor read FStatusColor write SetStatusColor;
      property IsNew: Boolean read fIsNew write fIsNew;
      property IsOpen: Boolean read fIsOpen write fIsOpen;
      property Status: UTF8String read fStatus write fStatus;
      property Category: UTF8String read fCategory write fCategory;
    end;

    { TShiftDocumentAfterRenameTask }

    TShiftDocumentAfterRenameTask = class(TDeferredTask)
    private
      fNewIndex: Integer;
    protected
      procedure DoTask(Input: TPromise); override;
      function CreatePromise: TPromise; override;
    public
      constructor Defer(aNewIndex: Integer; aInputPromise: TRenameDocumentPromise);
    end;

  private
    { private declarations }
    fUIUpdateCount: Integer;
    fStatuses: TKeywordColorMap;
    fCategoryColors: TKeywordColorMap;
    fCategoryGlyphIndexes: TColorNumberMap;
    procedure UpdateCategoryGlyphs;
    procedure SetupControls;
    procedure CreateNewDocument(aChild: Boolean);
  protected
    function GetTreeNodeForDocument(aDocument: TDocumentPath): TProjectInspectorNode;
    function BuildTreeNodeForDocument(aDocument: TDocumentPath): TProjectInspectorNode;
    procedure UpdateNode(aNode: TProjectInspectorNode; aInfo: TDocumentInfo);
    procedure UpdateNode(aNode: TProjectInspectorNode; aProperties: TDocumentProperties);
    procedure UpdateNodeStyle(aNode: TProjectInspectorNode);
    procedure BeginUIUpdate;
    procedure EndUIUpdate;
    procedure ProjectPropertiesUpdated(aProperties: TProjectProperties2);
    procedure DocumentUpdated(aParent: TDocumentPath; aContents: TDocumentInfoArray);
    procedure DocumentUpdated(aParent: TDocumentPath; aProperties: TDocumentProperties);
    procedure RefreshDocument(aDocument: TDocumentPath);
    procedure ObserveMainForm(Sender: TMainForm; aAction: TMainFormAction; aDocument: TDocumentPath);
    procedure ObserveProject(Sender: TStewProject; Event: TProjectEvent);

  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  dialogs, sys_types;

{$R *.lfm}

{ TProjectManager.TShiftDocumentAfterRenameTask }

procedure TProjectManager.TShiftDocumentAfterRenameTask.DoTask(Input: TPromise);
begin
  if MainForm.Project <> nil then
     MainForm.Project.ShiftDocumentTo((Input as TRenameDocumentPromise).New,fNewIndex).After(@SubPromiseResolved,@SubPromiseRejected);
end;

function TProjectManager.TShiftDocumentAfterRenameTask.CreatePromise: TPromise;
begin
  result := TPromise.Create;
end;

constructor TProjectManager.TShiftDocumentAfterRenameTask.Defer(
  aNewIndex: Integer; aInputPromise: TRenameDocumentPromise);
begin
  fNewIndex := aNewIndex;
  inherited Defer(aInputPromise);
end;

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

{ TProjectManager }

procedure TProjectManager.ProjectExplorerExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  lDocument: TDocumentPath;
begin
  AllowExpansion := true;
  if not (Node as TProjectInspectorNode).ExpandedFirstTime then
  begin
    lDocument := (Node as TProjectInspectorNode).DocumentID;
    (Node as TProjectInspectorNode).ExpandedFirstTime := true;
    (Node as TProjectInspectorNode).ExpandOnList:=true;
    // don't allow expansion, ExpandOnList will take care of that.
    AllowExpansion:=false;
    if MainForm.Project <> nil then
       MainForm.Project.ListDocumentsInFolder(lDocument);

  end;
end;

procedure TProjectManager.ProjectExplorerSelectionChanged(Sender: TObject);
begin
  SetupControls;
end;

procedure TProjectManager.RenameDocumentButtonClick(Sender: TObject);
var
  lNode: TProjectInspectorNode;
  lDocument: TDocumentPath;
  lOldName: String;
  lNewName: String;
  lNewDocument: TDocumentPath;
begin
  if MainForm.Project <> nil then
  begin
    lNode := ProjectExplorer.Selected as TProjectInspectorNode;
    lDocument := lNode.DocumentID;
    lOldName := lDocument.Name;
    lNewName := lOldName;
    if InputQuery(MainForm.Caption,'Enter new name for document:',lNewName) then
    begin
      if IsNameTroublesome(lNewName) then
         ShowMessage('The name "' + lNewName + '" would cause problems as a filename on some computers.' + LineEnding +
                     'For example, the following characters are not allowed: <, >, :, ", /, \, |, ?, *, %, [, ], ~,{ ,}, ;' + LineEnding +
                     'Plus, spaces at the beginning or end of the name, a hyphen at the beginning, or two or more spaces together in the middle of a name.')
      else
      begin
        lNewDocument := lDocument.Container.GetContainedDocument(lNewName);

        // check if the new document name already exists. We're going to be lazy
        // and just check in the project inspector itself.
        if GetTreeNodeForDocument(lNewDocument) <> nil then
        begin
          ShowMessage('There is already another document with the name "' + lNewName + '".');
          Exit;
        end;

        TShiftDocumentAfterRenameTask.Defer(lNode.Index,MainForm.Project.RenameDocument(lDocument,lNewDocument));
        // set the current node to point to the new document, so that it remains selected.
        lNode.DocumentID := lNewDocument;
      end;
    end;
  end;
end;

procedure TProjectManager.ObserveMainForm(Sender: TMainForm;
  aAction: TMainFormAction; aDocument: TDocumentPath);
var
  lNode: TProjectInspectorNode;
begin
  case aAction of
    mfaProjectOpened:
    begin
      MainForm.Project.AddObserver(@ObserveProject);
      MainForm.Project.ListDocumentsInFolder(TDocumentPath.Root);
      MainForm.Project.ReadProjectProperties;
    end;
    mfaProjectClosed:
    begin
      MainForm.Project.RemoveObserver(@ObserveProject);
      ProjectExplorer.Items.Clear;
    end;
    mfaDocumentOpened, mfaDocumentClosed:
    begin
      lNode := GetTreeNodeForDocument(aDocument);
      if lNode <> nil then
      begin
        ProjectExplorer.BeginUpdate;
        try
          UpdateNodeStyle(lNode);
        finally
          ProjectExplorer.EndUpdate;
        end;
      end;
    end;
  end;
end;

procedure TProjectManager.ObserveProject(Sender: TStewProject;
  Event: TProjectEvent);
var
  lDocument: TDocumentPath;
  lNewDocument: TDocumentPath;
begin
  case Event.Action of
   paProjectPropertiesDataReceived:
   begin
     ProjectPropertiesUpdated((Event as TProjectPropertiesDataReceivedEvent).Properties);
   end;
   paListingDocumentFiles:
     BeginUIUpdate;
   paDocumentFilesListed,
   paDocumentsFileListingFailed:
     EndUIUpdate;
   paLoadingAttachmentFile,
   paSavingAttachmentFile:
   begin
     if ((Event as TAttachmentEvent).Attachment = atProperties) then
        BeginUIUpdate;
   end;
   paAttachmentFileLoaded,
   paAttachmentFileLoadingFailed,
   paAttachmentFileSaved,
   paAttachmentFileSavingFailed:
     if ((Event as TAttachmentEvent).Attachment = atProperties) then
        EndUIUpdate;
   paDocumentListDataReceived:
   begin
     DocumentUpdated((Event as TDocumentListDataReceivedEvent).Document,
                    (Event as TDocumentListDataReceivedEvent).List);
     SetupControls;
   end;
   paAttachmentDataReceived:
     if ((Event as TAttachmentEvent).Attachment = atProperties) then
     begin
       DocumentUpdated((Event as TAttachmentEvent).Document,
                       (Event as TDocumentPropertiesDataReceivedEvent).Properties);
     end;
   paShadowCreated,
   paShadowUncreated,
   paDocumentShifted:
   begin
     RefreshDocument((Event as TDocumentEvent).Document.Container);
   end;
   paDocumentRenamed:
   begin
     // refresh the containers...
     lDocument := (Event as TDocumentRenameEvent).Document.Container;
     lNewDocument := (Event as TDocumentRenameEvent).NewDocument.Container;
     RefreshDocument(lDocument);
     if lDocument <> lNewDocument then
        RefreshDocument(lNewDocument);
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
  lNode: TProjectInspectorNode;
  lSiblingNode: TProjectInspectorNode;
  lDoc: TDocumentPath;
begin
  if MainForm.Project <> nil then
  begin
    lNode := ProjectExplorer.Selected as TProjectInspectorNode;
    lSiblingNode := lNode.GetNextSibling as TProjectInspectorNode;
    if lSiblingNode = nil then
      Exit;
    lDoc := lNode.DocumentID;
    MainForm.Project.ShiftDocumentDown(lDoc);
  end;

end;

procedure TProjectManager.MoveDocumentUpButtonClick(Sender: TObject);
var
  lNode: TProjectInspectorNode;
  lSiblingNode: TProjectInspectorNode;
  lDoc: TDocumentPath;
begin
  if MainForm.Project <> nil then
  begin
    lNode := ProjectExplorer.Selected as TProjectInspectorNode;
    lSiblingNode := lNode.GetPrevSibling as TProjectInspectorNode;
    if lSiblingNode = nil then
      Exit;
    lDoc := lNode.DocumentID;
    MainForm.Project.ShiftDocumentUp(lDoc);
  end;

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

    if IsOpen then
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsBold]
    else
      Sender.Canvas.Font.Style := Sender.Canvas.Font.Style - [fsBold];

    if cdsSelected in State then
    begin
      // FUTURE: This isn't doing anything...
      Canvas.Brush.Color := StatusColor;
      Canvas.Font.Color := InvertColor(StatusColor);
    end;

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
  lDraggingNode: TProjectInspectorNode;
  lTargetNode: TProjectInspectorNode;
  lDraggingDocID: TDocumentPath;
  lTargetDocID: TDocumentPath;
  lNewDocID: TDocumentPath;
begin
  if Source = Sender then
  begin
    if fUIUpdateCount > 0 then
       // currently updating, don't do anything.
       Exit;
    lTargetNode := ProjectExplorer.GetNodeAt(X,Y) as TProjectInspectorNode;
    lDraggingNode := ProjectExplorer.Selected as TProjectInspectorNode;
    if (lDraggingNode <> nil) and (lTargetNode <> lDraggingNode) and
       (MainForm.Project <> nil) then
    begin
      lDraggingDocID := lDraggingNode.DocumentID;
      if lTargetNode <> nil then
      begin
        lTargetDocID := lTargetNode.DocumentID;
      end
      else
      begin
        lTargetDocID := TDocumentPath.Root;
      end;

      lNewDocID := lTargetDocID.GetContainedDocument(lDraggingDocID.Name);

      if GetTreeNodeForDocument(lNewDocID) <> nil then
      begin
        ShowMessage('A document named "' + lNewDocID.Name + '" already exists there.');
        Exit;
      end;

      if (lTargetNode <> nil) and (not lTargetNode.HasChildren) then
      begin
        // make sure the node shows as having children...
        lTargetNode.HasChildren := true;
        lTargetNode.ExpandedFirstTime:=false;
      end;

      MainForm.Project.RenameDocument(lDraggingDocID,lNewDocID);
    end
  end;
end;

procedure TProjectManager.ProjectExplorerDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  lTargetNode: TProjectInspectorNode;
  lDraggingNode: TProjectInspectorNode;
begin
  Accept := false;
  if Source = Sender then
  begin
    if fUIUpdateCount > 0 then
    begin
       Exit;
    end;
    lDraggingNode := ProjectExplorer.Selected as TProjectInspectorNode;
    if lDraggingNode <> nil then
    begin
      lTargetNode := ProjectExplorer.GetNodeAt(X,Y) as TProjectInspectorNode;
      if lTargetNode <> nil then
      begin
        if lTargetNode.HasAsParent(lDraggingNode) then
        begin
           Exit;
        end;
        if lTargetNode = lDraggingNode.Parent then
        begin
           Exit;
        end;
        if lTargetNode.HasChildren and not lTargetNode.ExpandedFirstTime then
        begin
          // can't see if we can put it in there yet, so don't accept, but
          // cause the documents to be listed.
          lTargetNode.Expanded := true;
        end
        else
        begin
          Accept := true;
        end;
      end
      else
      begin
        // target is the root. Just accept it.
        Accept := true;
      end;

    end;
  end
  else
  begin
    Accept := false;
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

procedure TProjectManager.UpdateCategoryGlyphs;
const
  shadowOffset: Integer = 2;
  dogEarWidthMultiple: Integer = 3;
  pageWidthAspect: Double = (8.5/11);
var
  iconWidth: Integer;
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

var
  lIndex: Integer;
  lColor: TColor;

begin
  if (MainForm.Project <> nil) then
  begin
    DocumentGlyphs.BeginUpdate;
    try
      DocumentGlyphs.Clear;
      fCategoryGlyphIndexes.Clear;
      iconWidth := DocumentGlyphs.Width;
      CalculateGlyphPoints;
      // add the basic, default glyph.
      fCategoryGlyphIndexes[clDefault] := AddBitmapForColor(clDefault);
      for i := 0 to fCategoryColors.Count - 1 do
      begin
        lColor := fCategoryColors.Data[i];
        if not fCategoryGlyphIndexes.Find(lColor,lIndex) then
           fCategoryGlyphIndexes[lColor] := AddBitmapForColor(lColor);
      end;
    finally
      DocumentGlyphs.EndUpdate;
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
    canAddChildNode := fUIUpdateCount = 0;
    canAddSibNode := false;
    canMoveDown := false;
    canMoveUp := false;
  end
  else
  begin
    canManageNode := fUIUpdateCount = 0;
    canAddChildNode :=
        (fUIUpdateCount = 0) and
        (ProjectExplorer.Selected as TProjectInspectorNode).ExpandedFirstTime;
    // if we're adding a sibling node, then the document is probably already
    // listed.
    canAddSibNode := fUIUpdateCount = 0;
    canMoveDown:=
       (fUIUpdateCount = 0) and
       (ProjectExplorer.Selected.GetNextSibling <> nil);
    canMoveUp :=
       (fUIUpdateCount = 0) and
       (ProjectExplorer.Selected.GetPrevSibling <> nil);
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
  lNode: TProjectInspectorNode;
  lDocument: TDocumentPath;
  lParent: TDocumentPath;
  lName: String;
begin
  if MainForm.Project <> nil then
  begin
    lName := '';
    if InputQuery(MainForm.Caption,'What would you like to name the new document?',lName) then
    begin
      lNode := ProjectExplorer.Selected as TProjectInspectorNode;
      if lNode = nil then
      begin
        if not aChild then
          raise Exception.Create('Can''t add a sibling document without a selected node');
        lParent := TDocumentPath.Root;
      end
      else
        lParent := lNode.DocumentID;
      if not aChild then
      begin
         lParent := lParent.Container;
         // the 'lDocument' should never be root, because of how controls
         // are enabled, so the above should always return *something*.
      end;
      if IsNameTroublesome(lName) then
         ShowMessage('The name "' + lName + '" would cause problems as a filename on some computers.' + LineEnding +
                     'For example, the following characters are not allowed: <, >, :, ", /, \, |, ?, *, %, [, ], ~,{ ,}, ;' + LineEnding +
                     'Plus, spaces at the beginning or end of the name, a hyphen at the beginning, or two or more spaces together in the middle of a name.')
      else
      begin
        lDocument := lParent.GetContainedDocument(lName);
        if GetTreeNodeForDocument(lDocument) <> nil then
        begin
            ShowMessage('A document named "' + lName + '" already exists here.');
            Exit;
        end;
        MainForm.Project.CreateShadow(lDocument);
        if aChild and (lNode <> nil) then
           lNode.Expanded := true;
      end;
    end;
  end;
end;

function TProjectManager.BuildTreeNodeForDocument(aDocument: TDocumentPath
  ): TProjectInspectorNode;
var
  lParent: TProjectInspectorNode;
begin
  if aDocument = TDocumentPath.Root then
  begin
    // essentially, it's already built.
    result := nil;
    Exit;
  end
  else
  begin
    lParent := BuildTreeNodeForDocument(aDocument.Container);
    if lParent = nil then
    begin
       result := ProjectExplorer.Items.GetFirstNode as TProjectInspectorNode
    end
    else
    begin
      result := lParent.GetFirstChild as TProjectInspectorNode;
    end;

    while result <> nil do
    begin
      if result.DocumentID = aDocument then
         Exit;
      result := result.GetNextSibling as TProjectInspectorNode;
    end;

    // if we didn't actually find one, then create one.
    result := ProjectExplorer.Items.AddChild(lParent,aDocument.Name) as TProjectInspectorNode;
    // create the node. Set it to 'IsShadow' because we don't have actual
    // data to establish the node exists. Set IsFolder to false in case
    // we don't actually get any data yet.
    UpdateNode(result,TDocumentInfo.Make(aDocument,false,true));
  end;
end;

procedure TProjectManager.UpdateNode(aNode: TProjectInspectorNode;
  aInfo: TDocumentInfo);
begin
  ProjectExplorer.BeginUpdate;
  try
    aNode.DocumentID := aInfo.Document;
    if (aNode.HasChildren <> aInfo.IsFolder) then
    begin
      if aInfo.IsFolder then
      begin
        aNode.HasChildren := true;
        aNode.ExpandedFirstTime := false
      end
      else
      begin
        aNode.DeleteChildren;
      end;

    end;
    aNode.IsNew := aInfo.IsShadow;
    UpdateNodeStyle(aNode);

  finally
    ProjectExplorer.EndUpdate;
  end;
end;

procedure TProjectManager.UpdateNode(aNode: TProjectInspectorNode;
  aProperties: TDocumentProperties);
var
  lName: String;
begin
  ProjectExplorer.BeginUpdate;
  try
    aNode.Status := aProperties.Status;
    aNode.Category := aProperties.Category;

    lName := aNode.DocumentID.Name;
    if aProperties.Title <> '' then
      lName := aProperties.Title + ' (' + lName + ')';
    aNode.Text := lName;
    UpdateNodeStyle(aNode);
  finally
    ProjectExplorer.EndUpdate;
  end;
end;

procedure TProjectManager.UpdateNodeStyle(aNode: TProjectInspectorNode);
var
  lIndex: Integer;
  lColor: TColor;
begin
  if fCategoryColors.Find(aNode.Category,lIndex) then
  begin
    if fCategoryGlyphIndexes.Find(fCategoryColors.Data[lIndex],lIndex) then
    begin
      lIndex := fCategoryGlyphIndexes.Data[lIndex];
    end
    else
      lIndex := 0;
  end
  else
     lIndex := 0;
  aNode.ImageIndex := lIndex;
  aNode.SelectedIndex:= aNode.ImageIndex;

  if fStatuses.Find(aNode.Status,lIndex) then
    lColor := fStatuses.Data[lIndex]
  else
    lColor := clDefault;
  aNode.StatusColor := lColor;

  aNode.IsOpen := MainForm.IsDocumentOpen(aNode.DocumentID);
  aNode.IsNew := (MainForm.Project <> nil) and MainForm.Project.IsShadow(aNode.DocumentID);

end;

procedure TProjectManager.BeginUIUpdate;
begin
  inc(fUIUpdateCount);
  if fUIUpdateCount = 1 then
  begin
    SetupControls;
  end;
end;

procedure TProjectManager.EndUIUpdate;
begin
  dec(fUIUpdateCount);
  if fUIUpdateCount = 0 then
  begin
    SetupControls;
  end;

end;

procedure TProjectManager.ProjectPropertiesUpdated(
  aProperties: TProjectProperties2);
var
  lNode: TProjectInspectorNode;
  lKeys: TStringArray;
  i: Integer;
begin

  lKeys := aProperties.Statuses.keys;
  fStatuses.Clear;
  for i := 0 to Length(lKeys) - 1 do
  begin
    fStatuses[lKeys[i]] := (aProperties.Statuses.Get(lKeys[i]) as TStatusDefinition).Color;
  end;

  lKeys := aProperties.Categories.keys;
  fCategoryColors.Clear;
  for i := 0 to Length(lKeys) - 1 do
  begin
    fCategoryColors[lKeys[i]] := (aProperties.Categories.Get(lKeys[i]) as TCategoryDefinition).Color;
  end;
  UpdateCategoryGlyphs;

  // now, for all of the nodes:
  lNode := ProjectExplorer.Items.GetFirstNode as TProjectInspectorNode;
  while lNode <> nil do
  begin
    UpdateNodeStyle(lNode);
    lNode := lNode.GetNext as TProjectInspectorNode;
  end;

end;

procedure TProjectManager.DocumentUpdated(aParent: TDocumentPath;
  aContents: TDocumentInfoArray);

  procedure UpdateMatch(aNode: TProjectInspectorNode; aInfo: TDocumentInfo);
  begin
    // if the node is not expanded, delete the children so
    // they get refreshed later instead of refreshing *everything*:
    if not aNode.Expanded then
      aNode.DeleteChildren;

    // update the node from it's info. This should also ensure
    // that HasChildren and such get set after deleting children above.
    UpdateNode(aNode,aInfo);
    // refresh the list and properties...
    RefreshDocument(aNode.DocumentID);

  end;

var
  lNode: TProjectInspectorNode;
  lChild: TProjectInspectorNode;
  lMatch: TProjectInspectorNode;
  i: Integer;
begin
  ProjectExplorer.BeginUpdate;
  try
    lNode := BuildTreeNodeForDocument(aParent);

    if lNode <> nil then
    begin
      lChild := lNode.GetFirstChild as TProjectInspectorNode;
    end
    else
    begin
      // we got the root document...
      lChild := ProjectExplorer.Items.GetFirstNode as TProjectInspectorNode;
    end;

    for i := 0 to Length(aContents) - 1 do
    begin
      // first, look for a match starting at the current node.
      lMatch := lChild; // if this is nil, then the next loop won't even run.
      while (lMatch <> nil) and (lMatch.DocumentID <> aContents[i].Document) do
        lMatch := lMatch.GetNextSibling as TProjectInspectorNode;

      // did we find a match? BTW: If child was nil then we didn't find a match.
      if lMatch <> nil then
      begin
        // is the match the same as the child we were looking at?
        if lMatch <> lChild then
        begin
          // move it before the one we had before.
          lMatch.MoveTo(lChild,naInsert);
          // switch the current child to this moved one.
          lChild := lMatch;
        end;
        // else do nothing and go on to the next

        UpdateMatch(lChild,aContents[i]);
      end
      else
      begin
        // we didn't find a match, so we need to create one.
        // create a new node that matches.
        if lChild <> nil then
           // insert new node after previous child
           lChild := ProjectExplorer.Items.Insert(lChild,aContents[i].Document.Name) as TProjectInspectorNode
        else
           // just add a new child to the parent node.
           lChild := ProjectExplorer.Items.AddChild(lNode,aContents[i].Document.Name) as TProjectInspectorNode;

        UpdateMatch(lChild,aContents[i]);
      end;

      // go on to the next sibling and the next item in the list.
      lChild := lChild.GetNextSibling as TProjectInspectorNode;

    end;

    // if achild is not nil then we reached the end of the list
    // and there are still some extra nodes left.
    while lChild <> nil do
    begin
      lMatch := lChild.GetNextSibling as TProjectInspectorNode;
      ProjectExplorer.Items.Delete(lChild);
      lChild := lMatch;
    end;

    if (lNode <> nil) and lNode.ExpandOnList then
    begin
      lNode.ExpandOnList := false;
      lNode.Expanded := true;
    end;

  finally
    ProjectExplorer.EndUpdate;
  end;
end;

procedure TProjectManager.DocumentUpdated(aParent: TDocumentPath;
  aProperties: TDocumentProperties);
var
  lNode: TProjectInspectorNode;
begin
  lNode := BuildTreeNodeForDocument(aParent);
  if lNode <> nil then
    UpdateNode(lNode,aProperties);
end;

procedure TProjectManager.RefreshDocument(aDocument: TDocumentPath);
var
  lNode: TProjectInspectorNode;
begin
  if MainForm.Project <> nil then
  begin
    if aDocument = TDocumentPath.Root then
    begin
      // we'll always refresh the root, since it should be refreshed from the start.
      MainForm.Project.ListDocumentsInFolder(aDocument);
    end
    else
    begin
      lNode := GetTreeNodeForDocument(aDocument);
      // only "refresh" the document if we already have a node.
      if lNode <> nil then
      begin
        // only list if they've been expanded already
         if lNode.ExpandedFirstTime then
            MainForm.Project.ListDocumentsInFolder(aDocument);
         // get properties for every one, so we get document colors correct.
         MainForm.Project.ReadDocumentProperties(aDocument);
         // No need to call 'after' on the promise, as the event should be triggered,
         // and this will be handled in ObserveProject.
      end;

    end;
  end;
end;

constructor TProjectManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fUIUpdateCount := 0;
  fStatuses := TKeywordColorMap.Create;
  // These maps have to be sorted for us to be able to find.
  fStatuses.Sorted:=true;
  fCategoryGlyphIndexes := TColorNumberMap.Create;
  fCategoryGlyphIndexes.Sorted:=true;
  fCategoryColors := TKeywordColorMap.Create;
  fCategoryColors.Sorted := true;
  MainForm.Observe(@ObserveMainForm);
  Text := 'Project';
  SetupControls;
end;

destructor TProjectManager.Destroy;
begin
  if MainForm.Project <> nil then
     MainForm.Project.RemoveObserver(@ObserveProject);
  MainForm.Unobserve(@ObserveMainForm);
  FreeAndNil(fStatuses);
  FreeAndNil(fCategoryColors);
  FreeAndNil(fCategoryGlyphIndexes);
  inherited Destroy;
end;

end.

