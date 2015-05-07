unit stewprojectmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, stewproject, stewfile, stewmainform, stewproperties;

// TODO: Need custom text or background color based on statuses in categories.
// - whenever the project properties are loaded or saved:
//   - update the treeview drawing
// - in the treeviewitem oncustomdraw, supposedly, we can set the font background color
//   and then just set DefaultDraw to true to get it to work.

// TODO: Should be called a 'manager', not an 'inspector'.

// TODO: Need to make use of status and category colors somehow. Category is
// actually associated with a glyph on a treeview, and should therefore be used
// to draw a little circle. While the status colors should change the text. I
// should also like to find out what the background is for the text and if it's
// too similar to the status text color, change it just for that node.

// TODO: Should use the 'title' property, if it's set, to display the
// name of the file. It would be displayed with the actual filename in parenthesis,
// for example:
//   The Doom of Valendia: The Final Chapter (the_doom_final_chapter)
// however, if I allow renaming of a file, it will effect the file name itself,
// and you need to change the Title property to get the other stuff.

type

  { TProjectManager }

  TProjectManager = class(TFrame)
    DocumentGlyphs: TImageList;
    ProjectExplorer: TTreeView;
    procedure ObserveMainForm(aAction: TMainFormAction; {%H-}aDocument: TDocumentID);
    procedure ProjectExplorerCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure ProjectExplorerDblClick(Sender: TObject);
    procedure ProjectExplorerExpanding(Sender: TObject; Node: TTreeNode;
      var {%H-}AllowExpansion: Boolean);
  private type

    { TProjectInspectorNode }

    TProjectInspectorNode = class(TTreeNode)
    private
      FDocumentID: TDocumentID;
      fExpandOnList: Boolean;
      procedure SetDocumentID(AValue: TDocumentID);
    public
      property DocumentID: TDocumentID read FDocumentID write SetDocumentID;
      property ExpandOnList: Boolean read fExpandOnList write fExpandOnList;
      procedure UpdateGlyph(aProps: TProjectProperties);
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
  protected
    property Project: TStewProject read GetProject;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  dialogs, Graphics, stewtypes;

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

procedure TProjectManager.TProjectInspectorNode.UpdateGlyph(
  aProps: TProjectProperties);
begin
  if aProps = nil then
  begin
    if (MainForm.Project <> nil) and MainForm.Project.IsOpened then
      aProps := MainForm.Project.Properties
    else
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
      Exit;
    end;
  end;
  ImageIndex := aProps.categories.IndexOf(MainForm.Project.GetDocument(DocumentID).Properties.category);
  SelectedIndex:= ImageIndex;
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

procedure TProjectManager.ObserveMainForm(aAction: TMainFormAction;
  aDocument: TDocumentID);
begin
  case aAction of
    mfaDocumentsListed,mfaDocumentPropertiesLoaded,mfaDocumentPropertiesSaved:
    begin
      ReloadNodeForDocument(aDocument);
    end;
    mfaProjectPropertiesLoaded, mfaProjectPropertiesSaved:
    begin
      UpdateCategoryGlyphs;
    end;
  end;
end;

procedure TProjectManager.ProjectExplorerCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TProjectInspectorNode;
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
      aNode.UpdateGlyph(aProps);
      aList := MainForm.Project.GetDocument(aNode.DocumentID).GetContents;
      aChild := aNode.GetFirstChild as TProjectInspectorNode;

    end
    else
    begin
      aList := MainForm.Project.GetDocument(RootDocument).GetContents;
      aChild := ProjectExplorer.Items.GetFirstNode as TProjectInspectorNode;
    end;

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

    if (aNode <> nil) and (aNode.ExpandOnList) then
    begin
      aNode.ExpandOnList := false;
      if Length(aList) > 0 then
         aNode.Expanded := true
      else
      begin
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
  docProps: TDocumentProperties;
begin
  aNode.DocumentID:=aDocument;
  aNode.HasChildren:=true;
  aNode.UpdateGlyph(aProps);
  if (MainForm.Project <> nil) then
  begin
    // we need property information to display it.
    docProps := MainForm.Project.GetDocument(aDocument).Properties;
    if docProps.FilingState = fsNotLoaded then
      docProps.Load;
  end;
end;

procedure TProjectManager.UpdateCategoryGlyphs;
var
  imHeight: Integer;
  props: TProjectProperties;
  i: Integer;

  function AddBitmapForColor(aColor: TColor): Integer;
  var
    bm: TBitmap;
  begin
    bm := TBitmap.Create;
    try
      bm.Width := imHeight;
      bm.Height := imHeight;
      with bm.Canvas do
      begin
        // TODO: okay.. this is pretty ugly, a png would be better,
        // but it will do for now since I'm going to switch to glyphs
        // later anyway.
        Brush.Color := clWindow;
        Pen.Color := clWindow;
        FillRect(0,0,imHeight,imHeight);
        Brush.Color := aColor;
        Pen.Color := clWindowText;
        Pen.Width := 2;
        Ellipse(2,2,imHeight - 2,imHeight - 2);
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
      imHeight := ProjectExplorer.DefaultItemHeight-1;
      for i := 0 to props.categories.NameCount - 1 do
        AddBitmapForColor((props.categories.Items[props.categories.Names[i]] as TKeywordDefinition).color);
    finally
      DocumentGlyphs.EndUpdate;
    end;
    ProjectExplorer.BeginUpdate;
    try
      for i := 0 to ProjectExplorer.Items.Count - 1 do
        (ProjectExplorer.Items[i] as TProjectInspectorNode).UpdateGlyph(props);
    finally
      ProjectExplorer.EndUpdate;
    end;
  end;
end;

constructor TProjectManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  MainForm.Observe(@ObserveMainForm);
  Text := 'Project';
end;

destructor TProjectManager.Destroy;
begin
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

end.

