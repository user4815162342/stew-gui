unit stewprojectinspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, stewproject, stewfile, stewmainform;

// TODO: Should use the 'title' property, if it's set, to display the
// name of the file. It would be displayed with the actual filename in parenthesis,
// for example:
//   The Doom of Valendia: The Final Chapter (the_doom_final_chapter)
// however, if I allow renaming of a file, it will effect the file name itself,
// and you need to change the Title property to get the other stuff.

type

  { TProjectInspectorFrame }

  TProjectInspectorFrame = class(TFrame)
    ProjectExplorer: TTreeView;
    procedure ObserveMainForm(aAction: TMainFormAction; {%H-}aDocument: TDocumentID);
    procedure ProjectDocumentsError(Data: String);
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
    end;
  private
    { private declarations }
    function GetProject: TStewProject;
    procedure ReloadNodeForDocument(aDocument: TDocumentID);
    procedure ReloadNode(aNode: TProjectInspectorNode);
    function GetTreeNodeForDocument(aDocument: TDocumentID): TProjectInspectorNode;
  protected
    property Project: TStewProject read GetProject;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshProject;
  end;

implementation

uses
  dialogs;

{$R *.lfm}

{ TProjectInspectorNode }

procedure TProjectInspectorFrame.TProjectInspectorNode.SetDocumentID(AValue: TDocumentID);
begin
  if FDocumentID <> AValue then
  begin
    FDocumentID := AValue;
    Text := MainForm.Project.GetBaseName(AValue);
  end;
end;

{ TProjectInspectorFrame }

procedure TProjectInspectorFrame.ProjectExplorerExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  Document: TDocumentID;
begin

  if (Node.HasChildren and (Node.GetFirstChild = nil)) then
  begin
    Document := (Node as TProjectInspectorNode).DocumentID;
    Project.ListDocuments(Document);
    // also need to reload the properties to get the right sort order.
    //Project.ListDocuments(Document,Node,@ProjectDocumentsListed,@ProjectDocumentsError);
    (Node as TProjectInspectorNode).ExpandOnList := true;
  end;
end;

procedure TProjectInspectorFrame.ObserveMainForm(aAction: TMainFormAction;
  aDocument: TDocumentID);
begin
  case aAction of
    mfaDocumentsListed:
    begin
      ReloadNodeForDocument(aDocument);
    end;
  end;
end;

procedure TProjectInspectorFrame.ProjectDocumentsError(Data: String);
begin
  ShowMessage('An error occurred while refreshin the project inspector.' + LineEnding +
              Data + LineEnding +
              'You may wish to wait a little bit and try again later.');
end;

procedure TProjectInspectorFrame.ProjectExplorerCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TProjectInspectorNode;
end;

procedure TProjectInspectorFrame.ProjectExplorerDblClick(Sender: TObject);
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

function TProjectInspectorFrame.GetProject: TStewProject;
begin
  result := MainForm.Project;
end;

procedure TProjectInspectorFrame.ReloadNodeForDocument(aDocument: TDocumentID);
var
  aNode: TProjectInspectorNode;
begin
  if aDocument <> RootDocument then
  begin
     aNode := GetTreeNodeForDocument(aDocument);
     ReloadNode(aNode);
  end
  else
     ReloadNode(nil);
  // else, I don't care about this document because I haven't loaded it yet.
end;

procedure TProjectInspectorFrame.ReloadNode(aNode: TProjectInspectorNode);
var
  aList: TDocumentList;
  i: Integer;
  aChild: TProjectInspectorNode;
  aMatch: TProjectInspectorNode;
begin

  ProjectExplorer.BeginUpdate;
  try
    if aNode <> nil then
    begin
      aList := MainForm.Project.GetDocumentList(aNode.DocumentID);
      aChild := aNode.GetFirstChild as TProjectInspectorNode;

    end
    else
    begin
      aList := MainForm.Project.GetDocumentList(RootDocument);
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
        aChild.DocumentID:=aList[i];
        achild.HasChildren:=true;
      end;

      // go on to the next sibling and the next item in the list.
      aChild := aChild.GetNextSibling as TProjectInspectorNode;

    end;

    // if achild is not nil then we reached the end of the list
    // and there are still some extra nodes left.
    if aChild <> nil then
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

function TProjectInspectorFrame.GetTreeNodeForDocument(aDocument: TDocumentID): TProjectInspectorNode;
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

constructor TProjectInspectorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  MainForm.Observe(@ObserveMainForm);
  Text := 'Project';
end;

destructor TProjectInspectorFrame.Destroy;
begin
  MainForm.Unobserve(@ObserveMainForm);
  inherited Destroy;
end;

procedure TProjectInspectorFrame.RefreshProject;
begin
  ProjectExplorer.Items.Clear;
  if (Project <> nil) then
  begin
    Project.ListDocuments(RootDocument);
  end;
end;

end.

