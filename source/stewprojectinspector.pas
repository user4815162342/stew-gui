unit stewprojectinspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, stewproject, stewfile, stewmainform;

// TODO: I've got to make my own custom tree node classes to store the actual
// document object.

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
  aList: TDocumentList;
  i: Integer;
  aChild: TProjectInspectorNode;
begin
  if aDocument <> RootDocument then
     aNode := GetTreeNodeForDocument(aDocument)
  else
     aNode := nil;
  if (aDocument = RootDocument) or (aNode <> nil) then
  begin
    ProjectExplorer.BeginUpdate;
    try
      aList := MainForm.Project.GetDocumentList(aDocument);
      // TODO: I'd like to replace them in place, so we can keep the expansion
      // state of some nodes.
      // -
      if aNode <> nil then
         aNode.DeleteChildren
      else
         ProjectExplorer.Items.Clear;
      for i := 0 to Length(aList) - 1 do
      begin
        aChild := ProjectExplorer.Items.AddChild(aNode,'') as TProjectInspectorNode;
        aChild.DocumentID:=aList[i];
        achild.HasChildren:=true;
      end;
      if (aNode <> nil) and (aNode.ExpandOnList) then
      begin
        aNode.ExpandOnList := false;
        aNode.Expanded := true;
      end;

    finally
      ProjectExplorer.EndUpdate;
    end;


  end;
  // else, I don't care about these.
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

