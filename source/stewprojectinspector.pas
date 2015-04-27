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
    procedure ObserveMainForm(aAction: TMainFormAction);
    procedure ProjectDocumentsError(Data: String);
    procedure ProjectExplorerCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure ProjectExplorerDblClick(Sender: TObject);
    procedure ProjectExplorerExpanding(Sender: TObject; Node: TTreeNode;
      var {%H-}AllowExpansion: Boolean);
    procedure ProjectDocumentsListed({%H-}aPath: TPacketName; aList: TDocumentList; aTarget: TObject);
  private
    { private declarations }
    function GetProject: TStewProject;
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

type

  { TProjectInspectorNode }

  TProjectInspectorNode = class(TTreeNode)
  private
    FDocumentID: TDocumentID;
    procedure SetDocumentID(AValue: TDocumentID);
  public
    property DocumentID: TDocumentID read FDocumentID write SetDocumentID;
  end;

{ TProjectInspectorNode }

procedure TProjectInspectorNode.SetDocumentID(AValue: TDocumentID);
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
    Project.ListDocuments(Document,Node,@ProjectDocumentsListed,@ProjectDocumentsError);
  end;
end;

procedure TProjectInspectorFrame.ObserveMainForm(aAction: TMainFormAction);
begin
  case aAction of
    mfaProjectRefresh:
      begin
        RefreshProject;
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

procedure TProjectInspectorFrame.ProjectDocumentsListed(aPath: TPacketName;
  aList: TDocumentList; aTarget: TObject);
var
  i: Integer;
  base: TTreeNode;
  node: TProjectInspectorNode;
begin
  if aTarget is TTreeNode then
     base := aTarget as TTreeNode
  else
     base := nil;

  if (Length(aList) > 0) then
  begin

    for i := 0 to Length(aList) - 1 do
    begin
      node := ProjectExplorer.Items.AddChild(base,'') as TProjectInspectorNode;
      node.DocumentID := aList[i];
      node.HasChildren := true;
    end;
  end
  else if base <> nil then
  begin
    base.HasChildren := false;
  end;

end;

function TProjectInspectorFrame.GetProject: TStewProject;
begin
  result := MainForm.Project;
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
    Project.ListRootDocuments(nil,@ProjectDocumentsListed,@ProjectDocumentsError);
  end;
end;

end.

