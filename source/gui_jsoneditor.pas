unit gui_jsoneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  Menus, ComCtrls, sys_dynval;

type

  { TJSONTreeNode }

  TJSONTreeNode = class(TTreeNode)
  strict private
    FDataType: TDynamicValueKind;
    function GetAsBoolean: Boolean;
    function GetAsNumber: Double;
    function GetAsString: String;
    function GetKey: String;
    function GetTextValue: String;
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsNumber(AValue: Double);
    procedure SetAsString(AValue: String);
    procedure SetDataType(AValue: TDynamicValueKind);
    procedure ExtractKeyAndValue(out aKey: String; out aValue: String);
    procedure SetKeyAndValue(const aKey: String; const aValue: String);
    procedure SetTextValue(const aValue: String);
    function IsValidTextValue(const aValue: String): Boolean;
    function IsValidKey(const aValue: String): Boolean;
  private
    procedure SetKey(AValue: String);
  public
    property DataType: TDynamicValueKind read fDataType write SetDataType;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsNumber: Double read GetAsNumber write SetAsNumber;
    property AsString: String read GetAsString write SetAsString;
    property Key: String read GetKey;
    property TextValue: String read GetTextValue;
    function IsProperty: Boolean;
    function Edited(var NewText: String): Boolean;
    procedure Editing(var AllowEdit: Boolean);
    procedure SetValue(aJSON: IDynamicValue);
    function GetValue: IDynamicValue;

  end;

  { TJSONEditor }

  TJSONEditor = class(TFrame)
    JSONTree: TTreeView;
    EditToolbar: TToolBar;
    DeleteElementButton: TToolButton;
    ToolButton1: TToolButton;
    AddObjectButton: TToolButton;
    AddArrayButton: TToolButton;
    AddBooleanButton: TToolButton;
    AddNumberButton: TToolButton;
    AddStringButton: TToolButton;
    procedure DeleteElementButtonClick(Sender: TObject);
    procedure JSONTreeAddition(Sender: TObject; {%H-}Node: TTreeNode);
    procedure JSONTreeCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure JSONTreeDeletion(Sender: TObject; {%H-}Node: TTreeNode);
    procedure JSONTreeEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure JSONTreeEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure JSONTreeMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure JSONTreeSelectionChanged(Sender: TObject);
    procedure AddArrayButtonClick(Sender: TObject);
    procedure AddBooleanButtonClick(Sender: TObject);
    procedure AddNullButtonClick(Sender: TObject);
    procedure AddNumberButtonClick(Sender: TObject);
    procedure AddObjectButtonClick(Sender: TObject);
    procedure AddStringButtonClick(Sender: TObject);
  strict private
    { private declarations }
    fModified: Boolean;
    procedure AddNode(aDataType: TDynamicValueKind);
    procedure EnableDisable;
    function GetModified: Boolean;
    procedure SetModified(AValue: Boolean);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SetMap(aData: IDynamicMap);
    function GetMap: IDynamicMap;
    property Modified: Boolean read GetModified write SetModified;
  end;

  procedure SetTreeNodeValue(aParent: TTreeNodes; aNode: TJSONTreeNode; aJSON: IDynamicValue);


implementation

uses
  Dialogs;

const
  TrueCaption: String = 'yes';
  FalseCaption: String = 'no';

procedure SetTreeNodeValue(aParent: TTreeNodes; aNode: TJSONTreeNode; aJSON: IDynamicValue);
var
  i: Integer;
  l: Integer;
  aChild: TJSONTreeNode;
  lList: IDynamicList;
  lEnumerator: IDynamicMapEnumerator;
begin
  if aNode <> nil then
  begin
    aNode.DataType := aJSON.KindOf;
    aNode.DeleteChildren;
  end;
  case aJSON.KindOf of
    dvkList:
      begin
        lList := aJSON as IDynamicList;
        l := lList.Length;
        for i := 0 to l - 1 do
        begin
          aChild := aParent.AddChild(aNode,'') as TJSONTreeNode;
          aChild.SetKey(IntToStr(i));
          aChild.SetValue(lList[i]);
        end;

      end;
    dvkMap:
      begin
        lEnumerator := (aJSON as IDynamicMap).Enumerate;
        while lEnumerator.Next do
        begin
          aChild := aParent.AddChild(aNode,'') as TJSONTreeNode;
          aChild.SetKey(lEnumerator.Key);
          aChild.SetValue(lEnumerator.Value);
        end;
        if aNode <> nil then
           aNode.Expanded := true;
      end;
    dvkNumber:
      if aNode <> nil then
         aNode.AsNumber := (aJSON as IDynamicNumber).Value;
    dvkString:
      if aNode <> nil then
         aNode.AsString := (aJSON as IDynamicString).Value;
    dvkBoolean:
      if aNode <> nil then
         aNode.AsBoolean := (aJSON as IDynamicBoolean).Value;
  else
    // setting the data type should have set the default value...
  end;
end;

{$R *.lfm}

{ TJSONTreeNode }

procedure TJSONTreeNode.SetDataType(AValue: TDynamicValueKind);
begin
  if FDataType=AValue then Exit;

  // clear all the data from the old data type
  DeleteChildren;
  FDataType := AValue;
  ImageIndex := ord(AValue);
  case AValue of
    dvkBoolean:
      AsBoolean := false;
    dvkNumber:
      AsNumber := 0;
    dvkString:
      AsString := '';
  else
    // everything else will be corrected by type.
    SetTextValue('');
  end;
end;

function CorrectTypeText(aClass: TDynamicValueKind; const aValue: String): String;
begin
  case aClass of
    dvkList:
      result := '<list>';
    dvkMap:
      result := '<map>';
    dvkNull:
      result := '<missing>';
    dvkUndefined:
      result := '<unassigned>';
    dvkObject:
      result := '<internal data>';
    dvkBoolean, dvkNumber, dvkString:
      result := aValue;
  else
    raise Exception.Create('Unknown dynamic value kind: ' + GUIDToString(DynamicValueKinds[aClass]));
  end;

end;

function BuildKeyAndValue(aType: TDynamicValueKind; const aKey: String; const aValue: String): String;
begin
  result := aKey + ': ' + CorrectTypeText(aType,aValue);
end;

function ExtractKeyAndValue(const Text: String; out aKey: String; out aValue: String): Boolean;
var
  aIndex: Integer;
begin
  aIndex := Pos(':',Text);
  if aIndex > 0 then
  begin
    aKey := Copy(Text,1,aIndex - 1);
    aValue := Trim(Copy(Text,aIndex + 1,Length(Text)));
    result := true;
  end
  else
  begin
    aKey := '';
    aValue := Text;
    result := false;
  end;

end;


procedure TJSONTreeNode.ExtractKeyAndValue(out aKey: String; out aValue: String
  );
var
  aText: String;
begin
  aText := Text;
  if IsProperty then
  begin
    if gui_jsoneditor.ExtractKeyAndValue(aText,aKey,aValue) then
      Exit;
  end;
  // if it's not a property, or if a ':' was not found, then
  // the key is the index.
  aKey := IntToStr(Index);
  aValue := aText;
end;

procedure TJSONTreeNode.SetKey(AValue: String);
begin
  SetKeyAndValue(AValue,GetTextValue);
end;

procedure TJSONTreeNode.SetKeyAndValue(const aKey: String; const aValue: String
  );
begin
  if IsProperty then
    Text := BuildKeyAndValue(DataType,aKey,aValue)
  else
    Text := CorrectTypeText(DataType,aValue);
end;

procedure TJSONTreeNode.SetTextValue(const aValue: String);
begin
  SetKeyAndValue(GetKey,aValue);
end;

function TJSONTreeNode.IsValidTextValue(const aValue: String): Boolean;
var
  aDummy1: Boolean;
  aDummy2: Double;
begin
  case FDataType of
    dvkBoolean:
      result := TryStrToBool(aValue,aDummy1);
    dvkNumber:
      result := TryStrToFloat(aValue,aDummy2);
    dvkString:
      result := true;
  else
    // accepts any value, but it will just be switched back to the type. This
    // makes it possible to change the key.
    result := false;
  end;
end;

function TJSONTreeNode.IsValidKey(const aValue: String): Boolean;
var
  aSibling: TJSONTreeNode;
begin
  result := true;
  if IsProperty then
  begin
    if Parent = nil then
       aSibling := TreeNodes.GetFirstNode as TJSONTreeNode
    else
       aSibling := Parent.GetFirstChild as TJSONTreeNode;
    while (aSibling <> nil) do
    begin
      if (aSibling <> Self) and (aSibling.Key = aValue) then
      begin
        result := false;
        exit;
      end;
      aSibling := aSibling.GetNextSibling as TJSONTreeNode;
    end;
  end;
end;

function TJSONTreeNode.IsProperty: Boolean;
var
  aParent: TJSONTreeNode;
begin
  aParent := Parent as TJSONTreeNode;
  result := (aParent = nil) or ((aParent.DataType = dvkMap));
end;

function TJSONTreeNode.Edited(var NewText: String): Boolean;
var
  aKey: String;
  aValue: String;
begin
  if IsProperty then
  begin
    if not gui_jsoneditor.ExtractKeyAndValue(NewText,aKey,aValue) then
    begin
      // the user didn't assign a property name, so we're going to make
      // some assumptions here:
      if FDataType in [dvkNull,dvkMap,dvkList,dvkUndefined] then
      begin
        // if this is a non-editable value, then assume the change was
        // supposed to be the property name.
        ExtractKeyAndValue(aKey,aValue);
        aKey := NewText;
      end
      else
      begin
        // otherwise, assume the user was just changing the value.
        ExtractKeyAndValue(aKey,aValue);
        aValue := NewText;
      end;
    end;
    // now, since the value is being changed, make sure it's valid,
    // and if so, make sure the new text has the key in there.
    if IsValidKey(aKey) and
       ((FDataType in [dvkNull,dvkMap,dvkList,dvkUndefined]) or
        IsValidTextValue(aValue)) then
       NewText := BuildKeyAndValue(FDataType,aKey,aValue)
    else
      NewText := Text;
  end
  else
  begin
    if not IsValidTextValue(NewText) then
      NewText := Text;
  end;
  result := NewText <> Text;

end;

procedure TJSONTreeNode.Editing(var AllowEdit: Boolean);
begin
  AllowEdit := (DataType in [dvkBoolean,dvkNumber,dvkString]) or IsProperty;
end;

procedure TJSONTreeNode.SetValue(aJSON: IDynamicValue);
begin
  SetTreeNodeValue(TreeNodes,Self,aJSON);
end;

function TJSONTreeNode.GetValue: IDynamicValue;
var
  rObject: IDynamicMap;
  rArray: IDynamicList;
  aChild: TJSONTreeNode;
begin
  case FDataType of
    dvkNull:
      result := TDynamicValues.Null;
    dvkBoolean:
      result := TDynamicValues.Boolean(AsBoolean);
    dvkNumber:
      result := TDynamicValues.NewNumber(AsNumber);
    dvkString:
      result := TDynamicValues.NewString(AsString);
    dvkList:
      begin
        rArray := TDynamicValues.NewList;
        result := rArray;
        aChild := GetFirstChild as TJSONTreeNode;
        while aChild <> nil do
        begin
          rArray.Add(aChild.GetValue);
          aChild := aChild.GetNextSibling as TJSONTreeNode;
        end;
      end;
    dvkMap:
      begin
        rObject := TDynamicValues.NewMap;
        result := rObject;
        aChild := GetFirstChild as TJSONTreeNode;
        while aChild <> nil do
        begin
          rObject.SetItem(aChild.GetKey,aChild.GetValue);
          aChild := aChild.GetNextSibling as TJSONTreeNode;
        end;
      end;
  else
    // undefined or some other data type which I don't know...
    ;
  end;
end;

function TJSONTreeNode.GetAsBoolean: Boolean;
begin
  case FDataType of
    dvkNumber:
      result := TextValue <> '0';
    dvkString:
      result := TextValue <> '';
    dvkBoolean:
      result := TextValue = TrueCaption;
    dvkMap,dvkList:
      result := true;
  else
    // everything else (undefined, null, etc.) returns false
      result := false;
  end;
end;

function TJSONTreeNode.GetAsNumber: Double;
begin
  case FDataType of
    dvkNumber, dvkString:
      if not TryStrToFloat(TextValue,result) then
         result := 0;
    dvkBoolean:
      if TextValue = TrueCaption then
        result := 1
      else
        result := 0;
  else
    // everything else returns 0.
      result := 0;
  end;

end;

function TJSONTreeNode.GetAsString: String;
begin
  result := TextValue;
end;

function TJSONTreeNode.GetKey: String;
var
  aDummy: String;
begin
  ExtractKeyAndValue(result,aDummy);
end;

function TJSONTreeNode.GetTextValue: String;
var
  aDummy: String;
begin
  ExtractKeyAndValue(aDummy,result);
end;

procedure TJSONTreeNode.SetAsBoolean(AValue: Boolean);
begin
  case FDataType of
    dvkNumber:
      AsNumber := ord(AValue);
    dvkString:
      AsString := BoolToStr(aValue,TrueCaption,FalseCaption);
    dvkBoolean:
      SetTextValue(BoolToStr(aValue,TrueCaption,FalseCaption));
  else
    // nothing
  end;
end;

procedure TJSONTreeNode.SetAsNumber(AValue: Double);
begin
  case FDataType of
    dvkNumber:
      SetTextValue(FloatToStr(AValue));
    dvkString:
      AsString := FloatToStr(AValue);
    dvkBoolean:
      AsBoolean := aValue <> 0;
  else
      // nothing
  end;
end;

procedure TJSONTreeNode.SetAsString(AValue: String);
begin
  case FDataType of
    dvkNumber:
      AsNumber := StrToFloat(AValue);
    dvkBoolean:
      AsBoolean := StrToBool(aValue);
    dvkString:
      SetTextValue(aValue);
  else
      // nothing;
  end;
end;

{ TJSONEditor }

procedure TJSONEditor.AddArrayButtonClick(Sender: TObject);
begin
  AddNode(dvkList);
end;

procedure TJSONEditor.AddBooleanButtonClick(Sender: TObject);
begin
  AddNode(dvkBoolean);
end;

procedure TJSONEditor.AddNullButtonClick(Sender: TObject);
begin
  AddNode(dvkNull);
end;

procedure TJSONEditor.AddNumberButtonClick(Sender: TObject);
begin
  AddNode(dvkNumber);
end;

procedure TJSONEditor.AddObjectButtonClick(Sender: TObject);
begin
  AddNode(dvkMap);
end;

procedure TJSONEditor.AddStringButtonClick(Sender: TObject);
begin
  AddNode(dvkString);
end;

procedure TJSONEditor.DeleteElementButtonClick(Sender: TObject);
begin
  if JSONTree.Selected <> nil then
  begin
    if MessageDlg('Are you sure you want to delete this data?',mtConfirmation,mbYesNo,0) = mrYes then
    begin
      JSONTree.Selected.Delete;
      fModified := true;
      EnableDisable;
    end;
  end;
  // Else delete button shouldn't even be enabled.
end;

procedure TJSONEditor.JSONTreeAddition(Sender: TObject; Node: TTreeNode);
begin
  fModified := true;
end;


procedure TJSONEditor.JSONTreeCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TJSONTreeNode;
end;

procedure TJSONEditor.JSONTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  fModified := true;
end;

procedure TJSONEditor.JSONTreeEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  if (Node as TJSONTreeNode).Edited(S) then
     fModified := true;
end;

procedure TJSONEditor.JSONTreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  (Node as TJSONTreeNode).Editing(AllowEdit);
end;

procedure TJSONEditor.JSONTreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  lNode: TTreeNode;
begin
  lNode := JSONTree.GetNodeAt(x,y);
  if lNode = nil then
     JSONTree.Selected := nil;
end;

procedure TJSONEditor.JSONTreeSelectionChanged(Sender: TObject);
begin
  EnableDisable;
end;

procedure TJSONEditor.AddNode(aDataType: TDynamicValueKind);
var
  aNew: TJSONTreeNode;
  aParent: TJSONTreeNode;
begin
  if (JSONTree.Selected = nil) then
    aParent := nil
  else if (JSONTree.Selected as TJSONTreeNode).DataType in [dvkMap,dvkList] then
    aParent := JSONTree.Selected as TJSONTreeNode
  else
    aParent := JSONTree.Selected.Parent as TJSONTreeNode;

  aNew := JSONTree.Items.AddChild(aParent,'') as TJSONTreeNode;
  aNew.DataType := aDataType;

  if aParent <> nil then
    aParent.Expanded := true;

  aNew.EditText;
  EnableDisable;

end;

procedure TJSONEditor.EnableDisable;
var
  aSelected: TJSONTreeNode;
  enableAdd: Boolean;
  enableDelete: Boolean;
begin
  aSelected := JSONTree.Selected as TJSONTreeNode;

  enableDelete := (aSelected <> nil);
  enableAdd := true;

  DeleteElementButton.Enabled := enableDelete;
  AddBooleanButton.Enabled := enableAdd;
  AddNumberButton.Enabled := enableAdd;
  AddObjectButton.Enabled := enableAdd;
  AddStringButton.Enabled := enableAdd;
  AddArrayButton.Enabled := enableAdd;
end;

function TJSONEditor.GetModified: Boolean;
begin
  result := fModified;
end;

procedure TJSONEditor.SetModified(AValue: Boolean);
begin
  fModified := AValue;
end;

constructor TJSONEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EnableDisable;
  fModified := false;
end;

procedure TJSONEditor.SetMap(aData: IDynamicMap);
var
  lMap: IDynamicMap;
begin
  JSONTree.Items.Clear;
  if aData <> nil then
  begin
     lMap := aData as IDynamicMap;
     SetTreeNodeValue(JSONTree.Items,nil,lMap);
     fModified := true;
  end
  else
  begin
    fModified := false;
  end;
end;

function TJSONEditor.GetMap: IDynamicMap;
var
  lChild: TJSONTreeNode;
begin
  lChild := JSONTree.Items.GetFirstNode as TJSONTreeNode;
  if lChild <> nil then
  begin
    result := TDynamicValues.NewMap;
    while lChild <> nil do
    begin
      result.SetItem(lChild.Key,lChild.GetValue);
      lChild := lChild.GetNextSibling as TJSONTreeNode;
    end;
  end
  else
    result := nil;
end;

end.

