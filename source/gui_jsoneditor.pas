unit gui_jsoneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  Menus, ComCtrls, fpjson;

type

  { TJSONTreeNode }

  TJSONTreeNode = class(TTreeNode)
  private
    FDataType: TJSONtype;
    function GetAsBoolean: Boolean;
    function GetAsNumber: Double;
    function GetAsString: String;
    function GetKey: String;
    function GetTextValue: String;
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsNumber(AValue: Double);
    procedure SetAsString(AValue: String);
    procedure SetDataType(AValue: TJSONtype);
    procedure ExtractKeyAndValue(out aKey: String; out aValue: String);
    procedure SetKey(AValue: String);
    procedure SetKeyAndValue(const aKey: String; const aValue: String);
    procedure SetTextValue(const aValue: String);
    function IsValidTextValue(const aValue: String): Boolean;
    function IsValidKey(const aValue: String): Boolean;
  public
    property DataType: TJSONtype read fDataType write SetDataType;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsNumber: Double read GetAsNumber write SetAsNumber;
    property AsString: String read GetAsString write SetAsString;
    property Key: String read GetKey;
    property TextValue: String read GetTextValue;
    function IsProperty: Boolean;
    procedure Edited(var NewText: String);
    procedure Editing(var AllowEdit: Boolean);
    procedure SetJSON(aJSON: TJSONData);
    function CreateJSON: TJSONData;
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
    AddNullButton: TToolButton;
    procedure DeleteElementButtonClick(Sender: TObject);
    procedure JSONTreeCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure JSONTreeEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure JSONTreeEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure JSONTreeSelectionChanged(Sender: TObject);
    procedure AddArrayButtonClick(Sender: TObject);
    procedure AddBooleanButtonClick(Sender: TObject);
    procedure AddNullButtonClick(Sender: TObject);
    procedure AddNumberButtonClick(Sender: TObject);
    procedure AddObjectButtonClick(Sender: TObject);
    procedure AddStringButtonClick(Sender: TObject);
  private
    { private declarations }
    procedure AddNode(aDataType: TJSONtype);
    procedure EnableDisable;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SetJSON(aData: TJSONData);
    function CreateJSON: TJSONData;
  end;

implementation

uses
  Dialogs;

const
  TrueCaption: String = 'true';
  FalseCaption: String = 'false';

{$R *.lfm}

{ TJSONTreeNode }

procedure TJSONTreeNode.SetDataType(AValue: TJSONtype);
begin
  if FDataType=AValue then Exit;

  // clear all the data from the old data type
  DeleteChildren;
  FDataType := AValue;
  ImageIndex := ord(FDataType);
  case FDataType of
    jtBoolean:
      AsBoolean := false;
    jtNumber:
      AsNumber := 0;
    jtString:
      AsString := '';
  else
    SetTextValue('');
  end;
end;

function CorrectTypeText(aType: TJSONType; const aValue: String): String;
begin
  case aType of
    jtArray:
      result := '<array>';
    jtObject:
      result := '<object>';
    jtNull:
      result := '<null>';
    jtUnknown:
      result := '<undefined>';
  else
    result := aValue;
  end;

end;

function BuildKeyAndValue(aType: TJSONtype; const aKey: String; const aValue: String): String;
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
    jtArray, jtUnknown, jtNull, jtObject:
      // accepts any value, but it will just be switched back to the type. This
      // makes it possible to change the key.
      result := false;
    jtBoolean:
      result := TryStrToBool(aValue,aDummy1);
    jtNumber:
      result := TryStrToFloat(aValue,aDummy2);
    jtString:
      result := true;
  end;
end;

function TJSONTreeNode.IsValidKey(const aValue: String): Boolean;
var
  aSibling: TJSONTreeNode;
begin
  result := true;
  if IsProperty then
  begin
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
  result := (aParent <> nil) and (aParent.DataType = jtObject);
end;

procedure TJSONTreeNode.Edited(var NewText: String);
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
      if FDataType in [jtArray,jtNull,jtObject,jtUnknown] then
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
       ((FDataType in [jtArray,jtNull,jtObject,jtUnknown]) or
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

end;

procedure TJSONTreeNode.Editing(var AllowEdit: Boolean);
begin
  AllowEdit := (DataType in [jtBoolean,jtNumber,jtString]) or IsProperty;
end;

procedure TJSONTreeNode.SetJSON(aJSON: TJSONData);
var
  i: Integer;
  aChild: TJSONTreeNode;
begin
  DataType := aJSON.JSONType;
  DeleteChildren;
  case aJSON.JSONType of
    jtObject:
      begin

        for i := 0 to aJSON.Count -1 do
        begin
          aChild := TreeNodes.AddChild(Self,'') as TJSONTreeNode;
          aChild.SetKey((aJSON as TJSONObject).Names[i]);
          aChild.SetJSON(aJSON.Items[i]);
        end;
        Expanded := true;
      end;
    jtArray:
      begin
        for i := 0 to aJSON.Count -1 do
        begin
          aChild := TreeNodes.AddChild(Self,'') as TJSONTreeNode;
          aChild.SetJSON(aJSON.Items[i]);
        end;
        Expanded := true;
      end;
    jtNumber:
      AsNumber := aJSON.AsFloat;
    jtString:
      AsString := aJSON.AsString;
    jtBoolean:
      AsBoolean := aJSON.AsBoolean;
  end;
end;

function TJSONTreeNode.CreateJSON: TJSONData;
var
  rObject: TJSONObject;
  rArray: TJSONArray;
  aChild: TJSONTreeNode;
begin
  case FDataType of
    jtUnknown:
      ;
    jtNull:
      result := fpjson.CreateJSON;
    jtBoolean:
      result := fpjson.CreateJSON(AsBoolean);
    jtNumber:
      result := fpjson.CreateJSON(AsNumber);
    jtString:
      result := fpjson.CreateJSON(AsString);
    jtArray:
      begin
        rArray := fpjson.CreateJSONArray([]);
        result := rArray;
        aChild := GetFirstChild as TJSONTreeNode;
        while aChild <> nil do
        begin
          rArray.Add(aChild.CreateJSON);
          aChild := aChild.GetNextSibling as TJSONTreeNode;
        end;
      end;
    jtObject:
      begin
        rObject := fpjson.CreateJSONObject([]);
        result := rObject;
        aChild := GetFirstChild as TJSONTreeNode;
        while aChild <> nil do
        begin
          rObject.Add(aChild.GetKey,aChild.CreateJSON);
          aChild := aChild.GetNextSibling as TJSONTreeNode;
        end;
      end;
  end;
end;

function TJSONTreeNode.GetAsBoolean: Boolean;
begin
  case FDataType of
    jtNumber:
      result := TextValue <> '0';
    jtString:
      result := TextValue <> '';
    jtBoolean:
      result := TextValue = TrueCaption;
    jtNull, jtUnknown:
      result := false;
    jtObject, jtArray:
      result := true;
  end;
end;

function TJSONTreeNode.GetAsNumber: Double;
begin
  case FDataType of
    jtNumber, jtString:
      if not TryStrToFloat(TextValue,result) then
         result := 0;
    jtBoolean:
      if TextValue = TrueCaption then
        result := 1
      else
        result := 0;
    jtNull, jtUnknown, jtObject, jtArray:
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
    jtNumber:
      AsNumber := ord(AValue);
    jtString:
      AsString := BoolToStr(aValue,TrueCaption,FalseCaption);
    jtBoolean:
      SetTextValue(BoolToStr(aValue,TrueCaption,FalseCaption));
    jtNull, jtUnknown,jtObject,jtArray:
      // nothing
  end;
end;

procedure TJSONTreeNode.SetAsNumber(AValue: Double);
begin
  case FDataType of
    jtNumber:
      SetTextValue(FloatToStr(AValue));
    jtString:
      AsString := FloatToStr(AValue);
    jtBoolean:
      AsBoolean := aValue <> 0;
    jtNull, jtUnknown, jtObject, jtArray:
      // nothing
  end;
end;

procedure TJSONTreeNode.SetAsString(AValue: String);
begin
  case FDataType of
    jtNumber:
      AsNumber := StrToFloat(AValue);
    jtBoolean:
      AsBoolean := StrToBool(aValue);
    jtString:
      SetTextValue(aValue);
    jtNull, jtUnknown, jtObject, jtArray:
      // nothing;
  end;
end;

{ TJSONEditor }

procedure TJSONEditor.AddArrayButtonClick(Sender: TObject);
begin
  AddNode(jtArray);
end;

procedure TJSONEditor.AddBooleanButtonClick(Sender: TObject);
begin
  AddNode(jtBoolean);
end;

procedure TJSONEditor.AddNullButtonClick(Sender: TObject);
begin
  AddNode(jtNull);
end;

procedure TJSONEditor.AddNumberButtonClick(Sender: TObject);
begin
  AddNode(jtNumber);
end;

procedure TJSONEditor.AddObjectButtonClick(Sender: TObject);
begin
  AddNode(jtObject);
end;

procedure TJSONEditor.AddStringButtonClick(Sender: TObject);
begin
  AddNode(jtString);
end;

procedure TJSONEditor.DeleteElementButtonClick(Sender: TObject);
begin
  if JSONTree.Selected <> nil then
  begin
    if MessageDlg('Are you sure you want to delete this data?',mtConfirmation,mbYesNo,0) = mrYes then
    begin
      JSONTree.Selected.Delete;
      EnableDisable;
    end;
  end;
  // Else delete button shouldn't even be enabled.
end;

procedure TJSONEditor.JSONTreeCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass := TJSONTreeNode;
end;

procedure TJSONEditor.JSONTreeEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  (Node as TJSONTreeNode).Edited(S);
end;

procedure TJSONEditor.JSONTreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  (Node as TJSONTreeNode).Editing(AllowEdit);
end;

procedure TJSONEditor.JSONTreeSelectionChanged(Sender: TObject);
begin
  EnableDisable;
end;

procedure TJSONEditor.AddNode(aDataType: TJSONtype);
var
  aNew: TJSONTreeNode;
  aParent: TJSONTreeNode;
begin
  if (JSONTree.Selected = nil) then
  begin
    if (JSONTree.Items.Count = 0) then
    begin
      aNew := JSONTree.Items.AddChild(nil,'') as TJSONTreeNode;
      aNew.DataType := aDataType;
      aNew.EditText;
    end
  end
  else
  begin
    if (JSONTree.Selected as TJSONTreeNode).DataType in [jtObject,jtArray] then
      aParent := JSONTree.Selected as TJSONTreeNode
    else
      aParent := JSONTree.Selected.Parent as TJSONTreeNode;
    if aParent <> nil then
    begin
      aNew := JSONTree.Items.AddChild(aParent,'') as TJSONTreeNode;
      aNew.DataType := aDataType;
      aParent.Expanded := true;
      aNew.EditText;
    end;
  end;
  EnableDisable;

end;

procedure TJSONEditor.EnableDisable;
var
  aSelected: TJSONTreeNode;
  aParent: TJSONTreeNode;
  enableAdd: Boolean;
  enableDelete: Boolean;
begin
  aSelected := JSONTree.Selected as TJSONTreeNode;

  enableDelete := (aSelected <> nil);
  enableAdd := false;
  if aSelected <> nil then
  begin
    enableDelete := true;
    if aSelected.DataType in [jtObject,jtArray] then
      enableAdd := true
    else
    begin
      aParent := aSelected.Parent as TJSONTreeNode;
      if (aParent <> nil) and (aParent.DataType in [jtObject,jtArray]) then
        enableAdd := true;
    end;
  end
  else if JSONTree.Items.Count = 0 then
     enableAdd := true;

  DeleteElementButton.Enabled := enableDelete;
  AddBooleanButton.Enabled := enableAdd;
  AddNullButton.Enabled := enableAdd;
  AddNumberButton.Enabled := enableAdd;
  AddObjectButton.Enabled := enableAdd;
  AddStringButton.Enabled := enableAdd;
  AddArrayButton.Enabled := enableAdd;
end;

constructor TJSONEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  EnableDisable;
end;

procedure TJSONEditor.SetJSON(aData: TJSONData);
var
  aRoot: TJSONTreeNode;
begin
  JSONTree.Items.Clear;
  if aData <> nil then
  begin
    aRoot := JSONTree.Items.AddChild(nil,'') as TJSONTreeNode;
    aRoot.SetJSON(aData);
  end;
end;

function TJSONEditor.CreateJSON: TJSONData;
var
  aRoot: TJSONTreeNode;
begin
  aRoot := JSONTree.Items.GetFirstNode as TJSONTreeNode;
  if aRoot <> nil then
    result := aRoot.CreateJSON
  else
    result := nil;
end;

end.

