unit gui_jsoneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  Menus, ComCtrls, sys_json;

type

  { TJSONTreeNode }

  TJSONTreeNode = class(TTreeNode)
  strict private
    FDataType: TJSValueClass;
    function GetAsBoolean: Boolean;
    function GetAsNumber: Double;
    function GetAsString: String;
    function GetKey: String;
    function GetTextValue: String;
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsNumber(AValue: Double);
    procedure SetAsString(AValue: String);
    procedure SetDataType(AValue: TJSValueClass);
    procedure ExtractKeyAndValue(out aKey: String; out aValue: String);
    procedure SetKeyAndValue(const aKey: String; const aValue: String);
    procedure SetTextValue(const aValue: String);
    function IsValidTextValue(const aValue: String): Boolean;
    function IsValidKey(const aValue: String): Boolean;
  private
    procedure SetKey(AValue: String);
  public
    property DataType: TJSValueClass read fDataType write SetDataType;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsNumber: Double read GetAsNumber write SetAsNumber;
    property AsString: String read GetAsString write SetAsString;
    property Key: String read GetKey;
    property TextValue: String read GetTextValue;
    function IsProperty: Boolean;
    function Edited(var NewText: String): Boolean;
    procedure Editing(var AllowEdit: Boolean);
    procedure SetJSON(aJSON: TJSValue);
    function AddToJSON(aParent: TJSValue; aKey: UTF8String): TJSValue;
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
    procedure AddNode(aDataType: TJSValueClass);
    procedure EnableDisable;
    function GetModified: Boolean;
    procedure SetModified(AValue: Boolean);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SetJSON(aData: TJSObject);
    function CreateJSON2: TJSObject;
    property Modified: Boolean read GetModified write SetModified;
  end;

implementation

uses
  Dialogs, sys_types;

const
  TrueCaption: String = 'yes';
  FalseCaption: String = 'no';

{$R *.lfm}

{ TJSONTreeNode }

procedure TJSONTreeNode.SetDataType(AValue: TJSValueClass);
begin
  if FDataType=AValue then Exit;

  // clear all the data from the old data type
  DeleteChildren;
  FDataType := AValue;
  ImageIndex := ord(AValue.GetTypeOf);
  case AValue.GetTypeOf of
    jstBoolean:
      AsBoolean := false;
    jstNumber:
      AsNumber := 0;
    jstString:
      AsString := '';
  else
    SetTextValue('');
  end;
end;

function CorrectTypeText(aClass: TJSValueClass; const aValue: String): String;
begin
  if aClass = nil then
    result := aValue
  else
  begin
    case aClass.GetTypeOf of
      jstObject:
        if aClass.InheritsFrom(TJSArray) then
           result := '<array>'
        else
           result := '<object>';
      jstNull:
        result := '<null>';
      jstUndefined:
        result := '<undefined>';
    else
      result := aValue;
    end;
  end;

end;

function BuildKeyAndValue(aType: TJSValueClass; const aKey: String; const aValue: String): String;
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
  case FDataType.GetTypeOf of
    jstUndefined, jstNull, jstObject:
      // accepts any value, but it will just be switched back to the type. This
      // makes it possible to change the key.
      result := false;
    jstBoolean:
      result := TryStrToBool(aValue,aDummy1);
    jstNumber:
      result := TryStrToFloat(aValue,aDummy2);
    jstString:
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
  result := (aParent = nil) or ((aParent.DataType.GetTypeOf = jstObject) and (not aParent.DataType.InheritsFrom(TJSArray)));
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
      if FDataType.GetTypeOf in [jstNull,jstObject,jstUndefined] then
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
       ((FDataType.GetTypeOf in [jstNull,jstObject,jstUndefined]) or
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
  AllowEdit := (DataType.GetTypeOf in [jstBoolean,jstNumber,jstString]) or IsProperty;
end;

procedure TJSONTreeNode.SetJSON(aJSON: TJSValue);
var
  i: Integer;
  l: Integer;
  aChild: TJSONTreeNode;
  lKeys: TStringArray;
begin
  DataType := TJSValueClass(aJSON.ClassType);
  DeleteChildren;
  case DataType.GetTypeOf of
    jstObject:
      if DataType.InheritsFrom(TJSArray) then
      begin
        l := trunc(aJSON.Get('length').AsNumber);
        for i := 0 to l - 1 do
        begin
          aChild := TreeNodes.AddChild(Self,'') as TJSONTreeNode;
          aChild.SetKey(IntToStr(i));
          aChild.SetJSON(aJSON.Get(i));
        end;

      end
      else
      begin
        lKeys := aJSON.keys;
        for i := 0 to Length(lKeys) -1 do
        begin
          aChild := TreeNodes.AddChild(Self,'') as TJSONTreeNode;
          aChild.SetKey(lKeys[i]);
          aChild.SetJSON(aJSON.Get(lkeys[i]));
        end;
        Expanded := true;
      end;
    jstNumber:
      AsNumber := aJSON.AsNumber;
    jstString:
      AsString := aJSON.AsString;
    jstBoolean:
      AsBoolean := aJSON.AsBoolean;
  end;
end;

function TJSONTreeNode.AddToJSON(aParent: TJSValue; aKey: UTF8String): TJSValue;
var
  rObject: TJSObject;
  rArray: TJSArray;
  aChild: TJSONTreeNode;
begin
  case FDataType.GetTypeOf of
    jstUndefined:
      ;
    jstNull:
      result := aParent.PutNull(aKey);
    jstBoolean:
      result := aParent.Put(aKey,AsBoolean);
    jstNumber:
      result := aParent.Put(aKey,AsNumber);
    jstString:
      result := aParent.Put(aKey,AsString);
    jstObject:
      begin
        if FDataType.InheritsFrom(TJSArray) then
        begin
          rArray := aParent.PutNewArray(aKey) as TJSArray;
          result := rArray;
          aChild := GetFirstChild as TJSONTreeNode;
          while aChild <> nil do
          begin
            aChild.AddToJSON(rArray,IntToStr(rArray.Length));
            aChild := aChild.GetNextSibling as TJSONTreeNode;
          end;
        end
        else
        begin
          rObject := aParent.PutNewObject(aKey) as TJSObject;
          result := rObject;
          aChild := GetFirstChild as TJSONTreeNode;
          while aChild <> nil do
          begin
            aChild.AddToJSON(rObject,aChild.GetKey);
            aChild := aChild.GetNextSibling as TJSONTreeNode;
          end;
        end;
      end;
  end;
end;

function TJSONTreeNode.GetAsBoolean: Boolean;
begin
  case FDataType.GetTypeOf of
    jstNumber:
      result := TextValue <> '0';
    jstString:
      result := TextValue <> '';
    jstBoolean:
      result := TextValue = TrueCaption;
    jstNull, jstUndefined:
      result := false;
    jstObject:
      result := true;
  end;
end;

function TJSONTreeNode.GetAsNumber: Double;
begin
  case FDataType.GetTypeOf of
    jstNumber, jstString:
      if not TryStrToFloat(TextValue,result) then
         result := 0;
    jstBoolean:
      if TextValue = TrueCaption then
        result := 1
      else
        result := 0;
    jstNull, jstUndefined, jstObject:
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
  case FDataType.GetTypeOf of
    jstNumber:
      AsNumber := ord(AValue);
    jstString:
      AsString := BoolToStr(aValue,TrueCaption,FalseCaption);
    jstBoolean:
      SetTextValue(BoolToStr(aValue,TrueCaption,FalseCaption));
  else
    // nothing
  end;
end;

procedure TJSONTreeNode.SetAsNumber(AValue: Double);
begin
  case FDataType.GetTypeOf of
    jstNumber:
      SetTextValue(FloatToStr(AValue));
    jstString:
      AsString := FloatToStr(AValue);
    jstBoolean:
      AsBoolean := aValue <> 0;
  else
      // nothing
  end;
end;

procedure TJSONTreeNode.SetAsString(AValue: String);
begin
  case FDataType.GetTypeOf of
    jstNumber:
      AsNumber := StrToFloat(AValue);
    jstBoolean:
      AsBoolean := StrToBool(aValue);
    jstString:
      SetTextValue(aValue);
  else
      // nothing;
  end;
end;

{ TJSONEditor }

procedure TJSONEditor.AddArrayButtonClick(Sender: TObject);
begin
  AddNode(TJSArray);
end;

procedure TJSONEditor.AddBooleanButtonClick(Sender: TObject);
begin
  AddNode(TJSBoolean);
end;

procedure TJSONEditor.AddNullButtonClick(Sender: TObject);
begin
  AddNode(TJSNull);
end;

procedure TJSONEditor.AddNumberButtonClick(Sender: TObject);
begin
  AddNode(TJSNumber);
end;

procedure TJSONEditor.AddObjectButtonClick(Sender: TObject);
begin
  AddNode(TJSObject);
end;

procedure TJSONEditor.AddStringButtonClick(Sender: TObject);
begin
  AddNode(TJSString);
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

procedure TJSONEditor.AddNode(aDataType: TJSValueClass);
var
  aNew: TJSONTreeNode;
  aParent: TJSONTreeNode;
begin
  if (JSONTree.Selected = nil) then
    aParent := nil
  else if (JSONTree.Selected as TJSONTreeNode).DataType.GetTypeOf in [jstObject] then
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

procedure TJSONEditor.SetJSON(aData: TJSObject);
var
  lKeys: TStringArray;
  i: Integer;
  lChild: TJSONTreeNode;
begin
  JSONTree.Items.Clear;
  if aData <> nil then
  begin
    lKeys := aData.keys;
    for i := 0 to length(lKeys) - 1 do
    begin
      lChild := JSONTree.Items.AddChild(nil,'') as TJSONTreeNode;
      lChild.SetKey(lKeys[i]);
      lChild.SetJSON(aData.Get(lKeys[i]));
    end;
  end;
  fModified := true;
end;

function TJSONEditor.CreateJSON2: TJSObject;
var
  lChild: TJSONTreeNode;
begin
  lChild := JSONTree.Items.GetFirstNode as TJSONTreeNode;
  if lChild <> nil then
  begin
    result := TJSObject.Create;
    lChild.AddToJSON(Result,lChild.Key);
    lChild := lChild.GetNextSibling as TJSONTreeNode;
  end
  else
    result := nil;
end;

end.

