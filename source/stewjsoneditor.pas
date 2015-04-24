unit stewjsoneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  Menus, ComCtrls, fpjson;

type

  { Towards a better interface:

  I could create a whole bunch of complex controls and the like, but...

  When you "edit" a tree node, you can easily edit the name of the property
  and the value, simply by changing the text on the side of the property. If
  you edit and leave out the ':', then it can easily raise an error, or put
  one in. This is simple.

  Then, we just need a way to specify data types, and also
  setting to null. I can do that with a toolbar to specify the
  type of data, converting as necessary, and show the "type" of the value
  in the glyph. Validation can be done appropriately after editing the
  tree node.

  And finally, a way to "add"/"delete" items from the object or array.
  Again, this would be button.

  }

  { TJSONEditor }

  TJSONEditor = class(TFrame)
    JSONTree: TTreeView;
    EditToolbar: TToolBar;
    AddElementButton: TToolButton;
    DeleteElementButton: TToolButton;
    ToolButton1: TToolButton;
    SetObjectButton: TToolButton;
    SetArrayButton: TToolButton;
    SetBooleanButton: TToolButton;
    SetNumberButton: TToolButton;
    SetStringButton: TToolButton;
    SetNullButton: TToolButton;
    procedure AddElementButtonClick(Sender: TObject);
    procedure DeleteElementButtonClick(Sender: TObject);
    procedure JSONTreeEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure JSONTreeEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure JSONTreeSelectionChanged(Sender: TObject);
    procedure SetArrayButtonClick(Sender: TObject);
    procedure SetBooleanButtonClick(Sender: TObject);
    procedure SetNullButtonClick(Sender: TObject);
    procedure SetNumberButtonClick(Sender: TObject);
    procedure SetObjectButtonClick(Sender: TObject);
    procedure SetStringButtonClick(Sender: TObject);
  private
    { private declarations }
    procedure AddNodes(aParent: TTreeNode; aData: TJSONData);
    procedure SetToolsEnabled(aValue: Boolean);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    procedure SetJSON(aData: TJSONData);
    function GetJSON: TJSONData;
  end;

implementation

uses
  Dialogs;

{$R *.lfm}

{ TJSONValueEditor }

const
  NullCaption: String = '∅';
  TrueCaption: String = 'yes';
  FalseCaption: String = 'no';

function GetTypeCaption(aType: TJSONtype): String; overload;
begin
  case aType of
    jtNull:
      result := NullCaption;
    jtObject:
      result := '<object>';
    jtArray:
      result := '<array>';
  else
    result := '';
  end;

end;

function GetCaption(aData: TJSONData): String; overload;
begin
  case aData.JSONType of
    jtNumber, jtString:
      result := aData.AsString;
    jtBoolean:
      result := BoolToStr(aData.AsBoolean,TrueCaption,FalseCaption);
    jtNull, jtObject, jtArray:
      result := GetTypeCaption(aData.JSONType);
  else
    result := '';
  end;
end;

function BuildKeyAndValue(aKey: String; aValue: String): String;
begin
  result := aKey + ': ' + aValue;
end;

procedure ExtractKeyAndValue(const aData: String; out aKey: String; out aValue: String);
var
  aIndex: Integer;
begin
  aIndex := Pos(':',aData);
  if aIndex > 0 then
  begin
    aKey := Copy(aData,1,aIndex - 1);
    aValue := Trim(Copy(aData,aIndex + 1,Length(aData)));
  end
  else
  begin
    aKey := aData;
    aValue := '';
  end;

end;

{ TJSONEditor }

procedure TJSONEditor.JSONTreeSelectionChanged(Sender: TObject);
begin
  SetToolsEnabled(JSONTree.Selected <> nil);
end;

procedure TJSONEditor.SetArrayButtonClick(Sender: TObject);
var
  aParent: TTreeNode;
  aChild: TTreeNode;
  aProperty: String;
  aValue: String;
  i: Integer;
begin
  if JSONTree.Selected <> nil then
  begin
    case JSONTree.Selected.ImageIndex of
      ord(jtArray):
        // do nothing, it's already an array.
        // TODO: Should the button be disabled for this?
        exit;
      ord(jtObject):
        begin
          if MessageDlg('Are you sure you want to do this? You will lose all property names',mtConfirmation,mbYesNo,0) = mrYes then
          begin
            // Have to turn all of the children into elements.
            JSONTree.Selected.ImageIndex := ord(jtArray);
            aChild := JSONTree.Selected.GetFirstChild;
            i := 0;
            while aChild <> nil do
            begin
              ExtractKeyAndValue(aChild.Text,aProperty,aValue);
              aChild.Text := aValue;
              aChild := aChild.GetNextSibling;
              inc(i)
            end;
          end;
        end;
    end;
    aParent := JSONTree.Selected.Parent;
    JSONTree.Selected.ImageIndex := ord(jtArray);
    if aParent.ImageIndex = ord(jtObject) then
    begin
      ExtractKeyAndValue(JSONTree.Selected.Text,aProperty,aValue);
      JSONTree.Selected.Text := BuildKeyAndValue(aProperty,GetTypeCaption(jtArray));
    end
    else
    begin
      JSONTree.Selected.Text := GetTypeCaption(jtArray);
    end;
    JSONTree.Selected.Expanded := true;
  end;
end;

procedure TJSONEditor.SetBooleanButtonClick(Sender: TObject);
var
  aProperty: String;
  aValue: String;
  aParent: TTreeNode;
begin
  if JSONTree.Selected <> nil then
  begin
    if JSONTree.Selected.ImageIndex in [ord(jtArray),ord(jtObject)] then
    begin
      if MessageDlg('Are you sure you want to do this? You will lose all contained data',mtConfirmation,mbYesNo,0) = mrYes then
      begin
        JSONTree.Selected.DeleteChildren;
      end
      else
        Exit;
    end;

    aParent := JSONTree.Selected.Parent;
    JSONTree.Selected.ImageIndex := ord(jtBoolean);
    if aParent.ImageIndex = ord(jtObject) then
    begin
       ExtractKeyAndValue(JSONTree.Selected.Text,aProperty,aValue);
       JSONTree.Selected.Text := BuildKeyAndValue(aProperty,TrueCaption);
    end
    else
    begin
      JSONTree.Selected.Text := TrueCaption;
    end;
  end;
end;

procedure TJSONEditor.SetNullButtonClick(Sender: TObject);
var
  aProperty: String;
  aValue: String;
  aParent: TTreeNode;
begin
  if JSONTree.Selected <> nil then
  begin
    if JSONTree.Selected.ImageIndex in [ord(jtArray),ord(jtObject)] then
    begin
      if MessageDlg('Are you sure you want to do this? You will lose all contained data',mtConfirmation,mbYesNo,0) = mrYes then
      begin
        JSONTree.Selected.DeleteChildren;
      end
      else
        Exit;
    end;

    aParent := JSONTree.Selected.Parent;
    JSONTree.Selected.ImageIndex := ord(jtNull);
    if aParent.ImageIndex = ord(jtObject) then
    begin
       ExtractKeyAndValue(JSONTree.Selected.Text,aProperty,aValue);
       JSONTree.Selected.Text := BuildKeyAndValue(aProperty,NullCaption);
    end
    else
    begin
      JSONTree.Selected.Text := NullCaption;
    end;
  end;
end;

procedure TJSONEditor.SetNumberButtonClick(Sender: TObject);
var
  aProperty: String;
  aValue: String;
  aParent: TTreeNode;
  aNewValue: Double;
begin
  if JSONTree.Selected <> nil then
  begin
    if JSONTree.Selected.ImageIndex in [ord(jtArray),ord(jtObject)] then
    begin
      if MessageDlg('Are you sure you want to do this? You will lose all contained data',mtConfirmation,mbYesNo,0) = mrYes then
      begin
        JSONTree.Selected.DeleteChildren;
      end
      else
        Exit;
    end;

    aParent := JSONTree.Selected.Parent;
    JSONTree.Selected.ImageIndex := ord(jtNumber);
    if aParent.ImageIndex = ord(jtObject) then
    begin
       ExtractKeyAndValue(JSONTree.Selected.Text,aProperty,aValue);
       if not TryStrToFloat(aValue,aNewValue) then
         aNewValue := 0;
       JSONTree.Selected.Text := BuildKeyAndValue(aProperty,FloatToStr(aNewValue));
    end
    else
    begin
      if not TryStrToFloat(JSONTree.Selected.Text,aNewValue) then
        aNewValue := 0;
      JSONTree.Selected.Text := FloatToStr(0);
    end;
  end;
end;

procedure TJSONEditor.SetObjectButtonClick(Sender: TObject);
var
  aParent: TTreeNode;
  aChild: TTreeNode;
  aProperty: String;
  aValue: String;
  i: Integer;
begin
  if JSONTree.Selected <> nil then
  begin
    case JSONTree.Selected.ImageIndex of
      ord(jtArray):
        begin
          // Have to turn all of the children into elements.
          JSONTree.Selected.ImageIndex := ord(jtObject);
          aChild := JSONTree.Selected.GetFirstChild;
          i := 0;
          while aChild <> nil do
          begin
            aChild.Text := BuildKeyAndValue(IntToStr(i),aChild.Text);
            aChild := aChild.GetNextSibling;
            inc(i)
          end;
        end;
      ord(jtObject):
        // do nothing, it's already an object.
        // TODO: Should the button be disabled for this?
        exit;
    end;
    aParent := JSONTree.Selected.Parent;
    JSONTree.Selected.ImageIndex := ord(jtObject);
    if aParent.ImageIndex = ord(jtObject) then
    begin
       ExtractKeyAndValue(JSONTree.Selected.Text,aProperty,aValue);
       JSONTree.Selected.Text := BuildKeyAndValue(aProperty,GetTypeCaption(jtObject));
    end
    else
    begin
      JSONTree.Selected.Text := GetTypeCaption(jtObject);
    end;
    JSONTree.Selected.Expanded := true;
  end;
end;

procedure TJSONEditor.SetStringButtonClick(Sender: TObject);
var
  aProperty: String;
  aValue: String;
  aParent: TTreeNode;
begin
  // TODO: Have to convert the old stuff...
  if JSONTree.Selected <> nil then
  begin
    if JSONTree.Selected.ImageIndex in [ord(jtArray),ord(jtObject)] then
    begin
      if MessageDlg('Are you sure you want to do this? You will lose all contained data',mtConfirmation,mbYesNo,0) = mrYes then
      begin
        JSONTree.Selected.DeleteChildren;
      end
      else
        Exit;
    end;

    aParent := JSONTree.Selected.Parent;
    JSONTree.Selected.ImageIndex := ord(jtString);
    if aParent.ImageIndex = ord(jtObject) then
    begin
       ExtractKeyAndValue(JSONTree.Selected.Text,aProperty,aValue);
       JSONTree.Selected.Text := BuildKeyAndValue(aProperty,aValue);
    end
    else
    begin
      JSONTree.Selected.Text := JSONTree.Selected.Text;
    end;
  end;

end;

procedure TJSONEditor.AddElementButtonClick(Sender: TObject);
var
  aNew: TTreeNode;
  aParent: TTreeNode;
begin
  if (JSONTree.Selected = nil) then
  begin
    if (JSONTree.Items.Count = 0) then
    begin
      aNew := JSONTree.Items.AddChild(nil,GetTypeCaption(jtObject));
      aNew.ImageIndex := ord(jtObject);
    end
    // TODO: Add button should be disabled if the above isn't true.
  end
  else
  begin
    if JSONTree.Selected.ImageIndex in [ord(jtObject),ord(jtArray)] then
      aParent := JSONTree.Selected
    else
      aParent := JSONTree.Selected.Parent;
    if aParent <> nil then
    begin
      case aParent.ImageIndex of
        ord(jtObject):
          begin
            aNew := JSONTree.Items.AddChild(aParent,'property-name:');
            aNew.ImageIndex := ord(jtString);
          end;
        ord(jtArray):
          begin
            aNew := JSONTree.Items.AddChild(aParent,'');
            aNew.ImageIndex := ord(jtString);
          end;
      end;
      aParent.Expanded := true;
      aNew.EditText;
      // TODO: It would be nice if I could 'control' the editing,
      // so the user doesn't mess with the property name.
    end;
    // TODO: Add button should be disabled if the above isn't true.
  end;
end;

procedure TJSONEditor.DeleteElementButtonClick(Sender: TObject);
begin
  if JSONTree.Selected <> nil then
  begin
    if MessageDlg('Are you sure you want to delete this data?',mtConfirmation,mbYesNo,0) = mrYes then
    begin
      JSONTree.Selected.Delete;
    end;
  end;
  // Else delete button shouldn't even be enabled.
end;

procedure TJSONEditor.JSONTreeEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  aParent: TTreeNode;
  isProperty: Boolean;
  aValue: String;
  aKey: String;
  aBool: Boolean;
  aDble: Double;
begin
  aParent := Node.Parent;
  isProperty := false;
  if (aParent <> nil) and (aParent.ImageIndex = ord(jtObject)) then
  begin
    if Pos(':',S) = 0 then
    begin
       S := Node.Text;
       Exit;
    end;
    isProperty := true;
  end;
  if isProperty then
    ExtractKeyAndValue(S,aKey,aValue)
  else
    aValue := S;
  case Node.ImageIndex of
    ord(jtBoolean):
      // TODO: Need to accept things like 'yes', 'no', etc.
      if TryStrToBool(aValue,aBool) then
      begin
        if isProperty then
          S := BuildKeyAndValue(aKey,BoolToStr(aBool,TrueCaption,FalseCaption))
        else
          S := BoolToStr(aBool,TrueCaption,FalseCaption);
      end
      else
        s := Node.Text;
    ord(jtNumber):
      if TryStrToFloat(aValue,aDble) then
      begin
        if isProperty then
          S := BuildKeyAndValue(aKey,FloatToStr(aDble))
        else
          S := FloatToStr(aDble);
      end
      else
        s := Node.Text;
    ord(jtString):
      // do nothing, just accept it.
      ;
  else
    S := Node.Text;
  end;
end;

procedure TJSONEditor.JSONTreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := (Node.ImageIndex in [ord(jtNumber),ord(jtString),ord(jtBoolean)]);;
end;

procedure TJSONEditor.AddNodes(aParent: TTreeNode; aData: TJSONData);
var
  aArray: TJSONArray;
  aObject: TJSONObject;
  i: Integer;
  aKey: String;
  aChildNode: TTreeNode;
  aChildJSON: TJSONData;
begin
  case aData.JSONType of
    jtArray:
      begin
        aArray := aData as TJSONArray;
        for i := 0 to aArray.Count - 1 do
        begin
          aChildJSON := aArray[i];
          aChildNode := JSONTree.Items.AddChild(aParent,GetCaption(aChildJSON));
          aChildNode.ImageIndex:=ord(aChildJSON.JSONType);
          AddNodes(aChildNode,aChildJSON);
        end;
        aParent.Expanded := true;
      end;
    jtObject:
      begin
        aObject := aData as TJSONObject;
        for i := 0 to aObject.Count - 1 do
        begin
          aKey := aObject.Names[i];
          aChildJSON := aObject[aKey];
          aChildNode := JSONTree.Items.AddChild(aParent,BuildKeyAndValue(aKey,GetCaption(aChildJSON)));
          aChildNode.ImageIndex:=ord(aChildJSON.JSONType);
          AddNodes(aChildNode,aChildJSON);
        end;
        aParent.Expanded := true;
      end;
  end;
end;

procedure TJSONEditor.SetToolsEnabled(aValue: Boolean);
begin
  DeleteElementButton.Enabled := aValue;
  SetArrayButton.Enabled := aValue;
  SetObjectButton.Enabled := aValue;
  SetNullButton.Enabled:=aValue;
  SetNumberButton.Enabled := aValue;
  SetStringButton.Enabled := aValue;
  SetBooleanButton.Enabled := aValue;
end;

constructor TJSONEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SetToolsEnabled(JSONTree.Selected <> nil);
end;

procedure TJSONEditor.SetJSON(aData: TJSONData);
var
  aRoot: TTreeNode;
begin
  JSONTree.Items.Clear;
  if aData <> nil then
  begin
    aRoot := JSONTree.Items.AddChild(nil,GetCaption(aData));
    aRoot.ImageIndex := ord(aData.JSONType);
    AddNodes(aRoot,aData);
  end;
end;

function TJSONEditor.GetJSON: TJSONData;
begin
  // TODO:
end;

end.

