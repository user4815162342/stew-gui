unit sys_dynval;

{$mode objfpc}{$H+}
// I'm using com interfaces here, so that I get reference counting, which I want.
{$interfaces COM}

interface

uses
  Classes, SysUtils, sys_types;

{



TODO: Make use of this
1. Don't do this from the top down, changing everything at once. Go from the bottom
up. So, instead of just switching from TProjectProperties to TProjectProperties2 in
stew_project, switch the various editors over, using some conversion functions to
quickly convert back and forth when communicating with higher level units. If,
in the process of doing this, I discover another more complicated place, then I
can backtrack and start off with a smaller section.
-- DONE gui_jsoneditor
-- DONE gui_config -- convert over to the new JSON format for saving data.
-- stew_project -- start converting everything into the new properties.
   -- projectproperties
   -- documentproperties
-- gui_documenteditor -- Can wait until we switch over from Stew_Project, because
there isn't going to be too much to change.
-- gui_projectsettingseditor -- same as gui_documenteditor.
-- gui_mainform -- Not sure here, but probably the same as gui_documenteditor
-- stew_properties -- get rid of the old properties
-- sys_dynval -- get rid of the conversion functions.
-- sys_json -- get rid of this entirely.

2. Make use of the ToOldJSONValue functions to do this.
3. As things are moved over, make them use dynval natively, but allow conversion to
other stuff as necessary. Then, once everything is using dynval natively, we can
get rid of the TJSValue stuff.
4. Let's start with the gui_jsoneditor, since that's actually the most complex. Then,
work with the document settings editor, the project settings editor, and project explorer,
then the various property promises, and finally stew_project itself.



A replacement for sys_json, which solves several problems:
1) removes the extra functionality such as strictly-typed properties and easy
type conversion which is really unnecessary for our purposes.
2) reduces the issues caused by memory management, which are a problem when these
config objects can be passed all around the application whenever you want. This is
really a problem when we get into querying the data.

Decisions:
- Classes require create and destroy, which adds complication for use when passing
data around.
- Records can't be recursive, except by using pointers, which again adds the complications
of memory management. (This includes variant records and advanced records).
- Objects: turns out that these stack-based class-like structures can't be passed
around dynamically. If you assign a ODynamicString to a variable that expects
ODynamicPrimitive, then you lose the string data when the value is copied. The
solution is pointers, which again adds memory management complications.
- Garbage Collection: As long as this is not built into freepascal, this just
pushes the memory management complications into some other place. I'd still have
to make sure objects are retained and released for garbage collection *and* I'd
need to make sure the garbage collector runs occasionally.
- Variants: Can't handle the structured types.
- The solution appears to be Interfaces, which are automatically reference
counted, not truly garbage collected, but memory management is much easier. I
just make sure that they can't 'own' themselves in circular references, and
there's no problem with that.

}
const
  DynamicValueGUID = '{B2B8454B-103C-4690-ACC1-EC75F65F2D76}';
  DynamicNullGUID = '{99D3970C-83F8-4DBE-A282-44D7AD500B1C}';
  DynamicBooleanGUID = '{D44976DF-AED6-4626-BA33-EE83DC4080CA}';
  DynamicNumberGUID = '{2970D3E8-3C5C-4D41-8D13-95CEBE87AB9C}';
  DynamicStringGUID = '{D919EF1C-2E6B-4CE4-B2B2-07545A1BE7F7}';
  DynamicListGUID = '{E7D0AE4B-435B-45E5-BC6F-E1F478DD33BC}';
  DynamicMapGUID = '{B59C4808-7966-4AB2-9E6E-4153F0985562}';
  DynamicObjectGUID = '{95E0839B-256C-4323-8AE9-237C9AF814CF}';

type

  TDynamicValueKind = (dvkUndefined,
                       dvkNull,
                       dvkBoolean,
                       dvkNumber,
                       dvkString,
                       dvkList,
                       dvkMap,
                       dvkObject);

const
  DynamicValueKinds: array[TDynamicValueKind] of TGUID =
                      (DynamicValueGUID,
                      DynamicNullGUID,
                      DynamicBooleanGUID,
                      DynamicNumberGUID,
                      DynamicStringGUID,
                      DynamicListGUID,
                      DynamicMapGUID,
                      DynamicObjectGUID);

type

  { IDynamicValue }

  IDynamicValue = interface(IUnknown)
    [DynamicValueGUID]
    function GetItem(const aKey: IDynamicValue): IDynamicValue;
    function GetKindOf: TDynamicValueKind;
    procedure SetItem(const aKey: IDynamicValue; const AValue: IDynamicValue);
    function Owns(const aValue: IDynamicValue): Boolean;
    function IsDefined: Boolean;
    // TODO: I don't think IsEqualTo and IsStructurallyEqualTo are really used
    // anywhere, except in testing. They are not supported in IDynamicObject
    // at all. We can probably get rid of these.
    function IsStructurallyEqualTo(const aValue: IDynamicValue): Boolean;
    function IsEqualTo(const aValue: IDynamicValue): Boolean;
    property Item[aKey: IDynamicValue]: IDynamicValue read GetItem write SetItem; default;
    property KindOf: TDynamicValueKind read GetKindOf;
    function Clone: IDynamicValue;
  end;

  { IDynamicNull }

  IDynamicNull = interface(IDynamicValue)
    [DynamicNullGUID]
  end;

  { IDynamicBoolean }

  IDynamicBoolean = interface(IDynamicValue)
    [DynamicBooleanGUID]
    function GetValue: Boolean;
    property Value: Boolean read GetValue;
  end;

  { IDynamicNumber }

  IDynamicNumber = interface(IDynamicValue)
    [DynamicNumberGUID]
    function GetValue: Double;
    property Value: Double read GetValue;
  end;

  { IDynamicString }

  IDynamicString = interface(IDynamicValue)
    [DynamicStringGUID]
    function GetValue: UTF8String;
    property Value: UTF8String read GetValue;
  end;

  { IDynamicList }

  IDynamicList = interface(IDynamicValue)
    [DynamicListGUID]
    function GetItem(const aIndex: Longint): IDynamicValue; overload;
    function GetLength: Longint;
    procedure SetItem(const aIndex: Longint; const AValue: IDynamicValue); overload;
    procedure SetLength(const AValue: Longint);
    property Item[aIndex: Longint]: IDynamicValue read GetItem write SetItem; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(const aItem: IDynamicValue);
    procedure Delete(const aIndex: Longint);
    procedure Clear;
    function IndexOf(const aValue: IDynamicValue): Longint;
  end;

  { IDynamicMapEnumerator }

  IDynamicMapEnumerator = interface(IUnknown)
    ['{F826FD16-733D-4EE5-9921-12D689857449}']
    function GetKey: UTF8String;
    function GetValue: IDynamicValue;
    function Next: Boolean;
    property Key: UTF8String read GetKey;
    property Value: IDynamicValue read GetValue;
  end;

  { IDynamicMap }

  IDynamicMap = interface(IDynamicValue)
    [DynamicMapGUID]
    function GetItem(const aKey: UTF8String): IDynamicValue; overload;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue); overload;
    property Item[aKey: UTF8String]: IDynamicValue read GetItem write SetItem; default;
    function GetKeys: TStringArray2;
    function Has(const aKey: UTF8String): Boolean;
    procedure Delete(const aKey: UTF8String);
    procedure Clear;
    function Enumerate: IDynamicMapEnumerator;
  end;

  // This one can be used to allow native objects to be stored inside of
  // a dynamic map or list. This is going to be useful for dynamic values
  // used for property entities, since I can now essentially use the IDynamicMap
  // as a sort of hash map inside. Objects can not be created with the class
  // factory below. More than likely, they'll be separate object types anyway.
  IDynamicObject = interface(IDynamicValue)
    [DynamicObjectGUID]
  end;

  { TDynamicValues }

  {
  Yes, this is currently "stuck" to sys_dynobject_implementation. But, if
  absolutely necessary, I could unstick it.
  }
  TDynamicValues = class
  private
    class var fUndefined: IDynamicValue;
    class var fNull: IDynamicNull;
    class var fTrue: IDynamicBoolean;
    class var fFalse: IDynamicBoolean;
    class destructor DestroyClass;
  public
    // the first three don't have 'New', because I don't want to imply
    // that I'm not sharing values...
    class function Undefined: IDynamicValue;
    class function Null: IDynamicNull;
    class function Boolean(const aValue: Boolean): IDynamicBoolean;
    class function NewString(const aValue: UTF8String): IDynamicString;
    class function NewNumber(const aValue: Double): IDynamicNumber;
    class function NewList: IDynamicList;
    class function NewMap: IDynamicMap;
  end;

  { TDynamicValueWriter }

  TDynamicValueWriter = class abstract
  private
    fStack: IDynamicList;
    fFoundItem: Boolean;
  strict protected
    procedure WriteNullToken; virtual; abstract;
    procedure WriteNumberToken(const aValue: Double); virtual; abstract;
    procedure WriteBooleanToken(const aValue: Boolean); virtual; abstract;
    procedure WriteStringToken(const aValue: UTF8String); virtual; abstract;
    procedure WriteMapStartToken; virtual; abstract;
    procedure WriteMapEndToken; virtual; abstract;
    procedure WriteListStartToken; virtual; abstract;
    procedure WriteListEndToken; virtual; abstract;
    procedure WriteKeyIndicatorToken; virtual; abstract;
    procedure WriteGap(aIndentCount: Longint); virtual; abstract;
    procedure WriteListSeparatorToken; virtual; abstract;
    procedure WritePrimitive(const aValue: IDynamicValue);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteMapStart(const aValue: IDynamicMap);
    procedure WriteMapEnd;
    procedure WriteListStart(const aValue: IDynamicList);
    procedure WriteListEnd;
    procedure WriteList(const AValue: IDynamicList);
    procedure WriteMap(const aValue: IDynamicMap);
    procedure WriteValue(const aValue: IDynamicValue);
    procedure WriteKeyValue(const aKey: UTF8String; const aValue: IDynamicValue);
    procedure WriteKey(const aKey: UTF8String);
  end;

  { TDynamicValueReader }

  TDynamicValueReader = class abstract
  private type
    TStackInfo = record
      ListDoesNotContinue: Boolean;
      IsInlist: Boolean;
    end;

  private
    fStack: array of TStackInfo;
    procedure Push;
    procedure Pop;
    function Peek: TStackInfo;
    procedure SetListDoesNotContinueToStack;
  strict protected
    procedure ReadListSeparatorToken; virtual; abstract;
    procedure ReadNullToken; virtual; abstract;
    function ReadNumberToken: Double; virtual; abstract;
    function ReadBooleanToken: Boolean; virtual; abstract;
    function ReadStringToken: UTF8String; virtual; abstract;
    procedure ReadMapStartToken; virtual; abstract;
    procedure ReadMapEndToken; virtual; abstract;
    procedure ReadListStartToken; virtual; abstract;
    procedure ReadListEndToken; virtual; abstract;
    procedure ReadKeyIndicatorToken; virtual; abstract;
    procedure ReadListSeparator;
    procedure CheckListContinues;
  public
    function IsMapStart: Boolean; virtual; abstract;
    function IsListStart: Boolean; virtual; abstract;
    function IsString: Boolean; virtual; abstract;
    function IsBoolean: Boolean; virtual; abstract;
    function IsNumber: Boolean; virtual; abstract;
    function IsNull: Boolean; virtual; abstract;
    function IsMapEnd: Boolean; virtual; abstract;
    function IsListEnd: Boolean; virtual; abstract;
    constructor Create;
    procedure ReadMapStart;
    function ReadMapKey: UTF8String;
    procedure ReadMapEnd;
    procedure ReadListStart;
    procedure ReadListEnd;
    function ReadString: IDynamicString;
    function ReadBoolean: IDynamicBoolean;
    function ReadNumber: IDynamicNumber;
    function ReadNull: IDynamicNull;
    function ReadList: IDynamicList;
    function ReadMap: IDynamicMap;
    function ReadValue: IDynamicValue;
  end;

  // Just overload the operators we need as they are needed.
  // These operator overloads make it easier to move real values and primitives around
  // inside maps (combines with the Default property of an IDynamicValue which
  // allows getting and setting based on index.
  operator :=(aValue: Double): IDynamicValue;
  operator :=(aValue: UTF8String): IDynamicValue;
  operator :=(aValue: Boolean): IDynamicValue;


implementation

uses
  sys_dynval_implementation;

operator:=(aValue: Double): IDynamicValue;
begin
  result := TDynamicValues.NewNumber(aValue);
end;


operator:=(aValue: UTF8String): IDynamicValue;
begin
  result := TDynamicValues.NewString(aValue);
end;

operator:=(aValue: Boolean): IDynamicValue;
begin
  result := TDynamicValues.Boolean(aValue);

end;

operator:=(aValue: Longint): IDynamicNumber;
begin
  result := TDynamicValues.NewNumber(aValue);
end;

operator:=(aValue: ShortInt): IDynamicNumber;
begin
  result := TDynamicValues.NewNumber(aValue);
end;

{ TDynamicValueWriter }

procedure TDynamicValueWriter.WritePrimitive(const aValue: IDynamicValue);
begin
  if (fStack.Length > 0) and
     (fStack[fStack.Length - 1] is IDynamicList) then
  begin
    if fFoundItem then
       WriteListSeparatorToken;
    WriteGap(fStack.Length);
  end;
  if aValue is IDynamicBoolean then
     WriteBooleanToken(IDynamicBoolean(aValue).Value)
  else if aValue is IDynamicNumber then
     WriteNumberToken(IDynamicNumber(aValue).Value)
  else if aValue is IDynamicString then
    WriteStringToken(IDynamicString(aValue).Value)
  else
    WriteNullToken;
  fFoundItem := true;
end;

constructor TDynamicValueWriter.Create;
begin
  inherited Create;
  fStack := TDynamicValues.NewList;
end;

destructor TDynamicValueWriter.Destroy;
begin
  fStack := nil;
  inherited Destroy;
end;

procedure TDynamicValueWriter.WriteMapStart(const aValue: IDynamicMap);
begin
  if fStack.IndexOf(aValue) > -1 then
     raise Exception.Create('Attempt to write recursive map');
  if (fStack.Length > 0) and
     (fStack[fStack.Length - 1] is IDynamicList) then
  begin
    if fFoundItem then
       WriteListSeparatorToken;
    WriteGap(fStack.Length);
  end;
  fFoundItem := false;
  if aValue <> nil then
     fStack.Add(aValue)
  else
     fStack.Add(TDynamicValues.NewMap);
  WriteMapStartToken;
end;

procedure TDynamicValueWriter.WriteMapEnd;
begin
  if (fStack.Length > 0) and
     (fStack[fStack.Length - 1] is IDynamicMap) then
  begin
    WriteGap(fStack.Length - 1);
  end
  else
    raise Exception.Create('Can''t write map end, not in map.');
  WriteMapEndToken;
  fStack.Delete(fStack.Length - 1);
  fFoundItem := True;
end;

procedure TDynamicValueWriter.WriteListStart(const aValue: IDynamicList);
begin
  if fStack.IndexOf(aValue) > -1 then
     raise Exception.Create('Attempt to write recursive map');
  if (fStack.Length > 0) and
     (fStack[fStack.Length - 1] is IDynamicList) then
  begin
    if fFoundItem then
       WriteListSeparatorToken;
    WriteGap(fStack.Length);
  end;
  fFoundItem := false;
  if aValue <> nil then
     fStack.Add(aValue)
  else
     fStack.Add(TDynamicValues.NewList);
  WriteListStartToken;
end;

procedure TDynamicValueWriter.WriteListEnd;
begin
  if (fStack.Length > 0) and
     (fStack[fStack.Length - 1] is IDynamicList) then
  begin
    WriteGap(fStack.Length - 1);
  end
  else
  begin
    raise Exception.Create('Can''t write list end, not in list');
  end;
  WriteListEndToken;
  fStack.Delete(fStack.Length - 1);
  fFoundItem := True;
end;

procedure TDynamicValueWriter.WriteList(const AValue: IDynamicList);
var
  l: Longint;
  i: Longint;
begin
  WriteListStart(AValue);
  l := aValue.Length - 1;
  for i := 0 to l do
  begin
    WriteValue(aValue[i]);
  end;
  WriteListEnd;
end;

procedure TDynamicValueWriter.WriteMap(const aValue: IDynamicMap);
var
  lEnum: IDynamicMapEnumerator;
begin
  WriteMapStart(aValue);
  lEnum := aValue.Enumerate;
  while lEnum.Next do
  begin
    WriteKeyValue(lEnum.Key,lEnum.Value);
  end;
  WriteMapEnd;
end;

procedure TDynamicValueWriter.WriteValue(const aValue: IDynamicValue);
begin
  if aValue is IDynamicList then
     WriteList(IDynamicList(aValue))
  else if aValue is IDynamicMap then
     WriteMap(IDynamicMap(aValue))
  else
     WritePrimitive(aValue);
end;

procedure TDynamicValueWriter.WriteKeyValue(const aKey: UTF8String;
  const aValue: IDynamicValue);
begin
  WriteKey(aKey);
  WriteValue(aValue);
end;

procedure TDynamicValueWriter.WriteKey(const aKey: UTF8String);
begin
  if (fStack.Length > 0) and
     (fStack[0] is IDynamicMap) then
  begin
     if fFoundItem then
        WriteListSeparatorToken;
     WriteGap(fStack.Length);
  end
  else
     raise Exception.Create('Can''t write key-value, not in map');
  WriteStringToken(aKey);
  WriteKeyIndicatorToken;
end;

{ TDynamicValueReader }

procedure TDynamicValueReader.Push;
var
  l: LongInt;
begin
  l := Length(fStack);
  SetLength(fStack,l + 1);
  fStack[l].ListDoesNotContinue := false;
  fStack[l].IsInlist := true;

end;

procedure TDynamicValueReader.Pop;
begin
  SetLength(fStack,Length(fStack) - 1);

end;

function TDynamicValueReader.Peek: TStackInfo;
var
  l: LongInt;
begin
  l := Length(fStack);
  if l > 0 then
  begin
     result := fStack[l - 1]
  end
  else
  begin
     result.IsInList := false;
     result.ListDoesNotContinue := false;
  end;
end;

procedure TDynamicValueReader.SetListDoesNotContinueToStack;
var
  l: Longint;
begin
  l := Length(fStack);
  if l > 0 then
     fStack[l - 1].ListDoesNotContinue := true;
end;

procedure TDynamicValueReader.ReadListSeparator;
var
  lStack: TStackInfo;
begin
  lStack := Peek;
  if lStack.IsInlist and not (IsMapEnd or IsListEnd) then
  begin
     ReadListSeparatorToken;
  end
  else
  begin
     SetListDoesNotContinueToStack;
  end;

end;

procedure TDynamicValueReader.CheckListContinues;
var
  lStack: TStackInfo;
begin
  lStack := Peek;
  // an error occurs if the list doesn't continue and we're not at the
  // end of a list or the end of a map. This means, if other code calls
  // Read* when it should have known better, the error will be an unexpected
  // list end. Whereas if it's a matter of the JSON syntax being incorrect,
  // the error will be here.
  if (lStack.ListDoesNotContinue) and (IsListEnd or IsMapEnd) then
  begin
     raise Exception.Create('List shouldn''t continue');
  end;

end;

constructor TDynamicValueReader.Create;
begin
  SetLength(fStack,0);
end;

procedure TDynamicValueReader.ReadMapStart;
begin
  CheckListContinues;
  ReadMapStartToken;
  Push;
end;

function TDynamicValueReader.ReadMapKey: UTF8String;
begin
  CheckListContinues;
  result := ReadStringToken;
  ReadKeyIndicatorToken;
end;

procedure TDynamicValueReader.ReadMapEnd;
begin
  ReadMapEndToken;
  Pop;
  ReadListSeparator;

end;

procedure TDynamicValueReader.ReadListStart;
begin
  CheckListContinues;
  Push;
  ReadListStartToken;

end;

procedure TDynamicValueReader.ReadListEnd;
begin
  ReadListEndToken;
  Pop;
  ReadListSeparator;

end;

function TDynamicValueReader.ReadString: IDynamicString;
begin
  CheckListContinues;
  result := TDynamicValues.NewString(ReadStringToken);
  ReadListSeparator;

end;

function TDynamicValueReader.ReadBoolean: IDynamicBoolean;
begin
  CheckListContinues;
  result := TDynamicValues.Boolean(ReadBooleanToken);
  ReadListSeparator;

end;

function TDynamicValueReader.ReadNumber: IDynamicNumber;
begin
  CheckListContinues;
  result := TDynamicValues.NewNumber(ReadNumberToken);
  ReadListSeparator;

end;

function TDynamicValueReader.ReadNull: IDynamicNull;
begin
  CheckListContinues;
  ReadNullToken;
  result := TDynamicValues.Null;
  ReadListSeparator;

end;

function TDynamicValueReader.ReadList: IDynamicList;
begin
  ReadListStart;
  result := TDynamicValues.NewList;
  while not IsListEnd do
  begin
    result.Add(ReadValue);
  end;
  ReadListEnd;
end;

function TDynamicValueReader.ReadMap: IDynamicMap;
var
  lKey: UTF8String;
begin
  ReadMapStart;
  result := TDynamicValues.NewMap;
  while not IsMapEnd do
  begin
    lKey := ReadMapKey;
    result[lKey] := ReadValue;
  end;
  ReadMapEnd;
end;

function TDynamicValueReader.ReadValue: IDynamicValue;
begin
  CheckListContinues;
  if IsMapStart then
  begin
     result := ReadMap;
  end
  else if IsListStart then
  begin
     result := ReadList;
  end
  else if IsNull then
  begin
     result := ReadNull;
  end
  else if IsBoolean then
  begin
     result := ReadBoolean;
  end
  else if IsNumber then
  begin
     result := ReadNumber;
  end
  else if IsString then
  begin
     result := ReadString;
  end
  else
    raise Exception.Create('Invalid reader state');

end;

{ TDynamicValues }

class destructor TDynamicValues.DestroyClass;
begin
  fNull := nil;
  fUndefined := nil;
  fTrue := nil;
  fFalse := nil;
end;

class function TDynamicValues.Undefined: IDynamicValue;
begin
  if fUndefined = nil then
     fUndefined := TDynamicValue.Create;
  result := fUndefined;
end;

class function TDynamicValues.Null: IDynamicNull;
begin
  if fNull = nil then
     fNull := TDynamicNull.Create;
  result := fNull;

end;

class function TDynamicValues.NewString(const aValue: UTF8String): IDynamicString;
begin
  result := TDynamicString.Create(aValue);
end;

class function TDynamicValues.Boolean(const aValue: Boolean): IDynamicBoolean;
begin
  if aValue then
  begin
    if fTrue = nil then
       fTrue := TDynamicBoolean.Create(true);
    result := fTrue;
  end
  else
  begin
    if fFalse = nil then
       fFalse := TDynamicBoolean.Create(false);
    result := fFalse;
  end;
end;

class function TDynamicValues.NewNumber(const aValue: Double): IDynamicNumber;
begin
  result := TDynamicNumber.Create(aValue);
end;

class function TDynamicValues.NewList: IDynamicList;
begin
  result := TDynamicList.Create;
end;

class function TDynamicValues.NewMap: IDynamicMap;
begin
  result := TDynamicMap.Create;
end;

end.

