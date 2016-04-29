unit sys_dynobject;

{$mode objfpc}{$H+}
// I'm using com interfaces here, so that I get reference counting. In this case, I *want* reference counting.
{$interfaces COM}

interface

uses
  Classes, SysUtils, sys_types;

{

TODO: Units should be dynval, not dynobject.

TODO: I need to make sure everything's got a const, but that should be in the
master branch.

TODO:
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


TODO: There are going to be several parts to this:
- sys_dynobject:
  - defines interfaces for dynamic objects. Each one is as simple as possible.
- sys_dynobject_implementation:
  - defines implementations and a class factory for dynamic objects.
- sys_dynobject_json:
  - defines json serializers and deserializers which work with the interfaces
  - reader basically consists of a bunch of methods which "expect" various tokens.
  - saver basically works backwards to write out tokens, so you would do
    WriteObjectStart, WriteObjectProperty, WriteObjectEnd, etc.
  - fromJSON and ToJSOn are going to be utility methods which just read into
  default values, but you can use readers and writers directly as well.
- sys_dynobject_storage:
  - (not sure about name) These are wrapper objects which maintain references to
  dynobjects, but control access to them through a class hierarchy, and validate
  the syntax as they are being read from JSON.
    - constructor takes a stream or a JSONReader (or nil to create a new one), and
    the reader is used to load it in. Reading this consists of ReadObjectStart,
    followed by a series of ReadObjectKey. Once a key is found, it calls a virtual
    function CreateChild(aKey): which returns another config object or nil. If
    the config object is returned, then the reader is passed on to that constructor.
    Otherwise, the value is read and stored on a cached dynobject for saving to
    later. Writing out works the opposite way.
    - ReadKey(Key, Reader, var handled)
      - if not handled then put into the inner object.
    - ReadItem(Reader, var handled) - for an array.
    - When writing, the object had to write out it's own objects to the JSON
      as well.



}

type

  { IDynamicValue }

  IDynamicValue = interface(IUnknown)
    ['{B2B8454B-103C-4690-ACC1-EC75F65F2D76}']
    function Owns(aValue: IDynamicValue): Boolean;
    function IsDefined: Boolean;
    function EqualsDeeply(aValue: IDynamicValue): Boolean;
  end;

  { IDynamicNull }

  IDynamicNull = interface(IDynamicValue)
    ['{99D3970C-83F8-4DBE-A282-44D7AD500B1C}']
  end;

  { IDynamicBoolean }

  IDynamicBoolean = interface(IDynamicValue)
    ['{D44976DF-AED6-4626-BA33-EE83DC4080CA}']
    function GetValue: Boolean;
    property Value: Boolean read GetValue;
  end;

  { IDynamicNumber }

  IDynamicNumber = interface(IDynamicValue)
    ['{2970D3E8-3C5C-4D41-8D13-95CEBE87AB9C}']
    function GetValue: Double;
    property Value: Double read GetValue;
  end;

  { IDynamicString }

  IDynamicString = interface(IDynamicValue)
    ['{D919EF1C-2E6B-4CE4-B2B2-07545A1BE7F7}']
    function GetValue: UTF8String;
    property Value: UTF8String read GetValue;
  end;

  { IDynamicList }

  IDynamicList = interface(IDynamicValue)
    ['{E7D0AE4B-435B-45E5-BC6F-E1F478DD33BC}']
    function GetItem(aIndex: Longint): IDynamicValue;
    function GetLength: Longint;
    procedure SetItem(aIndex: Longint; AValue: IDynamicValue);
    procedure SetLength(AValue: Longint);
    property Item[aIndex: Longint]: IDynamicValue read GetItem write SetItem; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(aItem: IDynamicValue);
    procedure Delete(aIndex: Longint);
    procedure Clear;
    function IndexOf(aValue: IDynamicValue): Longint;
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
    ['{B59C4808-7966-4AB2-9E6E-4153F0985562}']
    function GetItem(aKey: UTF8String): IDynamicValue;
    procedure SetItem(aKey: UTF8String; AValue: IDynamicValue);
    property Item[aKey: UTF8String]: IDynamicValue read GetItem write SetItem; default;
    function GetKeys: TStringArray;
    function Has(aKey: UTF8String): Boolean;
    procedure Delete(aKey: UTF8String);
    procedure Clear;
    function Enumerate: IDynamicMapEnumerator;
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
    class function Boolean(aValue: Boolean): IDynamicBoolean;
    class function NewString(aValue: UTF8String): IDynamicString;
    class function NewNumber(aValue: Double): IDynamicNumber;
    class function NewList: IDynamicList;
    class function NewMap: IDynamicMap;
  end;

  TDynamicValueWriter = class abstract
  public
    procedure WriteMapStart; virtual; abstract;
    procedure WriteMapKey(aValue: UTF8String); virtual; abstract;
    procedure WriteMapEnd; virtual; abstract;
    procedure WriteListStart; virtual; abstract;
    procedure WriteListEnd; virtual; abstract;
    procedure WriteNull; virtual; abstract;
    procedure WriteNumber(aValue: Double); virtual; abstract;
    procedure WriteBoolean(aValue: Boolean); virtual; abstract;
    procedure WriteString(aValue: UTF8String); virtual; abstract;
  end;

  TDynamicValueReader = class abstract
  public
    function IsMapStart: Boolean; virtual; abstract;
    procedure ReadMapStart; virtual; abstract;
    function ReadMapKey: UTF8String; virtual; abstract;
    function IsMapEnd: Boolean; virtual; abstract;
    procedure ReadMapEnd; virtual; abstract;
    function IsListStart: Boolean; virtual; abstract;
    procedure ReadListStart; virtual; abstract;
    function IsListEnd: Boolean; virtual; abstract;
    procedure ReadListEnd; virtual; abstract;
    function IsString: Boolean; virtual; abstract;
    function ReadString: UTF8String; virtual; abstract;
    function IsBoolean: Boolean; virtual; abstract;
    function ReadBoolean: Boolean; virtual; abstract;
    function IsNumber: Boolean; virtual; abstract;
    function ReadNumber: Double; virtual; abstract;
    function IsNull: Boolean; virtual; abstract;
    procedure ReadNull; virtual; abstract;
  end;

  function ReadDynamicValue(aReader: TDynamicValueReader): IDynamicValue;

  procedure WriteDynamicValue(aValue: IDynamicValue; aWriter: TDynamicValueWriter);


implementation

uses
  sys_dynobject_implementation;

function ReadDynamicValue(aReader: TDynamicValueReader): IDynamicValue;

  function ReadValue: IDynamicValue; forward;

  function ReadMap: IDynamicMap;
  var
    lKey: UTF8String;
  begin
    aReader.ReadMapStart;
    result := TDynamicValues.NewMap;
    while not aReader.IsMapEnd do
    begin
      lKey := aReader.ReadMapKey;
      result[lKey] := ReadValue;
    end;
    aReader.ReadMapEnd;

  end;

  function ReadList: IDynamicList;
  begin
    aReader.ReadListStart;
    result := TDynamicValues.NewList;
    while not aReader.IsListEnd do
    begin
      result.Add(ReadValue);
    end;
    aReader.ReadMapEnd;

  end;

  function ReadValue: IDynamicValue;
  begin
    if aReader.IsMapStart then
       result := ReadMap
    else if aReader.IsListStart then
       result := ReadList
    else if aReader.IsNull then
    begin
      aReader.ReadNull;
      result := TDynamicValues.Null;
    end
    else if aReader.IsBoolean then
    begin
      result := TDynamicValues.Boolean(aReader.ReadBoolean);
    end
    else if aReader.IsNumber then
    begin
      Result := TDynamicValues.NewNumber(aReader.ReadNumber);
    end
    else if aReader.IsString then
    begin
      Result := TDynamicValues.NewString(aReader.ReadString);
    end
    else
      raise Exception.Create('Invalid reader state');
  end;

begin
  result := ReadValue;
end;

procedure WriteDynamicValue(aValue: IDynamicValue; aWriter: TDynamicValueWriter);

  procedure WriteValue(aValue: IDynamicValue); forward;

  procedure WriteMap(aValue: IDynamicMap);
  var
    lEnum: IDynamicMapEnumerator;
  begin
    aWriter.WriteMapStart;
    lEnum := aValue.Enumerate;
    while lEnum.Next do
    begin
      aWriter.WriteMapKey(lEnum.Key);
      WriteValue(lEnum.Value);
    end;
    aWriter.WriteMapEnd;

  end;

  procedure WriteList(aValue: IDynamicList);
  var
    l: Longint;
    i: Longint;
  begin
    aWriter.WriteListStart;
    l := aValue.Length - 1;
    for i := 0 to l do
    begin
      WriteValue(aValue);
    end;
    aWriter.WriteListEnd;

  end;

  procedure WriteValue(aValue: IDynamicValue);
  begin
    if aValue is IDynamicBoolean then
       aWriter.WriteBoolean(IDynamicBoolean(aValue).Value)
    else if aValue is IDynamicNumber then
       aWriter.WriteNumber(IDynamicNumber(aValue).Value)
    else if aValue is IDynamicString then
       aWriter.WriteString(IDynamicString(aValue).Value)
    else if aValue is IDynamicList then
       WriteList(IDynamicList(aValue))
    else if aValue is IDynamicMap then
       WriteMap(IDynamicMap(aValue))
    else
      aWriter.WriteNull;
  end;

begin
  WriteValue(aValue);
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

class function TDynamicValues.NewString(aValue: UTF8String): IDynamicString;
begin
  result := TDynamicString.Create(aValue);
end;

class function TDynamicValues.Boolean(aValue: Boolean): IDynamicBoolean;
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
       fFalse := TDynamicBoolean.Create(true);
    result := fFalse;
  end;
end;

class function TDynamicValues.NewNumber(aValue: Double): IDynamicNumber;
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

