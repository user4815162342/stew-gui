unit sys_dynval_data;

{$mode objfpc}{$H+}

interface

{
TODO: It seems to me that the object style is a better idea. However, I need
to make it fit the architecture of the Class-style that I have, where it
deserializes itself. That way structural errors are caught on initialization.
So:
1. Make the 'class' hierarchy I've got here into an 'object' hierarchy. Try
to use the techniques developed below to reduce the amount of 'virtual'
methods if possible (should just be the Read... and Write... thingies).
2. Add constructors to those objects which accept readers, streams and strings,
so I don't have to create an empty backing and then get it replaced with another
if I'm going to be serializing immediately.

TODO: I need to decide between the class-style and the object-style and make
use of them:
class
+works in a traditional manner
-requires extra memory management and not easy to pass around
 (however, unlike the dynamic values themselves, I'm *not* passing these around
 a lot)

object
-objects are really meant for being dealt with by pointer, and I'm not
+can allocate on the stack without freeing data.


Basically the objects defined here can be serialized and deserialized from a
DynamicValueReader and -Writer. Where a map key or list index matches up with
a managed property, it will read that property's data from the reader. If it
is not, however, that data is stored into a IDynamicValue backing data store.
When the object is serialized again, that data will also be serialized, in
addition to the properties the object needs to serialize.

}

uses
  Classes, SysUtils, sys_dynval;

type

  { TDataStoreObject }

  TDataStoreObject = class
  protected
    procedure InitializeBlank; virtual; abstract;
  public
    constructor Create;
    procedure Serialize(aWriter: TDynamicValueWriter); virtual; abstract; overload;
    procedure Serialize(aStream: TStream); overload;
    function Serialize: UTF8String; overload;
    procedure Deserialize(aReader: TDynamicValueReader); virtual; abstract; overload;
    procedure Deserialize(aStream: TStream); overload;
    procedure Deserialize(aString: UTF8String); overload;

  end;

  { TDataStoreObject2 }

  TDataStoreObject2 = object
  protected
    fBacking: IDynamicValue;
    function GetString(const aIndex: IDynamicValue; const aDefault: UTF8String): UTF8String;
    function GetInteger(const aIndex: IDynamicValue; const aDefault: Longint): Longint;
    function GetBoolean(const aIndex: IDynamicValue; const aDefault: Boolean): Boolean;
    function GetFloat(const aIndex: IDynamicValue; const aDefault: Double): Double;
    function GetMap(const aIndex: IDynamicValue): IDynamicMap;
    function GetList(const aIndex: IDynamicValue): IDynamicList;
    function GetDateTime(const aIndex: IDynamicValue): TDateTime;
    procedure SetValue(const aIndex: IDynamicValue; const aValue: UTF8String; const aDefault: UTF8String);
    procedure SetValue(const aIndex: IDynamicValue; const aValue: Longint; const aDefault: Longint);
    procedure SetValue(const aIndex: IDynamicValue; const aValue: Boolean; const aDefault: Boolean);
    procedure SetValue(const aIndex: IDynamicValue; const aValue: Double; const aDefault: Double);
    procedure SetValue(const aIndex: IDynamicValue; const aValue: TDateTime);
    // Just because you override the constructor doesn't mean there's some other
    // way of getting an invalid backing in. Deserialize can also provide a bad
    // one, and that can be called after the construction. So, override your
    // constructor to get compile-time type checks, and override this
    // to confirm the serialized stuff.
    procedure InitializeBacking(const aBacking: IDynamicValue); virtual;
  public
    constructor Initialize(const aBacking: IDynamicValue);
    class function Deserialize(aReader: TDynamicValueReader): IDynamicValue; overload;
    class function Deserialize(aStream: TStream): IDynamicValue; overload;
    class function Deserialize(aString: UTF8String): IDynamicValue; overload;
    procedure Serialize(aWriter: TDynamicValueWriter); overload;
    procedure Serialize(aStream: TStream; aIndent: Longint = 0); overload;
    function Serialize(aIndent: Longint = 0): UTF8String; overload;
  end;

  { TDataStoreMap2 }

  TDataStoreMap2 = object(TDataStoreObject2)
  protected
    procedure InitializeBacking(const aBacking: IDynamicValue); virtual;
  public
    constructor Initialize(const aBacking: IDynamicMap);
  end;

  { TDataStoreList2 }

  TDataStoreList2 = object(TDataStoreObject2)
  protected
    procedure InitializeBacking(const aBacking: IDynamicValue); virtual;
  public
    constructor Initialize(const aBacking: IDynamicList);
  end;

  { TDataStoreMap }

  TDataStoreMap = class(TDataStoreObject)
  private
    // backing is not visible outside by default, if you want it to be, you
    // have to handle it yourself.
    fBacking: IDynamicMap;
  protected
    property Backing: IDynamicMap read fBacking;
    function ReadManagedKey(const {%H-}aKey: UTF8String; {%H-}aReader: TDynamicValueReader): Boolean; virtual;
    procedure WriteManagedKeys({%H-}aWriter: TDynamicValueWriter); virtual;
  public
    procedure Serialize(aWriter: TDynamicValueWriter); override;
    procedure Deserialize(aReader: TDynamicValueReader); override;
  end;

  { TDataStoreList }
  TDataStoreList = class(TDataStoreObject)
  private
    fBacking: IDynamicList;
  protected
    property Backing: IDynamicList read fBacking;
    function ReadManagedItem({%H-}aReader: TDynamicValueReader): Boolean; virtual;
    procedure WriteManagedItems({%H-}aWriter: TDynamicValueWriter); virtual;
  public
    procedure Serialize(aWriter: TDynamicValueWriter); override;
    procedure Deserialize(aReader: TDynamicValueReader); override;
  end;

implementation

uses
  sys_dynval_json, sys_types;

{ TDataStoreList2 }

procedure TDataStoreList2.InitializeBacking(const aBacking: IDynamicValue);
begin
  if aBacking is IDynamicList then
     inherited InitializeBacking(aBacking)
  else
     inherited InitializeBacking(TDynamicValues.NewList);

end;

constructor TDataStoreList2.Initialize(const aBacking: IDynamicList);
begin
     inherited Initialize(aBacking)
end;

{ TDataStoreMap2 }

procedure TDataStoreMap2.InitializeBacking(const aBacking: IDynamicValue);
begin
  if aBacking is IDynamicMap then
     inherited InitializeBacking(aBacking)
  else
     inherited InitializeBacking(TDynamicValues.NewMap);
end;

constructor TDataStoreMap2.Initialize(const aBacking: IDynamicMap);
begin
   inherited Initialize(aBacking)

end;

{ TDataStoreObject2 }

function TDataStoreObject2.GetString(const aIndex: IDynamicValue;
  const aDefault: UTF8String): UTF8String;
var
  lResult: IDynamicValue;
begin
  lResult := fBacking[aIndex];
  if lResult is IDynamicString then
     result := IDynamicString(lResult).Value
  else
     result := aDefault;
end;

function TDataStoreObject2.GetInteger(const aIndex: IDynamicValue;
  const aDefault: Longint): Longint;
var
  lResult: IDynamicValue;
begin
  lResult := fBacking[aIndex];
  if lResult is IDynamicNumber then
     result := trunc(IDynamicNumber(lResult).Value)
  else
     result := aDefault;
end;

function TDataStoreObject2.GetBoolean(const aIndex: IDynamicValue;
  const aDefault: Boolean): Boolean;
var
  lResult: IDynamicValue;
begin
  lResult := fBacking[aIndex];
  if lResult is IDynamicBoolean then
     result := IDynamicBoolean(lResult).Value
  else
     result := aDefault;
end;

function TDataStoreObject2.GetFloat(const aIndex: IDynamicValue;
  const aDefault: Double): Double;
var
  lResult: IDynamicValue;
begin
  lResult := fBacking[aIndex];
  if lResult is IDynamicNumber then
     result := IDynamicNumber(lResult).Value
  else
     result := aDefault;
end;

function TDataStoreObject2.GetMap(const aIndex: IDynamicValue): IDynamicMap;
var
  lResult: IDynamicValue;
begin
  lResult := fBacking[aIndex];
  if lResult is IDynamicMap then
  begin
    result := IDynamicMap(lResult);
  end
  else
  begin
    result := TDynamicValues.NewMap;
    fBacking[aIndex] := result;
  end;
end;

function TDataStoreObject2.GetList(const aIndex: IDynamicValue): IDynamicList;
var
  lResult: IDynamicValue;
begin
  lResult := fBacking[aIndex];
  if lResult is IDynamicList then
  begin
    result := IDynamicList(lResult);
  end
  else
  begin
    result := TDynamicValues.NewList;
    fBacking[aIndex] := result;
  end;
end;

function TDataStoreObject2.GetDateTime(const aIndex: IDynamicValue): TDateTime;
var
  lString: UTF8String;
begin
  lString := GetString(aIndex,'');
  try
    result := ISO8601ToDateTime(lString);
  except
    on E: EConvertError do
      result := 0;
  end;
end;

procedure TDataStoreObject2.SetValue(const aIndex: IDynamicValue;
  const aValue: UTF8String; const aDefault: UTF8String);
begin
  if (aValue = aDefault) and (fBacking is IDynamicMap) and (aIndex is IDynamicString) then
     IDynamicMap(fBacking).Delete(IDynamicString(aIndex).Value)
  else
     fBacking[aIndex] := aValue;
end;

procedure TDataStoreObject2.SetValue(const aIndex: IDynamicValue;
  const aValue: Longint; const aDefault: Longint);
begin
  if (aValue = aDefault) and (fBacking is IDynamicMap) and (aIndex is IDynamicString) then
     IDynamicMap(fBacking).Delete(IDynamicString(aIndex).Value)
  else
     fBacking[aIndex] := aValue;
end;

procedure TDataStoreObject2.SetValue(const aIndex: IDynamicValue;
  const aValue: Boolean; const aDefault: Boolean);
begin
  if (aValue = aDefault) and (fBacking is IDynamicMap) and (aIndex is IDynamicString) then
     IDynamicMap(fBacking).Delete(IDynamicString(aIndex).Value)
  else
     fBacking[aIndex] := aValue;

end;

procedure TDataStoreObject2.SetValue(const aIndex: IDynamicValue;
  const aValue: Double; const aDefault: Double);
begin
  if (aValue = aDefault) and (fBacking is IDynamicMap) and (aIndex is IDynamicString) then
     IDynamicMap(fBacking).Delete(IDynamicString(aIndex).Value)
  else
     fBacking[aIndex] := aValue;

end;

procedure TDataStoreObject2.SetValue(const aIndex: IDynamicValue;
  const aValue: TDateTime);
begin
  SetValue(aIndex,DateTimeToISO8601(aValue,true),'');
end;

procedure TDataStoreObject2.InitializeBacking(const aBacking: IDynamicValue);
begin
  fBacking := aBacking;
end;

constructor TDataStoreObject2.Initialize(const aBacking: IDynamicValue);
begin
  InitializeBacking(aBacking);
end;

class function TDataStoreObject2.Deserialize(aReader: TDynamicValueReader
  ): IDynamicValue;
begin
  result := aReader.ReadValue;
end;

class function TDataStoreObject2.Deserialize(aStream: TStream): IDynamicValue;
var
  lReader: TJSONReader;
begin
  lReader := TJSONReader.Create(aStream);
  try
    result := Deserialize(lReader);
  finally
    lReader.Free;
  end;
end;

class function TDataStoreObject2.Deserialize(aString: UTF8String
  ): IDynamicValue;
var
  lStream: TStringStream;
begin
  lStream := TStringStream.Create(aString);
  try
    result := Deserialize(lStream);

  finally
    lStream.Free;
  end;

end;

procedure TDataStoreObject2.Serialize(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteValue(fBacking);
end;

procedure TDataStoreObject2.Serialize(aStream: TStream; aIndent: Longint);
var
  lWriter: TJSONWriter;
begin
  lWriter := TJSONWriter.Create(aStream,aIndent);
  try
    Serialize(lWriter);
  finally
    lWriter.Free;
  end;

end;

function TDataStoreObject2.Serialize(aIndent: Longint): UTF8String;
var
  lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  try
    Serialize(lStream,aIndent);
    result := lStream.DataString;
  finally
    lStream.Free;
  end;

end;

{ TDataStoreObject }

constructor TDataStoreObject.Create;
begin
  inherited Create;
  InitializeBlank;
end;

procedure TDataStoreObject.Serialize(aStream: TStream);
var
  lWriter: TJSONWriter;
begin
  lWriter := TJSONWriter.Create(aStream,0);
  try
    Serialize(lWriter);
  finally
    lWriter.Free;
  end;

end;

function TDataStoreObject.Serialize: UTF8String;
var
  lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  try
    Serialize(lStream);
    result := lStream.DataString;
  finally
    lStream.Free;
  end;

end;

procedure TDataStoreObject.Deserialize(aStream: TStream);
var
  lReader: TJSONReader;
begin
  lReader := TJSONReader.Create(aStream);
  try
    Deserialize(lReader);
  finally
    lReader.Free;
  end;
end;

procedure TDataStoreObject.Deserialize(aString: UTF8String);
var
  lStream: TStringStream;
begin
  lStream := TStringStream.Create(aString);
  try
    Deserialize(lStream);

  finally
    lStream.Free;
  end;

end;


{ TDataStoreMap }

function TDataStoreMap.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  result := false;
end;

procedure TDataStoreMap.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  // do nothing... subclass handles this...
end;

procedure TDataStoreMap.Serialize(aWriter: TDynamicValueWriter);
var
  lEnum: IDynamicMapEnumerator;
begin
  aWriter.WriteMapStart(fBacking);
  WriteManagedKeys(aWriter);
  lEnum := fBacking.Enumerate;
  while lEnum.Next do
  begin
    aWriter.WriteKeyValue(lEnum.Key,lEnum.Value);
  end;
  aWriter.WriteMapEnd;
end;

procedure TDataStoreMap.Deserialize(aReader: TDynamicValueReader);
var
  lKey: UTF8String;
begin
  fBacking := TDynamicValues.NewMap;
  InitializeBlank;
  if aReader <> nil then
  begin;
    aReader.ReadMapStart;
    while not aReader.IsMapEnd do
    begin
      lKey := aReader.ReadMapKey;
      if not ReadManagedKey(lKey,aReader) then
         fBacking[lKey] := aReader.ReadValue;
    end;
    aReader.ReadMapEnd;

  end
end;


{ TDataStoreList }

function TDataStoreList.ReadManagedItem(aReader: TDynamicValueReader): Boolean;
begin
  result := false;
end;

procedure TDataStoreList.WriteManagedItems(aWriter: TDynamicValueWriter);
begin
  // do nothing... subclass handles this...
end;

procedure TDataStoreList.Serialize(aWriter: TDynamicValueWriter);
var
  i: Longint;
  l: Longint;
begin
  aWriter.WriteListStart(fBacking);
  WriteManagedItems(aWriter);
  l := fBacking.Length - 1;
  for i := 0 to l do
  begin
    aWriter.WriteValue(fBacking[i]);
  end;
  aWriter.WriteListEnd;
end;

procedure TDataStoreList.Deserialize(aReader: TDynamicValueReader);
begin
  fBacking := TDynamicValues.NewList;
  InitializeBlank;
  if aReader <> nil then
  begin
    aReader.ReadListStart;
    // basically, two loops. The first one gives us a chance to read items
    // that are managed, up until the first item that isn't manageable.
    while not aReader.IsListEnd do
    begin
      if not ReadManagedItem(aReader) then
         break;
    end;
    // the second loop reads in all of the unmanaged ones. It's possible
    // that this one contains values that *could* be managed, but if we
    // put those in, then we lose the original order.
    while not aReader.IsListEnd do
    begin
      // read the unmanaged items.
      fBacking.Add(aReader.ReadValue);
    end;
    aReader.ReadListEnd;
  end
end;



end.

