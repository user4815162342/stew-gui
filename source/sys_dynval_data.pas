unit sys_dynval_data;

{$mode objfpc}{$H+}

interface

{
TODO: It seems to me that the class-style is the better idea. If only because the
'object' style is poorly supported, and therefore may not advance with the future.

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

