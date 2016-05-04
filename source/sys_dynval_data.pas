unit sys_dynval_data;

{$mode objfpc}{$H+}

interface

{

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
  public
    constructor Create(aReader: TDynamicValueReader);
    procedure Serialize(aWriter: TDynamicValueWriter); virtual; abstract;
    procedure Deserialize(aReader: TDynamicValueReader); virtual; abstract;

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
    function ReadManagedItem({%H-}aReader: TDynamicValueReader): IDynamicValue; virtual;
    procedure WriteManagedItems({%H-}aWriter: TDynamicValueWriter); virtual;
  public
    procedure Serialize(aWriter: TDynamicValueWriter); override;
    procedure Deserialize(aReader: TDynamicValueReader); override;
  end;

implementation

{ TDataStoreObject }

constructor TDataStoreObject.Create(aReader: TDynamicValueReader);
begin
  inherited Create;
  if aReader <> nil then
  begin
    Deserialize(aReader);
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
  aReader.ReadMapStart;
  fBacking := TDynamicValues.NewMap;
  while not aReader.IsMapEnd do
  begin
    lKey := aReader.ReadMapKey;
    if not ReadManagedKey(lKey,aReader) then
       fBacking[lKey] := aReader.ReadValue;
  end;
  aReader.ReadMapEnd;
end;


{ TDataStoreList }

function TDataStoreList.ReadManagedItem(aReader: TDynamicValueReader
  ): IDynamicValue;
begin
  result := nil;
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
var
  lUnmanaged: IDynamicValue;
begin
  aReader.ReadListStart;
  fBacking := TDynamicValues.NewList;
  lUnmanaged := nil;
  // allow the subclass to read in managed items until the items are no longer
  // managed, then start putting into backing from there.
  while (lUnmanaged <> nil) and not aReader.IsListEnd do
  begin
    lUnmanaged := ReadManagedItem(aReader);
    if lUnmanaged <> nil then
       fBacking.Add(lUnmanaged);
  end;
  while not aReader.IsListEnd do
  begin
    fBacking.Add(aReader.ReadValue);
  end;
  aReader.ReadListEnd;
end;



end.

