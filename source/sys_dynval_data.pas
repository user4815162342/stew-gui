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
  TDataObject = class
  public
    procedure Serialize(aWriter: TDynamicValueWriter); virtual; abstract;

  end;

  { TDataMap }

  TDataMap = class(TDataObject)
  private
    // backing is not visible outside by default, if you want it to be, you
    // have to handle it yourself.
    fBacking: IDynamicMap;
  protected
    function ReadManagedKey(const {%H-}aKey: UTF8String; {%H-}aReader: TDynamicValueReader): Boolean; virtual;
    procedure WriteManagedKeys({%H-}aWriter: TDynamicValueWriter); virtual;
  public
    constructor Create(aReader: TDynamicValueReader);
    procedure Serialize(aWriter: TDynamicValueWriter); override;
  end;

  { TDataList }

  TDataList = class(TDataObject)
  private
    fBacking: IDynamicList;
  protected
    function ReadManagedItem({%H-}aReader: TDynamicValueReader): Boolean; virtual;
    procedure WriteManagedItems({%H-}aWriter: TDynamicValueWriter); virtual;
  public
    constructor Create(aReader: TDynamicValueReader);
    procedure Serialize(aWriter: TDynamicValueWriter); override;
  end;

implementation


{ TDataMap }

function TDataMap.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  result := false;
end;

procedure TDataMap.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  // do nothing... subclass handles this...
end;

constructor TDataMap.Create(aReader: TDynamicValueReader);
var
  lKey: UTF8String;
  lHandled: Boolean;
begin
  inherited Create;
  aReader.ReadMapStart;
  fBacking := TDynamicValues.NewMap;
  while not aReader.IsMapEnd do
  begin
    lKey := aReader.ReadMapKey;
    lHandled := ReadManagedKey(lKey,aReader);
    if not lHandled then
       fBacking[lKey] := aReader.ReadValue;
  end;
  aReader.ReadMapEnd;

end;

procedure TDataMap.Serialize(aWriter: TDynamicValueWriter);
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


{ TDataList }

function TDataList.ReadManagedItem(aReader: TDynamicValueReader): Boolean;
begin
  result := false;
end;

procedure TDataList.WriteManagedItems(aWriter: TDynamicValueWriter);
begin
  // do nothing... subclass handles this...
end;

constructor TDataList.Create(aReader: TDynamicValueReader);
var
  lManaged: Boolean;
begin
  inherited Create;
  aReader.ReadListStart;
  fBacking := TDynamicValues.NewList;
  lManaged := true;
  // allow the subclass to read in managed items until the items are no longer
  // managed, then start putting into backing from there.
  while lManaged and not aReader.IsListEnd do
  begin
    lManaged := ReadManagedItem(aReader);
    if not lManaged then
       fBacking.Add(aReader.ReadValue);
  end;
  while not aReader.IsListEnd do
  begin
    fBacking.Add(aReader.ReadValue);
  end;
  aReader.ReadListEnd;
end;

procedure TDataList.Serialize(aWriter: TDynamicValueWriter);
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



end.

