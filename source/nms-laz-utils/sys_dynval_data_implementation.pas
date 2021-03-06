unit sys_dynval_data_implementation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_dynval, sys_dynval_implementation, sys_dynval_data, sys_types;

type
  { TDataStoreObject }

  // NOTE: I'm not exactly sure what is going on here. IDataStoreObject descends
  // from IDynamicObject already. However, if I don't put IDynamicObject in this
  // implements list, then the various descendants don't register as implementing
  // IDynamicObject. It may be possible that the 'is' operator, or Supports method,
  // or any of those, only look at interfaces implemented directly by a class,
  // and ignore interfaces implemented by interfaces. That would be a bug in
  // FreePascal. At some point, once I move beyond to a new version of FPC,
  // try this again and see if it works.
  TDataStoreObject = class(TDynamicValue, IDataStoreObject, IDynamicObject)
  strict protected
    procedure InitializeBlank; virtual; abstract;
    function GetKindOf: TDynamicValueKind; override;
    procedure BuildClone(var aValue: TDynamicValue); override;
  public
    constructor Create;
    procedure Serialize(aWriter: TDynamicValueWriter); virtual; abstract; overload;
    procedure Serialize(aStream: TStream; const aIndent: Longint); overload;
    function Serialize: UTF8String; overload;
    procedure Deserialize(aReader: TDynamicValueReader); virtual; abstract; overload;
    procedure Deserialize(aStream: TStream); overload;
    procedure Deserialize(aString: UTF8String); overload;
  end;

  { TDataStoreMap }

  // Data store object that uses a map as a backing to store the unknown keys,
  // and also acts as a map for the data it has. The backing data is not available
  // to the outside, and can't be edited, but the editable data is available by
  // key. It is up to the implementation for how to store the accessible data.
  // ** Although it behaves like a map, I can't justify implementing IDynamicMap,
  // because that will make the Readers and Writers be able to treat it like one,
  // which it shouldn't.

  TDataStoreMap = class(TDataStoreObject)
  private
    // backing is not visible outside by default, if you want it to be, you
    // have to handle it yourself.
    fBacking: IDynamicMap;
  strict protected
    property Backing: IDynamicMap read fBacking;
    procedure InitializeBlank; override;
    function ReadManagedKey(const {%H-}aKey: UTF8String; {%H-}aReader: TDynamicValueReader): Boolean; virtual;
    procedure WriteManagedKeys({%H-}aWriter: TDynamicValueWriter); virtual;
    procedure ListManagedKeys(var {%H-}aValue: TStringArray2); virtual;
    function GetItem(const {%H-}aKey: UTF8String): IDynamicValue; overload; virtual;
    procedure SetItem(const {%H-}aKey: UTF8String; const {%H-}AValue: IDynamicValue); overload; virtual;
    function GetItem(const {%H-}aKey: IDynamicValue): IDynamicValue; override; overload;
    procedure SetItem(const {%H-}aKey: IDynamicValue; const {%H-}AValue: IDynamicValue); override; overload;
    procedure BuildClone(var aValue: TDynamicValue); override;
  public
    procedure Serialize(aWriter: TDynamicValueWriter); override;
    procedure Deserialize(aReader: TDynamicValueReader); override;
    function Owns(const {%H-}aValue: IDynamicValue): Boolean; override;
    function IsStructurallyEqualTo(const {%H-}aValue: IDynamicValue): Boolean; override;
    function IsEqualTo(const {%H-}aValue: IDynamicValue): Boolean; override;
    property Item[aKey: UTF8String]: IDynamicValue read GetItem write SetItem; default;
  end;

  { TDataStoreList }
  // Data store object that uses a map as a backing to store the unknown keys,
  // and also acts as a listfor the data it has. The backing data is not available
  // to the outside, and can't be edited, but the editable data is available by
  // key. It is up to the implementation for how to store the accessible data.
  // ** Although it behaves like a list, I can't justify implementing IDynamicList,
  // because that will make the Readers and Writers be able to treat it like one,
  // which it shouldn't.


  TDataStoreList = class(TDataStoreObject)
  private
    fItems: IDynamicList;
  strict protected
    procedure InitializeBlank; override;
    function ReadManagedItem(aReader: TDynamicValueReader): IDynamicValue; virtual; abstract;
    property Items: IDynamicList read fItems;
    procedure CheckInsertingItem(aValue: IDynamicValue); virtual; abstract;
    function GetItem(const aKey: Longint): IDynamicValue; virtual;
    procedure SetItem(const aKey: Longint; aValue: IDynamicValue); virtual;
    function GetItem(const {%H-}aKey: IDynamicValue): IDynamicValue; override; overload;
    procedure SetItem(const {%H-}aKey: IDynamicValue; const {%H-}AValue: IDynamicValue); override; overload;
    procedure BuildClone(var aValue: TDynamicValue); override;
  public
    procedure Serialize(aWriter: TDynamicValueWriter); override;
    procedure Deserialize(aReader: TDynamicValueReader); override;
    function Owns(const {%H-}aValue: IDynamicValue): Boolean; override;
    function IsStructurallyEqualTo(const {%H-}aValue: IDynamicValue): Boolean; override;
    function IsEqualTo(const {%H-}aValue: IDynamicValue): Boolean; override;
  end;


implementation

uses
  sys_dynval_json;

{ TDataStoreObject }

constructor TDataStoreObject.Create;
begin
  inherited Create;
  InitializeBlank;
end;

procedure TDataStoreObject.Serialize(aStream: TStream; const aIndent: Longint);
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

function TDataStoreObject.Serialize: UTF8String;
var
  lStream: TStringStream;
begin
  lStream := TStringStream.Create('');
  try
    Serialize(lStream,0);
    result := lStream.DataString;
  finally
    lStream.Free;
  end;

end;

procedure TDataStoreObject.Deserialize(aStream: TStream);
var
  lReader: TJSONReader;
begin
  if aStream <> nil then
  begin
    lReader := TJSONReader.Create(aStream);
    try
      Deserialize(lReader);
    finally
      lReader.Free;
    end;

  end
  else
  begin
    Deserialize((TDynamicValueReader(nil)));
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

function TDataStoreObject.GetKindOf: TDynamicValueKind;
begin
  Result:=dvkObject;
end;

procedure TDataStoreObject.BuildClone(var aValue: TDynamicValue);
begin
  if aValue = nil then
    raise Exception.Create('Can''t clone a TDataStoreObject directly, it''s abstract');
  inherited BuildClone(aValue);
end;


{ TDataStoreMap }

procedure TDataStoreMap.InitializeBlank;
begin
  fBacking := TDynamicValues.NewMap;
end;

function TDataStoreMap.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  result := false;
end;

procedure TDataStoreMap.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  // do nothing... subclass handles this...
end;

procedure TDataStoreMap.ListManagedKeys(var aValue: TStringArray2);
begin
  // nothing yet...
end;

function TDataStoreMap.GetItem(const aKey: UTF8String): IDynamicValue;
begin
  result := TDynamicValues.Undefined;
end;

procedure TDataStoreMap.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);
begin
  // do nothing...
end;

function TDataStoreMap.GetItem(const aKey: IDynamicValue): IDynamicValue;
begin
  if aKey is IDynamicString then
  begin
    result := GetItem(IDynamicString(aKey).Value);
    Exit;
  end;
  Result:=inherited GetItem(aKey);
end;

procedure TDataStoreMap.SetItem(const aKey: IDynamicValue;
  const AValue: IDynamicValue);
begin
  if aKey is IDynamicString then
  begin
    SetItem(IDynamicString(aKey).Value,AValue);
    Exit;
  end;
  inherited SetItem(aKey, AValue);
end;

procedure TDataStoreMap.BuildClone(var aValue: TDynamicValue);
begin
  if aValue = nil then
    aValue := TDataStoreMap.Create;
  (aValue as TDataStoreMap).fBacking := fBacking.Clone as IDynamicMap;
  inherited BuildClone(aValue);
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

function TDataStoreMap.Owns(const aValue: IDynamicValue): Boolean;
var
  lKeys: TStringArray2;
  i: Longint;
  l: Longint;
  lItem: IDynamicValue;
begin
  result := fBacking.Owns(aValue);
  if not result then
  begin
    {%H-}lKeys.Count := 0;
    ListManagedKeys(lKeys);
    l := lKeys.Count;
    for i := 0 to l - 1 do
    begin
      lItem := Item[lKeys[i]];
      result := (lItem = aValue) or
                (lItem.Owns(aValue));
      if result then
         break;
    end;

  end;
end;

function TDataStoreMap.IsStructurallyEqualTo(const aValue: IDynamicValue
  ): Boolean;
begin
  // TODO: I don't know for sure that I even need this, so I simply won't
  // support it.
  result := false;
end;

function TDataStoreMap.IsEqualTo(const aValue: IDynamicValue): Boolean;
begin
  result := false;
end;


{ TDataStoreList }

procedure TDataStoreList.InitializeBlank;
begin
  fItems := TDynamicValues.NewList;
end;

function TDataStoreList.GetItem(const aKey: Longint): IDynamicValue;
begin
  result := fItems.GetItem(aKey);
end;

procedure TDataStoreList.SetItem(const aKey: Longint; aValue: IDynamicValue);
begin
  CheckInsertingItem(aValue);
  fItems.SetItem(aKey,aValue)
end;

procedure TDataStoreList.Serialize(aWriter: TDynamicValueWriter);
var
  i: Longint;
  l: Longint;
begin
  aWriter.WriteListStart(fItems);
  l := fItems.Length - 1;
  for i := 0 to l do
  begin
    if fItems[i] is IDataStoreObject then
       (fItems[i] as IDataStoreObject).Serialize(aWriter)
    else
       aWriter.WriteValue(fItems[i]);
  end;
  aWriter.WriteListEnd;
end;

procedure TDataStoreList.Deserialize(aReader: TDynamicValueReader);
begin
  InitializeBlank;
  if aReader <> nil then
  begin
    aReader.ReadListStart;
    while not aReader.IsListEnd do
    begin
      fItems.Add(ReadManagedItem(aReader));
    end;
    aReader.ReadListEnd;
  end
end;

function TDataStoreList.Owns(const aValue: IDynamicValue): Boolean;
begin
  result := fItems.Owns(aValue);
end;

function TDataStoreList.IsStructurallyEqualTo(const aValue: IDynamicValue
  ): Boolean;
begin
  // TODO: I don't know for sure that I even need this, so I simply won't
  // support it.
  result := false;
end;

function TDataStoreList.IsEqualTo(const aValue: IDynamicValue): Boolean;
begin
  result := false;
end;

function TDataStoreList.GetItem(const aKey: IDynamicValue): IDynamicValue;
var
  lIndexFloat: Double;
  lIndex: LongInt;
begin
  if aKey is IDynamicNumber then
  begin
     lIndexFloat := IDynamicNumber(aKey).Value;
     lIndex := trunc(lIndexFloat);
     if lIndex = lIndexFloat then
     begin
       result := GetItem(lIndex);
       Exit;
     end;
  end;
  result := inherited GetItem(aKey);
end;

procedure TDataStoreList.SetItem(const aKey: IDynamicValue;
  const AValue: IDynamicValue);
var
  lIndexFloat: Double;
  lIndex: LongInt;
begin
  if aKey is IDynamicNumber then
  begin
     lIndexFloat := IDynamicNumber(aKey).Value;
     lIndex := trunc(lIndexFloat);
     if lIndex = lIndexFloat then
     begin
       SetItem(lIndex,AValue);
       Exit;
     end;
  end;
  inherited SetItem(aKey,AValue);
end;

procedure TDataStoreList.BuildClone(var aValue: TDynamicValue);
begin
  if aValue = nil then
     raise Exception.Create('Can''t clone a TDataStoreList directly, it''s abstract');
  (aValue as TDataStoreList).fItems := fItems.Clone as IDynamicList;
  inherited BuildClone(aValue);
end;

end.

