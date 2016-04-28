unit sys_dynobject_implementation;

{$mode objfpc}{$H+}

interface



uses
  Classes, SysUtils, sys_types, sys_dynobject;

type

  { TDynamicValue }

  TDynamicValue = class(TInterfacedObject, IDynamicValue)
  public
    function Owns({%H-}aValue: IDynamicValue): Boolean; virtual;
    function IsEqualTo({%H-}aValue: IDynamicValue): Boolean; virtual;
    function IsDefined: Boolean; virtual;
  end;

  { TDynamicNull }

  TDynamicNull = class(TDynamicValue,IDynamicNull)
  public
    function IsEqualTo(aValue: IDynamicValue): Boolean; override;
  end;

  { TDynamicBoolean }

  TDynamicBoolean = class(TDynamicValue,IDynamicBoolean)
  strict private
    fValue: Boolean;
    function GetValue: Boolean;
  public
    constructor Create(aValue: Boolean);
    property Value: Boolean read GetValue;
    function IsEqualTo(aValue: IDynamicValue): Boolean; override;
  end;

  { TDynamicNumber }

  TDynamicNumber = class(TDynamicValue,IDynamicNumber)
  strict private
    fValue: Double;
    function GetValue: Double;
  public
    constructor Create(aValue: Double);
    property Value: Double read GetValue;
    function IsEqualTo(aValue: IDynamicValue): Boolean; override;
  end;

  { TDynamicString }

  TDynamicString = class(TDynamicValue,IDynamicString)
  strict private
    fValue: UTF8String;
    function GetValue: UTF8String;
  public
    constructor Create(aValue: UTF8String);
    property Value: UTF8String read GetValue;
    function IsEqualTo(aValue: IDynamicValue): Boolean; override;
  end;

  TDynamicValueArray = array of IDynamicValue;

  { TDynamicList }

  TDynamicList = class(TDynamicValue,IDynamicList)
  strict private
    fList: TDynamicValueArray;
    function GetItem(aIndex: Longint): IDynamicValue;
    function GetLength: Longint;
    procedure SetItem(aIndex: Longint; AValue: IDynamicValue);
    procedure SetLength(AValue: Longint);
  public
    constructor Create;
    property Item[aIndex: Longint]: IDynamicValue read GetItem write SetItem; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(aItem: IDynamicValue);
    procedure Delete(aIndex: Longint);
    procedure Clear;
    function IndexOf(aValue: IDynamicValue): Longint;
    function IsEqualTo(aValue: IDynamicValue): Boolean; override;
    function Owns(aValue: IDynamicValue): Boolean; override;

  end;

  TNamedValue = record
    Name: UTF8String;
    Value: IDynamicValue;
  end;
  TNamedValueArray = array of TNamedValue;

  TDynamicMap = class;

  TDynamicMapEnumerator = class(TInterfacedObject,IDynamicMapEnumerator)
  private
    fIndex: Longint;
    fMap: TDynamicMap;
    function GetKey: UTF8String;
    function GetValue: IDynamicValue;
  public
    constructor Create(aMap: TDynamicMap);
    function Next: Boolean;
    property Key: UTF8String read GetKey;
    property Value: IDynamicValue read GetValue;
  end;

  { TDynamicMap }

  {
  I could have implemented this as a hash table. However, I have no plans to
  use the maps for large numbers of keys, so lookups are not expected to be very
  costly. Hashtables allow us to trade memory space and simplicity for speed,
  but only when that speed is actually going to be worth it. I'm not certain
  it is worth it at this point.
  }
  TDynamicMap = class(TDynamicValue,IDynamicMap)
  private
    fList: TNamedValueArray;
    function GetItem(aKey: UTF8String): IDynamicValue;
    procedure SetItem(aKey: UTF8String; AValue: IDynamicValue);
    function Find(aKey: UTF8String): Longint;
  public
    constructor Create;
    property Item[aKey: UTF8String]: IDynamicValue read GetItem write SetItem; default;
    function GetKeys: TStringArray;
    function Has(aKey: UTF8String): Boolean;
    procedure Delete(aKey: UTF8String);
    procedure Clear;
    function Enumerate: IDynamicMapEnumerator;
    function IsEqualTo(aValue: IDynamicValue): Boolean; override;
    function Owns(aValue: IDynamicValue): Boolean; override;
  end;

implementation

{ TDynamicMap.TDynamicMapEnumerator }

constructor TDynamicMapEnumerator.Create(aMap: TDynamicMap);
begin
  inherited Create;
  fMap := aMap;
  fIndex := -1;
end;

function TDynamicMapEnumerator.GetKey: UTF8String;
begin
  result := fMap.fList[fIndex].Name;
end;

function TDynamicMapEnumerator.GetValue: IDynamicValue;
begin
  result := fMap.fList[fIndex].Value;
end;

function TDynamicMapEnumerator.Next: Boolean;
begin
  inc(fIndex);
  result := fIndex < System.Length(fMap.fList);;
end;

{ TDynamicMap }

function TDynamicMap.GetItem(aKey: UTF8String): IDynamicValue;
var
  lIndex: Longint;
begin
  lIndex := Find(aKey);
  if lIndex > -1 then
  begin
     result := fList[lIndex].Value;
  end
  else
  begin
    // return an undefined value
    result := TDynamicValues.Undefined;
  end;
end;

procedure TDynamicMap.SetItem(aKey: UTF8String; AValue: IDynamicValue);
var
  lNamedValue: TNamedValue;
  lIndex: LongInt;
  l: Longint;
begin
  if AValue.Owns(Self) then
  begin
     raise Exception.Create('Attempt to create a circular reference in a dynamic map');
  end;
  if not AValue.IsDefined then
  begin
     raise Exception.Create('Attempt to add an undefined value to a dynamic map');
  end;
  lIndex := Find(aKey);
  if lIndex > -1 then
  begin
     fList[lIndex].Value := AValue;
  end
  else
  begin
    lNamedValue.Name := aKey;
    lNamedValue.Value := AValue;
    l := System.Length(fList);
    SetLength(fList,l + 1);
    fList[l] := lNamedValue;
  end;
end;

function TDynamicMap.Find(aKey: UTF8String): Longint;
var
  l: Longint;
  i: Longint;
begin
  result := -1;
  l := System.Length(fList) - 1;
  for i := 0 to l do
  begin
    if fList[i].Name = aKey then
    begin
       result := i;
       break;
    end;
  end;
end;

constructor TDynamicMap.Create;
begin
  inherited Create;
  SetLength(fList,0);
end;

function TDynamicMap.GetKeys: TStringArray;
var
  l: Longint;
  i: Longint;
begin
  l := System.Length(fList);
  SetLength(Result,l);
  for i := 0 to l - 1 do
  begin
    Result[i] := fList[i].Name;
  end;
end;

function TDynamicMap.Has(aKey: UTF8String): Boolean;
begin
  result := Find(aKey) > -1;

end;

procedure TDynamicMap.Delete(aKey: UTF8String);
var
  l: Longint;
  lIndex: Longint;
  i: Longint;
begin
  lIndex := Find(aKey);
  if lIndex > -1 then
  begin
     l := System.Length(fList);
     for i := lIndex to l - 2 do
     begin
       fList[i] := fList[i + 1];
     end;
     SetLength(fList,l - 1);
  end;
end;

procedure TDynamicMap.Clear;
begin
  SetLength(fList,0);

end;

function TDynamicMap.Enumerate: IDynamicMapEnumerator;
begin
  result := TDynamicMapEnumerator.Create(Self);
end;

function TDynamicMap.IsEqualTo(aValue: IDynamicValue): Boolean;
begin
  if aValue is IDynamicMap then
     result := (aValue as IDynamicMap) = (Self as IDynamicMap);
end;

function TDynamicMap.Owns(aValue: IDynamicValue): Boolean;
var
  i: Longint;
  l: Longint;
begin
  result := false;
  l := Length(fList) - 1;
  for i := 0 to l do
  begin
    result := fList[i].Value.IsEqualTo(aValue) or
       fList[i].Value.Owns(aValue);
    if result then
       break;
  end;
end;

{ TDynamicList }

function TDynamicList.GetItem(aIndex: Longint): IDynamicValue;
begin
   if (aIndex >= 0) and (aIndex < Length) then
      result := fList[aIndex]
   else
      result := TDynamicValues.Undefined;
end;

function TDynamicList.GetLength: Longint;
begin
  result := System.Length(fList);

end;

procedure TDynamicList.SetItem(aIndex: Longint; AValue: IDynamicValue);
begin
  if (aIndex >= 0) then
  begin
     if AValue.Owns(Self) then
     begin
        raise Exception.Create('Attempt to create a circular reference in a dynamic list');
     end;
     if not AValue.IsDefined then
     begin
        raise Exception.Create('Attempt to add an undefined value to a dynamic list');
     end;
     if aIndex >= Length then
     begin
        // only increase the length to one less than index, so we
        // don't create a null value we're just going to throw away.
        Length := aIndex;
        System.SetLength(fList,Length + 1);
     end;
     fList[aIndex] := AValue;
  end
  else
    raise ERangeError.Create('Invalid index to set in dynamic list: ' + IntToStr(aIndex));
end;

procedure TDynamicList.SetLength(AValue: Longint);
var
  l: Longint;
begin
  l := Length;
  System.SetLength(fList,AValue);
  while (aValue > l) do
  begin
    fList[l] := TDynamicValues.Null;
    inc(l);
  end;

end;

constructor TDynamicList.Create;
begin
  inherited Create;
  System.SetLength(fList,0);
end;

procedure TDynamicList.Add(aItem: IDynamicValue);
begin
  SetItem(Length,aItem);
end;

procedure TDynamicList.Delete(aIndex: Longint);
var
  l: Longint;
  i: Longint;
begin
  l := Length;
  for i := aIndex to l - 2 do
  begin
    fList[i] := fList[i + 1];
  end;
  System.SetLength(fList,l - 1);
end;

procedure TDynamicList.Clear;
begin
  System.SetLength(fList,0);

end;

function TDynamicList.IndexOf(aValue: IDynamicValue): Longint;
var
  l: Longint;
  i: longint;
begin
  result := -1;
  l := System.Length(fList) - 1;
  for i := 0 to l do
  begin
    if fList[i] = aValue then
    begin
       result := i;
       break;
    end;
  end;
end;

function TDynamicList.IsEqualTo(aValue: IDynamicValue): Boolean;
begin
  Result:= (aValue is IDynamicList);
  if result then
  begin
    result := (aValue as IDynamicList) = (Self as IDynamicList);
  end;
end;

function TDynamicList.Owns(aValue: IDynamicValue): Boolean;
var
  i: Longint;
  l: Longint;
begin
  result := false;
  l := Length - 1;
  for i := 0 to l do
  begin
    result := fList[i].IsEqualTo(aValue) or
       fList[i].Owns(aValue);
    if result then
       break;
  end;

end;

{ TDynamicString }

function TDynamicString.GetValue: UTF8String;
begin
  result := fValue;
end;

constructor TDynamicString.Create(aValue: UTF8String);
begin
  inherited Create;
  fValue := aValue;
end;

function TDynamicString.IsEqualTo(aValue: IDynamicValue): Boolean;
begin
  Result:= (aValue is IDynamicString) and ((aValue as IDynamicString).Value = Value);
end;

{ TDynamicNumber }

function TDynamicNumber.GetValue: Double;
begin
  result := fValue;
end;

constructor TDynamicNumber.Create(aValue: Double);
begin
  inherited Create;
  fValue := aValue;

end;

function TDynamicNumber.IsEqualTo(aValue: IDynamicValue): Boolean;
begin
  Result:= (aValue is IDynamicNumber) and ((aValue as IDynamicNumber).Value = Value);
end;

{ TDynamicNull }

function TDynamicNull.IsEqualTo(aValue: IDynamicValue): Boolean;
begin
  Result:= (aValue is IDynamicNull);
end;

{ TDynamicBoolean }

function TDynamicBoolean.GetValue: Boolean;
begin
  result := fValue;
end;

constructor TDynamicBoolean.Create(aValue: Boolean);
begin
  inherited Create;
  fValue := aValue;

end;

function TDynamicBoolean.IsEqualTo(aValue: IDynamicValue): Boolean;
begin
  Result:= (aValue is IDynamicBoolean) and ((aValue as IDynamicBoolean).Value = Value);
end;

{ TDynamicValue }

function TDynamicValue.Owns(aValue: IDynamicValue): Boolean;
begin
  result := false;
end;

function TDynamicValue.IsEqualTo(aValue: IDynamicValue): Boolean;
begin
  result := false;
end;

function TDynamicValue.IsDefined: Boolean;
begin
  result :=
    (Self is IDynamicNull) or
    (Self is IDynamicBoolean) or
    (Self is IDynamicNumber) or
    (Self is IDynamicString) or
    (self is IDynamicList) or
    (Self is IDynamicMap);
end;

initialization

end.

