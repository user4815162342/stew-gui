unit sys_dynval_implementation;

{$mode objfpc}{$H+}

interface



uses
  Classes, SysUtils, sys_types, sys_dynval;

type

  { TDynamicValue }

  TDynamicValue = class(TInterfacedObject, IDynamicValue)
  protected
    function GetItem(const {%H-}aKey: IDynamicValue): IDynamicValue; virtual;
    procedure SetItem(const {%H-}aKey: IDynamicValue; const {%H-}AValue: IDynamicValue); virtual;
  public
    function Owns(const {%H-}aValue: IDynamicValue): Boolean; virtual;
    function IsDefined: Boolean; virtual;
    function EqualsDeeply(const {%H-}aValue: IDynamicValue): Boolean;
    property Item[aKey: IDynamicValue]: IDynamicValue read GetItem write SetItem;
  end;

  { TDynamicNull }

  TDynamicNull = class(TDynamicValue,IDynamicNull)
  public
    function EqualsDeeply(const aValue: IDynamicValue): Boolean;
  end;

  { TDynamicBoolean }

  TDynamicBoolean = class(TDynamicValue,IDynamicBoolean)
  strict private
    fValue: Boolean;
    function GetValue: Boolean;
  public
    constructor Create(aValue: Boolean);
    property Value: Boolean read GetValue;
    function EqualsDeeply(const aValue: IDynamicValue): Boolean;
  end;

  { TDynamicNumber }

  TDynamicNumber = class(TDynamicValue,IDynamicNumber)
  strict private
    fValue: Double;
    function GetValue: Double;
  public
    constructor Create(aValue: Double);
    property Value: Double read GetValue;
    function EqualsDeeply(const aValue: IDynamicValue): Boolean;
  end;

  { TDynamicString }

  TDynamicString = class(TDynamicValue,IDynamicString)
  strict private
    fValue: UTF8String;
    function GetValue: UTF8String;
  public
    constructor Create(aValue: UTF8String);
    property Value: UTF8String read GetValue;
    function EqualsDeeply(const aValue: IDynamicValue): Boolean;
  end;

  TDynamicValueArray = array of IDynamicValue;

  { TDynamicList }

  TDynamicList = class(TDynamicValue,IDynamicList)
  strict private
    fList: TDynamicValueArray;
    function GetItem(const aIndex: Longint): IDynamicValue; overload;
    function GetLength: Longint;
    procedure SetItem(const aIndex: Longint; const AValue: IDynamicValue); overload;
    procedure SetLength(const AValue: Longint);
  protected
    function GetItem(const aKey: IDynamicValue): IDynamicValue; override;
    procedure SetItem(const aKey: IDynamicValue; const AValue: IDynamicValue); override;
  public
    constructor Create;
    property Item[aIndex: Longint]: IDynamicValue read GetItem write SetItem; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(const aItem: IDynamicValue);
    procedure Delete(const aIndex: Longint);
    procedure Clear;
    function IndexOf(const aValue: IDynamicValue): Longint;
    function EqualsDeeply(const aValue: IDynamicValue): Boolean;
    function Owns(const aValue: IDynamicValue): Boolean; override;

  end;

  TNamedValue = record
    Key: UTF8String;
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
    function GetItem(const aKey: UTF8String): IDynamicValue; overload;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue); overload;
    function Find(const aKey: UTF8String): Longint;
  protected
    function GetItem(const aKey: IDynamicValue): IDynamicValue; override;
    procedure SetItem(const aKey: IDynamicValue; const AValue: IDynamicValue); override;
  public
    constructor Create;
    property Item[aKey: UTF8String]: IDynamicValue read GetItem write SetItem; default;
    function GetKeys: TStringArray;
    function Has(const aKey: UTF8String): Boolean;
    procedure Delete(const aKey: UTF8String);
    procedure Clear;
    function Enumerate: IDynamicMapEnumerator;
    function EqualsDeeply(const aValue: IDynamicValue): Boolean;
    function Owns(const aValue: IDynamicValue): Boolean; override;
  end;

  procedure QuickSortNamedValues(var aList: TNamedValueArray; L: Longint; R: Longint);

implementation

uses
  LazUTF8;

// blatantly ripped off from QuickSort in classes unit.
procedure QuickSortNamedValues(var aList: TNamedValueArray; L: Longint; R: Longint);
var
  I: Longint;
  J: Longint;
  P: UTF8String;
  Q: TNamedValue;
begin
 repeat
   I := L;
   J := R;
   P := aList[ (L + R) div 2 ].Key;
   repeat
     while UTF8CompareStr(P,aList[I].Key) > 0 do
       I := I + 1;
     while UTF8CompareStr(P, aList[J].Key) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := aList[I];
       aList[I] := aList[J];
       aList[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if J - L < R - I then
   begin
     if L < J then
       QuickSortNamedValues(aList, L, J);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSortNamedValues(aList, I, R);
     R := J;
   end;
 until L >= R;
end;

{ TDynamicNull }

function TDynamicNull.EqualsDeeply(const aValue: IDynamicValue): Boolean;
begin
  result := aValue is IDynamicNull;
end;

{ TDynamicMap.TDynamicMapEnumerator }

constructor TDynamicMapEnumerator.Create(aMap: TDynamicMap);
begin
  inherited Create;
  fMap := aMap;
  fIndex := -1;
end;

function TDynamicMapEnumerator.GetKey: UTF8String;
begin
  result := fMap.fList[fIndex].Key;
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

function TDynamicMap.GetItem(const aKey: UTF8String): IDynamicValue;
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

procedure TDynamicMap.SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
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
    lNamedValue.Key := aKey;
    lNamedValue.Value := AValue;
    l := System.Length(fList);
    SetLength(fList,l + 1);
    fList[l] := lNamedValue;
  end;
end;

function TDynamicMap.GetItem(const aKey: IDynamicValue): IDynamicValue;
begin
  if aKey is IDynamicString then
  begin
    result := GetItem(IDynamicString(aKey).Value);
    Exit;
  end;
  Result:=inherited GetItem(aKey);
end;

procedure TDynamicMap.SetItem(const aKey: IDynamicValue; const AValue: IDynamicValue);
begin
  if aKey is IDynamicString then
  begin
    SetItem(IDynamicString(aKey).Value,AValue);
    Exit;
  end;
  inherited SetItem(aKey, AValue);
end;

function TDynamicMap.Find(const aKey: UTF8String): Longint;
var
  l: Longint;
  i: Longint;
begin
 result := -1;
 l := System.Length(fList) - 1;
 for i := 0 to l do
 begin
   if fList[i].Key = aKey then
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
    Result[i] := fList[i].Key;
  end;
end;

function TDynamicMap.Has(const aKey: UTF8String): Boolean;
begin
  result := Find(aKey) > -1;

end;

procedure TDynamicMap.Delete(const aKey: UTF8String);
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
  // this isn't going to change the sorting status at all...
end;

procedure TDynamicMap.Clear;
begin
  SetLength(fList,0);
end;

function TDynamicMap.Enumerate: IDynamicMapEnumerator;
begin
  result := TDynamicMapEnumerator.Create(Self);
end;

function TDynamicMap.EqualsDeeply(const aValue: IDynamicValue): Boolean;
var
  lItemsLength: Longint;
  lTheirValueList: TNamedValueArray;

  procedure GetTheirValues;
  var
    lEnum: IDynamicMapEnumerator;
  begin
    lEnum := IDynamicMap(aValue).Enumerate;
    lItemsLength := 0;
    while lEnum.Next do
    begin
       SetLength(lTheirValueList,lItemsLength + 1);
       lTheirValueList[lItemsLength].Key := lEnum.Key;
       lTheirValueList[lItemsLength].Value := lEnum.Value;
       inc(lItemsLength);
    end;
  end;

var
  lMyValues: TNamedValueArray;
  i: Longint;
begin
  result := aValue is IDynamicMap;
  if result then
  begin
    GetTheirValues;
    result := lItemsLength = Length(fList);
    if result and (lItemsLength > 0) then
    begin
      if lItemsLength = 1 then
      begin
        // this is much faster since I don't have to copy and sort at all...
        result := (fList[0].Key = lTheirValueList[0].Key) and
                  (fList[0].Value.EqualsDeeply(lTheirValueList[0].Value));
      end
      else
      begin
        // I don't want to sort the real items, because that might confuse
        // the user. So, make a copy first.
        lMyValues := Copy(fList,0,lItemsLength);
        // this seems to be better than a brute force double loop.
        QuickSortNamedValues(lMyValues,0,lItemsLength - 1);
        QuickSortNamedValues(lTheirValueList,0,lItemsLength - 1);
        for i := 0 to lItemsLength - 1 do
        begin
          result := (lMyValues[i].Key = lTheirValueList[i].Key) and
                    (lMyValues[i].Value.EqualsDeeply(lTheirValueList[i].Value));
          if not result then
             break;

        end;
      end;

    end;
  end;
end;

function TDynamicMap.Owns(const aValue: IDynamicValue): Boolean;
var
  i: Longint;
  l: Longint;
begin
  result := false;
  l := Length(fList) - 1;
  for i := 0 to l do
  begin
    result := (fList[i].Value = aValue) or
       fList[i].Value.Owns(aValue);
    if result then
       break;
  end;
end;

{ TDynamicList }

function TDynamicList.GetItem(const aIndex: Longint): IDynamicValue;
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

procedure TDynamicList.SetItem(const aIndex: Longint; const AValue: IDynamicValue);
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

procedure TDynamicList.SetLength(const AValue: Longint);
var
  l: Longint;
begin
  l := Length;
  System.SetLength(fList,AValue);
  while (aValue > l) do
  begin
    // in this case, assign null. I don't want to assign undefined values here,
    // because then it looks like we're 'containing' undefined values.
    fList[l] := TDynamicValues.Null;
    inc(l);
  end;

end;

function TDynamicList.GetItem(const aKey: IDynamicValue): IDynamicValue;
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

procedure TDynamicList.SetItem(const aKey: IDynamicValue; const AValue: IDynamicValue);
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

constructor TDynamicList.Create;
begin
  inherited Create;
  System.SetLength(fList,0);
end;

procedure TDynamicList.Add(const aItem: IDynamicValue);
begin
  SetItem(Length,aItem);
end;

procedure TDynamicList.Delete(const aIndex: Longint);
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

function TDynamicList.IndexOf(const aValue: IDynamicValue): Longint;
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

function TDynamicList.EqualsDeeply(const aValue: IDynamicValue): Boolean;
var
  i: Longint;
  l: LongInt;
  lValue: IDynamicList;
begin
  result := aValue is IDynamicList;
  if result then
  begin
     l :=  Length;
     lValue := IDynamicList(aValue);
     result := l = lValue.Length;
     if result then
     begin
        for i := 0 to l - 1 do
        begin
          result := Item[i].EqualsDeeply(lValue[i]);
          if not result then
             break;
        end;

     end;
  end;
end;

function TDynamicList.Owns(const aValue: IDynamicValue): Boolean;
var
  i: Longint;
  l: Longint;
begin
  result := false;
  l := Length - 1;
  for i := 0 to l do
  begin
    result := (fList[i] = aValue) or
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

function TDynamicString.EqualsDeeply(const aValue: IDynamicValue): Boolean;
begin
  result := (aValue is IDynamicString) and (IDynamicString(aValue).Value = Value);

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

function TDynamicNumber.EqualsDeeply(const aValue: IDynamicValue): Boolean;
begin
  result := (aValue is IDynamicNumber) and (IDynamicNumber(aValue).Value = Value);

end;

{ TDynamicNull }

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

function TDynamicBoolean.EqualsDeeply(const aValue: IDynamicValue): Boolean;
begin
  result := (aValue is IDynamicBoolean) and (IDynamicBoolean(aValue).Value = Value);
end;

{ TDynamicValue }

function TDynamicValue.GetItem(const aKey: IDynamicValue): IDynamicValue;
begin
  result := TDynamicValues.Undefined;
end;

procedure TDynamicValue.SetItem(const aKey: IDynamicValue; const AValue: IDynamicValue);
begin
 // nothing...
end;

function TDynamicValue.Owns(const aValue: IDynamicValue): Boolean;
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

function TDynamicValue.EqualsDeeply(const aValue: IDynamicValue): Boolean;
begin
  result := false;
end;

initialization

end.

