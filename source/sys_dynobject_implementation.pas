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

  { TDynamicArray }

  TDynamicArray = class(TDynamicValue,IDynamicArray)
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

  { TDynamicMap }

  {
  I could have implemented this as a hash table. However, I have no plans to
  use the maps for large numbers of keys, so lookups are not expected to be very
  costly. Hashtables allow us to trade memory space and simplicity for speed,
  but only when that speed is actually going to be worth it. I'm not certain
  it is worth it at this point.
  }
  TDynamicMap = class(TDynamicValue,IDynamicMap)
  strict private type
    TNamedValue = record
      Name: UTF8String;
      Value: IDynamicValue;
    end;
    TNamedValueArray = array of TNamedValue;

    { TEnumerator }

    TEnumerator = class(TInterfacedObject,IDynamicMapKeyEnumerator)
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
    function Enumerate: IDynamicMapKeyEnumerator;
    function IsEqualTo(aValue: IDynamicValue): Boolean; override;
    function Owns(aValue: IDynamicValue): Boolean; override;
  end;

  procedure ToJSON(aObject: IDynamicValue; aStream: TStream; aSpace: UTF8String = '');
  function ToJSON(aObject: IDynamicValue; aSpace: UTF8String = ''): UTF8String;

  const
    NullText: UTF8String = 'null';
    TrueText: UTF8String = 'true';
    FalseText: UTF8String = 'false';

implementation

uses
  Math;

procedure ToJSON(aObject: IDynamicValue; aStream: TStream; aSpace: UTF8String);
var
  lStack: IDynamicArray;
  lIndent: UTF8String;
  lGap: UTF8String;

  procedure Write(aValue: UTF8String);
  begin
    if aValue <> '' then
       aStream.Write(aValue[1],Length(aValue));
  end;

  procedure WriteValue(aValue: IDynamicValue); forward;

  procedure WriteQuote(aValue: UTF8String);
  var
    lText: UTF8String;
    i: Integer;
    c: Char;
  begin
    lText := '"';
    for i := 1 to Length(aValue) do
    begin
      c := aValue[i];
      case c of
        '"','\':
          lText := lText + '\' + c;
        #$08,#$0C,#$0A,#$0D,#$09:
        begin
          lText := lText + '\';
          case c of
            #$08:
              lText := lText + 'b';
            #$0C:
              lText := lText + 'f';
            #$0A:
              lText := lText + 'n';
            #$0D:
              lText := lText + 'r';
            #$09:
              lText := lText + 't';
          end;
        end;
        else if c < ' ' then
        begin
          lText := lText +  '\' + 'u' + hexStr(ord(c),4);
        end
        else
          lText := lText + c;

      end;
    end;
    lText := lText + '"';
    Write(lText);
  end;

  procedure WriteMap(aValue: IDynamicMap);
  var
    lStepback: UTF8String;
    lPropsWritten: Boolean;
    lHeader: UTF8String;
    lSeparator: UTF8String;
    lFooter: UTF8String;
    lKey: UTF8String;
    lEnum: IDynamicMapKeyEnumerator;
    lItem: IDynamicValue;
  begin
    if lStack.IndexOf(aValue) > -1 then
       raise Exception.Create('Cyclical JSON is not allowed');
    lStack.Add(aValue);
    lStepback := lIndent;
    lIndent := lIndent + lGap;
    lPropsWritten := false;

    if lGap = '' then
    begin
      lHeader := '';
      lSeparator := ',';
      lFooter := '';
    end
    else
    begin
      lHeader := #10 + lIndent;
      lSeparator := ',' + #10 + lIndent;
      lFooter := #10 + lStepback;
    end;

    Write('{');
    lEnum := aValue.Enumerate;
    while lEnum.Next do
    begin
      lKey := lEnum.Key;
      lItem := lEnum.Value;
      if lItem.IsDefined then
      begin
        if not lPropsWritten then
          Write(lHeader)
        else
          Write(lSeparator);
        WriteQuote(lKey);
        if lGap <> '' then
          Write(': ')
        else
          Write(':');
        WriteValue(lItem);
        lPropsWritten := true;
      end;
    end;


    if lPropsWritten then
       Write(lFooter);
    Write('}');


    lStack.Delete(lStack.Length - 1);
    lIndent := lStepback;
  end;

  procedure WriteArray(aValue: IDynamicArray);
  var
    lStepback: UTF8String;
    lItemsWritten: Boolean;
    lHeader: UTF8String;
    lSeparator: UTF8String;
    lFooter: UTF8String;
    lArrLen: Integer;
    i: Integer;
    lItem: IDynamicValue;
  begin
    if lStack.IndexOf(aValue) > -1 then
       raise Exception.Create('Cyclical JSON is not allowed');
    lStack.Add(aValue);
    lStepback := lIndent;
    lIndent := lIndent + lGap;
    lItemsWritten := false;

    if lGap = '' then
    begin
      lHeader := '';
      lSeparator := ',';
      lFooter := '';
    end
    else
    begin
      lHeader := #10 + lIndent;
      lSeparator := ',' + #10 + lIndent;
      lFooter := #10 + lStepback;
    end;

    Write('[');

    lArrLen := aValue.Length;
    for i := 0 to lArrLen - 1 do
    begin
      if not lItemsWritten then
        Write(lHeader)
      else
        Write(lSeparator);
      lItem := aValue[i];
      if not lItem.IsDefined then
        Write(NullText)
      else
        WriteValue(lItem);
      lItemsWritten := true;
    end;

    if lItemsWritten then
       Write(lFooter);
    Write(']');

    lStack.Delete(lStack.Length - 1);
    lIndent := lStepback;

  end;

  procedure WriteValue(aValue: IDynamicValue);
  begin
       if aValue is IDynamicNumber then
          Write(FloatToStr((aValue as IDynamicNumber).Value))
       else if aValue is IDynamicBoolean then
          Write(BoolToStr((aValue as IDynamicBoolean).Value,TrueText,FalseText))
       else if aValue is IDynamicString then
          WriteQuote((aValue as IDynamicString).Value)
       else if aValue is IDynamicArray then
          WriteArray((aValue as IDynamicArray))
       else if aValue is IDynamicMap then
          WriteMap((aValue as IDynamicMap))
       else
          Write(NullText);
  end;

begin
  lStack := TDynamicValues.NewArray;
  lIndent := '';
  // Per EcmaScript specs, the longest indent is 10 characters.
  // I'm doing a sanity filter by also making sure the string is only
  // spaces, and not something else.
  lGap := StringOfChar(' ',math.Min(Length(aSpace),10));
  WriteValue(aObject);
end;

function ToJSON(aObject: IDynamicValue; aSpace: UTF8String): UTF8String;
var
  lOutput: TStringStream;
begin
  lOutput := TStringStream.Create('');
  try
    ToJSON(aObject,lOutput,aSpace);
    Result := lOutput.DataString;
  finally
    lOutput.Free;
  end;

end;

{ TDynamicMap.TEnumerator }

constructor TDynamicMap.TEnumerator.Create(aMap: TDynamicMap);
begin
  inherited Create;
  fMap := aMap;
  fIndex := -1;
end;

function TDynamicMap.TEnumerator.GetKey: UTF8String;
begin
  result := fMap.fList[fIndex].Name;
end;

function TDynamicMap.TEnumerator.GetValue: IDynamicValue;
begin
  result := fMap.fList[fIndex].Value;
end;

function TDynamicMap.TEnumerator.Next: Boolean;
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

function TDynamicMap.Enumerate: IDynamicMapKeyEnumerator;
begin
  result := TEnumerator.Create(Self);
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

{ TDynamicArray }

function TDynamicArray.GetItem(aIndex: Longint): IDynamicValue;
begin
   if (aIndex >= 0) and (aIndex < Length) then
      result := fList[aIndex]
   else
      result := TDynamicValues.Undefined;
end;

function TDynamicArray.GetLength: Longint;
begin
  result := System.Length(fList);

end;

procedure TDynamicArray.SetItem(aIndex: Longint; AValue: IDynamicValue);
begin
  if (aIndex >= 0) then
  begin
     if AValue.Owns(Self) then
     begin
        raise Exception.Create('Attempt to create a circular reference in a dynamic array');
     end;
     if not AValue.IsDefined then
     begin
        raise Exception.Create('Attempt to add an undefined value to a dynamic array');
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
    raise ERangeError.Create('Invalid index to set in dynamic array: ' + IntToStr(aIndex));
end;

procedure TDynamicArray.SetLength(AValue: Longint);
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

constructor TDynamicArray.Create;
begin
  inherited Create;
  System.SetLength(fList,0);
end;

procedure TDynamicArray.Add(aItem: IDynamicValue);
begin
  SetItem(Length,aItem);
end;

procedure TDynamicArray.Delete(aIndex: Longint);
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

procedure TDynamicArray.Clear;
begin
  System.SetLength(fList,0);

end;

function TDynamicArray.IndexOf(aValue: IDynamicValue): Longint;
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

function TDynamicArray.IsEqualTo(aValue: IDynamicValue): Boolean;
begin
  Result:= (aValue is IDynamicArray);
  if result then
  begin
    result := (aValue as IDynamicArray) = (Self as IDynamicArray);
  end;
end;

function TDynamicArray.Owns(aValue: IDynamicValue): Boolean;
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
    (self is IDynamicArray) or
    (Self is IDynamicMap);
end;


procedure Test;
var
  lValue: IDynamicValue;
begin
  lValue := TDynamicValues.NewMap;
  IDynamicMap(lValue)['null'] := TDynamicValues.Null;
  IDynamicMap(lValue)['string'] := TDynamicValues.NewString('foo');
  IDynamicMap(lValue)['number'] := TDynamicValues.NewNumber(42.24);
  IDynamicMap(lValue)['array'] := TDynamicValues.NewArray;
  IDynamicArray(IDynamicMap(lValue)['array']).Add(TDynamicValues.NewNumber(1));
  IDynamicArray(IDynamicMap(lValue)['array']).Add(TDynamicValues.NewNumber(2));
  IDynamicArray(IDynamicMap(lValue)['array']).Add(TDynamicValues.NewString('c'));
  IDynamicMap(lValue)['true'] := TDynamicValues.Boolean(true);
  WriteLn(ToJSON(lValue,'  '));




end;

initialization
  Test;

end.

