unit sys_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stew_types, contnrs, jsonscanner;

type

   {This is a somewhat more flexible replacement for the fpjson and jsonparser units.
   Features available that weren't provided by the others:
   - More controlled creation of member-values, allowing for context-specific
     custom javascript objects. Fpjson has re-assignable 'create json' functions,
     but these are global, so if you want a different object to be created based
     on its location in its parent, for example, it would take a lot of work.
   - More controlled and obvious lifetime of objects. With fpjson, assignments
     of values (at least in most cases) were copied into a new object. This
     meant you would suddenly have two objects where you might have been expecting
     only one. In this unit, there is no way to assign non-primitive objects,
     so copying is the only way, and it's clearer who manages the lifetime of
     the objects.
   - More similarity to the way JavaScript objects work.
     - in Fpjson, the default index properties of the objects were all 'integers',
     while this unit and JavaScript defaults to strings.
     - values are more 'dynamic'. They all support the same object map interface,
     even if they aren't capable of storing properties. This allows you to
     essentially create your own, completely different custom object that's not
     even a descendant of TJSObject, but still is capable of handling the same
     functionality.
   - It is possible to create 'custom objects' with native pascal properties and
     functions, and have them create the appropriate values beneath them based
     on property name, without having to worry about overriding the streaming
     of fpjsonrtti.}

  // FUTURE: This has inspired me to recreate the entire EcmaScript standard
  // functionality in some sort of engine. YOU MUST RESIST THIS. The primary issue with
  // that is coming up with a garbage collection service, the rest, except
  // things like 'eval' that require parsing language, would be simple to do.
  // But, the intention of this unit is to represent JSON objects in pascal,
  // not be a javascript engine.


  TJSType = (jstUndefined, jstBoolean, jstNumber, jstString, jstObject, jstNull);
  TJSValueClass = class of TJSValue;

  { TJSValue }

  TJSUndefined = class;
  TJSNull = class;

  TJSValue = class
  private
    class var fUndefined: TJSUndefined;
    class var fNull: TJSNull;
  private
  protected
    // override this to behave the way Javascript '"" + x' might behave
    // for your given value.
    function GetAsString: UTF8String; virtual; abstract;
    // override this to behave the way Javascript 'Number(x)' might behave
    // for your given value.
    function GetAsNumber: Double; virtual; abstract;
    // override this to behave the way Javascript '!!x' might behave
    // for your given value.
    function GetAsBoolean: Boolean; virtual; abstract;
    // override these to handle conversion of native primitive properties values
    // to the object when read from a stream or other source.
    procedure SetAsBoolean(AValue: Boolean); virtual; abstract;
    procedure SetAsNumber(AValue: Double); virtual; abstract;
    procedure SetAsString(AValue: UTF8String); virtual; abstract;
    // override this to behave the way Javascript 'x["key"] = y' might behave for your value of x
    // and any value of y of the specified type. You are allowed to not create a
    // new instance (perhaps returning the value that's already there), and return
    // a completely different type than what is requested. Other functionality will
    // expect it to behave like the requested type, however, so if you do this,
    // your type should be able to accept that behavior without error. If you plan
    // to do this, you can look at the GetTypeOf function on the requested type
    // to find the behavior expected.
    function CreateValue({%H-}aKey: UTF8String; {%H-}aRequestType: TJSValueClass): TJSValue; virtual; overload;
    function CreateValue(aKey: Integer; aRequestType: TJSValueClass): TJSValue; overload;
    class function CreateValue(aRequestType: TJSValueClass): TJSValue; overload;
    // override this to handle assigning a value to the specified key. This
    // value *should* have been created using CreateValue, so there shouldn't be
    // any reason to expect the wrong type here.
    function Put({%H-}aKey: UTF8String; {%H-}aValue: TJSValue): TJSValue; virtual; overload;
    function Put(aKey: Integer; aValue: TJSValue): TJSValue; overload;
  public
    constructor Create; virtual;
    class constructor Initialize;
    class destructor Finalize;
    class property Undefined: TJSUndefined read fUndefined;
    class property Null: TJSNull read fNull;
    procedure Assign(aValue: TJSValue); virtual;
    function Clone: TJSValue; virtual;
    function DeepEquals(aValue: TJSValue): Boolean; virtual;
    // override this only if you have a "custom" value, not descended from
    // the primitives or TJSObject, and you want it to be able to be cloned
    // to another JSObject.
    procedure AssignTo({%H-}aTarget: TJSValue); virtual;
    // NOTE: These are read-only because otherwise we would have to
    // put in event handlers to catch the change when the parent object
    // wants to know when a value has changed.
    property AsString: UTF8String read GetAsString;
    property AsNumber: Double read GetAsNumber;
    property AsBoolean: Boolean read GetAsBoolean;
    // override this to return an appropriate enumeration value from above
    // Note that this is a 'class' function, which means that you can access
    // it from the type definition in your overridden 'CreateValue' function.
    class function GetTypeOf: TJSType;  virtual; abstract;
    property TypeOf: TJSType read GetTypeOf;
    // override this to behave the way Javascript 'Object.keys(x)' might behave
    // for your given value.
    function keys: TStringArray; virtual;
    // override this to behave the way Javascript 'x.hasOwnProperty' might behave
    // for your given value.
    function hasOwnProperty({%H-}aKey: UTF8String): Boolean; virtual; overload;
    function hasOwnProperty(aKey: Integer): Boolean; overload;
    // override this to behave the way Javascript 'x["key"]' might behave for your value of x.
    // If the call would normally return undefined, it should return nil
    function Get({%H-}aKey: UTF8String): TJSValue; virtual; overload;
    // override this the same way as GetValue if your object can handle integer indexes
    function Get(aKey: Integer): TJSValue; overload;
    // override this to behave the way Javascript 'delete x["key"]' might behave
    // for your given value.
    procedure delete({%H-}aKey: UTF8String); virtual; overload;
    // override this the same way as delete if your object can handle integer
    // keys.
    procedure delete(aKey: Integer); overload;
    procedure Put(aKey: UTF8String; aValue: UTF8String); virtual; overload;
    procedure Put(aKey: Integer; aValue: UTF8String); overload;
    procedure Put(aKey: UTF8String; aValue: Double); virtual; overload;
    procedure Put(aKey: Integer; aValue: Double); overload;
    procedure Put(aKey: UTF8String; aValue: Boolean); virtual; overload;
    procedure Put(aKey: Integer; aValue: Boolean); overload;
    // non-primitive 'puts' don't need to be virtual because their behavior
    // can be overridden in CreateValue and PutValue. The primitive ones
    // might need to be overridden in a way that doesn't deal with TJSValue.
    procedure PutNull(aKey: UTF8String); overload;
    procedure PutNull(aKey: Integer); overload;
    procedure PutUndefined(aKey: UTF8String); overload;
    procedure PutUndefined(aKey: Integer); overload;
    function PutNewObject(aKey: UTF8String): TJSValue; overload;
    function PutNewObject(aKey: Integer): TJSValue; overload;
    function PutNewArray(aKey: UTF8String): TJSValue; overload;
    function PutNewArray(aKey: Integer): TJSValue; overload;
    class function MakeString(aValue: UTF8String): TJSValue;
    class function MakeNumber(aValue: Double): TJSValue;
    class function MakeBoolean(aValue: Boolean): TJSValue;
  end;

  { TJSString }

  TJSString = class(TJSValue)
  private
    fValue: UTF8String;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsNumber(AValue: Double); override;
    procedure SetAsString(AValue: UTF8String); override;
  public
    class function GetTypeOf: TJSType; override;
    property Value: UTF8String read fValue write fValue;
    procedure Assign(aValue: TJSValue); override;
  end;

  { TJSNumber }

  TJSNumber = class(TJSValue)
  private
    fValue: Double;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsNumber(AValue: Double); override;
    procedure SetAsString(AValue: UTF8String); override;
  public
    class function GetTypeOf: TJSType; override;
    property Value: Double read fValue write fValue;
    procedure Assign(aValue: TJSValue); override;
  end;

  { TJSBoolean }

  TJSBoolean = class(TJSValue)
  private
    fValue: Boolean;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsNumber(AValue: Double); override;
    procedure SetAsString(AValue: UTF8String); override;
  public
    class function GetTypeOf: TJSType; override;
    property Value: Boolean read fValue write fValue;
    procedure Assign(aValue: TJSValue); override;
  end;

  { TJSUndefined }

  // NOTE: Undefined is a singletone type. It is represented by 'nil'
  // in the data itself, and is never actually stored.
  TJSUndefined = class(TJSValue)
  protected
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean({%H-}AValue: Boolean); override;
    procedure SetAsNumber({%H-}AValue: Double); override;
    procedure SetAsString({%H-}AValue: UTF8String); override;
  public
    constructor Create; override;
    procedure Assign({%H-}aValue: TJSValue); override;
    class function GetTypeOf: TJSType; override;
  end;

  { TJSNull }

  TJSNull = class(TJSValue)
  protected
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean({%H-}AValue: Boolean); override;
    procedure SetAsNumber({%H-}AValue: Double); override;
    procedure SetAsString({%H-}AValue: UTF8String); override;
  public
    constructor Create; override;
    procedure Assign({%H-}aValue: TJSValue); override;
    class function GetTypeOf: TJSType; override;
  end;

  { TJSObject }
  // You can make "custom" objects fairly easily. Just:
  // - create a native property that translates between Get(aKey...) and Put(aKey..)
  // - override Put(aKey, TJSValue) to control what happens when a value is set.
  // - override CreateValue(aKey, RequestType) to control what happens when a new value
  //   is created.
  //
  // I considered making a "persistent" JSObject which automatically managed
  // published properties in the manner above, but complications arose when I
  // tried to abstract TJSValues as delphi native primitive types.

  TJSObject = class(TJSValue)
  private
    fList: TFPHashObjectList;
  protected
    function Put(aKey: UTF8String; aValue: TJSValue): TJSValue; override;
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    procedure SetAsBoolean({%H-}AValue: Boolean); override;
    procedure SetAsNumber({%H-}AValue: Double); override;
    procedure SetAsString({%H-}AValue: UTF8String); override;
    function CreateValue({%H-}aKey: UTF8String; aType: TJSValueClass): TJSValue;
       override; overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetTypeOf: TJSType; override;
    procedure Assign(aValue: TJSValue); override;
    // override this to behave the way Javascript 'Object.keys(x)' might behave
    // for your given value.
    function keys: TStringArray; override;
    // override this to behave the way Javascript 'x.hasOwnProperty' might behave
    // for your given value.
    function hasOwnProperty(aKey: UTF8String): Boolean; override; overload;
    // override this to behave the way Javascript 'delete x["key"]' might behave
    // for your given value.
    procedure delete(aKey: UTF8String); override; overload;
    // override this to behave the way Javascript 'x["key"]' might behave for your value of x.
    // If the call would normally return undefined, it should return nil
    function Get(aKey: UTF8String): TJSValue; override; overload;
    function Move(aKey: UTF8String; aNewOwner: TJSObject; aNewKey: UTF8String
      ): TJSValue; overload;
  end;

  { TJSArray }
  TJSArray = class(TJSObject)
  private
  protected
    function Put(aKey: UTF8String; aValue: TJSValue): TJSValue; override; overload;
    procedure SetLength(AValue: Integer);
    function GetLength: Integer;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    function GetAsBoolean: Boolean; override;
  public
    const LengthKey: UTF8String = 'length';
    constructor Create; override;
    destructor Destroy; override;
    property Length: Integer read GetLength write SetLength;
    function Join(sep: UTF8String = ','): UTF8String;
  end;

  { TJSONParser }

  TJSONParser = class
  private
    fScanner: TJSONScanner;
  public type
    TCreateJSValueFunction = function(aKey: UTF8String; aRequestType: TJSValueClass): TJSValue of object;
  protected
    procedure SkipWhitespace;
    function DefaultCreator({%H-}aKey: UTF8String; aRequestType: TJSValueClass): TJSValue;
  public
    constructor Create(aStream: TStream);
    destructor Destroy; override;
    function Parse: TJSValue;
    function Parse(aCreationName: UTF8String; aCreator: TCreateJSValueFunction
      ): TJSValue;
    procedure Parse(aTarget: TJSValue);
    procedure ParseString(aTarget: TJSValue);
    procedure ParseNumber(aTarget: TJSValue);
    procedure ParseBoolean(aTarget: TJSValue);
    procedure ParseNull;
    procedure ParseObject(aTarget: TJSValue);
    procedure ParseArray(aTarget: TJSValue);
  end;

  function JSStringToNumber(aValue: UTF8String): Double;
  function JSStringToBoolean(aValue: UTF8String): Boolean;
  function JSNumberToString(aValue: Double): UTF8String;
  function JSNumbertoBoolean(aValue: Double): Boolean;
  function JSBooleanToString(aValue: Boolean): UTF8String;
  function JSBooleanToNumber(aValue: Boolean): Double;

  // writes the data from a value to a stream.
  procedure ToJSON(aObject: TJSValue; aStream: TStream; aSpace: UTF8String = '');
  function ToJSON(aObject: TJSValue; aSpace: UTF8String = ''): UTF8String;
  function FromJSON(aClass: TJSValueClass; aStream: TStream): TJSValue;
  procedure FromJSON(aObject: TJSValue; aStream: TStream);
  function FromJSON(aClass: TJSValueClass; aData: UTF8String): TJSValue;
  procedure FromJSON(AObject: TJSVAlue; aData: UTF8String);
  function FromJSON(aData: UTF8String): TJSValue;
  function FromJSON(aStream: TStream): TJSValue;

  const NullText: UTF8String = 'null';
  const TrueText: UTF8String = 'true';
  const FalseText: UTF8String = 'false';

implementation

uses
  math;

function JSStringToNumber(aValue: UTF8String): Double;
begin
  if Trim(aValue) = '' then
     result := 0
  else if not TryStrToFloat(aValue,result) then
     result := math.NaN;
end;

function JSStringToBoolean(aValue: UTF8String): Boolean;
begin
  result := aValue <> '';
end;

function JSNumberToString(aValue: Double): UTF8String;
begin
  result := FloatToStr(aValue);
end;

function JSNumbertoBoolean(aValue: Double): Boolean;
begin
  result := aValue <> 0;
end;

function JSBooleanToString(aValue: Boolean): UTF8String;
begin
  if aValue then
     result := TrueText
  else
     result := FalseText;
end;

function JSBooleanToNumber(aValue: Boolean): Double;
begin
  result := ord(aValue);
end;

procedure ToJSON(aObject: TJSValue; aStream: TStream; aSpace: UTF8String = '');
var
  lStack: TList;
  lIndent: UTF8String;
  lGap: UTF8String;

  procedure Write(aValue: UTF8String);
  begin
    aStream.Write(aValue[1],Length(aValue));
  end;

  procedure WriteValue(aValue: TJSValue); forward;

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

  procedure WriteObject(aValue: TJSObject);
  var
    lStepback: UTF8String;
    lKeys: TStringArray;
    lPropsWritten: Boolean;
    lHeader: UTF8String;
    lSeparator: UTF8String;
    lFooter: UTF8String;
    lKey: UTF8String;
    i: Integer;
    lItem: TJSValue;
  begin
    if lStack.IndexOf(aValue) > -1 then
       raise Exception.Create('Cyclical JSON is not allowed');
    lStack.Add(aValue);
    lStepback := lIndent;
    lIndent := lIndent + lGap;
    lKeys := aValue.keys;
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

    for i := 0 to Length(lKeys) - 1 do
    begin
      lKey := lKeys[i];
      lItem := aValue.Get(lKey);
      if lItem <> TJSValue.Undefined then
      begin
        if i = 0 then
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


    lStack.Delete(lStack.Count - 1);
    lIndent := lStepback;
  end;

  procedure WriteArray(aValue: TJSArray);
  var
    lStepback: UTF8String;
    lItemsWritten: Boolean;
    lHeader: UTF8String;
    lSeparator: UTF8String;
    lFooter: UTF8String;
    lArrLen: Integer;
    i: Integer;
    lItem: TJSValue;
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
      if i = 0 then
        Write(lHeader)
      else
        Write(lSeparator);
      lItem := aValue.Get(i);
      if lItem = TJSValue.Undefined then
        Write(NullText)
      else
        WriteValue(lItem);
      lItemsWritten := true;
    end;

    if lItemsWritten then
       Write(lFooter);
    Write(']');

    lStack.Delete(lStack.Count - 1);
    lIndent := lStepback;

  end;

  procedure WriteValue(aValue: TJSValue);
  begin
    case aValue.TypeOf of
      jstUndefined:;
      jstNull:
        Write(NullText);
      jstBoolean:
      begin
        if aValue.AsBoolean then
          Write(TrueText)
        else
          Write(FalseText);
      end;
      jstString:
        WriteQuote(aValue.AsString);
      jstNumber:
      begin
        if (aValue.AsNumber = math.NaN) or (aValue.AsNumber = math.Infinity) then
          Write(NullText)
        else
          Write(aValue.AsString);
      end;
      jstObject:
      if aValue is TJSArray then
        WriteArray(aValue as TJSArray)
      else if aValue is TJSObject then
        WriteObject(aValue as TJSObject);

    end;

  end;

begin
  lStack := TList.Create;
  try
    lIndent := '';
    // Per EcmaScript specs, the longest indent is 10 characters.
    // I'm doing a sanity filter by also making sure the string is only
    // spaces, and not something else.
    lGap := StringOfChar(' ',math.Min(Length(aSpace),10));
    WriteValue(aObject);
  finally
    lStack.Free;
  end;
end;

function ToJSON(aObject: TJSValue; aSpace: UTF8String): UTF8String;
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

function FromJSON(aClass: TJSValueClass; aStream: TStream): TJSValue;
begin
  result := aClass.Create;
  FromJSON(result,aStream);
end;

procedure FromJSON(aObject: TJSValue; aStream: TStream);
var
  lParser: TJSONParser;
begin
  lParser := TJSONParser.Create(aStream);
  try
    lParser.Parse(aObject);
  finally
    lParser.Free;
  end;
end;

function FromJSON(aClass: TJSValueClass; aData: UTF8String): TJSValue;
var
  lInput: TStringStream;
begin
  lInput := TStringStream.Create(aData);
  try
    result := FromJSON(aClass,lInput);
  finally
    lInput.Free;
  end;

end;

procedure FromJSON(AObject: TJSVAlue; aData: UTF8String);
var
  lInput: TStringStream;
begin
  lInput := TStringStream.Create(aData);
  try
    FromJSON(aObject,lInput);
  finally
    lInput.Free;
  end;

end;

function FromJSON(aData: UTF8String): TJSValue;
var
  lInput: TStringStream;
begin
  lInput := TStringStream.Create(aData);
  try
    result := FromJSON(lInput);
  finally
    lInput.Free;
  end;

end;

function FromJSON(aStream: TStream): TJSValue;
var
  lParser: TJSONParser;
begin
  lParser := TJSONParser.Create(aStream);
  try
    result := lParser.Parse;
  finally
    lParser.Free;
  end;
end;

{ TJSONParser }

procedure TJSONParser.SkipWhitespace;
begin
  while fScanner.CurToken = tkWhitespace do
    fScanner.FetchToken;
end;

procedure TJSONParser.ParseString(aTarget: TJSValue);
begin
  SkipWhitespace;
  if fScanner.CurToken = tkString then
  begin
    aTarget.SetAsString(fScanner.CurTokenString);
    fScanner.FetchToken;
  end
  else
    raise Exception.Create('Expected string');
end;

procedure TJSONParser.ParseNumber(aTarget: TJSValue);
begin
  SkipWhitespace;
  if fScanner.CurToken = tkNumber then
  begin
    aTarget.SetAsString(fScanner.CurTokenString);
    fScanner.FetchToken;
  end
  else
    raise Exception.Create('Expected number');

end;

procedure TJSONParser.ParseBoolean(aTarget: TJSValue);
begin
  SkipWhitespace;
  if fScanner.CurToken in [tkTrue,tkFalse] then
  begin
    aTarget.SetAsBoolean(fScanner.CurToken = tkTrue);
    fScanner.FetchToken;
  end
  else
    raise Exception.Create('Expected boolean');

end;

procedure TJSONParser.ParseNull;
begin
  SkipWhitespace;
  if fScanner.CurToken = tkNull then
    fScanner.FetchToken
  else
    raise Exception.Create('Expected null');

end;

procedure TJSONParser.ParseObject(aTarget: TJSValue);
var
  lKey: UTF8String;
  lValue: TJSValue;
begin
  SkipWhitespace;
  if fScanner.CurToken <> tkCurlyBraceOpen then
    raise Exception.Create('Expected object open');
  fScanner.FetchToken;
  SkipWhitespace;
  // look for a property
  while not (fScanner.CurToken in [tkCurlyBraceClose,tkEOF]) do
  begin
    if fScanner.CurToken <> tkString then
      raise Exception.Create('Expected property name');
    lKey := fScanner.CurTokenString;
    fScanner.FetchToken;
    SkipWhitespace;
    if fScanner.CurToken <> tkColon then
      raise Exception.Create('Expected colon');
    fScanner.FetchToken;
    lValue := Parse(lKey,@aTarget.CreateValue);
    aTarget.Put(lKey,lValue);
    SkipWhitespace;
    if fScanner.CurToken = tkComma then
      SkipWhitespace
    else if fScanner.CurToken <> tkCurlyBraceClose then
      raise Exception.Create('Expected comma');
  end;
  if fScanner.CurToken <> tkCurlyBraceClose then
    raise Exception.Create('Expected object close');
  fScanner.FetchToken;

end;

procedure TJSONParser.ParseArray(aTarget: TJSValue);
var
  lKey: Integer;
  lValue: TJSValue;
begin
  SkipWhitespace;
  if fScanner.CurToken <> tkSquaredBraceOpen then
    raise Exception.Create('Expected array open');
  fScanner.FetchToken;
  // look for a item
  lKey := 0;
  while not (fScanner.CurToken in [tkSquaredBraceClose,tkEOF]) do
  begin
    lValue := Parse(IntToStr(lKey),@aTarget.CreateValue);
    aTarget.Put(lKey,lValue);
    SkipWhitespace;
    if fScanner.CurToken = tkComma then
    begin
      fScanner.FetchToken;
      lKey := lKey + 1;
    end
    else if fScanner.CurToken <> tkSquaredBraceClose then
      raise Exception.Create('Expected comma');
  end;
  if fScanner.CurToken <> tkSquaredBraceClose then
    raise Exception.Create('Expected array close');
  fScanner.FetchToken;

end;

function TJSONParser.DefaultCreator(aKey: UTF8String;
  aRequestType: TJSValueClass): TJSValue;
begin
  result := aRequestType.Create;
end;

constructor TJSONParser.Create(aStream: TStream);
begin
  fScanner := TJSONScanner.Create(aStream,true);
  fScanner.Strict := true;
end;

destructor TJSONParser.Destroy;
begin
  FreeAndNil(fScanner);
  inherited Destroy;
end;

function TJSONParser.Parse: TJSValue;
begin
  result := Parse('',@DefaultCreator);
end;

function TJSONParser.Parse(aCreationName: UTF8String; aCreator: TCreateJSValueFunction): TJSValue;
begin
  Skipwhitespace;
  case fScanner.CurToken of
    tkEOF:
      raise Exception.Create('Unexpected end of file');
    //tkWhitespace,
    tkString:
    begin
      result := aCreator(aCreationName,TJSString);
      ParseString(result);
    end;
    tkNumber:
    begin
      result := aCreator(aCreationName,TJSNumber);
      ParseNumber(result);
    end;
    tkTrue, tkFalse:
    begin
      result := aCreator(aCreationName,TJSBoolean);
      ParseBoolean(result);
    end;
    tkNull:
    begin
      result := aCreator(aCreationName,TJSNull);
      ParseNull;
    end;
    // Simple (one-character) tokens
    tkComma:
      raise Exception.Create('Unexpected comma in file');
    tkColon:
      raise Exception.Create('Unexpected colon in file');
    tkCurlyBraceOpen:
    begin
      result := aCreator(aCreationName,TJSObject);
      ParseObject(result as TJSObject);
    end;
    tkCurlyBraceClose:
      raise Exception.Create('Unexpected end of object in file');
    tkSquaredBraceOpen:
    begin
      result := aCreator(aCreationName,TJSArray);
      ParseArray(result as TJSObject);
    end;
    tkSquaredBraceClose:
      raise Exception.Create('Unexpected end of array in file');
    tkIdentifier:
      raise Exception.Create('Unexpected identifier in file (only strict JSON is allowed)');
    tkUnknown:
      raise Exception.Create('Unknown token in file');
  end;
end;

procedure TJSONParser.Parse(aTarget: TJSValue);
begin
  SkipWhitespace;
  case aTarget.TypeOf of
    jstUndefined:;
    jstNull:
      ParseNull;
    jstBoolean:
      ParseBoolean(aTarget);
    jstNumber:
      ParseNumber(aTarget);
    jstString:
      ParseString(aTarget);
    jstObject:
      if aTarget is TJSArray then
         ParseArray(aTarget as TJSObject)
      else
         ParseObject(aTarget as TJSObject);
  end;

end;

{ TJSArray }

function TJSArray.Put(aKey: UTF8String; aValue: TJSValue): TJSValue;
var
  lOldLength: Integer;
  lNewLength: Integer;
  lIndex: Integer;
begin
  lOldLength := Length;
  if aKey = LengthKey then
  begin
    lNewLength := Trunc(aValue.AsNumber);
    if lNewLength <> aValue.AsNumber then
       raise Exception.Create('Length value is out of range');
    aValue.SetAsNumber(lNewLength);
    while lNewLength < lOldLength do
    begin
      lOldLength := lOldLength - 1;
      try
         delete(lOldLength);
      except
        aValue.SetAsNumber(lOldLength + 1);
        result := inherited Put(aKey,aValue);
        raise;
      end;

    end;
    result := inherited Put(aKey,aValue);
  end
  else if TryStrToInt(aKey,lIndex) then
  begin
    result := inherited Put(aKey,aValue);
    if lIndex > lOldLength then
       Put(LengthKey,lIndex + 1);

  end
  else
    result := inherited Put(aKey,aValue);

end;

procedure TJSArray.SetLength(AValue: Integer);
begin
  Put(LengthKey,AValue);
end;

function TJSArray.GetLength: Integer;
begin
  result := Trunc(Get(LengthKey).AsNumber);
end;

constructor TJSArray.Create;
begin
  inherited Create;
  SetLength(0);
end;

destructor TJSArray.Destroy;
begin
  inherited Destroy;
end;

function TJSArray.GetAsNumber: Double;
begin
  // yes, this is the way the EcmaScript spec says it happens.
  result := JSStringToNumber(GetAsString);
end;

function TJSArray.GetAsString: UTF8String;
begin
  result := Join;
end;

function TJSArray.GetAsBoolean: Boolean;
begin
  result := true;
end;

function TJSArray.Join(sep: UTF8String): UTF8String;
var
  len: Integer;
  element0: TJSValue;
  element: TJSValue;
  k: Integer;
  R: UTF8String;
  S: UTF8String;
  next: UTF8String;
begin
  // Taken from Ecmascript standard for correctness...
  len := Trunc(Length);
  if len = 0 then
  begin
     result := '';
     exit;
  end;
  element0 := Get('0');
  if element0.TypeOf in [jstUndefined,jstNull] then
     R := ''
  else
     R := element0.AsString;
  k := 1;
  while k < len do
  begin
    S := R + sep;
    element := Get(IntToStr(k));
    if element.TypeOf in [jstUndefined,jstNull] then
       next := ''
    else
       next := element.AsString;
    R := S + next;
    k := k + 1;
  end;
  result := R;
end;

{ TJSUndefined }

function TJSUndefined.GetAsNumber: Double;
begin
  result := math.NaN;
end;

function TJSUndefined.GetAsString: UTF8String;
begin
  result := 'undefined';
end;

function TJSUndefined.GetAsBoolean: Boolean;
begin
  result := false;
end;

class function TJSUndefined.GetTypeOf: TJSType;
begin
  result := jstUndefined;
end;

procedure TJSUndefined.SetAsBoolean(AValue: Boolean);
begin
  raise Exception.Create('Can''t set value of undefined');
end;

procedure TJSUndefined.SetAsNumber(AValue: Double);
begin
  raise Exception.Create('Can''t set value of undefined');

end;

procedure TJSUndefined.SetAsString(AValue: UTF8String);
begin
  raise Exception.Create('Can''t set value of undefined');

end;

constructor TJSUndefined.Create;
begin
  if fUndefined <> nil then
     raise Exception.Create('Don''t create an instance of undefined. Just use the global TJSUndefined.Undefined');
  inherited Create;

end;

procedure TJSUndefined.Assign(aValue: TJSValue);
begin
  raise Exception.Create('Can''t set assign to undefined');
end;

{ TJSValue }

class function TJSValue.CreateValue(aRequestType: TJSValueClass): TJSValue;
begin
  if aRequestType <> nil then
  begin;
     if aRequestType = TJSUndefined then
       result := TJSUndefined.Undefined
     else if aRequestType = TJSNull then
       result := TJSNull.Null
     else
       result := aRequestType.Create
  end
  else
     result := TJSUndefined.Undefined;
end;

function TJSValue.{%H-}CreateValue(aKey: UTF8String; aRequestType: TJSValueClass): TJSValue;
begin
  raise Exception.Create('Object type does not support contained objects.');
end;

function TJSValue.CreateValue(aKey: Integer; aRequestType: TJSValueClass
  ): TJSValue;
begin
  result := CreateValue(IntToStr(aKey),aRequestType);
end;

function TJSValue.{%H-}Put(aKey: UTF8String; aValue: TJSValue): TJSValue;
begin

  raise Exception.Create('Object type does not support properties');
end;

function TJSValue.Put(aKey: Integer; aValue: TJSValue): TJSValue;
begin
  result := Put(IntToStr(aKey),aValue);
end;

constructor TJSValue.Create;
begin
end;

class constructor TJSValue.Initialize;
begin
  fUndefined := TJSUndefined.Create;
  fNull := TJSNull.Create;
end;

class destructor TJSValue.Finalize;
begin
  FreeAndNil(fNull);
  FreeAndNil(fUndefined);
end;

procedure TJSValue.Assign(aValue: TJSValue);
begin
   aValue.AssignTo(Self);
end;

function TJSValue.Clone: TJSValue;
begin
  result := TJSValueClass(ClassType).Create;
  result.Assign(Self);
end;

function TJSValue.DeepEquals(aValue: TJSValue): Boolean;
var
  lKeys: TStringArray;
  i: Integer;
begin
  if TypeOf = aValue.TypeOf then
  begin
    case TypeOf of
      jstUndefined:
        result := (aValue = TJSValue.Undefined) and (Self = TJSValue.Undefined);
      jstNull:
        result := (aValue = TJSValue.Null) and (Self = TJSValue.Null);
      jstBoolean:
        result := AsBoolean = aValue.AsBoolean;
      jstNumber:
        result := AsNumber = aValue.AsNumber;
      jstString:
        result := AsString = aValue.AsString;
      jstObject:
      begin
        lKeys := keys;
        if Length(lKeys) = Length(aValue.keys) then
        begin
          result := true;
          for i := 0 to Length(lKeys) - 1 do
          begin
            if (not aValue.hasOwnProperty(lKeys[i])) or
               (not Get(lKeys[i]).DeepEquals(aValue.Get(lKeys[i]))) then
            begin
              result := false;
              break;
            end;
          end;

        end
        else
          result := false;
      end;
    end;

  end
  else
    result := false;
end;

procedure TJSValue.AssignTo(aTarget: TJSValue);
begin
  raise Exception.Create('I don''t know how to assign this custom js object to another object.');
end;

function TJSValue.keys: TStringArray;
begin
  SetLength(Result,0);
end;

function TJSValue.hasOwnProperty(aKey: UTF8String): Boolean;
begin
  result := false;

end;

function TJSValue.hasOwnProperty(aKey: Integer): Boolean;
begin
  result := hasOwnProperty(IntToStr(aKey));
end;

function TJSValue.{%H-}Get(aKey: UTF8String): TJSValue;
begin
  raise Exception.Create('Object type does not support properties');
end;

function TJSValue.Get(aKey: Integer): TJSValue;
begin
  result := Get(IntToStr(aKey));
end;

procedure TJSValue.delete(aKey: UTF8String);
begin
  raise Exception.Create('Object type does not support properties');
end;

procedure TJSValue.delete(aKey: Integer);
begin
  delete(IntToStr(aKey));

end;

procedure TJSValue.Put(aKey: UTF8String; aValue: UTF8String);
begin
  Put(aKey,CreateValue(aKey,TJSString)).SetAsString(aValue);
end;

procedure TJSValue.Put(aKey: Integer; aValue: UTF8String);
begin
  Put(IntToStr(aKey),aValue);
end;

procedure TJSValue.Put(aKey: UTF8String; aValue: Double);
begin
  Put(aKey,CreateValue(aKey,TJSNumber)).SetAsNumber(aValue);

end;

procedure TJSValue.Put(aKey: Integer; aValue: Double);
begin
  Put(IntToStr(aKey),aValue);

end;

procedure TJSValue.Put(aKey: UTF8String; aValue: Boolean);
begin
  Put(aKey,CreateValue(aKey,TJSBoolean)).SetAsBoolean(aValue);

end;

procedure TJSValue.Put(aKey: Integer; aValue: Boolean);
begin
  Put(IntToStr(aKey),aValue);
end;

procedure TJSValue.PutNull(aKey: UTF8String);
begin
  Put(aKey,CreateValue(aKey,TJSNull));
end;

procedure TJSValue.PutNull(aKey: Integer);
begin
  PutNull(IntToStr(aKey));
end;

procedure TJSValue.PutUndefined(aKey: UTF8String);
begin
  Put(aKey,CreateValue(aKey,TJSUndefined));
end;

procedure TJSValue.PutUndefined(aKey: Integer);
begin
  PutUndefined(IntToStr(aKey));
end;

function TJSValue.PutNewObject(aKey: UTF8String): TJSValue;
begin
  result := Put(aKey,CreateValue(aKey,TJSObject));

end;

function TJSValue.PutNewObject(aKey: Integer): TJSValue;
begin
  result := PutNewObject(IntToStr(aKey));

end;

function TJSValue.PutNewArray(aKey: UTF8String): TJSValue;
begin
  result := Put(aKey,CreateValue(aKey,TJSArray));

end;

function TJSValue.PutNewArray(aKey: Integer): TJSValue;
begin
  result := PutNewArray(IntToStr(aKey));

end;

class function TJSValue.MakeString(aValue: UTF8String): TJSValue;
begin
  result := CreateValue(TJSString);
  Result.SetAsString(aValue);
end;

class function TJSValue.MakeNumber(aValue: Double): TJSValue;
begin
  result := CreateValue(TJSNumber);
  Result.SetAsNumber(aValue);
end;

class function TJSValue.MakeBoolean(aValue: Boolean): TJSValue;
begin
  result := CreateValue(TJSBoolean);
  Result.SetAsBoolean(aValue);
end;

{ TJSNull }

function TJSNull.GetAsBoolean: Boolean;
begin
  result := false;
end;

function TJSNull.GetAsNumber: Double;
begin
  result := 0;
end;

function TJSNull.GetAsString: UTF8String;
begin
  result := NullText;
end;

class function TJSNull.GetTypeOf: TJSType;
begin
  result := jstNull;
end;

procedure TJSNull.SetAsBoolean(AValue: Boolean);
begin
  raise Exception.Create('Can''t set value of null');

end;

procedure TJSNull.SetAsNumber(AValue: Double);
begin
  raise Exception.Create('Can''t set value of null');

end;

procedure TJSNull.SetAsString(AValue: UTF8String);
begin
  raise Exception.Create('Can''t set value of null');

end;

constructor TJSNull.Create;
begin
  if fNull <> nil then
     raise Exception.Create('Don''t create an instance of null. Just use the global TJSNull.Null.');
  inherited Create;
end;

procedure TJSNull.Assign(aValue: TJSValue);
begin
  raise Exception.Create('Can''t assign to null');
end;

{ TJSBoolean }

class function TJSBoolean.GetTypeOf: TJSType;
begin
  result := jstBoolean;
end;

function TJSBoolean.GetAsBoolean: Boolean;
begin
  result := fValue;
end;

function TJSBoolean.GetAsNumber: Double;
begin
  result := JSBooleanToNumber(fValue);

end;

function TJSBoolean.GetAsString: UTF8String;
begin
  result := JSBooleanToString(fValue);
end;

procedure TJSBoolean.SetAsBoolean(AValue: Boolean);
begin
  fValue := AValue;
end;

procedure TJSBoolean.SetAsNumber(AValue: Double);
begin
  fValue := JSNumbertoBoolean(AValue);
end;

procedure TJSBoolean.SetAsString(AValue: UTF8String);
begin
  fValue := JSStringToBoolean(AValue);
end;

procedure TJSBoolean.Assign(aValue: TJSValue);
begin
  if (aValue is TJSString) or
     (aValue is TJSBoolean) or
     (aValue is TJSNumber) or
     (aValue is TJSNull) or
     (aValue is TJSUndefined) then
    fValue := aValue.AsBoolean
  else
    inherited Assign(aValue);
end;

{ TJSNumber }

function TJSNumber.GetAsBoolean: Boolean;
begin
  result := JSNumbertoBoolean(fValue);
end;

function TJSNumber.GetAsNumber: Double;
begin
  result := fValue;
end;

function TJSNumber.GetAsString: UTF8String;
begin
  result := JSNumberToString(fValue);
end;

procedure TJSNumber.SetAsBoolean(AValue: Boolean);
begin
  fValue := JSBooleanToNumber(AValue);
end;

procedure TJSNumber.SetAsNumber(AValue: Double);
begin
  fValue := AValue;
end;

procedure TJSNumber.SetAsString(AValue: UTF8String);
begin
  fValue := JSStringToNumber(AValue);
end;

procedure TJSNumber.Assign(aValue: TJSValue);
begin
  if (aValue is TJSString) or
     (aValue is TJSBoolean) or
     (aValue is TJSNumber) or
     (aValue is TJSNull) or
     (aValue is TJSUndefined) then
    fValue := aValue.AsNumber
  else
    inherited Assign(aValue);
end;

class function TJSNumber.GetTypeOf: TJSType;
begin
  result := jstNumber;
end;

{ TJSString }

class function TJSString.GetTypeOf: TJSType;
begin
  result := jstString;
end;

function TJSString.GetAsBoolean: Boolean;
begin
  result := JSStringToBoolean(fValue);
end;

function TJSString.GetAsNumber: Double;
begin
  result := JSStringToNumber(fValue);
end;

function TJSString.GetAsString: UTF8String;
begin
  result := fValue;
end;

procedure TJSString.SetAsBoolean(AValue: Boolean);
begin
  fValue := JSBooleanToString(AValue);
end;

procedure TJSString.SetAsNumber(AValue: Double);
begin
  fValue := JSNumberToString(AValue);
end;

procedure TJSString.SetAsString(AValue: UTF8String);
begin
  fValue := AValue;
end;

procedure TJSString.Assign(aValue: TJSValue);
begin
  if (aValue is TJSString) or
     (aValue is TJSBoolean) or
     (aValue is TJSNumber) or
     (aValue is TJSNull) or
     (aValue is TJSUndefined) then
    fValue := aValue.AsString
  else
    inherited Assign(aValue);
end;

{ TJSObject }

function TJSObject.GetAsBoolean: Boolean;
begin
  result := true;
end;

function TJSObject.GetAsNumber: Double;
begin
  result := math.NaN;
end;

function TJSObject.GetAsString: UTF8String;
begin
  result := '[object Object]';
end;

procedure TJSObject.SetAsBoolean(AValue: Boolean);
begin
  raise Exception.Create('Can''t set primitive value on object');
end;

procedure TJSObject.SetAsNumber(AValue: Double);
begin
  raise Exception.Create('Can''t set primitive value on object');
end;

procedure TJSObject.SetAsString(AValue: UTF8String);
begin
  raise Exception.Create('Can''t set primitive value on object');
end;

function TJSObject.CreateValue(aKey: UTF8String; aType: TJSValueClass
  ): TJSValue;
begin
  result := CreateValue(aType);
end;

function TJSObject.Put(aKey: UTF8String; aValue: TJSValue): TJSValue;
var
  idx: Integer;
  value: TJSValue;
begin
  // if the value is already there, then delete it, unless it's the same value
  // object. This allows us to notify the object after changing the value data.
  idx := fList.FindIndexOf(aKey);
  if idx > -1 then
  begin
     value := fList[idx] as TJSValue;
     if value <> aValue then
     begin
       fList.Delete(idx);
       if not ((value is TJSNull) or (value is TJSUndefined)) then
          FreeAndNil(value)
       else
          value := nil;

     end
     else
       Exit;
  end;
  fList.Add(aKey,aValue);
  result := aValue;
end;

procedure TJSObject.delete(aKey: UTF8String);
var
  idx: Integer;
  value: TJSValue;
begin
  idx := fList.FindIndexOf(aKey);
  if idx > -1 then
  begin
     value := fList[idx] as TJSValue;
     fList.Delete(idx);
     if not ((value is TJSNull) or (value is TJSUndefined)) then
        FreeAndNil(value)
     else
        value := nil;
     fList.Delete(idx);
  end;
end;

function TJSObject.Get(aKey: UTF8String): TJSValue;
begin
  result := fList.Find(aKey) as TJSValue;
  if result = nil then
     result := TJSUndefined.Undefined;
end;

constructor TJSObject.Create;
begin
  inherited Create;
  fList := TFPHashObjectList.Create(false);
end;

destructor TJSObject.Destroy;
var
  i: Integer;
  value: TJSValue;
begin
  // clear the list
  for i := 0 to fList.Count - 1 do
  begin
    value := fList[i] as TJSValue;
    if not ((value is TJSNull) or (value is TJSUndefined)) then
      value.Free;
  end;
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure TJSObject.Assign(aValue: TJSValue);
var
  lKeys: TStringArray;
  i: Integer;
  lValue: TJSValue;
begin
  if aValue is TJSObject then
  begin
    lKeys := (aValue as TJSObject).keys;
    for i := 0 to Length(lKeys) - 1 do
    begin
      lValue := (aValue as TJSObject).Get(lKeys[i]);
      if lValue is TJSArray then
        PutNewArray(lKeys[i]).Assign(lValue)
      else if lValue is TJSObject then
        PutNewObject(lKeys[i]).Assign(lValue)
      else if lValue is TJSNull then
        PutNull(lKeys[i])
      else if lValue is TJSUndefined then
        PutUndefined(lKeys[i])
      else if lValue is TJSString then
        Put(lKeys[i],lValue.AsString)
      else if lValue is TJSBoolean then
        Put(lKeys[i],lValue.AsBoolean)
      else if lValue is TJSNumber then
        Put(lKeys[i],lValue.AsNumber)
      else
      begin
        CreateValue(lKeys[i],TJSValueClass(lValue.ClassType)).Assign(lValue);
      end;
    end;
  end
  else
    inherited Assign(aValue);
end;

function TJSObject.hasOwnProperty(aKey: UTF8String): Boolean;
begin
  result := fList.FindIndexOf(aKey) > -1;
end;

class function TJSObject.GetTypeOf: TJSType;
begin
  result := jstObject;
end;

function TJSObject.keys: TStringArray;
var
  i: Integer;
begin
  SetLength(result,fList.Count);
  for i := 0 to fList.Count - 1 do
  begin
    result[i] := fList.NameOfIndex(i);
  end;
end;

function TJSObject.Move(aKey: UTF8String; aNewOwner: TJSObject;
  aNewKey: UTF8String): TJSValue;
var
  index: Integer;
begin
  index := fList.FindIndexOf(aKey);
  if index > -1 then
  begin
     result := fList[index] as TJSValue;
     fList.Delete(index);
  end
  else
      result := TJSValue.Undefined;

  if aNewOwner <> nil then
     aNewOwner.Put(aNewKey,result);

end;

end.

