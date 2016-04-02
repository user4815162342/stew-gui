unit sys_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_types, contnrs, jsonscanner;

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
  strict private
    class var fUndefined: TJSUndefined;
    class var fNull: TJSNull;
  strict protected
    // override this to behave the way Javascript '"" + x' might behave
    // for your given value.
    function GetAsString: UTF8String; virtual; abstract;
    // override this to behave the way Javascript 'Number(x)' might behave
    // for your given value.
    function GetAsNumber: Double; virtual; abstract;
    // override this to behave the way Javascript '!!x' might behave
    // for your given value.
    function GetAsBoolean: Boolean; virtual; abstract;
    // override this to behave the way Javascript 'x["key"] = y' might behave for your value of x
    // and any value of y of the specified type. You are allowed to not create a
    // new instance (perhaps returning the value that's already there), and return
    // a completely different type than what is requested. Other functionality will
    // expect it to behave like the requested type, however, so if you do this,
    // your type should be able to accept that behavior without error. If you plan
    // to do this, you can look at the GetTypeOf function on the requested type
    // to find the behavior expected.
    function CreateValue({%H-}aKey: UTF8String; {%H-}aRequestType: TJSValueClass): TJSValue; virtual; overload;
    function CreateStringValue({%H-}aKey: UTF8String; {%H-}aRequestType: TJSValueClass; {%H-}aValue: UTF8String): TJSValue; virtual; overload;
    function CreateNumberValue({%H-}aKey: UTF8String; {%H-}aRequestType: TJSValueClass; {%H-}aValue: Double): TJSValue; virtual; overload;
    function CreateBooleanValue({%H-}aKey: UTF8String; {%H-}aRequestType: TJSValueClass; {%H-}aValue: Boolean): TJSValue; virtual; overload;
    function CreateValue(aKey: Integer; aRequestType: TJSValueClass): TJSValue; overload;
    function CreateBooleanValue(aRequestType: TJSValueClass; aValue: Boolean
      ): TJSValue;
    function CreateNumberValue(aRequestType: TJSValueClass; aValue: Double
      ): TJSValue;
    function CreateStringValue(aRequestType: TJSValueClass; aValue: UTF8String
      ): TJSValue;
    function CreateValue(aRequestType: TJSValueClass): TJSValue;
    // override this to handle assigning a value to the specified key. This
    // value *should* have been created using CreateValue, so there shouldn't be
    // any reason to expect the wrong type here.
    function Put({%H-}aKey: UTF8String; {%H-}aValue: TJSValue): TJSValue; virtual; overload;
    function Put(aKey: Integer; aValue: TJSValue): TJSValue; overload;
  public
    constructor Create; virtual;
    constructor CreateString(aValue: UTF8String); virtual; abstract;
    constructor CreateNumber(aValue: Double); virtual; abstract;
    constructor CreateBoolean(aValue: Boolean); virtual; abstract;
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
    function GetDefault(aKey: UTF8String; aValue: UTF8String): UTF8String;
    function GetDefault(aKey: UTF8String; aValue: Double): Double;
    function GetDefault(aKey: UTF8String; aValue: Boolean): Boolean;
    // override this to behave the way Javascript 'delete x["key"]' might behave
    // for your given value.
    procedure delete({%H-}aKey: UTF8String); virtual; overload;
    // override this the same way as delete if your object can handle integer
    // keys.
    procedure delete(aKey: Integer); overload;
    function Put(aKey: UTF8String; aValue: UTF8String): TJSValue; virtual; overload;
    function Put(aKey: Integer; aValue: UTF8String): TJSValue; overload;
    function Put(aKey: UTF8String; aValue: Double): TJSValue; virtual; overload;
    function Put(aKey: Integer; aValue: Double): TJSValue; overload;
    function Put(aKey: UTF8String; aValue: Boolean): TJSValue; virtual; overload;
    function Put(aKey: Integer; aValue: Boolean): TJSValue; overload;
    // non-primitive 'puts' don't need to be virtual because their behavior
    // can be overridden in CreateValue and PutValue. The primitive ones
    // might need to be overridden in a way that doesn't deal with TJSValue.
    function PutNull(aKey: UTF8String): TJSValue; overload;
    function PutNull(aKey: Integer): TJSValue; overload;
    function PutUndefined(aKey: UTF8String): TJSValue; overload;
    function PutUndefined(aKey: Integer): TJSValue; overload;
    function PutNewObject(aKey: UTF8String): TJSValue; overload;
    function PutNewObject(aKey: Integer): TJSValue; overload;
    function PutNewArray(aKey: UTF8String): TJSValue; overload;
    function PutNewArray(aKey: Integer): TJSValue; overload;
  end;

  { TJSString }

  TJSString = class(TJSValue)
  strict private
    fValue: UTF8String;
  strict protected
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
  public
    constructor Create; override;
    constructor CreateBoolean(aValue: Boolean); override;
    constructor CreateNumber(aValue: Double); override;
    constructor CreateString(aValue: UTF8String); override;
    class function GetTypeOf: TJSType; override;
    property Value: UTF8String read fValue write fValue;
    procedure Assign(aValue: TJSValue); override;
  end;

  { TJSNumber }

  TJSNumber = class(TJSValue)
  strict private
    fValue: Double;
  strict protected
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
  public
    constructor Create; override;
    constructor CreateBoolean(aValue: Boolean); override;
    constructor CreateNumber(aValue: Double); override;
    constructor CreateString(aValue: UTF8String); override;
    class function GetTypeOf: TJSType; override;
    property Value: Double read fValue write fValue;
    procedure Assign(aValue: TJSValue); override;
  end;

  { TJSBoolean }

  TJSBoolean = class(TJSValue)
  strict private
    fValue: Boolean;
  strict protected
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
  public
    constructor Create; override;
    constructor CreateBoolean(aValue: Boolean); override;
    constructor CreateNumber(aValue: Double); override;
    constructor CreateString(aValue: UTF8String); override;
    class function GetTypeOf: TJSType; override;
    property Value: Boolean read fValue write fValue;
    procedure Assign(aValue: TJSValue); override;
  end;

  { TJSUndefined }

  // NOTE: Undefined is a singletone type. It is represented by 'nil'
  // in the data itself, and is never actually stored.
  TJSUndefined = class(TJSValue)
  strict protected
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    function GetAsBoolean: Boolean; override;
  public
    constructor Create; override;
    procedure Assign({%H-}aValue: TJSValue); override;
    constructor CreateBoolean({%H-}aValue: Boolean); override;
    constructor CreateNumber({%H-}aValue: Double); override;
    constructor CreateString({%H-}aValue: UTF8String); override;
    class function GetTypeOf: TJSType; override;
  end;

  { TJSNull }

  TJSNull = class(TJSValue)
  strict protected
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    function GetAsBoolean: Boolean; override;
  public
    constructor Create; override;
    procedure Assign({%H-}aValue: TJSValue); override;
    constructor CreateBoolean({%H-}aValue: Boolean); override;
    constructor CreateNumber({%H-}aValue: Double); override;
    constructor CreateString({%H-}aValue: UTF8String); override;
    class function GetTypeOf: TJSType; override;
  end;

  { TJSObject }
  // You can make "custom" objects fairly easily. Just:
  // - create a native property that translates between Get(aKey...) and Put(aKey..)
  // - override Put(aKey, TJSValue) to control what happens when a value is set.
  // - override RequestType(aKey, RequestType) to control what type is created
  //   for a given key.
  // - override CreateValue(aKey, RequestType) to more specifically control what
  //   happens when a new value is created.
  //
  // I considered making a "persistent" JSObject which automatically managed
  // published properties in the manner above, but complications arose when I
  // tried to abstract TJSValues as delphi native primitive types.
  //

  TJSObject = class(TJSValue)
  strict private
    fList: TFPHashObjectList;
  strict protected
    function Put(aKey: UTF8String; aValue: TJSValue): TJSValue; override;
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
    function RequestType({%H-}aKey: UTF8String; aType: TJSValueClass): TJSValueClass; virtual;
    function CreateValue({%H-}aKey: UTF8String; aType: TJSValueClass): TJSValue;
       override; overload;
    function CreateNumberValue({%H-}aKey: UTF8String; aRequestType: TJSValueClass;
      aValue: Double): TJSValue; override; overload;
    function CreateBooleanValue({%H-}aKey: UTF8String; aRequestType: TJSValueClass;
      aValue: Boolean): TJSValue; override; overload;
    function CreateStringValue({%H-}aKey: UTF8String; aRequestType: TJSValueClass;
      aValue: UTF8String): TJSValue; override; overload;
    property HashList: TFPHashObjectList read fList;
  public
    constructor Create; override;
    constructor CreateBoolean({%H-}aValue: Boolean); override;
    constructor CreateNumber({%H-}aValue: Double); override;
    constructor CreateString({%H-}aValue: UTF8String); override;
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
    function Move(aKey: UTF8String; aNewKey: UTF8String): TJSValue; overload;
    function Move(aKey: UTF8String; aNewOwner: TJSObject): TJSValue; overload;
  end;

  { TJSArray }
  TJSArray = class(TJSObject)
  strict private
  strict protected
    function CreateValue(aKey: UTF8String; aType: TJSValueClass): TJSValue;
      override; overload;
    function CreateStringValue(aKey: UTF8String; aRequestType: TJSValueClass;
      aValue: UTF8String): TJSValue; override; overload;
    function CreateBooleanValue(aKey: UTF8String; aRequestType: TJSValueClass;
      aValue: Boolean): TJSValue; override; overload;
    function CreateNumberValue(aKey: UTF8String; aRequestType: TJSValueClass;
       aValue: Double): TJSValue; override; overload;
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
    function IndexOf(aValue: TJSValue): Integer;
    function IndexOf(aValue: UTF8String): Integer;
    function IndexOf(aValue: Double): Integer;
    procedure Unshift;
    procedure Splice(aStart: Integer; aDeleteCount: Integer; aInsertCount: Integer = 0);
    function CreateList: TList;
  end;

  { TJSDate }

  TJSDate = class(TJSValue)
  strict private
    fValue: TDateTime;
  strict protected
    function GetAsBoolean: Boolean; override;
    function GetAsNumber: Double; override;
    function GetAsString: UTF8String; override;
  public
    constructor Create; override;
    constructor CreateBoolean({%H-}aValue: Boolean); override;
    constructor CreateNumber(aValue: Double); override;
    constructor CreateString(aValue: UTF8String); override;
    constructor CreateDate(aValue: TDateTime);
    class function GetTypeOf: TJSType; override;
    property Value: TDateTime read fValue write fValue;
    procedure Assign(aValue: TJSValue); override;
  end;

  { TJSONParser }
  EJSONParserSyntaxError = class(Exception)
  end;

  // Overriding persistence:
  //
  // Occasionally, you may need to override how JSON data is parsed into the object.
  // For example, you depend on being backwards compatible with an old JSON format
  // structure. There are several ways to do this, as indicated by this chart,
  // where the letters along the left side represent the old format types and
  // letters along the top indicate the new format types:
  //
  //   O A S N B
  // O 1 2 2 2 2
  // A 2 1 2 2 2
  // S 3 3 - 3 3
  // N 3 3 3 - 3
  // B 3 3 3 3 -
  //
  // 1. If both old and new are object or old and new are array, but the structure
  //    has changed: If old property names have been deprecated, simply override
  //    'Put' to put a converted value in the correct property. If member structures
  //    and types have changed, handle the conversion according to the table
  //    above.
  // 2. If the old type is an object and the new type is an array, or vice versa,
  //    or the new type is a primitive and the old type is structured:
  //    the parser will check if the value returned by 'PutNewObject' or 'PutNewArray'
  //    is the correct type for the syntax. If it is not, it will create a
  //    temporary object of of the correct type, and call Assign on the returned
  //    value once it is parsed. Do your conversion in the overridden Assign
  //    method.
  // 3. If the old value is primitive and the new value is a structure, or the
  //    new value is a primitive of a different type: make sure
  //    the subclass that handles the new value implements the CreateString,
  //    CreateNumber, CreateBoolean constructors in a way that converts
  //    appropriately.

  TJSONParser = class
  strict private
    fScanner: TJSONScanner;
  strict protected
    procedure SkipWhitespace;
    procedure Expected(const aMessage: String);
    procedure Unexpected(const aMessage: String);
  public
    constructor Create(aStream: TStream);
    destructor Destroy; override;
    function Parse: TJSValue;
    function Parse(aClass: TJSValueClass): TJSValue;
    function Parse(aClass: TJSValueClass; aCreationName: UTF8String; aParent: TJSValue): TJSValue;
    function ParseString(aClass: TJSValueClass; aCreationName: UTF8String; aParent: TJSValue): TJSValue;
    function ParseNumber(aClass: TJSValueClass; aCreationName: UTF8String; aParent: TJSValue): TJSValue;
    function ParseBoolean(aClass: TJSValueClass; aCreationName: UTF8String; aParent: TJSValue): TJSValue;
    function ParseNull(aClass: TJSValueClass; aCreationName: UTF8String; aParent: TJSValue): TJSValue;
    function ParseObject(aClass: TJSValueClass; aCreationName: UTF8String; aParent: TJSValue): TJSValue;
    procedure ParseObject(aObject: TJSObject);
    function ParseArray(aClass: TJSValueClass; aCreationName: UTF8String; aParent: TJSValue): TJSValue;
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
  function FromJSON(aClass: TJSValueClass; aData: UTF8String): TJSValue;
  function FromJSON(aData: UTF8String): TJSValue;
  function FromJSON(aStream: TStream): TJSValue;
  procedure FromJSON(aObject: TJSObject; aStream: TStream);

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
    if aValue <> '' then
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
        if (math.IsNan(aValue.AsNumber)) or (math.IsInfinite(aValue.AsNumber)) then
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
var
  lParser: TJSONParser;
begin
  lParser := TJSONParser.Create(aStream);
  try
    result := lParser.Parse(aClass);
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

procedure FromJSON(aObject: TJSObject; aStream: TStream);
var
  lParser: TJSONParser;
begin
  lParser := TJSONParser.Create(aStream);
  try
    lParser.ParseObject(aObject);
  finally
    lParser.Free;
  end;
end;

{ TJSDate }

function TJSDate.GetAsBoolean: Boolean;
begin
  result := true;
end;

function TJSDate.GetAsNumber: Double;
begin
  result := DateTimeToMsecs(fValue);
end;

function TJSDate.GetAsString: UTF8String;
begin
  result := DateTimeToISO8601(fValue,True);
end;

constructor TJSDate.Create;
begin
  CreateDate(Now);
end;

constructor TJSDate.CreateBoolean(aValue: Boolean);
begin
  CreateDate(Now);
end;

constructor TJSDate.CreateNumber(aValue: Double);
begin
  CreateDate(MsecsToDateTime(trunc(aValue)));

end;

constructor TJSDate.CreateString(aValue: UTF8String);
begin
  CreateDate(ISO8601ToDateTime(aValue));
end;

constructor TJSDate.CreateDate(aValue: TDateTime);
begin
  inherited Create;
  fValue := aValue;
end;

class function TJSDate.GetTypeOf: TJSType;
begin
  result := jstString;
end;

procedure TJSDate.Assign(aValue: TJSValue);
begin
  if (aValue is TJSValue) then
  begin
    fValue := (aValue as TJSDate).fValue;
  end
  else if (aValue is TJSString) then
  begin
    fValue := ISO8601ToDateTime(aValue.AsString)
  end
  else if (aValue is TJSNumber) then
  begin
    fValue := MsecsToDateTime(trunc(aValue.AsNumber));
  end
  else if (aValue is TJSNumber) or
     (aValue is TJSNull) or
     (aValue is TJSUndefined) then
  begin
    fValue := Now
  end
  else
    inherited Assign(aValue);
end;

{ TJSONParser }

procedure TJSONParser.SkipWhitespace;
begin
  while fScanner.CurToken = tkWhitespace do
    fScanner.FetchToken;
end;

procedure TJSONParser.Expected(const aMessage: String);
begin
  raise EJSONParserSyntaxError.Create('(' + IntToStr(fScanner.CurRow) + ',' + IntToStr(fScanner.CurColumn) + ') Expected ' + aMessage);
end;

procedure TJSONParser.Unexpected(const aMessage: String);
begin
  raise EJSONParserSyntaxError.Create('(' + IntToStr(fScanner.CurRow) + ',' + IntToStr(fScanner.CurColumn) + ') Unexpected ' + aMessage);
end;

function TJSONParser.ParseString(aClass: TJSValueClass;
  aCreationName: UTF8String; aParent: TJSValue): TJSValue;
begin
  SkipWhitespace;
  if fScanner.CurToken = tkString then
  begin
    if aClass = nil then
      aClass := TJSString;
    if aParent <> nil then
      result := aParent.Put(aCreationName,fScanner.CurTokenString)
    else
      result := aClass.CreateString(fScanner.CurTokenString);
    fScanner.FetchToken;
  end
  else
    Expected('string');
end;

function TJSONParser.ParseNumber(aClass: TJSValueClass;
  aCreationName: UTF8String; aParent: TJSValue): TJSValue;
begin
  SkipWhitespace;
  if fScanner.CurToken = tkNumber then
  begin
    if aClass = nil then
      aClass := TJSNumber;
    if aParent <> nil then
      result := aParent.Put(aCreationName,JSStringToNumber(fScanner.CurTokenString))
    else
      result := aClass.CreateNumber(JSStringToNumber(fScanner.CurTokenString));
    fScanner.FetchToken;
  end
  else
    Expected('number');
end;

function TJSONParser.ParseBoolean(aClass: TJSValueClass;
  aCreationName: UTF8String; aParent: TJSValue): TJSValue;
begin
  SkipWhitespace;
  if fScanner.CurToken in [tkTrue,tkFalse] then
  begin
    if aClass = nil then
      aClass := TJSBoolean;
    if aParent <> nil then
      result := aParent.Put(aCreationName,fScanner.CurToken = tkTrue)
    else
      result := aClass.CreateBoolean(fScanner.CurToken = tkTrue);
    fScanner.FetchToken;
  end
  else
    Expected('boolean');

end;

function TJSONParser.ParseNull(aClass: TJSValueClass;
  aCreationName: UTF8String; aParent: TJSValue): TJSValue;
begin
  SkipWhitespace;
  if fScanner.CurToken = tkNull then
  begin
    if (aClass = nil) then
       aClass := TJSNull;
    if aParent <> nil then
      result := aParent.PutNull(aCreationName)
    else if aClass = TJSNull then
      result := TJSValue.Null
    else
      result := aClass.Create;
    fScanner.FetchToken
  end
  else
    Expected('null');

end;

function TJSONParser.ParseObject(aClass: TJSValueClass;
  aCreationName: UTF8String; aParent: TJSValue): TJSValue;
var
  lAssignToObject: TJSValue;
begin
  if aClass = nil then
    aClass := TJSObject;
  if aParent <> nil then
     result := aParent.PutNewObject(aCreationName)
  else
     result := aClass.Create;
  try
    // if the wrong type is returned, then we are possibly using
    // an old syntax, so create the correct type and we will assign
    // to it later.
    if (result is TJSArray) or (not (result is TJSObject)) then
    begin
      lAssignToObject := Result;
      result := TJSObject.Create;
    end
    else
    begin
      // make sure this is nil so we know not to assign.
      lAssignToObject := nil;
    end;

    try
      ParseObject(result as TJSObject);


      // if we are assigning to an actual object, do the
      // assignment.
      if lAssignToObject <> nil then
      begin
        lAssignToObject.Assign(Result);
      end;
    finally
      // if we were assigning to an actual object, free
      // the fake result.
      if lAssignToObject <> nil then
      begin
        FreeAndNil(Result);
        result := lAssignToObject;
      end;
    end;
  except
    // if the parent is nil, then the result is not owned,
    // so free it.
    if (aParent = nil) then
       FreeAndNil(result);
    raise;
  end;

end;

procedure TJSONParser.ParseObject(aObject: TJSObject);
var
  lKey: UTF8String;
begin
  SkipWhitespace;
  if fScanner.CurToken <> tkCurlyBraceOpen then
    Expected('object start');
  fScanner.FetchToken;
  SkipWhitespace;
  // look for a property
  while not (fScanner.CurToken in [tkCurlyBraceClose,tkEOF]) do
  begin
    if fScanner.CurToken <> tkString then
      Expected('property name');
    lKey := fScanner.CurTokenString;
    fScanner.FetchToken;
    SkipWhitespace;
    if fScanner.CurToken <> tkColon then
      Expected('colon');
    fScanner.FetchToken;
    Parse(nil,lKey,aObject);
    SkipWhitespace;
    if fScanner.CurToken = tkComma then
    begin
      fScanner.FetchToken;
      SkipWhitespace;
    end
    else if fScanner.CurToken <> tkCurlyBraceClose then
      Expected('comma');
  end;
  if fScanner.CurToken <> tkCurlyBraceClose then
    Expected('object end');
  fScanner.FetchToken;

end;

function TJSONParser.ParseArray(aClass: TJSValueClass;
  aCreationName: UTF8String; aParent: TJSValue): TJSValue;
var
  lKey: Integer;
  lAssignToObject: TJSValue;
begin
  if aClass = nil then
     aClass := TJSArray;
  if aParent <> nil then
     result := aParent.PutNewArray(aCreationName)
  else
    result := aClass.Create;
  try
    // if the wrong type is returned, then we are possibly using
    // an old syntax, so create the correct type and we will assign
    // to it later.
    if (not (result is TJSArray)) then
    begin
      lAssignToObject := Result;
      result := TJSArray.Create;
    end
    else
    begin
      // make sure this is nil so we know not to assign.
      lAssignToObject := nil;
    end;

    try

      SkipWhitespace;
      if fScanner.CurToken <> tkSquaredBraceOpen then
        Expected('array start');
      fScanner.FetchToken;
      SkipWhitespace;
      // look for a item
      lKey := 0;
      while not (fScanner.CurToken in [tkSquaredBraceClose,tkEOF]) do
      begin
        Parse(nil,IntToStr(lKey),Result);
        SkipWhitespace;
        if fScanner.CurToken = tkComma then
        begin
          fScanner.FetchToken;
          lKey := lKey + 1;
          SkipWhitespace;
        end
        else if fScanner.CurToken <> tkSquaredBraceClose then
          Expected('comma');
      end;
      if fScanner.CurToken <> tkSquaredBraceClose then
        Expected('array end');
      fScanner.FetchToken;

      // if we are assigning to an actual object, do the
      // assignment.
      if lAssignToObject <> nil then
      begin
        lAssignToObject.Assign(Result);
      end;
    finally
      // if we were assigning to an actual object, free
      // the fake result.
      if lAssignToObject <> nil then
      begin
        FreeAndNil(Result);
        result := lAssignToObject;
      end;
    end;

  except
    // if the parent is nil, then the result is not owned,
    // so free it.
    if aParent = nil then
       FreeAndNil(result);
    raise;
  end;

end;

constructor TJSONParser.Create(aStream: TStream);
begin
  fScanner := TJSONScanner.Create(aStream,true);
  fScanner.Strict := true;
  fScanner.FetchToken;
end;

destructor TJSONParser.Destroy;
begin
  FreeAndNil(fScanner);
  inherited Destroy;
end;

function TJSONParser.Parse: TJSValue;
begin
  result := Parse(nil,'',nil);
end;

function TJSONParser.Parse(aClass: TJSValueClass): TJSValue;
begin
  result := Parse(aClass,'',nil);
end;

function TJSONParser.Parse(aClass: TJSValueClass; aCreationName: UTF8String;
  aParent: TJSValue): TJSValue;
begin
  Skipwhitespace;
  case fScanner.CurToken of
    tkEOF:
      Unexpected('end of file');
    //tkWhitespace,
    tkString:
    begin
      result := ParseString(aClass,aCreationName,aParent);
    end;
    tkNumber:
    begin
      result := ParseNumber(aClass,aCreationName,aParent);
    end;
    tkTrue, tkFalse:
    begin
      result := ParseBoolean(aClass,aCreationName,aParent);
    end;
    tkNull:
    begin
      result := ParseNull(aClass,aCreationName,aParent);
    end;
    // Simple (one-character) tokens
    tkComma:
      Unexpected('comma');
    tkColon:
      Unexpected('colon');
    tkCurlyBraceOpen:
    begin
      result := ParseObject(aClass,aCreationName,aParent);
    end;
    tkCurlyBraceClose:
      Unexpected('object end');
    tkSquaredBraceOpen:
    begin
      result := ParseArray(aClass,aCreationName,aParent);
    end;
    tkSquaredBraceClose:
      Unexpected('array end');
    tkIdentifier:
      Unexpected('identifier'); // only strict JSON is allowed.
    tkUnknown:
      Unexpected('unknown token');
  end;
end;

{ TJSArray }

function TJSArray.CreateValue(aKey: UTF8String; aType: TJSValueClass): TJSValue;
begin
  if aKey = LengthKey then
    raise Exception.Create('Invalid value for array length');
  Result:=inherited CreateValue(aKey, aType);
end;

function TJSArray.CreateStringValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: UTF8String): TJSValue;
begin
  if aKey = LengthKey then
     result := CreateNumberValue(aKey,TJSNumber,JSStringToNumber(aValue))
  else
     Result:=inherited CreateStringValue(aKey, aRequestType, aValue);
end;

function TJSArray.CreateBooleanValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: Boolean): TJSValue;
begin
  if aKey = LengthKey then
     result := CreateNumberValue(aKey,TJSNumber,JSBooleanToNumber(aValue))
  else
     Result:=inherited CreateBooleanValue(aKey, aRequestType, aValue);
end;

function TJSArray.CreateNumberValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: Double): TJSValue;
var
  lNewLength: Integer;
begin
  if aKey = LengthKey then
  begin
    lNewLength := trunc(aValue);
    if lNewLength <> aValue then
        raise Exception.Create('Length value is out of range');
    result := TJSNumber.CreateNumber(lNewLength);
  end
  else
     Result:=inherited CreateNumberValue(aKey, aRequestType, aValue);
end;

function TJSArray.Put(aKey: UTF8String; aValue: TJSValue): TJSValue;
var
  lOldLength: Integer;
  lNewLength: Integer;
  lIndex: Integer;
begin
  lOldLength := Length;
  if aKey = LengthKey then
  begin
    // Should be automatically created by CreateValue, so we know
    // it's a TJSNumber and the value is valid.
    lNewLength := Trunc(aValue.AsNumber);
    while lNewLength < lOldLength do
    begin
      lOldLength := lOldLength - 1;
      delete(lOldLength);
    end;
    result := inherited Put(aKey,aValue);
  end
  else if TryStrToInt(aKey,lIndex) then
  begin
    result := inherited Put(aKey,aValue);
    if lIndex > (lOldLength - 1) then
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
var
  lResult: Double;
begin
  lResult := Get(LengthKey).AsNumber;
  if math.IsNan(lResult) then
     result := 0
  else
     result := Trunc(lResult);
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

procedure TJSArray.Unshift;
begin
  Splice(0,0,1);
end;

procedure TJSArray.Splice(aStart: Integer; aDeleteCount: Integer;
  aInsertCount: Integer);
var
  O: TJSObject;
  lenVal: TJSValue;
  len: Integer;
  relativeStart: Integer;
  actualStart: Integer;
  actualDeleteCount: Integer;
  from: UTF8String;
  &to: UTF8String;
  fromPresent: Boolean;
  itemCount: Integer;
  k: Integer;
begin
  // Follow the EcmaScript algorithm, except:
  // - the insert count simply indicates how many indices to leave blank,
  // they must be filled in later.
  // - do not return the array of deleted items. I don't need it, do you?
  O := Self;
  lenVal := O.Get('length');
  len := trunc(lenVal.AsNumber);
  relativeStart:=aStart;
  if relativeStart < 0 then
     actualStart := Math.Max((len + relativeStart),0)
  else
     actualStart := math.Min(relativeStart,len);
  actualDeleteCount := math.Min(math.Max(aDeleteCount,0), len - actualStart);
  k := 0;
  itemCount:=aInsertCount;
  if itemCount < actualDeleteCount then
  begin
    k := actualStart;
    while k < (len - actualDeleteCount) do
    begin
      from := IntToStr(k + actualDeleteCount);
      &to := IntToStr(k + itemCount);
      fromPresent:=O.hasOwnProperty(from);
      if fromPresent then
      begin
        O.Move(from,&to);
      end
      else
        O.delete(&to);
      inc(k)
    end;
    k := len;
    while k > (len - actualDeleteCount + itemCount) do
    begin
      O.delete(IntToStr(k - 1));
      Dec(k);

    end;
  end
  else if itemCount > actualDeleteCount then
  begin
    k := (len - actualDeleteCount);
    while k > actualStart do
    begin
      from := IntToStr(k + actualDeleteCount - 1);
      &to := IntToStr(k + itemCount - 1);
      fromPresent := O.hasOwnProperty(from);
      if fromPresent then
      begin
        O.Move(from,&to);
      end
      else
        O.delete(&to);
      Dec(k);
    end;
  end;
  k := actualStart;
  O.Put('length',(len - actualDeleteCount + itemCount));
end;

function TJSArray.CreateList: TList;
var
  i: Integer;
begin
  result := TList.Create;
  try
    for i := 0 to Length - 1 do
      result.Add(Get(i));
  except
    result.Free;
    raise;
  end;
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

function TJSArray.IndexOf(aValue: TJSValue): Integer;
begin
  for result := 0 to Length - 1 do
  begin
    if Get(result) = aValue then
       Exit;
  end;
  result := -1;
end;

function TJSArray.IndexOf(aValue: UTF8String): Integer;
begin
  for result := 0 to Length - 1 do
  begin
    if Get(result).AsString = aValue then
       Exit;
  end;
  result := -1;

end;

function TJSArray.IndexOf(aValue: Double): Integer;
begin
  for result := 0 to Length - 1 do
  begin
    if Get(result).AsNumber = aValue then
       Exit;
  end;
  result := -1;

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

constructor TJSUndefined.CreateBoolean(aValue: Boolean);
begin
  raise Exception.Create('Can''t set value of undefined');
end;

constructor TJSUndefined.CreateNumber(aValue: Double);
begin
  raise Exception.Create('Can''t set value of undefined');
end;

constructor TJSUndefined.CreateString(aValue: UTF8String);
begin
  raise Exception.Create('Can''t set value of undefined');
end;

{ TJSValue }

function TJSValue.CreateValue(aRequestType: TJSValueClass): TJSValue;
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

function TJSValue.CreateStringValue(aRequestType: TJSValueClass;
  aValue: UTF8String): TJSValue;
begin
  if aRequestType <> nil then
  begin;
     if aRequestType = TJSUndefined then
       result := TJSUndefined.Undefined
     else if aRequestType = TJSNull then
       result := TJSNull.Null
     else
       result := aRequestType.CreateString(aValue)
  end
  else
     result := TJSUndefined.Undefined;
end;

function TJSValue.CreateNumberValue(aRequestType: TJSValueClass; aValue: Double
  ): TJSValue;
begin
  if aRequestType <> nil then
  begin;
     if aRequestType = TJSUndefined then
       result := TJSUndefined.Undefined
     else if aRequestType = TJSNull then
       result := TJSNull.Null
     else
       result := aRequestType.CreateNumber(aValue)
  end
  else
     result := TJSUndefined.Undefined;
end;

function TJSValue.CreateBooleanValue(aRequestType: TJSValueClass;
  aValue: Boolean): TJSValue;
begin
  if aRequestType <> nil then
  begin;
     if aRequestType = TJSUndefined then
       result := TJSUndefined.Undefined
     else if aRequestType = TJSNull then
       result := TJSNull.Null
     else
       result := aRequestType.CreateBoolean(aValue)
  end
  else
     result := TJSUndefined.Undefined;
end;

function TJSValue.{%H-}CreateValue(aKey: UTF8String; aRequestType: TJSValueClass): TJSValue;
begin
  raise Exception.Create('This type does not support contained objects.');
end;

function TJSValue.{%H-}CreateStringValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: UTF8String): TJSValue;
begin
  raise Exception.Create('This type does not support contained objects.');
end;

function TJSValue.{%H-}CreateNumberValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: Double): TJSValue;
begin
  raise Exception.Create('This type does not support contained objects.');
end;

function TJSValue.{%H-}CreateBooleanValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: Boolean): TJSValue;
begin
  raise Exception.Create('This type does not support contained objects.');
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
  raise Exception.Create('I don''t know how to assign this js object to another object.');
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

function TJSValue.GetDefault(aKey: UTF8String; aValue: UTF8String): UTF8String;
begin
  if hasOwnProperty(aKey) then
     result := Get(aKey).AsString
  else
     result := aValue;
end;

function TJSValue.GetDefault(aKey: UTF8String; aValue: Double): Double;
begin
  if hasOwnProperty(aKey) then
     result := Get(aKey).AsNumber
  else
     result := aValue;

end;

function TJSValue.GetDefault(aKey: UTF8String; aValue: Boolean): Boolean;
begin
  if hasOwnProperty(aKey) then
     result := Get(aKey).AsBoolean
  else
     result := aValue;

end;

procedure TJSValue.delete(aKey: UTF8String);
begin
  raise Exception.Create('Object type does not support properties');
end;

procedure TJSValue.delete(aKey: Integer);
begin
  delete(IntToStr(aKey));

end;

function TJSValue.Put(aKey: UTF8String; aValue: UTF8String): TJSValue;
begin
  result := Put(aKey,CreateStringValue(aKey,TJSString,aValue));
end;

function TJSValue.Put(aKey: Integer; aValue: UTF8String): TJSValue;
begin
  result := Put(IntToStr(aKey),aValue);
end;

function TJSValue.Put(aKey: UTF8String; aValue: Double): TJSValue;
begin
  result := Put(aKey,CreateNumberValue(aKey,TJSNumber,aValue));

end;

function TJSValue.Put(aKey: Integer; aValue: Double): TJSValue;
begin
  result := Put(IntToStr(aKey),aValue);

end;

function TJSValue.Put(aKey: UTF8String; aValue: Boolean): TJSValue;
begin
  result := Put(aKey,CreateBooleanValue(aKey,TJSBoolean,aValue));

end;

function TJSValue.Put(aKey: Integer; aValue: Boolean): TJSValue;
begin
  result := Put(IntToStr(aKey),aValue);
end;

function TJSValue.PutNull(aKey: UTF8String): TJSValue;
begin
  result := Put(aKey,CreateValue(aKey,TJSNull));
end;

function TJSValue.PutNull(aKey: Integer): TJSValue;
begin
  result := PutNull(IntToStr(aKey));
end;

function TJSValue.PutUndefined(aKey: UTF8String): TJSValue;
begin
  result := Put(aKey,CreateValue(aKey,TJSUndefined));
end;

function TJSValue.PutUndefined(aKey: Integer): TJSValue;
begin
  result := PutUndefined(IntToStr(aKey));
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

constructor TJSNull.CreateBoolean(aValue: Boolean);
begin
  raise Exception.Create('Can''t set value of null');
end;

constructor TJSNull.CreateNumber(aValue: Double);
begin
  raise Exception.Create('Can''t set value of null');
end;

constructor TJSNull.CreateString(aValue: UTF8String);
begin
  raise Exception.Create('Can''t set value of null');
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

constructor TJSBoolean.Create;
begin
  CreateBoolean(False);
end;

constructor TJSBoolean.CreateBoolean(aValue: Boolean);
begin
  inherited Create;
  fValue := aValue;
end;

constructor TJSBoolean.CreateNumber(aValue: Double);
begin
  CreateBoolean(JSNumbertoBoolean(aValue));

end;

constructor TJSBoolean.CreateString(aValue: UTF8String);
begin
  CreateBoolean(JSStringToBoolean(aValue));

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

constructor TJSNumber.Create;
begin
  CreateNumber(0);
end;

constructor TJSNumber.CreateBoolean(aValue: Boolean);
begin
  CreateNumber(JSBooleanToNumber(aValue));
end;

constructor TJSNumber.CreateNumber(aValue: Double);
begin
  inherited Create;
  fValue := aValue;

end;

constructor TJSNumber.CreateString(aValue: UTF8String);
begin
  CreateNumber(JSStringToNumber(aValue));
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

constructor TJSString.Create;
begin
  CreateString('');
end;

constructor TJSString.CreateBoolean(aValue: Boolean);
begin
  CreateString(JSBooleanToString(aValue));
end;

constructor TJSString.CreateNumber(aValue: Double);
begin
  CreateString(JSNumberToString(aValue));
end;

constructor TJSString.CreateString(aValue: UTF8String);
begin
  inherited Create;
  fValue := aValue;

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

function TJSObject.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  result := aType;
end;

function TJSObject.CreateValue(aKey: UTF8String; aType: TJSValueClass
  ): TJSValue;
begin
  result := CreateValue(RequestType(aKey,aType));
end;

function TJSObject.CreateNumberValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: Double): TJSValue;
begin
  result := CreateNumberValue(RequestType(aKey,aRequestType),aValue);
end;

function TJSObject.CreateBooleanValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: Boolean): TJSValue;
begin
  Result:=CreateBooleanValue(RequestType(aKey,aRequestType), aValue);
end;

function TJSObject.CreateStringValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: UTF8String): TJSValue;
begin
  result := CreateStringValue(RequestType(aKey,aRequestType),aValue);
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

constructor TJSObject.CreateBoolean(aValue: Boolean);
begin
  raise Exception.Create('Can''t set primitive value on object');
end;

constructor TJSObject.CreateNumber(aValue: Double);
begin
  raise Exception.Create('Can''t set primitive value on object');
end;

constructor TJSObject.CreateString(aValue: UTF8String);
begin
  raise Exception.Create('Can''t set primitive value on object');
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

function TJSObject.Move(aKey: UTF8String; aNewKey: UTF8String): TJSValue;
begin
  result := Move(aKey,Self,aNewKey);
end;

function TJSObject.Move(aKey: UTF8String; aNewOwner: TJSObject): TJSValue;
begin
  result := Move(aKey,aNewOwner,aKey);
end;

end.

