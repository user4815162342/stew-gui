unit test_sys_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_json, test_registry;

type

  { TJSONTester }

  TJSONTester = class(TTester)
  published
    procedure TestCreateValues(aCallback: TCallback);
    procedure TestVirtualCreateValues(aCallback: TCallback);
    procedure TestPrimitives(aCallback: TCallback);
    procedure TestTypeOf(aCallback: TCallback);
  end;

{
TODO: Tests to run:
- objects:
  - should be possible to add a key with any type of value to an object.
  - adding a key to an object should have that key appear in the keys function.
  - deleting a key should have that key no longer appear in the keys function.
  - hasOwnProperty should return true for any key that was added.
  - should be possible to retrieve a value by key and get an equal value that was added.
  - all key functions above should accept integers and behave the same.
  - moving a value between keys should not change the object
- arrays
  - Array length should return the same value as the length key
  - array length should increase when items are added beyond the length
  - array items should be deleted with the length is reduced
  - array join should work appropriately
- conversion
  - strings should convert to numbers appropriately
  - strings should convert to boolean appropriately
  - numbers should convert to strings appropriately
  - numbers should convert to boolean appropriately
  - boolean should convert to strings appropriately
  - boolean should convert to number appropriately
  - undefined should convert to any primitive appropriately
  - null should convert to any primitive appropriately
- lifetime:
  - should be able to .assign values to another value of the same base type
  - should be possible to .clone a value and get the same data and type.
- parsing:
  - valid JSON should be parsed into valid objects (look for a JSON test bed or something)
  - should be able to parse strings as well as streams
  - the same json string should always produce an equivalent object (use deepEquals)
  - should be able to parse json into a 'subclass' of object and get the appropriate class.

}

implementation

{ TJSONTester }

procedure TJSONTester.TestCreateValues(aCallback: TCallback);
begin
  TJSString.Create.Free;
  TJSNumber.Create.Free;
  TJSBoolean.Create.Free;
  TJSObject.Create.Free;
  TJSArray.Create.Free;
  aCallback;
end;

procedure TJSONTester.TestVirtualCreateValues(aCallback: TCallback);
var
  lType: TJSValueClass;
begin
  lType := TJSString;
  lType.Create.Free;
  lType := TJSNumber;
  lType.Create.Free;
  lType := TJSBoolean;
  lType.Create.Free;
  lType := TJSObject;
  lType.Create.Free;
  lType := TJSArray;
  lType.Create.Free;
  aCallback;
end;

procedure TJSONTester.TestPrimitives(aCallback: TCallback);
var
  lValue: TJSValue;
const
  STest: String = 'STEST';
  NTest: Double = 42;
  BTest: Boolean = true;
begin
  with TJSValue.MakeString(STest) do
  try
    Assert(AsString = STest,'String values must return what is stored.');
  finally
    Free;
  end;

  with TJSValue.MakeNumber(NTest) do
  try
    Assert(AsNumber = NTest,'Number values must return what is stored.');
  finally
    Free;
  end;

  with TJSValue.MakeBoolean(BTest) do
  try
    Assert(AsBoolean = BTest,'Boolean values must return what is stored.');
  finally
    Free;
  end;

  aCallback;

end;

procedure CheckTypeOf(aValueClass: TJSValueClass; aType: TJSType; aName: String);
begin
  with aValueClass.Create do
  try
    Assert(TypeOf = aType,aName + ' values should claim their typeof is "' + aName + '"');
  finally
    Free;
  end;
  Assert(aValueClass.GetTypeOf = aType,aName + ' class should claim its typeof is "' + aName + '"');

end;

procedure TJSONTester.TestTypeOf(aCallback: TCallback);
begin
  CheckTypeOf(TJSString,jstString,'string');
  CheckTypeOf(TJSNumber,jstNumber,'number');
  CheckTypeOf(TJSBoolean,jstBoolean,'boolean');
  CheckTypeOf(TJSObject,jstObject,'object');
  Assert(TJSValue.Null.TypeOf = jstNull,'null values should claim their typeof is "null"');
  Assert(TJSValue.Undefined.TypeOf = jstUndefined,'undefined values should claim their typeof is "undefined"');
  aCallback;
end;

end.

