unit test_sys_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_json, test_registry;

type

  { TJSONSpec }

  TJSONSpec = class(TTestSpec)
  published
    procedure Test_Create_values;
    procedure Test_Create_values_virtually;
    procedure Test_Primitives_store_correct_values;
    procedure Test_TypeOf_returns_correct_value;
    procedure Test_Add_values_to_object;
    procedure Test_Added_values_should_be_in_keys;
    procedure Test_deleting_keys;
    procedure Test_hasOwnProperty_should_work;
    procedure Test_equality_of_object_key_values;
    procedure Test_moving_object_key_value;
    procedure Test_array_length_value_should_be_same_as_length_key;
    procedure Test_array_length_should_increase_when_items_added;
    procedure Test_array_items_should_be_deleted_when_length_decreased;
    procedure Test_array_join;
    procedure Test_conversions;
    procedure Test_assign_and_clone;
    procedure Test_parsing;
    procedure Test_serializing;
  end;

  TTestObject = class(TJSObject)
  end;

implementation

{ TJSONSpec }

procedure TJSONSpec.Test_Create_values;
begin
  TJSString.Create.Free;
  TJSNumber.Create.Free;
  TJSBoolean.Create.Free;
  TJSObject.Create.Free;
  TJSArray.Create.Free;
end;

procedure TJSONSpec.Test_Create_values_virtually;
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
end;

procedure TJSONSpec.Test_Primitives_store_correct_values;
const
  STest: String = 'STEST';
  NTest: Double = 42;
  BTest: Boolean = true;
var
  DTest: TDateTime;
begin
  with TJSString.CreateString(STest) do
  try
    Assert(AsString = STest,'String values must return what is stored.');
  finally
    Free;
  end;

  with TJSNumber.CreateNumber(NTest) do
  try
    Assert(AsNumber = NTest,'Number values must return what is stored.');
  finally
    Free;
  end;

  with TJSBoolean.CreateBoolean(BTest) do
  try
    Assert(AsBoolean = BTest,'Boolean values must return what is stored.');
  finally
    Free;
  end;

  DTest := Now;
  with TJSDate.CreateDate(DTest) do
  try
    Assert(Value = DTest,'Date values must return what is stored.');
  finally
    Free;
  end;


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

procedure TJSONSpec.Test_TypeOf_returns_correct_value;
begin
  CheckTypeOf(TJSString,jstString,'string');
  CheckTypeOf(TJSNumber,jstNumber,'number');
  CheckTypeOf(TJSBoolean,jstBoolean,'boolean');
  CheckTypeOf(TJSObject,jstObject,'object');
  CheckTypeOf(TJSDate,jstString,'string');
  Assert(TJSValue.Null.TypeOf = jstNull,'null values should claim their typeof is "null"');
  Assert(TJSValue.Undefined.TypeOf = jstUndefined,'undefined values should claim their typeof is "undefined"');
end;

procedure TJSONSpec.Test_Add_values_to_object;
begin
  with TJSObject.Create do
  try
    Put('boolean',true);
    Put('number',42);
    Put('string','string');
    PutNull('null');
    PutUndefined('undefined');
    PutNewObject('object');
    PutNewArray('array');
  finally
    Free;
  end;
end;

procedure TJSONSpec.Test_Added_values_should_be_in_keys;
begin
  with TJSObject.Create do
  try
    Put('foo','bar');
    Assert(Length(keys) = 1,'Object should now contain exactly one key');
    Assert(keys[0] = 'foo','Objects only key should be "foo"');
  finally
    Free;
  end;
end;

procedure TJSONSpec.Test_deleting_keys;
begin
  with TJSObject.Create do
  try
    Put('foo','bar');
    Assert(Length(keys) = 1,'Object should now contain exactly one key');
    delete('foo');
    Assert(Length(keys) = 0,'Object should now be empty');
  finally
    Free;
  end;
end;

procedure TJSONSpec.Test_hasOwnProperty_should_work;
begin
  with TJSObject.Create do
  try
    Put('foo','bar');
    Assert(hasOwnProperty('foo'),'hasOwnProperty should return true after adding the key');
    Assert(not hasOwnProperty('bar'),'hasOwnProperty should return false if key has not been added');
  finally
    Free;
  end;

end;

procedure TJSONSpec.Test_equality_of_object_key_values;
const
  STest: String = 'STEST';
  NTest: Double = 42;
  BTest: Boolean = true;
begin
  with TJSObject.Create do
  try
    Put('string',STest);
    Put('number',NTest);
    Put('bool',BTest);
    PutNull('null');
    PutUndefined('undefined');
    Assert(Get('string').AsString = STest,'String values must return what is stored');
    Assert(Get('number').AsNumber = NTest,'Number values must return what is stored');
    Assert(Get('bool').AsBoolean = BTest,'Number values must return what is stored');
    Assert(Get('null') = TJSValue.Null,'Null value should be returned when stored');
    Assert(Get('undefined') = TJSValue.Undefined,'Undefined value should be returned when stored');
    Assert(Get('not defined') = TJSValue.Undefined,'Undefined value should be returned when a value is not stored');
  finally
    Free;
  end;

end;

procedure TJSONSpec.Test_moving_object_key_value;
var
  lValue: TJSValue;
begin
  with TJSObject.Create do
  try
    Put('foo','bar');
    lValue := Get('foo');
    Move('foo','bar');
    Assert(lValue = Get('bar'),'Moving between keys should not recreate the value object.');
  finally
    Free;
  end;

end;

procedure TJSONSpec.Test_array_length_value_should_be_same_as_length_key;
begin
  with TJSArray.Create do
  try
    Length := 2;
    Assert(Get('length').AsNumber = Length,'Length value should be same as length key');

  finally
    Free;
  end;
end;

procedure TJSONSpec.Test_array_length_should_increase_when_items_added;
begin
  with TJSArray.Create do
  try
    Put(0,'0');
    Assert(Get(0).AsString = '0','First element should exist');
    Assert(Length = 1,'Length should now be 1');
    Put(1,'1');
    Assert(Get(0).AsString = '0','First element should exist');
    Assert(Get(1).AsString = '1','Second element should exist');
    Assert(Length = 2,'Length should now be 2');

  finally
    Free;
  end;
end;

procedure TJSONSpec.Test_array_items_should_be_deleted_when_length_decreased;
begin
  with TJSArray.Create do
  try
    Put(0,'0');
    Put(1,'1');
    Put(2,'2');
    Assert(Length = 3,'Length should now be 3');
    Assert(hasOwnProperty(2),'Should have an item at index 2');
    Length := 2;
    Assert(not hasOwnProperty(2),'should not have an item at index 2');
  finally
    Free;
  end;
end;

procedure TJSONSpec.Test_array_join;
begin
  with TJSArray.Create do
  try
    Put(Length,'a');
    Assert(Join = 'a','Array join should work with one item');
    Put(Length,'b');
    Assert(Join = 'a,b','Array join should work with two items');
    Put(Length,'c');
    Assert(Join = 'a,b,c','Array join should work with three items');
    Assert(Join(';') = 'a;b;c','Array join should work with alternate delimiters');

  finally
    Free;
  end;
end;

procedure TJSONSpec.Test_conversions;
begin
  TJSString.CreateNumber(1).Free;
  TJSString.CreateBoolean(true).Free;
  TJSNumber.CreateString('42').Free;
  TJSNumber.CreateBoolean(true).Free;
  TJSBoolean.CreateString('x').Free;
  TJSBoolean.CreateNumber(2).Free;
  Assert(TJSValue.Null.AsNumber = 0,'Null should be 0');
  Assert(TJSValue.Null.AsString = 'null','Null should be "null"');
  Assert(TJSValue.Null.AsBoolean = false,'Null should be false');
  Assert(TJSValue.Undefined.AsNumber = 0,'Undefined should be 0');
  Assert(TJSValue.Undefined.AsString = 'undefined','Undefined should be "undefined"');
  Assert(TJSValue.Undefined.AsBoolean = false,'Undefined should be false');
end;

procedure TJSONSpec.Test_parsing;
var
  lFile1: TFileStream;
  lFile2: TFileStream;
  lObject1: TJSValue;
  lObject2: TJSValue;
begin

  lFile1 := TFileStream.Create('../test-data/json-parse-test.json',fmOpenRead + fmShareDenyNone);
  try
    lObject1 := FromJSON(lFile1);
    try
       Assert(lObject1.Get('glossary').Get('GlossDiv').Get('GlossList').Get('GlossEntry').get('GlossDef').get('GlossSeeAlso').get(1).AsString = 'XML','Data should have parsed correctly');

       lFile2 := TFileStream.Create('../test-data/json-parse-test.json',fmOpenRead + fmShareDenyNone);
       try
          // make sure this all works with a descendant of jsobject as well.
          lObject2 := FromJSON(TTestObject,lFile2);
          Assert(lObject2.DeepEquals(lObject1),'Objects parsed from the same file should be deep equals');
       finally
         lFile2.Free;
       end;
    finally
      lObject1.Free;
    end;
  finally
    lFile1.Free;
  end;

end;

procedure TJSONSpec.Test_serializing;
var
  lObject1: TJSValue;
  lObject2: TJSValue;
  lJSON: UTF8String;
begin
  lObject1 := TJSObject.Create;
  try
    lObject1.Put('string','foo');
    lObject1.Put('number',42);
    lObject1.Put('boolean',true);
    with lObject1.PutNewObject('object') do
    begin
      Put('string2','bar');
    end;

    with lObject1.PutNewArray('array') do
    begin
      Put(Get('length').AsString,'hi');
      Put(Get('length').AsString,'there!');
    end;
    // these "number" != null checks are in here because of an early bug found
    // using this test. I don't want the error to happen again, so I'm testing for
    // it here to avoid having it creeping up again.
    Assert(lObject1.Get('number') <> TJSValue.Null,'"number" property should not be null.');

    lJSON := ToJSON(lObject1);
    Assert(Pos('"number":null',lJSON) = 0,'"number" property should not have been written as null.');

    lObject2 := FromJSON(lJSON);
    try
      Assert(lObject2.Get('number') <> TJSValue.Null,'"number" property should not be null.');
      Assert(lObject2.DeepEquals(lObject1),'Object created from the JSON serialized from another object should be deep equals with that other object');
    finally
      lObject2.Free;
    end;
  finally
    lObject1.Free;
  end;
end;

procedure TJSONSpec.Test_assign_and_clone;
var
  a: TJSValue;
begin
  with TJSString.CreateString('foo') do
  try
    a := TJSString.CreateString('bar');
    try
      Assign(a);
      Assert(AsString = a.AsString,'assigned values should be the same.');
      Assert(DeepEquals(a),'assigned values should be deep equal.');
    finally
      a.Free;
    end;
  finally
    Free;
  end;

  with TJSNumber.CreateNumber(23.23) do
  try
    a := TJSNumber.CreateNumber(42.42);
    try
      Assign(a);
      Assert(AsNumber = a.AsNumber,'assigned values should be the same.');
      Assert(DeepEquals(a),'assigned values should be deep equal.');
    finally
      a.Free;
    end;
  finally
    Free;
  end;

  with TJSBoolean.CreateBoolean(true) do
  try
    a := TJSBoolean.CreateBoolean(false);
    try
      Assign(a);
      Assert(AsBoolean = a.AsBoolean,'assigned values should be the same.');
      Assert(DeepEquals(a),'assigned values should be deep equal.');
    finally
      a.Free;
    end;
  finally
    Free;
  end;

  with TJSString.CreateString('foo') do
  try
    a := TJSNumber.CreateNumber(42);
    try
      Assign(a);
      Assert(AsString = a.AsString,'assigned values should be the same.');
      Assert(AsString = '42','Assigning number to string should be the same as assigning the string value of that number');
      Assert(not DeepEquals(a),'assigned values should not be deep equal.');
    finally
      a.Free;
    end;
  finally
    Free;
  end;

  with TJSObject.Create do
  try
    a := TJSObject.Create;
    try
      a.Put('string','foo');
      a.Put('number',42);
      a.Put('boolean',true);
      with a.PutNewObject('object') do
      begin
        Put('string2','bar');
      end;
      Assign(a);
      Assert(DeepEquals(a),'assigned values should be deep equal.');
    finally
      a.Free;
    end;

    a := Clone;
    try
      Assert(a is TJSObject,'cloned object should be the same class');
      Assert(DeepEquals(a),'cloned object should be deep equals with the original');
    finally
      a.Free;
    end;
  finally
    Free;
  end;

end;

end.

