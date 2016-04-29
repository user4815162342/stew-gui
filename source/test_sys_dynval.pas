unit test_sys_dynval;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_dynval, sys_dynval_json, test_registry;

type

  { TDynValSpec }

  TDynValSpec = class(TTestSpec)
  private
  published
    procedure Test_Create_values;
    procedure Test_Primitives_store_correct_values;
    procedure Test_Added_values_should_be_in_keys;
    procedure Test_Add_values_to_map;
    procedure Test_has_should_work;
    procedure Test_list_items_should_be_deleted_when_length_decreased;
    procedure Test_list_length_should_increase_when_items_added;
    procedure Test_deleting_keys;
    procedure Test_equality_of_object_key_values;
    procedure Test_moving_object_key_value;
    procedure Test_parsing;
    procedure Test_serializing;
  end;

implementation

{ TDynValSpec }

procedure TDynValSpec.Test_Create_values;
begin
  // In theory these items should be automatically dereffed and freed.
  // I'm not exactly certain how to test this.
  TDynamicValues.NewString('test');
  TDynamicValues.NewNumber(42);
  TDynamicValues.Boolean(true);
  TDynamicValues.NewMap;
  TDynamicValues.NewList;
end;

procedure TDynValSpec.Test_Primitives_store_correct_values;
const
  STest: String = 'STEST';
  NTest: Double = 42;
  BTest: Boolean = true;
begin
  with TDynamicValues.NewString(STest) do
  begin
    Assert(Value = STest,'String values must return what is stored.');
  end;

  with TDynamicValues.NewNumber(NTest) do
  begin
    Assert(Value = NTest,'Number values must return what is stored.');
  end;

  with TDynamicValues.Boolean(BTest) do
  begin
    Assert(Value = BTest,'Boolean values must return what is stored.');
  end;

end;

procedure TDynValSpec.Test_Add_values_to_map;
begin
  with TDynamicValues.NewMap do
  begin
    Item['boolean'] := TDynamicValues.Boolean(true);
    Item['number'] := TDynamicValues.NewNumber(42);
    Item['string'] := TDynamicValues.NewString('string');
    Item['null'] := TDynamicValues.Null;
    Item['object'] := TDynamicValues.NewMap;
    Item['array'] := TDynamicValues.NewList;
  end;
end;

procedure TDynValSpec.Test_Added_values_should_be_in_keys;
begin
  with TDynamicValues.NewMap do
  begin
    Item['foo'] := TDynamicValues.NewString('bar');
    Assert(Length(GetKeys) = 1,'Object should now contain exactly one key');
    Assert(GetKeys[0] = 'foo','Objects only key should be "foo"');
  end;
end;

procedure TDynValSpec.Test_deleting_keys;
begin
  with TDynamicValues.NewMap do
  begin
    Item['foo'] := TDynamicValues.NewString('bar');
    Assert(Length(GetKeys) = 1,'Object should now contain exactly one key');
    delete('foo');
    Assert(Length(GetKeys) = 0,'Object should now be empty');
  end;
end;

procedure TDynValSpec.Test_has_should_work;
begin
  with TDynamicValues.NewMap do
  begin
    Item['foo'] := TDynamicValues.NewString('bar');
    Assert(Has('foo'),'hasOwnProperty should return true after adding the key');
    Assert(not Has('bar'),'hasOwnProperty should return false if key has not been added');
  end;

end;

procedure TDynValSpec.Test_equality_of_object_key_values;
const
  STest: String = 'STEST';
  NTest: Double = 42;
  BTest: Boolean = true;
begin
  with TDynamicValues.NewMap do
  begin
    Item['string'] := TDynamicValues.NewString(STest);
    Item['number'] := TDynamicValues.NewNumber(NTest);
    Item['bool'] := TDynamicValues.Boolean(BTest);
    Item['null'] := TDynamicValues.Null;
    Assert(IDynamicString(Item['string']).Value = STest,'String values must return what is stored');
    Assert(IDynamicNumber(Item['number']).Value = NTest,'Number values must return what is stored');
    Assert(IDynamicBoolean(Item['bool']).Value = BTest,'Number values must return what is stored');
    Assert(Item['null'] is IDynamicNull,'Null value should be returned when stored');
    Assert(not Item['not defined'].IsDefined,'Undefined value should be returned when a value is not stored');
  end;

end;

procedure TDynValSpec.Test_moving_object_key_value;
var
  lValue: IDynamicValue;
begin
  with TDynamicValues.NewMap do
  begin
    Item['foo'] := TDynamicValues.NewString('bar');
    lValue := Item['foo'];
    Item['bar'] := lValue;
    Assert(lValue = Item['bar'],'Moving between keys should not recreate the value object.');
  end;

end;

procedure TDynValSpec.Test_list_length_should_increase_when_items_added;
begin
  with TDynamicValues.NewList do
  begin
    Item[0] := TDynamicValues.NewNumber(0);
    Assert(IDynamicNumber(Item[0]).Value = 0,'First element should exist');
    Assert(Length = 1,'Length should now be 1');
    Item[1] := TDynamicValues.NewNumber(1);
    Assert(IDynamicNumber(Item[0]).Value = 0,'First element should exist');
    Assert(IDynamicNumber(Item[1]).Value = 1,'Second element should exist');
    Assert(Length = 2,'Length should now be 2');

  end;
end;

procedure TDynValSpec.Test_list_items_should_be_deleted_when_length_decreased;
begin
  with TDynamicValues.NewList do
  begin
    Item[0] := 0;
    Item[1] := 1;
    Item[2] := 2;
    Assert(Length = 3,'Length should now be 3');
    Assert(Item[2].IsDefined,'Should have an item at index 2');
    Length := 2;
    Assert(not Item[2].IsDefined,'should not have an item at index 2');
  end;
end;

procedure TDynValSpec.Test_parsing;
var
  lFile1: TFileStream;
  lFile2: TFileStream;
  lObject1: IDynamicValue;
  lObject2: IDynamicValue;
begin

  lFile1 := TFileStream.Create('../test-data/json-parse-test.json',fmOpenRead + fmShareDenyNone);
  try
    lObject1 := FromJSON(lFile1);
       Assert(IDynamicString(lObject1['glossary']['GlossDiv']['GlossList']['GlossEntry']['GlossDef']['GlossSeeAlso'][1]).Value = 'XML','Data should have parsed correctly');

       lFile2 := TFileStream.Create('../test-data/json-parse-test.json',fmOpenRead + fmShareDenyNone);
       try
          // make sure this all works with a descendant of jsobject as well.
          lObject2 := FromJSON(lFile2);
          Assert(lObject2.EqualsDeeply(lObject1),'Objects parsed from the same file should be deep equals');
       finally
         lFile2.Free;
       end;
  finally
    lFile1.Free;
  end;

end;

procedure TDynValSpec.Test_serializing;
var
  lObject1: IDynamicValue;
  lObject2: IDynamicValue;
  lJSON: UTF8String;
begin
  lObject1 := TDynamicValues.NewMap;
  lObject1['string'] := 'foo';
  lObject1['number'] := 42;
  lObject1['boolean'] := true;
  lObject1['object'] := TDynamicValues.NewMap;
  lObject1['object']['string2'] := 'bar';

  lObject1['array'] := TDynamicValues.NewList;
  with IDynamicList(lObject1['array']) do
  begin
    Item[Length] := 'hi';
    Item[Length] := 'there!';
  end;

  lJSON := ToJSON(lObject1);

  lObject2 := FromJSON(lJSON);
  Assert(lObject2.EqualsDeeply(lObject1),'Object created from the JSON serialized from another object should be deep equals with that other object');
end;

end.

