unit test_longstringmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_filecache, stew_types;

type
// This is important to the workings of CachedFiles, and I did a few
// tricky things there, so I should probably test it.

  { TLongStringMapSpec }

  TLongStringMapSpec = class(TTestSpec)
  private
    fIterationCount: Longint;
    fIterationKeys: TStringArray;
    procedure CallbackTest(aKey: UTF8String; aValue: TObject);
  published
    procedure TestBehavior;
  end;


implementation

const
  cKeyA = 'foo';
  // 300 characters
  cKeyB = 'huAQsMHEBUkqluo0n567T4FKS33eHKMqLs4BKnrVnn63m7P6wA7HYUNCkEXCwucq4r5kuHYs11XFC32rK7abbRkFqrS88mBo0peqpMBbAFwBkcS6PPzr8AnZJmjvDmtNW9L7Fy82bsXD0guM3A1oUPArEF5v6nJI24sRf3lfGtY09GUF6y053i8ur15MVM2DMgkSCSoPpD11c01Z8OcYTEZqH60VpnjoDIX6ATOraLQDLmHxcgri1ViK5gxKyxro50UlrbAlFZbpYaarNNEU235R9vIT7xo5Q1mAngnMRlNK';
  cKeyC = 'mgIoGMgpBLoUyRzC2jwKj23F2wgqVCGlB5yW6IE6Ezh3tHYUTcWlmWyaAYsafwCKThZ5256PXbnLfBOKMjvOhVywBbuhCWTwimR7TFJuS5GxmgfHpDEBYaGmFuuuXIgfsO5KgQR6ljoQyLqKnVOhyVmcUpNIXlg0znHo7kAa7Aacg5nkLwSxWQ1lTrOLcaCaAuTS74w1qDGGgR2F1rZuXxxTOW3lqN6hG6wbhwWGuKauDeImWPz2siSfPnzn4Bt7RJECGT2PLGzUVhF90AAJRg5GGKqA7DGq1Fx6MZw24FAu';
  // 300 characters with same 255 character startup.
  cKeyD = '4FhB5bHew7QtmDY8AiYcwWRwcLYQuMcNhersPU53Iy51hwe1YCcARLVWTOP4OokKVO0tpoVB8Z0r9aPwKL7qpp6TIzxOOB3YXisRqoLC1nleBGF0rTZowggXUtRylCj8hxUrh40mIwz9QnUwEX3prcFgxBS4JM248T8zATpriwgbDqWu0S7es4f2LFH0cSN8UVBEbKOcFfmvqTqRxQarorQyy7yvJOpRRpQy4mh5M6W0DXAi80tGhA06rF6jVlWafCjxmsw9rbV2AViH2Pm1jtg8nks1CpvBxonOEEqo9cOm';
  cKeyE = '4FhB5bHew7QtmDY8AiYcwWRwcLYQuMcNhersPU53Iy51hwe1YCcARLVWTOP4OokKVO0tpoVB8Z0r9aPwKL7qpp6TIzxOOB3YXisRqoLC1nleBGF0rTZowggXUtRylCj8hxUrh40mIwz9QnUwEX3prcFgxBS4JM248T8zATpriwgbDqWu0S7es4f2LFH0cSN8UVBEbKOcFfmvqTqRxQarorQyy7yvJOpRRpQy4mh5M6W0DXAi80tGhA06rF6jVlWkljs4H7MkTGWxTyUEXouBy913jP00AxM8YGaNwen026Ga';
  cKeyF = '4FhB5bHew7QtmDY8AiYcwWRwcLYQuMcNhersPU53Iy51hwe1YCcARLVWTOP4OokKVO0tpoVB8Z0r9aPwKL7qpp6TIzxOOB3YXisRqoLC1nleBGF0rTZowggXUtRylCj8hxUrh40mIwz9QnUwEX3prcFgxBS4JM248T8zATpriwgbDqWu0S7es4f2LFH0cSN8UVBEbKOcFfmvqTqRxQarorQyy7yvJOpRRpQy4mh5M6W0DXAi80tGhA06rF6jVlWEnEQU8UuxnnHSWXtnU7az0Q47nsgfwjmt5gZclNjXOt4X';
  // 510 characters long (two nested short string maps plus 0 characters)
  cKeyG = 'Tf1SlibnsuiacUkP00lQy4O8zLRWTSNCPVvPDBQSWUjhZHuqwhVwrLSqFIxlxfVg1csF0wM7Rv0xprQYgUs2KsqeL8AVZ9GnoirTfBJk3mqvXMGNY6CDEhlEh50DIJHzcJ5MF1ktykZrHLxVW7Zh6PG5vVioAf9VQE951aQ8mWYLh7cCi2N3msYubXolE2whx7M7nrckDK4XkX10rOMOqFMVmhXj0WpmxQ5Zuk5DbGjAXDoaNEoZ6OzoeRTr2HIAt601eNwE6T5DIXAhOshO55MMF03bRH3MUViEoQtGEFJ4uuJ6cISBjJfcESfv28pmiwRhIjUzpKSGWi6LnwuuysCz1ntev80BWVlE6Y1VLKv44JICaSiavcpjEftSJowivYSjbUBwCV151IXzVcTbF4UqBwqukNpyvrel22OV8bEibYNwzgZeVwEML1VQvECYbu0qeYar5iXPtPV4f0kI7QVFBklYhKvWKjjY27Ig9mStT0kvW8LMSRFxeM7Kr1';
  // 520 characters long (two nested short string maps plus 10 characters)
  cKeyH = 'oJ3Hcg8ozv2geEBj1TGobKDFOyQfu9TpCUns9xwpTFsuTymgsYBIgCrJhSFZRoRHZ50kYQ0ruur79ZKq8yUKTY9e8XmD5nKcCUif70oMlJHhIviIT9HiRgHGLeJwvMwWistnKBq5gZy7TPGVwYRHoyBa6X5Euk7prqBqzvnwPGUTVkICjAnFKKY3xVRkwDT38wx49KB7U8Icyy8mGXYhb1V0OeD2iF4ar79HswzwhcwxL8W9fO8qZs4fnjlK71NVW50moxYGpOf2RVU9N28pOWj3QoI5CP7FF4Y0Ztb7K4pDLihA1wvIjsVyOX4hDEvw0Im0zU1kZnjaxLMUY9Ce0pUGMHbLoHiber8N6gweOSK6aUUkb6HspkuqusyCWrH1SiyS99hfGgPbHuoqLkTchOWnrFSOObSnL5M2IXMm0B0xnHT9SlJpmGEsBUJUUgl2SO2Urz9rAEtSiU6YvhHcbssWsJzZK1vgPhRxeu3W9jJbcsWmg3en8U9HgJTOMzBEoYCsO4TN';
  // 1000 characters long
  cKeyI = 'Bof35NXoyNia2wL7eaveZnVKNoo8V1ttX5NwRt9pfVoSQTFYlH9b1JEW0OsbTWX03zSZ8s2MEV4FEh2wYXgxz38MX5V9vHi9zHgGaqatNWXBnsefBDFlEjXk9C9jq70Vk7Z2aDAw7LkIaL2gHkSeHVfT3xDxEptvlpn5swliLOjfUxKimtxfb4U1SpOcz9i1WC3YFFmY5xqWLN161u98IhaANcba7U6tk5sTCVrrrWgR8ouXH7cQ8834ktF6vyCS3tq9SWF47xp8U3au9co67rvoUP4XPmKKgU6w6JP9KeIfK4xMAq7BRqpzSG9OiYcMDHcOhEmkCPLoSRwDmvq1Zob2y7wTkTeP0UcKjzimDzx6U3Qb7GJEWpoVBb0OiXxZZxb5XlD7bVqh4JPIcfs1lrCFJlme9kDeVEGUSnUSvIrbDR1G3uiWIDKXx3XrCugQ83Mi1EZN5pD1w6y3bP3GzSnQFYZDkJaCjUz2eU5DAKUugKiT9gOrvzs6bUL0tH6ReTOW1Y7OFzRG9ZFcYSX5QFhSgsiqBe4XJkM0RyjFnS7lKopSCWYbb2CxWB73CxP0VQw5m3QVD8HbnBFaRR27x0bPrKsz4wzJz27ITR8u0XPDbrv8qG5Ct5ljpSumLp1LQzuhAGXmEQNDj0nybI9uC6YOC1X1rBFZgm7eig70GjhU9j4SRqRyptJxR02eVO9kUbrO0kbRB9HmiQTFJz9V2Jp13mqIqpaAQrYSrIEW5b4C8KS5OCpsJpGBFuCAV1mKP2fWVQ6Z2TQ6jTzK7mFp9X1ZjhBPctzkhRrAsmVJMCb7TuLMgQawgcSbFDz8HLo5rbLX63ZfCRqfiLL5gv7a8K5EBy8USAR3RrJMC7NQzUL0WXD3BwKUmUDzyhEyMNwC7eQkeuK4rAhODBSbExy3oFvzeLF0LqAXIFuNMvIUZq37RUml6tRPj0SCmqYyCFuRiQ4V9VQixTMNxFgTVLzuSvD5Z0yQQhBy4AyPL4DQ';
  // 500 characters long
  cKeyJ = 'JOZc6WMaleqAcTVeu0kEyR6h62FPtKOMr3bpKjVfIUfao9vHnMY7hvcxLoN6qUAPU9OMCGeFmF2FBQr8aLrI8XO43fYUxVUqyZv0DBS8e8QYb5139gI07RbD5O6ngXGTePn0lclfcD9Wu1taaSRZsbbLHVe0mRhkN4sTwymcULK9SRi9Tc7y1b7tPf6eGRBJ5AAji9v8RsNX8kazfqonXALNqXUT0yyIPVkLhMTmS6bNUfS13qK1tYJXlAbmjbBiZ7kt2w10H48DvowE92xjZxZzE11u17DSstNeY5Z0Rq2RZC5OpBjRwTF4aVAaTy8JIJlI5mZ4TNwjsc9YfEp0cm6BbgOWLvzo4xUVnbTeBFMsywUZm2wQelfUv7nUM6RzSY8VfrhjPPf1iPmtE6CCSY4GFTQTP2uc7yTKfjxUf2jN5pQuqi5i0axYxvhRbkpNijXJ5Qnxv1xB5XQyQOamuDv2W1MytfxTna8TPQrUIHUADcSNDNA4';

type

  { TTestObject }

  TTestObject = class
  private
    fKey: UTF8String;
    class var fCount: Integer;
  public
    class constructor Create;
    constructor Create(aKey: UTF8String);
    destructor Destroy; override;
    property Key: UTF8String read fKey;
    class property Count: Integer read fCount;
  end;

{ TTestObject }

class constructor TTestObject.Create;
begin
  fCount := 0;;
end;

constructor TTestObject.Create(aKey: UTF8String);
begin
  fKey := aKey;
  inc(fCount)
end;

destructor TTestObject.Destroy;
begin
  Dec(fCount);
  inherited Destroy;
end;

{ TLongStringMapSpec }

procedure TLongStringMapSpec.CallbackTest(aKey: UTF8String; aValue: TObject);
var
  i: Integer;
  l: Integer;
  lNewIterationKeys: TStringArray;
begin
  Inc(fIterationCount);

  if aValue is TTestObject then
  begin
    Assert((aValue as TTestObject).Key = aKey,'Iterating should return the correct objects correcly');
  end;

  l := 0;
  SetLength(lNewIterationKeys,l);
  for i := 0 to Length(fIterationKeys) - 1 do
  begin
    if fIterationKeys[i] <> aKey then
    begin
      SetLength(lNewIterationKeys,l + 1);
      lNewIterationKeys[l] := fIterationKeys[i];
      l := l + 1;
    end;

  end;
  Assert(Length(lNewIterationKeys) = (Length(fIterationKeys) - 1),'Long String Map should only be able to iterate keys it returned from GetKeys');
  fIterationKeys := lNewIterationKeys;

end;

procedure TLongStringMapSpec.TestBehavior;
var
  lMap: TLongStringMap;
  lKeys: TStringArray;
begin
  lMap := TLongStringMap.Create(true);
  try
    lMap[cKeyA] := TTestObject.Create(cKeyA);
    lMap[cKeyB] := TTestObject.Create(cKeyB);
    lMap[cKeyC] := TTestObject.Create(cKeyC);
    lMap[cKeyD] := TTestObject.Create(cKeyD);
    lMap[cKeyE] := TTestObject.Create(cKeyE);
    lMap[cKeyF] := TTestObject.Create(cKeyF);
    lMap[cKeyG] := TTestObject.Create(cKeyG);
    lMap[cKeyH] := TTestObject.Create(cKeyH);
    lMap[cKeyI] := TTestObject.Create(cKeyI);
    lMap[cKeyJ] := nil;
    Assert((lMap[cKeyA] as TTestObject).Key = cKeyA,'Long String Map Should Map Correctly');
    Assert((lMap[cKeyB] as TTestObject).Key = cKeyB,'Long String Map Should Map Correctly');
    Assert((lMap[cKeyC] as TTestObject).Key = cKeyC,'Long String Map Should Map Correctly');
    Assert((lMap[cKeyD] as TTestObject).Key = cKeyD,'Long String Map Should Map Correctly');
    Assert((lMap[cKeyE] as TTestObject).Key = cKeyE,'Long String Map Should Map Correctly');
    Assert((lMap[cKeyF] as TTestObject).Key = cKeyF,'Long String Map Should Map Correctly');
    Assert((lMap[cKeyG] as TTestObject).Key = cKeyG,'Long String Map Should Map Correctly');
    Assert((lMap[cKeyH] as TTestObject).Key = cKeyH,'Long String Map Should Map Correctly');
    Assert((lMap[cKeyI] as TTestObject).Key = cKeyI,'Long String Map Should Map Correctly');
    Assert(lMap[cKeyJ] = nil,'Long String Map should map a nil value correctly');
    Assert(lMap.Has(cKeyJ),'Long string map should still report that it has that key, however');
    Assert(not lMap.Has('key does not exist'),'Long string map should not report that it has a key that it shouldn''t have');
    lKeys := lMap.GetKeys;
    Assert(Length(lKeys) = 10,'Long String Map should return correct number of keys');
    lMap.Delete(cKeyB);
    lKeys := lMap.GetKeys;
    Assert(Length(lKeys) = 9,'Long String Map should return correct number of keys');
    lMap.Delete(cKeyD);
    lKeys := lMap.GetKeys;
    // delete one which is only one of one in a submap.
    Assert(Length(lKeys) = 8,'Long String Map should return correct number of keys');
    fIterationCount := 0;
    fIterationKeys := lMap.GetKeys;
    lMap.ForEachCall(@CallbackTest);
    Assert(fIterationCount = 8,'Long String Map should have iterated once for every key');
    Assert(Length(fIterationKeys) = 0,'Long String Map should have iterated once for every key');
    lMap.Clear;
    Assert(TTestObject.Count = 0,'Clearing the map should have deleted all of the objects');
    // test deleting something that isn't there...
    lMap.Delete('Not Here');
  finally
    FreeAndNil(lMap);
  end;
end;

end.

