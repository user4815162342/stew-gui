unit sys_types;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils;


type


  TStringArray = array of string;

  { TEZSortStringList }

  TEZSortStringListCompare = function(List: TStringList; Index1, Index2: Integer): Integer of object;

  TEZSortStringList = class(TStringList)
  private
    FOnEZSort: TEZSortStringListCompare;
    procedure SetOnEZSort(AValue: TEZSortStringListCompare);
  public
    procedure EZSort;
    property OnEZSort: TEZSortStringListCompare read FOnEZSort write SetOnEZSort;
  end;

  { GSimpleArray }

  // TODO: Just a proof of concept. It simplifies a lot of the common
  // things I do with dynamic arrays here, and it shouldn't take up anymore
  // space. I can also add some other utilities onto this, like pop and
  // push to turn it into a stack (and alternatively shift/unshift)
  // I could however, start to make use of this instead of the other dynamic
  // arrays.
  // - a separate 'length' private field would allow me to increase capacity in
  // large jumps instead of one at a time, which may or may not work.
  generic GArray<ItemType> = record
  public type
    ArrayType = array of ItemType;
  private
    fItems: ArrayType;
    function GetItem(Index: Integer): ItemType; inline;
    function GetCount: Integer; inline;
    procedure SetItem(Index: Integer; AValue: ItemType); inline;
    procedure SetCount(AValue: Integer); inline;
  public
    procedure Add(Item: ItemType); inline;
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: ItemType read GetItem write SetItem; default;
  end;

  TStringArray2 = specialize GArray<UTF8String>;


implementation

{ GSimpleList }

function GArray.GetItem(Index: Integer): ItemType;
begin
  if (Index >= 0) and (Index < Count) then
    result := fItems[Index]
  else
    raise Exception.Create('Invalid index');
end;

function GArray.GetCount: Integer;
begin
  result := Length(fItems);
end;

procedure GArray.SetItem(Index: Integer; AValue: ItemType);
begin
  if (Index >= 0) then
  begin
    if Index >= Count then
      Count := Index + 1;
    fItems[Index] := aValue;
  end
  else
    raise Exception.Create('Invalid index');
end;

procedure GArray.SetCount(AValue: Integer);
begin
  SetLength(fItems,AValue);
end;

procedure GArray.Add(Item: ItemType);
begin
  Items[Count] := Item;

end;

{ TEZSortStringList }

procedure TEZSortStringList.SetOnEZSort(AValue: TEZSortStringListCompare);
begin
  if FOnEZSort=AValue then Exit;
  FOnEZSort:=AValue;
end;

function DoEZSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  with List as TEZSortStringList do
  begin
    if FOnEZSort <> nil then
       result := FOnEZSort(List,Index1,Index2)
    else
      result := CompareText(List[Index1],List[Index2]);
  end;
end;

procedure TEZSortStringList.EZSort;
begin
  CustomSort(@DoEZSortCompare);
end;

end.

