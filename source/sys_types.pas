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
  // large jumps instead of one at a time, which may or may not work. However,
  // the caller can also set the 'Count' button to be sure.
  // - it's not very efficient for arrays that are going to be inserted into.
  // For those, you really need a list.
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

const
  ISO8601 = 'yyyymmdd"T"hhnnss';
  ISO8601Extended = 'yyyy-mm-dd"T"hh:nn:ss';

  function DateTimeToISO8601(const ADateTime: TDateTime; aExtended: Boolean): string;
  function ISO8601ToDateTime(const AValue: string): TDateTime;
  function DateTimeToMsecs(const aDateTime: TDateTime): Comp;
  function MsecsToDateTime(const aValue: Comp): TDateTime;



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

function DateTimeToISO8601(const ADateTime: TDateTime; aExtended: Boolean
  ): string;
begin
  if aExtended then
     Result := FormatDateTime(ISO8601Extended, ADateTime)
  else
     Result := FormatDateTime(ISO8601, ADateTime);
end;


function ISO8601ToDateTime(const AValue: string): TDateTime;
var
  ly: Word;
  lm: Word;
  ld: Word;
  lh: Word;
  ln: Word;
  ls: Word;
  lExtended: Boolean;
begin
//                     12345678901234567890
//  ISO8601 =         'yyyymmddThhnnss';
//  ISO8601Extended = 'yyyy-mm-ddThh:nn:ss';

  lExtended := AValue[5] = '-';

  lY := StrToInt(Copy(AValue,1,4));
  if (lExtended) then
  begin
    lm := StrToInt(Copy(AValue,6,2));
    ld := StrToInt(Copy(AValue,9,2));
    lh := StrToInt(Copy(AValue,12,2));
    ln := StrToInt(Copy(AValue,15,2));
    ls := StrToInt(Copy(AValue,18,2));
  end
  else
  begin
    lm := StrToInt(Copy(AValue,5,2));
    ld := StrToInt(Copy(AValue,7,2));
    lh := StrToInt(Copy(AValue,10,2));
    ln := StrToInt(Copy(AValue,12,2));
    ls := StrToInt(Copy(AValue,14,2));
  end;

  result := EncodeDate(ly,lm,ld) + EncodeTime(lh,ln,ls,0);
end;

function DateTimeToMsecs(const aDateTime: TDateTime): Comp;
begin
  result := TimeStampToMSecs(DateTimeToTimeStamp(aDateTime));
end;

function MsecsToDateTime(const aValue: Comp): TDateTime;
begin
  result := TimeStampToDateTime(MSecsToTimeStamp(aValue));
end;



end.

