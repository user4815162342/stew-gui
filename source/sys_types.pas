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

  // I can't use TFPGList for methods, because when methods are compared with
  // equals or not equals, only the code part is checked, not the data. So,
  // until they allow me operator overloading for those, I need my own list.
  // Doing this was easier than creating my own version of TFPGList just to
  // override indexof (which isn't virtual). Plus, as the list isn't going to
  // be changing a lot, the array will work fine.

  { GMethodList }

  generic GMethodList<ItemType> = record
  public type
     ArrayType = array of ItemType;
  private
     fItems: ArrayType;
     function GetItem(Index: Integer): ItemType; inline;
     function GetCount: Integer; inline;
  public
     procedure Init; inline;
     procedure Add(Item: ItemType); inline;
     procedure Remove(Item: ItemType); inline;
     procedure Delete(Index: Integer); inline;
     procedure Clear;
     function IndexOf(Item: ItemType): Integer; inline;
     property Count: Integer read GetCount;
     property Items[Index: Integer]: ItemType read GetItem; default;
  end;

type

  TStringArray2 = specialize GArray<UTF8String>;

const
  ISO8601 = 'yyyymmdd"T"hhnnss';
  ISO8601Extended = 'yyyy-mm-dd"T"hh:nn:ss';

  function DateTimeToISO8601(const ADateTime: TDateTime; aExtended: Boolean): string;
  function ISO8601ToDateTime(const AValue: string): TDateTime;
  function DateTimeToMsecs(const aDateTime: TDateTime): Comp;
  function MsecsToDateTime(const aValue: Comp): TDateTime;
  function DateTimeToRelativeEnglish(aDate: TDateTime): String;
  function MonthDifference(aDate: TDateTime; bDate: TDateTime): Integer;
  function WorkingDaysDifference(aDate: TDateTime; bDate: TDateTime): Integer;


implementation

{ GMethodList }

function GMethodList.GetItem(Index: Integer): ItemType;
begin
  result := fItems[Index];
end;

function GMethodList.GetCount: Integer;
begin
  result := Length(fItems);
end;

procedure GMethodList.Init;
begin
  Clear;
end;

procedure GMethodList.Add(Item: ItemType);
var
  l: Integer;
begin
  l := Length(fItems);
  SetLength(fItems,l + 1);
  fItems[l] := Item;
end;

procedure GMethodList.Remove(Item: ItemType);
var
  i: Integer;
begin
  i := IndexOf(Item);
  if (i > -1) then
  begin
    Delete(i);
  end;

end;

procedure GMethodList.Delete(Index: Integer);
var
  {%H-}lOld: ItemType;
  i: Integer;
  l: Integer;
begin
  // force a range check error if that is turned on.
  lOld := fItems[Index];
  l := Length(fItems);
  for i := Index to l - 2 do
  begin
    fItems[i] := fItems[i + 1];
  end;
  SetLength(fItems,l - 1);
end;

procedure GMethodList.Clear;
begin
  SetLength(fItems,0);
end;

function GMethodList.IndexOf(Item: ItemType): Integer;
var
  l: Integer;
begin
  Result := 0;
  l := Length(fItems);
  while (Result < l) and
        not ((TMethod(fItems[Result]).Code = TMethod(Item).Code) and
             (TMethod(fItems[Result]).Data = TMethod(Item).Data)) do
  begin
     inc(Result)
  end;
  if Result = l then
     result := -1;
end;

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

function DateTimeToRelativeEnglish(aDate: TDateTime): String;
var
  lNow: TDateTime;
  lDays: Integer;
  lMonths: Integer;
begin
  // TODO: This is coming out wrong. January 7th should be three months,
  // or maybe 2 months, but it's coming out next month instead.
  // Mar 7 should also be 4 months. I think I somehow have to go
  // to the 'middle' of the month to get it. Or, I actually count
  // the months?
  lNow := trunc(Now);
  lDays := trunc(aDate - lNow);
  case lDays of
     low(Integer)..-731:
       result := IntToStr((-lDays) div 365) + ' YEARS AGO!';
     -730..-366:
       result := 'LAST YEAR!';
     -365..-48:
       begin
         lMonths := MonthDifference(aDate,lNow);
         if lMonths = -1 then
            result := 'LAST MONTH!'
         else
            result := IntToStr(-lMonths) + ' MONTHS AGO!';
       end;
     -47..-14:
       result := IntToStr((-lDays) div 7) + ' WEEKS AGO!';
     -13..-7:
       result := 'LAST WEEK!';
     -6..-2:
       result := IntToStr(-lDays) + ' DAYS AGO!';
     -1:
       result := 'YESTERDAY!';
     0:
       result := 'TODAY!';
     1:
       result := 'TOMORROW!';
     2..6:
       result := 'in ' + IntToStr(lDays) + ' days!';
     7..13:
       result := 'next week!';
     14..47:
       result := 'in ' + IntToStr(lDays div 7) + ' weeks';
     48..365:
       begin
         lMonths := MonthDifference(aDate,lNow);
         if lMonths = 1 then
            result := 'next month'
         else
            result := 'in ' + IntToStr(lMonths) + ' months';
       end;
     366..730:
       result := 'next year';
     731..high(Integer):
       result := 'in ' + IntToStr(lDays div 365) + ' years';
  end;
end;

function MonthDifference(aDate: TDateTime; bDate: TDateTime): Integer;
var
  aY: Word;
  aM: Word;
  aD: Word;
  bY: Word;
  bM: Word;
  bD: Word;
begin
  DecodeDate(aDate,aY,aM,aD);
  DecodeDate(bDate,bY,bM,BD);
  result := (aY - bY) * 12;
  result := result + (aM - bM);

end;

function WorkingDaysDifference(aDate: TDateTime; bDate: TDateTime): Integer;
var
  bDOW: Word;
  lDays: Integer;
  lWeeks: Integer;
  lRem: Integer;
begin
  lDays := trunc(aDate) - trunc(bDate);
  lWeeks := lDays div 7;
  result := lDays - (lWeeks * 2);

  lRem := lDays mod 7;
  if lRem > 0 then
  begin
    bDOW:=DayOfWeek(bDate);
    // 1..7 => Sunday..Saturday
    if bDOW = 7 then
    begin
      // the last day falls on Saturday. Since the remainder can't
      // be greater than 6, the earliest the full weeks can end on is
      // Monday, so we only need to count this Saturday.
      result := result - 1;
    end
    else
    begin
      // the last day falls on Sunday through Friday.
      // if the remainder is less than the day of the week, then the
      // full weeks ended on Sunday or later, so we don't have a weekend
      // in the remaining part.
      if lRem = bDOW then
      begin
      // if the remainder equals the DOW, then the remainder includes only
      // Sunday.
         result := result - 1
      end
      else if lRem > bDOW then
      begin
      // otherwise, the remainder also includes Saturday.
        result := result - 2;
      // Note that if the last day is Friday, the earliest the full weeks
      // can end is on Saturday, so we'll never hit this.

      end;

    end;
  end;

end;


end.

