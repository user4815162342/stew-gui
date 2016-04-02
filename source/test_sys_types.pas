unit test_sys_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_types;

type

  { TTypesSpec }

  TTypesSpec = class(TTestSpec)
  published
    procedure Test_MonthDifference;
    procedure Test_RelativeEnglishDates;
  end;

implementation

{ TTypesSpec }

procedure TTypesSpec.Test_MonthDifference;
begin
  Assert(MonthDifference(EncodeDate(2015,1,1),EncodeDate(2015,1,15)) = 0,'MonthDifference [0]');
  Assert(MonthDifference(EncodeDate(2015,2,1),EncodeDate(2015,1,15)) = 1,'MonthDifference [1]');
  Assert(MonthDifference(EncodeDate(2015,4,1),EncodeDate(2015,1,15)) = 3,'MonthDifference [2]');
  Assert(MonthDifference(EncodeDate(2016,1,1),EncodeDate(2015,1,15)) = 12,'MonthDifference [3]');
  Assert(MonthDifference(EncodeDate(2016,2,1),EncodeDate(2015,1,15)) = 13,'MonthDifference [4]');
  Assert(MonthDifference(EncodeDate(2015,1,1),EncodeDate(2015,3,15)) = -2,'MonthDifference [5]');
  Assert(MonthDifference(EncodeDate(2015,1,1),EncodeDate(2016,1,15)) = -12,'MonthDifference [6]');
  Assert(MonthDifference(EncodeDate(2015,3,1),EncodeDate(2016,1,15)) = -10,'MonthDifference [7]');
  Assert(MonthDifference(EncodeDate(2015,12,1),EncodeDate(2016,1,15)) = -1,'MonthDifference [8]');
end;

procedure TTypesSpec.Test_RelativeEnglishDates;
var
  lNow: TDateTime;
begin
  lNow := Trunc(Now);
  Assert(DateTimeToRelativeEnglish(lNow + 800) = 'in 2 years','RelativeEnglishDates [0]');
  Assert(DateTimeToRelativeEnglish(lNow + 367) = 'next year','RelativeEnglishDates [1]');
  Assert(DateTimeToRelativeEnglish(lNow + 365) = 'in 12 months','RelativeEnglishDates [2]');
  Assert(DateTimeToRelativeEnglish(lNow + 95) = 'in 3 months','RelativeEnglishDates [3]');
  Assert(DateTimeToRelativeEnglish(lNow + 40) = 'in 5 weeks','RelativeEnglishDates [4]');
  Assert(DateTimeToRelativeEnglish(lNow + 24) = 'in 3 weeks','RelativeEnglishDates [5]');
  Assert(DateTimeToRelativeEnglish(lNow + 8) = 'next week!','RelativeEnglishDates [6]');
  Assert(DateTimeToRelativeEnglish(lNow + 6) = 'in 6 days!','RelativeEnglishDates [7]');
  Assert(DateTimeToRelativeEnglish(lNow + 2) = 'in 2 days!','RelativeEnglishDates [8]');
  Assert(DateTimeToRelativeEnglish(lNow + 1) = 'TOMORROW!','RelativeEnglishDates [9]');
  Assert(DateTimeToRelativeEnglish(lNow - 800) = '2 YEARS AGO!','RelativeEnglishDates [10]');
  Assert(DateTimeToRelativeEnglish(lNow - 367) = 'LAST YEAR!','RelativeEnglishDates [11]');
  Assert(DateTimeToRelativeEnglish(lNow - 365) = '12 MONTHS AGO!','RelativeEnglishDates [12]');
  Assert(DateTimeToRelativeEnglish(lNow - 100) = '4 MONTHS AGO!','RelativeEnglishDates [13]');
  Assert(DateTimeToRelativeEnglish(lNow - 40) = '5 WEEKS AGO!','RelativeEnglishDates [14]');
  Assert(DateTimeToRelativeEnglish(lNow - 24) = '3 WEEKS AGO!','RelativeEnglishDates [15]');
  Assert(DateTimeToRelativeEnglish(lNow - 8) = 'LAST WEEK!','RelativeEnglishDates [16]');
  Assert(DateTimeToRelativeEnglish(lNow - 6) = '6 DAYS AGO!','RelativeEnglishDates [17]');
  Assert(DateTimeToRelativeEnglish(lNow - 2) = '2 DAYS AGO!','RelativeEnglishDates [18]');
  Assert(DateTimeToRelativeEnglish(lNow - 1) = 'YESTERDAY!','RelativeEnglishDates [19]');
  Assert(DateTimeToRelativeEnglish(lNow) = 'TODAY!','RelativeEnglishDates [20]');

end;

end.

