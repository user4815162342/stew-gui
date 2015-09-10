unit test_registry;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, contnrs, ExtCtrls, forms;
// extctrls needed for ttimer.
// forms needed for Application.QueueAsyncCall and TExceptionEvent

type
  // Subclassed so that IDE can be configured to ignore expected failures.
  ETestFailure = class(Exception);

  TTestMessageEvent = procedure(const aSender: TObject; const aTestName: String; const aMessage: String) of object;
  TTestEvent = procedure(const aSender: TObject; const aTestName: String) of object;

  TTest = procedure of object;

  TTestList = specialize TFPGList<TTest>;

  TTestRegistry = class;

  { TTestSpec }

  {$M+}
  TTestSpec = class
  private
    fRegistry: TTestRegistry;
  protected
    function BeginAsync: Integer;
    procedure EndAsync(aTestID: Integer);
    procedure Defer(aFunc: TDataEvent; aData: PtrInt);
    procedure FailAsync(aTestID: Integer; const aMessage: String);
    // I know there's an assert function in pascal, but that is only
    // compiled when a certain switch is on.
    procedure AssertAsync(aCondition: Boolean; aError: String; aTestID: Integer);
    procedure Assert(aCondition: Boolean; aError: String);
    procedure SetupTest; virtual;
    procedure CleanupTest; virtual;
    procedure Alert(aMessage: String);
    procedure Report(aMessage: String);
  public
    constructor Create(aRegistry: TTestRegistry);
  end;
  {$M-}

  TTestSpecClass = class of TTestSpec;

  { TTestRegistry }

  TTestRegistry = class
  private
    fIsAsync: Boolean;
    FOnAsyncStarted: TTestEvent;
    FOnCancelled: TNotifyEvent;
    fOnCompleted: TNotifyEvent;
    FOnException: TExceptionEvent;
    FOnFailed: TTestMessageEvent;
    fOnAlert: TTestMessageEvent;
    fOnMessage: TTestMessageEvent;
    FOnSucceeded: TTestEvent;
    fOwnedTestSpecs: TObjectList;
    fTestNames: TStringList;
    fTests: TTestList;
    fCancelled: Boolean;
    fRunning: Boolean;
    fCurrent: Integer;
    fTimer: TTimer;
    fDisableTimeOuts: Boolean;
    function GetTimeout: Cardinal;
    procedure SetOnAsyncStarted(AValue: TTestEvent);
    procedure SetOnCancelled(AValue: TNotifyEvent);
    procedure SetOnCompleted(AValue: TNotifyEvent);
    procedure SetOnException(AValue: TExceptionEvent);
    procedure SetOnFailed(AValue: TTestMessageEvent);
    procedure SetOnAlert(AValue: TTestMessageEvent);
    procedure SetOnReport(AValue: TTestMessageEvent);
    procedure SetOnSucceeded(AValue: TTestEvent);
    procedure SetTimeout(AValue: Cardinal);
  protected
    function BeginAsync: Integer;
    procedure EndAsync(aTestID: Integer);
    procedure QueueNextTest;
    procedure RunNextTest({%H-}Data: PtrInt);
    procedure StartTimer;
    procedure StopTimer;
    procedure TestSucceeded(aTestID: Integer);
    procedure TestFailed(aTestID: Integer; const aMessage: String);
    procedure TestsCompleted;
    procedure TestException(Sender: TObject; E: Exception);
    procedure TestTimedout(Sender: TObject);
    procedure Alert(aMessage: String);
    procedure Report(aMessage: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTest(const aName: String; aTest: TTest);
    procedure AddTests(aClass: TTestSpecClass);
    procedure AddTests(aObject: TTestSpec);
    procedure ClearTests;
    procedure Run(aDisableTimeOuts: Boolean);
    procedure Cancel;
    property OnSucceeded: TTestEvent read FOnSucceeded write SetOnSucceeded;
    property OnFailed: TTestMessageEvent read FOnFailed write SetOnFailed;
    property OnAlert: TTestMessageEvent read fOnAlert write SetOnAlert;
    property OnMessage: TTestMessageEvent read fOnMessage write SetOnReport;
    property OnAsyncStarted: TTestEvent read FOnAsyncStarted write SetOnAsyncStarted;
    property OnCompleted: TNotifyEvent read fOnCompleted write SetOnCompleted;
    property OnCancelled: TNotifyEvent read FOnCancelled write SetOnCancelled;
    property OnException: TExceptionEvent read FOnException write SetOnException;
    property Timeout: Cardinal read GetTimeout write SetTimeout;
  end;

  function CalculateTestName(aObject: TObject; aMethodName: String): String;

implementation

function CalculateTestName(aObject: TObject; aMethodName: String): String;
var
  lSpecName: String;
  lTestName: String;
  lNormalName: Boolean;
begin
  lSpecName := aObject.ClassType.ClassName;
  lTestName := aMethodName;
  if Pos('Spec',lSpecName) = (Length(lSpecName) - 3) then
  begin
    if lSpecName[1] = 'T' then
    begin
      lSpecName := Copy(lSpecName,2,Length(lSpecName) - 5);
      lNormalName := true;

      if Pos('Test',aMethodName) = 1 then
      begin
        lTestName := Copy(lTestName,5,Length(lTestName));
        lTestName := Trim(StringReplace(lTestName,'_',' ',[rfReplaceAll]));
      end;
    end;
  end;
  if lNormalName then
     result := lSpecName + ': ' + lTestName
  else
     result := lSpecName + '.' + lTestName;
end;

{ TTestSpec }

function TTestSpec.BeginAsync: Integer;
begin
 result := fRegistry.BeginAsync;
end;

procedure TTestSpec.EndAsync(aTestID: Integer);
begin
  fRegistry.EndAsync(aTestID);
end;

procedure TTestSpec.Defer(aFunc: TDataEvent; aData: PtrInt);
begin
  Application.QueueAsyncCall(aFunc,aData);
end;

procedure TTestSpec.FailAsync(aTestID: Integer; const aMessage: String);
begin
  fRegistry.TestFailed(aTestID,aMessage);
end;

procedure TTestSpec.AssertAsync(aCondition: Boolean; aError: String; aTestID: Integer
  );
begin
  if not aCondition then
     FailAsync(aTestID,aError);
end;

procedure TTestSpec.Assert(aCondition: Boolean; aError: String);
begin
  if not aCondition then
    raise ETestFailure.Create(aError);
end;

procedure TTestSpec.SetupTest;
begin
  // can be overridden to set the environent up for the test.
end;

procedure TTestSpec.CleanupTest;
begin
  // can be overridden to clean up the environment after a test.
end;

procedure TTestSpec.Alert(aMessage: String);
begin
  fRegistry.Alert(aMessage);
end;

procedure TTestSpec.Report(aMessage: String);
begin
  fRegistry.Report(aMessage);

end;

constructor TTestSpec.Create(aRegistry: TTestRegistry);
begin
  inherited Create;
  fRegistry := aRegistry;
end;

{ TTestRegistry }

procedure TTestRegistry.RunNextTest(Data: PtrInt);
var
  lTest: TTest;
begin
  fCurrent := fCurrent + 1;
  if not fCancelled and (fCurrent < fTests.Count) then
  begin
    lTest := fTests[fCurrent];
    try
      StartTimer;
      lTest;
      if not fIsAsync then
        TestSucceeded(fCurrent);
    except
      on E: Exception do
        TestFailed(fCurrent,E.Message);
    end
  end
  else
    TestsCompleted;
end;

procedure TTestRegistry.StartTimer;
begin
  if not fDisableTimeOuts then
     fTimer.Enabled := true;
end;

procedure TTestRegistry.StopTimer;
begin
  fTimer.Enabled := false;
end;

procedure TTestRegistry.TestSucceeded(aTestID: Integer);
begin
  // not interested in successful tests which were timed out...
  if fRunning and (aTestID = fCurrent) then
  begin
    fIsAsync := false;
    StopTimer;
    if FOnSucceeded <> nil then
      FOnSucceeded(Self,fTestNames[aTestID]);
    QueueNextTest;
  end;
end;

procedure TTestRegistry.TestFailed(aTestID: Integer; const aMessage: String);
begin
  // not interested in failed tests which were timed out...
  if fRunning and (aTestID = fCurrent) then
  begin
    fIsAsync := false;
    StopTimer;
    if FOnFailed <> nil then
      FOnFailed(Self,fTestNames[aTestID],aMessage);
    QueueNextTest;

  end;
end;

procedure TTestRegistry.TestsCompleted;
begin
  fRunning := false;
  if fCancelled and (fOnCancelled <> nil) then
    fOnCancelled(Self);
  if fOnCompleted <> nil then
    fOnCompleted(Self);
end;

procedure TTestRegistry.SetOnCompleted(AValue: TNotifyEvent);
begin
  if fOnCompleted=AValue then Exit;
  fOnCompleted:=AValue;
end;

procedure TTestRegistry.SetOnException(AValue: TExceptionEvent);
begin
  if FOnException=AValue then Exit;
  FOnException:=AValue;
end;

procedure TTestRegistry.TestException(Sender: TObject; E: Exception);
begin
  // There's really no way for us to know for sure what test the error
  // occurred in. So, just raise an event. If an async test stopped due to
  // this exception, then a timeout will occur.
  if FOnException <> nil then
    FOnException(Self,E)
  else
    Application.ShowException(E);
end;

procedure TTestRegistry.TestTimedout(Sender: TObject);
begin
  // This is almost certainly the current test that failed.
  TestFailed(fCurrent,'Timed out after waiting ' + FloatToStr(Timeout/1000) + ' seconds for a response');
end;

procedure TTestRegistry.Alert(aMessage: String);
begin
  if fOnAlert<>nil then
    fOnAlert(Self,fTestNames[fCurrent],aMessage);
end;

procedure TTestRegistry.Report(aMessage: String);
begin
  if fOnMessage<>nil then
    fOnMessage(Self,fTestNames[fCurrent],aMessage);
end;

function TTestRegistry.GetTimeout: Cardinal;
begin
  result := fTimer.Interval;
end;

procedure TTestRegistry.SetOnAsyncStarted(AValue: TTestEvent);
begin
  if FOnAsyncStarted=AValue then Exit;
  FOnAsyncStarted:=AValue;
end;

procedure TTestRegistry.SetOnCancelled(AValue: TNotifyEvent);
begin
  if FOnCancelled=AValue then Exit;
  FOnCancelled:=AValue;
end;

procedure TTestRegistry.SetOnFailed(AValue: TTestMessageEvent);
begin
  if FOnFailed=AValue then Exit;
  FOnFailed:=AValue;
end;

procedure TTestRegistry.SetOnAlert(AValue: TTestMessageEvent);
begin
  if fOnAlert=AValue then Exit;
  fOnAlert:=AValue;
end;

procedure TTestRegistry.SetOnReport(AValue: TTestMessageEvent);
begin
  if fOnMessage=AValue then Exit;
  fOnMessage:=AValue;
end;

procedure TTestRegistry.SetOnSucceeded(AValue: TTestEvent);
begin
  if FOnSucceeded=AValue then Exit;
  FOnSucceeded:=AValue;
end;

procedure TTestRegistry.SetTimeout(AValue: Cardinal);
begin
  fTimer.Interval := AValue;
end;

function TTestRegistry.BeginAsync: Integer;
begin
  fIsAsync := true;
  result := fCurrent;
  if FOnAsyncStarted <> nil then
    FOnAsyncStarted(Self,fTestNames[result]);

end;

procedure TTestRegistry.EndAsync(aTestID: Integer);
begin
  fIsAsync := false;
  TestSucceeded(aTestID);
end;

procedure TTestRegistry.QueueNextTest;
begin
  Application.QueueAsyncCall(@RunNextTest,0)
end;

constructor TTestRegistry.Create;
begin
  inherited Create;
  fOwnedTestSpecs := TObjectList.create(true);
  fTests := TTestList.Create;
  fTestNames := TStringList.Create;
  fTimer := TTimer.Create(nil);
  fTimer.Enabled := false;
  fTimer.Interval := 1000;
  fTimer.OnTimer:=@TestTimedout;
  Application.AddOnExceptionHandler(@TestException);
  fCurrent := -1;
  fCancelled := false;
  fIsAsync := false;
end;

destructor TTestRegistry.Destroy;
begin
  StopTimer;
  Application.RemoveOnExceptionHandler(@TestException);
  FreeAndNil(fTimer);
  FreeAndNil(fTestNames);
  FreeAndNil(fTests);
  FreeAndNil(fOwnedTestSpecs);
  inherited Destroy;
end;

procedure TTestRegistry.AddTest(const aName: String; aTest: TTest);
begin
  fTestNames.Add(aName);
  fTests.Add(aTest);

end;

// Code copied from https://github.com/graemeg/fptest/blob/master/src/fpchelper.pas
// which is copied from objpas.inc:  class function TObject.MethodAddress()
procedure TTestRegistry.AddTests(aClass: TTestSpecClass);
var
  lObject: TTestSpec;
begin
  lObject := aClass.Create(Self);
  fOwnedTestSpecs.Add(lObject);
  AddTests(lObject);
end;

procedure TTestRegistry.AddTests(aObject: TTestSpec);
type
  TMethodNameRec = packed record
    name: pshortstring;
    addr: pointer;
  end;

  TMethodNameTable = packed record
    count: dword;
    entries: packed array[0..0] of TMethodNameRec;
  end;

  PMethodNameTable =  ^TMethodNameTable;

var
  methodTable: PMethodNameTable;
  i: dword;
  j: Integer;
  vmt: TClass;
  list: TStringList;
  lMethod: TMethod;
begin
  AddTest(CalculateTestName(aObject,'Set Up'),@aObject.SetupTest);
  list := TStringList.Create;
  try
    vmt := aObject.ClassType;
    while Assigned(vmt) do
    begin
      methodTable := pMethodNameTable((Pointer(vmt) + vmtMethodTable)^);
      if Assigned(MethodTable) then
      begin
        for i := 0 to MethodTable^.count - 1 do
          list.Add(MethodTable^.entries[i].name^);
      end;
      vmt := pClass(pointer(vmt) + vmtParent)^;
    end;
    for j := 0 to list.Count - 1 do
    begin
      if Pos('test',LowerCase(list[j])) = 1 then
      begin
        lMethod.Data := aObject;
        lMethod.Code := aObject.MethodAddress(list[j]);
        // TODO: Note that I can find no way of verifying it's the
        // right method. This doesn't raise an error, as I would expect it
        // to, if the method is invalid. This can cause problems. Perhaps
        // there's some other way to report it as async.
        AddTest(CalculateTestName(aObject,list[j]),TTest(lMethod));
      end;
    end;
  finally
    list.Free;
  end;
  AddTest(CalculateTestName(aObject,'Clean Up'),@aObject.CleanupTest);

end;

procedure TTestRegistry.ClearTests;
begin
  if fRunning then
    raise Exception.Create('Can''t clear tests while running');
  fTestNames.Clear;
  fTests.Clear;
end;

procedure TTestRegistry.Run(aDisableTimeOuts: Boolean);
begin
  fCancelled := false;
  fCurrent := -1;
  fRunning := true;
  fDisableTimeOuts := aDisableTimeOuts;
  QueueNextTest;
end;

procedure TTestRegistry.Cancel;
begin
  fCancelled := true;
end;

end.

