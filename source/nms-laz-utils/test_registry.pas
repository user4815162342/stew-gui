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
  // I've got GMethodList in sys_types, but a test shouldn't be using the
  // modules it's testing...

  { TTestHolder }

  TTestHolder = record
    Value: TTest;
    class operator=(a: TTestHolder; b: TTestHolder): Boolean;
  end;

  TTestList = specialize TFPGList<TTestHolder>;

  TTestRegistry = class;

  { TTestSpec }

  {$M+}
  TTestSpec = class
  private
    fRegistry: TTestRegistry;
  protected
    // This procedure can be used to indicate the beginning of a slow
    // test, especially user-interactive tests. It turns off the timeout
    // and indicates an event so that a message can be logged.
    procedure BeginInteractive;
    function BeginAsync: Integer;
    procedure EndAsync(aTestID: Integer);
    procedure Defer(aFunc: TDataEvent; aData: PtrInt);
    procedure FailAsync(aTestID: Integer; const aMessage: String);
    // I know there's an assert function in pascal, but that is only
    // compiled when a certain switch is on.
    function AssertAsync(aCondition: Boolean; aError: String; aTestID: Integer): Boolean;
    procedure Assert(aCondition: Boolean; aError: String);
    procedure SetupTest; virtual;
    procedure CleanupTest; virtual;
    procedure Alert(aMessage: String);
    procedure Report(aMessage: String);
    function CopyTemporaryFileData(aSource: String): String;
    function CreateTemporaryDirectory: String;
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
    FOnInteractiveStarted: TTestEvent;
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
    fTemporaryFiles: TStringList;
    function GetTimeout: Cardinal;
    procedure SetOnAsyncStarted(AValue: TTestEvent);
    procedure SetOnCancelled(AValue: TNotifyEvent);
    procedure SetOnCompleted(AValue: TNotifyEvent);
    procedure SetOnException(AValue: TExceptionEvent);
    procedure SetOnFailed(AValue: TTestMessageEvent);
    procedure SetOnAlert(AValue: TTestMessageEvent);
    procedure SetOnInteractiveStarted(AValue: TTestEvent);
    procedure SetOnReport(AValue: TTestMessageEvent);
    procedure SetOnSucceeded(AValue: TTestEvent);
    procedure SetTimeout(AValue: Cardinal);
  protected
    procedure BeginInteractive;
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
    procedure DeleteTemporaryFileData;
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
    property OnInteractiveStarted: TTestEvent read FOnInteractiveStarted write SetOnInteractiveStarted;
    property OnCompleted: TNotifyEvent read fOnCompleted write SetOnCompleted;
    property OnCancelled: TNotifyEvent read FOnCancelled write SetOnCancelled;
    property OnException: TExceptionEvent read FOnException write SetOnException;
    property Timeout: Cardinal read GetTimeout write SetTimeout;
    function CopyTemporaryFileData(aSource: String): String;
    function CreateTemporaryDirectory: String;
  end;

  function CalculateTestSpecName(aClass: TClass): String;
  function CalculateTestSpecName(aClass: TClass; out aNormalName: Boolean): String;

  function CalculateTestName(aObject: TObject; aMethodName: String): String;

implementation

uses
  FileUtil;

function CalculateTestSpecName(aClass: TClass): String;
var
  l: Boolean;
begin
  result := CalculateTestSpecName(aClass,l);

end;

function CalculateTestSpecName(aClass: TClass; out aNormalName: Boolean
  ): String;
begin
  result := aClass.ClassName;
  if Pos('Spec',Result) = (Length(Result) - 3) then
  begin
    if Result[1] = 'T' then
    begin
      Result := Copy(Result,2,Length(Result) - 5);
      aNormalName := true;

    end;
  end
  else
  begin
    aNormalName := false;
  end;
end;

function CalculateTestName(aObject: TObject; aMethodName: String): String;
var
  lSpecName: String;
  lTestName: String;
  lNormalName: Boolean;
  lFirstSpacePos: Integer;
  lNumber: Integer;
begin
  lSpecName := CalculateTestSpecName(aObject.ClassType,lNormalName);
  lTestName := aMethodName;
  if lNormalName then
  begin
    if Pos('Test',aMethodName) = 1 then
    begin
      lTestName := Copy(aMethodName,5,Length(aMethodName));
      lTestName := Trim(StringReplace(lTestName,'_',' ',[rfReplaceAll]));
      lFirstSpacePos:=Pos(' ',lTestName);
      if TryStrToInt(Copy(lTestName,1,lFirstSpacePos - 1),lNumber) then
      begin
        // the number is meant only for sorting, and should be removed.
        lTestName := Copy(lTestName,lFirstSpacePos + 1,MaxInt);
      end;

    end;
    result := lSpecName + ': ' + lTestName
  end
  else
  begin
     result := lSpecName + '.' + lTestName;
  end;
end;

{ TTestHolder }

class operator TTestHolder.=(a: TTestHolder; b: TTestHolder): Boolean;
begin
  result := (TMethod(a.Value).Code = TMethod(b.Value).Code) and
            (TMethod(a.Value).Data = TMethod(b.Value).Data);
end;

{ TTestSpec }

procedure TTestSpec.BeginInteractive;
begin
  fRegistry.BeginInteractive;

end;

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

function TTestSpec.AssertAsync(aCondition: Boolean; aError: String;
  aTestID: Integer): Boolean;
begin
  Result := aCondition;
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

function TTestSpec.CopyTemporaryFileData(aSource: String): String;
begin
  result := fRegistry.CopyTemporaryFileData(aSource);
end;

function TTestSpec.CreateTemporaryDirectory: String;
begin
  result := fRegistry.CreateTemporaryDirectory;
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
  if not fCancelled and (fCurrent < fTests.Count) then
  begin
    lTest := fTests[fCurrent].Value;
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
  DeleteTemporaryFileData;
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

procedure TTestRegistry.DeleteTemporaryFileData;
var
  i: Integer;
begin
  for i := 0 to fTemporaryFiles.Count - 1 do
    DeleteDirectory(fTemporaryFiles[i],false);
  fTemporaryFiles.Clear;
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

procedure TTestRegistry.SetOnInteractiveStarted(AValue: TTestEvent);
begin
  if FOnInteractiveStarted=AValue then Exit;
  FOnInteractiveStarted:=AValue;
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

procedure TTestRegistry.BeginInteractive;
begin
  StopTimer;
  if FOnInteractiveStarted <> nil then
     FOnInteractiveStarted(Self,fTestNames[fCurrent]);
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
  // TestSucceeded should check against current and isasync etc.
  TestSucceeded(aTestID);
end;

procedure TTestRegistry.QueueNextTest;
begin
  fCurrent := fCurrent + 1;
  Application.QueueAsyncCall(@RunNextTest,0)
end;

constructor TTestRegistry.Create;
begin
  inherited Create;
  fTemporaryFiles := TStringList.Create;
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
  FreeAndNil(fTemporaryFiles);
  inherited Destroy;
end;

procedure TTestRegistry.AddTest(const aName: String; aTest: TTest);
var
  lHolder: TTestHolder;
begin
  fTestNames.Add(aName);
  lHolder.Value:=aTest;
  fTests.Add(lHolder);

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
        {$R-}
        for i := 0 to MethodTable^.count - 1 do
          list.Add(MethodTable^.entries[i].name^);
        {$R+}
      end;
      vmt := pClass(pointer(vmt) + vmtParent)^;
    end;
    // sort the tests so we can handle making sure the tests
    // are in order by adding numbers to them.
    list.Sort;
    for j := 0 to list.Count - 1 do
    begin
      if Pos('test',LowerCase(list[j])) = 1 then
      begin
        lMethod.Data := aObject;
        lMethod.Code := aObject.MethodAddress(list[j]);
        // I can find no way of verifying it's the
        // right type of method. This doesn't raise an error, as I would expect it
        // to, if the method doesn't match the signature. This can cause
        // problems. If there were a way to get the signature, I could also
        // use that to determine whether it's async or not.
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

function TTestRegistry.CopyTemporaryFileData(aSource: String): String;
begin
  result := GetTempFilename('','');
  fTemporaryFiles.Add(result);
  CopyDirTree(IncludeTrailingPathDelimiter(aSource),IncludeTrailingPathDelimiter(result));

end;

function TTestRegistry.CreateTemporaryDirectory: String;
begin
  result := GetTempFilename('','');
  fTemporaryFiles.Add(Result);
  CreateDir(result);
end;

end.

