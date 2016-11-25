unit test_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, ExtCtrls;

type

  { TTesterSpec }

  TTesterSpec = class(TTestSpec)
  published
    procedure AsyncTestDone(aTestID: Integer; const {%H-}aMessage: String);
    procedure Test_Sync_Test;
    procedure Test_Async_Test;
    procedure Test_Sync_Test_with_Error;
    procedure Test_Async_Test_with_Error;
    procedure Test_Async_Test_with_Failure;
  end;

  { TAsyncTester }
  TAsyncCallback = procedure(aTestID: Integer; const aMessage: String) of object;

  TAsyncTester = class
    procedure ErrorTimer(Sender: TObject);
    procedure Fail({%H-}Data: PtrInt);
    procedure FailTimer(Sender: TObject);
    procedure RaiseError({%H-}Data: PtrInt);
    procedure Success({%H-}Data: PtrInt);
    procedure SuccessTimer(Sender: TObject);
  private
    fCallback: TAsyncCallback;
    fTimer: TTimer;
    fTestID: Integer;
  public
    constructor Create(aTestID: Integer; aCallback: TAsyncCallback);
    destructor Destroy; override;
    procedure SimulateSuccess;
    procedure SimulateError;
    procedure SimulateFailure;
  end;


implementation

uses
  forms;

procedure TTesterSpec.AsyncTestDone(aTestID: Integer; const aMessage: String);
begin
  EndAsync(aTestID);
end;

procedure TTesterSpec.Test_Sync_Test;
begin
  // do nothing, that means it succeeds.
end;

procedure TTesterSpec.Test_Async_Test;
begin
  TAsyncTester.Create(BeginAsync,@AsyncTestDone).SimulateSuccess;
end;

procedure TTesterSpec.Test_Sync_Test_with_Error;
begin
  raise ETestFailure.Create('This test should fail');
end;

procedure TTesterSpec.Test_Async_Test_with_Error;
begin
  TAsyncTester.Create(BeginAsync,@AsyncTestDone).SimulateError;
end;

procedure TTesterSpec.Test_Async_Test_with_Failure;
begin
  TAsyncTester.Create(BeginAsync,@FailAsync).SimulateFailure;
end;

{ TAsyncTester }

procedure TAsyncTester.ErrorTimer(Sender: TObject);
begin
  Application.QueueAsyncCall(@RaiseError,0);
  fTimer.Enabled := false;
end;

procedure TAsyncTester.Fail(Data: PtrInt);
begin
  fCallback(fTestID,'This test should fail');
end;

procedure TAsyncTester.FailTimer(Sender: TObject);
begin
  Application.QueueAsyncCall(@Fail,0);
  fTimer.Enabled := false;
end;

procedure TAsyncTester.RaiseError(Data: PtrInt);
begin
  raise ETestFailure.Create('This test should raise an uncaught exception and time out');
end;

procedure TAsyncTester.Success(Data: PtrInt);
begin
  fCallback(fTestID,'');
end;

procedure TAsyncTester.SuccessTimer(Sender: TObject);
begin
  Application.QueueAsyncCall(@Success,0);
  fTimer.Enabled := false;
end;

constructor TAsyncTester.Create(aTestID: Integer; aCallback: TAsyncCallback);
begin
  fCallback := aCallback;
  fTestID := aTestID;
  fTimer := TTimer.Create(nil);
  fTimer.Enabled := false;
  fTimer.Interval := 1000;
end;

destructor TAsyncTester.Destroy;
begin
  FreeAndNil(fTimer);
  inherited Destroy;
end;

procedure TAsyncTester.SimulateSuccess;
begin
  fTimer.OnTimer:=@SuccessTimer;
  fTimer.Enabled := true;
end;

procedure TAsyncTester.SimulateError;
begin
  fTimer.OnTimer:=@ErrorTimer;
  fTimer.Enabled := true;
end;

procedure TAsyncTester.SimulateFailure;
begin
  fTimer.OnTimer:=@FailTimer;
  fTimer.Enabled := true;
end;

end.

