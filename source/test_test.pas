unit test_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, ExtCtrls;

type

  TTesterTester = class(TTester)
  published
    procedure TestSync(aCallback: TCallback);
    procedure TestAsync(aCallback: TCallback);
    procedure TestSyncRaise({%H-}aCallback: TCallback);
    procedure TestAsyncRaise(aCallback: TCallback);
  end;

  { TAsyncTester }

  TAsyncTester = class
    procedure Fail({%H-}Data: PtrInt);
    procedure FailTimer(Sender: TObject);
    procedure Success({%H-}Data: PtrInt);
    procedure SuccessTimer(Sender: TObject);
  private
    fCallback: TCallback;
    fTimer: TTimer;
  public
    constructor Create(aCallback: TCallback);
    destructor Destroy; override;
    procedure SimulateSuccess;
    procedure SimulateFailure;
  end;


implementation

uses
  forms;

procedure TTesterTester.TestSync(aCallback: TCallback);
begin
  aCallback;
end;

procedure TTesterTester.TestAsync(aCallback: TCallback);
begin
  TAsyncTester.Create(aCallback).SimulateSuccess;
end;

procedure TTesterTester.TestSyncRaise(aCallback: TCallback);
begin
  raise ETestFailure.Create('This test should fail');
end;

procedure TTesterTester.TestAsyncRaise(aCallback: TCallback);
begin
  TAsyncTester.Create(aCallback).SimulateFailure;
end;

{ TAsyncTester }

procedure TAsyncTester.Fail(Data: PtrInt);
begin
  raise ETestFailure.Create('This test should fail');
end;

procedure TAsyncTester.FailTimer(Sender: TObject);
begin
  Application.QueueAsyncCall(@Fail,0);
  fTimer.Enabled := false;
end;

procedure TAsyncTester.Success(Data: PtrInt);
begin
  fCallback;

end;

procedure TAsyncTester.SuccessTimer(Sender: TObject);
begin
  Application.QueueAsyncCall(@Success,0);
  fTimer.Enabled := false;
end;

constructor TAsyncTester.Create(aCallback: TCallback);
begin
  fCallback := aCallback;
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

procedure TAsyncTester.SimulateFailure;
begin
  fTimer.OnTimer:=@FailTimer;
  fTimer.Enabled := true;
end;

end.

