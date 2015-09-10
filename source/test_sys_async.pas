unit test_sys_async;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_async;

type

  { TAsyncSpec }

  TAsyncSpec = class(TTestSpec)
    procedure After_Callback_Error(Sender: TPromise);
    procedure After_Chain(Sender: TPromise);
    procedure After_Deferred(Sender: TPromise);
    procedure After_Promise_Multiple_2(Sender: TPromise);
    procedure Catch_Callback_Error(Sender: TPromise; aError: TPromiseError);
    procedure Catch_Chain(Sender: TPromise; aError: TPromiseError);
    procedure Catch_Deferred(Sender: TPromise; aError: TPromiseError);
    procedure Catch_Promise_Multiple(Sender: TPromise; aError: TPromiseError
      );
  private
    fAsyncCode: Integer;
    fMultipleCount: Integer;
    procedure After_Promise_Basics_Resolved(Sender: TPromise);
    procedure After_Promise_Basics_Errored(Sender: TPromise);
    procedure After_Promise_Basics_Rejected(Sender: TPromise);
    procedure After_Promise_Multiple_1(Sender: TPromise);
    procedure Catch_Promise_Basics_Errored(Sender: TPromise;
      {%H-}aError: TPromiseError);
    procedure Catch_Promise_Basics_Rejected(Sender: TPromise;
      {%H-}aError: TPromiseError);
    procedure Catch_Promise_Basics_Resolved(Sender: TPromise;
      {%H-}aError: TPromiseError);
    procedure FinishCleanup(Data: PtrInt);
  public
    procedure SetupTest; override;
    procedure CleanupTest; override;
  published
    procedure Test_Promise_Resolve;
    procedure Test_Promise_Reject;
    procedure Test_Promise_Error;
    procedure Test_Multiple_Callbacks;
    procedure Test_Callback_Error;
    procedure Test_Chains;
    procedure Test_Deferrance;
    procedure Test_DeferredTask;
  end;

  { TTestPromise }
  TTestPromise = class(TPromise)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TTestQueuedTask }

  TTestQueuedTask = class(TQueuedTask)
  protected
    function CreatePromise: TPromise; override;
  public
    constructor Enqueue;
    destructor Destroy; override;
  end;

  { TTestPromiseResolve }

  TTestPromiseResolve = class(TTestQueuedTask)
  public
    procedure DoTask; override;
  end;

  { TTestPromiseReject }

  TTestPromiseReject = class(TTestQueuedTask)
  public
    procedure DoTask; override;
  end;

  { TTestPromiseError }

  TTestPromiseError = class(TTestQueuedTask)
  public
    procedure DoTask; override;
  end;

  { TTestChainPromise }

  TTestChainPromise = class(TTestQueuedTask)
    procedure Fail(Sender: TPromise; aError: TPromiseError);
    procedure Success(Sender: TPromise);
  private
    fCount: Integer;
  public
    procedure DoTask; override;
    constructor Enqueue(aCount: Integer);
  end;

  { TTestDeferralOfPromise }

  TTestDeferralOfPromise = class(TTestQueuedTask)
  private
    FDone: Boolean;
  public
    constructor Enqueue;
    procedure DoTask; override;
    property Done: Boolean read FDone;
  end;

  { TTestDeferredTask }

  TTestDeferredTask = class(TDeferredTask2)
  protected
    function CreatePromise: TPromise; override;
    procedure DoTask({%H-}Input: TPromise); override;
  public
    constructor Defer(aInputPromise: TPromise);
    destructor Destroy; override;
  end;


  var
    gPromiseCounter: Integer = 0;


implementation

uses
  gui_async;

{ TTestDeferredTask }

function TTestDeferredTask.CreatePromise: TPromise;
begin
  result := TTestPromise.Create;
end;

procedure TTestDeferredTask.DoTask(Input: TPromise);
begin
  Resolve;
end;

constructor TTestDeferredTask.Defer(aInputPromise: TPromise);
begin
  inherited Defer(aInputPromise);
  Inc(gPromiseCounter);
end;

destructor TTestDeferredTask.Destroy;
begin
  Dec(gPromiseCounter);
  inherited Destroy;
end;

{ TTestQueuedTask }

function TTestQueuedTask.CreatePromise: TPromise;
begin
  result := TTestPromise.Create;
end;

constructor TTestQueuedTask.Enqueue;
begin
  inherited Enqueue;
  inc(gPromiseCounter)
end;

destructor TTestQueuedTask.Destroy;
begin
  Dec(gPromiseCounter);
  inherited Destroy;
end;

{ TTestDeferralOfPromise }

constructor TTestDeferralOfPromise.Enqueue;
begin
  inherited Enqueue;
  FDone := false;
end;

procedure TTestDeferralOfPromise.DoTask;
begin
  FDone := true;
  Resolve;
end;

{ TTestChainPromise }

procedure TTestChainPromise.Fail(Sender: TPromise; aError: TPromiseError);
begin
  Reject(aError);
end;

procedure TTestChainPromise.Success(Sender: TPromise);
begin
  Resolve;
end;

procedure TTestChainPromise.DoTask;
begin
  if fCount = 0 then
     TTestPromiseResolve.Enqueue.After(@Success,@Fail)
  else
     TTestChainPromise.Enqueue(fCount - 1).After(@Success,@Fail);
end;

constructor TTestChainPromise.Enqueue(aCount: Integer);
begin
  inherited Enqueue;
  fCount := aCount;
end;

{ TTestPromise }

constructor TTestPromise.Create;
begin
  inherited Create;
  inc(gPromiseCounter);

end;

destructor TTestPromise.Destroy;
begin
  dec(gPromiseCounter);
  inherited Destroy;
end;

{ TTestPromiseError }

procedure TTestPromiseError.DoTask;
begin
  raise ETestFailure.Create('Promise raised error');
end;

{ TTestPromiseReject }

procedure TTestPromiseReject.DoTask;
begin
  Reject('Promise rejected');
end;

{ TTestPromise }

procedure TTestPromiseResolve.DoTask;
begin
  Resolve;
end;

{ TAsyncSpec }

procedure TAsyncSpec.After_Promise_Basics_Errored(Sender: TPromise);
begin
  FailAsync(fAsyncCode,'Promise raising error should not have resolved');
end;
{%H-}// Without this You'll see a Hint: Parameter "aResult" not used here. This is a problem with
// generics support which will hopefully be fixed someday.

procedure TAsyncSpec.After_Promise_Basics_Rejected(Sender: TPromise);
begin
  FailAsync(fAsyncCode,'Promise rejected should not have been resolved');
end;

procedure TAsyncSpec.Catch_Promise_Basics_Errored(Sender: TPromise;
  aError: TPromiseError);
begin
  EndAsync(fAsyncCode);
end;

procedure TAsyncSpec.Catch_Promise_Basics_Rejected(Sender: TPromise;
  aError: TPromiseError);
begin
  EndAsync(fAsyncCode);
end;

procedure TAsyncSpec.Catch_Promise_Basics_Resolved(Sender: TPromise;
  aError: TPromiseError);
begin
  FailAsync(fAsyncCode,'Promise resolved should not have been rejected');
end;

procedure TAsyncSpec.FinishCleanup(Data: PtrInt);
begin
  RemoveAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
  if gPromiseCounter > 0 then
     FailAsync(Data,IntToStr(gPromiseCounter) + ' promise(s) or task(s) didn''t get freed')
  else
      EndAsync(Data);
end;

procedure TAsyncSpec.After_Promise_Multiple_1(Sender: TPromise);
begin
  inc(fMultipleCount);

end;

procedure TAsyncSpec.After_Callback_Error(Sender: TPromise);
begin
  raise ETestFailure.Create('This callback raises an error');
end;

procedure TAsyncSpec.After_Chain(Sender: TPromise);
begin
  EndAsync(fAsyncCode);
end;

procedure TAsyncSpec.After_Deferred(Sender: TPromise);
begin
  EndAsync(fAsyncCode);
end;

procedure TAsyncSpec.After_Promise_Multiple_2(Sender: TPromise);
begin
  inc(fMultipleCount);
  Assert(fMultipleCount = 2,'Should have called exactly two callbacks by now');
  EndAsync(fAsyncCode);
end;

procedure TAsyncSpec.Catch_Callback_Error(Sender: TPromise;
  aError: TPromiseError);
begin
  Assert(aError = 'This callback raises an error','The promise should have been rejected here');
  EndAsync(fAsyncCode);
end;

procedure TAsyncSpec.Catch_Chain(Sender: TPromise; aError: TPromiseError);
begin
  FailAsync(fAsyncCode,aError);
end;

procedure TAsyncSpec.Catch_Deferred(Sender: TPromise; aError: TPromiseError
  );
begin
  FailAsync(fAsyncCode,aError);
end;

procedure TAsyncSpec.Catch_Promise_Multiple(Sender: TPromise;
  aError: TPromiseError);
begin
  FailAsync(fAsyncCode,aError);
end;

procedure TAsyncSpec.After_Promise_Basics_Resolved(Sender: TPromise);
begin
  EndAsync(fAsyncCode);
end;

procedure TAsyncSpec.SetupTest;
begin
  inherited SetupTest;
  SetAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
end;

procedure TAsyncSpec.CleanupTest;
begin
  // Need to give the promises a chance to clean themselves up. Before this:
  // 1. The last promise would handle it's callback
  // 2. That would call EndAsync
  // 3. The test registry would queue up the 'CleanupTest' procedure
  // 4. control returns to the promise, which queues it's check for callbacks
  // 5. The CleanupTest is called, and the last promise is not yet freed, causing an error here.
  // 6. The promise checks its callbacks, finds them empty and frees itself.
  // Now:
  // 1. The last promise handles its callback
  // 2. That calls EndAsync
  // 3. The test registry queues CleanupTest
  // 4. The promise queues its check for callbacks
  // 5. CleanupTest is called, and we queue up the finishing of the cleanup
  // 6. The promise checks its callbacks, finds them empty and frees itself.
  // 7. FinishCleanup is finally called, which checks if all of the promises have been
  //    freed.
  Defer(@FinishCleanup,BeginAsync);
end;

procedure TAsyncSpec.Test_Promise_Resolve;
begin
  fAsyncCode := BeginAsync;
  TTestPromiseResolve.Enqueue.After(@After_Promise_Basics_Resolved,@Catch_Promise_Basics_Resolved);
end;

procedure TAsyncSpec.Test_Promise_Reject;
begin
  fAsyncCode := BeginAsync;
  TTestPromiseReject.Enqueue.After(@After_Promise_Basics_Rejected,@Catch_Promise_Basics_Rejected);

end;

procedure TAsyncSpec.Test_Promise_Error;
begin
  fAsyncCode := BeginAsync;
  TTestPromiseError.Enqueue.After(@After_Promise_Basics_Errored,@Catch_Promise_Basics_Errored);

end;

procedure TAsyncSpec.Test_Multiple_Callbacks;
begin
  fMultipleCount := 0;
  fAsyncCode := BeginAsync;
  TTestPromiseResolve.Enqueue.After(@After_Promise_Multiple_1,@Catch_Promise_Multiple).After(@After_Promise_Multiple_2);
end;

procedure TAsyncSpec.Test_Callback_Error;
begin
  fAsyncCode := BeginAsync;
  TTestPromiseResolve.Enqueue.After(@After_Callback_Error,@Catch_Callback_Error);
end;

procedure TAsyncSpec.Test_Chains;
begin
  fAsyncCode := BeginAsync;
  TTestChainPromise.Enqueue(2).After(@After_Chain,@Catch_Chain);
end;

procedure TAsyncSpec.Test_Deferrance;
var
  lPromise: TTestDeferralOfPromise;
begin
  lPromise := TTestDeferralOfPromise.Enqueue;
  if lPromise.Done then
     raise Exception.Create('Promise functionality was not deferred');
end;

procedure TAsyncSpec.Test_DeferredTask;
begin
  fAsyncCode := BeginAsync;
  TTestDeferredTask.Defer(TTestPromiseResolve.Enqueue.Promise).After(@After_Deferred,@Catch_Deferred);
end;

end.

