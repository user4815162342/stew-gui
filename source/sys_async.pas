unit sys_async;

{$mode objfpc}{$H+}

interface

// TODO: Once everything's using promises, get rid of the stuff here that
// doesn't belong.

{This unit allows the other units to easily behave in an asynchronous manner.
The mechanism isn't to create actual, separate threads, but to split longer
tasks into smaller tasks and queue them into the application's event loop,
to be handled after user input. As long as you're using this right, the user
should see a responsive interface, and no "hanging". If you don't break down
tasks far enough, you will see moments of hanging, but at least it won't be
in the middle of a mouse click, leaving the button depressed and the window
not repainting.

That said, this isn't a complete replacement for using threads where threads
are the preferred option. For example, network communication is very slow,
and should always be done in a separate thread. Also loading and processing
of large files. And, even if you *can* break down a long running operation
into short bits, if there are a lot of short bits, it can still cause problems
with the interface -- I would expect some flickering and lagging. However,
the architecture of the classes below should be able to be used to manage
such threaded operations and communicate back to the interface, with a little
bit of work.

Please don't attempt to override the benefits of this setup by trying to create
a "WaitFor" version of the async classes, or call Application.ProcessMessages
until the code is completed. The idea of these classes is to make these sorts
of things unnecessary by making "asynchronous callback" type code easier to
write.

NOTE: In order to use this, you need to set up a queue mechanism. In a GUI
application, you can do this with Application.QueueAsyncCall, but in other
environments, you'll have to come up with your own alternative. See SetAsyncCaller
function for more information.
}

{
Promises are a relatively old idea which have been popularized recently by async
coding techniques in JavaScript. It is an attempt to solve the problem of returning
data from asynchronous code (such as network requests). The basic idea is that
the promise is returned from the initial call to the function, and can be hooked
into by anything that needs to know this result. It is similar to the concept
of attaching to an event on a network request that gets called when the request
is resolved.

Translating the concept of promises over from JavaScript to a static compiled
language like pascal required a few tricks. Without closures, it is necessary
to use classes as the mechanism used to make the async calls. Below, these
are referred to as "TAsyncTasks". A TAsyncTask represents the input to an
asynchronous function, and the code required to resolve the function. It's
result is available as a Promise on the Task object.

The TPromise itself is meant only to represent the output that results from such
a task, and it contains ways that code can hook in to obtain these results once
they are ready (or be notified of any errors in the processing itself). TPromises
are kept separate from the TAsyncTasks themselves to maintain independence between
the method and the result: there are many ways of getting the same result, and it
shouldn't be necessary for the TPromise to know how it's data was retrieved.

The asynchronicity of the TAsyncTask can be handled in any number of ways. Right
now there are two options: TQueuedTask simply queues the action to happen later in
an even loop. This causes the action to block when it's less likely to cause
problems. TDeferredTask is set up to wait for another promise to return before
it acts, this is useful for chaining tasks. At some point there may be a
TThreadedTask available which would act in another thread, but that's not
absolutely necessary with the scale of work we are expecting in this program.

RULES
=====
* In order for a Task to be completed, "Resolve" or "Reject" *must* be called.
If this does not happen, the task isr never finished, the promise is also never
resolved, and code will *hang*.
* Tasks are freed automatically by calls to Resolve and Reject. Do *not* keep
tasks around anywhere, unless you are going to make absolutely sure that
references to them are also removed upon completion. Do *not* attempt to free
the task yourself, or access violations are likely.
* Promises are also freed automatically once all of it's callbacks are called
in resolve or reject. Follow the same rules as Tasks.
* Due to static typing, it is impossible to control what type of Promise you
are getting in the callbacks. Even once full generics are available in FreePascal,
this is going to be difficult to do. So, you must *know* what type of Promise
you are supposed to get back in the Sender parameter of a callback, and use
the 'as' operator to convert and get the output data you need.
* For simplicity, many promises allow public access to setting the answers.
Although this does not prevent observers from changing it during their callback,
this shouldn't be done. There is no guarantee of the order in which observers
are called, so changing the answer may mess up the functionality of previous
callers.

}

// FUTURE: Once Generics have better supported in Freepascal, make use
// of them to avoid having to typecaste the promises in callbacks in order
// to get the data.

uses
  Classes, SysUtils, fgl;

type

  { TPromise }

  TPromise = class;


  // Since exceptions are freed after they are caught, or raised to the top,
  // we have to use strings to report errors. However, I wouldn't be against
  // using a structured type at some point. Just have to make it easy to
  // copy an Exception into that structure.
  TPromiseException = string;
  TErrorback = procedure(Sender: TPromise; aError: TPromiseException) of object;
  TErrorbackList = specialize TFPGList<TErrorback>;
  TCallback = procedure(Sender: TPromise) of object;
  TCallbackList = specialize TFPGList<TCallback>;
  TPromiseState = (psInitialized,psResolving,psRejecting);

  TPromise = class
  strict private
    fCallbacks: TCallbackList;
    fErrorbacks: TErrorbackList;
    fError: TPromiseException;
    fState: TPromiseState; // should be initialized to psInitialized.
    fTag: Integer;
    procedure RunCallbacks;
    procedure RunErrorbacks;
  protected
    procedure Resolve;
    procedure Reject(aError: TPromiseException);
  public
    constructor Create;
    destructor Destroy; override;
    function After(aCallback: TCallback; aErrorback: TErrorback = nil): TPromise;
    procedure Catch(aErrorback: TErrorback);
    property Tag: Integer read fTag write fTag;
  end;

  { TAsyncTask }

  TAsyncTask = class(TObject)
  private
    fPromise: TPromise;
  protected
    procedure Resolve;
    procedure Reject(aError: TPromiseException);
    function CreatePromise: TPromise; virtual; abstract;
    // Useful for chaining promises.. just add this function as the catch
    // to a subpromise or subtask and it will automatically reject this promise.
    procedure SubPromiseRejected(Sender: TPromise; aError: TPromiseException);
    procedure SubPromiseResolved(Sender: TPromise);
  public
    constructor Create;
    function After(aCallback: TCallback; aErrorback: TErrorback = nil): TPromise;
    procedure Catch(aErrorback: TErrorback);
    property Promise: TPromise read fPromise;
  end;

  { TQueuedTask }

  TQueuedTask = class(TAsyncTask)
  private
    procedure RunQueuedTask;
    procedure DoQueue;
  protected
    procedure DoTask; virtual; abstract;
  public
    constructor Enqueue;
  end;

  { TDeferredTask2 }

  TDeferredTask2 = class(TAsyncTask)
  private
    fInputPromise: TPromise;
    procedure InputPromiseResolved(Sender: TPromise);
    procedure InputPromiseRejected(Sender: TPromise; aError: TPromiseException);
  protected
    property InputPromise: TPromise read fInputPromise;
    procedure DoTask(Input: TPromise); virtual; abstract;
  public
    constructor Defer(aInputPromise: TPromise);
  end;

  { TNotifyOnPromiseResolvedTask }

  // This is a simple task that returns a simple, undecorated
  // promise when another promise returns, so you can hide
  // the data on that original promise from the caller.
  TNotifyOnPromiseResolvedTask = class(TDeferredTask2)
  protected
    procedure DoTask({%H-}Input: TPromise); override;
    function CreatePromise: TPromise; override;
  end;

  // TODO: Should be able to get rid of the following...

  // Use this one to create your own deferred code. Just override DoCallback to
  // complete the functionality.

  { TDeferredCall }

  TDeferredCall = class
  private
    procedure Callback;
  protected
    procedure DoCallback; virtual; abstract;
  public
    constructor Create;
    procedure Enqueue;
  end;

  TDeferredCallback = procedure of object;
  TDeferredStringCallback = procedure(Data: String) of object;
  TDeferredBooleanCallback = procedure(Data: Boolean) of object;
  TDeferredStringArrayCallback = procedure(Data: array of String) of object;
  // Exceptions are freed after being caught, so deferring them to pass them onward
  // doesn't work. We need to pass the message instead. Someday, I may need
  // to pass more structured data.
  TDeferredExceptionCallback = procedure(Data: String) of object;

  TDeferredTask = class(TDeferredCall)
  private
    fErrorback: TDeferredExceptionCallback;
  protected
    procedure DoCallback; override;
    procedure DoTask; virtual; abstract;
    property ErrorBack: TDeferredExceptionCallback read fErrorback;
  public
    constructor Create(aErrorBack: TDeferredExceptionCallback);
  end;

  TAsyncCallQueuer = procedure(aCallback: TDeferredCallback);

var
  AsyncCallQueuer: TAsyncCallQueuer;

procedure SetAsyncCallQueuer(aQueuer: TAsyncCallQueuer);
procedure RemoveAsyncCallQueuer(aQueuer: TAsyncCallQueuer);


implementation

procedure SetAsyncCallQueuer(aQueuer: TAsyncCallQueuer);
begin
  if AsyncCallQueuer <> nil then
    raise Exception.Create('An async queuer method is already set. Please only set this once.');
  AsyncCallQueuer := aQueuer;
end;

procedure RemoveAsyncCallQueuer(aQueuer: TAsyncCallQueuer);
begin
  if AsyncCallQueuer <> aQueuer then
    raise Exception.Create('To avoid conflicts, please don''t unset the async queuer method without access to the original pointer.');
  AsyncCallQueuer := nil;
end;

{ TNotifyOnPromiseResolvedTask }

procedure TNotifyOnPromiseResolvedTask.DoTask(Input: TPromise);
begin
  Resolve;
end;

function TNotifyOnPromiseResolvedTask.CreatePromise: TPromise;
begin
  result := TPromise.Create;
end;

{ TDeferredTask2 }

procedure TDeferredTask2.InputPromiseRejected(Sender: TPromise;
  aError: TPromiseException);
begin
  Reject(aError);
end;

procedure TDeferredTask2.InputPromiseResolved(Sender: TPromise);
begin
  try
    DoTask(Sender);
  except
    on E: Exception do
      Reject(E.Message);
  end;
end;

constructor TDeferredTask2.Defer(aInputPromise: TPromise);
begin
  inherited Create;
  fInputPromise := aInputPromise;
  aInputPromise.After(@InputPromiseResolved,@InputPromiseRejected);

end;

{ TQueuedTask }

procedure TQueuedTask.RunQueuedTask;
begin
  try
    DoTask;
  except
    on E: Exception do
      Reject(E.Message);
  end;
end;

procedure TQueuedTask.DoQueue;
begin
  if AsyncCallQueuer <> nil then
     AsyncCallQueuer(@RunQueuedTask)
  else
     raise Exception.Create('Async system is not set up');
end;

constructor TQueuedTask.Enqueue;
begin
  inherited Create;
  DoQueue;
end;

{ TAsyncTask }

procedure TAsyncTask.Resolve;
begin
  fPromise.Resolve;
  Free;
end;

procedure TAsyncTask.Reject(aError: TPromiseException);
begin
  fPromise.Reject(aError);
  Free;
end;

procedure TAsyncTask.SubPromiseRejected(Sender: TPromise;
  aError: TPromiseException);
begin
  Reject(aError);
end;

procedure TAsyncTask.SubPromiseResolved(Sender: TPromise);
begin
  Resolve;
end;

constructor TAsyncTask.Create;
begin
  inherited Create;
  fPromise := CreatePromise;

end;

function TAsyncTask.After(aCallback: TCallback; aErrorback: TErrorback
  ): TPromise;
begin
  result := fPromise.After(aCallback,aErrorback);
end;

procedure TAsyncTask.Catch(aErrorback: TErrorback);
begin
  fPromise.Catch(aErrorback);
end;

{ GPromise }

procedure TPromise.RunCallbacks;
var
  lCallback: TCallback;
begin
  if fCallbacks.Count > 0 then
  begin;
    lCallback := fCallbacks[0];
    fCallbacks.Delete(0);
    try
       lCallback(Self);
    except
      on E: Exception do
      begin
        Reject(E.Message);
        Exit;
      end;
    end;
    AsyncCallQueuer(@RunCallbacks);
  end
  else
      Free;

end;

procedure TPromise.RunErrorbacks;
var
  lCallback: TErrorback;
begin
  if fErrorbacks.Count > 0 then
  begin;
    lCallback := fErrorbacks[0];
    fErrorbacks.Delete(0);
    try
       // don't want an error to happen here. This does
       // mean the error is lost, but error handling shouldn't
       // do much that is intensive enough to avoid errors anyway.
       // rollbacks should be done in the DoTask.
       lCallback(Self,fError);
    except
      // do nothing here, I don't like it, but it's necessary.
    end;
    AsyncCallQueuer(@RunErrorbacks);
  end
  else
      Free;
end;


procedure TPromise.Resolve;
begin
  if fState in [psResolving,psRejecting] then
     raise Exception.Create('Promise already resolved');
  fState := psResolving;
  // we should know by now if the async system isn't setup, since that
  // is checked in the constructor.
  if AsyncCallQueuer <> nil then
     AsyncCallQueuer(@RunCallbacks)
  else
     raise Exception.Create('Async system is not set up');
end;

procedure TPromise.Reject(aError: TPromiseException);
begin
  fError := aError;
  fState := psRejecting;
  if AsyncCallQueuer <> nil then
     AsyncCallQueuer(@RunErrorbacks)
  else
     raise Exception.Create('Async system is not set up');
end;

constructor TPromise.Create;
begin
  inherited Create;
  fCallbacks := TCallbackList.Create;
  fErrorbacks := TErrorbackList.Create;
  fError := '';
end;

destructor TPromise.Destroy;
begin
  FreeAndNil(fCallbacks);
  FreeAndNil(fErrorbacks);
  inherited Destroy;
end;

function TPromise.After(aCallback: TCallback; aErrorback: TErrorback): TPromise;
begin
  result := Self;
  case fState of
    psInitialized, psResolving:
    begin
      if aCallback <> nil then
         fCallbacks.Add(aCallback);
      if aErrorback <> nil then
         fErrorbacks.Add(aErrorback);
    end;
    psRejecting:
      // don't bother adding the callback.
      if aErrorback <> nil then
         fErrorbacks.Add(aErrorback);
  end;
end;

procedure TPromise.Catch(aErrorback: TErrorback);
begin
  After(nil,aErrorback);
end;

procedure TDeferredCall.Callback;
begin
  DoCallback;
  Free;
end;

constructor TDeferredCall.Create;
begin
  inherited Create;
end;

procedure TDeferredCall.Enqueue;
begin
  if AsyncCallQueuer <> nil then
     AsyncCallQueuer(@Self.Callback)
  else
     raise Exception.Create('Async system is not set up');
end;


procedure TDeferredTask.DoCallback;
begin
  try
    DoTask;
  except
    on E: Exception do
    begin
       fErrorback(E.Message);
       //TDeferredExceptionCall.Create(fErrorback,E.Message).Enqueue;
    end;
  end;
end;

constructor TDeferredTask.Create(aErrorBack: TDeferredExceptionCallback);
begin
  inherited Create;
  fErrorback := aErrorBack;
end;


end.

