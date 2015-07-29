unit sys_async;

{$mode objfpc}{$H+}

interface

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

uses
  Classes, SysUtils, fgl;

type

  { TPromise }

  // TODO: Convert over to using this for all async code.
  // Advantages:
  // - chains of functionality are easier because I can *add* callbacks on.
  // - I can just return the resulting object from the async function, and
  // the caller can add it's own callbacks on.
  // - I can even add callbacks on after the result is found, and these will
  //   be automatically deferred.
  // - I think it's easier to create then "TDeferredTask", because it's mostly just
  //   a matter of extending the generic and overriding DoTask.
  // - I think the system is more robust, and more flexible.
  // - I'm deferring the callbacks as well here, so it should spread out code
  //   even more.
  //
  // Some rules:
  // The TPromise is freed automatically after all callbacks are called.
  // - Don't keep a reference to this object, except as the result of a function.
  //   That way, you won't be tempted to add something to the promise later.
  // - You can add callbacks in the async functions themselves, through the sender
  //   argument, and they'll be added to the list in order and called after all
  //   of the other callbacks are done.
  //
  // - In order to chain data processing which changes the result value, just
  //   create a new TPromise class that handles the new data types, implement
  //   it's DoTask to do the processing, and queue that up with some callbacks.
  // - remember that DoTask does not have to be synchronous. You can create further
  //   async code that is triggered in DoTask and calls callbacks in the object,
  //   and only resolves or rejects once you get there.
  //
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
  TPromiseState = (psQueued,psResolving,psRejecting);

  // - To input data into a promise, one would simply create private variables
  // on a subclass and include these in parameters to an overridden Enqueue
  // constructor.
  // - To get output data from a promise, just use the 'as' operator on the
  // Promise parameter of the callback event to get your subclass of promise.
  TPromise = class
  strict private
    fCallbacks: TCallbackList;
    fErrorbacks: TErrorbackList;
    fError: TPromiseException;
    fState: TPromiseState;
    fTag: Integer;
    procedure RunCallbacks;
    procedure RunErrorbacks;
  strict protected
    procedure DoTask; virtual; abstract;
    procedure RunQueuedTask; virtual;
    procedure Resolve;
    procedure Reject(aError: TPromiseException);
    // Useful for chaining promises.. just add this function as the catch
    // to a subpromise and it will automatically reject this promise.
    procedure SubPromiseRejected(Sender: TPromise; aError: TPromiseException);
  public
    constructor Enqueue;
    destructor Destroy; override;
    function After(aCallback: TCallback; aErrorback: TErrorback = nil): TPromise;
    procedure Catch(aErrorback: TErrorback);
    property Tag: Integer read fTag write fTag;
  end;

  TBooleanPromise = class(TPromise)
  protected
    fAnswer: Boolean;
  public
    property Answer: Boolean read fAnswer;
  end;

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

procedure TPromise.RunQueuedTask;
begin
  try
    DoTask;
  except
    on E: Exception do
      Reject(E.Message);
  end;
end;

procedure TPromise.Resolve;
begin
  if fState <> psQueued then
     raise Exception.Create('Promise already resolved');
  fState := psResolving;
  // we should know by now if the async system isn't setup, since that
  // is checked in the constructor.
  AsyncCallQueuer(@RunCallbacks);
end;

procedure TPromise.Reject(aError: TPromiseException);
begin
  fError := aError;
  fState := psRejecting;
  AsyncCallQueuer(@RunErrorbacks);
end;

procedure TPromise.SubPromiseRejected(Sender: TPromise;
  aError: TPromiseException);
begin
  Reject(aError);
end;

constructor TPromise.Enqueue;
begin
  inherited Create;
  fCallbacks := TCallbackList.Create;
  fErrorbacks := TErrorbackList.Create;
  fState := psQueued;
  // In a future version of FPC, this should work: fResult := default(OutputType);
  fError := '';
  if AsyncCallQueuer <> nil then
     AsyncCallQueuer(@RunQueuedTask)
  else
     raise Exception.Create('Async system is not set up');
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
    psQueued, psResolving:
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

