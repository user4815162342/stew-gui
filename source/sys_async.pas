unit sys_async;

{$mode objfpc}{$H+}

interface

// TODO: Need to handle errors as well. Could I create some sort of simple
// Promise architecture? Promises are a bit more complex, but maybe there's
// a really simple mechanism.
//
// Although, with a type-safe language, I am guaranteed not to accidentally
// call a procedure that requires a callback.

{This unit allows the other units to easily behave in an asynchronous manner.
The mechanism isn't to create actual, separate threads, but to split longer
tasks into smaller tasks and queue them into the application's event loop,
to be handled after user input. As long as you're using this right, the user
should see a responsive interface, and no "hanging". If you don't break down
tasks far enough, you will see moments of hanging, but at least it won't be
in the middle of a mouse click.

NOTE: In order to use this, you need to set up a queue mechanism. In a GUI
application, you can do this with Application.QueueAsyncCall, but in other
environments, you'll have to come up with your own mechanism.
}

uses
  Classes, SysUtils, fgl;

type

  { TPromise }

  // This one is basically a proof of concept. It's not used anywhere in the code,
  // but it might be nice to try converting over in order to get this correct.
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
  TPromiseException = string;
  TErrorback = procedure(Sender: TObject; aError: TPromiseException) of object;
  TErrorbackList = specialize TFPGList<TErrorback>;
  TPromiseState = (psQueued,psResolving,psRejecting);

  // - I'm also considering a TCustomPromise which does a bunch of stuff here, then
  //   a TNoInputPromise<OutputType> and a TNoOutputPromise<InputType>, both
  //   of which are useful.
  // TODO: Consider adding:
  // - a 'Cancel' command which sets a flag on the promise for long chains,
  // and can be checked before calling DoTask.
  // - a ProgressCallback option which can be used to watch for progress.
  // - In order to allow the promise to stick around for a while, and possibly
  //   be cancelled if necessary, use a static TList or maybe a hashlist and
  //   some sort of unique instance identifier guaranteed to be 0 if the object
  //   is not created, on create, add it to the list, on destroy, remove it. And,
  //   if the object is not present in the list, then it is destroyed.
  generic TPromise<InputType,OutputType> = class
  private type
  public type
    // Since exceptions are freed after they are caught, or raised to the top,
    // we have to deal with strings. However, I wouldn't be against using a
    // structured type at some point.
    TCallback = procedure(Sender: TObject; aResult: OutputType) of object;
    TCallbackList = specialize TFPGList<TCallback>;
  private
    fCallbacks: TCallbackList;
    fErrorbacks: TErrorbackList;
    fInput: InputType;
    fResult: OutputType;
    fError: TPromiseException;
    fState: TPromiseState;
    procedure RunCallbacks;
    procedure RunErrorbacks;
  protected
    // FUTURE: Once I can have abstract methods here, this should be abstract.
    // TODO: Don't forget to call Resolve to complete the promise.
    procedure DoTask; virtual;
    procedure RunQueuedTask; virtual;
    procedure Resolve(aResult: OutputType);
    procedure Reject(aError: TPromiseException);
    // FUTURE: Once I can have abstract methods here, this should be abstract.
    procedure DestroyResult(var {%H-}aResult: OutputType); virtual;
  public
    constructor Enqueue(aInput: InputType);
    destructor Destroy; override;
    procedure After(aCallback: TCallback; aErrorback: TErrorback = nil);
    procedure Catch(aErrorback: TErrorback);
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


implementation

procedure SetAsyncCallQueuer(aQueuer: TAsyncCallQueuer);
begin
  AsyncCallQueuer := aQueuer;
end;

{ TPromise }

procedure TPromise.RunCallbacks;
var
  lCallback: TCallback;
begin
  if fCallbacks.Count > 0 then
  begin;
    lCallback := fCallbacks[0];
    fCallbacks.Delete(0);
    try
       lCallback(Self,fResult);
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
    AsyncCallQueuer(@RunCallbacks);
  end
  else
      Free;
end;

procedure TPromise.DoTask;
begin
  // this function is implemented in order to avoid a compile error.
  // Future versions of FPC are supposed to allow abstract methods in generics
  // and this won't be necessary.
  AbstractError;
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

procedure TPromise.Resolve(aResult: OutputType);
begin
  if fState <> psQueued then
     raise Exception.Create('Promise already resolved');
  fResult := aResult;
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

procedure TPromise.DestroyResult(var aResult: OutputType);
begin
  // do nothing, because I don't know what type of result it is.
end;

constructor TPromise.Enqueue(aInput: InputType);
begin
  inherited Create;
  fCallbacks := TCallbackList.Create;
  fErrorbacks := TErrorbackList.Create;
  fInput := aInput;
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
  DestroyResult(fResult);
  FreeAndNil(fCallbacks);
  FreeAndNil(fErrorbacks);
  inherited Destroy;
end;

procedure TPromise.After(aCallback: TCallback; aErrorback: TErrorback);
begin
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

