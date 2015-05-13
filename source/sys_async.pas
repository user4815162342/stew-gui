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
  Classes, SysUtils;

type

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

