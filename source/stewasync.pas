unit stewasync;

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

NOTE: This currently uses the Application.QueueAsyncCall, which is located in
the forms unit, which is why I've separated this out into a separate unit.
Any future non-GUI use of this code will require re-writing this for it's
own message loop.
}

uses
  Classes, SysUtils, Forms, LCLProc;

type

  // Use this one to create your own callbacks which might be more complex than
  // the Generics, below. Just override DoCallback to complete the functionality.

  { TDeferredCall }

  TDeferredCall = class
  private
    procedure Callback({%H-}Data: PtrInt);
  protected
    procedure DoCallback; virtual; abstract;
  public
    constructor Create;
    procedure Enqueue;
  end;

  TDeferredCallback = procedure of object;

  generic GDeferredCallback<T> = procedure(Data: T) of object;
  TDeferredObjectCallback = specialize GDeferredCallback<TObject>;
  TDeferredStringCallback = specialize GDeferredCallback<String>;
  TDeferredIntegerCallback = specialize GDeferredCallback<Integer>;
  TDeferredBooleanCallback = specialize GDeferredCallback<Boolean>;
  TDeferredStringsCallback = specialize GDeferredCallback<TStringList>;
  // Exceptions are freed after being caught, so deferring them to pass them onward
  // doesn't work. We need to pass the message instead.
  TDeferredExceptionCallback = specialize GDeferredCallback<String>;

  { TDeferredCaller }
  generic GDeferredCall<T,U> = class(TDeferredCall)
  private
    fData: T;
    {NOTE: If I refer to this as "specialize TDeferredCallback<T>", it won't
    let me call 'fCallback' as a method (I get an 'illegal expression' when attempting
    to compile that line). This ensures that I can still do it. However, if I use
    this mechanism to define the parameter to the constructor, then I get a different
    compiler error there. So, I still need the generic procedure types.}
    fCallback: procedure(Data: T) of object;
  protected
    procedure DoCallback; override;
  public
    constructor Create(aCallback: U; aData: T);
  end;

  // And this is how simple it is to create objects that can call Deferred methods
  // with different value types.
  TDeferredObjectCall = specialize GDeferredCall<TObject,TDeferredObjectCallback>;
  TDeferredStringCall = specialize GDeferredCall<String,TDeferredStringCallback>;
  TDeferredIntegerCall = specialize GDeferredCall<Integer,TDeferredIntegerCallback>;
  TDeferredBooleanCall = specialize GDeferredCall<Boolean,TDeferredBooleanCallback>;
  TDeferredStringsCall = specialize GDeferredCall<TStringList,TDeferredStringsCallback>;
  // Exceptions are freed after being caught, so deferring them to pass them onward
  // doesn't work. We need to pass the message instead.
  TDeferredExceptionCall = specialize GDeferredCall<String,TDeferredExceptionCallback>;

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



implementation

procedure TDeferredCall.Callback(Data: PtrInt);
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
  // Notice that I'm sending 0 as the data parameter to the Deferred call. Rather
  // then dealing with pointers, I've already got a pointer to the object in the
  // method, so I can store the data on there.
  Application.QueueAsyncCall(@Self.Callback,0);
end;



procedure GDeferredCall.DoCallback;
begin
  fCallback(fData);
end;

constructor GDeferredCall.Create(aCallback: U; aData: T);
begin
  inherited Create;
  fData := aData;
  fCallback := aCallback;
end;


procedure TDeferredTask.DoCallback;
begin
  try
    DoTask;
  except
    on E: Exception do
    begin
       TDeferredExceptionCall.Create(fErrorback,E.Message).Enqueue;
    end;
  end;
end;

constructor TDeferredTask.Create(aErrorBack: TDeferredExceptionCallback);
begin
  inherited Create;
  fErrorback := aErrorBack;
end;


end.

