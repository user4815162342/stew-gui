unit gui_async;

{$mode objfpc}{$H+}

{Required in order to make use of sys_async in a GUI environment.}

interface

uses
  Classes, SysUtils, sys_async;

type
  { TAsyncCallback }

  TAsyncCallback = class
    procedure Callback({%H-}Data: PtrInt);
  private
    fCallback: TDeferredCallback;
  public
    constructor Create(aCallback: TDeferredCallback);
    procedure Enqueue;
  end;


procedure GUIQueueAsyncCall(aCallback: TDeferredCallback);

implementation

uses
  Forms;

procedure GUIQueueAsyncCall(aCallback: TDeferredCallback);
begin
  TAsyncCallback.Create(aCallback).Enqueue;
end;

{ TAsyncCallback }

procedure TAsyncCallback.Callback(Data: PtrInt);
begin
  fCallback;
  Free;
end;

constructor TAsyncCallback.Create(aCallback: TDeferredCallback);
begin
  inherited Create;
  fCallback := aCallback;
end;

procedure TAsyncCallback.Enqueue;
begin
  // Notice that I'm sending 0 as the data parameter to the Deferred call. Rather
  // then dealing with pointers, I've already got a pointer to the object in the
  // method, so I can store the data on there.
  Application.QueueAsyncCall(@Callback,0);
end;

end.

