unit gui_async;

{$mode objfpc}{$H+}

{Required in order to make use of sys_async in a GUI environment.}

{
FUTURE:
One issue with the way this is done. If you were to queue up a lot of tasks,
the application will still block while waiting for those tasks to complete --
the async queue runs *after* process messages, and doesn't return to processing
messages until it's done, and the OnIdle event is called after. So far, this
hasn't been a problem. It is possible to set some timer code in here, and the
longest I've seen a single queue of tasks takes is 100ms before returning to
processmessages.

However, if it does appear to become a problem, there are a few solutions, but
they're more complicated. Most of them involve keeping my own queue of tasks,
and limiting the number that can be run at one time. Which means I'm just rewriting
the asynccallqueue stuff anyway. A better solution would be a minor modification
to application.inc that would limit these things, but I'm not sure if that would
even happen if I suggested it.

}

interface

uses
  Classes, SysUtils, sys_async;

type
    { TQueuedCall }

  TQueuedCall = class
  strict private
    fCallback: TQueuedCallback;
    procedure Callback({%H-}Data: PtrInt);
  public
    constructor Create(aCallback: TQueuedCallback);
    procedure Enqueue;
  end;


procedure GUIQueueAsyncCall(aCallback: TQueuedCallback);

implementation

uses
  Forms, LCLIntf, sys_log;

procedure GUIQueueAsyncCall(aCallback: TQueuedCallback);
begin
  TQueuedCall.Create(aCallback).Enqueue;
end;

{ TQueuedCall }

procedure TQueuedCall.Callback(Data: PtrInt);
begin
  try
    try
      fCallback;
    except
      // Catch any exceptions so they can be reported here...
      on E: Exception do
      begin
        LogException('gui_async.TQueuedCall.Callback',E);
        Application.ShowException(E);
      end;
    end;
  finally
    Free;
  end;
end;

constructor TQueuedCall.Create(aCallback: TQueuedCallback);
begin
  inherited Create;
  fCallback := aCallback;
end;

procedure TQueuedCall.Enqueue;
begin
  // Notice that I'm sending 0 as the data parameter to the Deferred call. Rather
  // then dealing with pointers, I've already got a pointer to the object in the
  // method, so I can store the data on there.
  Application.QueueAsyncCall(@Callback,0);
end;



end.

