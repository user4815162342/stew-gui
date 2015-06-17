unit test_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, test_registry;

type

  { TMainForm }

  TMainForm = class(TForm)
    ResultsMemo: TMemo;
    MainToolbar: TToolBar;
    RunButton: TToolButton;
    CancelButton: TToolButton;
    CloseButton: TToolButton;
    RunTesterTestsButton: TToolButton;
    TestTimeoutTimer: TTimer;
    procedure CancelButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
    procedure RunTesterTestsButtonClick(Sender: TObject);
    procedure TestException(Sender: TObject; E: Exception);
    procedure TestTimeoutTimerTimer(Sender: TObject);
  private
    { private declarations }
    fRegistry: TTestRegistry;
    fCancelled: Boolean;
    fCurrent: Integer;
    procedure InitRegistry(aTestTester: Boolean);
    procedure RunNextTest({%H-}Data: PtrInt);
    procedure StartTests({%H-}Data: PtrInt);
    procedure TestsComplete;
    procedure QueueNextTest;
    procedure TestFailed(const aMessage: String);
    procedure TestSucceeded;
    procedure CancelTests;
  public
    { public declarations }
    procedure Log(const aMessage: String);
  end;

var
  MainForm: TMainForm;

implementation

uses
  test_test, test_sys_json;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@StartTests,0);

end;

procedure TMainForm.CancelButtonClick(Sender: TObject);
begin
  CancelTests;
end;

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException:=@TestException;
end;

procedure TMainForm.RunButtonClick(Sender: TObject);
begin
  StartTests(0);
end;

procedure TMainForm.RunTesterTestsButtonClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@StartTests,1);
end;

procedure TMainForm.TestException(Sender: TObject; E: Exception);
begin
  if fRegistry <> nil then
     TestFailed(E.Message)
  else
    Application.ShowException(E);
end;

procedure TMainForm.TestTimeoutTimerTimer(Sender: TObject);
begin
  CancelTests;
  TestFailed('Timed out after waiting ' + FloatToStr(TestTimeoutTimer.Interval/1000) + ' seconds for a response');
end;

procedure TMainForm.InitRegistry(aTestTester: Boolean);
begin
  if fRegistry = nil then
  begin
    fCancelled := false;
    fCurrent := -1;
    fRegistry := TTestRegistry.Create;
    if aTestTester then
       TTesterTester.Register(fRegistry)
    else
    begin
      // TODO: Create the various test objects and register them
      // in order, here.
      TJSONTester.Register(fRegistry);
    end;
  end;
end;

procedure TMainForm.StartTests(Data: PtrInt);
begin
  RunButton.Enabled := false;
  RunTesterTestsButton.Enabled := false;
  CancelButton.Enabled := true;
  InitRegistry(Data = 1);
  Log('Starting Test');
  QueueNextTest;

end;

procedure TMainForm.TestsComplete;
begin
  if fCancelled then
  begin
    Log('Tests cancelled');
  end
  else
  begin
    Log('Tests complete');
  end;
  FreeAndNil(fRegistry);
  CancelButton.Enabled := false;
  RunButton.Enabled := true;
  RunTesterTestsButton.Enabled := true;
end;

procedure TMainForm.RunNextTest(Data: PtrInt);
var
  lTest: TTest;
begin
  fCurrent := fCurrent + 1;
  lTest := fRegistry.Get(fCurrent);
  if lTest <> nil then
    try
      TestTimeoutTimer.Enabled := true;
      lTest(@TestSucceeded);
    except
      on E: Exception do
        TestFailed(E.Message);
    end
  else
    TestsComplete;
end;

procedure TMainForm.QueueNextTest;
begin
  if not fCancelled then
     Application.QueueAsyncCall(@RunNextTest,0)
  else
    TestsComplete;
end;

procedure TMainForm.TestFailed(const aMessage: String);
begin
  TestTimeoutTimer.Enabled := false;
  Log(fRegistry.GetName(fCurrent) + ' Failed: ' + aMessage);
  QueueNextTest;
end;

procedure TMainForm.TestSucceeded;
begin
  TestTimeoutTimer.Enabled := false;
  Log(fRegistry.GetName(fCurrent) + ' Succeeded');
  QueueNextTest;
end;

procedure TMainForm.CancelTests;
begin
  fCancelled := true;
end;

procedure TMainForm.Log(const aMessage: String);
begin
  ResultsMemo.Lines.Add(FormatDateTime('yyyy"-"mm"-"dd" "hh":"mm":"ss',Now) + ': ' + aMessage);

end;

initialization

end.

