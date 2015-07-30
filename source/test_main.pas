unit test_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RichMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, test_registry;

type

  {
  TODO: This testing is being done as part of a slight refactoring to make use
  of TPromises and TJSValues. I will slowly be converting other code to work with
  this in the GUI, and create test code as I do. The next steps:
  1. Convert the various properties objects to make use of TJSObjects. And,
  create tests to validate that they are working (so we can get rid of the
  'backup files').
  - all of these should be found in stew_properties and gui_config (this one
  won't be tested, though). Although, since I'm getting rid of the ability to
  load and save from within the object, some things will have to be changed in
  stew_project as well -- the properties become a Metadata object like synopsis
  (and these are probably moved into stew_persist).
  2. Create some tests to work with TStewProject opening, initializing and retrieving
  data. This will require some test projects to work with. Don't create a *lot*
  of tests, just basic operations. More tests will be added to this section as
  bugs are reported in the GUI. This process should also involve moving those
  things over to TPromises.
  3. Convert the rest of the project stuff to make use of promises for various
  things instead of a whole bunch of events (although events might remain there
  yet for at least some cases where the application needs to know when data has
  changed).

  TODO: Consider keeping a "log" (this would be an application setting)
  of "actions", and their responses in the StewProject option. That will, at least,
  make it easier to debug and reproduce errors, if we can get users to reproduce
  and send the logs to us. Basically, it would tell us what order actions happened
  in order to create tests around them at the project level.
  }

  { TMainForm }

  TMainForm = class(TForm)
    MainToolbar: TToolBar;
    OutputMemo: TRichMemo;
    RunButton: TToolButton;
    CancelButton: TToolButton;
    CloseButton: TToolButton;
    RunTesterTestsButton: TToolButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RunButtonClick(Sender: TObject);
    procedure RunTesterTestsButtonClick(Sender: TObject);
    procedure TestAlert(const {%H-}aSender: TObject; const aTestName: String;
      const aMessage: String);
    procedure TestFailed(const {%H-}aSender: TObject; const aTestName: String;
      const aMessage: String);
    procedure TestsCancelled(Sender: TObject);
    procedure TestsCompleted(Sender: TObject);
    procedure TestsException(Sender: TObject; E: Exception);
    procedure TestStarted(const {%H-}aSender: TObject; const aTestName: String);
    procedure TestSucceeded(const {%H-}aSender: TObject; const aTestName: String);
  private
    { private declarations }
    fRegistry: TTestRegistry;
    procedure InitRegistry(aTestTester: Boolean);
    procedure StartTests({%H-}Data: PtrInt);
  public
    { public declarations }
    procedure Log(const aMessage: String; aType: TMsgDlgType);
  end;

var
  MainForm: TMainForm;

implementation

uses
  test_test, test_sys_json, test_sys_async, test_sys_localfile;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@StartTests,0);

end;

procedure TMainForm.CancelButtonClick(Sender: TObject);
begin
  if fRegistry <> nil then
    fRegistry.Cancel;
end;

procedure TMainForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fRegistry);
end;

procedure TMainForm.RunButtonClick(Sender: TObject);
begin
  StartTests(0);
end;

procedure TMainForm.RunTesterTestsButtonClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@StartTests,1);
end;

procedure TMainForm.TestAlert(const aSender: TObject; const aTestName: String;
  const aMessage: String);
begin
  Log(aTestName + ' reports: ' + aMessage,mtWarning);
end;

procedure TMainForm.TestFailed(const aSender: TObject; const aTestName: String;
  const aMessage: String);
begin
  Log(aTestName + ' FAILED: ' + aMessage,mtError);
end;

procedure TMainForm.TestsCancelled(Sender: TObject);
begin
  Log('User Cancelled Tests',mtWarning);
end;

procedure TMainForm.TestsCompleted(Sender: TObject);
begin
  Log('Tests Completed',mtInformation);
  CancelButton.Enabled := false;
  RunButton.Enabled := true;
  RunTesterTestsButton.Enabled := true;
end;

procedure TMainForm.TestsException(Sender: TObject; E: Exception);
begin
  Log('UNCAUGHT EXCEPTION: ' + E.Message,mtError);
end;

procedure TMainForm.TestStarted(const aSender: TObject; const aTestName: String
  );
begin
  Log(aTestName + ' started',mtInformation);
end;

procedure TMainForm.TestSucceeded(const aSender: TObject;
  const aTestName: String);
begin
  Log(aTestName + ' succeeded',mtInformation);
end;

procedure TMainForm.InitRegistry(aTestTester: Boolean);
begin
  if fRegistry = nil then
  begin
    fRegistry := TTestRegistry.Create;
    fRegistry.OnCancelled:=@TestsCancelled;
    fRegistry.OnCompleted:=@TestsCompleted;
    fRegistry.OnFailed:=@TestFailed;
    fRegistry.OnSucceeded:=@TestSucceeded;
    fRegistry.OnException:=@TestsException;
    fRegistry.OnAsyncStarted:=@TestStarted;
    fRegistry.OnAlert:=@TestAlert;
    fRegistry.Timeout := 5000;
  end;
  fRegistry.ClearTests;
  if aTestTester then
     fRegistry.AddTests(TTesterSpec)
  else
  begin
    // TODO: Create the various test objects and register them
    // in order, here. I don't do this with 'initialization' because
    // I want to control the order.
    fRegistry.AddTests(TJSONSpec);
    fRegistry.AddTests(TAsyncSpec);
    fRegistry.AddTests(TLocalFileSpec);
  end;
end;

procedure TMainForm.StartTests(Data: PtrInt);
begin
  RunButton.Enabled := false;
  RunTesterTestsButton.Enabled := false;
  CancelButton.Enabled := true;
  InitRegistry(Data = 1);
  Log('Starting Test',mtInformation);
  fRegistry.Run;

end;

procedure TMainForm.Log(const aMessage: String; aType: TMsgDlgType);
var
  i, j: integer;
  aStyle: TFontParams;
  lMessage: String;
begin
  lMessage := FormatDateTime('yyyy"-"mm"-"dd" "hh":"mm":"ss',Now) + ': ' + aMessage;
  i := Length(OutputMemo.Lines.Text);
  if Length(LineEnding) = 2 then
    {%H-}i := i - - OutputMemo.Lines.Count;// cr as #10#13 is counted only once so subtract it once
  j := Length(lMessage) + 1; // +1 to make the cr the same format
  OutputMemo.Lines.Add(lMessage);
  // get the 'default' style
  if OutputMemo.GetTextAttributes(0,aStyle{%H-}) then
  begin
    case aType of
      mtError:
      begin
        aStyle.Color := clRed;
        aStyle.Style := [fsBold];
      end;
      mtWarning:
        aStyle.Color := clYellow;
    end;
    OutputMemo.SetTextAttributes(i, j, aStyle);  //apply now
  end;
end;

initialization

end.

