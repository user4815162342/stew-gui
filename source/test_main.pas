unit test_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RichMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, test_registry;

type

  {
  TODO: Working on upgrading stew project to new stuff. See test_stew_project.

  TODO: Consider some renaming and refactoring:
  - I know the whole 'T' prefix is old hat with Pascal, but it does get a little
  old after a while. I would like to consider changing *my* classes, at least the
  ones that don't descend from the LCL, to get rid of all such prefixes. I'm not
  stuck on this, though. *At least, we could get rid of them on inner classes,
  see below*. Right now, however, I've got a list of the following 'prefixes'
  which I make use of:
    "T": Indicates a type
    "G": Indicates a generic (This is non-standard, but I feel like there needs
         to be a difference between a TList and a GList<ContainerType>
         -- I should probably come up with a prefix for the type argument of
            a generic.
    "g": Indicates a global variable (yes, it could get confused with "G", but
         I rarely use global variables, I'm more likely to
    "f": Indicates a private member variable (field) of a class, record, etc.
    - The main reason for this one is to avoid naming conflicts, because of
      Pascal's case-insensitivity. In C-based languages, I can use a lower case
      to indicate the field and an upper-case to indicate the property.
    "a": Indicates an argument variable in a function/procedure/method.
    "l": Indicates a local variable in a function/procedure/method.
  - I would also like to consider making use of nested types to simplify some of
  the very long names. For instance, all of those TDoThisAndThatAndThenSomethingElseTasks,
  combined with similar Promises. Instead of TFileCacheReadFileTask, I could make
  a TReadFileTask class that's an inner type on TFileCache. Or TFileReadTask
  into TFile.ReadTask, for that matter.

  TODO: Consider keeping a "log" (this would be an application setting)
  of "actions", and their responses in the StewProject option. That will, at least,
  make it easier to debug and reproduce errors, if we can get users to reproduce
  and send the logs to us. Basically, it would tell us what order actions happened
  in order to create tests around them at the project level.

  TODO: Also, consider being able to 'plug in' a project to test certain basic operations
  with, so users can write their own tests and use actual data.
  }

  { TMainForm }

  TMainForm = class(TForm)
    MainToolbar: TToolBar;
    OutputMemo: TRichMemo;
    RunButton: TToolButton;
    CancelButton: TToolButton;
    CloseButton: TToolButton;
    RunTesterTestsButton: TToolButton;
    MainStatusBar: TStatusBar;
    TimeoutCheckButton: TToolButton;
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
    procedure TestMessage(const {%H-}aSender: TObject; const aTestName: String;
      const aMessage: String);
    procedure TestsCancelled(Sender: TObject);
    procedure TestsCompleted(Sender: TObject);
    procedure TestsException(Sender: TObject; E: Exception);
    procedure TestStarted(const {%H-}aSender: TObject; const aTestName: String);
    procedure TestSucceeded(const {%H-}aSender: TObject; const aTestName: String);
  private
    { private declarations }
    fRegistry: TTestRegistry;
    fErrorCount: Integer;
    procedure InitRegistry(aTestTester: Boolean);
    procedure StartTests({%H-}Data: PtrInt);
  public
    { public declarations }
    procedure AddMessage(const aMessage: String; aType: TMsgDlgType);
    procedure Log(const aMessage: String; aType: TMsgDlgType);
  end;

var
  MainForm: TMainForm;

implementation

uses
  test_test, test_sys_json, test_sys_async, test_sys_localfile, test_stew_properties,
  test_stew_project, test_longstringmap, test_sys_filecache_local;

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

procedure TMainForm.TestMessage(const aSender: TObject;
  const aTestName: String; const aMessage: String);
begin
  AddMessage(' ---------> ' + aTestName + ': ' + aMessage,mtInformation);
end;

procedure TMainForm.TestsCancelled(Sender: TObject);
begin
  Log('User Cancelled Tests',mtWarning);
end;

procedure TMainForm.TestsCompleted(Sender: TObject);
begin
  Log('Tests Completed',mtCustom);
  MainStatusBar.SimpleText := 'Found ' + IntToStr(fErrorCount) + ' Errors';
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
    fRegistry.OnMessage:=@TestMessage;
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
    fRegistry.AddTests(TLongStringMapSpec);
    fRegistry.AddTests(TLocalFileCacheSpec);
    fRegistry.AddTests(TPropertiesSpec);
    fRegistry.AddTests(TProjectSpec);
  end;
end;

procedure TMainForm.StartTests(Data: PtrInt);
begin
  RunButton.Enabled := false;
  RunTesterTestsButton.Enabled := false;
  CancelButton.Enabled := true;
  InitRegistry(Data = 1);
  Log('Starting Tests',mtCustom);
  MainStatusBar.SimpleText := 'Running Tests...';
  fErrorCount := 0;
  fRegistry.Run(TimeoutCheckButton.Down);

end;

procedure TMainForm.AddMessage(const aMessage: String; aType: TMsgDlgType);
var
  i, j: integer;
  aStyle: TFontParams;
begin
  i := Length(OutputMemo.Lines.Text);
  if Length(LineEnding) = 2 then
    {%H-}i := i - - OutputMemo.Lines.Count;// cr as #10#13 is counted only once so subtract it once
  j := Length(aMessage) + 1; // +1 to make the cr the same format
  OutputMemo.Lines.Add(aMessage);
  // get the 'default' style
  if OutputMemo.GetTextAttributes(0,aStyle{%H-}) then
  begin
    aStyle.Style:=[];
    case aType of
      mtError:
      begin
        aStyle.Color := clRed;
        aStyle.Style := [fsBold];
        inc(fErrorCount);
      end;
      mtWarning:
      begin
        aStyle.Color := clYellow;
      end;
      mtCustom:
      begin
        aStyle.Style := [fsBold];
      end;
    end;
    OutputMemo.SetTextAttributes(i, j, aStyle);  //apply now
  end;
  // move log down so we can watch results...
  OutputMemo.SelStart := i + j;
end;

procedure TMainForm.Log(const aMessage: String; aType: TMsgDlgType);
begin
  AddMessage(FormatDateTime('yyyy"-"mm"-"dd" "hh":"mm":"ss',Now) + ': ' + aMessage,aType);
end;

initialization

end.

