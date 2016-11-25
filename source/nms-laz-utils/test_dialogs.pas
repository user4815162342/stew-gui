unit test_dialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry;

type

  { TDialogsSpec }

  TDialogsSpec = class(TTestSpec)
  published
    procedure Test_ButtonDialog;
    procedure Test_MessageDialog;
    procedure Test_AboutDialog;
    procedure Test_ChoiceDialog;
  end;

{
function ButtonDialog(const aTitle: String;
  const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aButtons: TMsgDlgButtons = [mbOk];
  aButtonCaptions: array of String): Integer; overload;
procedure ShowMessage(const aTitle: String;
  const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aAcceptCaption: String = 'Got it');
procedure ShowMessage(const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aAcceptCaption: String = 'Got it');
procedure AboutDialog;
function ChoiceDialog(aCaption: String; aChoices: TStringArray; var aChoice: Integer): Boolean;


}


implementation

uses
  Dialogs, gui_dialogs, sys_types;

{ TDialogsSpec }

procedure TDialogsSpec.Test_ButtonDialog;
begin
  BeginInteractive;
  ButtonDialog('Button Dialog','This is a message' + #10 + 'This is a really long message. Note that the message has to be extra, extra, extra long to get all of the long button names to show up. This should be fixed.',
               mtWarning,mbYesNoCancel,['Yep!','No way.','What are you talking about?']);
end;

procedure TDialogsSpec.Test_MessageDialog;
begin
  BeginInteractive;
  MessageDialog('This is just a simple message',mtWarning,'Oh no!');
end;

procedure TDialogsSpec.Test_AboutDialog;
begin
  BeginInteractive;
  AboutDialog;
end;

procedure TDialogsSpec.Test_ChoiceDialog;
var
  lList: TStringArray;
  lResult: Longint;
begin
  BeginInteractive;
  SetLength(lList,3);
  lList[0] := 'Choice 1';
  lList[1] := 'Choice B';
  lList[2] := 'Choice III';
  lResult := 2;
  if ChoiceDialog('Choose one',lList,lResult) then
     MessageDialog(lList[lResult]);

end;

end.

