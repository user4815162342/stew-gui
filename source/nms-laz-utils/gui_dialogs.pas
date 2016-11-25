unit gui_dialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, sys_types;


function ButtonDialog(const aTitle: String;
  const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aButtons: TMsgDlgButtons = [mbOk];
  aButtonCaptions: array of String): Integer;
function ButtonDialog(const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aButtons: TMsgDlgButtons = [mbOk];
  aButtonCaptions: array of String): Integer;

procedure MessageDialog(const aTitle: String;
  const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aAcceptCaption: String = 'Got it');
procedure MessageDialog(const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aAcceptCaption: String = 'Got it');
procedure AboutDialog;
function ChoiceDialog(const aTitle: String; const aCaption: String; const aChoices: TStringArray; var aChoice: Integer): Boolean;
function ChoiceDialog(const aCaption: String; const aChoices: TStringArray; var aChoice: Integer): Boolean;




implementation

uses
  Forms, Buttons, StdCtrls, sys_versionsupport, gui_listdialog;

type
  // This is used as a trick in order to access a protected method on the buttons
  // in message dialog. This is actually a case where that protected method, which
  // really doesn't make any changes to the object, doesn't need to be protected
  // in my opinion, so I'm willing to do this.
  TButtonAccess = class(TBitBtn);



function ButtonDialog(const aTitle: String;
  const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aButtons: TMsgDlgButtons = [mbOk];
  aButtonCaptions: array of String): Integer;
var
  lMsgdlg: TForm;
  i: Integer;
  lButton: TBitBtn;
  lCaptionindex: Integer;
  lLastButton: TBitBtn;
  lButtonWidth: Integer = 0;
  lButtonHeight: Integer = 0;
begin
  // inspired by: http://stackoverflow.com/a/18620157/300213
  // Except that was written for delphi, and lazarus seems to work differently...
  // Basically, we have to assign different captions after creating the
  // dialog itself. In the stack overflow comment, they just assign the captions.
  // However, in Lazarus, that does not change the button widths, and the captions
  // get cut off. So, I have to do that, as well as align the buttons appropriately.

  lMsgdlg := createMessageDialog(aMessage, aDlgType, aButtons);
  try
     lMsgdlg.Caption := aTitle;
     lMsgdlg.BiDiMode := Application.BidiMode;
     lCaptionindex := Length(aButtonCaptions) - 1;
     lLastButton := nil;

     // Basically, we're working backwards, because we need to justify
     // the buttons to the right (although if BiDiMode is right to left,
     // do I need to change that?), which means that I need to start with
     // the rightmost button (which is the last button added) and go on to
     // the left.
     for i := lMsgdlg.componentcount - 1 downto 0 Do
     begin
       if (lMsgdlg.components[i] is TCustomButton) then
       Begin
         lButton := TBitBtn(lMsgdlg.components[i]);
         if lCaptionindex >= 0 then
         begin
           // Yes, No
           lButton.Caption := aButtonCaptions[lCaptionindex];
           TButtonAccess(lButton).CalculatePreferredSize(lButtonWidth,lButtonHeight,true);
           lButton.Width := lButtonWidth + 10;
           if lLastButton <> nil then
           begin
             lButton.Left := (lLastButton.Left - 10) - lButton.Width;
           end
           else
           begin
             lButton.Left := (lMsgdlg.ClientWidth - 10) - lButton.Width;
           end;

           lLastButton := lButton;
         end;
         dec(lCaptionindex);
       end
     end;
     Result := lMsgdlg.Showmodal;

  finally
    lMsgdlg.Free;
  end;
end;

function ButtonDialog(const aMessage: String; aDlgType: TMsgDlgType;
  aButtons: TMsgDlgButtons; aButtonCaptions: array of String): Integer;
begin
  result := ButtonDialog(Application.Title,aMessage,aDlgType,aButtons,aButtonCaptions);
end;

procedure MessageDialog(const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aAcceptCaption: String = 'Got it');
begin
  MessageDialog(Application.Title,aMessage,aDlgType,aAcceptCaption);
end;

procedure AboutDialog;
var
  lAboutText: TAboutText;
begin
  lAboutText := GetAboutText(Application);
  MessageDialog(lAboutText.Title,lAboutText.Copyright + LineEnding +
                               lAboutText.Description + LineEnding +
                               lAboutText.BuildInfo,mtCustom,'Thanks for Using This Program!');
end;

procedure MessageDialog(const aTitle: String;
  const aMessage: String;
  aDlgType: TMsgDlgType = mtInformation;
  aAcceptCaption: String = 'Got it');
begin
  ButtonDialog(aTitle,aMessage,aDlgType,[mbOK],[aAcceptCaption]);
end;

function ChoiceDialog(const aTitle: String; const aCaption: String;
  const aChoices: TStringArray; var aChoice: Integer): Boolean;
begin
  with TListDialog.Create(nil) do
  try
    Caption := aTitle;
    Message := aCaption;
    Choices := aChoices;
    Choice := aChoice;
    Result := Execute;
    if Result then
       aChoice := Choice;
  finally
    FreeOnRelease;
  end;
end;

function ChoiceDialog(const aCaption: String; const aChoices: TStringArray;
  var aChoice: Integer): Boolean;
begin
  result := ChoiceDialog(Application.Title,aCaption,aChoices,aChoice);
end;


end.

