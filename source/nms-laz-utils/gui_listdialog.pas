unit gui_listdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, sys_types;

type

  { TListDialog }

  TListDialog = class(TForm)
    AcceptButton: TButton;
    CancelButton: TButton;
    MessageLabel: TLabel;
    ChoiceList: TListBox;
  strict private
    function GetMessage: String;
    function GetChoice: Integer;
    function GetChoices: TStringArray;
    procedure SetMessage(AValue: String);
    procedure SetChoice(AValue: Integer);
    procedure SetChoices(AValue: TStringArray);
    { private declarations }
  public
    { public declarations }
    property Choices: TStringArray read GetChoices write SetChoices;
    property Choice: Integer read GetChoice write SetChoice;
    property Message: String read GetMessage write SetMessage;
    function Execute: Boolean;
  end;


implementation

{$R *.lfm}

{ TListDialog }

function TListDialog.GetMessage: String;
begin
  result := MessageLabel.Caption;
end;

function TListDialog.GetChoice: Integer;
begin
  result := ChoiceList.ItemIndex;
end;

function TListDialog.GetChoices: TStringArray;
var
  i: Integer;
  l: Integer;
begin
  l := ChoiceList.Items.Count;
  SetLength(Result,l);
  for i := 0 to l - 1 do
    Result[i] := ChoiceList.Items[i];

end;

procedure TListDialog.SetMessage(AValue: String);
begin
  MessageLabel.Caption := AValue;
end;

procedure TListDialog.SetChoice(AValue: Integer);
begin
  if (AValue < ChoiceList.Items.Count) then
     ChoiceList.ItemIndex := AValue
  else
     ChoiceList.ItemIndex := -1;

end;

procedure TListDialog.SetChoices(AValue: TStringArray);
var
  i: Integer;
  l: Integer;
begin
  l := Length(AValue);
  ChoiceList.Clear;
  for i := 0 to l - 1 do
    ChoiceList.Items.Add(AValue[i]);
end;

function TListDialog.Execute: Boolean;
begin
  result := (ShowModal = mrOK) and (ChoiceList.ItemIndex > -1);
end;

end.

