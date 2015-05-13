unit gui_listdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, stew_types;

type

  { TListDialog }

  TListDialog = class(TForm)
    AcceptButton: TButton;
    CancelButton: TButton;
    CaptionLabel: TLabel;
    ChoiceList: TListBox;
  private
    function GetCaption: String;
    function GetChoice: String;
    function GetChoices: TStringArray;
    procedure SetCaption(AValue: String);
    procedure SetChoice(AValue: String);
    procedure SetChoices(AValue: TStringArray);
    { private declarations }
  public
    { public declarations }
    property Choices: TStringArray read GetChoices write SetChoices;
    property Choice: String read GetChoice write SetChoice;
    property Caption: String read GetCaption write SetCaption;
    function Execute: Boolean;
  end;

  function ChoiceQuery(aCaption: String; aChoices: TStringArray; var aChoice: String): Boolean;

var
  ListDialog: TListDialog;

implementation

function ChoiceQuery(aCaption: String; aChoices: TStringArray;
  var aChoice: String): Boolean;
begin
  with TListDialog.Create(nil) do
  try
    Caption := aCaption;
    Choices := aChoices;
    Choice := aChoice;
    Result := Execute;
    if Result then
       aChoice := Choice;
  finally
    FreeOnRelease;
  end;
end;

{$R *.lfm}

{ TListDialog }

function TListDialog.GetCaption: String;
begin
  result := CaptionLabel.Caption;
end;

function TListDialog.GetChoice: String;
begin
  result := ChoiceList.Items[ChoiceList.ItemIndex];
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

procedure TListDialog.SetCaption(AValue: String);
begin
  CaptionLabel.Caption := AValue;
end;

procedure TListDialog.SetChoice(AValue: String);
var
  idx: Integer;
begin
  idx := ChoiceList.Items.IndexOf(AValue);
  if idx > - 1 then
     ChoiceList.ItemIndex := idx;

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

