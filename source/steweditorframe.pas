unit steweditorframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, stewproject;

// TODO: This is where we set up the common look and feel for the documents,
// preferences and project settings editors.

// NOTE: At one point I considered setting up a layout mechanism, like I do on
// the main form, and separating everything out into frames. I have abandoned that:
// - doing it the old fashioned way gives me a better preview of
// what the form is going to look like while designing it
// - separating controls into multiple frames would mean an abundance of files
// I would have to go through if I needed to tweak one little thing.
// - Since the EditorFrame has controls of it's own, which I might want to be
// customizable, I would have to add lots of additional code to handle that.
// - really, the layout of document panels is not going to be customizable, at
// least in the short run, and in the long run, I can add 'hooks' where I want
// to make things customizable.

// TODO: Need a set of standard glyphs for the toolbuttons?

type

  { TEditorFrame }

  TEditorFrame = class(TFrame)
    CloseButton: TToolButton;
    RightHandToolbar: TToolBar;
    LeftHandToolbar: TToolBar;
    ToolbarPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private
    fDocument: TDocumentID;
    { private declarations }
  protected
    procedure SetDocument(AValue: TDocumentID); virtual;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function CloseQuery: Boolean; virtual;
    property Document: TDocumentID read fDocument write SetDocument;
  end;

  TEditorFrameClass = class of TEditorFrame;

implementation

uses
  stewmainform;

{$R *.lfm}

{ TEditorFrame }

procedure TEditorFrame.CloseButtonClick(Sender: TObject);
begin
  MainForm.DocumentTabCloseRequested(Self);
end;

procedure TEditorFrame.SetDocument(AValue: TDocumentID);
begin
  if fDocument <> AValue then
  begin
   fDocument := AValue;
  end;
end;

constructor TEditorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

function TEditorFrame.CloseQuery: Boolean;
begin
  result := true;
end;

end.

