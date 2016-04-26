unit gui_editorframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, stew_project;

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


type

  { TEditorFrame }

  TEditorFrame = class(TFrame)
    CloseButton: TToolButton;
    RightHandToolbar: TToolBar;
    LeftHandToolbar: TToolBar;
    ToolbarPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  strict private
    fDocument: TDocumentPath;
  strict protected
    procedure SetDocument(AValue: TDocumentPath); virtual;
    procedure SetupGlyphs; virtual;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    function CloseQuery: Boolean; virtual;
    property Document: TDocumentPath read fDocument write SetDocument;
  end;

  TEditorFrameClass = class of TEditorFrame;

implementation

uses
  gui_mainform, sys_log;

{$R *.lfm}

{ TEditorFrame }

procedure TEditorFrame.CloseButtonClick(Sender: TObject);
const
  cMethod: String = 'TEditorFrame.CloseButtonClick';
begin
  LogAction(cMethod,Document.ID);
  MainForm.RequestTabClose(Self);
end;

procedure TEditorFrame.SetDocument(AValue: TDocumentPath);
begin
  if fDocument <> AValue then
  begin
   fDocument := AValue;
  end;
end;

procedure TEditorFrame.SetupGlyphs;
begin
  if MainForm.ApplicationImages <> nil then
  begin
    LeftHandToolbar.Images := MainForm.ApplicationImages;
    RightHandToolbar.Images := MainForm.ApplicationImages;

  end;
end;

constructor TEditorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fDocument := TDocumentPath.Null;
  SetupGlyphs;
end;

function TEditorFrame.CloseQuery: Boolean;
begin
  result := true;
end;

end.

