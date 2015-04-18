unit steweditorframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, ExtCtrls, stewproject;

// TODO: This is where we set up the common look and feel for the documents,
// preferences and project settings editors.

{
TODO: Similar 'layout' system as the main form, although this isn't done here,
we just have the functionality for it.

Unlike MainForm, we don't have sidebars, footers, etc. We just have one location,
and frames can be stacked (so we can have things align to the sides, but they
won't have splitters). There is also an option to make the interface 'Tabbed',
with tabs across the bottom, and if so, then laying out a frame requires a tab name
(this isn't the caption, necessarily, as that would make it more difficult to
translate the UI.

function LayoutFrame(AControl: TControlClass; aLocation: TAlign; aTabIndex: Integer; aTabCaption: String): TControl;
- If the tab doesn't exist, creates it. But only if CreateTabs was called initially
and no other controls have been laid out. Otherwise, it's just placed on the main
panel.
- adds the control to the specified tab with the specified alignment. If there
is already a control at that location, no errors occur.
- the tab caption is only used if the tab hasn't already been created.

function LayoutTabs(aDefaultTabCaption: String): TControl;
- tells the form to create a tab sheet for laying out controls,
with one tab that has the specified caption.
- creates the tabsheet. This must be called before calling LayoutFrame. Calling
it after frames have been added will cause an error. Calling it more than once
will also cause an error.

function AddToolButton(): TToolButton;

}

// TODO: Also add a 'AddToolbutton' method to add special toolbuttons
// to the main toolbar.
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

