unit cm_gui_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynEditHighlighterFoldBase;

type

  { TMarkdownEditor }

  TCommonMarkEditor = class(TSynEdit)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Graphics;



{ TMarkdownEditor }

constructor TCommonMarkEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // TODO: Do these need to be assigned from a style set of some sort?
  // TODO: The font, at least, should be configurable.
  Color := clWindow;
  Font.Color := clWindowText;
  Font.Name:='Courier 10 Pitch';
  // TODO: Also need to change some of the control colors:
  // SelectedColor
  // IncrementColor
  // HighlightAllColor
  // BracketHighlightStyle
  // BracketMatchColor
  // FoldedCodeColor
  // MouseLinkColor
  // LineHighlightColor
  // SynLeftGutterPartList...SynGutterLineNumber.MarkupInfo
  // SynLeftGutterPartList...SynGutterChanges.ModifiedColor
  // SynLeftGutterPartList...SynGutterChanges.SavedColor
  // SynLeftGutterPartList...SynGutterSeparator.MarkupInfo
  // SynLeftGutterPartList...SynGutterCodeFolding.MarkupInfo
  // TODO: And then, the highlighter colors, which I can't figure out...
end;

end.

