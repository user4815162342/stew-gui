unit gui_glyphs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  TStewButtonGlyph = (
    sbgUnknown,
    sbgNew,
    sbgOpen,
    sbgPreferences,
    sbgProperties,
    sbgQuit,
    sbgAbout,
    sbgNewSibling,
    sbgNewChild,
    sbgEditName,
    sbgMoveUp,
    sbgMoveDown,
    sbgDelete,
    sbgClose,
    sbgSave,
    sbgRefresh,
    sbgCreatePrimary,
    sbgEditPrimary,
    sbgCreateNotes,
    sbgEditNotes,
    sbgNewJSONObject,
    sbgNewJSONList,
    sbgNewJSONBoolean,
    sbgNewJSONNumber,
    sbgNewJSONString
  );

function GetStewButtonIcon(aID: TStewButtonGlyph; aRequestedWidth: Longint): TCustomBitmap; platform;


implementation

uses
  gui_gtk2_glyphs, buttons, LCLType;


function GetStewButtonIcon(aID: TStewButtonGlyph; aRequestedWidth: Longint): TCustomBitmap;
var
  aBitmap: HBitMap;
  aMask: HBitMap;

begin
  aBitmap := 0;
  aMask := 0;
{$IFDEF LCLGTK2}
  Gtk2GetStewButtonIcon(aID,aRequestedWidth,aBitmap,aMask);
{$ELSE}
{$ERROR Required code is not yet written for this platform.}
{$ENDIF}
  if aBitmap <> 0 then
  begin;
    result := TBitmap.Create;
    result.Handle := aBitmap;
    if aMask <> 0 then
       result.MaskHandle := aMask;

  end
  else
  begin
    result := TBitmap.Create;
    result.Width := aRequestedWidth;
    result.Height := aRequestedWidth;
    with result.Canvas do
    begin
      Brush.Color := clWindow;
      Pen.Color := clWindow;
      FillRect(0,0,aRequestedWidth,aRequestedWidth);
      // now, start drawing the glyph
      Pen.Width := 1;

      // the shadow
      Brush.Color := clBtnShadow;
      Pen.Color := clBtnShadow;
      Rectangle(0,0,aRequestedWidth,aRequestedWidth);

    end;

  end;
end;

end.

