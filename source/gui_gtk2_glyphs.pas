unit gui_gtk2_glyphs;

{$mode objfpc}{$H+}

interface
{$IFDEF LCLGTK2}

uses
  Classes, SysUtils, Graphics, gui_glyphs, gtk2, LCLType;

const IconMap: array[TGUIButtonGlyph] of String =
(
// https://docs.google.com/spreadsheets/d/1HavJQRPpMuq-N0GoN1wJR-9KEGXpKy3-NEPpZZkUGJY/pub?output=html
'image-missing',
'document-new',
'document-open',
'preferences-system',
'document-properties',
'application-exit',
'help-about',
'document-new',
'document-new',
'gtk-edit', // TODO: migration guide does not provide an appropriate icon.
'go-up',
'go-down',
'edit-delete',
'window-close',
'document-save',
'view-refresh',
'document-new',
'gtk-edit',
'document-new',
'gtk-edit',
'list-add',
'list-add',
'list-add',
'list-add',
'list-add'
);

function Gtk2GetGUIButtonIcon(aID: TGUIButtonGlyph; aRequestedSize: Longint; out oBitmap: HBitmap; out oMask: HBitmap): Boolean;

{$ENDIF}
implementation
{$IFDEF LCLGTK2}

uses
  Gtk2Def,
  gdk2pixbuf,
  Gtk2Proc,
  Gtk2Int,
  gdk2,
  glib2;

function Gtk2GetGUIButtonIcon(aID: TGUIButtonGlyph; aRequestedSize: Longint; out oBitmap: HBitmap; out oMask: HBitmap): Boolean;
var
  GDIObj: PGDIObject;
  StockName: PChar;
  //Style: PGtkStyle;
  //IconSet: PGtkIconSet;
  lPixbuf: PGDKPixbuf;
  lIconTheme: PGtkIconTheme;
  lPError: PGError;
  //lState: TGtkStateType;
begin
  result := false;
  StockName := PChar(IconMap[aID]);

  {
  The old way:
  Style := GetStyle(lgsButton);

  if (Style = nil) or (not GTK_IS_STYLE(Style)) then
  begin
    oBitmap := 0;
    oMask := 0;
    Exit;
  end;

  IconSet := gtk_style_lookup_icon_set(Style, StockName);

  if (IconSet = nil) then
  begin
    oBitmap := 0;
    oMask := 0;
    Exit;
  end;

  if aDisabled then
     lState := GTK_STATE_INSENSITIVE
  else
     lState := GTK_STATE_NORMAL;

  lPixbuf := gtk_icon_set_render_icon(IconSet, Style,
      GTK_TEXT_DIR_NONE, lState, GTK_ICON_SIZE_BUTTON, GetStyleWidget(lgsbutton), nil);}
  lIconTheme := gtk_icon_theme_get_default;
  lPError := nil;
  lPixbuf := gtk_icon_theme_load_icon(lIconTheme,StockName,aRequestedSize,GTK_ICON_LOOKUP_USE_BUILTIN,@lPError);

  if lPixbuf <> nil then
  begin
    GDIObj := Gtk2Widgetset.NewGDIObject(gdiBitmap);
    with GDIObj^ do
    begin
      GDIBitmapType := gbPixbuf;
      visual := gdk_visual_get_system();
      gdk_visual_ref(visual);
      colormap := gdk_colormap_get_system();
      gdk_colormap_ref(colormap);
      GDIPixbufObject := lPixbuf;
    end;

    oBitmap := HBitmap({%H-}PtrUInt(GDIObj));
  end
  else
  begin
    raise Exception.Create('Error loading stock icon: ' + lPError^.message);
    oBitmap := 0;
  end;

  oMask := 0;
  Result := True;
end;

procedure ListIcons;
var
  lList: PGList;
  lItem: PGList;
  lIconTheme: PGtkIconTheme;
begin
  lIconTheme := gtk_icon_theme_get_default;
  lList := gtk_icon_theme_list_icons(lIconTheme,nil);
  try
    lItem := lList;
    while lItem <> nil do
    begin
      WriteLn(PChar(lItem^.data));
      g_free(lItem^.data);
      lItem := lItem^.next;
    end;

  finally
    g_list_free(lList);
  end;

end;

initialization

{$ENDIF}
end.

