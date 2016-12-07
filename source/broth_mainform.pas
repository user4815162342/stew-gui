unit broth_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Menus, brothmark_gui_editor, brothmark_gui_highlighter, SynEditHighlighter;

type
  { The goal of this application is to be a simple one-window-per-file
    editor for BrothMark Markdown files, and a test bed for those controls.
    The controls created here should be easy to pull into Stew so that it looks
    like its embedding the functionality from broth onto tabs to edit markdown
    documents.
  }

  // TODO: Okay.... Here's what I need to create:
  // - BrothMarkEditor: A SynEditor that manages itself as a decent editor with
  //   appropriate default behavior for editing Markdown for novels. It also provides
  //   default styles for displaying the text. And, it contains a BrothMarkMap (below).
  //   (When the buffer changes, or the highlighter, the Map has to change).
  // - BrothMarkHighlighter: A SynEditFoldHighlighter that knows how to highlight
  //   Markdown in appropriate tokens, the ranges are foldable ranges and can
  //   be used to build a map (see the Navigator)
  // - BrothMarkLog: A listview or memo that reports events going on in the highlighter.
  //   This is going to be useful for working on the Navigator.
  // - BrothMarkNavigator: A TreeView that connects to a BrothMarkEditor and updates
  //   a sort of "map" of the file based on changes to the ranges made by the
  //   BrothMarkHighlighter, and the text values at the lines referenced in those
  //   ranges. The Navigator will only display if the Editor has a BrothMarkHighlighter.
  //   It should automatically reconnect itself if the Editor's buffer is switched to
  //   a different one, or if the highlighter changes (these events appear to be
  //   on the lines buffer).
  // - BrothMarkPropertyManager: A component which connects to a BrothMarkEditor,
  //   and uses the current cursor position and the Ranges created by a BrothMarkHighlighter
  //   to update properties of the current section.

  // TODO: Maintaining the navigator and the property manager:
  // The Highlighter would create FoldableRanges,
  // which can be retrieved from the range list on a SynEdit TextBuffer. (You need
  // to know what highlighter is being used to get the correct range list). These
  // ranges are a simple navigation system, and help you figure out where new
  // sections start, etc. To get the data for sections, you need to look at the
  // lines and do some RegEx Parsing. Events on the TextBuffer and the RangeList
  // will tell when these things change. The Log is necessary to get an idea of
  // the lifecycle of these events, so we know what I can use to build the navigator.


  { TMainForm }

  TMainForm = class(TForm)
    LeftSidebarPanel: TPanel;
    LeftSidebarSplitter: TSplitter;
    FooterPanel: TPanel;
    MainMenu: TMainMenu;
    MainPageControl: TPageControl;
    FileMenuItem: TMenuItem;
    FileOpenMenuItem: TMenuItem;
    FileNewMenuItem: TMenuItem;
    FileSaveMenuItem: TMenuItem;
    FileExitMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    ViewLeftSidebarMenuItem: TMenuItem;
    ViewFooterMenuItem: TMenuItem;
    ViewRightSidebarMenuItem: TMenuItem;
    SearchMenuItem: TMenuItem;
    ViewMenuItem: TMenuItem;
    ToolsMenuItem: TMenuItem;
    HelpMenuItem: TMenuItem;
    Separator1: TMenuItem;
    RightSidebarPanel: TPanel;
    RightSidebarSplitter: TSplitter;
    FooterSplitter: TSplitter;
    MainEditorTab: TTabSheet;
    MainGridTab: TTabSheet;
    procedure FileExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ViewFooterMenuItemClick(Sender: TObject);
    procedure ViewLeftSidebarMenuItemClick(Sender: TObject);
    procedure ViewRightSidebarMenuItemClick(Sender: TObject);
  private
    { private declarations }
    fBrothMarkEditor: TBrothMarkEditor;
    fBrothMarkHighlighter: TBrothMarkHighlighter; // TODO: Make this type specific
    fRightSidebarWidth: Longint;
    fLeftSidebarWidth: Longint;
    fFooterHeight: Longint;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  SynHighlighterDiff;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fBrothMarkHighlighter := TBrothMarkHighlighter.Create(Self);
  fBrothMarkEditor := TBrothMarkEditor.Create(Self);
  fBrothMarkEditor.Align := alClient;
  fBrothMarkEditor.Parent := MainEditorTab;
  // TODO: Once I figure out the speed problems, this should go here.
  //fBrothMarkEditor.Highlighter := fBrothMarkHighlighter;
end;

procedure TMainForm.FileExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // TODO: Need some basic configuration here...Load from a JSON file.
  // - Store Window Size and position
  // - Allow changing of font name and basic size.
  WindowState := wsMaximized;

  // initialize the default panel widths.
  fRightSidebarWidth := RightSidebarPanel.Width;
  fLeftSidebarWidth := LeftSidebarPanel.Width;
  fFooterHeight := FooterPanel.Height;
  // now click them to make sure they're at default
  ViewRightSidebarMenuItemClick(nil);
  ViewLeftSidebarMenuItemClick(nil);
  ViewFooterMenuItemClick(nil);

  // Now, load any files from the command line:
  if Paramcount > 0 then
     fBrothMarkEditor.Lines.LoadFromFile(ExpandFileNameUTF8(ParamStr(1)));
  fBrothMarkEditor.Highlighter := fBrothMarkHighlighter;
end;

procedure TMainForm.ViewFooterMenuItemClick(Sender: TObject);
begin
  if ViewFooterMenuItem.Checked then
  begin
    FooterPanel.Height := fFooterHeight;
  end
  else
  begin
    fFooterHeight := FooterPanel.Height;
    FooterPanel.Height := 0;
  end;
end;

procedure TMainForm.ViewLeftSidebarMenuItemClick(Sender: TObject);
begin
  if ViewLeftSidebarMenuItem.Checked then
  begin
    LeftSidebarPanel.Width := fLeftSidebarWidth;
  end
  else
  begin
    fLeftSidebarWidth := LeftSidebarPanel.Width;
    LeftSidebarPanel.Width := 0;
  end;
end;

procedure TMainForm.ViewRightSidebarMenuItemClick(Sender: TObject);
begin
  if ViewRightSidebarMenuItem.Checked then
  begin
    RightSidebarPanel.Width := fRightSidebarWidth;
  end
  else
  begin
    fRightSidebarWidth := RightSidebarPanel.Width;
    RightSidebarPanel.Width := 0;
  end;
end;

end.

