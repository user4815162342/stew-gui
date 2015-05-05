unit stewpreferenceseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, steweditorframe;
// TODO: Show application preferences (some of the stuff in the config
// file, although not things like UI locations).
// - Some things to show here:
//   - Whether to edit each of the following extensions in built-in editors
//     or in an outside application.
//     - .rtf, .txt, .md/.markdown
//     - Possibly, it might be better to supply a list of built-in internal editor
//       capabilities and then have the user specify a list of file extensions
//       to open with that editor.
//   - Possibly, a list of commands to use to open up certain files when editing
//     them externally. I'm loathe to do this, because I don't want to get
//     involved in that game, but I know there are some situations where the
//     default application is a viewer, not an editor, and therefore not useful.
//   - Also, default command locations for things like publishing and other tools,
//     since these won't necessarily be available on the path on certain operating
//     systems, and might want to be configured differently for others.
// - Note that at one point I had some of this stuff done a per-project basis,
//   and it's not a bad idea to do that there. However, I could simply duplicate
//   the functionality and have this as a default.

type

  { TApplicationPreferencesEditor }

  TApplicationPreferencesEditor = class(TEditorFrame)
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TApplicationPreferencesEditor }

constructor TApplicationPreferencesEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := 'Preferences';
end;

end.

