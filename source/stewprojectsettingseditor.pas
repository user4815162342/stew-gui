unit stewprojectsettingseditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, steweditorframe;
// TODO: Show Project Options. This is both the "stew" file and the project root properties.
//    The possible options include:
//    - category definitions, including publishing details.
//    - status definitions
//    - default file extensions (primary, note, thumbnail)
//    - editors for certain file extensions (See Preferences Menu).
//    - "Type" of project:
//      - Notebook: In this project type, there is only a 'primary', no notes,
//        and no thumbnail, and no synopsis, and as many editors are set to
//        internal as possible. There is also no "publish" and "title" properties,
//        and the publishing stuff which might be needed in the stew file.
//      - Manuscript: In this project type, there are separate notes and things,
//        basically, everything that's always been in stew.
//    - user settings (the stew-cli stuff allows user settings both in stew and in the project root)
//    - Root Index (if allowed for editing in the documents, then it should be here too).
//    - Notes

type

  { TProjectSettingsEditor }

  TProjectSettingsEditor = class(TEditorFrame)
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TProjectSettingsEditor }

constructor TProjectSettingsEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := 'Project Settings';
end;

end.

