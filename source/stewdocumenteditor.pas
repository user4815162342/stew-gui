unit stewdocumenteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls,
  stewproject, steweditorframe;

type

  // TODO: Instead of a DocumentProperties, we need a DocumentMetadata,
  // that would contain the properties object itself, as well as the 'files'
  // available. This would allow us to quickly figure out what files are
  // available, and for that matter, I'm going to be reading them anyway when
  // I do a 'listdocuments' command. I just have to rework that to not hide
  // the specific files.

{
  - Directory Index? Although that might be managed in another way.
  - Status
  - Category
  - Publish/Title
  - References?
  - Tags?
  - Notes
  - Synopsis
  - Thumbnails
  - Backups
}

  { TDocumentEditor }

  TDocumentEditor = class(TEditorFrame)
    StatusLabel: TLabel;
    SaveButton: TToolButton;
    RefreshButton: TToolButton;
    procedure RefreshButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    { private declarations }
  protected
    procedure SetDocument(AValue: TDocumentID); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  stewmainform;

{$R *.lfm}

{ TDocumentEditor }

procedure TDocumentEditor.SaveButtonClick(Sender: TObject);
begin
  // TODO:

end;

procedure TDocumentEditor.RefreshButtonClick(Sender: TObject);
begin
  // TODO:

end;

procedure TDocumentEditor.SetDocument(AValue: TDocumentID);
var
  aName: String;
begin
  if Document <> AValue then
  begin
    inherited SetDocument(aValue);
    StatusLabel.Caption := AValue;
    aName := MainForm.Project.GetBaseName(AValue);
    if (Parent <> nil) and (Parent is ttabsheet) then
    begin
      Parent.Caption := aName;
    end;
  end;
end;

constructor TDocumentEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // force the LCL to create a unique name of it's own.
  Name := '';

end;

end.

