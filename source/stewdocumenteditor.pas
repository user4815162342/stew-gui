unit stewdocumenteditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, stewproject,
  steweditorframe;

type

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
    { private declarations }
  protected
    procedure SetDocument(AValue: TDocumentID); override;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  stewmainform, ComCtrls;

{$R *.lfm}

{ TDocumentEditor }

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

