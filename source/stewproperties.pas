unit stewproperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewpersist;

type

  { TProjectProperties }

  TProjectProperties = class(TJSONAsyncFileStoreContainer)
  private
    FdefaultDocExtension: String;
    FdefaultNotesExtension: String;
    FdefaultThumbnailExtension: String;
    procedure SetdefaultDocExtension(AValue: String);
    procedure SetdefaultNotesExtension(AValue: String);
    procedure SetdefaultThumbnailExtension(AValue: String);
  protected
    procedure Clear; override;
  public
    constructor Create(aProjectPath: TFilename);
    class function GetPath(aFolderPath: TFilename): TFilename;
  published
    property defaultDocExtension: String read FdefaultDocExtension write SetdefaultDocExtension;
    property defaultThumbnailExtension: String read FdefaultThumbnailExtension write SetdefaultThumbnailExtension;
    property defaultNotesExtension: String read FdefaultNotesExtension write SetdefaultNotesExtension;
    //
{
UserProperty,
CategoryDefinitions,
StatusDefinitions,
Editors,
}
  end;

implementation

{ TProjectProperties }

procedure TProjectProperties.SetdefaultDocExtension(AValue: String);
begin
  if FdefaultDocExtension=AValue then Exit;
  FdefaultDocExtension:=AValue;
end;

procedure TProjectProperties.SetdefaultNotesExtension(AValue: String);
begin
  if FdefaultNotesExtension=AValue then Exit;
  FdefaultNotesExtension:=AValue;
end;

procedure TProjectProperties.SetdefaultThumbnailExtension(AValue: String);
begin
  if FdefaultThumbnailExtension=AValue then Exit;
  FdefaultThumbnailExtension:=AValue;
end;

procedure TProjectProperties.Clear;
begin
  // TODO: Initialize all of the primitive data, clear all of the child data.
end;

constructor TProjectProperties.Create(aProjectPath: TFilename);
begin
  inherited Create(GetPath(aProjectPath),false);
end;

class function TProjectProperties.GetPath(aFolderPath: TFilename): TFilename;
begin
  result := IncludeTrailingPathDelimiter(aFolderPath) + '_stew.json';
end;

end.

