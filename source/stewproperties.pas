unit stewproperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewpersist;

type

  { TProjectProperties }

  TProjectProperties = class(TAsyncFileBackedStore)
  protected
    procedure Clear; override;
  public
    constructor Create(aProjectPath: TFilename);
    class function GetPath(aFolderPath: TFilename): TFilename;
  end;

implementation

{ TProjectProperties }

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

