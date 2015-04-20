unit stewproperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewjson;

type

  { TProjectProperties }

  TProjectProperties = class(TAsyncFileBackedJSONObject)
  public
    constructor Create(aProjectPath: TFilename);
    class function GetPath(aFolderPath: TFilename): TFilename;
  end;

implementation

{ TProjectProperties }

constructor TProjectProperties.Create(aProjectPath: TFilename);
begin
  inherited Create(GetPath(aProjectPath),false);
end;

class function TProjectProperties.GetPath(aFolderPath: TFilename): TFilename;
begin
  result := IncludeTrailingPathDelimiter(aFolderPath) + '_stew.json';
end;

end.

