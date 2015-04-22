unit stewproperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewpersist;

type

  { TProjectProperties }
  TCategoryDefinition = class(TJSONStoreCollectionItem)

  end;

  { TCategoryDefinitions }

  TCategoryDefinitions = class(TJSONStoreCollection)
  public
    constructor Create;
  end;

  TProjectProperties = class(TJSONAsyncFileStoreContainer)
  private
    Fcategories: TCategoryDefinitions;
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
    destructor Destroy; override;
    class function GetPath(aFolderPath: TFilename): TFilename;
  published
    property defaultDocExtension: String read FdefaultDocExtension write SetdefaultDocExtension;
    property defaultThumbnailExtension: String read FdefaultThumbnailExtension write SetdefaultThumbnailExtension;
    property defaultNotesExtension: String read FdefaultNotesExtension write SetdefaultNotesExtension;
    property categories: TCategoryDefinitions read Fcategories;
    //
{
UserProperty,
CategoryDefinitions,
StatusDefinitions,
Editors,
}
  end;

implementation

{ TCategoryDefinitions }

constructor TCategoryDefinitions.Create;
begin
  inherited Create(TCategoryDefinition);
end;

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
  Fcategories := TCategoryDefinitions.Create;
  Fcategories.FPOAttachObserver(Self);
end;

destructor TProjectProperties.Destroy;
begin
  Fcategories.FPODetachObserver(Self);
  FreeAndNil(Fcategories);
  inherited Destroy;
end;

class function TProjectProperties.GetPath(aFolderPath: TFilename): TFilename;
begin
  result := IncludeTrailingPathDelimiter(aFolderPath) + '_stew.json';
end;

end.

