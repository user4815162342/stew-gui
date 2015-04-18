unit stewconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, stewjson;

const
  MRUListMaxSize = 20;

type

  { TMainWindowConfig }

  TMainWindowConfig = class(TParentedJSONObject)
  private
  protected
    function IsManaged(aPropertyName: String): boolean; override;
    {%H-}constructor Create(aParent: TManagedJSONObject; aParentProperty: String); overload;
  public
    destructor Destroy; override;
    function GetHeight(aDefault: Integer): Integer;
    function GetMaximized(aDefault: Boolean): Boolean;
    function GetWidth(aDefault: Integer): Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetMaximized(AValue: Boolean);
    procedure SetWidth(AValue: Integer);
    function GetLeftPaneWidth(aDefault: Integer): Integer;
    procedure SetLeftPaneWidth(aValue: Integer);
    function GetRightPaneWidth(aDefault: Integer): Integer;
    procedure SetRightPaneWidth(aValue: Integer);
    function GetTopPaneHeight(aDefault: Integer): Integer;
    procedure SetTopPaneHeight(aValue: Integer);
    function GetBottomPaneHeight(aDefault: Integer): Integer;
    procedure SetBottomPaneHeight(aValue: Integer);
  end;

  { TStewApplicationConfig }

  TStewApplicationConfig = class(TFileBackedJSONObject)
  private
    fMainWindowConfig: TMainWindowConfig;
    function GetMRUProject: TFilename;
    procedure SetMRUProject(AValue: TFilename);
  protected
    function IsManaged(aPropertyName: String): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    property MainWindow: TMainWindowConfig read fMainWindowConfig;
    property MRUProject: TFilename read GetMRUProject write SetMRUProject;
  end;

implementation

uses
  jsonparser, FileUtil, math, LCLProc;

{ TStewApplicationConfig }

function TStewApplicationConfig.GetMRUProject: TFilename;
var
  List: TJSONArray;
begin
  List := Find('mruProjects',jtArray) as TJSONArray;
  if (list <> nil) and (list.Count > 0) then
  begin
     result := list[0].AsString;
  end;
end;

procedure TStewApplicationConfig.SetMRUProject(AValue: TFilename);
var
  List: TJSONArray;
  high: Integer;
  i: Integer;
  found: Boolean;
begin
  List := Find('mruProjects',jtArray) as TJSONArray;
  if (list = nil) then
  begin
     List := CreateManagedArray('mruProjects');
  end;

  high := Min(MRUListMaxSize - 1,List.Count - 1);
  found := false;
  for i := 0 to high do
  begin
    if List[i].AsString = AValue then
    begin
       found := true;
       if (i > 0) then
       begin
          // delete the old one, and put it on top.
          List.Delete(i);
          List.Insert(0,AValue);
          SetModified;
       end;
       // we've found it, so we don't have to look any further.
       break;
    end;
  end;

  if not found then
  begin
    List.Insert(0,AValue);
    SetModified;
  end;

  // now, trim off the end of the MRUList.
  while List.Count > MRUListMaxSize do
  begin
    List.Delete(MRUListMaxSize);
    SetModified;
  end;



end;


function TStewApplicationConfig.IsManaged(aPropertyName: String): boolean;
begin
  case aPropertyName of
    'mruProjects','mainWindow':
       result := true;
  else
    result := inherited IsManaged(aPropertyName);
  end;
end;

constructor TStewApplicationConfig.Create;
begin
  inherited Create(GetAppConfigDir(false) + 'config.json');
  fMainWindowConfig := TMainWindowConfig.Create(Self,'mainWindow');
end;

destructor TStewApplicationConfig.Destroy;
begin
  FreeAndNil(fMainWindowConfig);
end;

{ TMainWindowConfig }

function TMainWindowConfig.IsManaged(aPropertyName: String): boolean;
begin
  case aPropertyName of
    'height','width','maximized':
       result := true;
  else
    result := inherited IsManaged(aPropertyName);
  end;
end;

constructor TMainWindowConfig.Create(aParent: TManagedJSONObject;
  aParentProperty: String);
begin
  inherited Create(aParent,aParentProperty);
end;

destructor TMainWindowConfig.Destroy;
begin
  inherited Destroy;
end;

function TMainWindowConfig.GetHeight(aDefault: Integer): Integer;
begin
  result := FindOrDefault('height',aDefault);
end;

function TMainWindowConfig.GetMaximized(aDefault: Boolean): Boolean;
begin
  result := FindOrDefault('maximized',aDefault);
end;

function TMainWindowConfig.GetWidth(aDefault: Integer): Integer;
begin
  result := FindOrDefault('width',aDefault);
end;

procedure TMainWindowConfig.SetHeight(AValue: Integer);
begin
  SetManagedValue('height',AValue);
end;

procedure TMainWindowConfig.SetMaximized(AValue: Boolean);
begin
  SetManagedValue('maximized',AValue);
end;

procedure TMainWindowConfig.SetWidth(AValue: Integer);
begin
  SetManagedValue('width',AValue);
end;

function TMainWindowConfig.GetLeftPaneWidth(aDefault: Integer): Integer;
begin
  result := FindOrDefault('leftPaneWidth',aDefault);
end;

procedure TMainWindowConfig.SetLeftPaneWidth(aValue: Integer);
begin
  SetManagedValue('leftPaneWidth',aValue);
end;

function TMainWindowConfig.GetRightPaneWidth(aDefault: Integer): Integer;
begin
  result := FindOrDefault('rightPaneWidth',aDefault);
end;

procedure TMainWindowConfig.SetRightPaneWidth(aValue: Integer);
begin
  SetManagedValue('rightPaneWidth',aValue);
end;

function TMainWindowConfig.GetTopPaneHeight(aDefault: Integer): Integer;
begin
  result := FindOrDefault('topPaneHeight',aDefault);
end;

procedure TMainWindowConfig.SetTopPaneHeight(aValue: Integer);
begin
  SetManagedValue('topPaneHeight',aValue);

end;

function TMainWindowConfig.GetBottomPaneHeight(aDefault: Integer): Integer;
begin
  result := FindOrDefault('bottomPaneHeight',aDefault);
end;

procedure TMainWindowConfig.SetBottomPaneHeight(aValue: Integer);
begin
  SetManagedValue('bottomPaneHeight',aValue);

end;


end.

