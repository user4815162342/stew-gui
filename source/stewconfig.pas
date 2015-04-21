unit stewconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewpersist;

type

  { TMainWindowConfig }

  TMainWindowConfig = class(TConventionallyParentedStore)
  private
    FBottomPaneHeight: Integer;
    FHeight: Integer;
    FLeftPaneWidth: Integer;
    FMaximized: Boolean;
    FRightPaneWidth: Integer;
    FTopPaneHeight: Integer;
    FWidth: Integer;
    const DefaultWidth: Integer = 600;
    const DefaultHeight: Integer = 450;
    const DefaultVerticalPaneWidth: Integer = 200;
    const DefaultHorizontalPaneHeight: Integer = 50;
    procedure SetBottomPaneHeight(AValue: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetLeftPaneWidth(AValue: Integer);
    procedure SetMaximized(AValue: Boolean);
    procedure SetRightPaneWidth(AValue: Integer);
    procedure SetTopPaneHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected
    procedure Clear; override;
  public
    constructor Create(aParent: TStore);
  published
    property height: Integer read FHeight write SetHeight;
    property width: Integer read FWidth write SetWidth;
    property maximized: Boolean read FMaximized write SetMaximized;
    property leftPaneWidth: Integer read FLeftPaneWidth write SetLeftPaneWidth;
    property rightPaneWidth: Integer read FRightPaneWidth write SetRightPaneWidth;
    property topPaneHeight: Integer read FTopPaneHeight write SetTopPaneHeight;
    property bottomPaneHeight: Integer read FBottomPaneHeight write SetBottomPaneHeight;
  end;

  { TStewApplicationConfig }

  TStewApplicationConfig = class(TFilebackedStore)
  private
    fMainWindowConfig: TMainWindowConfig;
    fMRUProjects: TStrings;
    const MRUListMaxSize = 20;
    function GetMRUProject: TFilename;
    procedure SetMRUProject(AValue: TFilename);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    property MRUProject: TFilename read GetMRUProject write SetMRUProject;
  published
    property mainWindow: TMainWindowConfig read fMainWindowConfig;
    property mruProjects: TStrings read fMRUProjects write fMRUProjects;
  end;


implementation

uses
  jsonparser, FileUtil;

{ TStewApplicationConfig }

procedure TStewApplicationConfig.SetMRUProject(AValue: TFilename);
var
  index: Integer;
begin
  index := fMRUProjects.IndexOf(AValue);
  if index > -1 then
    fMRUProjects.Delete(index);
  fMRUProjects.Insert(0,AValue);
  while fMRUProjects.Count > MRUListMaxSize do
    fMRUProjects.Delete(MRUListMaxSize);
end;

function TStewApplicationConfig.GetMRUProject: TFilename;
begin
  if fMRUProjects.Count > 0 then
     result := fMRUProjects[0]
  else
    result := '';
end;

constructor TStewApplicationConfig.Create;
begin
  inherited Create(GetAppConfigDir(false) + 'config.json');
  // TODO: Need to set a way to mark it modified when these things change.
  fMainWindowConfig := TMainWindowConfig.Create(Self);
  FMRUProjects := TConventionallyParentedStringList.Create(Self);
end;

destructor TStewApplicationConfig.Destroy;
begin
  FreeAndNil(fMRUProjects);
  FreeAndNil(fMainWindowConfig);
  inherited Destroy;
end;

procedure TStewApplicationConfig.Clear;
begin
  fMainWindowConfig.Clear;
  fMRUProjects.Clear;
end;

{ TMainWindowConfig }

procedure TMainWindowConfig.SetBottomPaneHeight(AValue: Integer);
begin
  if FBottomPaneHeight=AValue then Exit;
  FBottomPaneHeight:=AValue;
  SetModified;
end;

procedure TMainWindowConfig.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  SetModified;
end;

procedure TMainWindowConfig.SetLeftPaneWidth(AValue: Integer);
begin
  if FLeftPaneWidth=AValue then Exit;
  FLeftPaneWidth:=AValue;
  SetModified;
end;

procedure TMainWindowConfig.SetMaximized(AValue: Boolean);
begin
  if FMaximized=AValue then Exit;
  FMaximized:=AValue;
  SetModified;
end;

procedure TMainWindowConfig.SetRightPaneWidth(AValue: Integer);
begin
  if FRightPaneWidth=AValue then Exit;
  FRightPaneWidth:=AValue;
  SetModified;
end;

procedure TMainWindowConfig.SetTopPaneHeight(AValue: Integer);
begin
  if FTopPaneHeight=AValue then Exit;
  FTopPaneHeight:=AValue;
  SetModified;
end;

procedure TMainWindowConfig.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  SetModified;
end;

procedure TMainWindowConfig.Clear;
begin
  FMaximized := false;
  FWidth := DefaultWidth;
  FHeight := DefaultHeight;
  FLeftPaneWidth := DefaultVerticalPaneWidth;
  FRightPaneWidth := DefaultVerticalPaneWidth;
  FBottomPaneHeight:= DefaultHorizontalPaneHeight;
  FTopPaneHeight := DefaultHorizontalPaneHeight;
end;

constructor TMainWindowConfig.Create(aParent: TStore);
begin
  inherited Create(aParent);
end;


end.

