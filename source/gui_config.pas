unit gui_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stew_persist, sys_file;

type

  { TMainWindowConfig }

  TMainWindowConfig = class(TJSONStore)
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
    procedure Clear;
  public
    constructor Create;
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

  TStewApplicationConfig = class(TJSONFileStoreContainer)
  private
    fMainWindowConfig: TMainWindowConfig;
    fMRUProjects: TStrings;
    const MRUListMaxSize = 20;
    function GetMRUProject: TFile;
    procedure SetMRUProject(AValue: TFile);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    property MRUProject: TFile read GetMRUProject write SetMRUProject;
  published
    property mainWindow: TMainWindowConfig read fMainWindowConfig;
    property mruProjects: TStrings read fMRUProjects write fMRUProjects;
  end;


implementation

uses
  jsonparser, FileUtil, sys_localfile;

{ TStewApplicationConfig }

procedure TStewApplicationConfig.SetMRUProject(AValue: TFile);
var
  index: Integer;
  aID: String;
begin
  // Sometimes, we're getting the last backslash on the file in here
  // which causes duplicate entries.
  // NOTE: The TFile should be handling this itself, shouldn't it?
  aID := ExcludeTrailingPathDelimiter(AValue.ID);
  index := fMRUProjects.IndexOf(aID);
  if index > -1 then
    fMRUProjects.Delete(index);
  fMRUProjects.Insert(0,aID);
  while fMRUProjects.Count > MRUListMaxSize do
    fMRUProjects.Delete(MRUListMaxSize);
end;

function TStewApplicationConfig.GetMRUProject: TFile;
begin
  if fMRUProjects.Count > 0 then
     result := LocalFile(fMRUProjects[0])
  else
    result := LocalFile('');
end;

constructor TStewApplicationConfig.Create;
begin
  inherited Create(GetAppConfigDir(false) + 'stew-gui-config.json');
  fMainWindowConfig := TMainWindowConfig.Create;
  fMainWindowConfig.FPOAttachObserver(Self);
  FMRUProjects := TStringList.Create;
  fMRUProjects.FPOAttachObserver(Self);
end;

destructor TStewApplicationConfig.Destroy;
begin
  fMRUProjects.FPODetachObserver(Self);
  FreeAndNil(fMRUProjects);
  fMainWindowConfig.FPODetachObserver(Self);
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
  FPONotifyObservers(Self,ooChange,nil);
end;

procedure TMainWindowConfig.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  FPONotifyObservers(Self,ooChange,nil);
end;

procedure TMainWindowConfig.SetLeftPaneWidth(AValue: Integer);
begin
  if FLeftPaneWidth=AValue then Exit;
  FLeftPaneWidth:=AValue;
  FPONotifyObservers(Self,ooChange,nil);
end;

procedure TMainWindowConfig.SetMaximized(AValue: Boolean);
begin
  if FMaximized=AValue then Exit;
  FMaximized:=AValue;
  FPONotifyObservers(Self,ooChange,nil);
end;

procedure TMainWindowConfig.SetRightPaneWidth(AValue: Integer);
begin
  if FRightPaneWidth=AValue then Exit;
  FRightPaneWidth:=AValue;
  FPONotifyObservers(Self,ooChange,nil);
end;

procedure TMainWindowConfig.SetTopPaneHeight(AValue: Integer);
begin
  if FTopPaneHeight=AValue then Exit;
  FTopPaneHeight:=AValue;
  FPONotifyObservers(Self,ooChange,nil);
end;

procedure TMainWindowConfig.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  FPONotifyObservers(Self,ooChange,nil);
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

constructor TMainWindowConfig.Create;
begin
  inherited Create;
  Clear;
end;


end.

