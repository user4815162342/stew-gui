unit gui_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stew_persist, sys_file, sys_json;

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

  { TMainWindowConfig2 }

  TMainWindowConfig2 = class(TJSObject)
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
    function GetBottomPaneHeight: Integer;
    function GetHeight: Integer;
    function GetLeftPaneWidth: Integer;
    function GetMaximized: Boolean;
    function GetRightPaneWidth: Integer;
    function GetTopPaneHeight: Integer;
    function GetWidth: Integer;
    procedure SetBottomPaneHeight(AValue: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetLeftPaneWidth(AValue: Integer);
    procedure SetMaximized(AValue: Boolean);
    procedure SetRightPaneWidth(AValue: Integer);
    procedure SetTopPaneHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    constructor Create;
  published
    property height: Integer read GetHeight write SetHeight;
    property width: Integer read GetWidth write SetWidth;
    property maximized: Boolean read GetMaximized write SetMaximized;
    property leftPaneWidth: Integer read GetLeftPaneWidth write SetLeftPaneWidth;
    property rightPaneWidth: Integer read GetRightPaneWidth write SetRightPaneWidth;
    property topPaneHeight: Integer read GetTopPaneHeight write SetTopPaneHeight;
    property bottomPaneHeight: Integer read GetBottomPaneHeight write SetBottomPaneHeight;
  end;

  { TMRUProjects }

  TMRUProjects = class(TJSArray)
  protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  end;


  { TStewApplicationConfig2 }

  TStewApplicationConfig2 = class(TJSObject)
  private
    const MRUListMaxSize = 20;
    function GetMainWindowConfig: TMainWindowConfig2;
    function GetMRUProject: TFile;
    function GetMRUProjects: TMRUProjects;
    procedure SetMRUProject(AValue: TFile);
  protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    constructor Create;
    property MRUProject: TFile read GetMRUProject write SetMRUProject;
    procedure Load;
    class function FileName: UTF8String;
    procedure Save;
  published
    property mainWindow: TMainWindowConfig2 read GetMainWindowConfig;
    property mruProjects: TMRUProjects read GetMRUProjects;
  end;

implementation

uses
  jsonparser, FileUtil, sys_localfile;

{ TMRUProjects }

function TMRUProjects.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  if aKey <> LengthKey then
     result := TJSString
  else
     Result:=inherited RequestType(aKey, aType);
end;

{ TStewApplicationConfig2 }

function TStewApplicationConfig2.GetMainWindowConfig: TMainWindowConfig2;
begin
  result := Get('mainWindow') as TMainWindowConfig2;
end;

function TStewApplicationConfig2.GetMRUProject: TFile;
begin
  if mruProjects.Length > 0 then
     result := LocalFile(mruProjects.Get(0).AsString)
  else
    result := LocalFile('');

end;

function TStewApplicationConfig2.GetMRUProjects: TMRUProjects;
begin
  result := Get('mruProjects') as TMRUProjects;
end;

procedure TStewApplicationConfig2.SetMRUProject(AValue: TFile);
var
  index: Integer;
  aID: String;
begin
  aID := ExcludeTrailingPathDelimiter(AValue.ID);
  index := MRUProjects.IndexOf(aID);
  if index > -1 then
    MRUProjects.Delete(index);
  MRUProjects.Unshift(aID);
  while MRUProjects.Length > MRUListMaxSize do
    MRUProjects.Length := MRUListMaxSize;
end;

function TStewApplicationConfig2.RequestType(aKey: UTF8String;
  aType: TJSValueClass): TJSValueClass;
begin
  case aKey of
    'mruProjects':
      result := TMRUProjects;
    'mainWindow':
      result := TMainWindowConfig2;
  else
    Result:=inherited RequestType(aKey, aType);
  end;
end;

constructor TStewApplicationConfig2.Create;
begin
  inherited Create;
  PutNewObject('mainWindow');
  PutNewObject('mruProjects');
end;

procedure TStewApplicationConfig2.Load;
var
  lStream: TFileStream;
  lFile: String;
begin
  lFile := FileName;
  if FileExists(lFile) then
  begin
    lStream := TFileStream.Create(FileName,fmOpenRead);
    try
      // TODO: Need a way to parse the stream into an existing object, or array.
      FromJSON(Self,lStream);
    finally
      lStream.Free;
    end;
  end
end;

class function TStewApplicationConfig2.FileName: UTF8String;
begin
  result := GetAppConfigDir(false) + 'stew-gui-config.json';
end;

procedure TStewApplicationConfig2.Save;
var
  lStream: TFileStream;
begin
  lStream := TFileStream.Create(FileName,fmCreate);
  try
    ToJSON(Self,lStream,'  ');
  finally
    lStream.Free;
  end;
end;

{ TMainWindowConfig2 }

function TMainWindowConfig2.GetBottomPaneHeight: Integer;
begin
  result := trunc(Get('bottomPaneHeight').AsNumber);
end;

function TMainWindowConfig2.GetHeight: Integer;
begin
  result := trunc(Get('height').AsNumber);
end;

function TMainWindowConfig2.GetLeftPaneWidth: Integer;
begin
  result := trunc(Get('leftPaneWidth').AsNumber);
end;

function TMainWindowConfig2.GetMaximized: Boolean;
begin
  result := Get('maximized').AsBoolean;

end;

function TMainWindowConfig2.GetRightPaneWidth: Integer;
begin
  result := trunc(Get('rightPaneWidth').AsNumber);
end;

function TMainWindowConfig2.GetTopPaneHeight: Integer;
begin
  result := trunc(Get('topPaneHeight').AsNumber);
end;

function TMainWindowConfig2.GetWidth: Integer;
begin
  result := trunc(Get('width').AsNumber);
end;

procedure TMainWindowConfig2.SetBottomPaneHeight(AValue: Integer);
begin
  Put('bottomPaneHeight',AValue);
end;

procedure TMainWindowConfig2.SetHeight(AValue: Integer);
begin
  Put('height',AValue);

end;

procedure TMainWindowConfig2.SetLeftPaneWidth(AValue: Integer);
begin
  Put('leftPaneWidth',AValue);
end;

procedure TMainWindowConfig2.SetMaximized(AValue: Boolean);
begin
  Put('maximized',AValue);
end;

procedure TMainWindowConfig2.SetRightPaneWidth(AValue: Integer);
begin
  Put('rightPaneWidth',AValue);
end;

procedure TMainWindowConfig2.SetTopPaneHeight(AValue: Integer);
begin
  Put('topPaneHeight',AValue);
end;

procedure TMainWindowConfig2.SetWidth(AValue: Integer);
begin
  Put('width',AValue);
end;

constructor TMainWindowConfig2.Create;
begin
  inherited Create;
  Maximized := false;
  Width := DefaultWidth;
  Height := DefaultHeight;
  LeftPaneWidth := DefaultVerticalPaneWidth;
  RightPaneWidth := DefaultVerticalPaneWidth;
  BottomPaneHeight:= DefaultHorizontalPaneHeight;
  TopPaneHeight := DefaultHorizontalPaneHeight;
end;

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

