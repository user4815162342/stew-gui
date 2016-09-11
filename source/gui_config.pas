unit gui_config;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_file, sys_json, sys_dynval_data;

type

  { IMainWindowConfig }

  IMainWindowConfig = interface(IDataStoreObject)
    ['{B510C73A-AF2E-485A-BE0A-24742AAA7AF0}']
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
    property height: Integer read GetHeight write SetHeight;
    property width: Integer read GetWidth write SetWidth;
    property maximized: Boolean read GetMaximized write SetMaximized;
    property leftPaneWidth: Integer read GetLeftPaneWidth write SetLeftPaneWidth;
    property rightPaneWidth: Integer read GetRightPaneWidth write SetRightPaneWidth;
    property topPaneHeight: Integer read GetTopPaneHeight write SetTopPaneHeight;
    property bottomPaneHeight: Integer read GetBottomPaneHeight write SetBottomPaneHeight;
  end;

  { IMRUProject }

  IMRUProject = interface(IDataStoreObject)
    ['{2A62CE6C-BF91-45F3-AB57-5BDABF0CA420}']
    function GetPath: TFile;
    procedure SetPath(AValue: TFile);
    property Path: TFile read GetPath write SetPath;
  end;

  IMRUProjects = interface(IDataStoreObject)
    ['{15BB1137-4CCC-4216-89F2-507E3804D314}']
    function GetProject(const aIndex: Longint): IMRUProject;
    function GetLength: Longint;
    procedure SetProject(const aIndex: Longint; const AValue: IMRUProject);
    procedure SetLength(const AValue: Longint);
    property Project[aIndex: Longint]: IMRUProject read GetProject write SetProject; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(const aItem: IMRUProject);
    procedure Add(const aPath: TFile);
    procedure Delete(const aIndex: Longint);
    procedure Clear;
    function IndexOf(const aValue: IMRUProject): Longint;
    function IndexOf(const aPath: TFile): LongInt;
  end;

  { IStewApplicationConfig }

  IStewApplicationConfig = interface(IDataStoreObject)
    ['{62D40346-22B5-47CF-8C86-A5F317450A19}']
    function GetMainWindow: IMainWindowConfig;
    function GetMRUProject: TFile;
    function GetMRUProjects: IMRUProjects;
    property MainWindow: IMainWindowConfig read GetMainWindow;
    property MRUProjects: IMRUProjects read GetMRUProjects;
    property MRUProject: TFile read GetMRUProject;
  end;

  { TConfigObjects }

  TConfigObjects = class
  public
    class function NewStewApplicationConfig: IStewApplicationConfig;
    class function Filename: UTF8String;
    class function LoadStewApplicationConfig: IStewApplicationConfig;
    class procedure Save(aConfig: IStewApplicationConfig);
  end;

  { TMainWindowConfig }

  TMainWindowConfig = class(TJSObject)
  strict private
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
    constructor Create; override;
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
  strict protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    function Find(aFile: TFile): Integer;
  end;

  TMRUProject = class(TJSObject)
  strict private
    function GetPath: TFile;
    procedure SetPath(AValue: TFile);
  strict protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    constructor CreateString(aValue: UTF8String); override;
    property Path: TFile read GetPath write SetPath;
  end;

  { TStewApplicationConfig }

  TStewApplicationConfig = class(TJSObject)
  strict private
    const MRUListMaxSize = 20;
    function GetMainWindowConfig: TMainWindowConfig;
    function GetMRUProject: TFile;
    function GetMRUProjects: TMRUProjects;
    procedure SetMRUProject(AValue: TFile);
  strict protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    constructor Create; override;
    property MRUProject: TFile read GetMRUProject write SetMRUProject;
    procedure Load;
    class function FileName: UTF8String;
    procedure Save;
  published
    property mainWindow: TMainWindowConfig read GetMainWindowConfig;
    property mruProjects: TMRUProjects read GetMRUProjects;
  end;

  const
    ConfigFileName = 'stew-gui-config.json';


implementation

uses
  jsonparser, FileUtil, sys_localfile, gui_config_implementation;

{ TConfigObjects }

class function TConfigObjects.NewStewApplicationConfig: IStewApplicationConfig;
begin
  result := gui_config_implementation.TStewApplicationConfig.Create;
end;

class function TConfigObjects.Filename: UTF8String;
begin
  result := GetAppConfigDir(false) + ConfigFileName;
end;

class function TConfigObjects.LoadStewApplicationConfig: IStewApplicationConfig;
var
  lStream: TFileStream;
  lFile: String;
begin
  result := NewStewApplicationConfig;
  lFile := FileName;
  if FileExists(lFile) then
  begin
    lStream := TFileStream.Create(FileName,fmOpenRead);
    try
      result.Deserialize(lStream);
    finally
      lStream.Free;
    end;
  end
end;

class procedure TConfigObjects.Save(aConfig: IStewApplicationConfig);
var
  lStream: TFileStream;
begin
  lStream := TFileStream.Create(FileName,fmCreate);
  try
    aConfig.Serialize(lStream);
  finally
    lStream.Free;
  end;

end;

{ TMRUProject }

function TMRUProject.GetPath: TFile;
begin
  case Get('system').AsString of
    'local':
      result := LocalFile(Get('fileID').AsString);
  else
    // we can't handle any other systems right now...
    result := LocalFile('');
  end;
end;

procedure TMRUProject.SetPath(AValue: TFile);
begin
  if AValue.System = TLocalFileSystem then
  begin
    Put('system','local');
  end
  else
  begin
    raise Exception.Create('MRU Project configuration can only handle local file systems right now');
  end;
  Put('fileID',AValue.ID);
end;

function TMRUProject.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  case aKey of
    'system':
      result := TJSString;
    'fileID':
      result := TJSString;
  else
    Result:=inherited RequestType(aKey, aType);
  end;
end;

constructor TMRUProject.CreateString(aValue: UTF8String);
begin
  // To support some older formats which only had strings, accept
  // the creation with a string value.
  Create;
  Path := LocalFile(aValue);
end;

{ TMRUProjects }

function TMRUProjects.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  if aKey <> LengthKey then
     result := TMRUProject
  else
     Result:=inherited RequestType(aKey, aType);
end;

function TMRUProjects.Find(aFile: TFile): Integer;
var
  l: Integer;
begin
  l := Length;
  for result := 0 to l - 1 do
  begin
    if (Get(result) as TMRUProject).Path = aFile then
       Exit;
  end;
  result := -1;
end;

{ TStewApplicationConfig }

function TStewApplicationConfig.GetMainWindowConfig: TMainWindowConfig;
begin
  result := Get('mainWindow') as TMainWindowConfig;
end;

function TStewApplicationConfig.GetMRUProject: TFile;
begin
  if mruProjects.Length > 0 then
     result := (mruProjects.Get(0) as TMRUProject).Path
  else
    result := LocalFile('');

end;

function TStewApplicationConfig.GetMRUProjects: TMRUProjects;
begin
  result := Get('mruProjects') as TMRUProjects;
end;

procedure TStewApplicationConfig.SetMRUProject(AValue: TFile);
var
  index: Integer;
begin
  // I shouldn't have to do this, but I was seeing some cases where
  // it was happening. This fix may be unnecessary, but I'm not absolutely
  // sure the problem is gone because I don't remember where it was.
  index := MRUProjects.Find(aValue);
  if index <> 0 then
  begin
    if index > -1 then
    begin
      mruProjects.Splice(index,1);
    end;
    MRUProjects.Unshift;
    (MRUProjects.PutNewObject('0') as TMRUProject).Path := AValue;
    if MRUProjects.Length > MRUListMaxSize then
      MRUProjects.Length := MRUListMaxSize;
  end;
end;

function TStewApplicationConfig.RequestType(aKey: UTF8String;
  aType: TJSValueClass): TJSValueClass;
begin
  case aKey of
    'mruProjects':
      result := TMRUProjects;
    'mainWindow':
      result := TMainWindowConfig;
  else
    Result:=inherited RequestType(aKey, aType);
  end;
end;

constructor TStewApplicationConfig.Create;
begin
  inherited Create;
  PutNewObject('mainWindow');
  PutNewObject('mruProjects');
end;

procedure TStewApplicationConfig.Load;
var
  lStream: TFileStream;
  lFile: String;
begin
  lFile := FileName;
  if FileExists(lFile) then
  begin
    lStream := TFileStream.Create(FileName,fmOpenRead);
    try
      FromJSON(Self,lStream);
    finally
      lStream.Free;
    end;
  end
end;

class function TStewApplicationConfig.FileName: UTF8String;
begin
  result := GetAppConfigDir(false) + 'stew-gui-config.json';
end;

procedure TStewApplicationConfig.Save;
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

{ TMainWindowConfig }

function TMainWindowConfig.GetBottomPaneHeight: Integer;
begin
  result := trunc(Get('bottomPaneHeight').AsNumber);
end;

function TMainWindowConfig.GetHeight: Integer;
begin
  result := trunc(Get('height').AsNumber);
end;

function TMainWindowConfig.GetLeftPaneWidth: Integer;
begin
  result := trunc(Get('leftPaneWidth').AsNumber);
end;

function TMainWindowConfig.GetMaximized: Boolean;
begin
  result := Get('maximized').AsBoolean;

end;

function TMainWindowConfig.GetRightPaneWidth: Integer;
begin
  result := trunc(Get('rightPaneWidth').AsNumber);
end;

function TMainWindowConfig.GetTopPaneHeight: Integer;
begin
  result := trunc(Get('topPaneHeight').AsNumber);
end;

function TMainWindowConfig.GetWidth: Integer;
begin
  result := trunc(Get('width').AsNumber);
end;

procedure TMainWindowConfig.SetBottomPaneHeight(AValue: Integer);
begin
  Put('bottomPaneHeight',AValue);
end;

procedure TMainWindowConfig.SetHeight(AValue: Integer);
begin
  Put('height',AValue);

end;

procedure TMainWindowConfig.SetLeftPaneWidth(AValue: Integer);
begin
  Put('leftPaneWidth',AValue);
end;

procedure TMainWindowConfig.SetMaximized(AValue: Boolean);
begin
  Put('maximized',AValue);
end;

procedure TMainWindowConfig.SetRightPaneWidth(AValue: Integer);
begin
  Put('rightPaneWidth',AValue);
end;

procedure TMainWindowConfig.SetTopPaneHeight(AValue: Integer);
begin
  Put('topPaneHeight',AValue);
end;

procedure TMainWindowConfig.SetWidth(AValue: Integer);
begin
  Put('width',AValue);
end;

constructor TMainWindowConfig.Create;
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


end.

