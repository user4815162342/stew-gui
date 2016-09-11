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

  { IMRUProjects }

  IMRUProjects = interface(IDataStoreObject)
    ['{15BB1137-4CCC-4216-89F2-507E3804D314}']
    function GetMRUProject: TFile;
    function GetProject(const aIndex: Longint): IMRUProject;
    function GetLength: Longint;
    procedure SetMRUProject(AValue: TFile);
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
    property MRUProject: TFile read GetMRUProject write SetMRUProject;
  end;

  { IStewApplicationConfig }

  IStewApplicationConfig = interface(IDataStoreObject)
    ['{62D40346-22B5-47CF-8C86-A5F317450A19}']
    function GetMainWindow: IMainWindowConfig;
    function GetMRUProjects: IMRUProjects;
    property MainWindow: IMainWindowConfig read GetMainWindow;
    property MRUProjects: IMRUProjects read GetMRUProjects;
    procedure Load;
    procedure Save;
  end;

  { TStewApplicationConfigObjects }

  TStewApplicationConfigObjects = class
  public
    class function NewStewApplicationConfig: IStewApplicationConfig;
    class function Filename: UTF8String;
    class function LoadStewApplicationConfig: IStewApplicationConfig;
  end;

  const
    ConfigFileName = 'stew-gui-config.json';


implementation

uses
  jsonparser, FileUtil, sys_localfile, gui_config_implementation;

{ TStewApplicationConfigObjects }

class function TStewApplicationConfigObjects.NewStewApplicationConfig: IStewApplicationConfig;
begin
  result := gui_config_implementation.TStewApplicationConfig.Create;
end;

class function TStewApplicationConfigObjects.Filename: UTF8String;
begin
  result := GetAppConfigDir(false) + ConfigFileName;
end;

class function TStewApplicationConfigObjects.LoadStewApplicationConfig: IStewApplicationConfig;
begin
  result := NewStewApplicationConfig;
  Result.Load;
end;



end.

