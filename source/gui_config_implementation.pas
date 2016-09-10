unit gui_config_implementation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_config, sys_dynval, sys_dynval_implementation, sys_dynval_data_implementation, sys_types, sys_file;

type
  TMainWindowConfig = class(TDataStoreMap, IMainWindowConfig)
  strict protected
    procedure InitializeBlank; override;
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    procedure BuildClone(var aValue: TDynamicValue); override;
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
    property height: Integer read GetHeight write SetHeight;
    property width: Integer read GetWidth write SetWidth;
    property maximized: Boolean read GetMaximized write SetMaximized;
    property leftPaneWidth: Integer read GetLeftPaneWidth write SetLeftPaneWidth;
    property rightPaneWidth: Integer read GetRightPaneWidth write SetRightPaneWidth;
    property topPaneHeight: Integer read GetTopPaneHeight write SetTopPaneHeight;
    property bottomPaneHeight: Integer read GetBottomPaneHeight write SetBottomPaneHeight;
  end;

  TMRUProject = class(TDataStoreMap, IMRUProject)
  strict protected
    procedure InitializeBlank; override;
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    procedure BuildClone(var aValue: TDynamicValue); override;
    function GetPath: TFile;
    procedure SetPath(AValue: TFile);
  public
    property Path: TFile read GetPath write SetPath;
  end;

  TMRUProjects = class(TDataStoreList, IMRUProjects)
  strict protected
    procedure InitializeBlank; override;
    function ReadManagedItem(aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedItems(aWriter: TDynamicValueWriter); override;
    function GetLength: Longint; override;
    procedure SetLength(const AValue: Longint); override;
    function GetItem(const aKey: IDynamicValue): IDynamicValue; override;
      overload;
    procedure SetItem(const aKey: IDynamicValue; const AValue: IDynamicValue);
       override; overload;
    procedure BuildClone(var aValue: TDynamicValue); override;
    function GetProject(const aIndex: Longint): IMRUProject;
    procedure SetProject(const aIndex: Longint; const AValue: IMRUProject);
  public
    property Project[aIndex: Longint]: IMRUProject read GetProject write SetProject; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(const aItem: IMRUProject);
    procedure Add(const aPath: TFile);
    procedure Delete(const aIndex: Longint); override;
    procedure Clear; override;
    function IndexOf(const aValue: IMRUProject): Longint;
    function IndexOf(const aPath: TFile): LongInt;
  end;

  TStewApplicationConfig = class(TDataStoreMap, IStewApplicationConfig)
  strict protected
    procedure InitializeBlank; override;
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    procedure BuildClone(var aValue: TDynamicValue); override;
    function GetMainWindow: IMainWindowConfig;
    function GetMRUProject: IMRUProject;
    function GetMRUProjects: IMRUProjects;
  public
    property MainWindow: IMainWindowConfig read GetMainWindow;
    property MRUProjects: IMRUProjects read GetMRUProjects;
    property MRUProject: IMRUProject read GetMRUProject;
  end;

implementation

end.

