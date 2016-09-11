unit gui_config_implementation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gui_config, sys_dynval, sys_dynval_implementation, sys_dynval_data_implementation, sys_types, sys_file;

type

  { TMainWindowConfig }

  TMainWindowConfig = class(TDataStoreMap, IMainWindowConfig)
  strict private
    const DefaultWidth: Integer = 600;
    const DefaultHeight: Integer = 450;
    const DefaultVerticalPaneWidth: Integer = 200;
    const DefaultHorizontalPaneHeight: Integer = 50;
  strict private
    fBottomPaneHeight: Integer;
    fHeight: Integer;
    fLeftPaneWidth: Integer;
    fMaximized: Boolean;
    fRightPaneWidth: Integer;
    fTopPaneHeight: Integer;
    fWidth: Integer;
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

  { TMRUProject }

  TMRUProject = class(TDataStoreMap, IMRUProject)
  private
    fPath: TFile;
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

  { TMRUProjects }

  TMRUProjects = class(TDataStoreList, IMRUProjects)
  strict private
    const MRUListMaxSize = 20;
  strict private
    fItems: IDynamicList;
  strict protected
    procedure InitializeBlank; override;
    function ReadManagedItem(aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedItems(aWriter: TDynamicValueWriter); override;
    function GetLength: Longint; override;
    procedure SetLength(const AValue: Longint); override;
    function GetItem(const aKey: Longint): IDynamicValue; overload; override;
    procedure SetItem(const aKey: Longint; const AValue: IDynamicValue);
      overload; override;
    procedure BuildClone(var aValue: TDynamicValue); override;
    function GetProject(const aIndex: Longint): IMRUProject;
    procedure SetProject(const aIndex: Longint; const AValue: IMRUProject);
    function GetMRUProject: TFile;
    procedure SetMRUProject(AValue: TFile);
  public
    property Project[aIndex: Longint]: IMRUProject read GetProject write SetProject; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(const aItem: IMRUProject);
    procedure Add(const aPath: TFile);
    procedure Unshift(const aPath: TFile);
    procedure Delete(const aIndex: Longint); override;
    procedure Clear; override;
    function IndexOf(const aValue: IMRUProject): Longint;
    function IndexOf(const aPath: TFile): LongInt;

  end;

  { TStewApplicationConfig }

  TStewApplicationConfig = class(TDataStoreMap, IStewApplicationConfig)
  private
    fMainWindow: IMainWindowConfig;
    fMRUProjects: IMRUProjects;
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
    function GetMRUProjects: IMRUProjects;
  public
    property MainWindow: IMainWindowConfig read GetMainWindow;
    property MRUProjects: IMRUProjects read GetMRUProjects;
    procedure Load;
    procedure Save;
  end;

  const
    BottomPaneHeightKey = 'bottomPaneHeight';
    LeftPaneWidthKey = 'leftPaneWidth';
    RightPaneWidthKey = 'rightPaneWidth';
    TopPaneHeightKey = 'topPaneHeight';
    HeightKey = 'height';
    WidthKey = 'width';
    MaximizedKey = 'maximized';
    FileSystemKey = 'system';
    LocalFileSystemValue = 'local';
    FileIDKey = 'fileID';
    MainWindowKey = 'mainWindow';
    MRUProjectsKey = 'mruProjects';


implementation

uses
  stew_properties_implementation, sys_localfile, math;

{ TStewApplicationConfig }

procedure TStewApplicationConfig.InitializeBlank;
begin
  fMainWindow := TMainWindowConfig.Create;
  fMRUProjects := TMRUProjects.Create;
  inherited InitializeBlank;
end;

function TStewApplicationConfig.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    MainWindowKey:
      fMainWindow.Deserialize(aReader);
    MRUProjectsKey:
      fMRUProjects.Deserialize(aReader);
  else
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;
end;

procedure TStewApplicationConfig.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKey(MainWindowKey);
  fMainWindow.Serialize(aWriter);
  aWriter.WriteKey(MRUProjectsKey);
  fMRUProjects.Serialize(aWriter);
  inherited WriteManagedKeys(aWriter);
end;

procedure TStewApplicationConfig.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(MainWindowKey);
  aValue.Add(MRUProjectsKey);
  inherited ListManagedKeys(aValue);
end;

function TStewApplicationConfig.GetItem(const aKey: UTF8String): IDynamicValue;
begin
  case aKey of
    MainWindowKey:
      result := fMainWindow;
    MRUProjectsKey:
      result := fMRUProjects;
  else
    Result:=inherited GetItem(aKey);
  end;
end;

procedure TStewApplicationConfig.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);
begin
  case aKey of
    MainWindowKey,
    MRUProjectsKey:
      raise Exception.Create('Stop trying to set the main window or mru projects configuration objects yourself.');
  else
    inherited SetItem(aKey, AValue);
  end;
end;

procedure TStewApplicationConfig.BuildClone(var aValue: TDynamicValue);
begin
  if aValue = nil then
     aValue := TStewApplicationConfig.Create;
  (aValue as TStewApplicationConfig).fMainWindow := fMainWindow.Clone as IMainWindowConfig;
  (aValue as TStewApplicationConfig).fMRUProjects := fMRUProjects.Clone as IMRUProjects;
  inherited BuildClone(aValue);
end;

function TStewApplicationConfig.GetMainWindow: IMainWindowConfig;
begin
  result := fMainWindow;

end;

function TStewApplicationConfig.GetMRUProjects: IMRUProjects;
begin
  result := fMRUProjects;
end;

procedure TStewApplicationConfig.Load;
var
  lStream: TFileStream;
  lFile: String;
begin
  lFile := TConfigObjects.Filename;
  if FileExists(lFile) then
  begin
    lStream := TFileStream.Create(lFile,fmOpenRead);
    try
      Deserialize(lStream);
    finally
      lStream.Free;
    end;
  end
  else
    InitializeBlank;
end;

procedure TStewApplicationConfig.Save;
var
  lStream: TFileStream;
begin
  lStream := TFileStream.Create(TConfigObjects.Filename,fmCreate);
  try
    Serialize(lStream);
  finally
    lStream.Free;
  end;
end;

{ TMRUProjects }

procedure TMRUProjects.InitializeBlank;
begin
  fItems := TDynamicValues.NewList;
  inherited InitializeBlank;
end;

function TMRUProjects.ReadManagedItem(aReader: TDynamicValueReader): Boolean;
var
  lProject: IMRUProject;
begin
  lProject := TMRUProject.Create;
  lProject.Deserialize(aReader);
  fItems.Add(lProject);
  result := true;
end;

procedure TMRUProjects.WriteManagedItems(aWriter: TDynamicValueWriter);
var
  l: longint;
  i: Longint;
begin
  l := fItems.Length;
  for i := 0 to l - 1 do
  begin
    (fItems[i] as IMRUProject).Serialize(aWriter);
  end;
  inherited WriteManagedItems(aWriter);
end;

function TMRUProjects.GetLength: Longint;
begin
  result := fItems.Length;
end;

procedure TMRUProjects.SetLength(const AValue: Longint);
begin
  fItems.Length := AValue;
end;

function TMRUProjects.GetItem(const aKey: Longint): IDynamicValue;
begin
  Result:=fItems.GetItem(aKey);
end;

procedure TMRUProjects.SetItem(const aKey: Longint; const AValue: IDynamicValue
  );
begin
  if AValue is IMRUProject then
     fItems.SetItem(aKey,AValue)
  else
     raise Exception.Create('MRUProjects can only hold MRUProject');
end;

procedure TMRUProjects.BuildClone(var aValue: TDynamicValue);
begin
  if aValue = nil then
     aValue := TMRUProjects.Create;
  (aValue as TMRUProjects).fItems := fItems.Clone as IDynamicList;
  inherited BuildClone(aValue);
end;

function TMRUProjects.GetProject(const aIndex: Longint): IMRUProject;
begin
  result := fItems[aIndex] as IMRUProject;
end;

procedure TMRUProjects.SetProject(const aIndex: Longint;
  const AValue: IMRUProject);
begin
  fItems[aIndex] := AValue;
end;

function TMRUProjects.GetMRUProject: TFile;
begin
  if Length > 0 then
     result := GetProject(0).Path
  else
    result := LocalFile('');
end;

procedure TMRUProjects.SetMRUProject(AValue: TFile);
var
  lOldIndex: Longint;
  i: Longint;
  l: Longint;
  lProject: IMRUProject;
begin
  lOldIndex := IndexOf(aValue);
  if lOldIndex <> 0 then
  begin
    if lOldIndex > -1 then
    begin
      // remove it from where it was...
      fItems.Delete(lOldIndex);
    end;
    // insert it at the beginning...
    l := Min(fItems.Length + 1,MRUListMaxSize);
    fItems.Length := l;
    for i := (l - 1) downto 1 do
      fItems.SetItem(i,fItems.GetItem(i - 1));
    lProject := TMRUProject.Create;
    lProject.Path := AValue;
    SetProject(0,lProject);
  end;
end;

procedure TMRUProjects.Add(const aItem: IMRUProject);
begin
  fItems.Add(aItem);
end;

procedure TMRUProjects.Add(const aPath: TFile);
var
  lProject: IMRUProject;
begin
  lProject := TMRUProject.Create;
  lProject.Path := aPath;
  Add(lProject);

end;

procedure TMRUProjects.Unshift(const aPath: TFile);
var
  lProject: IMRUProject;
  l: Longint;
  i: Longint;
begin
  lProject := TMRUProject.Create;
  lProject.Path := aPath;
  l := fItems.Length;
  fItems.Length := l + 1;
  for i := l downto 1 do
  begin
    fItems.SetItem(i,fItems.GetItem(i - 1));
  end;
  SetItem(0,lProject);
end;

procedure TMRUProjects.Delete(const aIndex: Longint);
begin
  fItems.Delete(aIndex);

end;

procedure TMRUProjects.Clear;
begin
  fItems.Clear;

end;

function TMRUProjects.IndexOf(const aValue: IMRUProject): Longint;
begin
  result := IndexOf(aValue.Path);

end;

function TMRUProjects.IndexOf(const aPath: TFile): LongInt;
var
  l: Longint;
  i: Longint;
begin
  result := -1;
  l := fItems.Length;
  for i := 0 to l - 1 do
  begin
    if (fItems[i] as IMRUProject).Path = aPath then
    begin
       result := i;
       break;
    end;
  end;

end;

{ TMRUProject }

procedure TMRUProject.InitializeBlank;
begin
  fPath := LocalFile('');
  inherited InitializeBlank;
end;

function TMRUProject.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    FileSystemKey,
    FileIDKey:
      SetItem(aKey,aReader.ReadValue);
  else
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;
end;

procedure TMRUProject.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKeyValue(FileSystemKey,GetItem(FileSystemKey));
  aWriter.WriteKeyValue(FileIDKey,GetItem(FileIDKey));
  inherited WriteManagedKeys(aWriter);
end;

procedure TMRUProject.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(FileSystemKey);
  aValue.Add(FileIDKey);
  inherited ListManagedKeys(aValue);
end;

function TMRUProject.GetItem(const aKey: UTF8String): IDynamicValue;
begin
  case aKey of
    FileSystemKey:
      if fPath.System = TLocalFileSystem then
         result := LocalFileSystemValue
      else
         raise Exception.Create('Invalid internal value: MRUProject can only handle local file system');
    FileIDKey:
      result := fPath.ID;
  else
    Result:=inherited GetItem(aKey);
  end;
end;

procedure TMRUProject.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);
begin
  case aKey of
    FileSystemKey:
      if (AValue is IDynamicString) and
         ((AValue as IDynamicString).Value = LocalFileSystemValue) then
      begin
        fPath := LocalFile(fPath.ID);
      end
      else
         RaiseInvalidKeyValue(aKey,AValue);
    FileIDKey:
      if AValue is IDynamicString then
        fPath := LocalFile((AValue as IDynamicString).Value)
      else
         RaiseInvalidKeyValue(aKey,AValue);
  else
    inherited SetItem(aKey, AValue);
  end;
end;

procedure TMRUProject.BuildClone(var aValue: TDynamicValue);
begin
  if aValue = nil then
     aValue := TMRUProject.Create;
  (aValue as TMRUProject).fPath := fPath;
  inherited BuildClone(aValue);
end;

function TMRUProject.GetPath: TFile;
begin
  result := fPath;
end;

procedure TMRUProject.SetPath(AValue: TFile);
begin
  fPath := AValue;
end;

{ TMainWindowConfig }

procedure TMainWindowConfig.InitializeBlank;
begin
  fBottomPaneHeight:= DefaultHorizontalPaneHeight;
  fHeight := DefaultHeight;
  fLeftPaneWidth := DefaultVerticalPaneWidth;
  fMaximized := False;
  fRightPaneWidth := DefaultVerticalPaneWidth;
  fTopPaneHeight := DefaultHorizontalPaneHeight;
  fWidth := DefaultWidth;
  inherited InitializeBlank;
end;

function TMainWindowConfig.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    BottomPaneHeightKey,
    HeightKey,
    LeftPaneWidthKey,
    MaximizedKey,
    RightPaneWidthKey,
    TopPaneHeightKey,
    WidthKey:
      SetItem(aKey,aReader.ReadValue)
  else
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;
end;

procedure TMainWindowConfig.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKeyValue(BottomPaneHeightKey,GetItem(BottomPaneHeightKey));
  aWriter.WriteKeyValue(HeightKey,GetItem(HeightKey));
  aWriter.WriteKeyValue(LeftPaneWidthKey,GetItem(LeftPaneWidthKey));
  aWriter.WriteKeyValue(MaximizedKey,GetItem(MaximizedKey));
  aWriter.WriteKeyValue(RightPaneWidthKey,GetItem(RightPaneWidthKey));
  aWriter.WriteKeyValue(TopPaneHeightKey,GetItem(TopPaneHeightKey));
  aWriter.WriteKeyValue(WidthKey,GetItem(WidthKey));
  inherited WriteManagedKeys(aWriter);
end;

procedure TMainWindowConfig.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(BottomPaneHeightKey);
  aValue.Add(HeightKey);
  aValue.Add(LeftPaneWidthKey);
  aValue.Add(MaximizedKey);
  aValue.Add(RightPaneWidthKey);
  aValue.Add(TopPaneHeightKey);
  aValue.Add(WidthKey);
  inherited ListManagedKeys(aValue);
end;

function TMainWindowConfig.GetItem(const aKey: UTF8String): IDynamicValue;
begin
  case aKey of
    BottomPaneHeightKey:
      result := GetBottomPaneHeight;
    HeightKey:
      result := GetHeight;
    LeftPaneWidthKey:
      result := GetLeftPaneWidth;
    MaximizedKey:
      result := GetMaximized;
    RightPaneWidthKey:
      result := GetRightPaneWidth;
    TopPaneHeightKey:
      result := GetTopPaneHeight;
    WidthKey:
      result := GetWidth;
  else
    Result:=inherited GetItem(aKey);
  end;
end;

procedure TMainWindowConfig.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);
begin
  case aKey of
    BottomPaneHeightKey:
      if AValue is IDynamicNumber then
         SetBottomPaneHeight(trunc((AValue as IDynamicNumber).Value))
      else
         RaiseInvalidKeyValue(aKey,AValue);
    HeightKey:
      if AValue is IDynamicNumber then
         SetHeight(trunc((AValue as IDynamicNumber).Value))
      else
         RaiseInvalidKeyValue(aKey,AValue);
    LeftPaneWidthKey:
      if AValue is IDynamicNumber then
         SetLeftPaneWidth(trunc((AValue as IDynamicNumber).Value))
      else
         RaiseInvalidKeyValue(aKey,AValue);
    MaximizedKey:
      if AValue is IDynamicBoolean then
         SetMaximized((AValue as IDynamicBoolean).Value)
      else
         RaiseInvalidKeyValue(aKey,AValue);
    RightPaneWidthKey:
      if AValue is IDynamicNumber then
         SetRightPaneWidth(trunc((AValue as IDynamicNumber).Value))
      else
         RaiseInvalidKeyValue(aKey,AValue);
    TopPaneHeightKey:
      if AValue is IDynamicNumber then
         SetTopPaneHeight(trunc((AValue as IDynamicNumber).Value))
      else
         RaiseInvalidKeyValue(aKey,AValue);
    WidthKey:
      if AValue is IDynamicNumber then
         SetWidth(trunc((AValue as IDynamicNumber).Value))
      else
         RaiseInvalidKeyValue(aKey,AValue);
  else
    inherited SetItem(aKey, AValue);
  end;
end;

procedure TMainWindowConfig.BuildClone(var aValue: TDynamicValue);
begin
  if aValue = nil then
     aValue := TMainWindowConfig.Create;
  (aValue as TMainWindowConfig).fBottomPaneHeight := fBottomPaneHeight;
  (aValue as TMainWindowConfig).fTopPaneHeight:= fTopPaneHeight;
  (aValue as TMainWindowConfig).fLeftPaneWidth:= fLeftPaneWidth;
  (aValue as TMainWindowConfig).fRightPaneWidth:=fRightPaneWidth;
  (aValue as TMainWindowConfig).fHeight := fHeight;
  (aValue as TMainWindowConfig).fWidth := fWidth;
  (aValue as TMainWindowConfig).fMaximized := fMaximized;
  inherited BuildClone(aValue);
end;

function TMainWindowConfig.GetBottomPaneHeight: Integer;
begin
  result := fBottomPaneHeight;
end;

function TMainWindowConfig.GetHeight: Integer;
begin
  result := fHeight;
end;

function TMainWindowConfig.GetLeftPaneWidth: Integer;
begin
  result := fLeftPaneWidth;
end;

function TMainWindowConfig.GetMaximized: Boolean;
begin
  Result := fMaximized;
end;

function TMainWindowConfig.GetRightPaneWidth: Integer;
begin
  result := fRightPaneWidth;
end;

function TMainWindowConfig.GetTopPaneHeight: Integer;
begin
  result := fTopPaneHeight;
end;

function TMainWindowConfig.GetWidth: Integer;
begin
  result := fWidth;
end;

procedure TMainWindowConfig.SetBottomPaneHeight(AValue: Integer);
begin
  fBottomPaneHeight := AValue;
end;

procedure TMainWindowConfig.SetHeight(AValue: Integer);
begin
  fHeight  := AValue;
end;

procedure TMainWindowConfig.SetLeftPaneWidth(AValue: Integer);
begin
  fLeftPaneWidth := AValue;
end;

procedure TMainWindowConfig.SetMaximized(AValue: Boolean);
begin
  fMaximized := AValue;
end;

procedure TMainWindowConfig.SetRightPaneWidth(AValue: Integer);
begin
  fRightPaneWidth := AValue;
end;

procedure TMainWindowConfig.SetTopPaneHeight(AValue: Integer);
begin
  fTopPaneHeight := AValue;
end;

procedure TMainWindowConfig.SetWidth(AValue: Integer);
begin
  fWidth := AValue;
end;

end.

