unit stewproject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewfile, stewasync, stewproperties, stewtypes;

type
  // All of these are just strings, representing paths off of the root
  // of the stew project (names always use '/' as separators, and must
  // start with a '/' if not a relative name. In the future I may
  // make use of more structured types.
  TDocumentBaseName = String;
  TDocumentID = String;

  TDocumentList = array of TDocumentID;
  TDeferredDocumentListCallback = procedure(Path: TPacketName; Data: TDocumentList; aTarget: TObject) of object;

  { TStewProject }

  TStewProject = class
  private
    fDisk: TFilename;
    FOnOpened: TNotifyEvent;
    FOnPropertiesError: TExceptionEvent;
    FOnPropertiesLoaded: TNotifyEvent;
    FOnPropertiesSaveConflicted: TNotifyEvent;
    FOnPropertiesSaved: TNotifyEvent;
    fProperties: TProjectProperties;
    function GetProperties: TProjectProperties;
    procedure OpenProjectProperties;
    procedure DoOpened;
    procedure SetOnOpened(AValue: TNotifyEvent);
    procedure SetOnPropertiesError(AValue: TExceptionEvent);
    procedure SetOnPropertiesLoaded(AValue: TNotifyEvent);
    procedure SetOnPropertiesSaveConflicted(AValue: TNotifyEvent);
    procedure SetOnPropertiesSaved(AValue: TNotifyEvent);
    procedure ProjectPropertiesLoaded(Sender: TObject);
    procedure ProjectPropertiesLoadFailed(Sender: TObject; aError: Exception);
    procedure ProjectPropertiesSaveConflicted(Sender: TObject);
    procedure ProjectPropertiesSaved(Sender: TObject);
    procedure ProjectPropertiesSaveFailed(Sender: TObject; aError: Exception);
  public
    constructor Create(const Path: TFilename);
    destructor Destroy; override;
    property DiskPath: TFilename read fDisk;
    // TODO: Figure out filtering...
    procedure ListDocuments(const ADocument: TDocumentID; aTarget: TObject;
      aCallback: TDeferredDocumentListCallback;
  aErrorback: TDeferredExceptionCallback); overload;
    procedure ListRootDocuments(aTarget: TObject;
      aCallback: TDeferredDocumentListCallback;
  aErrorback: TDeferredExceptionCallback); overload;
    // TODO: Figure out patterns and reg ex...
    // TODO: function Match: TProjectContentEnumerator;
    // TODO: function Add(Name: TPacketBaseName): T;
    // TODO: function Get(Name: TPacketName): T;
    // TODO: procedure MoveHere(NewChild: T);
    function GetDiskPath(const ADocument: TDocumentID): TFileName;
    function GetBaseName(const aDocument: TDocumentID): TDocumentBaseName;
    procedure OpenAtPath(aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
    procedure OpenInParentDirectory(aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
    procedure OpenNewAtPath;
    property OnOpened: TNotifyEvent read FOnOpened write SetOnOpened;
    property OnPropertiesLoaded: TNotifyEvent read FOnPropertiesLoaded write SetOnPropertiesLoaded;
    property OnPropertiesSaved: TNotifyEvent read FOnPropertiesSaved write SetOnPropertiesSaved;
    property OnPropertiesError: TExceptionEvent read FOnPropertiesError write SetOnPropertiesError;
    property OnPropertiesSaveConflicted: TNotifyEvent read FOnPropertiesSaveConflicted write SetOnPropertiesSaveConflicted;
    function GetProjectName: String;
    property Properties: TProjectProperties read GetProperties;
  end;

  function IncludeTrailingSlash(Const Path : String) : String;
  function ExcludeLeadingSlash(Const Path: string): string;

implementation

const
  RootDocument: TDocumentID = '/';

// document paths are always '/' whether that's the OS path delimiter or not.
function IncludeTrailingSlash(Const Path : String) : String;
Var
  l : Integer;
begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or (Result[l] <> '/') then
    Result:=Result+'/';
end;

function ExcludeLeadingSlash(Const Path: string): string;
Var
  L : Integer;
begin
  Result:=Path;
  L:=Length(Result);
  If (L>0) and (Result[1] = '/') then
    Delete(Result,1,1);
end;

function GetDiskPath(const DiskPath: TFileName; const ADocument: TDocumentID): TFilename;
begin
  result := DiskPath + ADocument;
end;



{ TStewProject }

procedure TStewProject.ProjectPropertiesLoaded(Sender: TObject);
begin
  if fProperties.Modified then // it was new, so we want to immediately save
                               // it to make sure the file was created.
    fProperties.Save;
  if FOnPropertiesLoaded <> nil then
    FOnPropertiesLoaded(Self);
end;

procedure TStewProject.ProjectPropertiesLoadFailed(Sender: TObject;
  aError: Exception);
begin
  if fOnPropertiesError <> nil then
    fOnPropertiesError(Self,aError);
end;

procedure TStewProject.ProjectPropertiesSaveConflicted(Sender: TObject);
begin
  if fOnPropertiesSaveConflicted <> nil then
    fOnPropertiesSaveConflicted(Self);
end;

procedure TStewProject.ProjectPropertiesSaved(Sender: TObject);
begin
  if FOnPropertiesSaved <> nil then
    FOnPropertiesSaved(Self);
end;

procedure TStewProject.ProjectPropertiesSaveFailed(Sender: TObject;
  aError: Exception);
begin
  if fOnPropertiesError <> nil then
    fOnPropertiesError(Self,aError);
end;

procedure TStewProject.OpenProjectProperties;
begin
  if fProperties = nil then
  begin
    fProperties := TProjectProperties.Create(fDisk);
    fProperties.OnFileLoaded:=@ProjectPropertiesLoaded;
    fProperties.OnFileLoadFailed:=@ProjectPropertiesLoadFailed;
    fProperties.OnFileSaveConflicted:=@ProjectPropertiesSaveConflicted;
    fProperties.OnFileSaved:=@ProjectPropertiesSaved;
    fProperties.OnFileSaveFailed:=@ProjectPropertiesSaveFailed;
    fProperties.Load;
  end;
end;

function TStewProject.GetProperties: TProjectProperties;
begin
  if fProperties = nil then
     raise Exception.Create('You must open the project before you can see the properties');
  result := fProperties;
end;

procedure TStewProject.DoOpened;
begin
  if FOnOpened <> nil then
    FOnOpened(Self);
  // open and save project properties immediately, so that the file exists.
  OpenProjectProperties;
end;

procedure TStewProject.SetOnOpened(AValue: TNotifyEvent);
begin
  if FOnOpened=AValue then Exit;
  FOnOpened:=AValue;
end;

procedure TStewProject.SetOnPropertiesError(AValue: TExceptionEvent);
begin
  if FOnPropertiesError=AValue then Exit;
  FOnPropertiesError:=AValue;
end;

procedure TStewProject.SetOnPropertiesLoaded(AValue: TNotifyEvent);
begin
  if FOnPropertiesLoaded=AValue then Exit;
  FOnPropertiesLoaded:=AValue;
end;

procedure TStewProject.SetOnPropertiesSaveConflicted(AValue: TNotifyEvent);
begin
  if FOnPropertiesSaveConflicted=AValue then Exit;
  FOnPropertiesSaveConflicted:=AValue;
end;

procedure TStewProject.SetOnPropertiesSaved(AValue: TNotifyEvent);
begin
  if FOnPropertiesSaved=AValue then Exit;
  FOnPropertiesSaved:=AValue;
end;

constructor TStewProject.Create(const Path: TFilename);
begin
  fDisk := IncludeTrailingPathDelimiter(Path);
  fProperties := nil;
end;

destructor TStewProject.Destroy;
begin
  if fProperties <> nil then
    fProperties.Free;
  inherited Destroy;
end;

type

  { TListDocuments }

  TListDocuments = class
  private
    fDiskPath: TFilename;
    fDocument: TDocumentID;
    fTarget: TObject;
    fListCallback: TDeferredDocumentListCallback;
    fErrorback: TDeferredExceptionCallback;
  protected
    procedure ConvertPacketListToDocumentList(aList: TPacketList);
  public
    constructor Create(const aDiskPath: TFileName; const aDocument: TDocumentID; aTarget: TObject;
      aCallback: TDeferredDocumentListCallback; aErrorback: TDeferredExceptionCallback);
    procedure Enqueue;
  end;

{ TListDocuments }

procedure TListDocuments.ConvertPacketListToDocumentList(aList: TPacketList);
var
  DocPath: TPacketName;
  Answer: TDocumentList;
  i: Integer;
begin
  DocPath := IncludeTrailingSlash(fDocument);
  SetLength(Answer,Length(aList));
  for i := 0 to Length(aList) - 1 do
  begin
    Answer[i] := DocPath + aList[i];
  end;
  fListCallback(DocPath,Answer,fTarget);

  // Now, I'm finished.
  Free;
end;

constructor TListDocuments.Create(const aDiskPath: TFileName;
  const aDocument: TDocumentID; aTarget: TObject;
  aCallback: TDeferredDocumentListCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  inherited Create;
  fDiskPath := aDiskPath;
  fErrorback := aErrorback;
  fDocument := aDocument;
  fListCallback := aCallback;
  fTarget := aTarget;
end;

procedure TListDocuments.Enqueue;
begin
  TListPackets.Create(fDiskPath,@ConvertPacketListToDocumentList,fErrorback).Enqueue;

end;

procedure TStewProject.ListDocuments(const ADocument: TDocumentID; aTarget: TObject;
  aCallback: TDeferredDocumentListCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TListDocuments.Create(GetDiskPath(ADocument),ADocument,aTarget,aCallback,aErrorback).Enqueue;
end;

procedure TStewProject.ListRootDocuments(
  aTarget: TObject;
  aCallback: TDeferredDocumentListCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TListDocuments.Create(GetDiskPath(RootDocument),RootDocument,aTarget,aCallback,aErrorback).Enqueue;
end;

function TStewProject.GetDiskPath(const ADocument: TDocumentID): TFileName;
begin
  result := stewproject.GetDiskPath(fDisk,ADocument);
end;

function TStewProject.GetBaseName(const aDocument: TDocumentID
  ): TDocumentBaseName;
begin
  result := ExtractFileName(aDocument);
end;

type

  { TProjectExists }

  TProjectExists = class(TFileExists)
  private
    fProject: TStewProject;
    fCallback: TDeferredBooleanCallback;
    procedure FileExistsCallback(Data: Boolean);
  public
    constructor Create(aProject: TStewProject; aCallback: TDeferredBooleanCallback;
      aErrorback: TDeferredExceptionCallback);
  end;

{ TProjectExists }

procedure TProjectExists.FileExistsCallback(Data: Boolean);
begin
  if (data) then
    fProject.DoOpened;
  fCallback(Data);
end;

constructor TProjectExists.Create(aProject: TStewProject;
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(TProjectProperties.GetPath(aProject.fDisk),@FileExistsCallback,aErrorback);
  fProject := aProject;
  fCallback := aCallback;
end;

procedure TStewProject.OpenAtPath(aCallback: TDeferredBooleanCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TProjectExists.Create(Self,aCallback,aErrorback).Enqueue;
end;

type

  { TSearchParentDirectories }

  TSearchParentDirectories = class
  private
    fProject: TStewProject;
    fPath: TFilename;
    fCallback: TDeferredBooleanCallback;
    fErrorback: TDeferredExceptionCallback;
    procedure FileExistsCallback(Data: Boolean);
  public
    constructor Create(aProject: TStewProject; aPath: TFilename; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
    procedure Enqueue;
  end;

{ TSearchParentDirectories }

procedure TSearchParentDirectories.FileExistsCallback(Data: Boolean);
var
  aPath: TFilename;
begin
  if (Data) then
  begin
    fProject.fDisk := fPath;
    fProject.DoOpened;
    fCallback(true);
  end
  else
  begin
    aPath := ExtractFileDir(ExcludeTrailingPathDelimiter(fPath));
    if aPath = fPath then
    begin
      fCallback(false);
    end
    else
    begin
      fPath := aPath;
      TFileExists.Create(TProjectProperties.GetPath(fPath),@FileExistsCallback,fErrorback).Enqueue;
    end;
  end;
end;

constructor TSearchParentDirectories.Create(aProject: TStewProject;
  aPath: TFilename; aCallback: TDeferredBooleanCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  inherited Create;
  fProject := aProject;
  fPath := aPath;
  fCallback := aCallback;
  fErrorback := aErrorback;
end;

procedure TSearchParentDirectories.Enqueue;
begin
  TFileExists.Create(TProjectProperties.GetPath(fPath),@FileExistsCallback,fErrorback).Enqueue;
end;


procedure TStewProject.OpenInParentDirectory(
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  TSearchParentDirectories.Create(Self,fDisk,aCallback,aErrorback).Enqueue;
end;

procedure TStewProject.OpenNewAtPath;
begin
  DoOpened;
end;

function TStewProject.GetProjectName: String;
begin
  result := ExtractFileName(ExcludeTrailingPathDelimiter(fDisk));
end;

end.

