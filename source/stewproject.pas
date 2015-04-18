unit stewproject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewfile, stewasync;

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
    procedure Exists(aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
    function GetProjectName: String;
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

constructor TStewProject.Create(const Path: TFilename);
begin
  fDisk := IncludeTrailingPathDelimiter(Path);
end;

destructor TStewProject.Destroy;
begin
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

procedure TStewProject.Exists(aCallback: TDeferredBooleanCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  TFileExists.Create(fDisk,aCallback,aErrorback).Enqueue;
end;

function TStewProject.GetProjectName: String;
begin
  result := ExtractFileName(ExcludeTrailingPathDelimiter(fDisk));
end;

end.

