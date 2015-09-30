unit sys_file;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, sys_async, sys_types;

const NewFileAge: Longint = -1;

type

  // Used when creating new files. The ID is system-specific.
  TTemplate = record
    Name: String;
    ID: String;
  end;

  TTemplateArray = array of TTemplate;

  TTemplateListCallback = procedure(Data: TTemplateArray) of object;

  TFileExistencePromise = class;

  TFileListPromise = class;

  TFileReadPromise = class;

  TFileWriteOption = (fwoCreateDir, fwoCheckAge);
  TFileWriteOptions = set of TFileWriteOption;
  TFileWritePromise = class;

  TFileCopyOption = (fcoOverwrite, fcoCreateDir);
  TFileCopyOptions = set of TFileCopyOption;

  TFileCopyPromise = class;

  TFileRenamePromise = class;

  TFileListTemplatesPromise = class;

  TFileSystem = class;

  TFileSystemClass = class of TFileSystem;

  { TFile }
  // allows encapsulation of a file object. It's built as a record
  // to allow quick allocation on the stack, and remove the need for
  // freeing objects later.
  TFile = record
  private
    fSystem: TFileSystemClass;
    fID: UTF8String;
    function GetBaseName: UTF8String;
    function GetDescriptor: UTF8String;
    function GetDirectory: TFile;
    function GetExtension: UTF8String;
    function GetName: UTF8String;
    function GetPacketName: UTF8String;
  public
    property System: TFileSystemClass read fSystem;
    property ID: UTF8String read fID;
    function List: TFileListPromise;
    function CheckExistence: TFileExistencePromise;
    // The convertor is a procedure that converts the stream into whatever
    // data format is actually required. The data is *not* available on the
    // TFileReadPromise itself, the promise only reports when the reading
    // is complete. If nil is passed, then the DataString property on the Promise
    // should contain the contents of the file as a string.
    function Read: TFileReadPromise;
    // The TFileWritePromise includes a 'Data' stream which can be used to write the
    // data to be written into the file. Or, to write simple strings, use the
    // overloads which take a text parameter at the end.
    function Write: TFileWritePromise; overload;
    // The 'Writer' is owned by the promise and destroyed when that is destroyed.
    function Write(aOptions: TFileWriteOptions): TFileWritePromise; overload;
    // The 'Writer' is owned by the promise and destroyed when that is destroyed.
    function Write(aOptions: TFileWriteOptions;
                    aFileAge: Longint): TFileWritePromise; overload;
    function Write(aText: UTF8String): TFileWritePromise; overload;
    // The 'Writer' is owned by the promise and destroyed when that is destroyed.
    function Write(aOptions: TFileWriteOptions; aText: UTF8String): TFileWritePromise; overload;
    // The 'Writer' is owned by the promise and destroyed when that is destroyed.
    function Write(aOptions: TFileWriteOptions;
                    aFileAge: Longint; aText: UTF8String): TFileWritePromise; overload;
    function CopyTo(aTarget: TFile; aFlags: TFileCopyOptions): TFileCopyPromise; overload;
    function CopyTo(aTarget: TFile): TFileCopyPromise; overload;
    function Rename(aTarget: TFile): TFileRenamePromise;
    procedure OpenInEditor;
    function ListTemplatesFor: TFileListTemplatesPromise;
    function CreateFromTemplate(aTemplate: TTemplate): TFileCopyPromise;
    property BaseName: UTF8String read GetBaseName;
    property PacketName: UTF8String read GetPacketName;
    property Name: UTF8String read GetName;
    property Directory: TFile read GetDirectory;
    property Extension: UTF8String read GetExtension;
    property Descriptor: UTF8String read GetDescriptor;
    function GetContainedFile(aPacketName: UTF8String;
      aDescriptor: UTF8String; aExtension: UTF8String; dotAndDashify: Boolean): TFile;
    function GetContainedFile(aName: UTF8String): TFile;
    function WithDifferentExtension(aExt: UTF8String): TFile;
    function WithDifferentDescriptorAndExtension(aDesc: UTF8String; aExt: UTF8String): TFile;
    function WithDifferentPacketName(aName: UTF8String): TFile;
    function Contains(aFile: TFile): Boolean;
    function SplitPath(aBasePath: TFile): TStringArray;
    class operator = (a: TFile; b: TFile): Boolean;
  end;

  TFileArray = array of TFile;

  { TFileInfo }

  TFileInfo = record
    Item: TFile;
    IsFolder: Boolean;
    class function Make(aItem: TFile; aIsFolder: Boolean): TFileInfo; static;
  end;

  TFileInfoArray = array of TFileInfo;

  { TFileListPromise }

  TFileListPromise = class(TPromise)
  private
    fPath: TFile;
  protected
    fFilesInfo: TFileInfoArray;
    fExists: Boolean;
    fIsFolder: Boolean;
  public
    constructor Create(aFile: TFile);
    procedure SetAnswer(aFiles: TFileInfoArray; aExists: Boolean; aIsFolder: Boolean
      );
    property FilesInfo: TFileInfoArray read fFilesInfo;
    property Path: TFile read fPath;
    property Exists: Boolean read fExists;
    property IsFolder: Boolean read fIsFolder;
    function GetJustFiles: TFileArray;
  end;

  { TFileCheckExistencePromise }

  { TFileExistencePromise }

  TFileExistencePromise = class(TPromise)
  private
    fPath: Tfile;
  protected
    fExists: Boolean;
    fIsFolder: Boolean;
  public
    constructor Create(aFile: TFile);
    procedure SetAnswer(aExists: Boolean; aIsFolder: Boolean);
    property Path: TFile read fPath;
    property Exists: Boolean read fExists;
    property IsFolder: Boolean read fIsFolder;
  end;

  { TFileReadPromise }

  TFileReadPromise = class(TPromise)
  private
    fPath: Tfile;
    function GetDoesNotExist: Boolean;
  protected
    fDataString: UTF8String;
    fAge: Longint;
    fExists: Boolean;
    fIsFolder: Boolean;
    fStream: TStream;
  public
    // The 'Reader' is owned by the promise and destroyed when that is destroyed.
    constructor Create(aFile: TFile);
    destructor Destroy; override;
    procedure SetAnswer(aStream: TStream; aAge: Longint;
      aExists: Boolean; aIsFolder: Boolean);
    property Path: TFile read FPath;
    property Age: Longint read FAge;
    property DoesNotExist: Boolean read GetDoesNotExist; deprecated;
    property Exists: Boolean read fExists;
    property IsFolder: Boolean read fIsFolder;
    // Keep in mind that the stream is destroyed with the promise...
    property Data: TStream read fStream;
    function ReadString: UTF8String;
  end;

  { TFileWritePromise }

  TFileWritePromise = class(TPromise)
  private
    fPath: Tfile;
  protected
    fAge: Longint;
    fIsConflict: Boolean;
    fStream: TStream;
  public
    constructor Create(aFile: TFile; aData: TStream); overload;
    destructor Destroy; override;
    procedure SetAnswer(aAge: Longint; aIsConflict: Boolean);
    property Path: TFile read fPath;
    property Age: Longint read fAge;
    property IsConflict: Boolean read fIsConflict;
    property Data: TStream read fStream;
    procedure WriteString(aData: UTF8String);
    function ReadString: UTF8String;
  end;

  { TFileWriteRequest }

  { TFileCopyPromise }

  TFileCopyPromise = class(TPromise)
  private
    fSource: TFile;
    fTarget: TFile;
  public
    constructor Create(aSource: TFile; aTarget: TFile);
    property Source: TFile read fSource;
    property Target: TFile read fTarget;
  end;

  { TFileRenamePromise }

  TFileRenamePromise = class(TPromise)
  private
    fSource: TFileArray;
    fTarget: TFileArray;
  public
    constructor Create(aSource: TFileArray; aTarget: TFileArray);
    property Source: TFileArray read fSource;
    property Target: TFileArray read fTarget;
  end;

  { TFileListTemplatesPromise }

  TFileListTemplatesPromise = class(TPromise)
  private
    fPath: TFile;
  protected
    fTemplates: TTemplateArray;
  public
    constructor Create(aFile: TFile);
    procedure SetAnswer(aTemplates: TTemplateArray);
    property Templates: TTemplateArray read fTemplates;
    property Path: TFile read fPath;
  end;

  { TFileSystem }
  // This is a generic interface for accessing file systems asynchronously.
  // It can be extended to support various back-end systems. Right now, only
  // the local file system will be available, but I'd like to someday support
  // more:
  // FUTURE: The following systems could be supported
  // - THTTPFileSystem: read-only system that reads stuff off of a remote web server
  // - TWebDAVFileSystem: writable http system
  // - TSSHFileSystem: secure networked file system, although this might be better
  //                   to just use a driver in the local operating system.
  // - TGoogleDriveFileSystem: access Google drive
  // - TDropBoxFileSystem: access Dropbox.
  // The biggest problem with most of these is actually the directory browser
  // to choose the location.

  TFileSystem = class
  protected
    // Almost functions are protected, because they should really only be
    // called from TFile.

    class function ListFiles(aFile: TFile): TFileListPromise; virtual; abstract;
    class function CheckFileExistence(aFile: TFile): TFileExistencePromise; virtual; abstract;
    // The 'Reader' is owned by the promise and destroyed when that is destroyed.
    class function ReadFile(aFile: TFile): TFileReadPromise; virtual; abstract;
    // The 'Writer' is owned by the promise and destroyed when that is destroyed.
    class function WriteFile(aFile: TFile; aOptions: TFileWriteOptions;
                        aFileAge: Longint): TFileWritePromise; virtual; abstract;
    class function CopyFile(aSource: TFile; aTarget: TFile; aOptions: TFileCopyOptions): TFileCopyPromise; virtual; abstract;
    class function RenameFile(aSource: TFile; aTarget: TFile): TFileRenamePromise; virtual;
    class function GetDirectory(aFile: TFile): TFile; virtual; abstract;
    class function GetName(aFile: TFile): UTF8String; virtual; abstract;
    class function GetContainedFile(aFile: TFile; aName: UTF8String): TFile; virtual; abstract;
    class procedure OpenInEditor(aFile: TFile); virtual; abstract;
    class function GetTemplatesFor(aFile: TFile): TFileListTemplatesPromise; virtual; abstract;
    class function CreateFileFromTemplate(aFile: TFile; aTemplate: TTemplate): TFileCopyPromise; virtual; abstract;
    class function SplitPath(aFile: TFile; aBasePath: TFile): TStringArray; virtual; abstract;

    class function GetFileSystemClass: TFileSystemClass; virtual; abstract;
  public
    // allows batch renames of multiple files.
    class function RenameFiles(aSource: TFileArray; aTarget: TFileArray): TFileRenamePromise; virtual; abstract;
    class function GetFile(ID: String): TFile;
  end;

  { TFileList }

  TFileList = class
  private
    fSystem: TFileSystemClass;
    // This is kind of a hack, I may change this someday to use a TList or something,
    // but this works for now.
    fList: TStringList;
    function GetItem(Index: Integer): TFile;
  public
    constructor Create(aSystem: TFileSystemClass);
    destructor Destroy; override;
    procedure Add(aFile: TFile);
    procedure Delete(Index: Integer);
    function Count: Integer;
    function IndexOf(aFile: TFile): Integer;
    property Item[Index: Integer]: TFile read GetItem; default;
  end deprecated;

  const
    ExtensionDelimiter: Char = '.';
    DescriptorDelimiter: Char = '_';




  function BuildFileName(aPacketName: UTF8String;
      aDescriptor: UTF8String; aExtension: UTF8String; dotAndDashify: Boolean): UTF8String;
  // some path functions
  function ExtractFileNameWithoutDescriptor(const Filename: UTF8String): UTF8String;
  function ExtractFileDescriptor(const Filename: UTF8String): UTF8String;
  function IncludeExtensionDelimiter(const aExt: UTF8String): UTF8String;
  function ExcludeExtensionDelimiter(const aExt: UTF8String): UTF8String;
  function IncludeDescriptorDelimiter(const aExt: UTF8String): UTF8String;
  function ExcludeDescriptorDelimiter(const aExt: UTF8String): UTF8String;


implementation

uses
  strutils, FileUtil;

{ TFileInfo }

class function TFileInfo.Make(aItem: TFile; aIsFolder: Boolean): TFileInfo;
begin
  result.Item := aItem;
  result.IsFolder := aIsFolder;
end;

{ TFileListTemplatesPromise }

constructor TFileListTemplatesPromise.Create(aFile: TFile);
begin
  inherited Create;
  fPath := aFile;
end;

procedure TFileListTemplatesPromise.SetAnswer(aTemplates: TTemplateArray);
begin
  fTemplates := aTemplates;
end;

{ TFileRenamePromise }

constructor TFileRenamePromise.Create(aSource: TFileArray;
  aTarget: TFileArray);
var
  l: Integer;
  i: Integer;
  aSystem: TFileSystemClass;
begin
  inherited Create;
  aSystem := nil;
  l := Length(aSource);
  if l <> Length(aTarget) then
    raise Exception.Create('Batch file rename requires the same number of files in source and target.');
  SetLength(fSource,l);
  SetLength(fTarget,l);
  for i := 0 to l - 1 do
  begin
    if aSystem = nil then
      aSystem := aSource[i].System
    else if ((aSystem <> aSource[i].System) or
             (aSystem <> aTarget[i].System)) then
      raise Exception.Create('Can''t batch rename across file systems');
    fSource[i] := aSource[i];
    fTarget[i] := aTarget[i];
  end;
end;

{ TFileCopyPromise }

constructor TFileCopyPromise.Create(aSource: TFile; aTarget: TFile);
begin
  inherited Create;
  if aTarget.System <> aSource.System then
     raise Exception.Create('Can''t copy files across file systems');
  fSource := aSource;
  fTarget := aTarget;
end;

{ TFileWritePromise }

constructor TFileWritePromise.Create(aFile: TFile; aData: TStream);
begin
  inherited Create;
  fIsConflict := false;
  fPath := aFile;
  fStream := aData;
  if fStream = nil then
    fStream := TMemoryStream.Create;
end;

destructor TFileWritePromise.Destroy;
begin
  FreeAndNil(fStream);
  inherited Destroy;
end;

procedure TFileWritePromise.SetAnswer(aAge: Longint; aIsConflict: Boolean);
begin
  fAge := aAge;
  fIsConflict := aIsConflict;
end;

procedure TFileWritePromise.WriteString(aData: UTF8String);
var
  lInput: TStringStream;
begin
  lInput := TStringStream.Create(aData);
  try
    Data.CopyFrom(lInput,0);
  finally
    lInput.Free;
  end;

end;

function TFileWritePromise.ReadString: UTF8String;
var
  lTarget: TStringStream;
begin
  if fStream <> nil then
  begin
    if fStream is TStringStream then
      result := (fStream as TStringStream).DataString
    else
    begin
      lTarget := TStringStream.Create('');
      try
        lTarget.CopyFrom(fStream,0);
        result := lTarget.DataString;
      finally
        lTarget.Free;
      end;
    end;
  end

end;

{ TFileReadPromise }

function TFileReadPromise.GetDoesNotExist: Boolean;
begin
  result := not fExists;
end;

constructor TFileReadPromise.Create(aFile: TFile);
begin
  inherited Create;
  fPath := aFile;
end;

destructor TFileReadPromise.Destroy;
begin
  if fStream <> nil then
  begin
    FreeAndNil(fStream);
  end;
  inherited Destroy;
end;

procedure TFileReadPromise.SetAnswer(aStream: TStream; aAge: Longint;
  aExists: Boolean; aIsFolder: Boolean);
begin
  fStream := aStream;
  fAge := aAge;
  fExists := aExists;
  fIsFolder := aIsFolder;
end;

function TFileReadPromise.ReadString: UTF8String;
var
  lTarget: TStringStream;
begin
  if fStream <> nil then
  begin
    if fStream is TStringStream then
      result := (fStream as TStringStream).DataString
    else
    begin
      lTarget := TStringStream.Create('');
      try
        lTarget.CopyFrom(fStream,0);
        result := lTarget.DataString;
      finally
        lTarget.Free;
      end;
    end;
  end


end;

{ TFileCheckExistencePromise }

constructor TFileExistencePromise.Create(aFile: TFile);
begin
  inherited Create;
  fPath := aFile;
end;

procedure TFileExistencePromise.SetAnswer(aExists: Boolean; aIsFolder: Boolean);
begin
  fExists := aExists;
  fIsFolder := aIsFolder;
end;

{ TFileListPromise }

constructor TFileListPromise.Create(aFile: TFile);
begin
  inherited Create;
  fPath := aFile;
  fExists := false;
  fIsFolder := false;
end;

procedure TFileListPromise.SetAnswer(aFiles: TFileInfoArray; aExists: Boolean;
  aIsFolder: Boolean);
begin
  fFilesInfo := aFiles;
  fExists := aExists;
  fIsFolder := aIsFolder;
end;

function TFileListPromise.GetJustFiles: TFileArray;
var
  i: Integer;
  l: Integer;
begin
  l := Length(fFilesInfo);
  SetLength(Result,l);
  for i := 0 to l - 1 do
  begin
    result[i] := fFilesInfo[i].Item;
  end;

end;

{ TFileList }

function TFileList.GetItem(Index: Integer): TFile;
begin
  result := fSystem.GetFile(fList[Index]);
end;

constructor TFileList.Create(aSystem: TFileSystemClass);
begin
  inherited Create;
  fSystem := aSystem;
  fList := TStringList.Create;
end;

destructor TFileList.Destroy;
begin
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure TFileList.Add(aFile: TFile);
begin
  fList.Add(aFile.ID);
end;

procedure TFileList.Delete(Index: Integer);
begin
  fList.Delete(Index);
end;

function TFileList.Count: Integer;
begin
  result := fList.Count;
end;

function TFileList.IndexOf(aFile: TFile): Integer;
begin
  result := -1;
  if aFile.System = fSystem then
     result := fList.IndexOf(aFile.ID);
end;

{ TFileSystem }

class function TFileSystem.RenameFile(aSource: TFile; aTarget: TFile
  ): TFileRenamePromise;
var
  aSourceArr: TFileArray;
  aTargetArr: TFileArray;
begin
  SetLength(aSourceArr,1);
  aSourceArr[0] := aSource;
  SetLength(aTargetArr,1);
  aTargetArr[0] := aTarget;
  result := RenameFiles(aSourceArr,aTargetArr);
end;

class function TFileSystem.GetFile(ID: String): TFile;
begin
  result.fSystem := GetFileSystemClass;
  result.fID := ExcludeTrailingPathDelimiter(ID);
end;

{ TFile }

function TFile.GetBaseName: UTF8String;
begin
  result := ExtractFileNameOnly(Name);
end;

function TFile.GetDescriptor: UTF8String;
begin
  result := ExcludeDescriptorDelimiter(ExtractFileDescriptor(Name));
end;

function TFile.GetDirectory: TFile;
begin
  result := fSystem.GetDirectory(Self);
end;

function TFile.GetExtension: UTF8String;
begin
  result := ExcludeExtensionDelimiter(ExtractFileExt(Name));
end;

function TFile.GetName: UTF8String;
begin
  result := fSystem.GetName(Self);
end;

function TFile.GetPacketName: UTF8String;
begin
  result := ExtractFileNameWithoutDescriptor(Name);
end;

function TFile.List: TFileListPromise;
begin
  result := fSystem.ListFiles(Self);
end;

function TFile.CheckExistence: TFileExistencePromise;
begin
  result := fSystem.CheckFileExistence(Self);

end;

function TFile.Read: TFileReadPromise;
begin
  result := fSystem.ReadFile(Self);
end;

function TFile.Write: TFileWritePromise;
begin
  result := fSystem.WriteFile(Self,[],NewFileAge);

end;

function TFile.Write(aOptions: TFileWriteOptions): TFileWritePromise;
begin
  result := fSystem.WriteFile(Self,aOptions,NewFileAge);
end;

function TFile.Write(aOptions: TFileWriteOptions; aFileAge: Longint
  ): TFileWritePromise;
begin
  result := fSystem.WriteFile(Self,aOptions,aFileAge);
end;

function TFile.Write(aText: UTF8String): TFileWritePromise;
begin
  // Okay, this is slightly annoying. You'd think the class scope would have
  // priority over the system procedure, so I wouldn't have to add the 'Self'.
  // But, good thing I've got tests that picked this up.
  result := Self.Write;
  Result.WriteString(aText);
end;

function TFile.Write(aOptions: TFileWriteOptions; aText: UTF8String
  ): TFileWritePromise;
begin
  result := Write(aOptions);
  Result.WriteString(aText);

end;

function TFile.Write(aOptions: TFileWriteOptions; aFileAge: Longint;
  aText: UTF8String): TFileWritePromise;
begin
  result := Write(aOptions,aFileAge);
  result.WriteString(aText);

end;

function TFile.CopyTo(aTarget: TFile; aFlags: TFileCopyOptions
  ): TFileCopyPromise;
begin
  result := fSystem.CopyFile(Self,aTarget,aFlags);
end;

function TFile.CopyTo(aTarget: TFile): TFileCopyPromise;
begin
  result := CopyTo(aTarget,[]);
end;

function TFile.Rename(aTarget: TFile): TFileRenamePromise;
begin
  result := fSystem.RenameFile(Self,aTarget);
end;

function TFile.GetContainedFile(aPacketName: UTF8String;
  aDescriptor: UTF8String; aExtension: UTF8String; dotAndDashify: Boolean
  ): TFile;
begin
  result := fSystem.GetContainedFile(Self,BuildFileName(aPacketName,aDescriptor,aExtension,dotAndDashify));
end;

function TFile.GetContainedFile(aName: UTF8String): TFile;
begin
  result := fSystem.GetContainedFile(Self,aName);
end;

function TFile.WithDifferentExtension(aExt: UTF8String): TFile;
begin
  result := Directory.GetContainedFile(PacketName,Descriptor,aExt,true);
end;

function TFile.WithDifferentDescriptorAndExtension(aDesc: UTF8String;
  aExt: UTF8String): TFile;
begin
  result := Directory.GetContainedFile(PacketName,aDesc,aExt,true);
end;

function TFile.WithDifferentPacketName(aName: UTF8String): TFile;
begin
  result := Directory.GetContainedFile(aName,Descriptor,Extension,true);
end;

function TFile.Contains(aFile: TFile): Boolean;
var
  lChild: TFile;
  lDirectory: TFile;
begin
  result := false;
  lChild := aFile;
  lDirectory := aFile.Directory;
  while (lDirectory <> lChild) and (lChild.ID <> '') do
  begin
    result := lDirectory = Self;
    if result then
       break;
    lChild := lDirectory;
    lDirectory := lChild.Directory;
  end;
end;

function TFile.SplitPath(aBasePath: TFile): TStringArray;
begin
  result := fSystem.SplitPath(Self,aBasePath);
end;

procedure TFile.OpenInEditor;
begin
  fSystem.OpenInEditor(Self);
end;

function TFile.ListTemplatesFor: TFileListTemplatesPromise;
begin
  result := fSystem.GetTemplatesFor(Self);
end;

function TFile.CreateFromTemplate(aTemplate: TTemplate): TFileCopyPromise;
begin
  result := fSystem.CreateFileFromTemplate(Self,aTemplate);
end;

class operator TFile.=(a: TFile; b: TFile): Boolean;
begin
  result := (a.System = b.System) and (a.ID = b.ID);
end;

function BuildFileName(aPacketName: UTF8String; aDescriptor: UTF8String;
  aExtension: UTF8String; dotAndDashify: Boolean): UTF8String;
begin
  if aExtension <> '' then
    aExtension := IncludeExtensionDelimiter(aExtension);
  if aDescriptor <> '' then
    aDescriptor := IncludeDescriptorDelimiter(aDescriptor);
  if dotAndDashify and (aExtension = '') and (Pos(ExtensionDelimiter,aPacketName) > 0) then
    aExtension := ExtensionDelimiter;
  if dotAndDashify and (aDescriptor = '') and (Pos(DescriptorDelimiter,aPacketName) > 0) then
    aDescriptor := DescriptorDelimiter;
  result := aPacketName + aDescriptor + aExtension;

end;

function ExtractFileNameWithoutDescriptor(const Filename: UTF8String): UTF8String;
var
  _p: Integer;
begin
  result := ExtractFileNameWithoutExt(Filename);
  _p := RPos(DescriptorDelimiter,Result);
  if _p <> 0 then
  begin
     result := Copy(Result,1,_p - 1);
  end;

end;

function ExtractFileDescriptor(const Filename: UTF8String): UTF8String;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[DescriptorDelimiter];
  while (I > 0) and not (FileName[I] in EndSep) do
    Dec(I);
  if (I > 0) and (FileName[I] = DescriptorDelimiter) then
  begin
    Result := Copy(FileName, I, MaxInt);
    Result := ExtractFileNameWithoutExt(Result);
  end
  else
    Result := '';
end;

function IncludeExtensionDelimiter(const aExt: UTF8String): UTF8String;
begin
  result := aExt;
  if (Length(result) > 0) then
  begin
    if result[1] <> ExtensionDelimiter then
      result := ExtensionDelimiter + result;
  end;
end;

function ExcludeExtensionDelimiter(const aExt: UTF8String): UTF8String;
begin
  result := aExt;
  if (Length(result) > 0) then
  begin
    if result[1] = ExtensionDelimiter then
      result := Copy(result,2,Length(result));
  end;
end;

function IncludeDescriptorDelimiter(const aExt: UTF8String): UTF8String;
begin
  result := aExt;
  if (Length(result) > 0) then
  begin
    if result[1] <> DescriptorDelimiter then
      result := DescriptorDelimiter + result;
  end
  else
    result := '';
end;

function ExcludeDescriptorDelimiter(const aExt: UTF8String): UTF8String;
begin
  result := aExt;
  if (Length(result) > 0) then
  begin
    if result[1] = DescriptorDelimiter then
      result := Copy(result,2,Length(result));
  end;

end;




end.

