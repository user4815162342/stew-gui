unit sys_file;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, sys_async;

const NewFileAge: Longint = -1;

  // TODO: Google Drive allows multiple files with the same name. This system
  // will have to account for it: ID's are separate from names, even if they're
  // the same thing in most systems. I also have to consider the possibility of
  // a file system using mime-types instead of extensions (as I
  // believe Google Drive does), that will cause an even bigger problem. And,
  // if I go that far, I could consider using other mechanisms for descriptors:
  // for example, google drive has file labels which could be used instead.
  // Another issue: Google Drive also allows a file to be contained in multiple
  // folders (this isn't just links).

type

  // Used when creating new files. The ID is system-specific.
  TTemplate = record
    Name: String;
    ID: String;
  end;

  TTemplateArray = array of TTemplate;

  TTemplateListCallback = procedure(Data: TTemplateArray) of object;

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
    function CheckExistence: TBooleanPromise;
    function Read: TFileReadPromise;
    function Write(const aData: UTF8String): TFileWritePromise; overload;
    function Write(aOptions: TFileWriteOptions;
                    const aData: UTF8String): TFileWritePromise; overload;
    function Write(aOptions: TFileWriteOptions;
                    aFileAge: Longint;
                    const aData: UTF8String): TFileWritePromise; overload;
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
  end;

  TFileArray = array of TFile;
  { TFileListPromise }

  TFileListPromise = class(TPromise)
  private
    fPath: TFile;
  protected
    fFiles: TFileArray;
  public
    constructor Enqueue(aFile: TFile);
    property Files: TFileArray read fFiles;
    property Path: TFile read fPath;
  end;

  { TFileCheckExistencePromise }

  TCheckExistencePromise = class(TBooleanPromise)
  private
    fPath: Tfile;
  public
    constructor Enqueue(aFile: TFile);
    property Path: TFile read fPath;
  end;

  { TFileReadPromise }

  // TODO: Very important... we need to make sure the stream gets freed!
  // TODO: Another possibility: Most of the time we're just reading the contents
  // of the stream into memory, what if this doesn't return the stream, but the
  // data itself (after closing the stream)? We might have to have some mechanism
  // for inserting a parser or something in there to return different kinds of
  // data, however. The other possibility is the Read isn't a promise, because
  // we don't want to risk someone else trying to read a stream that's already
  // been read.
  TFileReadPromise = class(TPromise)
  private
    fPath: Tfile;
  protected
    fData: TStream;
    fAge: Longint;
  public
    constructor Enqueue(aFile: TFile);
    property Path: TFile read FPath;
    property Data: TStream read FData;
    property Age: Longint read FAge;
  end;

  { TFileWritePromise }

  TFileWritePromise = class(TPromise)
  private
    fPath: Tfile;
    fOptions: TFileWriteOptions;
    fData: UTF8String;
  protected
    fAge: Longint;
    fIsConflict: Boolean;
  public
    constructor Enqueue(aFile: TFile; aOptions: TFileWriteOptions; aFileAge: Longint; const aData: UTF8String); overload;
    constructor Enqueue(aFile: TFile; aOptions: TFileWriteOptions; const aData: UTF8String); overload;
    constructor Enqueue(aFile: TFile; const aData: UTF8String); overload;
    property Data: UTF8String read fData;
    property Path: TFile read fPath;
    property Age: Longint read fAge;
    property Options: TFileWriteOptions read fOptions;
    property IsConflict: Boolean read fIsConflict;
  end;

  { TFileCopyPromise }

  TFileCopyPromise = class(TPromise)
  private
    fSource: TFile;
    fTarget: TFile;
    fOptions: TFileCopyOptions;
  public
    constructor Enqueue(aSource: TFile; aTarget: TFile; aOptions: TFileCopyOptions); overload;
    constructor Enqueue(aSource: TFile; aTarget: TFile); overload;
    property Source: TFile read fSource;
    property Target: TFile read fTarget;
    property Options: TFileCopyOptions read fOptions;
  end;

  { TFileRenamePromise }

  TFileRenamePromise = class(TPromise)
  private
    fSource: TFileArray;
    fTarget: TFileArray;
  public
    constructor Enqueue(aSource: TFileArray; aTarget: TFileArray);
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
    constructor Enqueue(aFile: TFile);
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
    class function CheckFileExistence(aFile: TFile): TBooleanPromise; virtual; abstract;
    class function ReadFile(aFile: TFile): TFileReadPromise; virtual; abstract;
    class function WriteFile(aFile: TFile; aOptions: TFileWriteOptions;
                        aFileAge: Longint;
                        const aData: UTF8String): TFileWritePromise; virtual; abstract;
    class function CopyFile(aSource: TFile; aTarget: TFile; aOptions: TFileCopyOptions): TFileCopyPromise; virtual; abstract;
    class function RenameFile(aSource: TFile; aTarget: TFile): TFileRenamePromise; virtual;
    class function GetDirectory(aFile: TFile): TFile; virtual; abstract;
    class function GetName(aFile: TFile): UTF8String; virtual; abstract;
    class function GetContainedFile(aFile: TFile; aName: UTF8String): TFile; virtual; abstract;
    class procedure OpenInEditor(aFile: TFile); virtual; abstract;
    class function GetTemplatesFor(aFile: TFile): TFileListTemplatesPromise; virtual; abstract;
    class function CreateFileFromTemplate(aFile: TFile; aTemplate: TTemplate): TFileCopyPromise; virtual; abstract;

    class function GetFileSystemClass: TFileSystemClass; virtual; abstract;
    class function GetFile(ID: String): TFile;
  public
    // allows batch renames of multiple files.
    class function RenameFiles(aSource: TFileArray; aTarget: TFileArray): TFileRenamePromise; virtual; abstract;
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
  end;

  const
    ExtensionDelimiter: Char = '.';
    DescriptorDelimiter: Char = '_';

  operator = (a: TFile; b: TFile): Boolean;



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

{ TFileListTemplatesPromise }

constructor TFileListTemplatesPromise.Enqueue(aFile: TFile);
begin
  fPath := aFile;
  inherited Enqueue;
end;

{ TFileRenamePromise }

constructor TFileRenamePromise.Enqueue(aSource: TFileArray;
  aTarget: TFileArray);
var
  l: Integer;
  i: Integer;
  aSystem: TFileSystemClass;
begin
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
  inherited Enqueue;
end;

{ TFileCopyPromise }

constructor TFileCopyPromise.Enqueue(aSource: TFile; aTarget: TFile;
  aOptions: TFileCopyOptions);
begin
  if aTarget.System <> aSource.System then
     raise Exception.Create('Can''t copy files across file systems');
  fSource := aSource;
  fTarget := aTarget;
  fOptions := aOptions;
  inherited Enqueue;
end;

constructor TFileCopyPromise.Enqueue(aSource: TFile; aTarget: TFile);
begin
  Enqueue(aSource,aTarget,[]);
end;

{ TFileWritePromise }

constructor TFileWritePromise.Enqueue(aFile: TFile;
  aOptions: TFileWriteOptions; aFileAge: Longint; const aData: UTF8String);
begin
  fIsConflict := false;
  fPath := aFile;
  fOptions := aOptions;
  fAge := aFileAge;
  fData := aData;
  inherited Enqueue;
end;

constructor TFileWritePromise.Enqueue(aFile: TFile;
  aOptions: TFileWriteOptions; const aData: UTF8String);
begin
  Enqueue(aFile,aOptions,NewFileAge,aData);
end;

constructor TFileWritePromise.Enqueue(aFile: TFile; const aData: UTF8String);
begin
  Enqueue(aFile,[],NewFileAge,aData);
end;

{ TFileReadPromise }

constructor TFileReadPromise.Enqueue(aFile: TFile);
begin
  fPath := aFile;
  inherited Enqueue;

end;

{ TFileCheckExistencePromise }

constructor TCheckExistencePromise.Enqueue(aFile: TFile);
begin
  fPath := aFile;
  inherited Enqueue;
end;

{ TFileListPromise }

constructor TFileListPromise.Enqueue(aFile: TFile);
begin
  fPath := aFile;
  inherited Enqueue;
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
  result.fID := ID;
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

function TFile.CheckExistence: TBooleanPromise;
begin
  result := fSystem.CheckFileExistence(Self);

end;

function TFile.Read: TFileReadPromise;
begin
  result := fSystem.ReadFile(Self);
end;

function TFile.Write(const aData: UTF8String): TFileWritePromise;
begin
  result := fSystem.WriteFile(Self,[],NewFileAge,aData);

end;

function TFile.Write(aOptions: TFileWriteOptions; const aData: UTF8String
  ): TFileWritePromise;
begin
  result := fSystem.WriteFile(Self,aOptions,NewFileAge,aData);
end;

function TFile.Write(aOptions: TFileWriteOptions; aFileAge: Longint;
  const aData: UTF8String): TFileWritePromise;
begin
  result := fSystem.WriteFile(Self,aOptions,aFileAge,aData);
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

operator=(a: TFile; b: TFile): Boolean;
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
  if dotAndDashify and (aDescriptor = DescriptorDelimiter) and (Pos(DescriptorDelimiter,aPacketName) > 0) then
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
     result := Copy(Result,0,_p - 1);
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

