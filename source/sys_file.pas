unit sys_file;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, sys_async, FileUtil;

const NewFileAge: Longint = -1;


type

  // Used when creating new files. The ID is system-specific.
  TTemplate = record
    Name: String;
    ID: String;
  end;

  TTemplateArray = array of TTemplate;

  TTemplateListCallback = procedure(Data: TTemplateArray) of object;

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
  public type
    // NOTE: In order to do this API, I need to be able to forward declare
    // The TFile record, because at least one of it's methods (ListFiles)
    // requires a type for an argument that references TFile (TDeferredFileListCallback).
    // (Among other issues)
    //
    // Options to resolve this situation that come up on the web:
    // - change to class, which can forward declare. This isn't applicable, because
    //   I want the "dynamic" memory allocation of the TFile.
    // - forward reference a pointer. This doesn't make sense, because the referencing
    //   type shouldn't have to use a pointer, and therefore dynamic memory allocation,
    //   it should be able to use a static variable.
    // - make use of a record helper to define the functions. This would actually
    //   work, but it has the unfortunate side effect of not providing code
    //   completion for the helper methods in Lazarus.
    // - Inner types are probably the best way to do this, even though it looks funny,
    //   it has the effect that I want.
    TFileArray = array of TFile;
    TDeferredFileListCallback = procedure(Data: TFileArray) of object;
    TReadFileCallback = procedure(aData: TStream; aFileAge: Longint) of object;
    TWriteFileCallback = procedure(aFileAge: Longint) of object;
  public
    property System: TFileSystemClass read fSystem;
    property ID: UTF8String read fID;
    procedure List(aCallback: TDeferredFileListCallback; aErrorBack: TDeferredExceptionCallback);
    procedure CheckExistence(aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
    procedure Read(aCallback: TReadFileCallback; aErrorback: TDeferredExceptionCallback);
    procedure Write(const aData: UTF8String; aCallback: TWriteFileCallback; aErrorback: TDeferredExceptionCallback); overload;
    procedure Write(aCreateDir: Boolean;
                    aCheckFileAge: Boolean;
                    aFileAge: Longint;
                    const aData: UTF8String;
                    aCallback: TWriteFileCallback;
                    aConflictBack: TWriteFileCallback;
                    aErrorback: TDeferredExceptionCallback); overload;
    procedure CopyTo(aTarget: TFile; aFlags: TCopyFileFlags; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); overload;
    procedure CopyTo(aTarget: TFile; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); overload;
    procedure Rename(aTarget: TFile; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
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

    procedure OpenInEditor;
    procedure ListTemplatesFor(aCallback: TTemplateListCallback; aErrorback: TDeferredExceptionCallback);
    procedure CreateFromTemplate(aTemplate: TTemplate; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
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
    // called from TFileSystem.

    class procedure ListFiles(aFile: TFile; aCallback: TFile.TDeferredFileListCallback; aErrorBack: TDeferredExceptionCallback); virtual; abstract;
    class procedure CheckFileExistence(aFile: TFile; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;
    class procedure ReadFile(aFile: TFile; aCallback: TFile.TReadFileCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;
    class procedure WriteFile(aFile: TFile;
                        aCreateDir: Boolean;
                        aCheckFileAge: Boolean;
                        aFileAge: Longint;
                        const aData: UTF8String;
                        aCallback: TFile.TWriteFileCallback;
                        aConflictBack: TFile.TWriteFileCallback;
                        aErrorback: TDeferredExceptionCallback); virtual; abstract;
    class procedure CopyFile(aSource: TFile; aTarget: TFile; aFlags: TCopyFileFlags; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;
    class procedure RenameFile(aSource: TFile; aTarget: TFile; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); virtual;
    class function GetDirectory(aFile: TFile): TFile; virtual; abstract;
    class function GetName(aFile: TFile): UTF8String; virtual; abstract;
    class function GetContainedFile(aFile: TFile; aName: UTF8String): TFile; virtual; abstract;
    class procedure OpenInEditor(aFile: TFile); virtual; abstract;
    class procedure GetTemplatesFor(aFile: TFile; aCallback: TTemplateListCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;
    class procedure CreateFileFromTemplate(aFile: TFile; aTemplate: TTemplate; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;

    class function GetFileSystemClass: TFileSystemClass; virtual; abstract;
    class procedure DoRenameFiles(aSource: TFile.TFileArray; aTarget: TFile.TFileArray; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback); virtual; abstract;
    class function GetFile(ID: String): TFile;
  public
    // allows batch renames of multiple files.
    class procedure RenameFiles(aSource: TFile.TFileArray; aTarget: TFile.TFileArray; aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
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
  strutils;

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

class procedure TFileSystem.RenameFile(aSource: TFile; aTarget: TFile;
  aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
var
  aSourceArr: TFile.TFileArray;
  aTargetArr: TFile.TFileArray;
begin
  SetLength(aSourceArr,1);
  aSourceArr[0] := aSource;
  SetLength(aTargetArr,1);
  aTargetArr[0] := aTarget;
  RenameFiles(aSourceArr,aTargetArr,aCallback,aErrorback);
end;

class procedure TFileSystem.RenameFiles(aSource: TFile.TFileArray;
  aTarget: TFile.TFileArray; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
var
  l: Integer;
  i: Integer;
  aSystem: TFileSystemClass;
begin
  aSystem := nil;
  l := Length(aSource);
  if l <> Length(aTarget) then
    raise Exception.Create('Batch file rename requires the same number of files in source and target.');
  if l = 0 then
    Exit;
  for i := 0 to l - 1 do
  begin
    if aSystem = nil then
      aSystem := aSource[i].System
    else if ((aSystem <> aSource[i].System) or
             (aSystem <> aTarget[i].System)) then
      raise Exception.Create('Can''t batch rename across file systems');
  end;
  aSystem.DoRenameFiles(aSource,aTarget,aCallback,aErrorback);
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

procedure TFile.List(aCallback: TDeferredFileListCallback;
  aErrorBack: TDeferredExceptionCallback);
begin
  fSystem.ListFiles(Self,aCallback,aErrorback);
end;

procedure TFile.CheckExistence(aCallback: TDeferredBooleanCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  fSystem.CheckFileExistence(Self,aCallback,aErrorback);

end;

procedure TFile.Read(aCallback: TReadFileCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  fSystem.ReadFile(Self,aCallback,aErrorback);
end;

procedure TFile.Write(const aData: UTF8String; aCallback: TWriteFileCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  Write(false,false,NewFileAge,aData,aCallback,Nil,aErrorback);

end;

procedure TFile.Write(aCreateDir: Boolean; aCheckFileAge: Boolean;
  aFileAge: Longint; const aData: UTF8String; aCallback: TWriteFileCallback;
  aConflictBack: TWriteFileCallback; aErrorback: TDeferredExceptionCallback);
begin
  fSystem.WriteFile(Self,aCreateDir,aCheckFileAge,aFileAge,aData,aCallback,aConflictBack,aErrorback);
end;

procedure TFile.CopyTo(aTarget: TFile; aFlags: TCopyFileFlags;
  aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
begin
  if aTarget.System <> fSystem then
     raise Exception.Create('Can''t copy files across file systems');
  fSystem.CopyFile(Self,aTarget,aFlags,aCallback,aErrorback);
end;

procedure TFile.CopyTo(aTarget: TFile; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  CopyTo(aTarget,[],aCallback,aErrorback);
end;

procedure TFile.Rename(aTarget: TFile; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  if aTarget.System <> fSystem then
     raise Exception.Create('Can''t move files across file systems');
  fSystem.RenameFile(Self,aTarget,aCallback,aErrorback);
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

procedure TFile.ListTemplatesFor(aCallback: TTemplateListCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  fSystem.GetTemplatesFor(Self,aCallback,aErrorback);
end;

procedure TFile.CreateFromTemplate(aTemplate: TTemplate;
  aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
begin
  fSystem.CreateFileFromTemplate(Self,aTemplate,aCallback,aErrorback);
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

