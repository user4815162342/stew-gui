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
  // folders (this isn't just symlinks). In fact, defining a file by it's
  // name doesn't even work, so the GetContainedFile isn't going to work.
  // -- back to the drawing board! Two things have to be done:
  // * Probably our best bet is to stop using
  // paths completely, and especially stop using the ID to denote the path.
  // You would create a LocalFile given a local path, sure, and internally the
  // local file would use the path as an ID, but you wouldn't actually use the
  // ID anywhere else. This ID would then be used as an actual ID for Google Drive.
  // * GetContainedFile would have to be changed. Instead of indicating a file
  // contained in a parent, it's a hypothetical file contained inside that
  // parent, so it would be called GetHypotheticalContainedFile.
  // Actually getting the real file would require a "List" async command,
  // but you could turn a hypothetical file into a real file when writing to it.
  // * We'd still depend on naming when looking in Google Drive, because there's
  // no other way to specify where the _stew.json file might be, or things like
  // that.
  // * We have to accept that there might be multiple files with the same name.
  // I'm almost doing that anyway, since files can have the same name and different
  // extensions. We just have to provide a file age when differentiating with the
  // user.

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
  end;

  TFileArray = array of TFile;
  { TFileListPromise }

  TFileListPromise = class(TPromise)
  private
    fPath: TFile;
  protected
    fFiles: TFileArray;
    fDoesNotExist: Boolean;
  public
    constructor Create(aFile: TFile);
    procedure SetAnswer(aFiles: TFileArray; aDoesNotExist: Boolean);
    property Files: TFileArray read fFiles;
    property Path: TFile read fPath;
    property DoesNotExist: Boolean read fDoesNotExist;
  end;

  { TFileCheckExistencePromise }

  { TFileExistencePromise }

  TFileExistencePromise = class(TPromise)
  private
    fPath: Tfile;
  protected
    fExists: Boolean;
  public
    constructor Create(aFile: TFile);
    procedure SetAnswer(aExists: Boolean);
    property Path: TFile read fPath;
    property Exists: Boolean read fExists;
  end;

  { TFileReadPromise }

  TFileReadPromise = class(TPromise)
  private
    fPath: Tfile;
  protected
    fDataString: UTF8String;
    fAge: Longint;
    fDoesNotExist: Boolean;
    fStream: TStream;
  public
    // The 'Reader' is owned by the promise and destroyed when that is destroyed.
    constructor Create(aFile: TFile);
    destructor Destroy; override;
    procedure SetAnswer(aStream: TStream; aAge: Longint;
      aDoesNotExist: Boolean);
    property Path: TFile read FPath;
    property Age: Longint read FAge;
    property DoesNotExist: Boolean read fDoesNotExist;
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

constructor TFileListTemplatesPromise.Create(aFile: TFile);
begin
  inherited Create;
  fPath := aFile;
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

{ TFileReadPromise }

constructor TFileReadPromise.Create(aFile: TFile);
begin
  inherited Create;
  fPath := aFile;
end;

destructor TFileReadPromise.Destroy;
begin
  if fStream <> nil then
    FreeAndNil(fStream);
  inherited Destroy;
end;

procedure TFileReadPromise.SetAnswer(aStream: TStream; aAge: Longint; aDoesNotExist: Boolean);
begin
  fStream := aStream;
  fAge := aAge;
  fDoesNotExist := aDoesNotExist;
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

procedure TFileExistencePromise.SetAnswer(aExists: Boolean);
begin
  fExists := aExists;
end;

{ TFileListPromise }

constructor TFileListPromise.Create(aFile: TFile);
begin
  inherited Create;
  fPath := aFile;
  fDoesNotExist := true;
end;

procedure TFileListPromise.SetAnswer(aFiles: TFileArray; aDoesNotExist: Boolean
  );
begin
  fFiles := aFiles;
  fDoesNotExist := aDoesNotExist;
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

