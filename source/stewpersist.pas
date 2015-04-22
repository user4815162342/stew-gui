unit stewpersist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewtypes, fpjson;

{$Interfaces CORBA}
{ I'm using CORBA here to avoid reference counting overhead and pitfalls. Without
it, I have to be careful of when and how I'm using references to the interface,
because it may accidentally get freed when I don't want it to be. It also makes
interfaces behave more like interfaces in other languages, since I don't need
to descend from a special object (TInterfacedObject) just to implement it.

I never liked the way Delphi implemented interfaces directly to COM, instead
of making the COM interface stuff a separate thing that you added in only if you
wanted it.}

type

  { IJSONStoreExtraProperties }
  // Used for allowing an object to retain persisted data which it doesn't
  // know anything about, for forwards compatibility, and for users to
  // put their own annotations in.
  IJSONStoreExtraProperties = interface
    procedure PersistExtraProperty(aName: TJSONStringType; aData: TJSONData);
    function CountPersistedProperties: Integer;
    procedure GetPersistedProperty(aIndex: Integer; out aName: TJSONStringType; out aData: TJSONData);
  end;

  { TJSONStore_ExtraPropertiesHelper }

  TJSONStoreExtraPropertiesHelper = class(IJSONStoreExtraProperties)
  private
    fData: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PersistExtraProperty(aName: TJSONStringType; aData: TJSONData);
    function CountPersistedProperties: Integer;
    procedure GetPersistedProperty(aIndex: Integer; out
       aName: TJSONStringType; out aData: TJSONData);

  end;

  { TJSONStore }
  TJSONStore = class(TPersistent,IJSONStoreExtraProperties)
  private
    fExtraProperties: TJSONStoreExtraPropertiesHelper;
    function GetExtraProperties: IJSONStoreExtraProperties;
  protected
    property ExtraProperties: IJSONStoreExtraProperties read GetExtraProperties implements IJSONStoreExtraProperties;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TJSONStoreContainer }
  // Basically an object that observes changes in others, and when it gets notified
  // marks itself as modified.
  TJSONStoreContainer = class(TJSONStore, IFPObserver)
  private
    FModified: Boolean;
  protected
    procedure FPOObservedChanged({%H-}ASender : TObject; {%H-}Operation : TFPObservedOperation; {%H-}Data : Pointer);
    procedure SetModified; virtual;
    procedure ClearModified; virtual;
  public
    property Modified: Boolean read FModified;
  end;

  { TJSONFileStoreContainer }

  TJSONFileStoreContainer = class(TJSONStoreContainer)
  private
    fFilename: TFilename;
  protected
    procedure Clear; virtual; abstract;
  public
    constructor Create(aFilename: TFilename);
    procedure Load;
    procedure Save;
  end;


  TFilingState = (fsInactive, fsLoading, fsSaving);

  { TJSONAsyncFileStoreContainer }
  // TODO: Should be TJSONAsyncFileStore

  TJSONAsyncFileStoreContainer = class(TJSONStoreContainer)
  private
    fFilename: TFilename;
    fFileAge: Longint;
    fCreateDir: Boolean;
    fOnFileLoaded: TNotifyEvent;
    fOnFileLoadFailed: TExceptionEvent;
    fOnFileSaved: TNotifyEvent;
    fOnFileSaveFailed: TExceptionEvent;
    fOnFileSaveConflicted: TNotifyEvent;
    fFilingState: TFilingState;
  protected
    procedure Clear; virtual; abstract;
    procedure FileLoaded(aData: TStream; aFileAge: Longint);
    procedure FileSaved(aFileAge: Longint);
    procedure FileLoadFailed(aError: Exception);
    procedure FileSaveFailed(aError: Exception);
    // File age is only passed for informational purposes, and I don't
    // need that information here.
    procedure FileSaveConflicted({%H-}aFileAge: Longint);
  public
    constructor Create(afileName: TFilename; aCreateDir: Boolean = false);
    destructor Destroy; override;
    procedure Load;
    // set force to true to ignore conflicts. This is usually done after
    // an attempt to save fails due to a file time conflict and the user chooses
    // to save anyway.
    procedure Save(aForce: Boolean = false);
    property FilingState: TFilingState read fFilingState;
    property OnFileLoaded: TNotifyEvent read fOnFileLoaded write fOnFileLoaded;
    property OnFileLoadFailed: TExceptionEvent read fOnFileLoadFailed write fOnFileLoadFailed;
    property OnFileSaved: TNotifyEvent read fOnFileSaved write fOnFileSaved;
    property OnFileSaveFailed: TExceptionEvent read fOnFileSaveFailed write fOnFileSaveFailed;
    property OnFileSaveConflicted: TNotifyEvent read fOnFileSaveConflicted write fOnFileSaveConflicted;
  end;

  { TJSONStoreCollectionItem }

  TJSONStoreCollectionItem = class(TCollectionItem,IJSONStoreExtraProperties)
  private
    fExtraProperties: TJSONStoreExtraPropertiesHelper;
    function GetExtraProperties: IJSONStoreExtraProperties;
  protected
    property ExtraProperties: IJSONStoreExtraProperties read GetExtraProperties implements IJSONStoreExtraProperties;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  end;

  TJSONStoreCollectionItemClass = class of TJSONStoreCollectionItem;

  TJSONStoreCollection = class(TCollection)
  public
    constructor Create(AItemClass: TJSONStoreCollectionItemClass);
  end;


implementation

uses
  fpjsonrtti, stewfile, jsonparser, rttiutils, typinfo;

type

  { TJSONStreamer }

  TJSONStreamer = class(fpjsonrtti.TJSONStreamer)
    procedure ClientBeforeStreamObject(Sender: TObject; AObject: TObject;
      JSON: TJSONObject);
    constructor Create(AOwner: TComponent); override;
  end;

  { TJSONDestreamer }

  TJSONDestreamer = class(fpjsonrtti.TJSONDestreamer)
    procedure ClientAfterReadObject(Sender: TObject; AObject: TObject;
      JSON: TJSONObject);
    constructor Create(AOwner: TComponent); override;
  end;

procedure ParseJSON(aObject: TObject; aData: TStream);
var
  loader: TJSONDeStreamer;
  data: TJSONData;
begin
  loader := TJSONDeStreamer.Create(nil);
  try
    with TJSONParser.Create(aData) do
    try
      data := Parse;
      if (data <> nil) and (data.JSONType = jtObject) then
      begin
        loader.JSONToObject(data as TJSONObject,aObject);
      end
      else
        raise Exception.Create('Invalid format for JSON file');
    finally
      Free;
    end;
  finally
    loader.Free;
  end;
end;

function GetJSONString(AObject: TObject): String;
var
  saver: TJSONStreamer;
  data : TJSONObject;
begin
  saver := TJSONStreamer.Create(nil);
  try
    saver.Options := [jsoUseFormatString,jsoTStringsAsArray];
    data := saver.ObjectToJSON(AObject);

    result := data.FormatJSON();
  finally
    saver.Free;
  end;

end;

{ TJSONStoreCollection }

constructor TJSONStoreCollection.Create(
  AItemClass: TJSONStoreCollectionItemClass);
begin
  inherited Create(AItemClass);
end;

{ TJSONStoreCollectionItem }

function TJSONStoreCollectionItem.GetExtraProperties: IJSONStoreExtraProperties;
begin
  result := fExtraProperties as IJSONStoreExtraProperties;
end;

constructor TJSONStoreCollectionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fExtraProperties := TJSONStoreExtraPropertiesHelper.Create;
end;

destructor TJSONStoreCollectionItem.Destroy;
begin
  FreeAndNil(fExtraProperties);
  inherited Destroy;
end;

{ TJSONDestreamer }

// This is basically just copied and fixed from the code in the destreamer JSONToObject.
// The problem is that we need a hook to handle properties found in the JSON that
// aren't found here, so they can be stored and re-saved. My decision lay between
// copying out the whole jsonrtti unit just so I could add an event, and then
// having to maintain that fix, or adding this kind of bulky event handler.
// FUTURE: Revisit this decision at some point in the future.
procedure TJSONDestreamer.ClientAfterReadObject(Sender: TObject;
  AObject: TObject; JSON: TJSONObject);
Var
  I : Integer;
  PIL : TPropInfoList;
  aName: String;
  PI: PPropInfo;
  aStore: IJSONStoreExtraProperties;
begin
  if (AObject is IJSONStoreExtraProperties) then
  begin
    aStore := AObject as IJSONStoreExtraProperties;
    Pil:=TPropInfoList.Create(AObject,tkProperties);
    try
      For I:=0 to JSON.Count - 1 do
      begin
        aName := JSON.Names[i];
        PI := PIL.Find(aName);
        if (PI = nil) then
          // unknown property...
          aStore.PersistExtraProperty(aName,JSON[aName]);
      end;
    finally
      FreeAndNil(PIL);
    end;
  end;

end;

constructor TJSONDestreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AfterReadObject:=@ClientAfterReadObject;

end;

{ TJSONStreamer }

procedure TJSONStreamer.ClientBeforeStreamObject(Sender: TObject;
  AObject: TObject; JSON: TJSONObject);
var
  I: Integer;
  aStore: IJSONStoreExtraProperties;
  aName: String;
  aData: TJSONData;
begin
  if (aObject is IJSONStoreExtraProperties) then
  begin
    aStore := AObject as IJSONStoreExtraProperties;
    for I := 0 to aStore.CountPersistedProperties - 1 do
    begin
      aStore.GetPersistedProperty(I,aName,aData);
      if (JSON.IndexOfName(aName) = -1) then
      begin
        if aData <> nil then
           JSON[aName] := aData.Clone
      end
      else
        raise Exception.Create('Extra property ' + aName + ' already restored');
    end;

  end;
end;

constructor TJSONStreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeforeStreamObject:=@ClientBeforeStreamObject;
end;


{ TJSONStore_ExtraPropertiesHelper }

constructor TJSONStoreExtraPropertiesHelper.Create;
begin
  inherited Create;
  fData := TJSONObject.Create;
end;

destructor TJSONStoreExtraPropertiesHelper.Destroy;
begin
  FreeAndNil(fData);
  inherited Destroy;
end;

procedure TJSONStoreExtraPropertiesHelper.PersistExtraProperty(
  aName: TJSONStringType; aData: TJSONData);
begin
  if fData.IndexOfName(aName) = -1 then
    fData[aName] := aData.Clone
  else
    raise Exception.Create('Extra property ' + aName + ' already persisted.');
end;

function TJSONStoreExtraPropertiesHelper.CountPersistedProperties: Integer;
begin
  result := fData.Count;
end;

procedure TJSONStoreExtraPropertiesHelper.GetPersistedProperty(
  aIndex: Integer; out aName: TJSONStringType; out aData: TJSONData);
begin
  aName := fData.Names[aIndex];
  aData := fData.Items[aIndex];
end;

{ TJSONStore }

function TJSONStore.GetExtraProperties: IJSONStoreExtraProperties;
begin
  result := fExtraProperties as IJSONStoreExtraProperties;
end;

constructor TJSONStore.Create;
begin
  inherited Create;
  fExtraProperties := TJSONStoreExtraPropertiesHelper.Create;
end;

destructor TJSONStore.Destroy;
begin
  FreeAndNil(fExtraProperties);
  inherited Destroy;
end;


{ TJSONAsyncFileStoreContainer }

procedure TJSONAsyncFileStoreContainer.FileLoaded(aData: TStream;
  aFileAge: Longint);
begin
  if (aData = nil) then
  begin
    // the file does not exist yet, so create a blank data object.
    Clear;
    // set it modified so that when it saves it creates an empty file.
    SetModified;
  end
  else
  begin
    ParseJSON(Self,aData);
    ClearModified;
  end;
  fFileAge := aFileAge;
  if fOnFileLoaded <> nil then
    fOnFileLoaded(Self);
  fFilingState := fsInactive;
end;

procedure TJSONAsyncFileStoreContainer.FileSaved(aFileAge: Longint);
begin
  ClearModified;
  fFileAge := aFileAge;
  if fOnFileSaved <> nil then
    fOnFileSaved(Self);
  fFilingState := fsInactive;
end;

procedure TJSONAsyncFileStoreContainer.FileLoadFailed(aError: Exception);
begin
  if fOnFileLoadFailed <> nil then
    fOnFileLoadFailed(Self,aError);
  fFilingState := fsInactive;
end;

procedure TJSONAsyncFileStoreContainer.FileSaveFailed(aError: Exception);
begin
  if fOnFileSaveFailed <> nil then
    fOnFileSaveFailed(Self,aError);
  fFilingState := fsInactive;
end;

procedure TJSONAsyncFileStoreContainer.FileSaveConflicted(aFileAge: Longint);
begin
  if fOnFileSaveConflicted <> nil then
    fOnFileSaveConflicted(Self)
  else if fOnFileSaveFailed <> nil then
    fOnFileSaveFailed(Self,Exception.Create('File could not be saved because it was changed on disk since the last save'));
  fFilingState := fsInactive;
end;

constructor TJSONAsyncFileStoreContainer.Create(afileName: TFilename;
  aCreateDir: Boolean);
begin
  inherited Create;
  fFilename := afileName;
  fCreateDir := aCreateDir;
  fFileAge := -1; // indicates that this might be a new file.
  Clear;
end;

destructor TJSONAsyncFileStoreContainer.Destroy;
begin
  inherited Destroy;
end;

procedure TJSONAsyncFileStoreContainer.Load;
begin
  if fFilingState = fsInactive then
     TReadFile.Create(fFilename,@FileLoaded,@FileLoadFailed).Enqueue
  else if fFilingState = fsSaving then
     raise Exception.Create('Can''t load JSON data while saving.');
  // otherwise, already loading, so ignore.
end;

procedure TJSONAsyncFileStoreContainer.Save(aForce: Boolean);
var
  text: UTF8String;
begin
  if Modified then
  begin
    if fFilingState = fsInactive then
    begin
      fFilingState := fsSaving;
      text := GetJSONString(Self);
      TWriteFile.Create(fFilename,fCreateDir and (fFileAge = -1),not aForce,fFileAge,text,@FileSaved,@FileSaveConflicted,@FileSaveFailed).Enqueue;

    end
    else if fFilingState = fsLoading then
      raise Exception.Create('Can''t save JSON data while still loading.');
    // otherwise, already saving, so don't worry about it.
  end;
end;

{ TJSONStoreContainer }

procedure TJSONStoreContainer.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  // pass it on...
  SetModified;
end;

procedure TJSONStoreContainer.SetModified;
begin
  FModified := true;
  FPONotifyObservers(Self,ooChange,nil);
end;

procedure TJSONStoreContainer.ClearModified;
begin
  FModified := false;
end;


{ TJSONFileStoreContainer }

constructor TJSONFileStoreContainer.Create(aFilename: TFilename);
begin
  inherited Create;
  fFilename := aFilename;
end;

procedure TJSONFileStoreContainer.Load;
var
  fs: TFileStream;
begin
  If Not FileExists(fFileName) then
  begin
    Clear;
    // set it modified so that save will create the new file.
    SetModified;
  end
  else
  begin
    fs := TFileStream.Create(fFilename,fmOpenRead or fmShareDenyWrite);
    try
      ParseJSON(Self,fs);
      ClearModified;

    finally
      fs.Free;
    end;
  end;

end;

procedure TJSONFileStoreContainer.Save;
var
  fs: TFileStream;
  text: UTF8String;
begin
  if Modified then
  begin
    if not DirectoryExists(ExtractFileDir(fFilename)) then
    begin
      ForceDirectories(ExtractFileDir(fFilename));
    end;
    fs := TFileStream.Create(fFilename,fmCreate);
    try
      text := GetJSONString(Self);
      fs.Write(text[1],Length(text));
      ClearModified;
    finally
      fs.Free;
    end;

  end;

end;

end.

