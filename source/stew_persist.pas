unit stew_persist deprecated;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_types, fpjson, fpjsonrtti, sys_file, sys_async;


{$Interfaces CORBA}
{ I'm using CORBA here to avoid reference counting overhead and pitfalls. Without
it, I have to be careful of when and how I'm using references to the interface,
because it may accidentally get freed when I don't want it to be. It also makes
interfaces behave more like interfaces in other languages, since I don't need
to descend from a special object (TInterfacedObject) just to implement it.

I never liked the way Delphi implemented interfaces directly to COM, instead
of making the COM interface stuff a separate thing that you added in only if you
wanted it.

One problem still remains:

Apparently it still has the problem of requiring an identity string for 'as' or 'is',
because when I didn't include one, objects were registering as supporting IJSONStoreCustomSerializer
when they supported IJSONStoreExtraProperties.

Seriously. I'd think the compiler would be able to just automatically insert the
type name in if one isn't found. It's possible that something else is going on,
but this was my solution.
}

type

  { IJSONStoreExtraProperties }
  // Used for allowing an object to retain persisted data which it doesn't
  // know anything about, for forwards compatibility, and for users to
  // put their own annotations in.
  IJSONStoreExtraProperties = interface
    ['IJSONStoreExtraProperties'] // Apparently, if I don't "Name" it, it's not possible to distinguish it from the other interfaces without names.
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

  IJSONCustomSerializer = interface
    ['IJSONCustomSerializer'] // Apparently, if I don't "Name" it, it's not possible to distinguish it from the other interfaces without names.
    procedure AfterSerialize(aSaver: TJSONStreamer; aTarget: TJSONObject);
    procedure BeforeDeserialize(aLoader: TJSONDeStreamer; aData: TJSONObject);
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
    procedure ClearModified; virtual;
    procedure ParseJSON(aObject: TObject; aData: String);
    function GetJSONString(AObject: TObject): String;
  public
    property Modified: Boolean read FModified;
    // Allow another object to mark it modified, to make sure things
    // get saved.
    procedure SetModified; virtual;
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


  { TJSONAsyncFileStoreContainer }

  TJSONAsyncFileStoreContainer = class(TJSONStoreContainer)
  private
    fFile: TFile;
    fFileAge: Longint;
    fCreateDir: Boolean;
    fOnFileLoaded: TNotifyEvent;
    fOnFileLoadFailed: TExceptionMessageEvent;
    fOnFileLoading: TNotifyEvent;
    fOnFileSaved: TNotifyEvent;
    fOnFileSaveFailed: TExceptionMessageEvent;
    fOnFileSaveConflicted: TNotifyEvent;
    fFilingState: TFilingState;
    fOnFileSaving: TNotifyEvent;
  protected
    procedure Clear; virtual; abstract;
    procedure FileLoaded(aSender: TPromise);
    procedure FileSaved(aSender: TPromise);
    procedure FileLoadFailed({%H-}aSender: TPromise; aError: String);
    procedure FileSaveFailed(aError: String);
    procedure FileSaveConflictCheck(aSender: TPromise; aError: String);
    // File age is only passed for informational purposes, and I don't
    // need that information here.
    procedure FileSaveConflicted({%H-}aFileAge: Longint);
    // These events are protected. In general, the project itself handles
    // these events, and I want all UI code to handle these actions via the
    // MainForm observation API. By making these protected, I can control
    // this better (The project overrides this by creating a subclass of
    // this that can handle the events).
    property OnFileLoaded: TNotifyEvent read fOnFileLoaded write fOnFileLoaded;
    property OnFileLoadFailed: TExceptionMessageEvent read fOnFileLoadFailed write fOnFileLoadFailed;
    property OnFileSaved: TNotifyEvent read fOnFileSaved write fOnFileSaved;
    property OnFileSaveFailed: TExceptionMessageEvent read fOnFileSaveFailed write fOnFileSaveFailed;
    property OnFileSaveConflicted: TNotifyEvent read fOnFileSaveConflicted write fOnFileSaveConflicted;
    property OnFileLoading: TNotifyEvent read fOnFileLoading write fOnFileLoading;
    property OnFileSaving: TNotifyEvent read fOnFileSaving write fOnFileSaving;
  public
    constructor Create(aFile: TFile; aCreateDir: Boolean);
    destructor Destroy; override;
    function Load: TFileReadPromise;
    // set force to true to ignore conflicts. This is usually done after
    // an attempt to save fails due to a file time conflict and the user chooses
    // to save anyway.
    procedure Save(aForce: Boolean = false);
    property FilingState: TFilingState read fFilingState;
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

  { TJSONStoreMapItem }

  TJSONStoreMapItem = class(TMappedCollectionItem,IJSONStoreExtraProperties)
  private
    fExtraProperties: TJSONStoreExtraPropertiesHelper;
    function GetExtraProperties: IJSONStoreExtraProperties;
  protected
    property ExtraProperties: IJSONStoreExtraProperties read GetExtraProperties implements IJSONStoreExtraProperties;
  public
    constructor Create(AMap: TMappedCollection; const aKey: String); override;
    destructor Destroy; override;
  end;

  TJSONStoreMap = class(TMappedCollection, IJSONCustomSerializer)
  protected
    procedure AfterSerialize(aSaver: TJSONStreamer; aTarget: TJSONObject); virtual;
    procedure BeforeDeserialize(aLoader: TJSONDeStreamer; aData: TJSONObject); virtual;
  end;


implementation

uses
  jsonparser, rttiutils, typinfo, LCLProc;

type

  { TJSONStreamer }

  TJSONStreamer = class(fpjsonrtti.TJSONStreamer)
    procedure ClientAfterStreamObject(Sender: TObject; AObject: TObject;
      JSON: TJSONObject);
    procedure ClientBeforeStreamObject(Sender: TObject; AObject: TObject;
      JSON: TJSONObject);
    constructor Create(AOwner: TComponent); override;
  end;

  { TJSONDestreamer }

  TJSONDestreamer = class(fpjsonrtti.TJSONDestreamer)
    procedure ClientAfterReadObject(Sender: TObject; AObject: TObject;
      JSON: TJSONObject);
    procedure ClientBeforeReadObject(Sender: TObject; AObject: TObject;
      JSON: TJSONObject);
    procedure ClientOnPropertyError(Sender: TObject; {%H-}AObject: TObject;
      {%H-}Info: PPropInfo; {%H-}AValue: TJSONData; {%H-}aError: Exception;
      var Continue: Boolean);
    constructor Create(AOwner: TComponent); override;
  end;

{ TJSONStoreMap }

procedure TJSONStoreMap.AfterSerialize(aSaver: fpjsonrtti.TJSONStreamer;
  aTarget: TJSONObject);
var
  i: Longint;
  aName: String;
begin
  for i := 0 to NameCount - 1 do
  begin
    aName := Names[i];
    aTarget[aName] := aSaver.ObjectToJSON(Items[aName]);
  end;
end;

procedure TJSONStoreMap.BeforeDeserialize(aLoader: fpjsonrtti.TJSONDeStreamer;
  aData: TJSONObject);
var
  i: Longint;
  aName: String;
begin

  Clear;
  For I:=0 to aData.Count-1 do
  begin
    aName := aData.Names[i];
    If (aData.Types[aName]<>jtObject) then
      raise Exception.CreateFmt('Object property %s is not a valid type for a map item: "%s"',[aName,JSONTypeName(aData.Types[aName])])
    else
      aLoader.JSONToObject(aData.Objects[aName],Add(aName));
  end;

end;

{ TJSONStoreMapItem }

function TJSONStoreMapItem.GetExtraProperties: IJSONStoreExtraProperties;
begin
  result := fExtraProperties as IJSONStoreExtraProperties;
end;

constructor TJSONStoreMapItem.Create(AMap: TMappedCollection; const aKey: String
  );
begin
  inherited Create(AMap, aKey);
  fExtraProperties := TJSONStoreExtraPropertiesHelper.Create;
end;

destructor TJSONStoreMapItem.Destroy;
begin
  FreeAndNil(fExtraProperties);
  inherited Destroy;
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

procedure TJSONDestreamer.ClientBeforeReadObject(Sender: TObject;
  AObject: TObject; JSON: TJSONObject);
begin
  if AObject is IJSONCustomSerializer then
  begin
    (AObject as IJSONCustomSerializer).BeforeDeserialize(Self,JSON);
  end;
end;

procedure TJSONDestreamer.ClientOnPropertyError(Sender: TObject;
  AObject: TObject; Info: PPropInfo; AValue: TJSONData; aError: Exception;
  var Continue: Boolean);
begin
  // the destreamer doesn't re-raise the error unless the event is assigned
  // and not handled. Otherwise, the error is just caught and that's it.
  DebugLn(aError.Message);
  Continue := false;
end;

constructor TJSONDestreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeforeReadObject:=@ClientBeforeReadObject;
  AfterReadObject:=@ClientAfterReadObject;
  OnPropertyError:=@ClientOnPropertyError;
end;

{ TJSONStreamer }

procedure TJSONStreamer.ClientAfterStreamObject(Sender: TObject;
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
        // I have to clone here, because the streaming system will
        // destroy the JSON after it's saved, which would destroy
        // our state.
           JSON[aName] := aData.Clone
      end
      // otherwise, skip it, because it's possible that the object
      // is storying it's own data under that property.
    end;

  end;
end;

procedure TJSONStreamer.ClientBeforeStreamObject(Sender: TObject;
  AObject: TObject; JSON: TJSONObject);
begin
  if AObject is IJSONCustomSerializer then
  begin
    (AObject as IJSONCustomSerializer).AfterSerialize(Self,JSON);
  end;
end;

constructor TJSONStreamer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AfterStreamObject:=@ClientAfterStreamObject;
;
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
  // I have to clone this data, because it is created and freed
  // by the streaming system.
  fData[aName] := aData.Clone
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

procedure TJSONAsyncFileStoreContainer.FileLoaded(aSender: TPromise);
begin
  if (not (aSender as TFileReadPromise).Exists) then
  begin
    // the file does not exist yet, so create a blank data object.
    Clear;
    // set it modified so that when it saves it creates an empty file.
    SetModified;
  end
  else
  begin
    Clear;
    ParseJSON(Self,(aSender as TFileReadPromise).ReadString);
    ClearModified;
  end;
  fFileAge := (aSender as TFileReadPromise).Age;
  fFilingState := fsLoaded;
  if fOnFileLoaded <> nil then
    fOnFileLoaded(Self);
end;

procedure TJSONAsyncFileStoreContainer.FileSaved(aSender: TPromise);
begin
  ClearModified;
  fFileAge := (aSender as TFileWritePromise).Age;
  fFilingState := fsLoaded;
  if fOnFileSaved <> nil then
    fOnFileSaved(Self);
end;

procedure TJSONAsyncFileStoreContainer.FileLoadFailed(aSender: TPromise;
  aError: String);
begin
  fFilingState := fsError;
  if fOnFileLoadFailed <> nil then
    fOnFileLoadFailed(Self,aError);
end;

procedure TJSONAsyncFileStoreContainer.FileSaveFailed(aError: String);
begin
  // this doesn't make the state an error, because the data in the
  // document is still valid, so just return it to loaded.
  fFilingState := fsLoaded;
  if fOnFileSaveFailed <> nil then
    fOnFileSaveFailed(Self,aError);
end;

procedure TJSONAsyncFileStoreContainer.FileSaveConflictCheck(aSender: TPromise;
  aError: String);
begin
  if (aSender as TFileWritePromise).IsConflict then
    FileSaveConflicted((aSender as TFileWritePromise).Age)
  else
    FileSaveFailed(aError);
end;

procedure TJSONAsyncFileStoreContainer.FileSaveConflicted(aFileAge: Longint);
begin
  fFilingState := fsConflict;
  if fOnFileSaveConflicted <> nil then
    fOnFileSaveConflicted(Self)
  else if fOnFileSaveFailed <> nil then
    fOnFileSaveFailed(Self,'File could not be saved because it was changed on disk since the last save');
end;

constructor TJSONAsyncFileStoreContainer.Create(aFile: TFile;
  aCreateDir: Boolean);
begin
  inherited Create;
  fFile := aFile;
  fCreateDir := aCreateDir;
  fFileAge := -1; // indicates that this might be a new file.
end;

destructor TJSONAsyncFileStoreContainer.Destroy;
begin
  inherited Destroy;
end;

function TJSONAsyncFileStoreContainer.Load: TFileReadPromise;
begin
  if fFilingState in [fsNotLoaded,fsLoaded,fsError,fsConflict] then
  begin
    fFilingState := fsLoading;
    if fOnFileLoading <> nil then
      fOnFileLoading(Self);
    result := fFile.Read.After(@FileLoaded,@FileLoadFailed) as TFileReadPromise;
  end
  else if fFilingState = fsSaving then
     raise Exception.Create('Can''t load JSON data while saving.');
  // otherwise, already loading, so ignore.
end;

procedure TJSONAsyncFileStoreContainer.Save(aForce: Boolean);
var
  text: UTF8String;
  lOptions: TFileWriteOptions;
begin
  if Modified then
  begin
    if fFilingState in [fsLoaded,fsConflict] then
    begin
      fFilingState := fsSaving;
      if fOnFileSaving <> nil then
        fOnFileSaving(Self);
      text := GetJSONString(Self);
      lOptions := [];
      if fCreateDir and (fFileAge = -1) then
        lOptions := lOptions + [fwoCreateDir];
      if not aForce then
        lOptions := lOptions + [fwoCheckAge];
      fFile.Write(lOptions,fFileAge,text).After(@FileSaved,@FileSaveConflictCheck);

    end
    else if fFilingState = fsNotLoaded then
      raise Exception.Create('Can''t save JSON data when it has not yet been loaded')
    else if fFilingState = fsLoading then
      raise Exception.Create('Can''t save JSON data while still loading.')
    else if fFilingState = fsError then
      raise Exception.Create('Can''t save a JSON file when the last load failed.');
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

procedure TJSONStoreContainer.ParseJSON(aObject: TObject; aData: String);
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

function TJSONStoreContainer.GetJSONString(AObject: TObject): String;
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


{ TJSONFileStoreContainer }

constructor TJSONFileStoreContainer.Create(aFilename: TFilename);
begin
  inherited Create;
  fFilename := aFilename;
end;

procedure TJSONFileStoreContainer.Load;
var
  fs: TFileStream;
  ss: TStringStream;
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
      ss := TStringStream.Create('');
      try
        ss.CopyFrom(fs,0);
        Clear;
        ParseJSON(Self,ss.DataString);
        ClearModified;
      finally
        ss.Free;
      end;

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

