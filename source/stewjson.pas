unit stewjson;

{$mode objfpc}{$H+}

interface

// FUTURE: I'd like to re-write the fcl JSON library so that it makes use of
// a class factory instead of just fpjson.CreateJSONObject, etc. That
// way, I would be able to construct "custom" JSON objects which expose
// appropriate properties (while at the same time allowing 'unmanaged' properties)
// However, since I don't have that, this is far simpler.



uses
  Classes, SysUtils, fpjson, stewfile, stewtypes;

type

  // TODO: What happens if they want to 'refresh' the data? We have pointers
  // all over that will be lost. Need to get rid of the fData aand ownsData, and
  // only the Filebacked object actually has data, the rest needing to get it
  // only if they use it.

  { TManagedJSONObject }

  TManagedJSONObject = class
  private
    function GetArray(const AName: String): TJSONArray;
    function GetBoolean(const AName: String): Boolean;
    function GetFloat(const AName: String): TJSONFloat;
    function GetInt64(const AName: String): Int64;
    function GetInteger(const AName: String): Integer;
    function GetIsNull(const AName: String): Boolean;
    function GetItem(const aName: String; const aClass: TJSONDataClass): TJSONData; overload;
    function GetObject(const AName: String): TJSONObject;
    function GetString(const AName: String): TJSONStringType;
    function GetType(const AName: String): TJSONType;
    procedure SetArray(const AName: String; AValue: TJSONArray);
    procedure SetBoolean(const AName: String; AValue: Boolean);
    procedure SetFloat(const AName: String; AValue: TJSONFloat);
    procedure SetInt64(const AName: String; AValue: Int64);
    procedure SetInteger(const AName: String; AValue: Integer);
    procedure SetIsNull(const AName: String; AValue: Boolean);
    procedure SetObject(const AName: String; AValue: TJSONObject);
    procedure SetString(const AName: String; AValue: TJSONStringType);
    procedure SetManagedValue(const aName: String; aValue: TJSONData); overload;
  protected
    function IsManaged({%H-}aPropertyName: String): boolean; virtual;
    function GetItem(const aName: String): TJSONData; overload; virtual;
    procedure SetItem(const aName: String; AValue: TJSONData); virtual;
    function NeedData(aCreate: Boolean): TJSONObject; virtual; abstract;
    procedure SetModified; virtual; abstract;
    function FindOrDefault(const aName: String; const aDefault: TJSONFloat): TJSONFloat; overload;
    function FindOrDefault(const aName: String; const aDefault: Integer): Integer; overload;
    function FindOrDefault(const aName: String; const aDefault: Int64): Int64; overload;
    function FindOrDefault(const aName: String; const aDefault: TJSONStringType): TJSONStringType; overload;
    function FindOrDefault(const aName: String; const aDefault: Boolean): Boolean; overload;
    function Find(const aName: String; const aType: TJSONtype): TJSONData;
    procedure SetManagedValue(const aName: String; const aValue: TJSONFloat); overload;
    procedure SetManagedValue(const aName: String; const aValue: Integer); overload;
    procedure SetManagedValue(const aName: String; const aValue: Int64); overload;
    procedure SetManagedValue(const aName: String; const aValue: TJSONStringType); overload;
    procedure SetManagedValue(const aName: String; const aValue: Boolean); overload;
    procedure SetManagedValueNull(const aName: String); overload;
    procedure DeleteManagedValue(const aName: String); overload;
    function CreateManagedObject(const aName: String): TJSONObject; overload;
    function CreateManagedArray(const aName: String): TJSONArray; overload;
  public
    property Item[const aName: String]: TJSONData read GetItem write SetItem; default;
    Property JSONType[const AName: String] : TJSONType Read GetType;
    Property Null[const AName: String] : Boolean Read GetIsNull Write SetIsNull;
    Property Float[const AName: String] : TJSONFloat Read GetFloat Write SetFloat;
    Property Integer[const AName: String] : Integer Read GetInteger Write SetInteger;
    Property Int64[const AName: String] : Int64 Read GetInt64 Write SetInt64;
    Property JSONString[const AName: String] : TJSONStringType Read GetString Write SetString;
    Property Boolean[const AName: String] : Boolean Read GetBoolean Write SetBoolean;
    Property JSONArray[const AName: String] : TJSONArray Read GetArray Write SetArray;
    Property JSONObject[const AName: String] : TJSONObject Read GetObject Write SetObject;
  end;

  { TParentedJSONObject }

  TParentedJSONObject = class(TManagedJSONObject)
  private
    fParent: TManagedJSONObject;
    fParentKey: String;
  protected
    function NeedData(aCreate: Boolean): TJSONObject; override;
    procedure SetModified; override;
  public
    constructor Create(aParent: TManagedJSONObject; aParentProperty: String);
  end;

  { TFileBackedJSONObject }

  TFileBackedJSONObject = class(TManagedJSONObject)
  private
    fData: TJSONObject;
    fFilename: TFilename;
    fModified: boolean;
  protected
    procedure SetModified; override;
    function NeedData({%H-}aCreate: Boolean): TJSONObject; override;
  public
    constructor Create(afileName: TFilename);
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property Modified: boolean read fModified;
  end;

  TJSONFilingState = (jfsInactive, jfsLoading, jfsSaving);

  { TAsyncFileBackedJSONObject }

  TAsyncFileBackedJSONObject = class(TManagedJSONObject)
  private
    fData: TJSONObject;
    fFilename: TFilename;
    fFileAge: Longint;
    fCreateDir: Boolean;
    fModified: boolean;
    fOnFileLoaded: TNotifyEvent;
    fOnFileLoadFailed: TExceptionEvent;
    fOnFileSaved: TNotifyEvent;
    fOnFileSaveFailed: TExceptionEvent;
    fOnFileSaveConflicted: TNotifyEvent;
    fFilingState: TJSONFilingState;
  protected
    procedure SetModified; override;
    function NeedData({%H-}aCreate: Boolean): TJSONObject; override;
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
    property Modified: boolean read fModified;
    property OnFileLoaded: TNotifyEvent read fOnFileLoaded write fOnFileLoaded;
    property OnFileLoadFailed: TExceptionEvent read fOnFileLoadFailed write fOnFileLoadFailed;
    property OnFileSaved: TNotifyEvent read fOnFileSaved write fOnFileSaved;
    property OnFileSaveFailed: TExceptionEvent read fOnFileSaveFailed write fOnFileSaveFailed;
    property OnFileSaveConflicted: TNotifyEvent read fOnFileSaveConflicted write fOnFileSaveConflicted;
  end;

  { TManagedJSONArray }
  // This is a very basic thing. It does not provide 'unmanaged' access like
  // the JSONObject above, because it is expected that all elements in an array
  // be completely managed. However, the managed access is highly type-specific,
  // so can't even be skeletoned out as an abstract (too context-specific even for
  // generics).
  //
  // I could have some sort of generic TManagedJSONData which can either take
  // an array or an object as the data source, but the number of combinations
  // was too complex.
  TManagedJSONArray = class
  private
    procedure SetManagedValue(const aIndex: Integer; aValue: TJSONData); overload;
  protected
    function NeedData(aCreate: Boolean): TJSONArray; virtual; abstract;
    procedure SetModified; virtual; abstract;
    function FindOrDefault(const aIndex: Integer; const aDefault: TJSONFloat): TJSONFloat; overload;
    function FindOrDefault(const aIndex: Integer; const aDefault: Integer): Integer; overload;
    function FindOrDefault(const aIndex: Integer; const aDefault: Int64): Int64; overload;
    function FindOrDefault(const aIndex: Integer; const aDefault: TJSONStringType): TJSONStringType; overload;
    function FindOrDefault(const aIndex: Integer; const aDefault: Boolean): Boolean; overload;
    function Find(const aIndex: Integer; const aType: TJSONtype): TJSONData;
    procedure SetManagedValue(const aIndex: Integer; const aValue: TJSONFloat); overload;
    procedure SetManagedValue(const aIndex: Integer; const aValue: Integer); overload;
    procedure SetManagedValue(const aIndex: Integer; const aValue: Int64); overload;
    procedure SetManagedValue(const aIndex: Integer; const aValue: TJSONStringType); overload;
    procedure SetManagedValue(const aIndex: Integer; const aValue: Boolean); overload;
    procedure SetManagedValueNull(const aIndex: Integer); overload;
    procedure DeleteManagedValue(const aIndex: Integer); overload;
    function CreateManagedObject(const aIndex: Integer): TJSONObject; overload;
    function CreateManagedArray(const aIndex: Integer): TJSONArray; overload;
  end;

  { TParentedJSONArray }

  TParentedJSONArray = class(TManagedJSONArray)
  private
    fParent: TManagedJSONObject;
    fParentKey: String;
  protected
    function NeedData(aCreate: Boolean): TJSONArray; override;
    procedure SetModified; override;
  public
    constructor Create(aParent: TManagedJSONObject; aParentProperty: String);
  end;



implementation

uses
  jsonparser, math, stewasync;


{ TAsyncFileBackedJSONObject }

procedure TAsyncFileBackedJSONObject.FileLoaded(aData: TStream; aFileAge: Longint);
var
  parser : TJSONParser;
  fileContents : TJSONData;
begin
  if (aData = nil) then
  begin
    // the file does not exist yet, so create a blank data object.
    fData := TJSONObject.Create;
    fFileAge := aFileAge;
    // set it modified so that when it saves it creates an empty file.
    SetModified;
  end
  else
  begin
    parser := TJSONParser.Create(aData);
    try
      fileContents := parser.Parse;
      if fileContents is TJSONObject then
      begin
        FreeAndNil(fData);
        fData := fileContents as TJSONObject;
        fFileAge := aFileAge;
        fModified := false;
      end
      else
      begin
        FreeAndNil(fileContents);
        raise Exception.Create('Application config did not contain a valid JSON object.');
      end;
    finally
      parser.Free;
    end;
  end;
  if fOnFileLoaded <> nil then
    fOnFileLoaded(Self);
  fFilingState := jfsInactive;
end;

procedure TAsyncFileBackedJSONObject.FileSaved(aFileAge: Longint);
begin
  fModified := false;
  fFileAge := aFileAge;
  if fOnFileSaved <> nil then
    fOnFileSaved(Self);
  fFilingState := jfsInactive;
end;

procedure TAsyncFileBackedJSONObject.FileLoadFailed(aError: Exception);
begin
  if fOnFileLoadFailed <> nil then
    fOnFileLoadFailed(Self,aError);
  fFilingState := jfsInactive;
end;

procedure TAsyncFileBackedJSONObject.FileSaveFailed(aError: Exception);
begin
  if fOnFileSaveFailed <> nil then
    fOnFileSaveFailed(Self,aError);
  fFilingState := jfsInactive;
end;

procedure TAsyncFileBackedJSONObject.FileSaveConflicted(aFileAge: Longint);
begin
  if fOnFileSaveConflicted <> nil then
    fOnFileSaveConflicted(Self)
  else if fOnFileSaveFailed <> nil then
    fOnFileSaveFailed(Self,Exception.Create('File could not be saved because it was changed on disk since the last save'));
  fFilingState := jfsInactive;
end;

procedure TAsyncFileBackedJSONObject.SetModified;
begin
  if not fModified then
  begin
    fModified := true;
  end;
end;

function TAsyncFileBackedJSONObject.NeedData(aCreate: Boolean): TJSONObject;
begin
  if (fData = nil)  then
    raise Exception.Create('JSON file must be loaded before it can be accessed');
  result := fData;
end;

constructor TAsyncFileBackedJSONObject.Create(afileName: TFilename;
  aCreateDir: Boolean);
begin
  inherited Create;
  fData := nil; // starts off blank until loaded.
  fModified := false;
  fFilename := afileName;
  fCreateDir := aCreateDir;
  fFileAge := -1; // indicates that this might be a new file.
end;

destructor TAsyncFileBackedJSONObject.Destroy;
begin
  if fData <> nil then
    FreeAndNil(fData);
end;

procedure TAsyncFileBackedJSONObject.Load;
begin
  if fFilingState = jfsInactive then
     TReadFile.Create(fFilename,@FileLoaded,@FileLoadFailed).Enqueue
  else if fFilingState = jfsSaving then
     raise Exception.Create('Can''t load JSON data while saving.');
  // otherwise, already loading, so ignore.
end;

procedure TAsyncFileBackedJSONObject.Save(aForce: Boolean = false);
var
  text: UTF8String;
begin
  if Modified then
  begin
    if fFilingState = jfsInactive then
    begin
      fFilingState := jfsSaving;
      text := UTF8String(fData.FormatJSON());
      TWriteFile.Create(fFilename,fCreateDir and (fFileAge = -1),not aForce,fFileAge,text,@FileSaved,@FileSaveConflicted,@FileSaveFailed).Enqueue;

    end
    else if fFilingState = jfsLoading then
      raise Exception.Create('Can''t save JSON data while still loading.');
    // otherwise, already saving, so don't worry about it.
  end;
end;

{ TParentedJSONArray }

function TParentedJSONArray.NeedData(aCreate: Boolean): TJSONArray;
begin
  if fparent <> nil then
  begin
    result := fparent.Find(fParentKey,jtArray) as TJSONArray;
    if (result = nil) and aCreate then
    begin
      result := fParent.CreateManagedArray(fParentKey);
    end;
  end
  else
  begin
    raise Exception.Create('Can''t create data. Parent JSON manager isn''t set.');
  end;
end;

procedure TParentedJSONArray.SetModified;
begin
  if fparent <> nil then
  begin
    fparent.SetModified;
  end;
end;

constructor TParentedJSONArray.Create(aParent: TManagedJSONObject;
  aParentProperty: String);
begin
  inherited Create;
  fparent := aParent;
  fParentKey := aParentProperty;
end;

{ TManagedJSONArray }

procedure TManagedJSONArray.SetManagedValue(const aIndex: Integer;
  aValue: TJSONData);
var
  aData: TJSONArray;
begin
  aData := NeedData(true);
  if AValue <> nil then
  begin
    adata[aIndex] := AValue;
  end
  else
  begin
    adata.Delete(aIndex);
  end;
  SetModified;
end;

function TManagedJSONArray.FindOrDefault(const aIndex: Integer;
  const aDefault: TJSONFloat): TJSONFloat;
var
  answer: TJSONData;
begin
  answer := Find(aIndex,jtNumber);
  if (answer <> nil) then
  begin
    result := answer.AsFloat;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONArray.FindOrDefault(const aIndex: Integer;
  const aDefault: Integer): Integer;
var
  answer: TJSONData;
begin
  answer := Find(aIndex,jtNumber);
  if (answer <> nil) then
  begin
    result := answer.AsInteger;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONArray.FindOrDefault(const aIndex: Integer;
  const aDefault: Int64): Int64;
var
  answer: TJSONData;
begin
  answer := Find(aIndex,jtNumber);
  if (answer <> nil) then
  begin
    result := answer.AsInt64;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONArray.FindOrDefault(const aIndex: Integer;
  const aDefault: TJSONStringType): TJSONStringType;
var
  answer: TJSONData;
begin
  answer := Find(aIndex,jtString);
  if (answer <> nil) then
  begin
    result := answer.AsString;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONArray.FindOrDefault(const aIndex: Integer;
  const aDefault: Boolean): Boolean;
var
  answer: TJSONData;
begin
  answer := Find(aIndex,jtBoolean);
  if (answer <> nil) then
  begin
    result := answer.AsBoolean;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONArray.Find(const aIndex: Integer; const aType: TJSONtype
  ): TJSONData;
var
  aData: TJSONArray;
begin
  result := nil;
  aData := NeedData(false);
  if (aData <> nil) and (aIndex >= 0) and (aIndex < aData.Count) then
  begin
    result := aData[aIndex];
    if result.JSONType <> aType then
    begin
      result := nil;
    end;
  end;
end;

procedure TManagedJSONArray.SetManagedValue(const aIndex: Integer;
  const aValue: TJSONFloat);
begin
  SetManagedValue(aIndex,TJSONFloatNumber.Create(aValue));

end;

procedure TManagedJSONArray.SetManagedValue(const aIndex: Integer;
  const aValue: Integer);
begin
  SetManagedValue(aIndex,TJSONIntegerNumber.Create(aValue));
end;

procedure TManagedJSONArray.SetManagedValue(const aIndex: Integer;
  const aValue: Int64);
begin
  SetManagedValue(aIndex,TJSONInt64Number.Create(aValue));
end;

procedure TManagedJSONArray.SetManagedValue(const aIndex: Integer;
  const aValue: TJSONStringType);
begin
  SetManagedValue(aIndex,TJSONString.Create(aValue));
end;

procedure TManagedJSONArray.SetManagedValue(const aIndex: Integer;
  const aValue: Boolean);
begin
  SetManagedValue(aIndex,TJSONBoolean.Create(aValue));
end;

procedure TManagedJSONArray.SetManagedValueNull(const aIndex: Integer);
begin
  SetManagedValue(aIndex,TJSONNull.Create);
end;

procedure TManagedJSONArray.DeleteManagedValue(const aIndex: Integer);
begin
  SetManagedValue(aIndex,nil);
end;

function TManagedJSONArray.CreateManagedObject(const aIndex: Integer): TJSONObject;
begin
  result := TJSONObject.Create;
  SetManagedValue(aIndex,Result);
end;

function TManagedJSONArray.CreateManagedArray(const aIndex: Integer): TJSONArray;
begin
  result := TJSONArray.Create;
  SetManagedValue(aIndex,Result);
end;

{ TParentedJSONObject }

function TParentedJSONObject.NeedData(aCreate: Boolean): TJSONObject;
begin
  if fparent <> nil then
  begin
    result := fparent.Find(fParentKey,jtObject) as TJSONObject;
    if (result = nil) and aCreate then
    begin
      result := fParent.CreateManagedObject(fParentKey);
    end;
  end
  else
  begin
    raise Exception.Create('Can''t create data. Parent JSON manager isn''t set.');
  end;
end;

procedure TParentedJSONObject.SetModified;
begin
  if fparent <> nil then
  begin
    fparent.SetModified;
  end;
end;

constructor TParentedJSONObject.Create(aParent: TManagedJSONObject;
  aParentProperty: String);
begin
  inherited Create;
  fparent := aParent;
  fParentKey := aParentProperty;
end;

{ TFileBackedJSONObject }

procedure TFileBackedJSONObject.SetModified;
begin
  if not fModified then
  begin
    fModified := true;
  end;
end;

function TFileBackedJSONObject.NeedData(aCreate: Boolean): TJSONObject;
begin
  if (fData = nil)  then
    raise Exception.Create('JSON file must be loaded before it can be accessed');
  result := fData;
end;

constructor TFileBackedJSONObject.Create(afileName: TFilename);
begin
  inherited Create;
  fFilename := afileName;
  fData := nil;
end;

destructor TFileBackedJSONObject.Destroy;
begin
  if fData <> nil then
    FreeAndNil(fData);
  inherited Destroy;
end;

procedure TFileBackedJSONObject.Load;
var
  parser : TJSONParser;
  fileContents : TJSONData;
  stream : TFileStream;
begin
  If Not FileExists(fFileName) then
  begin
    fData := TJSONObject.Create;
    // set it modified so that save will create the new file.
    SetModified;
  end
  else
  begin
    stream := TFileStream.Create(fFilename,fmOpenRead or fmShareDenyWrite);
    try
      parser := TJSONParser.Create(stream);
      try
        fileContents := parser.Parse;
        if fileContents is TJSONObject then
        begin
          FreeAndNil(fData);
          fData := fileContents as TJSONObject;
          fModified := false;
        end
        else
        begin
          FreeAndNil(fileContents);
          raise Exception.Create('Application config did not contain a valid JSON object.');
        end;
      finally
        parser.Free;
      end;

    finally
      stream.Free;
    end;
  end;

end;

procedure TFileBackedJSONObject.Save;
var
  stream: TFileStream;
  text: UTF8String;
begin
  if Modified then
  begin
    if not DirectoryExists(ExtractFileDir(fFilename)) then
    begin
      ForceDirectories(ExtractFileDir(fFilename));
    end;
    stream := TFileStream.Create(fFilename,fmCreate);
    try
      text := UTF8String(fData.FormatJSON());
      stream.Write(text[1],Length(text));
      fModified := False;
    finally
      stream.Free;
    end;

  end;

end;

{ TManagedJSONObject }

function TManagedJSONObject.GetArray(const AName: String): TJSONArray;
begin
  result := GetItem(aName,TJSONArray) as TJSONArray;

end;

function TManagedJSONObject.GetBoolean(const AName: String): Boolean;
var
  answer: TJSONBoolean;
begin
  answer := GetItem(AName,TJSONBoolean) as TJSONBoolean;
  result := (answer <> nil) and (answer.AsBoolean);
end;

function TManagedJSONObject.GetFloat(const AName: String): TJSONFloat;
var
  answer: TJSONNumber;
begin
  answer := GetItem(AName,TJSONNumber) as TJSONNumber;
  if (answer <> nil) then
  begin
     result := (answer.AsFloat)
  end
  else
  begin
    result := NaN;
  end;

end;

function TManagedJSONObject.GetInt64(const AName: String): Int64;
var
  answer: TJSONNumber;
begin
  answer := GetItem(AName,TJSONNumber) as TJSONNumber;
  if (answer <> nil) then
  begin
     result := (answer.AsInt64)
  end
  else
  begin
    result := 0;
  end;

end;

function TManagedJSONObject.GetInteger(const AName: String): Integer;
var
  answer: TJSONNumber;
begin
  answer := GetItem(AName,TJSONNumber) as TJSONNumber;
  if (answer <> nil) then
  begin
     result := (answer.AsInteger)
  end
  else
  begin
    result := 0;
  end;

end;

function TManagedJSONObject.GetIsNull(const AName: String): Boolean;
var
  answer: TJSONData;
begin
  answer := GetItem(AName);
  result := (answer = nil) or (answer is TJSONNull);
end;

function TManagedJSONObject.GetItem(const aName: String): TJSONData;
var
  aData: TJSONObject;
begin
  result := nil;
  if not IsManaged(aName) then
  begin
    aData := NeedData(false);
    if (adata <> nil) then
    begin
      result := adata.Find(aName);
    end
  end;

end;

function TManagedJSONObject.GetItem(const aName: String;
  const aClass: TJSONDataClass): TJSONData;
begin
  result := GetItem(aName);
  if not (result is aClass) then
  begin
    result := nil;
  end;
end;

function TManagedJSONObject.GetObject(const AName: String): TJSONObject;
begin
  result := GetItem(aName,TJSONObject) as TJSONObject;

end;

function TManagedJSONObject.GetString(const AName: String): TJSONStringType;
var
  answer: TJSONString;
begin
  answer := GetItem(AName,TJSONString) as TJSONString;
  if (answer <> nil) then
  begin
     result := (answer.AsString)
  end
  else
  begin
    result := '';
  end;
end;

function TManagedJSONObject.GetType(const AName: String): TJSONType;
var
  answer: TJSONData;
begin
  answer := GetItem(AName);
  if (answer <> nil) then
  begin
     result := answer.JSONType;
  end
  else
  begin
    result := jtUnknown;
  end;
end;

procedure TManagedJSONObject.SetArray(const AName: String; AValue: TJSONArray);
begin
  SetItem(aName,AValue);

end;

procedure TManagedJSONObject.SetBoolean(const AName: String; AValue: Boolean);
begin
  SetItem(AName,TJSONBoolean.Create(AValue));
end;

procedure TManagedJSONObject.SetFloat(const AName: String; AValue: TJSONFloat);
begin
  SetItem(AName,TJSONFloatNumber.Create(AValue));
end;

procedure TManagedJSONObject.SetInt64(const AName: String; AValue: Int64);
begin
  SetItem(AName,TJSONInt64Number.Create(AValue));
end;

procedure TManagedJSONObject.SetInteger(const AName: String; AValue: Integer);
begin
  SetItem(AName,TJSONIntegerNumber.Create(AValue));
end;

procedure TManagedJSONObject.SetIsNull(const AName: String; AValue: Boolean);
begin
  if AValue then
  begin
    SetItem(AName,TJSONNull.Create);
  end;
end;

procedure TManagedJSONObject.SetItem(const aName: String; AValue: TJSONData);
begin
  if not IsManaged(aName) then
  begin
    SetManagedValue(aName,AValue);
  end
  else
  begin
    raise Exception.Create('Property "' + aName + '" is managed, and can''t be set arbitrarily');
  end;

end;

procedure TManagedJSONObject.SetObject(const AName: String; AValue: TJSONObject);
begin
  SetItem(AName,AValue);
end;

procedure TManagedJSONObject.SetString(const AName: String; AValue: TJSONStringType
  );
begin
  SetItem(AName,TJSONString.Create(AValue));
end;

procedure TManagedJSONObject.SetManagedValue(const aName: String;
  aValue: TJSONData);
var
  aData: TJSONObject;
begin
  aData := NeedData(true);
  if AValue <> nil then
  begin
    adata[aName] := AValue;
  end
  else
  begin
    adata.Delete(aName);
  end;
  SetModified;
end;

function TManagedJSONObject.IsManaged(aPropertyName: String): boolean;
begin
// override in descendants to prevent 'special' properties from being set.
  result := false;
end;

function TManagedJSONObject.FindOrDefault(const aName: String;
  const aDefault: TJSONFloat): TJSONFloat;
var
  answer: TJSONData;
begin
  answer := Find(aName,jtNumber);
  if (answer <> nil) then
  begin
    result := answer.AsFloat;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONObject.FindOrDefault(const aName: String;
  const aDefault: Integer): Integer;
var
  answer: TJSONData;
begin
  answer := Find(aName,jtNumber);
  if (answer <> nil) then
  begin
    result := answer.AsInteger;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONObject.FindOrDefault(const aName: String;
  const aDefault: Int64): Int64;
var
  answer: TJSONData;
begin
  answer := Find(aName,jtNumber);
  if (answer <> nil) then
  begin
    result := answer.AsInt64;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONObject.FindOrDefault(const aName: String;
  const aDefault: TJSONStringType): TJSONStringType;
var
  answer: TJSONData;
begin
  answer := Find(aName,jtString);
  if (answer <> nil) then
  begin
    result := answer.AsString;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONObject.FindOrDefault(const aName: String;
  const aDefault: Boolean): Boolean;
var
  answer: TJSONData;
begin
  answer := Find(aName,jtBoolean);
  if (answer <> nil) then
  begin
    result := answer.AsBoolean;
  end
  else
  begin
    result := aDefault;
  end;
end;

function TManagedJSONObject.Find(const aName: String; const aType: TJSONtype
  ): TJSONData;
var
  aData: TJSONObject;
begin
  result := nil;
  aData := NeedData(false);
  if aData <> nil then
  begin
    result := adata.Find(aName,aType);
  end;
end;

procedure TManagedJSONObject.SetManagedValue(const aName: String;
  const aValue: TJSONFloat);
begin
  SetManagedValue(aName,TJSONFloatNumber.Create(aValue));

end;

procedure TManagedJSONObject.SetManagedValue(const aName: String;
  const aValue: Integer);
begin
  SetManagedValue(aName,TJSONIntegerNumber.Create(aValue));
end;

procedure TManagedJSONObject.SetManagedValue(const aName: String;
  const aValue: Int64);
begin
  SetManagedValue(aName,TJSONInt64Number.Create(aValue));
end;

procedure TManagedJSONObject.SetManagedValue(const aName: String;
  const aValue: TJSONStringType);
begin
  SetManagedValue(aName,TJSONString.Create(aValue));
end;

procedure TManagedJSONObject.SetManagedValue(const aName: String;
  const aValue: Boolean);
begin
  SetManagedValue(aName,TJSONBoolean.Create(aValue));
end;

procedure TManagedJSONObject.SetManagedValueNull(const aName: String);
begin
  SetManagedValue(aName,TJSONNull.Create);
end;

procedure TManagedJSONObject.DeleteManagedValue(const aName: String);
begin
  SetManagedValue(aName,nil);
end;

function TManagedJSONObject.CreateManagedObject(const aName: String
  ): TJSONObject;
begin
  result := TJSONObject.Create;
  SetManagedValue(aName,Result);
end;

function TManagedJSONObject.CreateManagedArray(const aName: String): TJSONArray;
begin
  result := TJSONArray.Create;
  SetManagedValue(aName,Result);
end;

end.

