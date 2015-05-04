unit stewproperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewpersist, Graphics, fpjsonrtti, fpjson, stewtypes, contnrs;

type

  // TODO: For the user property editor, need to make sure that when it's changed,
  // the properties themselves are marked as changed.
  // TODO: I'm going to skip 'editors' for now. Which means I'm done with
  // properties and ready to work with documents!


  { TDocumentProperties }

  TDocumentProperties = class(TJSONAsyncFileStoreContainer)
  private
    fIndex: TStringList;
    function GetIndex: TStrings;
  protected
    procedure Clear; override;
  public
    constructor Create(afileName: TFilename; aIsRoot: Boolean);
    destructor Destroy; override;
  published
    property index: TStrings read GetIndex;
  end;

  { TKeywordDefinition }

  TKeywordDefinition = class(TJSONStoreMapItem, IJSONCustomSerializer)
  private
    Fcolor: TColor;
    procedure Setcolor(AValue: TColor);
    procedure AfterSerialize({%H-}aSaver: TJSONStreamer; aTarget: TJSONObject);
    procedure BeforeDeserialize({%H-}aLoader: TJSONDeStreamer; aData: TJSONObject);
  public
    property color: TColor read Fcolor write Setcolor;
  end;

  { TKeywordDefinitions }

  TKeywordDefinitions = class(TJSONStoreMap)
  public
    constructor Create;
  end;

  { TProjectProperties }

  TProjectProperties = class(TJSONAsyncFileStoreContainer, IJSONCustomSerializer)
  private
    Fcategories: TKeywordDefinitions;
    FdefaultCategory: String;
    FdefaultDocExtension: String;
    FdefaultNotesExtension: String;
    FdefaultStatus: String;
    FdefaultThumbnailExtension: String;
    fStatuses: TKeywordDefinitions;
    fUserProperties: TJSONData;
    procedure SetdefaultCategory(AValue: String);
    procedure SetdefaultDocExtension(AValue: String);
    procedure SetdefaultNotesExtension(AValue: String);
    procedure SetdefaultStatus(AValue: String);
    procedure SetdefaultThumbnailExtension(AValue: String);
    procedure SetUserProperties(AValue: TJSONData);
  protected
    procedure Clear; override;
    procedure AfterSerialize({%H-}aSaver: TJSONStreamer; {%H-}aTarget: TJSONObject);
    procedure BeforeDeserialize({%H-}aLoader: TJSONDeStreamer; aData: TJSONObject);
  public
    constructor Create(aProjectPath: TFilename);
    destructor Destroy; override;
    class function GetPath(aFolderPath: TFilename): TFilename;
    // not published because I need to override the streaming of this.
    property user: TJSONData read fUserProperties write SetUserProperties;
  published
    property defaultDocExtension: String read FdefaultDocExtension write SetdefaultDocExtension;
    property defaultThumbnailExtension: String read FdefaultThumbnailExtension write SetdefaultThumbnailExtension;
    property defaultNotesExtension: String read FdefaultNotesExtension write SetdefaultNotesExtension;
    property categories: TKeywordDefinitions read Fcategories;
    property defaultCategory: String read FdefaultCategory write SetdefaultCategory;
    property statuses: TKeywordDefinitions read fStatuses;
    property defaultStatus: String read FdefaultStatus write SetdefaultStatus;
  end;

  // FUTURE: Okay, I've got a problem with these user properties... the jsonrtti
  // will only match arrays with collections. If it finds a TObject that's
  // not a collection, it expects an object, not an array in the JSON format.
  // And, if it finds a JSON array, it expects a TCollection. There really
  // isn't much choice. And, since a TCollection can't hold primitive data,
  // There's no easy way to store primitive arrays. I thought this would solve
  // it, but then, I realized that when I got to receiving an array, the
  // deserializer gave me no way of automatically doing it (JSONToObject only
  // takes TJSONObjects, not TJSONArrays). Therefore, I had to rethink this
  // whole thing and give up on this.
  // But, I'm leaving it here in case I ever decide to switch to a different
  // and more customizable streaming option.
  TUserPropertyArray = class;
  TUserPropertyObject = class;

  TUserPropertyValueType = (upNull, upBoolean, upNumber, upString, upArray, upObject);

  { TUserPropertyValue  (Not using see above)}

  TUserPropertyValue = class(TPersistent)
  private
    fValue: Variant;
    fObject: TUserPropertyObject;
    fArray: TUserPropertyArray;
    FValueType: TUserPropertyValueType;
    function GetAsArray: TUserPropertyArray;
    function GetAsBoolean: Boolean;
    function GetAsNumber: Double;
    function GetAsObject: TUserPropertyObject;
    function GetAsString: String;
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsNumber(AValue: Double);
    procedure SetAsString(AValue: String);
    procedure ClearValue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetNull;
    property AsNumber: Double read GetAsNumber write SetAsNumber;
    property AsString: String read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsArray: TUserPropertyArray read GetAsArray;
    property AsObject: TUserPropertyObject read GetAsObject;
    property Kind: TUserPropertyValueType read FValueType;
    function MakeArray: TUserPropertyArray;
    function MakeObject: TUserPropertyObject;
  end;


  { TUserPropertyArray  (Not using see above)}
  TUserPropertyArray = class(TPersistent)
  private
    fList: TFPObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TUserPropertyValue;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TUserPropertyValue;
    procedure Delete(Index: Integer);
    procedure Clear;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TUserPropertyValue read GetItem;
  end;

  { TUserPropertyObjectItem  (Not using see above) }

  TUserPropertyObjectItem = class(TMappedCollectionItem)
  private
    fValue: TUserPropertyValue;
    function GetValue: TUserPropertyValue;
  public
    destructor Destroy; override;
    property Value: TUserPropertyValue read GetValue;
  end;

  { TUserProperty  (Not using see above)}

  TUserPropertyObject = class(TMappedCollection, IJSONCustomSerializer)
  protected
    procedure AfterSerialize(aSaver: TJSONStreamer; aTarget: TJSONObject); virtual;
    procedure BeforeDeserialize(aLoader: TJSONDeStreamer; aData: TJSONObject); virtual;
  public
    constructor Create;
  end;

implementation

uses
  LCLProc, Math;

{ TDocumentProperties }

function TDocumentProperties.GetIndex: TStrings;
begin
  result := fIndex;
end;

procedure TDocumentProperties.Clear;
begin
  fIndex.Clear;
end;

constructor TDocumentProperties.Create(afileName: TFilename; aIsRoot: Boolean);
var
  path: String;
begin
  if aIsRoot then
    path := IncludeTrailingPathDelimiter(afileName) + '_properties.json'
  else
    path := ExcludeTrailingPathDelimiter(afileName) + '_properties.json';
  inherited Create(path,false);
  fIndex := TStringList.Create;
end;

destructor TDocumentProperties.Destroy;
begin
  FreeAndNil(fIndex);
  inherited Destroy;
end;

{ TUserPropertyArray }

function TUserPropertyArray.GetCount: Integer;
begin
  result := fList.Count;
end;

function TUserPropertyArray.GetItem(Index: Integer): TUserPropertyValue;
begin
  result := fList[Index] as TUserPropertyValue;
end;

constructor TUserPropertyArray.Create;
begin
  inherited Create;
  fList := TFPObjectList.Create(false);
end;

destructor TUserPropertyArray.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  inherited Destroy;
end;

function TUserPropertyArray.Add: TUserPropertyValue;
begin
  result := TUserPropertyValue.Create;
  fList.Add(Result);
end;

procedure TUserPropertyArray.Delete(Index: Integer);
var
  Val: TUserPropertyValue;
begin
  Val := fList[Index] as TUserPropertyValue;
  fList.Delete(Index);
  FreeAndNil(Val);
end;

procedure TUserPropertyArray.Clear;
begin
  while Count > 0 do
    Delete(0);
end;



{ TUserPropertyValue }

procedure TUserPropertyValue.SetAsBoolean(AValue: Boolean);
begin
  ClearValue;
  FValueType := upBoolean;
  fValue := AValue;
end;

function TUserPropertyValue.GetAsBoolean: Boolean;
begin
  case FValueType of
    upNull:
      result := false;
    upBoolean:
      result := fValue;
    upNumber:
      result := fValue <> 0;
    upString:
      result := fValue <> '';
    upObject, upArray:
      result := true;
  end;
end;

function TUserPropertyValue.GetAsArray: TUserPropertyArray;
begin
  if FValueType = upArray then
    result := fArray
  else
    result := nil;
end;

function TUserPropertyValue.GetAsNumber: Double;
begin
  case FValueType of
    upNull:
      result := 0;
    upBoolean:
      if fValue then
        result := 1
      else
        result := 0;
    upNumber:
      result := fValue;
    upString:
      if fValue = '' then
         result := 0
      else if not TryStrToFloat(fValue,result) then
        result := math.NaN;
    upObject, upArray:
      result := math.NaN;
  end;
end;

function TUserPropertyValue.GetAsObject: TUserPropertyObject;
begin
  if FValueType = upObject then
    result := fObject
  else
    result := nil;
end;

function TUserPropertyValue.GetAsString: String;
var
  i: Integer;
begin
  case FValueType of
    upNull:
      result := 'null';
    upBoolean:
      result := BoolToStr(fValue,'true','false');
    upNumber:
      result := FloatToStr(fValue);
    upString:
      result := fValue;
    upObject:
      result := '[object Object]';
    upArray:
    begin
      result := '';
      for i := 0 to fArray.Count do
      begin
        if i > 0 then
          result := result + ',';
        result := result + (fArray.Items[i] as TUserPropertyValue).AsString;
      end;
    end;
  end;
end;

procedure TUserPropertyValue.SetAsNumber(AValue: Double);
begin
  ClearValue;
  FValueType := upNumber;
  fValue := AValue;
end;

procedure TUserPropertyValue.SetAsString(AValue: String);
begin
  ClearValue;
  FValueType := upString;
  fValue := AValue;
end;

procedure TUserPropertyValue.ClearValue;
begin
  FreeAndNil(fArray);
  FreeAndNil(fValue);
  fValue := Null;

end;

constructor TUserPropertyValue.Create;
begin
  inherited Create;
  fValue := Null;
  FValueType := upNull;
  fArray := nil;
  fObject := nil;

end;

destructor TUserPropertyValue.Destroy;
begin
  ClearValue;
  inherited Destroy;
end;

procedure TUserPropertyValue.SetNull;
begin
  ClearValue;
  FValueType := upNull;
  fValue := Null;

end;

function TUserPropertyValue.MakeArray: TUserPropertyArray;
begin
  ClearValue;
  FValueType := upArray;
  fArray := TUserPropertyArray.Create;
  result := fArray;
end;

function TUserPropertyValue.MakeObject: TUserPropertyObject;
begin
  ClearValue;
  FValueType := upObject;
  fObject := TUserPropertyObject.Create;
  result := fObject;
end;


{ TUserPropertyObjectItem }

function TUserPropertyObjectItem.GetValue: TUserPropertyValue;
begin
  if fValue = nil then
  begin
    fValue := TUserPropertyValue.Create;
    fValue.FPOAttachObserver(Self);
  end;
  result := fValue;
end;

destructor TUserPropertyObjectItem.Destroy;
begin
  if Value <> nil then
  begin
    fValue.FPODetachObserver(Self);
    FreeAndNil(fValue);
  end;
  inherited Destroy;
end;

{ TUserPropertyObject }

procedure TUserPropertyObject.AfterSerialize(aSaver: TJSONStreamer;
  aTarget: TJSONObject);
var
  i: Integer;
  aName: String;
  aProperty: TUserPropertyObjectItem;
begin
  for i := 0 to NameCount - 1 do
  begin
    aName := Names[i];
    aProperty := Items[aName] as TUserPropertyObjectItem;
    case aProperty.Value.Kind of
      upNull:
        aTarget[aName] := CreateJSON;
      upNumber:
        aTarget[aName] := CreateJSON(aProperty.Value.AsNumber);
      upString:
        aTarget[aName] := CreateJSON(aProperty.Value.AsString);
      upBoolean:
        aTarget[aName] := CreateJSON(aProperty.Value.AsBoolean);
      upArray:
        raise Exception.Create('Can''t serialize arrays yet');
      upObject:
        aTarget[aName] := aSaver.ObjectToJSON(aProperty.Value.AsObject);
    else
      raise Exception.Create('Unknown JSON type for user property');
    end;
  end;

end;

procedure TUserPropertyObject.BeforeDeserialize(aLoader: TJSONDeStreamer;
  aData: TJSONObject);
var
  i: Integer;
  aName: String;
  aValue: TJSONData;
  aProperty: TUserPropertyObjectItem;
begin
  for i := 0 to aData.Count - 1 do
  begin
    aName := aData.Names[i];
    aProperty := Add(aName) as TUserPropertyObjectItem;
    aValue := aData[aName];
    case aValue.JSONType of
      jtNumber:
        aProperty.Value.AsNumber := aValue.AsFloat;
      jtString:
        aProperty.Value.AsString := aValue.AsString;
      jtBoolean:
        aProperty.Value.AsBoolean := aValue.AsBoolean;
      jtNull:
        aProperty.Value.SetNull;
      jtArray:
        raise Exception.Create('Can''t deserialize arrays');
      jtObject:
        aLoader.JSONToObject(aValue as TJSONObject,aProperty.Value.MakeObject);
    else
      raise Exception.Create('Unknown JSON type for user property');
    end;

  end;
end;

constructor TUserPropertyObject.Create;
begin
  inherited Create(TUserPropertyObjectItem);
end;


{ TKeywordDefinition }

procedure TKeywordDefinition.Setcolor(AValue: TColor);
begin
  if Fcolor=AValue then Exit;
  Fcolor:=AValue;
end;

function ToByte(x: Integer): Byte;
begin
  if x > high(Byte) then
    result := high(Byte)
  else if x < low(Byte) then
    result := Low(Byte)
  else
    result := Byte(x);
end;

procedure TKeywordDefinition.AfterSerialize(aSaver: TJSONStreamer;
  aTarget: TJSONObject);
var aNewData: TJSONObject;
begin
  if Fcolor <> clDefault then
  begin;
    aNewData := TJSONObject.Create;
    aTarget['color'] := aNewData;
    aNewData.Add('r',Red(Fcolor));
    aNewData.Add('g',Green(Fcolor));
    aNewData.Add('b',Blue(Fcolor));
  end
  else
    aTarget.Delete('color');

end;

procedure TKeywordDefinition.BeforeDeserialize(aLoader: TJSONDeStreamer;
  aData: TJSONObject);
var
  aOldData: TJSONData;
  r: Byte;
  g: Byte;
  b: Byte;
begin
  aOldData := aData.Find('color');
  if (aOldData <> nil) and (aOldData.JSONType = jtObject) then
  begin
    r := ToByte((aOldData as TJSONObject)['r'].AsInteger);
    g := ToByte((aOldData as TJSONObject)['g'].AsInteger);
    b := ToByte((aOldData as TJSONObject)['b'].AsInteger);
    Fcolor := RGBToColor(r,g,b);
  end
  else
    Fcolor := clDefault;
end;

{ TKeywordDefinitions }

constructor TKeywordDefinitions.Create;
begin
  inherited Create(TKeywordDefinition);
end;

{ TProjectProperties }

procedure TProjectProperties.SetdefaultDocExtension(AValue: String);
begin
  if FdefaultDocExtension=AValue then Exit;
  FdefaultDocExtension:=AValue;
end;

procedure TProjectProperties.SetdefaultCategory(AValue: String);
begin
  if FdefaultCategory=AValue then Exit;
  FdefaultCategory:=AValue;
end;

procedure TProjectProperties.SetdefaultNotesExtension(AValue: String);
begin
  if FdefaultNotesExtension=AValue then Exit;
  FdefaultNotesExtension:=AValue;
end;

procedure TProjectProperties.SetdefaultStatus(AValue: String);
begin
  if FdefaultStatus=AValue then Exit;
  FdefaultStatus:=AValue;
end;

procedure TProjectProperties.SetdefaultThumbnailExtension(AValue: String);
begin
  if FdefaultThumbnailExtension=AValue then Exit;
  FdefaultThumbnailExtension:=AValue;
end;

procedure TProjectProperties.SetUserProperties(AValue: TJSONData);
begin
  if fUserProperties <> nil then
    FreeAndNil(fUserProperties);
  if AValue <> nil then
  // I have to clone it here, because the streaming system might
  // destroy the original.
    fUserProperties := AValue.Clone;
  SetModified;
end;

procedure TProjectProperties.Clear;
begin
  // TODO: Initialize all of the primitive data, clear all of the child data.
  Fcategories.Clear;
  fStatuses.Clear;
  FdefaultCategory := '';
  FdefaultStatus := '';
  FdefaultDocExtension := '';
  FdefaultNotesExtension := '';
  FdefaultThumbnailExtension := '';
  FreeAndNil(fUserProperties);
end;

procedure TProjectProperties.AfterSerialize(aSaver: TJSONStreamer;
  aTarget: TJSONObject);
begin
  if fUserProperties <> nil then
  begin
    // I have to clone here, because the streaming tool is going
    // to delete the properties.
    aTarget['user'] := fUserProperties.Clone;
  end;
end;

procedure TProjectProperties.BeforeDeserialize(aLoader: TJSONDeStreamer;
  aData: TJSONObject);
var
  aStatuses: TJSONData;
  aStatusesArray: TJSONArray;
  aStatusesObject: TJSONObject;
  aStatus: TJSONObject;
  i: Integer;
  aUser: TJSONData;
begin
  // Have to convert an old format which had statuses as an array of strings.
  aStatuses := aData.Find('statuses');
  if (aStatuses <> nil) and (aStatuses.JSONType = jtArray) then
  begin
    aStatusesArray := aStatuses as TJSONArray;
    aStatusesObject := TJSONObject.Create;
    for i := 0 to aStatusesArray.Count - 1 do
    begin
      aStatus := TJSONObject.Create;
      aStatusesObject[aStatusesArray[i].AsString] := aStatus;
    end;
    aData['statuses'] := aStatusesObject;

  end;

  aUser := aData.Find('user');
  if aUser <> nil then
  begin
    SetUserProperties(aUser);
  end
  else
  begin
    SetUserProperties(nil);
  end;
end;

constructor TProjectProperties.Create(aProjectPath: TFilename);
begin
  inherited Create(GetPath(aProjectPath),false);
  Fcategories := TKeywordDefinitions.Create;
  Fcategories.FPOAttachObserver(Self);
  fStatuses := TKeywordDefinitions.Create;
  fStatuses.FPOAttachObserver(Self);
end;

destructor TProjectProperties.Destroy;
begin
  if fUserProperties <> nil then
     FreeAndNil(fUserProperties);
  Fcategories.FPODetachObserver(Self);
  FreeAndNil(Fcategories);
  fStatuses.FPODetachObserver(Self);
  FreeAndNil(fStatuses);
  inherited Destroy;
end;

class function TProjectProperties.GetPath(aFolderPath: TFilename): TFilename;
begin
  result := IncludeTrailingPathDelimiter(aFolderPath) + '_stew.json';
end;

end.

