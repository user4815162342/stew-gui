unit stew_properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stew_persist, Graphics, fpjsonrtti, fpjson, stew_types, contnrs, sys_file, sys_json;

type


  { TDocumentProperties }

  TDocumentProperties = class(TJSONAsyncFileStoreContainer, IJSONCustomSerializer)
    procedure IndexChanged(Sender: TObject);
  private
    fCategory: String;
    fIndex: TStringList;
    fPublish: boolean;
    fStatus: String;
    fTitle: String;
    fUserProperties: TJSONData;
    procedure SetUserProperties(AValue: TJSONData);
    function GetIndex: TStrings;
  protected
    procedure Clear; override;
    procedure AfterSerialize({%H-}aSaver: TJSONStreamer; aTarget: TJSONObject);
    procedure BeforeDeserialize({%H-}aLoader: TJSONDeStreamer; aData: TJSONObject);
  public
    constructor Create(afile: TFile; aIsRoot: Boolean);
    destructor Destroy; override;
    property user: TJSONData read fUserProperties write SetUserProperties;
  published
    property index: TStrings read GetIndex;
    property title: String read fTitle write fTitle;
    property publish: boolean read fPublish write fPublish;
    property category: String read fCategory write fCategory;
    property status: String read fStatus write fStatus;
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
  end;

  { TCategoryDefinition }

  TCategoryDefinition = class(TKeywordDefinition)
  private
    FpublishMarkerAfter: Boolean;
    FpublishMarkerBefore: Boolean;
    FpublishMarkerBetween: Boolean;
    FpublishTitle: Boolean;
    fpublishTitleLevel: Integer;
    FpublishTitlePrefix: String;
  published
    property publishTitle: Boolean read FpublishTitle write FpublishTitle default false;
    property publishTitleLevel: Integer read fpublishTitleLevel write FpublishTitleLevel default 0;
    property publishTitlePrefix: String read FpublishTitlePrefix write FpublishTitlePrefix;
    property publishMarkerBefore: Boolean read FpublishMarkerBefore write FpublishMarkerBefore default false;
    property publishMarkerAfter: Boolean read FpublishMarkerAfter write FpublishMarkerAfter default false;
    property publishMarkerBetween: Boolean read FpublishMarkerBetween write FpublishMarkerBetween default false;
  end;

  { TCategoryDefinitions }

  TCategoryDefinitions = class(TKeywordDefinitions)
    constructor Create;
  end;

  TStatusDefintion = class(TKeywordDefinition)
  end;

  { TStatusDefinitions }

  TStatusDefinitions = class(TKeywordDefinitions)
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
    constructor Create(aProjectFile: TFile);
    destructor Destroy; override;
    class function GetPath(aFolderPath: TFile): TFile;
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

  // TODO: This is the new stuff starting from here... Eventually, convert everything
  // over to this, and then get rid of the '2' at the end of the class names.

  { TProperties }

  TProperties = class(TJSObject)
  private
    function GetUser: TJSObject;
  protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
      ): TJSValueClass; override;
  public
    property User: TJSObject read GetUser;
  end;

  { TDocumentIndexProperty }

  TDocumentIndexProperty = class(TJSArray)
  protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  end;

  { TDocumentProperties2 }

  TDocumentProperties2 = class(TProperties)
  private
    function GetCategory: UTF8String;
    function GetIndex: TDocumentIndexProperty;
    function GetPublish: Boolean;
    function GetStatus: UTF8String;
    function GetTitle: UTF8String;
    procedure SetCategory(AValue: UTF8String);
    procedure SetPublish(AValue: Boolean);
    procedure SetStatus(AValue: UTF8String);
    procedure SetTitle(AValue: UTF8String);
  protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    property Title: UTF8String read GetTitle write SetTitle;
    property Publish: Boolean read GetPublish write SetPublish;
    property Category: UTF8String read GetCategory write SetCategory;
    property Status: UTF8String read GetStatus write SetStatus;
    property Index: TDocumentIndexProperty read GetIndex;
  end;

  { TJSColor }

  TJSColor = class(TJSObject)
  private
    function GetBlue: Byte;
    function GetColor: TColor;
    function GetGreen: Byte;
    function GetRed: Byte;
    procedure SetBlue(AValue: Byte);
    procedure SetColor(AValue: TColor);
    procedure SetGreen(AValue: Byte);
    procedure SetRed(AValue: Byte);
  protected
    function CreateNumberValue(aKey: UTF8String; aRequestType: TJSValueClass;
      aValue: Double): TJSValue; override; overload;
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    property Red: Byte read GetRed write SetRed;
    property Blue: Byte read GetBlue write SetBlue;
    property Green: Byte read GetGreen write SetGreen;
    property R: Byte read GetRed write SetRed;
    property B: Byte read GetBlue write SetBlue;
    property G: Byte read GetGreen write SetGreen;
    property AsColor: TColor read GetColor write SetColor;
  end;

  { TKeywordDefinition2 }

  TKeywordDefinition2 = class(TJSObject)
  private
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
  protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    property Color: TColor read GetColor write SetColor;
  end;

  TKeywordDefinitionClass = class of TKeywordDefinition2;

  { TCategoryDefinition2 }

  TCategoryDefinition2 = class(TKeywordDefinition2)
  private
    function GetPublishMarkerAfter: Boolean;
    function GetPublishMarkerBefore: Boolean;
    function GetPublishMarkerBetween: Boolean;
    function GetPublishTitle: Boolean;
    function GetPublishTitleLevel: Integer;
    function GetPublishTitlePrefix: UTF8String;
    procedure SetPublishMarkerAfter(AValue: Boolean);
    procedure SetPublishMarkerBefore(AValue: Boolean);
    procedure SetPublishMarkerBetween(AValue: Boolean);
    procedure SetPublishTitle(AValue: Boolean);
    procedure SetPublishTitleLevel(AValue: Integer);
    procedure SetPublishTitlePrefix(AValue: UTF8String);
  public
    property PublishTitle: Boolean read GetPublishTitle write SetPublishTitle;
    property PublishTitleLevel: Integer read GetPublishTitleLevel write SetPublishTitleLevel;
    property PublishTitlePrefix: UTF8String read GetPublishTitlePrefix write SetPublishTitlePrefix;
    property PublishMarkerBefore: Boolean read GetPublishMarkerBefore write SetPublishMarkerBefore;
    property PublishMarkerAfter: Boolean read GetPublishMarkerAfter write SetPublishMarkerAfter;
    property PublishMarkerBetween: Boolean read GetPublishMarkerBetween write SetPublishMarkerBetween;
  end;

  TStatusDefinition2 = class(TKeywordDefinition2)
    // just holds color, so nothing special...
  end;

  { TKeywordDefinitions2 }

  generic TKeywordDefinitions2<MemberType> = class(TJSObject)
  protected
    function RequestType({%H-}aKey: UTF8String; {%H-}aType: TJSValueClass
      ): TJSValueClass; override;
  public
    procedure Assign(aValue: TJSValue); override;
  end;

  TStatusDefinitions2 = specialize TKeywordDefinitions2<TStatusDefinition2>;

  TCategoryDefinitions2 = specialize TKeywordDefinitions2<TCategoryDefinition2>;

  { TProjectProperties2 }

  TProjectProperties2 = class(TProperties)
  private
    function GetCategories: TCategoryDefinitions2;
    function GetDefaultCategory: UTF8String;
    function GetDefaultDocExtension: UTF8String;
    function GetDefaultNotesExtension: UTF8String;
    function GetDefaultStatus: UTF8String;
    function GetDefaultThumbnailExtension: UTF8String;
    function GetStatuses: TStatusDefinitions2;
    procedure SetDefaultCategory(AValue: UTF8String);
    procedure SetDefaultDocExtension(AValue: UTF8String);
    procedure SetDefaultNotesExtension(AValue: UTF8String);
    procedure SetDefaultStatus(AValue: UTF8String);
    procedure SetDefaultThumbnailExtension(AValue: UTF8String);
  protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
    function CreateStringValue(aKey: UTF8String; aRequestType: TJSValueClass;
      aValue: UTF8String): TJSValue; override; overload;
  public
    class function GetPath(aFolderPath: TFile): TFile;
    property DefaultDocExtension: UTF8String read GetDefaultDocExtension write SetDefaultDocExtension;
    property DefaultThumbnailExtension: UTF8String read GetDefaultThumbnailExtension write SetDefaultThumbnailExtension;
    property DefaultNotesExtension: UTF8String read GetDefaultNotesExtension write SetDefaultNotesExtension;
    property Categories: TCategoryDefinitions2 read GetCategories;
    property DefaultCategory: UTF8String read GetDefaultCategory write SetDefaultCategory;
    property Statuses: TStatusDefinitions2 read GetStatuses;
    property DefaultStatus: UTF8String read GetDefaultStatus write SetDefaultStatus;
  end;

  const
    ProjectPropertiesDescriptor = 'stew';
    DocumentPropertiesDescriptor = 'properties';
    PropertiesExtension = 'json';
    UserKey = 'user';
    CategoriesKey = 'categories';
    StatusesKey = 'statuses';
    DefaultCategoryKey = 'defaultCategory';
    DefaultStatusKey = 'defaultStatus';
    DefaultDocExtensionKey = 'defaultDocExtension';
    DefaultNotesExtensionKey = 'defaultNotesExtension';
    DefaultThumbnailExtensionKey = 'defaultThumbnailExtension';
    PublishMarkerAfterKey = 'publishMarkerAfter';
    PublishMarkerBeforeKey = 'publishMarkerBefore';
    PublishMarkerBetweenKey = 'publishMarkerBetween';
    PublishTitleKey = 'publishTitle';
    PublishTitleLevelKey = 'publishTitleLevel';
    PublishTitlePrefixKey = 'publishTitlePrefix';
    ColorKey = 'color';
    RedKey = 'r';
    GreenKey = 'g';
    BlueKey = 'b';
    CategoryKey = 'category';
    StatusKey = 'status';
    IndexKey = 'index';
    PublishKey = 'publish';
    TitleKey = 'title';

(*

TODO: Put property names in constants...

*)


implementation

uses
  LCLProc, Math;

{ TProperties }

function TProperties.GetUser: TJSObject;
begin
  if not hasOwnProperty('user') then
     PutNewObject('user');
  result := Get('user') as TJSObject;
end;

function TProperties.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  if aKey = 'user' then
    result := TJSObject
  else
    Result:=inherited RequestType(aKey, aType);
end;

{ TProjectProperties2 }

function TProjectProperties2.GetCategories: TCategoryDefinitions2;
begin
  if not hasOwnProperty(CategoriesKey) then
     PutNewObject(CategoriesKey);
  result := Get(CategoriesKey) as TCategoryDefinitions2;
end;

function TProjectProperties2.GetDefaultCategory: UTF8String;
begin
  result := GetDefault(DefaultCategoryKey,'');
end;

function TProjectProperties2.GetDefaultDocExtension: UTF8String;
begin
  result := GetDefault(DefaultDocExtensionKey,'');
end;

function TProjectProperties2.GetDefaultNotesExtension: UTF8String;
begin
  result := GetDefault(DefaultNotesExtensionKey,'');
end;

function TProjectProperties2.GetDefaultStatus: UTF8String;
begin
  result := GetDefault(DefaultStatusKey,'');
end;

function TProjectProperties2.GetDefaultThumbnailExtension: UTF8String;
begin
  result := GetDefault(DefaultThumbnailExtensionKey,'');
end;

function TProjectProperties2.GetStatuses: TStatusDefinitions2;
begin
  if not hasOwnProperty(StatusesKey) then
     PutNewObject(StatusesKey);
  result := Get(StatusesKey) as TStatusDefinitions2;
end;

procedure TProjectProperties2.SetDefaultCategory(AValue: UTF8String);
begin
  Put(DefaultCategoryKey,AValue);
end;

procedure TProjectProperties2.SetDefaultDocExtension(AValue: UTF8String);
begin
  Put(DefaultDocExtensionKey,AValue);
end;

procedure TProjectProperties2.SetDefaultNotesExtension(AValue: UTF8String);
begin
  Put(DefaultNotesExtensionKey,AValue);
end;

procedure TProjectProperties2.SetDefaultStatus(AValue: UTF8String);
begin
  Put(DefaultStatusKey,AValue);
end;

procedure TProjectProperties2.SetDefaultThumbnailExtension(AValue: UTF8String);
begin
  Put(DefaultThumbnailExtensionKey,AValue);
end;

function TProjectProperties2.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  case aKey of
    StatusesKey:
      result := TStatusDefinitions2;
    CategoriesKey:
      result := TCategoryDefinitions2;
    DefaultStatusKey, DefaultCategoryKey,
    DefaultDocExtensionKey, DefaultThumbnailExtensionKey, DefaultNotesExtensionKey:
      result := TJSString;
  else
    Result:=inherited RequestType(aKey, aType);
  end;
end;

function TProjectProperties2.CreateStringValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: UTF8String): TJSValue;
begin
  case aKey of
    DefaultDocExtensionKey,
    DefaultNotesExtensionKey,
    DefaultThumbnailExtensionKey:
      // strip off extension delimiter for the default extension values...
      Result := inherited CreateStringValue(aKey,aRequestType,ExcludeExtensionDelimiter(aValue))
  else
    Result:=inherited CreateStringValue(aKey, aRequestType, aValue);
  end;
end;

class function TProjectProperties2.GetPath(aFolderPath: TFile): TFile;
begin
  result := aFolderPath.GetContainedFile('',ProjectPropertiesDescriptor,PropertiesExtension,false);
end;

{ TKeywordDefinitions2 }

function TKeywordDefinitions2.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  result := MemberType;
end;

procedure TKeywordDefinitions2.Assign(aValue: TJSValue);
var
  i: Integer;
  lValue: TJSArray;
  lItem: TJSValue;
begin
  if aValue is TJSArray then
  begin
    lValue := aValue as TJSArray;
    for i := 0 to lValue.Length - 1 do
    begin
      lItem := aValue.Get(i);
      if lItem.TypeOf in [jstBoolean,jstNumber,jstString] then
         // automatically creates a default definition under the specified name.
         PutNewObject(lItem.AsString)
      else
         PutNewObject(i).Assign(lItem);
    end;
  end
  else
     inherited Assign(aValue);
end;

{ TCategoryDefinition2 }

function TCategoryDefinition2.GetPublishMarkerAfter: Boolean;
begin
  result := GetDefault(PublishMarkerAfterKey,false);
end;

function TCategoryDefinition2.GetPublishMarkerBefore: Boolean;
begin
  Result := GetDefault(PublishMarkerBeforeKey,false);

end;

function TCategoryDefinition2.GetPublishMarkerBetween: Boolean;
begin
  result := GetDefault(PublishMarkerBetweenKey,false);
end;

function TCategoryDefinition2.GetPublishTitle: Boolean;
begin
  result := GetDefault(PublishTitleKey,false);
end;

function TCategoryDefinition2.GetPublishTitleLevel: Integer;
begin
  result := trunc(GetDefault(PublishTitleLevelKey,0));
end;

function TCategoryDefinition2.GetPublishTitlePrefix: UTF8String;
begin
  result := GetDefault(PublishTitlePrefixKey,'');

end;

procedure TCategoryDefinition2.SetPublishMarkerAfter(AValue: Boolean);
begin
  Put(PublishMarkerAfterKey,AValue);

end;

procedure TCategoryDefinition2.SetPublishMarkerBefore(AValue: Boolean);
begin
  Put(PublishMarkerBeforeKey,AValue);

end;

procedure TCategoryDefinition2.SetPublishMarkerBetween(AValue: Boolean);
begin
  Put(PublishMarkerBetweenKey,AValue);

end;

procedure TCategoryDefinition2.SetPublishTitle(AValue: Boolean);
begin
  Put(PublishTitleKey,aValue);

end;

procedure TCategoryDefinition2.SetPublishTitleLevel(AValue: Integer);
begin
  Put(PublishTitleLevelKey,aValue);

end;

procedure TCategoryDefinition2.SetPublishTitlePrefix(AValue: UTF8String);
begin
  Put(PublishTitlePrefixKey,aValue);

end;

{ TKeywordDefinition2 }

function TKeywordDefinition2.GetColor: TColor;
begin
  if hasOwnProperty(ColorKey) then
    result := (Get(ColorKey) as TJSColor).AsColor
  else
    result := clNone;
end;

procedure TKeywordDefinition2.SetColor(AValue: TColor);
var
  lValue: TJSColor;
begin
  if hasOwnProperty(ColorKey) then
    lValue := Get(ColorKey) as TJSColor
  else
    lValue := PutNewObject(ColorKey) as TJSColor;
  lValue.AsColor := AValue;
end;

function TKeywordDefinition2.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  if aKey = ColorKey then
    result := TJSColor
  else
    Result:=inherited RequestType(aKey, aType);
end;

{ TJSColor }

function TJSColor.GetBlue: Byte;
begin
  result := trunc(GetDefault(BlueKey,Graphics.Blue(clDefault)));
end;

function TJSColor.GetColor: TColor;
begin
  if hasOwnProperty(RedKey) or hasOwnProperty(GreenKey) or hasOwnProperty(BlueKey) then
     result := RGBToColor(R,G,B)
  else
    result := clDefault;
end;

function TJSColor.GetGreen: Byte;
begin
  result := trunc(GetDefault(GreenKey,Graphics.Green(clDefault)));

end;

function TJSColor.GetRed: Byte;
begin
  result := trunc(GetDefault(RedKey,Graphics.Red(clDefault)));
end;

procedure TJSColor.SetBlue(AValue: Byte);
begin
  Put(BlueKey,AValue);

end;

procedure TJSColor.SetColor(AValue: TColor);
begin
  if AValue = clDefault then
  begin
    delete(RedKey);
    delete(GreenKey);
    delete(BlueKey);
  end
  else
  begin
    R := Graphics.Red(AValue);
    G := Graphics.Green(AValue);
    B := Graphics.Blue(AValue);
  end;
end;

procedure TJSColor.SetGreen(AValue: Byte);
begin
  Put(GreenKey,AValue);

end;

procedure TJSColor.SetRed(AValue: Byte);
begin
  Put(RedKey,AValue);

end;

function TJSColor.CreateNumberValue(aKey: UTF8String;
  aRequestType: TJSValueClass; aValue: Double): TJSValue;
var
  lValue: Byte;
begin
  case aKey of
    RedKey,GreenKey,BlueKey:
    begin
      lValue := trunc(aValue);
      Result:=inherited CreateNumberValue(aKey, aRequestType, lValue);
    end
  else
    Result:=inherited CreateNumberValue(aKey, aRequestType, aValue);
  end;

end;

function TJSColor.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  case aKey of
    RedKey,GreenKey,BlueKey:
      result := TJSNumber;
  else
    result := inherited RequestType(aKey,aType);
  end;
end;

{ TDocumentIndexProperty }

function TDocumentIndexProperty.RequestType(aKey: UTF8String;
  aType: TJSValueClass): TJSValueClass;
begin
  if aKey <> LengthKey then
     result := TJSString
  else
     Result:=inherited RequestType(aKey, aType);
end;

{ TDocumentProperties2 }

function TDocumentProperties2.GetCategory: UTF8String;
begin
  result := GetDefault(CategoryKey,'');
end;

function TDocumentProperties2.GetIndex: TDocumentIndexProperty;
begin
  if not hasOwnProperty(IndexKey) then
     PutNewArray(IndexKey);
  result := Get(IndexKey) as TDocumentIndexProperty;
end;

function TDocumentProperties2.GetPublish: Boolean;
begin
  result := GetDefault(PublishKey,false);
end;

function TDocumentProperties2.GetStatus: UTF8String;
begin
  result := GetDefault(StatusKey,'');
end;

function TDocumentProperties2.GetTitle: UTF8String;
begin
  result := GetDefault(TitleKey,'');
end;

procedure TDocumentProperties2.SetCategory(AValue: UTF8String);
begin
  Put(CategoryKey,AValue);
end;

procedure TDocumentProperties2.SetPublish(AValue: Boolean);
begin
  Put(PublishKey,AValue);
end;

procedure TDocumentProperties2.SetStatus(AValue: UTF8String);
begin
  Put(StatusKey,AValue);
end;

procedure TDocumentProperties2.SetTitle(AValue: UTF8String);
begin
  Put(TitleKey,AValue);
end;

function TDocumentProperties2.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  case aKey of
    CategoryKey,StatusKey,TitleKey:
      result := TJSString;
    IndexKey:
      result := TDocumentIndexProperty;
    PublishKey:
      result := TJSBoolean;
  else
    Result:=inherited RequestType(aKey, aType);
  end;

end;

{ TCategoryDefinition }

{ TStatusDefinitions }

constructor TStatusDefinitions.Create;
begin
  inherited Create(TStatusDefintion);
end;

{ TCategoryDefinitions }

constructor TCategoryDefinitions.Create;
begin
  inherited Create(TCategoryDefinition);
end;

{ TDocumentProperties }

procedure TDocumentProperties.IndexChanged(Sender: TObject);
begin
  SetModified;
end;

procedure TDocumentProperties.SetUserProperties(AValue: TJSONData);
begin
  if fUserProperties <> nil then
    FreeAndNil(fUserProperties);
  if AValue <> nil then
  // I have to clone it here, because the streaming system might
  // destroy the original.
    fUserProperties := AValue.Clone;
  SetModified;
end;

function TDocumentProperties.GetIndex: TStrings;
begin
  result := fIndex;
end;

procedure TDocumentProperties.Clear;
begin
  fIndex.Clear;
  fCategory := '';
  fPublish := false;
  fStatus := '';
  fTitle := '';
  FreeAndNil(fUserProperties);
end;

procedure TDocumentProperties.AfterSerialize(aSaver: TJSONStreamer;
  aTarget: TJSONObject);
begin
  if fUserProperties <> nil then
  begin
    // I have to clone here, because the streaming tool is going
    // to delete the properties.
    aTarget['user'] := fUserProperties.Clone;
  end;
end;

procedure TDocumentProperties.BeforeDeserialize(aLoader: TJSONDeStreamer;
  aData: TJSONObject);
var
  aUser: TJSONData;
begin
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

constructor TDocumentProperties.Create(afile: TFile; aIsRoot: Boolean);
var
  path: TFile;
begin
  if aIsRoot then
    path := afile.GetContainedFile('','properties','json',false)
  else
    path := afile.WithDifferentDescriptorAndExtension('properties','json');
  inherited Create(path,true);
  fIndex := TStringList.Create;
  fIndex.OnChange:=@IndexChanged;
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

{ TProjectProperties }

procedure TProjectProperties.SetdefaultDocExtension(AValue: String);
begin
  if (aValue = '') or (aValue[1] <> '.') then
    aValue := '.' + aValue;
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
  if (aValue = '') or (aValue[1] <> '.') then
    aValue := '.' + aValue;
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
  if (aValue = '') or (aValue[1] <> '.') then
    aValue := '.' + aValue;
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
  // Initialize all of the primitive data, clear all of the child data.
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

constructor TProjectProperties.Create(aProjectFile: TFile);
begin
  inherited Create(GetPath(aProjectFile),false);
  Fcategories := TCategoryDefinitions.Create;
  Fcategories.FPOAttachObserver(Self);
  fStatuses := TStatusDefinitions.Create;
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

class function TProjectProperties.GetPath(aFolderPath: TFile): TFile;
begin
  result := aFolderPath.GetContainedFile('','stew','json',false);
end;

end.

