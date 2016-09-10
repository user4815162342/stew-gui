unit stew_properties;

{$mode objfpc}{$H+}
// I'm using com interfaces here, so that I get reference counting, which I want.
{$interfaces COM}

interface

uses
  Classes, SysUtils, Graphics, sys_file, sys_json, sys_dynval, sys_dynval_data, sys_types;

const
  PropertiesGUID = '{17109B5C-1A4E-433F-A1AE-F81713E6E765}';
  DocumentPropertiesGUID = '{03C69F3B-EE44-4C9E-9717-C2723D7D3333}';
  KeywordDefinitionGUID = '{FD202199-1585-43DB-85D5-4FFDA2CD77F8}';
  CategoryDefinitionGUID = '{6D1425B9-0E77-4E2D-851F-3400B61DD577}';
  StatusDefinitionGUID = '{4AD5C991-50EA-4C3E-848C-C89D83EC9DD4}';
  KeywordDefinitionsGUID = '{F63E5862-E538-4974-A098-4E7BB7735535}';
  CategoryDefinitionsGUID = '{02C48ED5-7B6B-45F4-9682-0396628921B0}';
  StatusDefinitionsGUID = '{DCE288FA-DA17-46D4-8680-0C90C0E47BEF}';
  DeadlineGUID = '{F06E93B3-6A7A-432B-A045-CD553D2588D2}';
  DeadlinesGUID = '{546DBE40-2DB8-494E-9E6A-8B7C113FBF2E}';
  ProjectPropertiesGUID = '{D2E4B8B4-2430-43D6-BAA6-F2865D5FA3A4}';

type

  { IProperties }

  IProperties = interface(IDataStoreObject)
    [PropertiesGUID]
    function GetUser: IDynamicValue;
    procedure SetUser(AValue: IDynamicValue);
    property User: IDynamicValue read GetUser write SetUser;
  end;

  { IDocumentProperties }

  IDocumentProperties = interface(IProperties)
    [DocumentPropertiesGUID]
    function GetCategory: UTF8String;
    function GetIndex: TStringArray2;
    function GetPublish: Boolean;
    function GetStatus: UTF8String;
    function GetTitle: UTF8String;
    procedure SetCategory(AValue: UTF8String);
    procedure SetIndex(AValue: TStringArray2);
    procedure SetPublish(AValue: Boolean);
    procedure SetStatus(AValue: UTF8String);
    procedure SetTitle(AValue: UTF8String);
    property Title: UTF8String read GetTitle write SetTitle;
    property Publish: Boolean read GetPublish write SetPublish;
    property Category: UTF8String read GetCategory write SetCategory;
    property Status: UTF8String read GetStatus write SetStatus;
    property Index: TStringArray2 read GetIndex write SetIndex;
  end;

  { IKeywordDefinition }

  IKeywordDefinition = interface(IDataStoreObject)
    [KeywordDefinitionGUID]
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
    property Color: TColor read GetColor write SetColor;
  end;

  { ICategoryDefinition }

  ICategoryDefinition = interface(IKeywordDefinition)
    [CategoryDefinitionGUID]
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
    property PublishTitle: Boolean read GetPublishTitle write SetPublishTitle;
    property PublishTitleLevel: Integer read GetPublishTitleLevel write SetPublishTitleLevel;
    property PublishTitlePrefix: UTF8String read GetPublishTitlePrefix write SetPublishTitlePrefix;
    property PublishMarkerBefore: Boolean read GetPublishMarkerBefore write SetPublishMarkerBefore;
    property PublishMarkerAfter: Boolean read GetPublishMarkerAfter write SetPublishMarkerAfter;
    property PublishMarkerBetween: Boolean read GetPublishMarkerBetween write SetPublishMarkerBetween;
  end;

  IStatusDefinition = interface(IKeywordDefinition)
    [StatusDefinitionGUID]
    // just holds color, so nothing special.
  end;

  IKeywordDefinitions = interface(IDataStoreObject)
    [KeywordDefinitionsGUID]
    function GetKeys: TStringArray2;
    function Has(const aKey: UTF8String): Boolean;
    procedure Delete(const aKey: UTF8String);
    procedure Clear;
    function Enumerate: IDynamicMapEnumerator;
    property Keys: TStringArray2 read GetKeys;
  end;

  { ICategoryDefinitions }

  ICategoryDefinitions = interface(IKeywordDefinitions)
    [CategoryDefinitionsGUID]
    function GetCategory(aKey: UTF8String): ICategoryDefinition;
    procedure SetCategory(aKey: UTF8String; AValue: ICategoryDefinition);
    property Category[aKey: UTF8String]: ICategoryDefinition read GetCategory write SetCategory; default;
  end;

  { IStatusDefinitions }

  IStatusDefinitions = interface(IKeywordDefinitions)
    [StatusDefinitionsGUID]
    function GetStatus(aKey: UTF8String): IStatusDefinition;
    procedure SetStatus(aKey: UTF8String; AValue: IStatusDefinition);
    property Status[aKey: UTF8String]: IStatusDefinition read GetStatus write SetStatus; default;
  end;

  { IDeadline }

  IDeadline = interface(IDataStoreObject)
    [DeadlineGUID]
    function GetDue: TDateTime;
    function GetName: UTF8String;
    procedure SetDue(AValue: TDateTime);
    procedure SetName(AValue: UTF8String);
    property Due: TDateTime read GetDue write SetDue;
    property Name: UTF8String read GetName write SetName;
  end;

  IDeadlines = interface(IDataStoreObject)
    [DeadlinesGUID]
    function GetDeadline(const aIndex: Longint): IDeadline; overload;
    function GetLength: Longint;
    procedure SetDeadline(const aIndex: Longint; const AValue: IDeadline); overload;
    procedure SetLength(const AValue: Longint);
    property Deadline[aIndex: Longint]: IDeadline read GetDeadline write SetDeadline; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(const aItem: IDeadline);
    procedure Add(const aName: UTF8String; const aDue: TDateTime);
    procedure Delete(const aIndex: Longint);
    procedure Clear;
    function IndexOf(const aValue: IDeadline): Longint;
  end;

  { IProjectProperties }

  IProjectProperties = interface(IProperties)
    [ProjectPropertiesGUID]
    function GetCategories: ICategoryDefinitions;
    function GetDeadlines: IDeadlines;
    function GetDefaultCategory: UTF8String;
    function GetDefaultDocExtension: UTF8String;
    function GetDefaultNotesExtension: UTF8String;
    function GetDefaultStatus: UTF8String;
    function GetDefaultThumbnailExtension: UTF8String;
    function GetStatuses: IStatusDefinitions;
    procedure SetDefaultCategory(AValue: UTF8String);
    procedure SetDefaultDocExtension(AValue: UTF8String);
    procedure SetDefaultNotesExtension(AValue: UTF8String);
    procedure SetDefaultStatus(AValue: UTF8String);
    procedure SetDefaultThumbnailExtension(AValue: UTF8String);
    property DefaultDocExtension: UTF8String read GetDefaultDocExtension write SetDefaultDocExtension;
    property DefaultThumbnailExtension: UTF8String read GetDefaultThumbnailExtension write SetDefaultThumbnailExtension;
    property DefaultNotesExtension: UTF8String read GetDefaultNotesExtension write SetDefaultNotesExtension;
    property Categories: ICategoryDefinitions read GetCategories;
    property DefaultCategory: UTF8String read GetDefaultCategory write SetDefaultCategory;
    property Statuses: IStatusDefinitions read GetStatuses;
    property DefaultStatus: UTF8String read GetDefaultStatus write SetDefaultStatus;
    property Deadlines: IDeadlines read GetDeadlines;
  end;

  { TPropertyObjects }

  TPropertyObjects = class
  public
    class function NewDocumentProperties: IDocumentProperties;
    class function NewProjectProperties: IProjectProperties;
    class function NewStatusDefinitions: IStatusDefinitions;
    class function NewCategoryDefinitions: ICategoryDefinitions;
    class function NewDeadlines: IDeadlines;
    class function NewStatusDefinition: IStatusDefinition;
    class function NewCategoryDefinition: ICategoryDefinition;
    class function NewDeadline: IDeadline;
  end;

  { TProperties }

  TProperties = class(TJSObject)
  strict private
    function GetUser: TJSObject;
  strict protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
      ): TJSValueClass; override;
  public
    property User: TJSObject read GetUser;
  end;

  { TDocumentIndexProperty }

  TDocumentIndexProperty = class(TJSArray)
  strict protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  end;

  { TDocumentProperties }

  TDocumentProperties = class(TProperties)
  strict private
    function GetCategory: UTF8String;
    function GetIndex: TDocumentIndexProperty;
    function GetPublish: Boolean;
    function GetStatus: UTF8String;
    function GetTitle: UTF8String;
    procedure SetCategory(AValue: UTF8String);
    procedure SetPublish(AValue: Boolean);
    procedure SetStatus(AValue: UTF8String);
    procedure SetTitle(AValue: UTF8String);
  strict protected
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
  strict private
    function GetBlue: Byte;
    function GetColor: TColor;
    function GetGreen: Byte;
    function GetRed: Byte;
    procedure SetBlue(AValue: Byte);
    procedure SetColor(AValue: TColor);
    procedure SetGreen(AValue: Byte);
    procedure SetRed(AValue: Byte);
  strict protected
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

  { TKeywordDefinition }

  TKeywordDefinition = class(TJSObject)
  strict private
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
  strict protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    property Color: TColor read GetColor write SetColor;
  end;

  TKeywordDefinitionClass = class of TKeywordDefinition;

  { TCategoryDefinition }

  TCategoryDefinition = class(TKeywordDefinition)
  strict private
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

  TStatusDefinition = class(TKeywordDefinition)
    // just holds color, so nothing special...
  end;

  { TKeywordDefinitions }

  generic TKeywordDefinitions<MemberType> = class(TJSObject)
  strict protected
    function RequestType({%H-}aKey: UTF8String; {%H-}aType: TJSValueClass
      ): TJSValueClass; override;
  public
    procedure Assign(aValue: TJSValue); override;
  end;

  { TStatusDefinitions }

  TStatusDefinitions = class(specialize TKeywordDefinitions<TStatusDefinition>)
  public
    function GetStatus(aKey: UTF8String): TStatusDefinition;
  end;

  { TCategoryDefinitions }

  TCategoryDefinitions = class(specialize TKeywordDefinitions<TCategoryDefinition>)
  public
    function GetCategory(aKey: UTF8String): TCategoryDefinition;
  end;

  { TDeadline }

  TDeadline = class(TJSObject)
  private
    function GetDue: TDateTime;
    function GetName: UTF8String;
    procedure SetDue(AValue: TDateTime);
    procedure SetName(AValue: UTF8String);
  protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  public
    property Name: UTF8String read GetName write SetName;
    property Due: TDateTime read GetDue write SetDue;
  end;


  { TDeadlines }

  TDeadlines = class(TJSArray)
  private
    function GetDeadline(aIndex: Integer): TDeadline;
  protected
     function RequestType(aKey: UTF8String; aType: TJSValueClass
        ): TJSValueClass; override;
  public
     function Add: TDeadline;
     function Add(aName: UTF8String; aDue: TDateTime): TDeadline;
     procedure Remove(aIndex: Integer);
     property Deadline[aIndex: Integer]: TDeadline read GetDeadline; default;
  end;

  { TProjectProperties }

  TProjectProperties = class(TProperties)
  strict private
    function GetCategories: TCategoryDefinitions;
    function GetDeadlines: TDeadlines;
    function GetDefaultCategory: UTF8String;
    function GetDefaultDocExtension: UTF8String;
    function GetDefaultNotesExtension: UTF8String;
    function GetDefaultStatus: UTF8String;
    function GetDefaultThumbnailExtension: UTF8String;
    function GetStatuses: TStatusDefinitions;
    procedure SetDefaultCategory(AValue: UTF8String);
    procedure SetDefaultDocExtension(AValue: UTF8String);
    procedure SetDefaultNotesExtension(AValue: UTF8String);
    procedure SetDefaultStatus(AValue: UTF8String);
    procedure SetDefaultThumbnailExtension(AValue: UTF8String);
  strict protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
    function CreateStringValue(aKey: UTF8String; aRequestType: TJSValueClass;
      aValue: UTF8String): TJSValue; override; overload;
  public
    property DefaultDocExtension: UTF8String read GetDefaultDocExtension write SetDefaultDocExtension;
    property DefaultThumbnailExtension: UTF8String read GetDefaultThumbnailExtension write SetDefaultThumbnailExtension;
    property DefaultNotesExtension: UTF8String read GetDefaultNotesExtension write SetDefaultNotesExtension;
    property Categories: TCategoryDefinitions read GetCategories;
    property DefaultCategory: UTF8String read GetDefaultCategory write SetDefaultCategory;
    property Statuses: TStatusDefinitions read GetStatuses;
    property DefaultStatus: UTF8String read GetDefaultStatus write SetDefaultStatus;
    property Deadlines: TDeadlines read GetDeadlines;
  end;

  const
    UserKey = 'user';
    CategoriesKey = 'categories';
    StatusesKey = 'statuses';
    DefaultCategoryKey = 'defaultCategory';
    DefaultStatusKey = 'defaultStatus';
    DeadlinesKey = 'deadlines';
    DueKey = 'due';
    NameKey = 'name';
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

implementation

uses
  LCLProc, stew_properties_implementation;

{ TPropertyObjects }

class function TPropertyObjects.NewDocumentProperties: IDocumentProperties;
begin
  result := stew_properties_implementation.TDocumentProperties.Create;
end;

class function TPropertyObjects.NewProjectProperties: IProjectProperties;
begin
  result := stew_properties_implementation.TProjectProperties.Create;
end;

class function TPropertyObjects.NewStatusDefinitions: IStatusDefinitions;
begin
  result := stew_properties_implementation.TStatusDefinitions.Create;
end;

class function TPropertyObjects.NewCategoryDefinitions: ICategoryDefinitions;

begin
  result := stew_properties_implementation.TCategoryDefinitions.Create;
end;

class function TPropertyObjects.NewDeadlines: IDeadlines;
begin
  result := stew_properties_implementation.TDeadlines.Create;
end;

class function TPropertyObjects.NewStatusDefinition: IStatusDefinition;
begin
  result := stew_properties_implementation.TStatusDefinition.Create;
end;

class function TPropertyObjects.NewCategoryDefinition: ICategoryDefinition;
begin
  result := stew_properties_implementation.TCategoryDefinition.Create;
end;

class function TPropertyObjects.NewDeadline: IDeadline;
begin
  result := stew_properties_implementation.TDeadline.Create;
end;


{ TDeadlines }

function TDeadlines.GetDeadline(aIndex: Integer): TDeadline;
begin
  result := Get(aIndex) as TDeadline;
end;

function TDeadlines.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  if aKey <> LengthKey then
     result := TDeadline
  else
     Result:=inherited RequestType(aKey, aType);
end;

function TDeadlines.Add: TDeadline;
begin
  result := PutNewObject(Length) as TDeadline;
end;

function TDeadlines.Add(aName: UTF8String; aDue: TDateTime): TDeadline;
begin
  result := Self.Add;
  result.Name := aName;
  result.Due := aDue;
end;

procedure TDeadlines.Remove(aIndex: Integer);
begin
  Splice(aIndex,1,0);
end;

{ TDeadline }

function TDeadline.GetDue: TDateTime;
begin
  result := (Get(DueKey) as TJSDate).Value;
end;

function TDeadline.GetName: UTF8String;
begin
  result := Get(NameKey).AsString;
end;

procedure TDeadline.SetDue(AValue: TDateTime);
begin
  Put(DueKey,TJSDate.CreateDate(AValue));
end;

procedure TDeadline.SetName(AValue: UTF8String);
begin
  Put(NameKey,AValue);

end;

function TDeadline.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  case aKey of
    DueKey:
       result := TJSDate;
    NameKey:
      result := TJSString;
  else
    Result:=inherited RequestType(aKey, aType);
  end;
end;

{ TCategoryDefinitions }

function TCategoryDefinitions.GetCategory(aKey: UTF8String
  ): TCategoryDefinition;
var
  lResult: TJSValue;
begin
  lResult := Get(aKey);
  if lResult is TJSUndefined then
    result := nil
  else
    result := lResult as TCategoryDefinition;
end;

{ TStatusDefinitions }

function TStatusDefinitions.GetStatus(aKey: UTF8String): TStatusDefinition;
var
  lResult: TJSValue;
begin
  lResult := Get(aKey);
  if lResult is TJSUndefined then
    result := nil
  else
    result := lResult as TStatusDefinition;
end;

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

{ TProjectProperties }

function TProjectProperties.GetCategories: TCategoryDefinitions;
begin
  if not hasOwnProperty(CategoriesKey) then
     PutNewObject(CategoriesKey);
  result := Get(CategoriesKey) as TCategoryDefinitions;
end;

function TProjectProperties.GetDeadlines: TDeadlines;
begin
  if not hasOwnProperty(DeadlinesKey) then
     PutNewObject(DeadlinesKey);
  result := Get(DeadlinesKey) as TDeadlines;
end;

function TProjectProperties.GetDefaultCategory: UTF8String;
begin
  result := GetDefault(DefaultCategoryKey,'');
end;

function TProjectProperties.GetDefaultDocExtension: UTF8String;
begin
  result := GetDefault(DefaultDocExtensionKey,'');
end;

function TProjectProperties.GetDefaultNotesExtension: UTF8String;
begin
  result := GetDefault(DefaultNotesExtensionKey,'');
end;

function TProjectProperties.GetDefaultStatus: UTF8String;
begin
  result := GetDefault(DefaultStatusKey,'');
end;

function TProjectProperties.GetDefaultThumbnailExtension: UTF8String;
begin
  result := GetDefault(DefaultThumbnailExtensionKey,'');
end;

function TProjectProperties.GetStatuses: TStatusDefinitions;
begin
  if not hasOwnProperty(StatusesKey) then
     PutNewObject(StatusesKey);
  result := Get(StatusesKey) as TStatusDefinitions;
end;

procedure TProjectProperties.SetDefaultCategory(AValue: UTF8String);
begin
  Put(DefaultCategoryKey,AValue);
end;

procedure TProjectProperties.SetDefaultDocExtension(AValue: UTF8String);
begin
  Put(DefaultDocExtensionKey,AValue);
end;

procedure TProjectProperties.SetDefaultNotesExtension(AValue: UTF8String);
begin
  Put(DefaultNotesExtensionKey,AValue);
end;

procedure TProjectProperties.SetDefaultStatus(AValue: UTF8String);
begin
  Put(DefaultStatusKey,AValue);
end;

procedure TProjectProperties.SetDefaultThumbnailExtension(AValue: UTF8String);
begin
  Put(DefaultThumbnailExtensionKey,AValue);
end;

function TProjectProperties.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  case aKey of
    StatusesKey:
      result := TStatusDefinitions;
    CategoriesKey:
      result := TCategoryDefinitions;
    DefaultStatusKey, DefaultCategoryKey,
    DefaultDocExtensionKey, DefaultThumbnailExtensionKey, DefaultNotesExtensionKey:
      result := TJSString;
    DeadlinesKey:
      result := TDeadlines;
  else
    Result:=inherited RequestType(aKey, aType);
  end;
end;

function TProjectProperties.CreateStringValue(aKey: UTF8String;
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

{ TKeywordDefinitions }

function TKeywordDefinitions.RequestType(aKey: UTF8String; aType: TJSValueClass
  ): TJSValueClass;
begin
  result := MemberType;
end;

procedure TKeywordDefinitions.Assign(aValue: TJSValue);
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

{ TCategoryDefinition }

function TCategoryDefinition.GetPublishMarkerAfter: Boolean;
begin
  result := GetDefault(PublishMarkerAfterKey,false);
end;

function TCategoryDefinition.GetPublishMarkerBefore: Boolean;
begin
  Result := GetDefault(PublishMarkerBeforeKey,false);
end;

function TCategoryDefinition.GetPublishMarkerBetween: Boolean;
begin
  result := GetDefault(PublishMarkerBetweenKey,false);
end;

function TCategoryDefinition.GetPublishTitle: Boolean;
begin
  result := GetDefault(PublishTitleKey,false);
end;

function TCategoryDefinition.GetPublishTitleLevel: Integer;
begin
  result := trunc(GetDefault(PublishTitleLevelKey,0));
end;

function TCategoryDefinition.GetPublishTitlePrefix: UTF8String;
begin
  result := GetDefault(PublishTitlePrefixKey,'');

end;

procedure TCategoryDefinition.SetPublishMarkerAfter(AValue: Boolean);
begin
  Put(PublishMarkerAfterKey,AValue);

end;

procedure TCategoryDefinition.SetPublishMarkerBefore(AValue: Boolean);
begin
  Put(PublishMarkerBeforeKey,AValue);

end;

procedure TCategoryDefinition.SetPublishMarkerBetween(AValue: Boolean);
begin
  Put(PublishMarkerBetweenKey,AValue);

end;

procedure TCategoryDefinition.SetPublishTitle(AValue: Boolean);
begin
  Put(PublishTitleKey,aValue);

end;

procedure TCategoryDefinition.SetPublishTitleLevel(AValue: Integer);
begin
  Put(PublishTitleLevelKey,aValue);

end;

procedure TCategoryDefinition.SetPublishTitlePrefix(AValue: UTF8String);
begin
  Put(PublishTitlePrefixKey,aValue);

end;

{ TKeywordDefinition }

function TKeywordDefinition.GetColor: TColor;
begin
  if hasOwnProperty(ColorKey) then
    result := (Get(ColorKey) as TJSColor).AsColor
  else
    result := clNone;
end;

procedure TKeywordDefinition.SetColor(AValue: TColor);
var
  lValue: TJSColor;
begin
  if hasOwnProperty(ColorKey) then
    lValue := Get(ColorKey) as TJSColor
  else
    lValue := PutNewObject(ColorKey) as TJSColor;
  lValue.AsColor := AValue;
end;

function TKeywordDefinition.RequestType(aKey: UTF8String; aType: TJSValueClass
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
      lValue := Byte(trunc(aValue));
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

{ TDocumentProperties }

function TDocumentProperties.GetCategory: UTF8String;
begin
  result := GetDefault(CategoryKey,'');
end;

function TDocumentProperties.GetIndex: TDocumentIndexProperty;
begin
  if not hasOwnProperty(IndexKey) then
     PutNewArray(IndexKey);
  result := Get(IndexKey) as TDocumentIndexProperty;
end;

function TDocumentProperties.GetPublish: Boolean;
begin
  result := GetDefault(PublishKey,false);
end;

function TDocumentProperties.GetStatus: UTF8String;
begin
  result := GetDefault(StatusKey,'');
end;

function TDocumentProperties.GetTitle: UTF8String;
begin
  result := GetDefault(TitleKey,'');
end;

procedure TDocumentProperties.SetCategory(AValue: UTF8String);
begin
  Put(CategoryKey,AValue);
end;

procedure TDocumentProperties.SetPublish(AValue: Boolean);
begin
  Put(PublishKey,AValue);
end;

procedure TDocumentProperties.SetStatus(AValue: UTF8String);
begin
  Put(StatusKey,AValue);
end;

procedure TDocumentProperties.SetTitle(AValue: UTF8String);
begin
  Put(TitleKey,AValue);
end;

function TDocumentProperties.RequestType(aKey: UTF8String; aType: TJSValueClass
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

{ TCategoryDefinitions }

{ TDocumentProperties }

{ TKeywordDefinition }

function ToByte(x: Integer): Byte;
begin
  if x > high(Byte) then
    result := high(Byte)
  else if x < low(Byte) then
    result := Low(Byte)
  else
    result := Byte(x);
end;

{ TProjectProperties }

end.

