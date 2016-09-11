unit stew_properties;

{$mode objfpc}{$H+}
// I'm using com interfaces here, so that I get reference counting, which I want.
{$interfaces COM}

interface

uses
  Classes, SysUtils, Graphics, sys_json, sys_dynval, sys_dynval_data, sys_types;

const
  PropertiesGUID = '{17109B5C-1A4E-433F-A1AE-F81713E6E765}';
  DocumentIndexGUID = '{3C75861F-E544-496D-964A-A83A0C19BBB4}';
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
    function GetUser: IDynamicMap;
    procedure SetUser(AValue: IDynamicMap);
    property User: IDynamicMap read GetUser write SetUser;
  end;

  { IDocumentIndex }

  IDocumentIndex = interface(IDataStoreObject)
    [DocumentIndexGUID]
    function GetIndexItem(aIndex: Longint): UTF8String;
    function GetLength: Longint;
    procedure SetIndexItem(aIndex: Longint; AValue: UTF8String);
    procedure SetLength(AValue: Longint);
    property Item[aIndex: Longint]: UTF8String read GetIndexItem write SetIndexItem; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(const aItem: UTF8String);
    procedure Delete(const aIndex: Longint);
    procedure Clear;
    function IndexOf(const aValue: UTF8String): Longint;
  end;

  { IDocumentProperties }

  IDocumentProperties = interface(IProperties)
    [DocumentPropertiesGUID]
    function GetCategory: UTF8String;
    function GetIndex: IDocumentIndex;
    function GetPublish: Boolean;
    function GetStatus: UTF8String;
    function GetTitle: UTF8String;
    procedure SetCategory(AValue: UTF8String);
    procedure SetPublish(AValue: Boolean);
    procedure SetStatus(AValue: UTF8String);
    procedure SetTitle(AValue: UTF8String);
    property Title: UTF8String read GetTitle write SetTitle;
    property Publish: Boolean read GetPublish write SetPublish;
    property Category: UTF8String read GetCategory write SetCategory;
    property Status: UTF8String read GetStatus write SetStatus;
    property Index: IDocumentIndex read GetIndex;
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
    property Items[aIndex: Longint]: IDeadline read GetDeadline write SetDeadline; default;
    property Length: Longint read GetLength write SetLength;
    procedure Add(const aItem: IDeadline);
    procedure Add(const aName: UTF8String; const aDue: TDateTime);
    procedure Delete(const aIndex: Longint);
    procedure Clear;
    function IndexOf(const aValue: IDeadline): Longint;
    procedure Sort;
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
    class function NewDocumentIndex: IDocumentIndex;
    class function NewStatusDefinitions: IStatusDefinitions;
    class function NewCategoryDefinitions: ICategoryDefinitions;
    class function NewDeadlines: IDeadlines;
    class function NewStatusDefinition: IStatusDefinition;
    class function NewCategoryDefinition: ICategoryDefinition;
    class function NewDeadline: IDeadline;
  end;


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

class function TPropertyObjects.NewDocumentIndex: IDocumentIndex;
begin
  result := stew_properties_implementation.TDocumentIndex.Create;
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


end.

