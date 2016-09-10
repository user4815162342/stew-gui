unit stew_properties_implementation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stew_properties, sys_dynval, sys_dynval_data, sys_types, Graphics;

type

  { TProperties }

  TProperties = class(TDataStoreMap,IProperties)
  private
    fUser: IDynamicValue;
  strict protected
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure InitializeBlank; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    function GetUser: IDynamicValue;
    procedure SetUser(AValue: IDynamicValue);
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
  public
    destructor Destroy; override;
  end;

  { TDocumentProperties }

  TDocumentProperties = class(TProperties,IDocumentProperties)
  private
    fCategory: UTF8String;
    fIndex: TStringArray2;
    fPublish: Boolean;
    fStatus: UTF8String;
    fTitle: UTF8String;
  strict protected
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure InitializeBlank; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
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
  public
    destructor Destroy; override;
  end;

  { TKeywordDefinition }

  TKeywordDefinition = class(TDataStoreMap,IKeywordDefinition)
  private
    fColor: TColor;
  strict protected
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure InitializeBlank; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    function GetColor: TColor;
    procedure SetColor(AValue: TColor);
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
  end;

  { TCategoryDefinition }

  TCategoryDefinition = class(TKeywordDefinition,ICategoryDefinition)
  private
    fPublishMarkerAfter: Boolean;
    fPublishMarkerBefore: Boolean;
    fPublishMarkerBetween: Boolean;
    fPublishTitle: Boolean;
    fPublishTitleLevel: Longint;
    fPublishTitlePrefix: UTF8String;
  strict protected
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure InitializeBlank; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
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
  end;

  TStatusDefinition = class(TKeywordDefinition,IStatusDefinition)
  end;

  { TKeywordDefinitions }

  TKeywordDefinitions = class(TDataStoreObject,IKeywordDefinitions)
  private
    fValues: IDynamicMap;
  strict protected
    procedure InitializeBlank; override;
    function GetKeys: TStringArray2;
    function Has(const aKey: UTF8String): Boolean;
    procedure Delete(const aKey: UTF8String);
    procedure Clear;
    function Enumerate: IDynamicMapEnumerator;
    function GetItem(const aKey: UTF8String): IKeywordDefinition;
    procedure SetItem(const aKey: UTF8String; const AValue: IKeywordDefinition);
      virtual; abstract;
    function GetItem(const {%H-}aKey: IDynamicValue): IDynamicValue; override; overload;
    procedure SetItem(const {%H-}aKey: IDynamicValue; const {%H-}AValue: IDynamicValue); override; overload;
    function CreateDefinition: IKeywordDefinition; virtual; abstract;
  public
    procedure Deserialize(aReader: TDynamicValueReader); override;
    procedure Serialize(aWriter: TDynamicValueWriter); override;
  end;

  { TCategoryDefinitions }

  TCategoryDefinitions = class(TKeywordDefinitions,ICategoryDefinitions)
  strict protected
    function GetCategory(aKey: UTF8String): ICategoryDefinition;
    procedure SetCategory(aKey: UTF8String; AValue: ICategoryDefinition);
    procedure SetItem(const aKey: UTF8String; const AValue: IKeywordDefinition); override;
    function CreateDefinition: IKeywordDefinition; override;

  end;

  { TStatusDefinitions }

  TStatusDefinitions = class(TKeywordDefinitions,IStatusDefinitions)
  strict protected
    function CreateDefinition: IKeywordDefinition; override;
    function GetStatus(aKey: UTF8String): IStatusDefinition;
    procedure SetStatus(aKey: UTF8String; AValue: IStatusDefinition);
    procedure SetItem(const aKey: UTF8String; const AValue: IKeywordDefinition); override;

  end;

  { TDeadline }

  TDeadline = class(TDataStoreMap,IDeadline)
  private
    fDue: TDateTime;
    fName: UTF8String;
  strict protected
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure InitializeBlank; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    function GetDue: TDateTime;
    procedure SetDue(AValue: TDateTime);
    function GetName: UTF8String;
    procedure SetName(AValue: UTF8String);
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
  end;

  { TDeadlines }

  TDeadlines = class(TDataStoreList,IDeadlines)
  private
    fItems: IDynamicList;
  strict protected
    function GetItem(const aKey: Longint): IDynamicValue; overload; override;
    function GetLength: Longint; override;
    procedure InitializeBlank; override;
    procedure SetItem(const aKey: Longint; const AValue: IDynamicValue);
      overload; override;
    procedure SetLength(const AValue: Longint); override;
    function GetDeadline(const aIndex: Longint): IDeadline; overload;
    procedure SetDeadline(const aIndex: Longint; const AValue: IDeadline); overload;
    function ReadManagedItem(aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedItems(aWriter: TDynamicValueWriter); override;
  public
    procedure Add(const aItem: IDynamicValue); override;
    procedure Add(const aItem: IDeadline);
    procedure Add(const aName: UTF8String; const aDue: TDateTime);
    procedure Clear; override;
    procedure Delete(const aIndex: Longint); override;
    function IndexOf(const aValue: IDynamicValue): Longint; override;
    function IndexOf(const aValue: IDeadline): Longint;
    function Owns(const aValue: IDynamicValue): Boolean; override;

  end;

  { TProjectProperties }

  TProjectProperties = class(TProperties,IProjectProperties)
  private
    fCategories: ICategoryDefinitions;
    fDeadlines: IDeadlines;
    fDefaultCategory: UTF8String;
    fDefaultDocExtension: UTF8String;
    fDefaultNotesExtension: UTF8String;
    fDefaultStatus: UTF8String;
    fDefaultThumbnailExtension: UTF8String;
    fStatuses: IStatusDefinitions;
  strict protected
    function GetItem(const aKey: UTF8String): IDynamicValue; overload; override;
    procedure InitializeBlank; override;
    procedure SetItem(const aKey: UTF8String; const AValue: IDynamicValue);
      overload; override;
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure ListManagedKeys(var aValue: TStringArray2); override;
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
  public
    destructor Destroy; override;
    property DefaultDocExtension: UTF8String read GetDefaultDocExtension write SetDefaultDocExtension;
    property DefaultThumbnailExtension: UTF8String read GetDefaultThumbnailExtension write SetDefaultThumbnailExtension;
    property DefaultNotesExtension: UTF8String read GetDefaultNotesExtension write SetDefaultNotesExtension;
    property Categories: ICategoryDefinitions read GetCategories;
    property DefaultCategory: UTF8String read GetDefaultCategory write SetDefaultCategory;
    property Statuses: IStatusDefinitions read GetStatuses;
    property DefaultStatus: UTF8String read GetDefaultStatus write SetDefaultStatus;
    property Deadlines: IDeadlines read GetDeadlines;
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

procedure RaiseInvalidKeyValue(const aKey: String; const aValue: IDynamicValue);


implementation

uses
  sys_file;

procedure RaiseInvalidKeyValue(const aKey: String; const aValue: IDynamicValue);
var
  lMessage: UTF8String;
begin
  lMessage := 'Invalid value for "' + aKey + '": ';
  case aValue.KindOf of
    dvkUndefined:
      lMessage := lMessage + 'undefined';
    dvkNull:
      lMessage := lMessage + 'null';
    dvkBoolean:
      lMessage := lMessage + BoolToStr((aValue as IDynamicBoolean).Value);
    dvkNumber:
      lMessage := lMessage + FloatToStr((aValue as IDynamicNumber).Value);
    dvkString:
      lMessage := lMessage + '"' + (aValue as IDynamicString).Value + '"';
    dvkList:
      lMessage := lMessage + 'list';
    dvkMap:
      lMessage := lMessage + 'map';
    dvkObject:
      lMessage := lMessage + 'object';
  end;
  raise Exception.Create(lMessage);
end;

{ TProjectProperties }

function TProjectProperties.GetItem(const aKey: UTF8String): IDynamicValue;
begin
  case aKey of
    CategoriesKey:
      result := fCategories;
    DeadlinesKey:
      result := fDeadlines;
    DefaultCategoryKey:
      result := fDefaultCategory;
    DefaultDocExtensionKey:
      result := fDefaultDocExtension;
    DefaultNotesExtensionKey:
      result := fDefaultNotesExtension;
    DefaultStatusKey:
      result := fDefaultStatus;
    DefaultThumbnailExtensionKey:
      result := fDefaultThumbnailExtension;
    StatusesKey:
      result := fStatuses;
  else
    Result:=inherited GetItem(aKey);
  end;
end;

procedure TProjectProperties.InitializeBlank;
begin
  fCategories := TPropertyObjects.NewCategoryDefinitions;
  fDeadlines := TPropertyObjects.NewDeadlines;
  fDefaultCategory := '';
  fDefaultDocExtension := '';
  fDefaultNotesExtension := '';
  fDefaultStatus := '';
  fDefaultThumbnailExtension := '';
  fStatuses := TPropertyObjects.NewStatusDefinitions;
  inherited InitializeBlank;
end;

procedure TProjectProperties.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);
begin
  case aKey of
    CategoriesKey,
    DeadlinesKey,
    StatusesKey:
      raise Exception.Create('Categories, deadlines, statuses can only be manipulated, not set');
    DefaultCategoryKey:
      if AValue is IDynamicString then
         DefaultCategory := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(DefaultCategoryKey,AValue);
    DefaultDocExtensionKey:
      if AValue is IDynamicString then
         DefaultDocExtension := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(DefaultDocExtensionKey,AValue);
    DefaultNotesExtensionKey:
      if AValue is IDynamicString then
         DefaultNotesExtension := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(DefaultNotesExtensionKey,AValue);
    DefaultStatusKey:
      if AValue is IDynamicString then
         DefaultStatus := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(DefaultStatusKey,AValue);
    DefaultThumbnailExtensionKey:
      if AValue is IDynamicString then
         DefaultThumbnailExtension := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(DefaultThumbnailExtensionKey,AValue);
  else
    inherited SetItem(aKey, AValue);
  end;
end;

function TProjectProperties.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    CategoriesKey:
      fCategories.Deserialize(aReader);
    DeadlinesKey:
      fDeadlines.Deserialize(aReader);
    StatusesKey:
      fStatuses.Deserialize(aReader);
    DefaultCategoryKey,
    DefaultDocExtensionKey,
    DefaultNotesExtensionKey,
    DefaultStatusKey,
    DefaultThumbnailExtensionKey:
      SetItem(aKey,aReader.ReadValue);
  else
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;

end;

procedure TProjectProperties.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKey(CategoriesKey);
  fCategories.Serialize(aWriter);
  aWriter.WriteKeyValue(DefaultCategoryKey,GetItem(DefaultCategoryKey));
  aWriter.WriteKey(StatusesKey);
  fStatuses.Serialize(aWriter);
  aWriter.WriteKeyValue(DefaultStatusKey,GetItem(DefaultStatusKey));
  aWriter.WriteKey(DeadlinesKey);
  fDeadlines.Serialize(aWriter);
  aWriter.WriteKeyValue(DefaultDocExtensionKey,GetItem(DefaultDocExtensionKey));
  aWriter.WriteKeyValue(DefaultNotesExtensionKey,GetItem(DefaultNotesExtensionKey));
  aWriter.WriteKeyValue(DefaultThumbnailExtensionKey,GetItem(DefaultThumbnailExtensionKey));
  inherited WriteManagedKeys(aWriter);
end;

procedure TProjectProperties.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(CategoriesKey);
  aValue.Add(DefaultCategoryKey);
  aValue.Add(StatusesKey);
  aValue.Add(DefaultStatusKey);
  aValue.Add(DeadlinesKey);
  aValue.Add(DefaultDocExtensionKey);
  aValue.Add(DefaultNotesExtensionKey);
  aValue.Add(DefaultThumbnailExtensionKey);
  inherited ListManagedKeys(aValue);
end;

function TProjectProperties.GetCategories: ICategoryDefinitions;
begin
  result := fCategories;
end;

function TProjectProperties.GetDeadlines: IDeadlines;
begin
  result := fDeadlines;
end;

function TProjectProperties.GetDefaultCategory: UTF8String;
begin
  result := fDefaultCategory;
end;

function TProjectProperties.GetDefaultDocExtension: UTF8String;
begin
  result := fDefaultDocExtension;
end;

function TProjectProperties.GetDefaultNotesExtension: UTF8String;
begin
  result := fDefaultNotesExtension;
end;

function TProjectProperties.GetDefaultStatus: UTF8String;
begin
  result := fDefaultStatus;
end;

function TProjectProperties.GetDefaultThumbnailExtension: UTF8String;
begin
  result := fDefaultThumbnailExtension;
end;

function TProjectProperties.GetStatuses: IStatusDefinitions;
begin
  result := fStatuses;
end;

procedure TProjectProperties.SetDefaultCategory(AValue: UTF8String);
begin
  fDefaultCategory := AValue;
end;

procedure TProjectProperties.SetDefaultDocExtension(AValue: UTF8String);
begin
  fDefaultDocExtension := ExcludeExtensionDelimiter(AValue);
end;

procedure TProjectProperties.SetDefaultNotesExtension(AValue: UTF8String);
begin
  fDefaultNotesExtension := ExcludeExtensionDelimiter(AValue);
end;

procedure TProjectProperties.SetDefaultStatus(AValue: UTF8String);
begin
  fDefaultStatus := AValue;
end;

procedure TProjectProperties.SetDefaultThumbnailExtension(AValue: UTF8String);
begin
  fDefaultThumbnailExtension := ExcludeExtensionDelimiter(AValue);
end;

destructor TProjectProperties.Destroy;
begin
  fStatuses := nil;
  fCategories := nil;
  fDeadlines := nil;
  inherited Destroy;
end;

{ TDeadlines }

procedure TDeadlines.Add(const aItem: IDynamicValue);
begin
   if aItem is IDeadline then
      fItems.Add(aItem)
   else
      raise Exception.Create('TDeadlines must contain TDeadline');
end;

procedure TDeadlines.Add(const aItem: IDeadline);
begin
  fItems.Add(aItem);
end;

procedure TDeadlines.Add(const aName: UTF8String; const aDue: TDateTime);
var
  lDeadline: IDeadline;
begin
  lDeadline := TPropertyObjects.NewDeadline;
  lDeadline.Name := aName;
  lDeadline.Due := aDue;
  Add(lDeadline);

end;

procedure TDeadlines.Clear;
begin
  fItems.Clear;
end;

procedure TDeadlines.Delete(const aIndex: Longint);
begin
  fItems.Delete(aIndex);
end;

function TDeadlines.GetItem(const aKey: Longint): IDynamicValue;
begin
  result := fItems.GetItem(aKey);
end;

function TDeadlines.GetLength: Longint;
begin
  result := fItems.Length;
end;

function TDeadlines.IndexOf(const aValue: IDynamicValue): Longint;
begin
  result := fItems.IndexOf(aValue);
end;

function TDeadlines.IndexOf(const aValue: IDeadline): Longint;
begin
  result := fItems.IndexOf(aValue);
end;

function TDeadlines.Owns(const aValue: IDynamicValue): Boolean;
begin
  result := fItems.Owns(aValue);
end;

procedure TDeadlines.InitializeBlank;
begin
  fItems := TDynamicValues.NewList;
  inherited InitializeBlank;
end;

procedure TDeadlines.SetItem(const aKey: Longint; const AValue: IDynamicValue);
begin
   if AValue is IDeadline then
      fItems.SetItem(aKey,AValue)
   else
      raise Exception.Create('TDeadlines must contain TDeadline');
end;

procedure TDeadlines.SetLength(const AValue: Longint);
begin
  fItems.SetLength(AValue);
end;

function TDeadlines.GetDeadline(const aIndex: Longint): IDeadline;
begin
  result := GetItem(aIndex) as IDeadline;
end;

procedure TDeadlines.SetDeadline(const aIndex: Longint; const AValue: IDeadline
  );
begin
  SetItem(aIndex,AValue);
end;

function TDeadlines.ReadManagedItem(aReader: TDynamicValueReader): Boolean;
begin
  Add(aReader.ReadValue);
  result := true;
end;

procedure TDeadlines.WriteManagedItems(aWriter: TDynamicValueWriter);
var
  i: Longint;
  l: Longint;
begin
  l := fItems.Length;
  for i := 0 to l - 1 do
     aWriter.WriteValue(fItems[i]);
end;

{ TDeadline }

function TDeadline.GetItem(const aKey: UTF8String): IDynamicValue;
begin
  case aKey of
    DueKey:
      result := fDue;
    NameKey:
      result := fName;
  else
    Result:=inherited GetItem(aKey);
  end;
end;

procedure TDeadline.InitializeBlank;
begin
  fDue := 0;
  fName := '';
  inherited InitializeBlank;
end;

procedure TDeadline.SetItem(const aKey: UTF8String; const AValue: IDynamicValue
  );
begin
  case aKey of
    DueKey:
      if AValue is IDynamicString then
        fDue := ISO8601ToDateTime((AValue as IDynamicString).Value)
      else
         RaiseInvalidKeyValue(DueKey,AValue);
    NameKey:
      if AValue is IDynamicString then
        fName := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(NameKey,AValue);
  else
    inherited SetItem(aKey, AValue);
  end;
end;

function TDeadline.GetDue: TDateTime;
begin
  result := fDue;
end;

procedure TDeadline.SetDue(AValue: TDateTime);
begin
  fDue := AValue;
end;

function TDeadline.GetName: UTF8String;
begin
  result := fName;
end;

procedure TDeadline.SetName(AValue: UTF8String);
begin
  fName := AValue;
end;

function TDeadline.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    DueKey,
    NameKey:
      SetItem(aKey,aReader.ReadValue);
  else
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;
end;

procedure TDeadline.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKeyValue(NameKey,GetItem(NameKey));
  aWriter.WriteKeyValue(DueKey,GetItem(DueKey));
  inherited WriteManagedKeys(aWriter);
end;

procedure TDeadline.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(NameKey);
  aValue.Add(DueKey);
  inherited ListManagedKeys(aValue);
end;

{ TStatusDefinitions }

function TStatusDefinitions.CreateDefinition: IKeywordDefinition;
begin
    result := TPropertyObjects.NewStatusDefinition;
end;

function TStatusDefinitions.GetStatus(aKey: UTF8String): IStatusDefinition;
begin
  result := GetItem(aKey) as IStatusDefinition;
end;

procedure TStatusDefinitions.SetStatus(aKey: UTF8String;
  AValue: IStatusDefinition);
begin
  SetItem(aKey,AValue);
end;

procedure TStatusDefinitions.SetItem(const aKey: UTF8String;
  const AValue: IKeywordDefinition);
begin
  if AValue is IStatusDefinition then
     fValues.SetItem(aKey,AValue)
  else
     raise Exception.Create('TStatusDefinitions can only contain TStatusDefinition');
end;

{ TCategoryDefinitions }

function TCategoryDefinitions.GetCategory(aKey: UTF8String
  ): ICategoryDefinition;
begin
  result := GetItem(aKey) as ICategoryDefinition;
end;

procedure TCategoryDefinitions.SetCategory(aKey: UTF8String;
  AValue: ICategoryDefinition);
begin
  SetItem(aKey,AValue);
end;

procedure TCategoryDefinitions.SetItem(const aKey: UTF8String;
  const AValue: IKeywordDefinition);
begin
  if AValue is ICategoryDefinition then
     fValues.SetItem(aKey,AValue)
  else
     raise Exception.Create('TCategoryDefinitions can only contain TCategoryDefinition');
end;

function TCategoryDefinitions.CreateDefinition: IKeywordDefinition;
begin
  result := TPropertyObjects.NewCategoryDefinition;
end;

{ TKeywordDefinitions }

procedure TKeywordDefinitions.InitializeBlank;
begin
    fValues := TDynamicValues.NewMap;
end;

function TKeywordDefinitions.GetKeys: TStringArray2;
begin
  result := fValues.GetKeys;
end;

function TKeywordDefinitions.Has(const aKey: UTF8String): Boolean;
begin
  result := fValues.Has(aKey);
end;

procedure TKeywordDefinitions.Delete(const aKey: UTF8String);
begin
  fValues.Delete(aKey);
end;

procedure TKeywordDefinitions.Clear;
begin
  fValues.Clear;
end;

function TKeywordDefinitions.Enumerate: IDynamicMapEnumerator;
begin
  result := fValues.Enumerate;
end;

function TKeywordDefinitions.GetItem(const aKey: UTF8String
  ): IKeywordDefinition;
begin
  Result:=fValues.GetItem(aKey) as IKeywordDefinition;
end;

function TKeywordDefinitions.GetItem(const aKey: IDynamicValue): IDynamicValue;
begin
  Result:=fValues.GetItem(aKey);
end;

procedure TKeywordDefinitions.SetItem(const aKey: IDynamicValue;
  const AValue: IDynamicValue);
begin
  if AValue is IKeywordDefinition then
     SetItem(aKey,AValue as IKeywordDefinition)
  else
     raise Exception.Create('Invalid value for keyword definitions');
end;

procedure TKeywordDefinitions.Deserialize(aReader: TDynamicValueReader);
var
  lKey: UTF8String;
  lKeywordDefinition: IKeywordDefinition;
begin
  InitializeBlank;
  if aReader <> nil then
  begin;
    if aReader.IsMapStart then
    begin
      aReader.ReadMapStart;
      while not aReader.IsMapEnd do
      begin
        lKey := aReader.ReadMapKey;
        lKeywordDefinition := CreateDefinition;
        fValues.SetItem(lKey,lKeywordDefinition);
        lKeywordDefinition.Deserialize(aReader);
      end;
      aReader.ReadMapEnd;
    end
    else if aReader.IsListStart then
    begin
      aReader.ReadListStart;
      // basically, two loops. The first one gives us a chance to read items
      // that are managed, up until the first item that isn't manageable.
      while not aReader.IsListEnd do
      begin
        lKey := aReader.ReadString.Value;
        lKeywordDefinition := CreateDefinition;
        fValues.SetItem(lKey,lKeywordDefinition);
      end;
      aReader.ReadListEnd;

    end;

  end
end;

procedure TKeywordDefinitions.Serialize(aWriter: TDynamicValueWriter);
var
  lEnum: IDynamicMapEnumerator;
begin
  aWriter.WriteMapStart(fValues);
  lEnum := fValues.Enumerate;
  while lEnum.Next do
  begin
    aWriter.WriteKey(lEnum.Key);
    (lEnum.Value as IDataStoreObject).Serialize(aWriter);
  end;
  aWriter.WriteMapEnd;
end;

{ TCategoryDefinition }

function TCategoryDefinition.GetItem(const aKey: UTF8String): IDynamicValue;
begin
  case aKey of
    PublishMarkerAfterKey:
      result := fPublishMarkerAfter;
    PublishMarkerBeforeKey:
      result := fPublishMarkerBefore;
    PublishMarkerBetweenKey:
      result := fPublishMarkerBetween;
    PublishTitleKey:
      result := fPublishTitle;
    PublishTitleLevelKey:
      result := fPublishTitleLevel;
    PublishTitlePrefixKey:
      result := fPublishTitlePrefix;
  else;
    Result:=inherited GetItem(aKey);
  end;
end;

procedure TCategoryDefinition.InitializeBlank;
begin
  fPublishMarkerAfter:= false;
  fPublishMarkerBefore:= false;
  fPublishMarkerBetween:= false;
  fPublishTitle:= false;
  fPublishTitleLevel:= 0;
  fPublishTitlePrefix:= '';
  inherited InitializeBlank;
end;

procedure TCategoryDefinition.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);
begin
  case aKey of
    PublishMarkerAfterKey:
      if AValue is IDynamicBoolean then
         fPublishMarkerAfter:= (AValue as IDynamicBoolean).Value
      else
         RaiseInvalidKeyValue(PublishMarkerAfterKey,AValue);
    PublishMarkerBeforeKey:
      if AValue is IDynamicBoolean then
         fPublishMarkerBefore:= (AValue as IDynamicBoolean).Value
      else
         RaiseInvalidKeyValue(PublishMarkerBeforeKey,AValue);
    PublishMarkerBetweenKey:
      if AValue is IDynamicBoolean then
         fPublishMarkerBetween:= (AValue as IDynamicBoolean).Value
      else
         RaiseInvalidKeyValue(PublishMarkerBetweenKey,AValue);
    PublishTitleKey:
      if AValue is IDynamicBoolean then
         fPublishTitle:= (AValue as IDynamicBoolean).Value
      else
         RaiseInvalidKeyValue(PublishTitleKey,AValue);
    PublishTitleLevelKey:
      if AValue is IDynamicNumber then
         fPublishTitleLevel:= trunc((AValue as IDynamicNumber).Value)
      else
         RaiseInvalidKeyValue(PublishTitleLevelKey,AValue);
    PublishTitlePrefixKey:
      if AValue is IDynamicString then
         fPublishTitlePrefix:= (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(PublishTitlePrefixKey,AValue);
  else;
    inherited SetItem(aKey,AValue);
  end;
end;

function TCategoryDefinition.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    PublishMarkerAfterKey,
    PublishMarkerBeforeKey,
    PublishMarkerBetweenKey,
    PublishTitleKey,
    PublishTitleLevelKey,
    PublishTitlePrefixKey:
    begin
      SetItem(aKey,aReader.ReadValue);
      result := true;
    end
  else;
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;
end;

procedure TCategoryDefinition.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKeyValue(PublishMarkerAfterKey,fPublishMarkerAfter);
  aWriter.WriteKeyValue(PublishMarkerBeforeKey,fPublishMarkerBefore);
  aWriter.WriteKeyValue(PublishMarkerBetweenKey,fPublishMarkerBetween);
  aWriter.WriteKeyValue(PublishTitleKey,fPublishTitle);
  aWriter.WriteKeyValue(PublishTitleLevelKey,fPublishTitleLevel);
  aWriter.WriteKeyValue(PublishTitlePrefixKey,fPublishTitlePrefix);
  inherited WriteManagedKeys(aWriter);
end;

procedure TCategoryDefinition.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(PublishMarkerAfterKey);
  aValue.Add(PublishMarkerBeforeKey);
  aValue.Add(PublishMarkerBetweenKey);
  aValue.Add(PublishTitleKey);
  aValue.Add(PublishTitleLevelKey);
  aValue.Add(PublishTitlePrefixKey);
  inherited ListManagedKeys(aValue);
end;

function TCategoryDefinition.GetPublishMarkerAfter: Boolean;
begin
  result := fPublishMarkerAfter;
end;

function TCategoryDefinition.GetPublishMarkerBefore: Boolean;
begin
  result := fPublishMarkerBefore;
end;

function TCategoryDefinition.GetPublishMarkerBetween: Boolean;
begin
  result := fPublishMarkerBetween;
end;

function TCategoryDefinition.GetPublishTitle: Boolean;
begin
  result := fPublishTitle;
end;

function TCategoryDefinition.GetPublishTitleLevel: Integer;
begin
  result := fPublishTitleLevel;
end;

function TCategoryDefinition.GetPublishTitlePrefix: UTF8String;
begin
  result := fPublishTitlePrefix;
end;

procedure TCategoryDefinition.SetPublishMarkerAfter(AValue: Boolean);
begin
  fPublishMarkerAfter:= AValue;
end;

procedure TCategoryDefinition.SetPublishMarkerBefore(AValue: Boolean);
begin
  fPublishMarkerBefore:=AValue;
end;

procedure TCategoryDefinition.SetPublishMarkerBetween(AValue: Boolean);
begin
  fPublishMarkerBetween:=AValue;
end;

procedure TCategoryDefinition.SetPublishTitle(AValue: Boolean);
begin
  fPublishTitle:=AValue;
end;

procedure TCategoryDefinition.SetPublishTitleLevel(AValue: Integer);
begin
  fPublishTitleLevel:=AValue;
end;

procedure TCategoryDefinition.SetPublishTitlePrefix(AValue: UTF8String);
begin
  fPublishTitlePrefix:=AValue;
end;

{ TKeywordDefinition }

function TKeywordDefinition.GetItem(const aKey: UTF8String): IDynamicValue;
var
  r: Byte;
  g: Byte;
  b: Byte;
begin
  if aKey = ColorKey then
  begin
     r := Graphics.Red(fColor);
     g := Graphics.Green(fColor);
     b := Graphics.Blue(fColor);
     result := TDynamicValues.NewMap;
     (result as IDynamicMap)[RedKey] := r;
     (result as IDynamicMap)[GreenKey] := g;
     (result as IDynamicMap)[BlueKey] := b;
  end
  else
     Result:=inherited GetItem(aKey);
end;

procedure TKeywordDefinition.InitializeBlank;
begin
 fColor := clDefault;
 inherited InitializeBlank;
end;

procedure TKeywordDefinition.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);

  function GetColorPart(aPartKey: UTF8String): Byte;
  var
    lItem: IDynamicValue;
  begin
     lItem := (AValue as IDynamicMap)[aPartKey];
     if lItem is IDynamicNumber then
     begin
        result := trunc((lItem as IDynamicNumber).Value);
     end
     else
     begin
       RaiseInvalidKeyValue(aKey + '[' + aPartKey + ']',lItem);
     end;

  end;

var
  r: Byte;
  g: Byte;
  b: Byte;
begin
  if aKey = ColorKey then
  begin
     if AValue is IDynamicMap then
     begin
        r := GetColorPart(RedKey);
        g := GetColorPart(GreenKey);
        b := GetColorPart(BlueKey);
        fColor := RGBToColor(r,g,b)
     end
     else
        RaiseInvalidKeyValue(ColorKey,AValue);
  end
  else
    inherited SetItem(aKey, AValue);
end;

function TKeywordDefinition.GetColor: TColor;
begin
 result := fColor;
end;

procedure TKeywordDefinition.SetColor(AValue: TColor);
begin
 fColor := AValue;

end;

function TKeywordDefinition.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
 if aKey = ColorKey then
 begin
    SetItem(ColorKey,aReader.ReadValue);
    result := true;
 end
 else
    Result:=inherited ReadManagedKey(aKey, aReader);
end;

procedure TKeywordDefinition.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKeyValue(ColorKey,GetItem(ColorKey));
  inherited WriteManagedKeys(aWriter);
end;

procedure TKeywordDefinition.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(ColorKey);
  inherited ListManagedKeys(aValue);
end;

{ TDocumentProperties }

function TDocumentProperties.GetItem(const aKey: UTF8String): IDynamicValue;
var
  i: Longint;
  l: Longint;
begin
  case aKey of
    CategoryKey:
      result := fCategory;
    IndexKey:
    begin
      result := TDynamicValues.NewList;
      l := fIndex.Count;
      for i := 0 to l - 1 do
         (result as IDynamicList).Add(fIndex[i]);
    end;
    PublishKey:
      result := fPublish;
    StatusKey:
      result := fStatus;
    TitleKey:
      result := fTitle;
  else
    Result:=inherited GetItem(aKey);
  end;
end;

procedure TDocumentProperties.InitializeBlank;
begin
  fCategory := '';
  fIndex.Count := 0;
  fPublish := false;
  fStatus := '';
  fTitle := '';
 inherited InitializeBlank;
end;

procedure TDocumentProperties.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);
var
  i: Longint;
  l: Longint;
  lItem: IDynamicValue;
  lNewIndex: TStringArray2;
begin
  case aKey of
    CategoryKey:
      if AValue is IDynamicString then
         fCategory := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(CategoryKey,AValue);
    IndexKey:
    begin
      if AValue is IDynamicList then
      begin
         l := (AValue as IDynamicList).Length;
         {%H-}lNewIndex.Count := 0;
         for i := 0 to l - 1 do
         begin
           lItem := (AValue as IDynamicList)[i];
           if lItem is IDynamicString then
           begin
              lNewIndex.Add((lItem as IDynamicString).Value);
           end
           else
           begin
              RaiseInvalidKeyValue(IndexKey + '[' + IntToStr(i) + ']',lItem);
           end;
         end;
         fIndex := lNewIndex;
      end
      else
        RaiseInvalidKeyValue(IndexKey,AValue);
    end;
    PublishKey:
      if AValue is IDynamicBoolean then
         fPublish := (AValue as IDynamicBoolean).Value
      else
         RaiseInvalidKeyValue(PublishKey,AValue);
    StatusKey:
      if AValue is IDynamicString then
         fStatus := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(StatusKey,AValue);
    TitleKey:
      if AValue is IDynamicString then
         fTitle := (AValue as IDynamicString).Value
      else
         RaiseInvalidKeyValue(TitleKey,AValue);
  else
    inherited SetItem(aKey,aValue);
  end;
end;

function TDocumentProperties.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    CategoryKey,
    IndexKey,
    PublishKey,
    StatusKey,
    TitleKey:
    begin
      SetItem(aKey,aReader.ReadValue);
      result := true;
    end
  else
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;
end;

procedure TDocumentProperties.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
 aWriter.WriteKeyValue(TitleKey,fTitle);
 aWriter.WriteKeyValue(CategoryKey,fCategory);
 aWriter.WriteKeyValue(StatusKey,fStatus);
 aWriter.WriteKeyValue(PublishKey,fPublish);
 aWriter.WriteKeyValue(IndexKey,GetItem(IndexKey));
 inherited WriteManagedKeys(aWriter);
end;

procedure TDocumentProperties.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(TitleKey);
  aValue.Add(CategoryKey);
  aValue.Add(StatusKey);
  aValue.Add(IndexKey);
  inherited ListManagedKeys(aValue);
end;

function TDocumentProperties.GetCategory: UTF8String;
begin
  result := fCategory;
end;

function TDocumentProperties.GetIndex: TStringArray2;
begin
  result := fIndex;
end;

function TDocumentProperties.GetPublish: Boolean;
begin
  result := fPublish;
end;

function TDocumentProperties.GetStatus: UTF8String;
begin
  result := fStatus;
end;

function TDocumentProperties.GetTitle: UTF8String;
begin
  result := fTitle;
end;

procedure TDocumentProperties.SetCategory(AValue: UTF8String);
begin
  fCategory := AValue;
end;

procedure TDocumentProperties.SetIndex(AValue: TStringArray2);
begin
  fIndex := AValue;
end;

procedure TDocumentProperties.SetPublish(AValue: Boolean);
begin
  fPublish := AValue;
end;

procedure TDocumentProperties.SetStatus(AValue: UTF8String);
begin
  fStatus := AValue;
end;

procedure TDocumentProperties.SetTitle(AValue: UTF8String);
begin
  fTitle := AValue;
end;

destructor TDocumentProperties.Destroy;
begin
  fIndex.Count := 0;
  inherited Destroy;
end;


{ TProperties }

function TProperties.GetItem(const aKey: UTF8String): IDynamicValue;
begin
  if aKey = UserKey then
     result := GetUser
  else
     result := inherited GetItem(aKey);

end;

procedure TProperties.InitializeBlank;
begin
  fUser := TDynamicValues.Undefined;
  inherited InitializeBlank;
end;

procedure TProperties.SetItem(const aKey: UTF8String;
  const AValue: IDynamicValue);
begin
  if aKey = UserKey then
     SetUser(AValue);
end;

function TProperties.GetUser: IDynamicValue;
begin
  result := fUser;
end;

procedure TProperties.SetUser(AValue: IDynamicValue);
begin
  fUser := AValue;

end;

function TProperties.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  if aKey = UserKey then
  begin
     fUser := aReader.ReadValue;
     result := true;
  end
  else
     Result:=inherited ReadManagedKey(aKey, aReader);
end;

procedure TProperties.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKeyValue(UserKey,fUser);
  inherited WriteManagedKeys(aWriter);
end;

procedure TProperties.ListManagedKeys(var aValue: TStringArray2);
begin
  aValue.Add(UserKey);
  inherited ListManagedKeys(aValue);
end;

destructor TProperties.Destroy;
begin
  fUser := nil;
  inherited Destroy;
end;

end.

