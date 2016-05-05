unit stew_properties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, contnrs, sys_file, sys_json, sys_dynval, sys_dynval_data, sys_types;

type


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

  { TProperties2 }

  TProperties2 = class(TDataStoreMap)
  private
    fUser: IDynamicValue;
  protected
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
  public
    property User: IDynamicValue read fUser write fUser;
  end;

  { TDocumentIndexProperty }

  TDocumentIndexProperty = class(TJSArray)
  strict protected
    function RequestType(aKey: UTF8String; aType: TJSValueClass
       ): TJSValueClass; override;
  end;

  { TDocumentIndexProperty2 }

  TDocumentIndexProperty2 = class(TDataStoreList)
  private
    fIndex: TStringArray2;
  protected
    function ReadManagedItem(aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedItems(aWriter: TDynamicValueWriter); override;
    procedure InitializeBlank; override;
  public
    property Index: TStringArray2 read fIndex;
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

  { TDocumentProperties2 }

  TDocumentProperties2 = class(TProperties2)
  private
    fTitle: UTF8String;
    fPublish: Boolean;
    fCategory: UTF8String;
    fStatus: UTF8String;
    fIndex: TDocumentIndexProperty2;
    function GetIndex: TStringArray2;
  protected
    function ReadManagedKey(const aKey: UTF8String; aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure InitializeBlank; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Title: UTF8String read fTitle write fTitle;
    property Publish: Boolean read fPublish write fPublish;
    property Category: UTF8String read fCategory write fCategory;
    property Status: UTF8String read fStatus write fStatus;
    property Index: TStringArray2 read GetIndex;
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

  { TKeywordDefinition2 }

  TKeywordDefinition2 = class(TDataStoreMap)
  private
    fColor: TColor;
  protected
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure InitializeBlank; override;
  public
    property Color: TColor read fColor write fColor;
  end;

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

  { TCategoryDefinition2 }

  TCategoryDefinition2 = class(TKeywordDefinition2)
  private
    fPublishMarkerAfter: Boolean;
    fPublishMarkerBefore: Boolean;
    fPublishMarkerBetween: Boolean;
    fPublishTitle: Boolean;
    fPublishTitleLevel: Integer;
    fPublishTitlePrefix: UTF8String;
  protected
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure InitializeBlank; override;
  public
    property PublishTitle: Boolean read fPublishTitle write fPublishTitle;
    property PublishTitleLevel: Integer read fPublishTitleLevel write fPublishTitleLevel;
    property PublishTitlePrefix: UTF8String read fPublishTitlePrefix write fPublishTitlePrefix;
    property PublishMarkerBefore: Boolean read fPublishMarkerBefore write fPublishMarkerBefore;
    property PublishMarkerAfter: Boolean read fPublishMarkerAfter write fPublishMarkerAfter;
    property PublishMarkerBetween: Boolean read fPublishMarkerBetween write fPublishMarkerBetween;

  end;

  TStatusDefinition = class(TKeywordDefinition)
    // just holds color, so nothing special...
  end;

  { TStatusDefinition2 }

  TStatusDefinition2 = class(TKeywordDefinition2)
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

  { GKeywordDefinitions2 }

  generic GKeywordDefinitions2<Membertype> = class(TDataStoreMap)
  private
    fList: TFPHashObjectList;
    function GetItem(const aName: UTF8String): MemberType;
    function GetKeys: TStringArray2;
  protected
    function ReadManagedKey(const {%H-}aKey: UTF8String;
       {%H-}aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure InitializeBlank; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Item[const aName: UTF8String]: MemberType read GetItem; default;
    property Keys: TStringArray2 read GetKeys;
    procedure Delete(const aName: UTF8String);
    function Has(const aName: UTF8String): Boolean;
    function Add(const aName: UTF8String): MemberType;

  end;

  { TStatusDefinitions }

  TStatusDefinitions = class(specialize TKeywordDefinitions<TStatusDefinition>)
  public
    function GetStatus(aKey: UTF8String): TStatusDefinition;
  end;

  { TStatusDefinitions2 }

  TStatusDefinitions2 = class(specialize GKeywordDefinitions2<TStatusDefinition2>)
  public
    procedure Deserialize(aReader: TDynamicValueReader); override;
  end;

  { TCategoryDefinitions }

  TCategoryDefinitions = class(specialize TKeywordDefinitions<TCategoryDefinition>)
  public
    function GetCategory(aKey: UTF8String): TCategoryDefinition;
  end;

  TCategoryDefinitions2 = class(specialize GKeywordDefinitions2<TCategoryDefinition2>)
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

  { TDeadLine2 }

  TDeadLine2 = class(TDataStoreMap)
  private
    fDue: TDateTime;
    fName: UTF8String;
  protected
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure InitializeBlank; override;
  public
    property Due: TDateTime read fDue write fDue;
    property Name: UTF8String read fName write fName;
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

  { TDeadlines2 }

  TDeadlines2 = class(TDataStoreList)
  private
    fList: TFPObjectList;
    function GetCount: Longint;
    function GetDeadline(const aIndex: Longint): TDeadLine2;
  protected
    function ReadManagedItem(aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedItems(aWriter: TDynamicValueWriter); override;
    procedure InitializeBlank; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Deserialize(aReader: TDynamicValueReader); override;
    property Count: Longint read GetCount;
    property Deadline[const aIndex: Longint]: TDeadLine2 read GetDeadline; default;
    function Add: TDeadline2;
    function Add(const aName: UTF8String; const aDue: TDateTime): TDeadline2;
    procedure Delete(aIndex: Longint);
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

  { TProjectProperties2 }

  TProjectProperties2 = class(TProperties2)
  private
    fCategories: TCategoryDefinitions2;
    fDeadlines: TDeadlines2;
    fDefaultCategory: UTF8String;
    fDefaultDocExtension: UTF8String;
    fDefaultNotesExtension: UTF8String;
    fDefaultStatus: UTF8String;
    fDefaultThumbnailExtension: UTF8String;
    fStatuses: TStatusDefinitions2;
    procedure SetDefaultDocExtension(AValue: UTF8String);
    procedure SetDefaultNotesExtension(AValue: UTF8String);
    procedure SetDefaultThumbnailExtension(AValue: UTF8String);
  protected
    function ReadManagedKey(const aKey: UTF8String;
       aReader: TDynamicValueReader): Boolean; override;
    procedure WriteManagedKeys(aWriter: TDynamicValueWriter); override;
    procedure InitializeBlank; override;
  public
    constructor Create;
    destructor Destroy; override;
    property DefaultDocExtension: UTF8String read fDefaultDocExtension write SetDefaultDocExtension;
    property DefaultThumbnailExtension: UTF8String read fDefaultThumbnailExtension write SetDefaultThumbnailExtension;
    property DefaultNotesExtension: UTF8String read fDefaultNotesExtension write SetDefaultNotesExtension;
    property Categories: TCategoryDefinitions2 read fCategories;
    property DefaultCategory: UTF8String read fDefaultCategory write fDefaultCategory;
    property Statuses: TStatusDefinitions2 read fStatuses;
    property DefaultStatus: UTF8String read fDefaultStatus write fDefaultStatus;
    property Deadlines: TDeadlines2 read fDeadlines;
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
  LCLProc;

{ TStatusDefinitions2 }

procedure TStatusDefinitions2.Deserialize(aReader: TDynamicValueReader);
begin
  // a special case for statuses, you can store an array of strings
  // in certain older versions of stew...
  if aReader.IsListStart then
  begin
    aReader.ReadListStart;
    while not aReader.IsListEnd do
    begin
      Add(aReader.ReadString.Value);
    end;
    aReader.ReadListEnd;
  end
  else
    inherited Deserialize(aReader);
end;

{ TProjectProperties2 }

procedure TProjectProperties2.SetDefaultDocExtension(AValue: UTF8String);
begin
  if fDefaultDocExtension=AValue then Exit;
  fDefaultDocExtension:=ExcludeExtensionDelimiter(aValue)
end;

procedure TProjectProperties2.SetDefaultNotesExtension(AValue: UTF8String);
begin
  if fDefaultNotesExtension=AValue then Exit;
  fDefaultNotesExtension:=ExcludeExtensionDelimiter(aValue)
end;

procedure TProjectProperties2.SetDefaultThumbnailExtension(AValue: UTF8String);
begin
  if fDefaultThumbnailExtension=AValue then Exit;
  fDefaultThumbnailExtension:=ExcludeExtensionDelimiter(aValue)
end;

function TProjectProperties2.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    DefaultDocExtensionKey:
      DefaultDocExtension := aReader.ReadString.Value;
    DefaultThumbnailExtensionKey:
      DefaultThumbnailExtension := aReader.ReadString.Value;
    DefaultNotesExtensionKey:
      DefaultNotesExtension := aReader.ReadString.Value;
    DefaultCategoryKey:
      fDefaultCategory := aReader.ReadString.Value;
    DefaultStatusKey:
      fDefaultStatus := aReader.ReadString.Value;
    CategoriesKey:
      fCategories.Deserialize(aReader);
    StatusesKey:
      fStatuses.Deserialize(aReader);
    DeadlinesKey:
      fDeadlines.Deserialize(aReader);
  else
     Result:=inherited ReadManagedKey(aKey, aReader);

  end;
end;

procedure TProjectProperties2.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  if fDefaultDocExtension > '' then
     aWriter.WriteKeyValue(DefaultDocExtensionKey,fDefaultDocExtension);
  if fDefaultThumbnailExtension > '' then
     aWriter.WriteKeyValue(DefaultThumbnailExtensionKey,fDefaultThumbnailExtension);
  if fDefaultNotesExtension > '' then
     aWriter.WriteKeyValue(DefaultNotesExtensionKey,fDefaultNotesExtension);
  if fDefaultCategory > '' then
     aWriter.WriteKeyValue(DefaultCategoryKey,fDefaultCategory);
  if fDefaultStatus > '' then
     aWriter.WriteKeyValue(DefaultStatusKey,fDefaultStatus);
  if fCategories <> nil then
  begin
    aWriter.WriteKey(CategoriesKey);
    fCategories.Serialize(aWriter);
  end;
  if fStatuses <> nil then
  begin
    aWriter.WriteKey(StatusesKey);
    fStatuses.Serialize(aWriter);
  end;
  if fDeadlines <> nil then
  begin
    aWriter.WriteKey(DeadlinesKey);
    fDeadlines.Serialize(aWriter);
  end;
  inherited WriteManagedKeys(aWriter);
end;

procedure TProjectProperties2.InitializeBlank;
begin
  fCategories.InitializeBlank;
  fDeadlines.InitializeBlank;
  fStatuses.InitializeBlank;
  fDefaultCategory := '';
  fDefaultDocExtension:= '';
  fDefaultNotesExtension:='';
  fDefaultStatus:= '';
  fDefaultThumbnailExtension:='';
end;

constructor TProjectProperties2.Create;
begin
  fStatuses := TStatusDefinitions2.Create;
  fCategories := TCategoryDefinitions2.Create;
  fDeadlines := TDeadlines2.Create;
  inherited Create;
end;

destructor TProjectProperties2.Destroy;
begin
  FreeAndNil(fStatuses);
  FreeAndNil(fCategories);
  FreeAndNil(fDeadlines);
  inherited Destroy;
end;

{ TDeadlines2 }

function TDeadlines2.GetCount: Longint;
begin
  result := fList.Count;
end;

function TDeadlines2.GetDeadline(const aIndex: Longint): TDeadLine2;
begin
  result := TDeadLine2(fList[aIndex]);
end;

function TDeadlines2.ReadManagedItem(aReader: TDynamicValueReader): Boolean;
var
  lItem: TDeadLine2;
begin
  result := aReader.IsMapStart;
  if result then
  begin
    lItem := TDeadLine2.Create;
     fList.Add(lItem);
     lItem.Deserialize(aReader);
  end;
end;

procedure TDeadlines2.WriteManagedItems(aWriter: TDynamicValueWriter);
var
  l: Longint;
  i: Longint;
begin
  l := fList.Count - 1;
  for i := 0 to l do
     TDeadLine2(fList[i]).Serialize(aWriter);
  inherited WriteManagedItems(aWriter);
end;

procedure TDeadlines2.InitializeBlank;
begin
  fList.Clear;

end;

constructor TDeadlines2.Create;
begin
  fList := TFPObjectList.Create(true);
  inherited Create;

end;

destructor TDeadlines2.Destroy;
begin
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure TDeadlines2.Deserialize(aReader: TDynamicValueReader);
begin
  fList.Clear;
  inherited Deserialize(aReader);
end;

function TDeadlines2.Add: TDeadline2;
begin
  result := TDeadLine2.Create;
  fList.Add(result);
end;

function TDeadlines2.Add(const aName: UTF8String; const aDue: TDateTime
  ): TDeadline2;
begin
  result := TDeadLine2.Create;
  fList.Add(Result);
  result.Name := aName;
  result.Due := aDue;

end;

procedure TDeadlines2.Delete(aIndex: Longint);
begin
  fList.Delete(aIndex);

end;

{ TDeadLine2 }

function TDeadLine2.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    DueKey:
      fDue := ISO8601ToDateTime(aReader.ReadString.Value);
    NameKey:
      fName := aReader.ReadString.Value;
  else
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;
end;

procedure TDeadLine2.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  if fName <> '' then
     aWriter.WriteKeyValue(NameKey,fName);
  if fDue <> 0 then
     aWriter.WriteKeyValue(DueKey,DateTimeToISO8601(fDue,false));
  inherited WriteManagedKeys(aWriter);
end;

procedure TDeadLine2.InitializeBlank;
begin
  fDue := 0;
  fName := '';
end;

{ GKeywordDefinitions2 }

function GKeywordDefinitions2.GetItem(const aName: UTF8String): MemberType;
begin
  result := MemberType(fList.Find(aName));
end;

function GKeywordDefinitions2.GetKeys: TStringArray2;
var
  i: Longint;
  l: Longint;
begin
  l := fList.Count;
  result{%H-}.Count := l;
  for i := 0 to l - 1 do
     result[i] := fList.NameOfIndex(i);
end;

function GKeywordDefinitions2.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  Add(aKey).Deserialize(aReader);
  result := true;
end;

procedure GKeywordDefinitions2.WriteManagedKeys(aWriter: TDynamicValueWriter);
var
  i: Longint;
  l: Longint;
begin
  l := fList.Count - 1;
  for i := 0 to l do
  begin
     aWriter.WriteKey(fList.NameOfIndex(i));
     MemberType(fList[i]).Serialize(aWriter);
  end;
end;

procedure GKeywordDefinitions2.InitializeBlank;
begin
  fList.Clear;
end;

constructor GKeywordDefinitions2.Create;
begin
  fList := TFPHashObjectList.Create(true);
  inherited Create;

end;

destructor GKeywordDefinitions2.Destroy;
begin
  FreeAndNil(fList);
  inherited Destroy;
end;

procedure GKeywordDefinitions2.Delete(const aName: UTF8String);
var
  i: Longint;
begin
  i := fList.FindIndexOf(aName);
  if i > -1 then
     fList.Delete(i);

end;

function GKeywordDefinitions2.Has(const aName: UTF8String): Boolean;
begin
  result := (fList.FindIndexOf(aName) > -1);
end;

function GKeywordDefinitions2.Add(const aName: UTF8String): MemberType;
begin
  result := MemberType.Create;
  fList.Add(aName,Result);
end;

{ TCategoryDefinition2 }

function TCategoryDefinition2.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  case aKey of
    PublishTitleKey:
       fPublishTitle:=aReader.ReadBoolean.Value;
    PublishTitleLevelKey:
       fPublishTitleLevel:=trunc(aReader.ReadNumber.Value);
    PublishTitlePrefixKey:
       fPublishTitlePrefix:=aReader.ReadString.Value;
    PublishMarkerBeforeKey:
       fPublishMarkerBefore:=aReader.ReadBoolean.Value;
    PublishMarkerAfterKey:
       fPublishMarkerAfter := aReader.ReadBoolean.Value;
    PublishMarkerBetweenKey:
       fPublishMarkerBetween:=aReader.ReadBoolean.Value;
  else
    Result:=inherited ReadManagedKey(aKey, aReader);
  end;
end;

procedure TCategoryDefinition2.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  if fPublishTitle then
     aWriter.WriteKeyValue(PublishTitleKey,fPublishTitle);
  if fPublishTitleLevel <> 0 then
     aWriter.WriteKeyValue(PublishTitleLevelKey,fPublishTitleLevel);
  if fPublishTitlePrefix <> '' then
     aWriter.WriteKeyValue(PublishTitlePrefixKey,fPublishTitlePrefix);
  if fPublishMarkerBefore then
     aWriter.WriteKeyValue(PublishMarkerBeforeKey,fPublishMarkerBefore);
  if fPublishMarkerAfter then
     aWriter.WriteKeyValue(PublishMarkerAfterKey,fPublishMarkerAfter);
  if fPublishMarkerBetween then
     aWriter.WriteKeyValue(PublishMarkerBetweenKey,fPublishMarkerBetween);
  inherited WriteManagedKeys(aWriter);
end;

procedure TCategoryDefinition2.InitializeBlank;
begin
  inherited InitializeBlank;
  fPublishMarkerAfter := false;
  fPublishMarkerBefore:= false;
  fPublishMarkerBetween:= False;
  fPublishTitle:= false;
  fPublishTitleLevel:=0;
  fPublishTitlePrefix:='';
end;

{ TDocumentIndexProperty2 }

function TDocumentIndexProperty2.ReadManagedItem(aReader: TDynamicValueReader
  ): Boolean;
begin
  result := aReader.IsString;
  if result then
  begin
     fIndex.Add(aReader.ReadString.Value)
  end
end;

procedure TDocumentIndexProperty2.WriteManagedItems(aWriter: TDynamicValueWriter
  );
var
  l: Longint;
  i: Longint;
begin
  l := fIndex.Count - 1;
  for i := 0 to l do
     aWriter.WriteValue(fIndex[i]);
end;

procedure TDocumentIndexProperty2.InitializeBlank;
begin
  fIndex.Count := 0;
end;

{ TKeywordDefinition2 }

function TKeywordDefinition2.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
var
  lKey: UTF8String;
  b: Byte;
  r: Byte;
  g: Byte;
begin
  if aKey = ColorKey then
  begin
    aReader.ReadMapStart;
    b := Graphics.Blue(clDefault);
    r := Graphics.Red(clDefault);
    g := Graphics.Green(clDefault);
    while not aReader.IsMapEnd do
    begin
      lKey := aReader.ReadMapKey;
      case lKey of
        BlueKey:
           b := trunc(aReader.ReadNumber.Value);
        GreenKey:
           g := trunc(aReader.ReadNumber.Value);
        RedKey:
           r := trunc(aReader.ReadNumber.Value);
      else
        // this one we ignore everything else.
        aReader.ReadValue;
      end;
    end;
    aReader.ReadMapEnd;
    fColor := RGBToColor(r,g,b);
    result := true;
  end
  else
     Result:=inherited ReadManagedKey(aKey, aReader);
end;

procedure TKeywordDefinition2.WriteManagedKeys(aWriter: TDynamicValueWriter);
var
  r: Byte;
  g: Byte;
  b: Byte;
begin
  inherited WriteManagedKeys(aWriter);
  r := Graphics.Red(fColor);
  g := Graphics.Green(fColor);
  b := Graphics.Blue(fColor);
  aWriter.WriteKey(ColorKey);
  aWriter.WriteMapStart(nil);
  aWriter.WriteKeyValue(RedKey,r);
  aWriter.WriteKeyValue(BlueKey,b);
  aWriter.WriteKeyValue(GreenKey,g);
  aWriter.WriteMapEnd;
end;

procedure TKeywordDefinition2.InitializeBlank;
begin
  fColor := clDefault;
end;

{ TDocumentProperties2 }

function TDocumentProperties2.GetIndex: TStringArray2;
begin
  result := fIndex.Index;
end;

function TDocumentProperties2.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  result := true;
  case aKey of
    TitleKey: fTitle := aReader.ReadString.Value;
    PublishKey: fPublish := aReader.ReadBoolean.Value;
    CategoryKey: fCategory := aReader.ReadString.Value;
    StatusKey: fStatus := aReader.ReadString.Value;
    IndexKey:
    begin
      fIndex.Deserialize(aReader);
    end
  else
    result := inherited ReadManagedKey(aKey,aReader);
  end;
end;

procedure TDocumentProperties2.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  inherited WriteManagedKeys(aWriter);
  if fTitle <> '' then
     aWriter.WriteKeyValue(TitleKey,fTitle);
  if fPublish then
     aWriter.WriteKeyValue(PublishKey,fPublish);
  if fCategory <> '' then
     aWriter.WriteKeyValue(CategoryKey,fCategory);
  if fStatus <> '' then
     aWriter.WriteKeyValue(StatusKey,fStatus);
  if fIndex <> nil then
  begin
    aWriter.WriteKey(IndexKey);
    fIndex.Serialize(aWriter);
  end;

end;

procedure TDocumentProperties2.InitializeBlank;
begin
  fTitle := '';
  fPublish := false;
  fCategory := '';
  fStatus := '';
  fIndex.InitializeBlank;
end;

constructor TDocumentProperties2.Create;
begin
  fIndex := TDocumentIndexProperty2.Create;
  inherited Create;
end;

destructor TDocumentProperties2.Destroy;
begin
  FreeAndNil(fIndex);
  inherited Destroy;
end;

{ TProperties2 }

function TProperties2.ReadManagedKey(const aKey: UTF8String;
  aReader: TDynamicValueReader): Boolean;
begin
  if aKey = UserKey then
     fUser := aReader.ReadValue
  else
     Result:=inherited ReadManagedKey(aKey, aReader);
end;

procedure TProperties2.WriteManagedKeys(aWriter: TDynamicValueWriter);
begin
  aWriter.WriteKeyValue(UserKey,fUser);
  inherited WriteManagedKeys(aWriter);
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

