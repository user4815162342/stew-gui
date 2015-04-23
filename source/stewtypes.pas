unit stewtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TExceptionEvent = procedure(Sender: TObject; aError: Exception) of object;
  TExceptionMessageEvent = procedure(Sender: TObject; aError: String) of object;

  TMappedCollection = class;

  { TMappedCollectionItem }

  TMappedCollectionItem = class(TPersistent)
  private
    fMap: TMappedCollection;
    function GetKey: String;
  protected
    procedure SetMap(Value: TMappedCollection; const Key: String); virtual;
    procedure Changed(AllItems: Boolean);
    function GetOwner: TPersistent; override;
  public
    constructor Create(AMap: TMappedCollection; const Key: String); virtual;
    destructor Destroy; override;
    property Map: TMappedCollection read fMap;
    property Key: String read GetKey;
  end;

  TMappedCollectionItemClass = class of TMappedCollectionItem;

  { TMappedCollection }

  TMappedCollection = class(TPersistent)
  private
    fItemClass: TMappedCollectionItemClass;
    fHash : TFPHashObjectList; // Careful : Names limited to 255 chars.
    fUpdateCount: Integer;
    function GetNameCount: Integer;
    procedure InsertItem(const Key: String; Item: TMappedCollectionItem);
    procedure RemoveItem(Item: TMappedCollectionItem);
    procedure DoClear;
  protected
    { Design-time editor support }
    procedure Changed;
    function GetItem(const Key: String): TMappedCollectionItem;
    procedure SetItem(const Key: String; Value: TMappedCollectionItem);
    function GetName(const Index: Integer): String;
    procedure Update(Item: TMappedCollectionItem); virtual;
    procedure Notify(Item: TMappedCollectionItem;Action: TCollectionNotification); virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    constructor Create(AItemClass: TMappedCollectionItemClass);
    destructor Destroy; override;
    function Add(const Key: String): TMappedCollectionItem;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure Clear;
    procedure EndUpdate; virtual;
    procedure Delete(const Key: String);
    function Insert(const Key: String): TMappedCollectionItem;
    function Find(const Key: String): TMappedCollectionItem;
    property ItemClass: TMappedCollectionItemClass read FItemClass;
    property Items[const Key: String]: TMappedCollectionItem read GetItem write SetItem; default;
    property NameCount: Integer read GetNameCount;
    property Names[const Index: Integer]: String read GetName;
  end;


implementation

{ TMappedCollectionItem }

function TMappedCollectionItem.GetKey: String;
begin
  if fMap<>nil then
    Result:=fMap.fHash.NameOfIndex(fMap.fHash.IndexOf(Self))
  else
    Result:='';
end;

procedure TMappedCollectionItem.SetMap(Value: TMappedCollection; const Key: String);
begin
  IF Value<>fMap then
    begin
    If fMap<>Nil then fMap.RemoveItem(Self);
    if Value<>Nil then Value.InsertItem(Key,Self);
    fMap:=Value;
    end;

end;

procedure TMappedCollectionItem.Changed(AllItems: Boolean);
begin
  If (fMap<>Nil) and (fMap.UpdateCount=0) then
   begin
   If AllItems then
     fMap.Update(Nil)
   else
     fMap.Update(Self);
   end;

end;

function TMappedCollectionItem.GetOwner: TPersistent;
begin
  Result:=fMap;
end;

constructor TMappedCollectionItem.Create(AMap: TMappedCollection; const Key: String);
begin
  inherited Create;
  SetMap(aMap,Key);
end;

destructor TMappedCollectionItem.Destroy;
begin
  SetMap(nil,'');
  inherited Destroy;
end;

{ TMappedCollection }

function TMappedCollection.GetNameCount: Integer;
begin
  result := fHash.Count;
end;

procedure TMappedCollection.InsertItem(const Key: String; Item: TMappedCollectionItem);
begin
  if not (Item is fItemClass) then
     exit;
  fHash.Add(Key,Item);
  Notify(Item,cnAdded);
  Changed;
end;

procedure TMappedCollection.RemoveItem(Item: TMappedCollectionItem);
begin
  Notify(Item,cnExtracting);
  Item := fHash.Find(Item.Key) as TMappedCollectionItem;
  if Item = nil then
     exit;
  fHash.Remove(Item);
  Item.fMap:= nil;
  Changed;
end;

procedure TMappedCollection.DoClear;
begin
  while fHash.Count > 0 do
    (fHash.Items[0] as TMappedCollectionItem).Free;
end;

procedure TMappedCollection.Changed;
begin
  if FUpdateCount=0 then
    Update(Nil);
end;

function TMappedCollection.GetItem(const Key: String): TMappedCollectionItem;
Var
  I : Integer;

begin
  I:=fHash.FindIndexOf(Key);
  If (I<>-1) then
    Result:=fHash[i] as TMappedCollectionItem
  else
    raise Exception.Create('Name not in map: ' + Key);
end;

procedure TMappedCollection.SetItem(const Key: String; Value: TMappedCollectionItem);
Var
  I : Integer;

begin
  I:=fHash.FindIndexOf(Key);
  If (I<>-1) then
    (fHash[i] as TMappedCollectionItem).Assign(Value)
  else
    raise Exception.Create('Name not in map: ' + Key);
end;

function TMappedCollection.GetName(const Index: Integer): String;
begin
  result := fHash.NameOfIndex(Index);
end;

procedure TMappedCollection.Update(Item: TMappedCollectionItem);
begin
  FPONotifyObservers(Self,ooChange,Pointer(Item));
end;

procedure TMappedCollection.Notify(Item: TMappedCollectionItem;
  Action: TCollectionNotification);
begin
  Case Action of
    cnAdded      : FPONotifyObservers(Self,ooAddItem,Pointer(Item));
    cnExtracting : FPONotifyObservers(Self,ooDeleteItem,Pointer(Item));
    cnDeleting   : FPONotifyObservers(Self,ooDeleteItem,Pointer(Item));
  end;
end;

constructor TMappedCollection.Create(AItemClass: TMappedCollectionItemClass);
begin
  inherited create;
  FItemClass:=AItemClass;
  fHash := TFPHashObjectList.Create(false);
end;

destructor TMappedCollection.Destroy;
begin
  BeginUpdate; // Prevent OnChange
  DoClear;
  FreeAndNil(fHash);
  Inherited Destroy;
end;

function TMappedCollection.Add(const Key: String): TMappedCollectionItem;
begin
  Result:=FItemClass.Create(Self,Key);
end;

procedure TMappedCollection.Assign(Source: TPersistent);
Var
  I : Longint;
  Name: String;
begin
  If Source is TMappedCollection then
    begin
    Clear;
    For I:=0 To TMappedCollection(Source).NameCount-1 do
    begin
      Name := TMappedCollection(Source).Names[I];
      Add(Name).Assign(TMappedCollection(Source).Items[Name]);
    end;
    exit;
    end
  else
    Inherited Assign(Source);
end;

procedure TMappedCollection.BeginUpdate;
begin
  inc(fUpdateCount);
end;

procedure TMappedCollection.Clear;
begin
  if fHash.Count=0 then
    exit; // Prevent Changed
  BeginUpdate;
  try
    DoClear;
  finally
    EndUpdate;
  end;
end;

procedure TMappedCollection.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount=0 then
    Changed;
end;

procedure TMappedCollection.Delete(const Key: String);
Var
  Item : TMappedCollectionItem;
begin
  Item:= GetItem(Key);
  Notify(Item,cnDeleting);
  Item.Free;
end;

function TMappedCollection.Insert(const Key: String): TMappedCollectionItem;
begin
  Result:=Add(Key);
end;

function TMappedCollection.Find(const Key: String): TMappedCollectionItem;
Var
  I : Integer;

begin
  I:=fHash.FindIndexOf(Key);
  If (I<>-1) then
    Result:=fHash[i] as TMappedCollectionItem
  else
    Result := nil;
end;

end.

