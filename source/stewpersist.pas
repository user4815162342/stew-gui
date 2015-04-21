unit stewpersist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TUpdateAware }

  TUpdateAware = class(TPersistent, IFPObserver)
  protected
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
  end;

  { TStore }
  TStore = class(TPersistent)
  protected
    procedure Clear; virtual; abstract;
    procedure SetModified; virtual; abstract;
  public
    constructor Create;
  end;

  { TParentedStore }

  { GParentedStore }
  generic GParentedStore<ParentType> = class(TStore)
  private
    fParent: ParentType;
  protected
    procedure SetModified; override;
  public
    constructor Create(aParent: ParentType);
  end;

  TConventionallyParentedStore = specialize GParentedStore<TStore>;

  // TCollection and TCollectionItem are both already very good
  // at what they do, plus the jsonrtti is capable of handling them
  // without problems, so it makes sense to use these. It does break
  // the hierarchy, and require some weirdness if you're going to store
  // something inside it, but it works.
  { TStoredArrayItem }
  TStoredArrayItem = class(TCollectionItem)
  protected
    procedure SetModified; inline;
  public
    constructor Create(ACollection: TCollection); override;
  end;

  TStoredArrayItemClass = class of TStoredArrayItem;

  { TStoredArray }

  TStoredArray = class(TCollection)
  private
    fParent: TStore;
  protected
    procedure SetModified; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(aParent: TStore; AItemClass: TStoredArrayItemClass);
  end;

  TArrayItemParentedStore = specialize GParentedStore<TStoredArrayItem>;

  { GStoredStringArray }

  generic GStoredStringArray<ParentType> = class(TStringList)
  private
    fParent: ParentType;
  protected
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(aParent: ParentType);
  end;

  TConventionallyParentedStringList = specialize GStoredStringArray<TStore>;
  TArrayItemParentedStringArray = specialize GStoredStringArray<TStoredArrayItem>;

  { TFilebackedStore }

  TFilebackedStore = class(TStore)
  private
    fFilename: TFilename;
    fModified: Boolean;
  protected
    procedure SetModified; override;
  public
    constructor Create(aFilename: TFilename);
    procedure Load;
    procedure Save;
    property Modified: Boolean read fModified;
  end;

implementation

uses
  fpjsonrtti;

{ TUpdateAware }

procedure TUpdateAware.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  // pass it on...
  FPONotifyObservers(ASender,Operation,Data);
end;

{ TStore }

constructor TStore.Create;
begin
  inherited Create;
  Clear;
end;

{ GStoredStringList }

procedure GStoredStringArray.SetUpdateState(Updating: Boolean);
begin
  inherited SetUpdateState(Updating);
  if not Updating then
     fParent.SetModified;
end;

constructor GStoredStringArray.Create(aParent: ParentType);
begin
  inherited Create;
  fParent := aParent;
end;

{ TStoredArrayItem }

procedure TStoredArrayItem.SetModified; inline;
begin
  Changed(false);
  // should automatically notify the owning collection that it changed
  // and that should call setmodified. This is basically intended as
  // a convenience method to keep things the same.
end;

constructor TStoredArrayItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

{ TStoredArray }

procedure TStoredArray.SetModified;
begin
  fParent.SetModified;
end;

procedure TStoredArray.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  SetModified;
end;

constructor TStoredArray.Create(aParent: TStore;
  AItemClass: TStoredArrayItemClass);
begin
  inherited Create(AItemClass);
  fParent := aParent;
end;

{ TFilebackedStore }

procedure TFilebackedStore.SetModified;
begin
  fModified := true;
end;

constructor TFilebackedStore.Create(aFilename: TFilename);
begin
  inherited Create;
  fModified := false;
  fFilename := aFilename;
end;

procedure TFilebackedStore.Load;
var
  loader: TJSONDeStreamer;
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
        loader := TJSONDeStreamer.Create(nil);
        try
          ss.CopyFrom(fs,0);
          loader.JSONToObject(ss.DataString,Self);

        finally
          loader.Free;
        end;
      finally
        ss.Free;
      end;

    finally
      fs.Free;
    end;
  end;

end;

procedure TFilebackedStore.Save;
var
  fs: TFileStream;
  text: UTF8String;
  saver: TJSONStreamer;
begin
  if Modified then
  begin
    if not DirectoryExists(ExtractFileDir(fFilename)) then
    begin
      ForceDirectories(ExtractFileDir(fFilename));
    end;
    fs := TFileStream.Create(fFilename,fmCreate);
    try
      saver := TJSONStreamer.Create(nil);
      try
        saver.Options := [jsoUseFormatString,jsoTStringsAsArray];
        text := saver.ObjectToJSONString(Self);
        fs.Write(text[1],Length(text));
        fModified := False;
      finally
      end;
    finally
      fs.Free;
    end;

  end;

end;

{ TParentedStore }

procedure GParentedStore.SetModified;
begin
  fParent.SetModified;
end;

constructor GParentedStore.Create(aParent: ParentType);
begin
  fParent := aParent;
end;

end.

