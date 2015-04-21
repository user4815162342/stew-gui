unit stewpersist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TStore }

  TStore = class(TPersistent)
    procedure ComponentChange(Sender: TObject);
  protected
    procedure Clear; virtual;
    procedure SetModified; virtual; abstract;
    function CreateStrings: TStrings;
  end;

  { TParentedStore }

  TParentedStore = class(TStore)
  private
    fParent: TStore;
  protected
    procedure SetModified; override;
  public
    constructor Create(aParent: TStore);
  end;

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

{ TStore }

procedure TStore.ComponentChange(Sender: TObject);
begin
  SetModified;
end;

procedure TStore.Clear;
begin
  SetModified;
end;

function TStore.CreateStrings: TStrings;
var
  answer: TStringList;
begin
  answer := TStringList.Create;
  result := answer;
  answer.OnChange:=@ComponentChange;
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

procedure TParentedStore.SetModified;
begin
  fParent.SetModified;
end;

constructor TParentedStore.Create(aParent: TStore);
begin
  fParent := aParent;
end;

end.

