unit stewfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewasync;

function ExtractPacketName(const aPath: String): String;

{ TPacketLister }
type
  TPacketName = String;
  TPacketList = array of TPacketName;
  TDeferredPacketListCallback = specialize GDeferredCallback<TPacketList>;

  TListPackets = class(TDeferredTask)
  private
    fPath: TFileName;
    fCallback: TDeferredPacketListCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFileName; aCallback: TDeferredPacketListCallback; aErrorback: TDeferredExceptionCallback);
  end;

  { TFileExists }

  TFileExists = class(TDeferredTask)
  private
    fPath: TFilename;
    fCallback: TDeferredBooleanCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFilename; aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
  end;

  TDeferredStreamCallback = specialize GDeferredCallback<TStream>;

  { TReadFile }

  TReadFile = class(TDeferredTask)
  private
    fPath: TFilename;
    fCallback: TDeferredStreamCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFilename; aCallback: TDeferredStreamCallback; aErrorback: TDeferredExceptionCallback);
  end;


  TDeferredCallback = procedure of object;

  { TWriteFile }

  TWriteFile = class(TDeferredTask)
  private
    fPath: TFilename;
    fCreateDir: Boolean;
    fData: UTF8String;
    fCallback: TDeferredCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFilename; const aData: UTF8String;
      aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
  overload;
    constructor Create(const aPath: TFilename; aCreateDir: Boolean;
      const aData: UTF8String; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback); overload;
  end;

implementation

uses
  strutils;

function ExtractPacketName(const aPath: String): String;
var
  _p: Integer;
begin
  result := ChangeFileExt(ExtractFileName(aPath),'');
  _p := RPos('_',Result);
  if _p <> 0 then
  begin
     result := Copy(Result,0,_p - 1);
  end;
end;

{ TWriteFile }

procedure TWriteFile.DoTask;
var
  stream: TFileStream;
begin
  if not DirectoryExists(ExtractFileDir(fPath)) and fCreateDir then
  begin
     ForceDirectories(ExtractFileDir(fPath));
  end;
  // otherwise, an error might occur, but that should be handled and reported by the base class here.

  stream := TFileStream.Create(fPath,fmCreate or fmShareDenyWrite);
  try
    stream.Write(fData[1],Length(fData));
  finally
    stream.Free;
  end;
  fCallback;

end;

constructor TWriteFile.Create(const aPath: TFilename; const aData: UTF8String;
  aCallback: TDeferredCallback; aErrorback: TDeferredExceptionCallback);
begin
  Create(aPath,false,aData,aCallback,aErrorback);

end;

constructor TWriteFile.Create(const aPath: TFilename; aCreateDir: Boolean;
  const aData: UTF8String; aCallback: TDeferredCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fCreateDir := aCreateDir;
  fData := aData;
  fCallback := aCallback;

end;


{ TReadFile }

procedure TReadFile.DoTask;
var
  stream: TFileStream;
begin
  if FileExists(fPath) then
  begin
     stream := TFileStream.Create(fPath,fmOpenRead or fmShareDenyWrite);
     try
       fCallback(stream);
     finally
       stream.Free;
     end;
  end
  else
  begin
    fCallback(nil);
  end;

end;

constructor TReadFile.Create(const aPath: TFilename;
  aCallback: TDeferredStreamCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fCallback := aCallback;
  fPath := aPath;
end;

{ TFileExists }

procedure TFileExists.DoTask;
begin
  fCallback(FileExists(fPath));

end;

constructor TFileExists.Create(const aPath: TFilename;
  aCallback: TDeferredBooleanCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fCallback  := aCallback;
end;

{ TPacketLister }

constructor TListPackets.Create(const aPath: TFileName; aCallback: TDeferredPacketListCallback; aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fCallback := aCallback;
end;

procedure TListPackets.DoTask;
var
  SR: TSearchRec;
  List: TStringList;
  Answer: TPacketList;
  i: Integer;
begin
  List := TStringList.Create;
  try
    List.Sorted := true;
    List.Duplicates := dupIgnore;
    // TODO: Should I allow case insensitive file names on linux?
    List.CaseSensitive := false;

    if DirectoryExists(fPath) then
    begin
         if FindFirst(IncludeTrailingPathDelimiter(ReplaceStr(fPath,'/',PathDelim)) + '*',faDirectory,SR) = 0 then
         begin
            try
              repeat
                if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Name[1] <> '_') then
                begin
                   List.Add(ExtractPacketName(SR.Name));
                end;
              until FindNext(SR) <> 0;
            finally
              FindClose(SR);
            end;
         end;
    end;
    SetLength(Answer,List.Count);
    for i := 0 to List.Count - 1 do
    begin
      Answer[i] := List[i];
    end;
  finally
    List.Free;
  end;
  fCallback(Answer);
end;

end.

