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

  TReadFileCallback = procedure(aData: TStream; aFileAge: Longint) of object;

  { TReadFile }

  TReadFile = class(TDeferredTask)
  private
    fPath: TFilename;
    fCallback: TReadFileCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFilename; aCallback: TReadFileCallback; aErrorback: TDeferredExceptionCallback);
  end;


  TWriteFileCallback = procedure(aFileAge: Longint) of object;

  { TWriteFile }

  TWriteFile = class(TDeferredTask)
  private
    fPath: TFilename;
    fFileAge: Longint;
    fCheckFileAge: Boolean;
    fCreateDir: Boolean;
    fData: UTF8String;
    fCallback: TWriteFileCallback;
    fConflictBack: TWriteFileCallback;
  protected
    procedure DoTask; override;
  public
    constructor Create(const aPath: TFilename; const aData: UTF8String;
      aCallback: TWriteFileCallback; aErrorback: TDeferredExceptionCallback);
  overload;
    // NOTE: for FileAge, pass the mtime retrieved by the TReadFile.
    // If the file was new, pass -1.
    constructor Create(const aPath: TFilename; aCreateDir: Boolean; aCheckFileAge: Boolean; aFileAge: Longint;
      const aData: UTF8String; aCallback: TWriteFileCallback; aConflictBack: TWriteFileCallback;
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

// TODO: At some point, I need to 'test' the save conflicts.
procedure TWriteFile.DoTask;
var
  stream: TFileStream;
  aCurrentAge: Longint;
begin
  // FUTURE: There is a potential race condition here. The file could
  // be modified between this file age check and the actual write.
  // However, by the time we get done with TFileStream.Create, there's
  // no way to check the previous creation time. So... we'll just have
  // to hope things aren't going too fast. I could possibly fix this by locking
  // the file, checking the mtime, then opening it, but I've never done that
  // before so I'd have to do some research that I don't have time for right now.
  if fCheckFileAge then
  begin
    aCurrentAge := FileAge(fPath);
    // if the file does not exist yet, it should return -1. If the file
    // did not exist before, the caller should have passed -1 to indicate a
    // new file. These are the possible conditions I see:
    // fFileAge  fCurrentAge   means
    // -1        -1            file did not exist before, file does not exist now, no conflict, write as normal.
    // -1        > -1          file did not exist before, file does exist now, conflict, so report and don't write.
    // > -1      = fFileAge    file existed before, file has same mtime as before, no conflict, write as normal.
    // > -1      <> fFileAgge  file existed before, file has different mtime now, conflict, so report and don't write.
    if aCurrentAge <> fFileAge then
    begin
      if fConflictBack <> nil then
        fConflictBack(aCurrentAge)
      else
        raise Exception.Create('File has been changed since last read. Can''t write.');
    end;
  end;

  if not DirectoryExists(ExtractFileDir(fPath)) and fCreateDir then
  begin
     ForceDirectories(ExtractFileDir(fPath));
  end;
  // otherwise, an error might occur, but that should be handled and reported by the base class here.

  stream := TFileStream.Create(fPath,fmCreate or fmShareDenyWrite);
  try
    aCurrentAge := FileAge(fPath);
    stream.Write(fData[1],Length(fData));
  finally
    stream.Free;
  end;
  fCallback(aCurrentAge);

end;

constructor TWriteFile.Create(const aPath: TFilename; const aData: UTF8String;
  aCallback: TWriteFileCallback; aErrorback: TDeferredExceptionCallback);
begin
  Create(aPath,false,false,-1,aData,aCallback,nil,aErrorback);

end;

constructor TWriteFile.Create(const aPath: TFilename; aCreateDir: Boolean;
  aCheckFileAge: Boolean; aFileAge: Longint; const aData: UTF8String;
  aCallback: TWriteFileCallback; aConflictBack: TWriteFileCallback;
  aErrorback: TDeferredExceptionCallback);
begin
  inherited Create(aErrorback);
  fPath := aPath;
  fFileAge := aFileAge;
  fCheckFileAge := aCheckFileAge;
  fCreateDir := aCreateDir;
  fData := aData;
  fCallback := aCallback;
  fConflictBack := aConflictBack;

end;


{ TReadFile }

procedure TReadFile.DoTask;
var
  stream: TFileStream;
  age: Longint;
begin
  if FileExists(fPath) then
  begin
     stream := TFileStream.Create(fPath,fmOpenRead or fmShareDenyWrite);
     try
       // get the age of the file now, while it's locked, to prevent certain
       // race conditions
       age := FileAge(fPath);
       fCallback(stream,age);
     finally
       stream.Free;
     end;
  end
  else
  begin
    fCallback(nil,-1);
  end;

end;

constructor TReadFile.Create(const aPath: TFilename;
  aCallback: TReadFileCallback; aErrorback: TDeferredExceptionCallback);
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

