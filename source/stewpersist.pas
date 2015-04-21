unit stewpersist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stewtypes;

type

  { TUpdateAware }
  // Basically an object that observes changes in others, and when it gets notified
  // marks itself as modified.
  TUpdateAware = class(TPersistent, IFPObserver)
  private
    FModified: Boolean;
  protected
    procedure FPOObservedChanged({%H-}ASender : TObject; {%H-}Operation : TFPObservedOperation; {%H-}Data : Pointer);
    procedure SetModified; virtual;
    procedure ClearModified; virtual;
  public
    property Modified: Boolean read FModified;
  end;

  { TFilebackedStore }

  TFilebackedStore = class(TUpdateAware)
  private
    fFilename: TFilename;
  protected
    procedure Clear; virtual; abstract;
  public
    constructor Create(aFilename: TFilename);
    procedure Load;
    procedure Save;
  end;


  TFilingState = (fsInactive, fsLoading, fsSaving);

  { TAsyncFileBackedStore }

  TAsyncFileBackedStore = class(TUpdateAware)
  private
    fFilename: TFilename;
    fFileAge: Longint;
    fCreateDir: Boolean;
    fOnFileLoaded: TNotifyEvent;
    fOnFileLoadFailed: TExceptionEvent;
    fOnFileSaved: TNotifyEvent;
    fOnFileSaveFailed: TExceptionEvent;
    fOnFileSaveConflicted: TNotifyEvent;
    fFilingState: TFilingState;
  protected
    procedure Clear; virtual; abstract;
    procedure FileLoaded(aData: TStream; aFileAge: Longint);
    procedure FileSaved(aFileAge: Longint);
    procedure FileLoadFailed(aError: Exception);
    procedure FileSaveFailed(aError: Exception);
    // File age is only passed for informational purposes, and I don't
    // need that information here.
    procedure FileSaveConflicted({%H-}aFileAge: Longint);
  public
    constructor Create(afileName: TFilename; aCreateDir: Boolean = false);
    destructor Destroy; override;
    procedure Load;
    // set force to true to ignore conflicts. This is usually done after
    // an attempt to save fails due to a file time conflict and the user chooses
    // to save anyway.
    procedure Save(aForce: Boolean = false);
    property FilingState: TFilingState read fFilingState;
    property OnFileLoaded: TNotifyEvent read fOnFileLoaded write fOnFileLoaded;
    property OnFileLoadFailed: TExceptionEvent read fOnFileLoadFailed write fOnFileLoadFailed;
    property OnFileSaved: TNotifyEvent read fOnFileSaved write fOnFileSaved;
    property OnFileSaveFailed: TExceptionEvent read fOnFileSaveFailed write fOnFileSaveFailed;
    property OnFileSaveConflicted: TNotifyEvent read fOnFileSaveConflicted write fOnFileSaveConflicted;
  end;

implementation

uses
  fpjsonrtti, stewfile;

{ TAsyncFileBackedStore }

procedure TAsyncFileBackedStore.FileLoaded(aData: TStream;
  aFileAge: Longint);
var
  loader: TJSONDeStreamer;
  ss: TStringStream;
begin
  if (aData = nil) then
  begin
    // the file does not exist yet, so create a blank data object.
    Clear;
    fFileAge := aFileAge;
    // set it modified so that when it saves it creates an empty file.
    SetModified;
  end
  else
  begin
    ss := TStringStream.Create('');
    try
      loader := TJSONDeStreamer.Create(nil);
      try
        ss.CopyFrom(aData,0);
        loader.JSONToObject(ss.DataString,Self);
        fFileAge := aFileAge;
        ClearModified;

      finally
        loader.Free;
      end;
    finally
      ss.Free;
    end;
  end;
  if fOnFileLoaded <> nil then
    fOnFileLoaded(Self);
  fFilingState := fsInactive;
end;

procedure TAsyncFileBackedStore.FileSaved(aFileAge: Longint);
begin
  ClearModified;
  fFileAge := aFileAge;
  if fOnFileSaved <> nil then
    fOnFileSaved(Self);
  fFilingState := fsInactive;
end;

procedure TAsyncFileBackedStore.FileLoadFailed(aError: Exception);
begin
  if fOnFileLoadFailed <> nil then
    fOnFileLoadFailed(Self,aError);
  fFilingState := fsInactive;
end;

procedure TAsyncFileBackedStore.FileSaveFailed(aError: Exception);
begin
  if fOnFileSaveFailed <> nil then
    fOnFileSaveFailed(Self,aError);
  fFilingState := fsInactive;
end;

procedure TAsyncFileBackedStore.FileSaveConflicted(aFileAge: Longint);
begin
  if fOnFileSaveConflicted <> nil then
    fOnFileSaveConflicted(Self)
  else if fOnFileSaveFailed <> nil then
    fOnFileSaveFailed(Self,Exception.Create('File could not be saved because it was changed on disk since the last save'));
  fFilingState := fsInactive;
end;

constructor TAsyncFileBackedStore.Create(afileName: TFilename;
  aCreateDir: Boolean);
begin
  inherited Create;
  fFilename := afileName;
  fCreateDir := aCreateDir;
  fFileAge := -1; // indicates that this might be a new file.
  Clear;
end;

destructor TAsyncFileBackedStore.Destroy;
begin
  inherited Destroy;
end;

procedure TAsyncFileBackedStore.Load;
begin
  if fFilingState = fsInactive then
     TReadFile.Create(fFilename,@FileLoaded,@FileLoadFailed).Enqueue
  else if fFilingState = fsSaving then
     raise Exception.Create('Can''t load JSON data while saving.');
  // otherwise, already loading, so ignore.
end;

procedure TAsyncFileBackedStore.Save(aForce: Boolean);
var
  text: UTF8String;
  saver: TJSONStreamer;
begin
  if Modified then
  begin
    if fFilingState = fsInactive then
    begin
      fFilingState := fsSaving;
      saver := TJSONStreamer.Create(nil);
      try
        saver.Options := [jsoUseFormatString,jsoTStringsAsArray];
        text := saver.ObjectToJSONString(Self);
        TWriteFile.Create(fFilename,fCreateDir and (fFileAge = -1),not aForce,fFileAge,text,@FileSaved,@FileSaveConflicted,@FileSaveFailed).Enqueue;
      finally
        saver.Free;
      end;

    end
    else if fFilingState = fsLoading then
      raise Exception.Create('Can''t save JSON data while still loading.');
    // otherwise, already saving, so don't worry about it.
  end;
end;

{ TUpdateAware }

procedure TUpdateAware.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  // pass it on...
  SetModified;
end;

procedure TUpdateAware.SetModified;
begin
  FModified := true;
  FPONotifyObservers(Self,ooChange,nil);
end;

procedure TUpdateAware.ClearModified;
begin
  FModified := false;
end;


{ TFilebackedStore }

constructor TFilebackedStore.Create(aFilename: TFilename);
begin
  inherited Create;
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
          ClearModified;

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
        ClearModified;
      finally
        saver.Free;
      end;
    finally
      fs.Free;
    end;

  end;

end;

end.

