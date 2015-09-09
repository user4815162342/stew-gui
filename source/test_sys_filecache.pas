unit test_sys_filecache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_file, sys_async, sys_filecache;

type

  { TCacheSpec }

  TCacheSpec = class(TTestSpec)
  private
    procedure EmptyWriteCallback(Sender: TPromise);
    procedure EmptyWriteCallback2(Sender: TPromise);
    procedure PromiseError(aSender: TPromise; aError: TPromiseException);
    procedure BasicWriteCallback(aSender: TPromise);
    procedure BasicWriteCallback2(aSender: TPromise);
    procedure BatchRenameCallback1(aSender: TPromise);
    procedure BatchRenameCallback2({%H-}aSender: TPromise);
    procedure CheckExistenceCallback1(aSender: TPromise);
    procedure CheckExistenceCallback2(aSender: TPromise);
    procedure ComplexWriteCallback1(aSender: TPromise);
    procedure ComplexWriteCallback2(aSender: TPromise);
    procedure ComplexWriteCallback3({%H-}aSender: TPromise);
    procedure ComplexWriteCallback4({%H-}aSender: TPromise);
    procedure ComplexWriteConflict1(aSender: TPromise; aData: String);
    procedure ComplexWriteConflict3(Sender: TPromise; aError: TPromiseException);
    procedure ComplexWriteConflict4(aSender: TPromise; aData: String);
    procedure ListFilesCallback(aSender: TPromise);
    procedure ReadTestCallback(aSender: TPromise);
  private
    fComplexFileAge: Longint;
    fCache: TFileSystemCache;
  protected
    function GetTestRootDir: TFile; virtual; abstract;
    function GetFileSystem: TFileSystemClass; virtual; abstract;
  public
    procedure SetupTest; override;
    procedure CleanupTest; override;
  published
    procedure Test_List_Files;
    procedure Test_Check_Existence;
    procedure Test_Read_File;
    procedure Test_Basic_File_Writing;
    procedure Test_Empty_File_Writing;
    procedure Test_File_Writing_with_Conflict_Checking;
    procedure Test_File_Batch_Renaming;
  end;

implementation

uses
  gui_async;

{ TCacheSpec }

const
  ExpectedFileNamesInList: array[0..11] of String = (
  'Chapter 1_properties.json',
  'Chapter 1.txt',
  'Chapter 3_properties.json',
  'Chapter 5_properties.json',
  'Notes',
  '_properties.json',
  'Chapter 1_synopsis.txt',
  'Chapter 2_properties.json',

  'Chapter 4_properties.json',
  'Epilogue_properties.json',
  'Notes_properties.json',
  '_stew.json'
  );

procedure TCacheSpec.ComplexWriteConflict3(Sender: TPromise;
  aError: TPromiseException);
var
  root: TFile;
begin
  if (Sender as TFileWritePromise).IsConflict then
  begin
    // should have worked correctly
    root := GetTestRootDir;
    fCache.WriteFile(root.GetContainedFile('bar').GetContainedFile('test.txt'),[fwoCheckAge],
                                                                fComplexFileAge,
                                                                'TEST2').After(
                                                                @ComplexWriteCallback4,
                                                                @ComplexWriteConflict4).Tag := Sender.Tag;
  end
  else
    PromiseError(Sender,aError);
end;

procedure TCacheSpec.EmptyWriteCallback(Sender: TPromise);
var
  root: TFile;
begin
  root := GetTestRootDir;
  fCache.CheckExistence(root.GetContainedFile('empty.txt')).After(@EmptyWriteCallback2,@PromiseError).Tag := Sender.Tag;
end;

procedure TCacheSpec.EmptyWriteCallback2(Sender: TPromise);
begin
  if (Sender as TFileExistencePromise).Exists then
    EndAsync(Sender.Tag)
  else
    FailAsync(Sender.Tag,'Empty write did not create a file');
end;

procedure TCacheSpec.PromiseError(aSender: TPromise; aError: TPromiseException);
begin
  FailAsync(aSender.Tag,aError);
end;

procedure TCacheSpec.BasicWriteCallback(aSender: TPromise);
var
  root: TFile;
begin
  if (aSender as TFileWritePromise).Age = NewFileAge then
    FailAsync(aSender.Tag,'Basic writing returned a "new" file age')
  else
  begin
    root := GetTestRootDir;
    fCache.ReadFile(root.GetContainedFile('foo.txt')).After(@BasicWriteCallback2,@PromiseError).Tag := aSender.Tag;
  end;
end;

procedure TCacheSpec.BasicWriteCallback2(aSender: TPromise);
var
  lData: UTF8String;
begin
  lData := (aSender as TFileReadPromise).ReadString;
  if lData <> 'TEST' then
    FailAsync(aSender.Tag,'Data read from file did not match data written')
  else
    EndAsync(aSender.Tag);

end;

procedure TCacheSpec.BatchRenameCallback1(aSender: TPromise);
var
  target: TFileArray;
  i: Integer;
  lData: TFileArray;
begin
  lData := (aSender as TFileListPromise).Files;
  SetLength(target,Length(lData));
  for i := 0 to Length(lData) -1 do
  begin
    target[i] := lData[i].Directory.GetContainedFile(lData[i].Name + '2');
  end;
  fCache.RenameFiles(lData,target).After(@BatchRenameCallback2,@PromiseError).Tag := aSender.Tag;

end;

procedure TCacheSpec.BatchRenameCallback2(aSender: TPromise);
begin
  EndAsync(aSender.Tag);
end;

procedure TCacheSpec.CheckExistenceCallback1(aSender: TPromise);
var
  root: TFile;
begin
  if not (aSender as TFileExistencePromise).Exists then
    FailAsync(aSender.Tag,'"_stew.json" should have existed in test data')
  else
  begin
    root := GetTestRootDir;
    fCache.CheckExistence(root.GetContainedFile('_foo.bar')).
       After(@CheckExistenceCallback2,@PromiseError).Tag := aSender.Tag;
  end;
end;

procedure TCacheSpec.CheckExistenceCallback2(aSender: TPromise);
begin
  if (aSender as TFileExistencePromise).Exists then
    FailAsync(aSender.Tag,'"_foo.bar" should not exist in test data.')
  else
    EndAsync(aSender.Tag);
end;

procedure TCacheSpec.ComplexWriteCallback1(aSender: TPromise);
var
  root: TFile;
begin
  if (aSender as TFileWritePromise).Age = NewFileAge then
    FailAsync(aSender.Tag,'Basic writing returned a "new" file age')
  else
  begin
    root := GetTestRootDir;
    fCache.ReadFile(root.GetContainedFile('bar').GetContainedFile('test.txt')).After(@ComplexWriteCallback2,@PromiseError).Tag := aSender.Tag;
  end;

end;

procedure TCacheSpec.ComplexWriteCallback2(aSender: TPromise);
var
  lCheck: UTF8String;
  root: TFile;
begin
  fComplexFileAge := (aSender as TFileReadPromise).Age;
  lCheck := (aSender as TFileReadPromise).ReadString;
  if lCheck <> 'TEST' then
    FailAsync(aSender.Tag,'Data read from file did not match data written')
  else
  begin
    root := GetTestRootDir;
    fCache.WriteFile(root.GetContainedFile('bar').GetContainedFile('test.txt'),[fwoCheckAge],
                                                                fComplexFileAge - 1,
                                                                'TEST2').After(
                                                                @ComplexWriteCallback3,
                                                                @ComplexWriteConflict3).Tag := aSender.Tag;
  end;

end;

procedure TCacheSpec.ComplexWriteCallback3(aSender: TPromise);
begin
  FailAsync(aSender.Tag,'Complex write with wrong file age should have reported a conflict.');
end;

procedure TCacheSpec.ComplexWriteCallback4(aSender: TPromise);
begin
  // should have worked correctly this time...
  EndAsync(aSender.Tag);

end;

procedure TCacheSpec.ComplexWriteConflict1(aSender: TPromise; aData: String);
begin
  if (aSender as TFileWritePromise).IsConflict then
     FailAsync(aSender.Tag,'Complex write first write should not have reported a conflict')
  else
     FailAsync(aSender.Tag,aData);
end;

procedure TCacheSpec.ComplexWriteConflict4(aSender: TPromise; aData: String);
begin
  if (aSender as TFileWritePromise).IsConflict then
      FailAsync(aSender.Tag,'Final write should not have reported conflict')
  else
      FailAsync(aSender.Tag,aData);
end;


procedure TCacheSpec.ListFilesCallback(aSender: TPromise);
var
  i: Integer;
  j: Integer;
  lFound: Boolean;
  lData: TFileArray;
begin
  if (aSender as TFileListPromise).DoesNotExist then
  begin
    FailAsync(aSender.Tag,'Expected list results to indicate that the directory exists');
    exit;
  end;

  lFound := false;
  lData := (aSender as TFileListPromise).Files;
  if Length(lData) <> Length(ExpectedFileNamesInList) then
  begin
    FailAsync(aSender.Tag,'Expected list results to have ' + IntToStr(Length(ExpectedFileNamesInList)) + ' results.');
    exit;

  end;

  for i := 0 to Length(lData) - 1 do
  begin
    lFound := false;
    for j := 0 to Length(ExpectedFileNamesInList) - 1 do
    begin
       if ExpectedFileNamesInList[j] = lData[i].Name then
       begin
         lFound := true;
         break;
       end;
    end;
    if (not lFound) then
    begin
      FailAsync(aSender.Tag,'List result "' + lData[i].Name  + '" was not expected.');
      exit;
    end;
  end;

  EndAsync(aSender.Tag);

end;

procedure TCacheSpec.ReadTestCallback(aSender: TPromise);
begin
  if (aSender as TFileReadPromise).DoesNotExist then
    FailAsync(aSender.Tag,'Read test did not return any data')
  else if (aSender as TFileReadPromise).Age = NewFileAge then
    FailAsync(aSender.Tag,'Read test returned "new" file')
  else
    EndAsync(aSender.Tag);
end;


procedure TCacheSpec.SetupTest;
begin
  inherited SetupTest;
  SetAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
  fCache := TFileSystemCache.Create(GetFileSystem);
end;

procedure TCacheSpec.CleanupTest;
begin
  FreeAndNil(fCache);
  RemoveAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
  inherited CleanupTest;
end;

procedure TCacheSpec.Test_List_Files;
var
  root: TFile;
begin
  root := GetTestRootDir;
  fCache.ListFiles(root).After(@ListFilesCallback,@PromiseError).Tag := BeginAsync;
end;

procedure TCacheSpec.Test_Check_Existence;
var
  root: TFile;
begin
  root := GetTestRootDir;
  fCache.CheckExistence(root.GetContainedFile('_stew.json')).
     After(@CheckExistenceCallback1,@PromiseError).Tag := BeginAsync;

end;

procedure TCacheSpec.Test_Read_File;
var
  root: TFile;
begin
  root := GetTestRootDir;
  fCache.ReadFile(root.GetContainedFile('_stew.json')).After(@ReadTestCallback,@PromiseError).Tag := BeginAsync;
end;

procedure TCacheSpec.Test_Basic_File_Writing;
var
  root: TFile;
begin
  root := GetTestRootDir;
  fCache.WriteFile(root.GetContainedFile('foo.txt'),'TEST').After(@BasicWriteCallback,@PromiseError).Tag := BeginAsync;

end;

procedure TCacheSpec.Test_Empty_File_Writing;
var
  root: TFile;
begin
  root := GetTestRootDir;
  fCache.WriteFile(root.GetContainedFile('empty.txt'),'').After(@EmptyWriteCallback,@PromiseError).Tag := BeginAsync;

end;

procedure TCacheSpec.Test_File_Writing_with_Conflict_Checking;
var
  root: TFile;
begin
  root := GetTestRootDir;
  fCache.WriteFile(root.GetContainedFile('bar').GetContainedFile('test.txt'),[fwoCreateDir,fwoCheckAge],
                                                                  NewFileAge,
                                                                  'TEST').After(
                                                                  @ComplexWriteCallback1,
                                                                  @ComplexWriteConflict1).Tag := BeginAsync;
end;

procedure TCacheSpec.Test_File_Batch_Renaming;
var
  root: TFile;
begin
  root := GetTestRootDir;
  fCache.ListFiles(root.GetContainedFile('Notes').GetContainedFile('Characters')).
     After(@BatchRenameCallback1,@PromiseError).Tag := BeginAsync;
end;

end.

