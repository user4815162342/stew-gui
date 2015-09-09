unit test_sys_file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_file, sys_async;

type

  { TFileSpec }

  TFileSpec = class(TTestSpec)
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
    procedure CopyFileCallback({%H-}aSender: TPromise);
    procedure CopyFileCallback2(aSender: TPromise);
    procedure CopyFileCallback3(aSender: TPromise);
    procedure ListFilesCallback(aSender: TPromise);
    procedure ReadTestCallback(aSender: TPromise);
    procedure RenameFileCallback({%H-}aSender: TPromise);
    procedure RenameFileCallback2(aSender: TPromise);
    procedure SystemFunctionalityCallback1(aSender: TPromise);
    procedure SystemFunctionalityCallback2({%H-}aSender: TPromise);
  private
    fComplexFileAge: Longint;
    fCopyFileContents: String;
  protected
    function GetTestRootDir: TFile; virtual; abstract;
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
    procedure Test_File_Copying;
    procedure Test_File_Renaming;
    procedure Test_File_Batch_Renaming;
    procedure Test_Filename_Routines;
    procedure Test_System_Functionality;
  end;

implementation

uses
  gui_async;

{ TFileSpec }

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

procedure TFileSpec.ComplexWriteConflict3(Sender: TPromise;
  aError: TPromiseException);
var
  root: TFile;
begin
  if (Sender as TFileWritePromise).IsConflict then
  begin
    // should have worked correctly
    root := GetTestRootDir;
    root.GetContainedFile('bar').GetContainedFile('test.txt').Write([fwoCheckAge],
                                                                fComplexFileAge,
                                                                'TEST2').After(
                                                                @ComplexWriteCallback4,
                                                                @ComplexWriteConflict4).Tag := Sender.Tag;
  end
  else
    PromiseError(Sender,aError);
end;

procedure TFileSpec.EmptyWriteCallback(Sender: TPromise);
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('empty.txt').CheckExistence.After(@EmptyWriteCallback2,@PromiseError).Tag := Sender.Tag;
end;

procedure TFileSpec.EmptyWriteCallback2(Sender: TPromise);
begin
  if (Sender as TFileExistencePromise).Exists then
    EndAsync(Sender.Tag)
  else
    FailAsync(Sender.Tag,'Empty write did not create a file');
end;

procedure TFileSpec.PromiseError(aSender: TPromise; aError: TPromiseException);
begin
  FailAsync(aSender.Tag,aError);
end;

procedure TFileSpec.BasicWriteCallback(aSender: TPromise);
var
  root: TFile;
begin
  if (aSender as TFileWritePromise).Age = NewFileAge then
    FailAsync(aSender.Tag,'Basic writing returned a "new" file age')
  else
  begin
    root := GetTestRootDir;
    root.GetContainedFile('foo.txt').Read.After(@BasicWriteCallback2,@PromiseError).Tag := aSender.Tag;
  end;
end;

procedure TFileSpec.BasicWriteCallback2(aSender: TPromise);
var
  lData: UTF8String;
begin
  lData := (aSender as TFileReadPromise).ReadString;
  if lData <> 'TEST' then
    FailAsync(aSender.Tag,'Data read from file did not match data written')
  else
    EndAsync(aSender.Tag);

end;

procedure TFileSpec.BatchRenameCallback1(aSender: TPromise);
var
  root: TFile;
  target: TFileArray;
  i: Integer;
  lData: TFileArray;
begin
  lData := (aSender as TFileListPromise).Files;
  SetLength(target,Length(lData));
  root := GetTestRootDir;
  for i := 0 to Length(lData) -1 do
  begin
    target[i] := lData[i].Directory.GetContainedFile(lData[i].Name + '2');
  end;
  root.System.RenameFiles(lData,target).After(@BatchRenameCallback2,@PromiseError).Tag := aSender.Tag;

end;

procedure TFileSpec.BatchRenameCallback2(aSender: TPromise);
begin
  EndAsync(aSender.Tag);
end;

procedure TFileSpec.CheckExistenceCallback1(aSender: TPromise);
var
  root: TFile;
begin
  if not (aSender as TFileExistencePromise).Exists then
    FailAsync(aSender.Tag,'"_stew.json" should have existed in test data')
  else
  begin
    root := GetTestRootDir;
    root.GetContainedFile('_foo.bar').CheckExistence.
       After(@CheckExistenceCallback2,@PromiseError).Tag := aSender.Tag;
  end;
end;

procedure TFileSpec.CheckExistenceCallback2(aSender: TPromise);
begin
  if (aSender as TFileExistencePromise).Exists then
    FailAsync(aSender.Tag,'"_foo.bar" should not exist in test data.')
  else
    EndAsync(aSender.Tag);
end;

procedure TFileSpec.ComplexWriteCallback1(aSender: TPromise);
var
  root: TFile;
begin
  if (aSender as TFileWritePromise).Age = NewFileAge then
    FailAsync(aSender.Tag,'Basic writing returned a "new" file age')
  else
  begin
    root := GetTestRootDir;
    root.GetContainedFile('bar').GetContainedFile('test.txt').Read.After(@ComplexWriteCallback2,@PromiseError).Tag := aSender.Tag;
  end;

end;

procedure TFileSpec.ComplexWriteCallback2(aSender: TPromise);
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
    root.GetContainedFile('bar').GetContainedFile('test.txt').Write([fwoCheckAge],
                                                                fComplexFileAge - 1,
                                                                'TEST2').After(
                                                                @ComplexWriteCallback3,
                                                                @ComplexWriteConflict3).Tag := aSender.Tag;
  end;

end;

procedure TFileSpec.ComplexWriteCallback3(aSender: TPromise);
begin
  FailAsync(aSender.Tag,'Complex write with wrong file age should have reported a conflict.');
end;

procedure TFileSpec.ComplexWriteCallback4(aSender: TPromise);
begin
  // should have worked correctly this time...
  EndAsync(aSender.Tag);

end;

procedure TFileSpec.ComplexWriteConflict1(aSender: TPromise; aData: String);
begin
  if (aSender as TFileWritePromise).IsConflict then
     FailAsync(aSender.Tag,'Complex write first write should not have reported a conflict')
  else
     FailAsync(aSender.Tag,aData);
end;

procedure TFileSpec.ComplexWriteConflict4(aSender: TPromise; aData: String);
begin
  if (aSender as TFileWritePromise).IsConflict then
      FailAsync(aSender.Tag,'Final write should not have reported conflict')
  else
      FailAsync(aSender.Tag,aData);
end;

procedure TFileSpec.CopyFileCallback(aSender: TPromise);
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').Read.After(@CopyFileCallback2,@PromiseError).Tag := aSender.Tag;

end;

procedure TFileSpec.CopyFileCallback2(aSender: TPromise);
var
  root: TFile;
begin
  fCopyFileContents := (aSender as TFileReadPromise).ReadString;
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json.bak').Read.After(@CopyFileCallback3,@PromiseError).Tag := aSender.Tag;
end;

procedure TFileSpec.CopyFileCallback3(aSender: TPromise);
begin
  if (aSender as TFileReadPromise).ReadString <> fCopyFileContents then
    FailAsync(aSender.Tag,'Copied file did not contain the same contents')
  else
    EndAsync(aSender.Tag);
end;

procedure TFileSpec.ListFilesCallback(aSender: TPromise);
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

procedure TFileSpec.ReadTestCallback(aSender: TPromise);
begin
  if (aSender as TFileReadPromise).DoesNotExist then
    FailAsync(aSender.Tag,'Read test did not return any data')
  else if (aSender as TFileReadPromise).Age = NewFileAge then
    FailAsync(aSender.Tag,'Read test returned "new" file')
  else
    EndAsync(aSender.Tag);
end;

procedure TFileSpec.RenameFileCallback(aSender: TPromise);
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json.2').CheckExistence.
    After(@RenameFileCallback2,@PromiseError).Tag := BeginAsync;
end;

procedure TFileSpec.RenameFileCallback2(aSender: TPromise);
begin
  if not (aSender as TFileExistencePromise).Exists then
    FailAsync(aSender.Tag,'Renamed file does not exist')
  else
    EndAsync(aSender.Tag);
end;

procedure TFileSpec.SystemFunctionalityCallback1(aSender: TPromise);
var
  root: TFile;
  lData: TTemplateArray;
begin
  root := GetTestRootDir;
  lData := (aSender as TFileListTemplatesPromise).Templates;
  if (Length(lData) = 0) then
  begin
    FailAsync(aSender.Tag,'No templates found for ".png" extension. This may actually be due to a normal local system configuration.');
  end
  else
  begin
    root.GetContainedFile('test.png').CreateFromTemplate(lData[0]).After(@SystemFunctionalityCallback2,@PromiseError).Tag := aSender.Tag;
  end;
end;

procedure TFileSpec.SystemFunctionalityCallback2(aSender: TPromise);
var
  root: TFile;
begin
  root := GetTestRootDir;
  Alert('The test will attempting to open a file in an editor. You may see other windows flash on screen. The test is successful even if these windows are error messages. For more information, see comments in test_sys_file.TFileSpec.SystemFunctionalityCallback2.');
  // If the temporary file has already been deleted by the time the editor application
  // runs, the system (or an application) may show a message box indicating problems
  // opening the file. This is not considered an error for this test, as there is no
  // way to test for whether the program succeed anyway.
  root.GetContainedFile('test.png').OpenInEditor;
  EndAsync(aSender.Tag);
end;

procedure TFileSpec.SetupTest;
begin
  inherited SetupTest;
  SetAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
end;

procedure TFileSpec.CleanupTest;
begin
  RemoveAsyncCallQueuer(@gui_async.GUIQueueAsyncCall);
  inherited CleanupTest;
end;

procedure TFileSpec.Test_List_Files;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.List.After(@ListFilesCallback,@PromiseError).Tag := BeginAsync;
end;

procedure TFileSpec.Test_Check_Existence;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').CheckExistence.
     After(@CheckExistenceCallback1,@PromiseError).Tag := BeginAsync;

end;

procedure TFileSpec.Test_Read_File;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').Read.After(@ReadTestCallback,@PromiseError).Tag := BeginAsync;
end;

procedure TFileSpec.Test_Basic_File_Writing;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('foo.txt').Write('TEST').After(@BasicWriteCallback,@PromiseError).Tag := BeginAsync;

end;

procedure TFileSpec.Test_Empty_File_Writing;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('empty.txt').Write('').After(@EmptyWriteCallback,@PromiseError).Tag := BeginAsync;

end;

procedure TFileSpec.Test_File_Writing_with_Conflict_Checking;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('bar').GetContainedFile('test.txt').Write([fwoCreateDir,fwoCheckAge],
                                                                  NewFileAge,
                                                                  'TEST').After(
                                                                  @ComplexWriteCallback1,
                                                                  @ComplexWriteConflict1).Tag := BeginAsync;
end;

procedure TFileSpec.Test_File_Copying;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').CopyTo(root.GetContainedFile('_stew.json.bak')).After(@CopyFileCallback,@PromiseError).Tag := BeginAsync;
end;

procedure TFileSpec.Test_File_Renaming;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').Rename(root.GetContainedFile('_stew.json.2')).After(@RenameFileCallback,@PromiseError).Tag := BeginAsync;
end;

procedure TFileSpec.Test_File_Batch_Renaming;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('Notes').GetContainedFile('Characters').List.
     After(@BatchRenameCallback1,@PromiseError).Tag := BeginAsync;
end;

procedure TFileSpec.Test_Filename_Routines;
var
  root: TFile;
  lChild: TFile;
begin
  root := GetTestRootDir;
  root := root.WithDifferentExtension('txt');
  Assert(root.Extension = 'txt','WithDifferentExtension should have changed the extension');
  root := root.WithDifferentDescriptorAndExtension('notes','doc');
  Assert((root.Descriptor = 'notes') and (root.Extension = 'doc'),'WithDifferentDescriptorAndExtention should have worked');
  root := root.WithDifferentPacketName('Foo');
  Assert(root.PacketName = 'Foo','WithDifferentPacketName should have worked');
  root := GetTestRootDir;
  lChild := root.GetContainedFile('Test').GetContainedFile('Foo').GetContainedFile('Bar');
  Assert(root.Contains(lChild),'Contains should return correct result');
  Assert(not lChild.Contains(root),'Contains should return correct result');
end;

procedure TFileSpec.Test_System_Functionality;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('test.png').ListTemplatesFor.After(@SystemFunctionalityCallback1,@PromiseError).Tag := BeginAsync;
end;

end.

