unit test_sys_file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry, sys_file;

type

  { TFileSpec }

  TFileSpec = class(TTestSpec)
    procedure BasicWriteCallback(aFileAge: Longint);
    procedure BasicWriteCallback2(aData: TStream; {%H-}aFileAge: Longint);
    procedure BasicWriteErrorBack(Data: String);
    procedure BatchRenameCallback1(Data: TFile.TFileArray);
    procedure BatchRenameCallback2;
    procedure BatchRenameError(Data: String);
    procedure CheckExistenceCallback1(Data: Boolean);
    procedure CheckExistenceCallback2(Data: Boolean);
    procedure CheckExistenceErrorBack(Data: String);
    procedure ComplexWriteCallback1(aFileAge: Longint);
    procedure ComplexWriteCallback2(aData: TStream; aFileAge: Longint);
    procedure ComplexWriteCallback3({%H-}aFileAge: Longint);
    procedure ComplexWriteCallback4({%H-}aFileAge: Longint);
    procedure ComplexWriteConflict1({%H-}aFileAge: Longint);
    procedure ComplexWriteConflict3({%H-}aFileAge: Longint);
    procedure ComplexWriteConflict4({%H-}aFileAge: Longint);
    procedure ComplexWriteError(Data: String);
    procedure ComplexWriteErrorback(Data: String);
    procedure CopyFileCallback;
    procedure CopyFileCallback2(aData: TStream; {%H-}aFileAge: Longint);
    procedure CopyFileCallback3(aData: TStream; {%H-}aFileAge: Longint);
    procedure CopyFileError(Data: String);
    procedure ListFilesCallback(Data: TFile.TFileArray);
    procedure ListFilesErrorBack(Data: String);
    procedure ReadTestCallback(aData: TStream; aFileAge: Longint);
    procedure ReadTestErrorback(Data: String);
    procedure RenameFileCallback;
    procedure RenameFileCallback2(Data: Boolean);
    procedure RenameFileError(Data: String);
    procedure SystemFunctionalityCallback1(Data: TTemplateArray);
    procedure SystemFunctionalityCallback2;
    procedure SystemFunctionalityError(Data: String);
  private
    // Once we have everything moved over to Promises, I shouldn't
    // have to keep this here. I can just make this part of the promise
    // and use Sender arguments on the callbacks.
    fAsyncCode: Integer;
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
    procedure Test_File_Writing_with_Conflict_Checking;
    procedure Test_File_Copying;
    procedure Test_File_Renaming;
    procedure Test_File_Batch_Renaming;
    procedure Test_Filename_Routines;
    procedure Test_System_Functionality;

  end;

implementation

uses
  sys_async, gui_async;

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

procedure TFileSpec.BasicWriteCallback(aFileAge: Longint);
var
  root: TFile;
begin
  if aFileAge = NewFileAge then
    FailAsync(fAsyncCode,'Basic writing returned a "new" file age')
  else
  begin
    root := GetTestRootDir;
    root.GetContainedFile('foo.txt').Read(@BasicWriteCallback2,@BasicWriteErrorback);
  end;
end;

procedure TFileSpec.BasicWriteCallback2(aData: TStream; aFileAge: Longint);
var
  check: TStringStream;
begin
  check := TStringStream.Create('');
  try
    check.CopyFrom(aData,0);
    if check.DataString <> 'TEST' then
      FailAsync(fAsyncCode,'Data read from file did not match data written')
    else
      EndAsync(fAsyncCode);
  finally
    check.Free;
  end;

end;

procedure TFileSpec.BasicWriteErrorBack(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.BatchRenameCallback1(Data: TFile.TFileArray);
var
  root: TFile;
  target: TFile.TFileArray;
  i: Integer;
begin
  SetLength(target,Length(Data));
  root := GetTestRootDir;
  for i := 0 to Length(Data) -1 do
  begin
    target[i] := Data[i].Directory.GetContainedFile(Data[i].Name + '2');
  end;
  root.System.RenameFiles(Data,target,@BatchRenameCallback2,@BatchRenameError);

end;

procedure TFileSpec.BatchRenameCallback2;
begin
  EndAsync(fAsyncCode);
end;

procedure TFileSpec.BatchRenameError(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.CheckExistenceCallback1(Data: Boolean);
var
  root: TFile;
begin
  if not Data then
    FailAsync(fAsyncCode,'"_stew.json" should have existed in test data')
  else
  begin
    root := GetTestRootDir;
    root.GetContainedFile('_foo.bar').CheckExistence(@CheckExistenceCallback2,@CheckExistenceErrorBack);
  end;
end;

procedure TFileSpec.CheckExistenceCallback2(Data: Boolean);
begin
  if Data then
    FailAsync(fAsyncCode,'"_foo.bar" should not exist in test data.')
  else
    EndAsync(fAsyncCode);
end;

procedure TFileSpec.CheckExistenceErrorBack(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.ComplexWriteCallback1(aFileAge: Longint);
var
  root: TFile;
begin
  if aFileAge = NewFileAge then
    FailAsync(fAsyncCode,'Basic writing returned a "new" file age')
  else
  begin
    root := GetTestRootDir;
    root.GetContainedFile('bar').GetContainedFile('test.txt').Read(@ComplexWriteCallback2,@ComplexWriteErrorback);
  end;

end;

procedure TFileSpec.ComplexWriteCallback2(aData: TStream; aFileAge: Longint);
var
  check: TStringStream;
  root: TFile;
begin
  fComplexFileAge := aFileAge;
  check := TStringStream.Create('');
  try
    check.CopyFrom(aData,0);
    if check.DataString <> 'TEST' then
      FailAsync(fAsyncCode,'Data read from file did not match data written')
    else
    begin
      root := GetTestRootDir;
      root.GetContainedFile('bar').GetContainedFile('test.txt').Write(false,
                                                                  true,
                                                                  fComplexFileAge - 1,
                                                                  'TEST2',
                                                                  @ComplexWriteCallback3,
                                                                  @ComplexWriteConflict3,
                                                                  @ComplexWriteError);
    end;
  finally
    check.Free;
  end;

end;

procedure TFileSpec.ComplexWriteCallback3(aFileAge: Longint);
begin
  FailAsync(fAsyncCode,'Complex write with wrong file age should have reported a conflict.');
end;

procedure TFileSpec.ComplexWriteCallback4(aFileAge: Longint);
begin
  // should have worked correctly this time...
  EndAsync(fAsyncCode);

end;

procedure TFileSpec.ComplexWriteConflict1(aFileAge: Longint);
begin
  FailAsync(fAsyncCode,'Complex write first write should not have reported a conflict');
end;

procedure TFileSpec.ComplexWriteConflict3(aFileAge: Longint);
var
  root: TFile;
begin
  // should have worked correctly
  root := GetTestRootDir;
  root.GetContainedFile('bar').GetContainedFile('test.txt').Write(false,
                                                              true,
                                                              fComplexFileAge,
                                                              'TEST2',
                                                              @ComplexWriteCallback4,
                                                              @ComplexWriteConflict4,
                                                              @ComplexWriteError);
end;

procedure TFileSpec.ComplexWriteConflict4(aFileAge: Longint);
begin
  FailAsync(fAsyncCode,'Final write should not have reported conflict');
end;

procedure TFileSpec.ComplexWriteError(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.ComplexWriteErrorback(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.CopyFileCallback;
var
  root: TFile;
begin
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').Read(@CopyFileCallback2,@CopyFileError);

end;

procedure TFileSpec.CopyFileCallback2(aData: TStream; aFileAge: Longint);
var
  lCheck: TStringStream;
  root: TFile;
begin
  lCheck := TStringStream.Create('');
  try
    lCheck.CopyFrom(aData,0);
    fCopyFileContents := lCheck.DataString;
    root := GetTestRootDir;
    root.GetContainedFile('_stew.json.bak').Read(@CopyFileCallback3,@CopyFileError);
  finally
    lCheck.Free;
  end;
end;

procedure TFileSpec.CopyFileCallback3(aData: TStream; aFileAge: Longint);
var
  lCheck: TStringStream;
begin
  lCheck := TStringStream.Create('');
  try
    lCheck.CopyFrom(aData,0);
    if lCheck.DataString <> fCopyFileContents then
      FailAsync(fAsyncCode,'Copied file did not contain the same contents')
    else
      EndAsync(fAsyncCode);
  finally
    lCheck.Free;
  end;
end;

procedure TFileSpec.CopyFileError(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.ListFilesCallback(Data: TFile.TFileArray);
var
  i: Integer;
  j: Integer;
  lFound: Boolean;
begin
  lFound := false;
  if Length(data) <> Length(ExpectedFileNamesInList) then
  begin
    FailAsync(fAsyncCode,'Expected list results to have ' + IntToStr(Length(ExpectedFileNamesInList)) + ' results.');
    exit;

  end;

  for i := 0 to Length(Data) - 1 do
  begin
    lFound := false;
    for j := 0 to Length(ExpectedFileNamesInList) - 1 do
    begin
       if ExpectedFileNamesInList[j] = Data[i].Name then
       begin
         lFound := true;
         break;
       end;
    end;
    if (not lFound) then
    begin
      FailAsync(fAsyncCode,'List result "' + Data[i].Name  + '" was not expected.');
      exit;
    end;
  end;

  EndAsync(fAsyncCode);

end;

procedure TFileSpec.ListFilesErrorBack(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.ReadTestCallback(aData: TStream; aFileAge: Longint);
begin
  if aData = nil then
    FailAsync(fAsyncCode,'Read test did not return any data')
  else if aFileAge = NewFileAge then
    FailAsync(fAsyncCode,'Read test returned "new" file')
  else
    EndAsync(fAsyncCode);
end;

procedure TFileSpec.ReadTestErrorback(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.RenameFileCallback;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json.2').CheckExistence(@RenameFileCallback2,@RenameFileError);
end;

procedure TFileSpec.RenameFileCallback2(Data: Boolean);
begin
  if not Data then
    FailAsync(fAsyncCode,'Renamed file does not exist')
  else
    EndAsync(fAsyncCode);
end;

procedure TFileSpec.RenameFileError(Data: String);
begin
  FailAsync(fAsyncCode,Data);
end;

procedure TFileSpec.SystemFunctionalityCallback1(Data: TTemplateArray);
var
  root: TFile;
begin
  root := GetTestRootDir;
  if (Length(Data) = 0) then
  begin
    FailAsync(fAsyncCode,'No templates found for ".txt" extension. This may actually be due to a normal local system configuration.');
  end
  else
  begin
    root.GetContainedFile('test.png').CreateFromTemplate(Data[0],@SystemFunctionalityCallback2,@SystemFunctionalityError);
  end;
end;

procedure TFileSpec.SystemFunctionalityCallback2;
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
  EndAsync(fAsyncCode);
end;

procedure TFileSpec.SystemFunctionalityError(Data: String);
begin
  FailAsync(fAsyncCode,Data);
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
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.List(@ListFilesCallback,@ListFilesErrorBack);
end;

procedure TFileSpec.Test_Check_Existence;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').CheckExistence(@CheckExistenceCallback1,@CheckExistenceErrorBack);

end;

procedure TFileSpec.Test_Read_File;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').Read(@ReadTestCallback,@ReadTestErrorback);
end;

procedure TFileSpec.Test_Basic_File_Writing;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('foo.txt').Write('TEST',@BasicWriteCallback,@BasicWriteErrorback);

end;

procedure TFileSpec.Test_File_Writing_with_Conflict_Checking;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('bar').GetContainedFile('test.txt').Write(true,
                                                                  true,
                                                                  NewFileAge,
                                                                  'TEST',
                                                                  @ComplexWriteCallback1,
                                                                  @ComplexWriteConflict1,
                                                                  @ComplexWriteError);
end;

procedure TFileSpec.Test_File_Copying;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').CopyTo(root.GetContainedFile('_stew.json.bak'),@CopyFileCallback,@CopyFileError);
end;

procedure TFileSpec.Test_File_Renaming;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('_stew.json').Rename(root.GetContainedFile('_stew.json.2'),@RenameFileCallback,@RenameFileError);
end;

procedure TFileSpec.Test_File_Batch_Renaming;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('Notes').GetContainedFile('Characters').List(@BatchRenameCallback1,@BatchRenameError);
end;

procedure TFileSpec.Test_Filename_Routines;
var
  root: TFile;
  name: String;
begin
  root := GetTestRootDir;
  root := root.WithDifferentExtension('txt');
  Assert(root.Extension = 'txt','WithDifferentExtension should have changed the extension');
  root := root.WithDifferentDescriptorAndExtension('notes','doc');
  Assert((root.Descriptor = 'notes') and (root.Extension = 'doc'),'WithDifferentDescriptorAndExtention should have worked');
  root := root.WithDifferentPacketName('Foo');
  Assert(root.PacketName = 'Foo','WithDifferentPacketName should have worked');
end;

procedure TFileSpec.Test_System_Functionality;
var
  root: TFile;
begin
  fAsyncCode := BeginAsync;
  root := GetTestRootDir;
  root.GetContainedFile('test.png').ListTemplatesFor(@SystemFunctionalityCallback1,@SystemFunctionalityError);
end;

end.

