unit test_sys_filecache_local;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_sys_filecache, sys_file;

type

  { TLocalFileSpec }

  { TLocalFileCacheSpec }

  TLocalFileCacheSpec = class(TCacheSpec)
  private
    fTestRootFilePath: String;
  protected
    function GetTestRootDir: TFile; override;
    function GetFileSystem: TFileSystemClass; override;
  public
    procedure SetupTest; override;
  end;


implementation

uses
  sys_localfile, FileUtil;

{ TLocalFileSpec }

function TLocalFileCacheSpec.GetTestRootDir: TFile;
begin
  result := LocalFile(fTestRootFilePath);
end;

function TLocalFileCacheSpec.GetFileSystem: TFileSystemClass;
begin
  result := TLocalFileSystem;
end;

procedure TLocalFileCacheSpec.SetupTest;
begin
  inherited SetupTest;
  fTestRootFilePath := CopyTemporaryFileData('../test-data/story/');
end;

end.

