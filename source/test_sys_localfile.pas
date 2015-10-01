unit test_sys_localfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_sys_file, sys_file;

type

  { TLocalFileSpec }

  TLocalFileSpec = class(TFileSpec)
  private
    fTestRootFilePath: String;
  protected
    function GetTestRootDir: TFile; override;
  public
    procedure SetupTest; override;
  end;


implementation

uses
  sys_localfile, FileUtil;

{ TLocalFileSpec }

function TLocalFileSpec.GetTestRootDir: TFile;
begin
  result := LocalFile(fTestRootFilePath);
end;

procedure TLocalFileSpec.SetupTest;
begin
  inherited SetupTest;
  fTestRootFilePath := CopyTemporaryFileData('../test-data/story/');
end;

end.

