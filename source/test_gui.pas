unit test_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, test_registry;

type
{
TODO: Save this testing until after the GUI works with the new architecture.

TODO: Basically, each "test" will consist of running a python script against
LTDP. The scripts will open up the most current gui executable and test the
interface against a bunch of operations, that are basically the same as testing
stew project. The only requirement of the script is that it must print out an
error if an error occurs, and a standard "ok" response if the test works.

The test methods on this spec will all just run the scripts in a TProcess, then
check for the "ok" in the output, or fail on whatever message *is* shown.

- config testing (probably after opening the initial project):
  - open up the GUI, maximize it, close it and reopen, make sure it's still maximized
  - shrink it to a certain size, close it and reopen, and make sure it's got the same
    dimensions.
  - do something similar with the width of the project explorer panel
  - make sure the MRU project thing lists things appropriately.
- open project that doesn't exist
- open project from a subfolder of an existing project
- open an existing project
- open up the project and verify that the list appears on the left
  - also, click on each one and verify that child documents also appear.
  - if possible, make sure that items show up as folders if necessary.
  - make sure that when you double-click on an element, that it opens up the properties.
- open up the project properties and attempt to do some editing.
- open up a document properties and attempt to do some editing.
  - also make sure synopsis is there and editable.
- create a new document, edit it and save it.
- shift a document up or down.
- "drag" a document to another location (rename)
- open up the editor for a document (this will be a little difficult since
  we can't guarantee what editor will open) and close it.
  - also same for notes
}


  { TGUISpec }

  TGUISpec = class(TTestSpec)
  private
    fTempDir: UTF8String;
  public
    procedure SetupTest; override;
  end;

implementation

uses
  FileUtil;

{ TGUISpec }

procedure TGUISpec.SetupTest;
begin
  inherited SetupTest;
  fTempDir := CopyTemporaryFileData('../test-data/story/');
  Alert('Make sure the GUI executable has been compiled with current code before accepting the results of this next test.');
end;

end.

