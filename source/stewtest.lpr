program stewtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, richmemopackage, test_main, test_registry, test_test, sys_json,
  test_sys_json, test_sys_async, gui_async, sys_async, test_sys_localfile, 
test_sys_file, sys_localfile, sys_file, sys_os, stew_properties, 
test_stew_properties
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

