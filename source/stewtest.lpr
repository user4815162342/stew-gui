program stewtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, richmemopackage, test_main, test_registry, test_test, test_sys_async,
  sys_async, test_sys_json, sys_json, stew_types, sys_file, test_sys_file,
  sys_localfile, sys_os, sys_filecache, test_sys_localfile, test_sys_filecache,
  gui_async, stew_project, test_sys_filecache_local;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

