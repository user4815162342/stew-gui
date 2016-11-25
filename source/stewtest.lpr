program stewtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, richmemopackage, stew_project, test_stew_project, stew_properties,
  test_stew_properties, test_gui, stew_query, stew_properties_implementation,
  test_test, test_sys_types, test_sys_localfile, test_sys_filecache_local,
  test_sys_filecache, test_sys_file, test_sys_dynval, test_sys_async,
  test_registry, test_longstringmap, test_configuration, sys_async, sys_dynval,
  sys_file, sys_filecache, sys_localfile, sys_types, test_main;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

