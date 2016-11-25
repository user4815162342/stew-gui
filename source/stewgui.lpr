program stewgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, gui_mainform, stew_project, gui_projectmanager, gui_config,
  gui_documenteditor, gui_preferenceseditor, gui_projectsettingseditor,
  gui_editorframe, stew_properties, stew_properties_implementation,
  gui_config_implementation, gui_async, test_sys_file,
  sys_dynval_implementation, gui_jsoneditor, sys_dynval, test_sys_async,
  sys_filecache, gui_gtk2_glyphs, test_sys_filecache_local, sys_log,
  test_sys_localfile, gui_listdialog, sys_types, sys_versionsupport,
  sys_dynval_data, test_sys_dynval, gui_glyphs, sys_os, sys_dynval_json,
  sys_file, sys_dynval_data_implementation, test_sys_filecache, sys_localfile,
  sys_async, test_sys_types;

{$R *.res}

begin
  {$IFNDEF Windows}
  // Windows won't have a console to work with, so I'll just get
  // an error here.
  if ParamStr(1) = VersionArgument then
  begin
    WriteLn(sys_versionsupport.GetFileVersion);
    Exit;
  end;
  {$ENDIF}
  // Set the output for the heap trace at the end...
  // This is really just for memory leaks. It depends on
  // -gh option being set in compiler.
  // I can find no way to ifdef whether that option is being
  // used, so if that gets turned off, this will have to
  // be commented out.
  SetHeapTraceOutput(InitializeLog('heap.trc',true));

  Application.Title:='Stew';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

