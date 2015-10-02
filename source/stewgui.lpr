program stewgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, gui_mainform, stew_project, sys_file, sys_async, gui_projectmanager,
  gui_config, sys_json, gui_documenteditor, gui_preferenceseditor,
  gui_projectsettingseditor, gui_editorframe, stew_properties,
  sys_types, gui_jsoneditor, sys_os, gui_listdialog,
sys_localfile, sys_versionsupport, gui_async, sys_filecache;

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

  Application.Title:='Stew';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

