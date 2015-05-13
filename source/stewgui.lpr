program stewgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, gui_mainform, stew_project, sys_file, sys_async, gui_projectmanager,
  gui_config, sys_json, gui_documenteditor, gui_preferenceseditor,
  gui_projectsettingseditor, gui_editorframe, gui_about, stew_properties,
  stew_types, stew_persist, gui_jsoneditor, sys_os, gui_listdialog;

{$R *.res}

begin
  Application.Title:='Stew';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TListDialog, ListDialog);
  Application.Run;
end.

