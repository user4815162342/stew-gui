program stewgui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, stewmainform, stewproject, stewfile,
  stewasync, stewprojectinspector, stewconfig, stewjson, stewdocumenteditor,
  stewpreferenceseditor, stewprojectsettingseditor, steweditorframe, stewabout,
  stewproperties, stewtypes, stewpersist, stewjsoneditor;

{$R *.res}

begin
  Application.Title:='stew-gui';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

