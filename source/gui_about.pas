unit gui_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CloseButton: TButton;
    TitleLabel: TLabel;
    BuildInfoLabel: TLabel;
    DescriptionLabel: TLabel;
    CopyrightLabel: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


var
  AboutForm: TAboutForm;

implementation

uses
  sys_versionsupport;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  // FUTURE: Make the Title a bigger font (and bold), and the version slightly bigger.
  // What I don't want to happen, though, is for it to end up using a different
  // font than the desktop theme expects. So, if I change the size, will that
  // change everything?
  TitleLabel.Caption := Application.Title  + ' v ' + sys_versionsupport.GetProductVersion;
  DescriptionLabel.Caption := sys_versionsupport.GetResourceString('FileDescription');
  CopyrightLabel.Caption := sys_versionsupport.GetResourceString('LegalCopyright');
  BuildInfoLabel.Caption := 'Executable: ' + ExtractFileNameOnly(Application.ExeName) + ' v ' + sys_versionsupport.GetFileVersion + LineEnding +
                            'Compiled On: ' + sys_versionsupport.GetCompiledDate + LineEnding +
                            'Compiler: ' + sys_versionsupport.GetCompilerInfo + LineEnding +
                            'Target: ' + sys_versionsupport.GetTargetInfo + LineEnding +
                            'OS: ' + sys_versionsupport.GetOS + LineEnding +
                            'LCL: ' + sys_versionsupport.GetLCLVersion + LineEnding +
                            'WidgetSet: ' + sys_versionsupport.GetWidgetSet;
end;

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

