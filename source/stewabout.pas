unit stewabout;

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
    VersionLabel: TLabel;
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
  VersionSupport;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  // TODO: Make the Title a bigger font (and bold), and the version slightly bigger.
  // What I don't want to happen, though, is for it to end up using a different
  // font than the desktop theme expects. So, if I change the size, will that
  // change everything?
  TitleLabel.Caption := VersionSupport.GetResourceString('ProductName');
  VersionLabel.Caption := 'version ' + VersionSupport.GetProductVersion;
  DescriptionLabel.Caption := VersionSupport.GetResourceString('FileDescription');
  CopyrightLabel.Caption := VersionSupport.GetResourceString('LegalCopyright');
  BuildInfoLabel.Caption := Application.Title + ' v ' + VersionSupport.GetFileVersion + LineEnding +
                            'Compiled On: ' + VersionSupport.GetCompiledDate + LineEnding +
                            'Compiler: ' + VersionSupport.GetCompilerInfo + LineEnding +
                            'Target: ' + VersionSupport.GetTargetInfo + LineEnding +
                            'OS: ' + VersionSupport.GetOS + LineEnding +
                            'LCL: ' + VersionSupport.GetLCLVersion + LineEnding +
                            'WidgetSet: ' + VersionSupport.GetWidgetSet;
end;

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

