program Raytracer;

uses
  Vcl.Forms,
  uForm in 'uForm.pas' {MainForm},
  uRaytracer in 'uRaytracer.pas',
  uViewer in 'uViewer.pas',
  uVectorTypes in 'uVectorTypes.pas',
  uSceneElements in 'uSceneElements.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
