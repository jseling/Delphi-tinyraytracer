program Raytracer;

uses
  Vcl.Forms,
  uForm in 'uForm.pas' {Form1},
  uRaytracer in 'uRaytracer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
