unit uForm;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Controls;

type
  TMainForm = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  Vcl.Graphics,
  DateUtils,
  uRaytracer,
  uSceneElements,
  uViewer,
  uVectorTypes;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  ivory, red_rubber, mirror, glass: TMaterial;
  ASpheres: TArray<TSphere>;
  ALights: TArray<TLight>;
  AViewer: TViewer;

  AStart, AEnd: TTime;
begin
//    480, 640   = ~740;
//    768, 1024  = ~1800;
//    720, 1280  = ~1750   //HD
//    768, 1366  = ~1900
//    1080, 1920 = ~3500   //Full HD
//    1500, 2000 = ~6200 / ~2900-3700
//    2160, 3840 = ~13900  //4k
//    3000, 4000 = ~24700;

  AViewer := TViewer.Create(480, 640);
  try
    ivory.DiffuseColor := TVector3f.Create(0.4, 0.4, 0.3);
    ivory.Albedo := TVector4f.Create(0.6, 0.3, 0.1, 0.0);
    ivory.SpecularExponent := 50;
    ivory.RefractiveIndex := 1;

    red_rubber.DiffuseColor := TVector3f.Create(0.3, 0.1, 0.1);
    red_rubber.Albedo := TVector4f.Create(0.9, 0.1, 0.0, 0.0);
    red_rubber.SpecularExponent := 10;
    red_rubber.RefractiveIndex := 1;

    mirror.DiffuseColor := TVector3f.Create(1, 1, 1);
    mirror.Albedo := TVector4f.Create(0.0, 10.0, 0.8, 0.0);
    mirror.SpecularExponent := 1425;
    mirror.RefractiveIndex := 1;

    glass.DiffuseColor := TVector3f.Create(0.6, 0.7, 0.8);
    glass.Albedo := TVector4f.Create(0.0, 0.5, 0.1, 0.8);
    glass.SpecularExponent := 125;
    glass.RefractiveIndex := 1.5;

    SetLength(ASpheres, 4);
    ASpheres[0] := TSphere.Create(TVector3f.Create(-3,    0,   -16), 2, ivory);
    ASpheres[1] := TSphere.Create(TVector3f.Create(-1.0, -1.5, -12), 2, glass);
    ASpheres[2] := TSphere.Create(TVector3f.Create( 1.5, -0.5, -18), 3, red_rubber);
    ASpheres[3] := TSphere.Create(TVector3f.Create( 7,    5,   -18), 4, mirror);

    SetLength(ALights, 3);
    ALights[0] := TLight.Create(TVector3f.Create(-20, 20,  20), 1.5);
    ALights[1] := TLight.Create(TVector3f.Create( 30, 50, -25), 1.8);
    ALights[2] := TLight.Create(TVector3f.Create( 30, 20,  30), 1.7);


    ////////////////////////////////////////////////////////////////////////////
    AStart := now;
    TRaytracer.RenderPPL(AViewer, ASpheres, ALights);
    AEnd := now;
    ////////////////////////////////////////////////////////////////////////////

    caption := MillisecondsBetween(AStart, AEnd).ToString + ' ms';

    Image.Picture.Assign(AViewer.ToBitmap);
    AViewer.SaveToBitmap('render '+caption+'.bmp');
  finally
    AViewer.Free;
  end;
end;

end.
