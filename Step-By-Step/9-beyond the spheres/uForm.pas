unit uForm;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.Forms, Vcl.Graphics,
  Vcl.Controls, Vcl.ExtCtrls, Math, uRaytracer;

type
  TForm1 = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer; _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>);
    function CastRay(_AOrig, _ADir: TVector3f; _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>; _ADepth: integer): TVector3f;
    function SceneIntersect(_AOrig, _ADir: TVector3f; _ASpheres: TArray<TSphere>;
  out hit: TVector3f; out n: TVector3f; out material: TMaterial): boolean;
  public
    { Public declarations }
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  ABitmap: TBitmap;
  ivory, red_rubber, mirror, glass: TMaterial;
  ASpheres: TArray<TSphere>;
  ALights: TArray<TLight>;
begin
  ABitmap := TBitmap.Create;
  try
    ABitmap.Height := 480;
    ABitmap.Width := 640;
//    ABitmap.Height := 768;
//    ABitmap.Width := 1024;
//    ABitmap.Height := 3000;
//    ABitmap.Width := 4000;


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
    ASpheres[0] := TSphere.Create(TVector3f.Create(-3, 0, -16), 2, ivory);
    ASpheres[1] := TSphere.Create(TVector3f.Create(-1.0, -1.5, -12), 2, glass);
    ASpheres[2] := TSphere.Create(TVector3f.Create( 1.5, -0.5, -18), 3, red_rubber);
    ASpheres[3] := TSphere.Create(TVector3f.Create(7,    5,   -18), 4, mirror);

//    SetLength(ASpheres, 1);
//    ASpheres[0] := TSphere.Create(TVector3f.Create(-1.0, -1.5, -12), 2, mirror);


    SetLength(ALights, 3);
    ALights[0] := TLight.Create(TVector3f.Create(-20, 20, 20), 1.5);
    ALights[1] := TLight.Create(TVector3f.Create(30, 50, -25), 1.8);
    ALights[2] := TLight.Create(TVector3f.Create(30, 20, 30), 1.7);

    Render(ABitmap.Canvas, ABitmap.Width, ABitmap.Height, ASpheres, ALights);
    ABitmap.SaveToFile('c:\tmp\render.bmp');
    Image.Picture.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TForm1.Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer; _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>);
var
  j, i: Integer;
  x, y: Single;
  AFOV: Single;
  Adir: Tvector3f;
  AColor: TVector3f;
begin
  AFOV := Pi/2;
  for j := 0 to _AHeight - 1 do
    for i := 0 to _AWidth - 1 do
    begin
      //_ACanvas.Pixels[i, j] := GetColor(j/_AHeight, i/_AWidth, 0);

      x :=  (2*(i + 0.5)/_AWidth  - 1)*tan(AFOV/2)*_AWidth/_AHeight;
      y := -(2*(j + 0.5)/_AHeight - 1)*tan(AFOV/2);

      Adir := TVector3f.Create(x, y, -1).Normalize;

      AColor := CastRay(TVector3f.Create(0,0,0), Adir, _ASpheres, _ALights, 0);

      _ACanvas.Pixels[i, j] := GetColor(AColor);
    end;
end;

function TForm1.SceneIntersect(_AOrig, _ADir: TVector3f; _ASpheres: TArray<TSphere>;
  out hit: TVector3f; out n: TVector3f; out material: TMaterial): boolean;
var
  ASphereDist: Single;
  ADist_i: Single;
  i: integer;
  checkerboard_dist: Single;
  d: Single;
  pt: TVector3f;
begin
  ASphereDist := MaxInt;
  for i := 0 to Length(_ASpheres) - 1 do
  begin
    if (_ASpheres[i].RayIntersect(_AOrig, _ADir, ADist_i) and (ADist_i < ASphereDist)) then
    begin
      ASphereDist := ADist_i;
      hit := _AOrig.Add(_ADir.Scale(ADist_i));
      N := hit.Subtract(_ASpheres[i].center).normalize();
      material := _ASpheres[i].material;
    end;
  end;

  checkerboard_dist := MaxInt;

  if (Abs(_ADir.y)> 0.001) then
  begin
    d := -(_AOrig.y+4)/_ADir.y; // the checkerboard plane has equation y = -4
    pt := _AOrig.Add(_ADir.Scale(d));
    if (d>0) and (abs(pt.x)<10) and (pt.z<-10) and (pt.z>-30) and (d<ASphereDist) then
    begin
      checkerboard_dist := d;
      hit := pt;
      N := TVector3f.Create(0,1,0);

      material.Initialize;
      if ((trunc(0.5*hit.x+1000) + trunc(0.5*hit.z)) and 1) = 1 then
        material.DiffuseColor := TVector3f.Create(0.3, 0.3, 0.3)
      else
        material.DiffuseColor := TVector3f.Create(0.3, 0.2, 0.1);
    end;
  end;

  result := Min(ASphereDist, checkerboard_dist) < 1000;
end;

function TForm1.CastRay(_AOrig, _ADir: TVector3f; _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>; _ADepth: integer): TVector3f;
var
  point, N: TVector3f;
  material: TMaterial;
  diffuse_light_intensity: Single;
  specular_light_intensity: Single;
  i: integer;
  light_dir: TVector3f;
  diffuse_color: Tvector3f;
  specular_color: TVector3f;
  light_distance: Single;
  shadow_orig: TVector3f;

  shadow_pt, shadow_N: TVector3f;
  tmpmaterial: TMaterial;

  reflect_dir: TVector3f;
  reflect_orig: TVector3f;
  reflect_color: TVector3f;

  ANewDepth: Integer;

  refract_dir: TVector3f;
  refract_orig: TVector3f;
  refract_color: TVector3f;
begin
  if (_ADepth > 4) or (not SceneIntersect(_AOrig, _ADir, _ASpheres, point, N, material)) then
  begin
    Result := TVector3f.Create(0.2, 0.7, 0.8); // background color
    exit;
  end;

  reflect_dir := _ADir.Reflect(N).Normalize;
  refract_dir := _ADir.Refract(N, material.RefractiveIndex).Normalize;

  if reflect_dir.DotProduct(N) < 0 then
		reflect_orig := point.Subtract(N.Scale(0.001))
	 else
		reflect_orig := point.Add(N.Scale(0.001));

  if refract_dir.DotProduct(N) < 0 then
		refract_orig := point.Subtract(N.Scale(0.001))
	 else
		refract_orig := point.Add(N.Scale(0.001));

  ANewDepth := _ADepth + 1;
  reflect_color := CastRay(reflect_orig, reflect_dir, _ASpheres, _ALights, ANewDepth);
  refract_color := CastRay(refract_orig, refract_dir, _ASpheres, _ALights, ANewDepth);

  diffuse_light_intensity := 0;
  specular_light_intensity := 0;

  for i := 0 to Length(_ALights) -1 do
  begin
    light_dir := _ALights[i].position.Subtract(point).normalize;

    light_distance := _ALights[i].position.Subtract(point).Magnitude;

    if light_dir.DotProduct(N) < 0 then
      shadow_orig := point.Subtract(N.Scale(0.001))
    else
      shadow_orig := point.Add(N.Scale(0.001));

    if (SceneIntersect(shadow_orig, light_dir, _ASpheres, shadow_pt, shadow_N, tmpmaterial) and
       (shadow_pt.Subtract(shadow_orig).Magnitude < light_distance)) then
        continue;

    diffuse_light_intensity := diffuse_light_intensity +
      _ALights[i].intensity * Max(0, light_dir.DotProduct(N));

    specular_light_intensity := specular_light_intensity +
      Power(Max(0, light_dir.Scale(-1).Reflect(N).Scale(-1).DotProduct(_ADir)),
        material.SpecularExponent) * _ALights[i].intensity;
  end;

  diffuse_color := material.DiffuseColor.Scale(diffuse_light_intensity * material.albedo.X);
  specular_color := TVector3f.Create(1, 1, 1).Scale(specular_light_intensity * material.albedo.Y);
  reflect_color := reflect_color.Scale(material.albedo.Z);
  refract_color := refract_color.Scale(material.albedo.W);

  result := diffuse_color.Add(specular_color).Add(reflect_color).Add(refract_color);
end;
end.
