unit uRaytracer;

interface

uses
  uVectorTypes,
  uSceneElements,
  uViewer;

type
  TRaytracer = class
  private
    class function CastRay(_AOrig, _ADir: TVector3f; _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>; _ADepth: integer): TVector3f;
    class function SceneIntersect(_AOrig, _ADir: TVector3f; _ASpheres: TArray<TSphere>; out hit: TVector3f; out n: TVector3f; out material: TMaterial): boolean;
  public
    class procedure Render(_AViewer: TViewer; _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>);
    class procedure RenderPPL(_AViewer: TViewer; _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>);
  end;

implementation

uses
  Math,
  System.Threading;

{ TRaytracer }

class function TRaytracer.CastRay(_AOrig, _ADir: TVector3f;
  _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>;
  _ADepth: integer): TVector3f;
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

  diffuse_color := material.CalculateDiffuseColorInScene(diffuse_light_intensity);
  specular_color := material.CalculateSpecularColorInScene(specular_light_intensity);
  reflect_color := material.CalculateReflectColorInScene(reflect_color);
  refract_color := material.CalculateRefractColorInScene(refract_color);

  result := diffuse_color.Add(specular_color).Add(reflect_color).Add(refract_color);
end;

class procedure TRaytracer.Render(_AViewer: TViewer; _ASpheres: TArray<TSphere>;
  _ALights: TArray<TLight>);
var
  j: Integer;
  i: integer;
  x, y: Single;
  AFOV: Single;
  Adir: Tvector3f;
  AColor: TVector3f;
begin
  AFOV := Pi/2;
  for j := 0 to _AViewer.Height - 1 do
    for i := 0 to _AViewer.Width - 1 do
    begin
      x :=  (2*(i + 0.5)/_AViewer.Width  - 1)*tan(AFOV/2)*_AViewer.Width/_AViewer.Height;
      y := -(2*(j + 0.5)/_AViewer.Height - 1)*tan(AFOV/2);

      Adir := TVector3f.Create(x, y, -1).Normalize;

      AColor := CastRay(TVector3f.Create(0,0,0), Adir, _ASpheres, _ALights, 0);

      _AViewer.SetPixel(i, j, AColor);
    end;
end;

class procedure TRaytracer.RenderPPL(_AViewer: TViewer;
  _ASpheres: TArray<TSphere>; _ALights: TArray<TLight>);
var
  AFOV: Single;
  AWidth, AHeight: Integer;
  ASlices: integer;

  //x, y: Single; //uncomment for concurrency bugs visualization
begin
  AWidth := _AViewer.Width;
  AHeight := _AViewer.Height;

  AFOV := Pi/2;


  ASlices := 8;

  TParallel.For(1, ASlices,
    procedure(k: integer)
    var
      ASliceIni,
      ASliceFin: integer;

      x, y: Single; //comment for concurrency bugs visualization
      i, j: Integer;

      xD, yD: Single;

      s: integer;
      sNum: integer;
      Adir: Tvector3f;

      AColor: TVector3f;
      AFinalColor: TVector3f;
//      ACorfrag: single; //for threads responsibilities visualization
    begin
        ASliceIni := ((AHeight div ASlices) * k) - ((AHeight div ASlices) - 1);
        ASliceFin := (AHeight div ASlices) * k;

        sNum := 4;

        for j := (ASliceIni - 1) to (ASliceFin - 1) do
          for i := 0 to AWidth - 1 do
          begin
            x :=  (2*(i + 0.5)/AWidth  - 1)*tan(AFOV/2)*AWidth/AHeight;
            y := -(2*(j + 0.5)/AHeight - 1)*tan(AFOV/2);

            for s := 0 to sNum - 1 do
            begin
              Randomize;
              xD := ((Random - 0.5) * 0.00001);
              yD := ((Random - 0.5) * 0.00001);

              Adir := TVector3f.Create(x + xD,
                                       y + yD,
                                      -1).Normalize;

              if s = 0 then
                AFinalColor := CastRay(TVector3f.Create(0,0,0), Adir, _ASpheres, _ALights, 0)
              else
              begin
                AColor := CastRay(TVector3f.Create(0,0,0), Adir, _ASpheres, _ALights, 0);
                AFinalColor := AFinalColor.Add(AColor);
              end;
            end;

            AFinalColor.Scale(1 / sNum);

//            ACorfrag := k / ASlices;
//            AColor := AColor.Scale(ACorfrag );

            _AViewer.SetPixel(i, j, AFinalColor);
          end;
    end);
end;

class function TRaytracer.SceneIntersect(_AOrig, _ADir: TVector3f;
  _ASpheres: TArray<TSphere>; out hit, n: TVector3f;
  out material: TMaterial): boolean;
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

end.
