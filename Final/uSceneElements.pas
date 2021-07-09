unit uSceneElements;

interface

uses
  uVectorTypes;

type
  TLight = record
    Position: TVector3f;
    Intensity: Single;
    constructor Create(_APosition: TVector3f; _AIntensity: Single);
  end;

  TMaterial = record
    DiffuseColor: TVector3f;
    Albedo: TVector4f;
    SpecularExponent: Single;
    RefractiveIndex: Single;
    procedure Initialize;
    function CalculateDiffuseColorInScene(const _ADiffuseLightIntensity: Single): TVector3f;
    function CalculateSpecularColorInScene(const _ASpecularLightIntensity: Single): TVector3f;
    function CalculateReflectColorInScene(const _AReflectColor: TVector3f): TVector3f;
    function CalculateRefractColorInScene(const _ARefractColor: TVector3f): TVector3f;
  end;

  TSphere = record
    Center: TVector3f;
    Radius: Single;
    Material: TMaterial;

    constructor Create(_ACenter: TVector3f; _ARadius: Single; _AMaterial: TMaterial);
    function RayIntersect(_AOrig, _ADir: TVector3f; out _At0: Single): Boolean;
  end;

implementation

{ TSphere }

constructor TSphere.Create(_ACenter: TVector3f; _ARadius: Single; _AMaterial: TMaterial);
begin
  Center := _ACenter;
  Radius := _ARadius;
  Material := _AMaterial;
end;

function TSphere.RayIntersect(_AOrig, _ADir: TVector3f; out _At0: Single): Boolean;
var
  AL: TVector3f;
  Atca: Single;
  Ad2: Single;
  Athc: Single;
  At1: Single;
begin
  AL := Center.Subtract(_AOrig);
  Atca := AL.DotProduct(_ADir);
  Ad2 := AL.DotProduct(AL) - (Atca * Atca);

  if (Ad2 > Radius * Radius) then
  begin
    Result := False;
    exit;
  end;

  Athc := Sqrt(Radius * Radius - Ad2);
  _At0 := Atca - Athc;
  At1 := Atca + Athc;

  if (_At0 < 0) then
    _At0 := At1;

  if (_At0 < 0) then
  begin
    Result := False;
    exit;
  end;

  Result := True;
end;

{ TLight }

constructor TLight.Create(_APosition: TVector3f; _AIntensity: Single);
begin
  Position := _APosition;
  Intensity := _AIntensity;
end;

{ TMaterial }

function TMaterial.CalculateDiffuseColorInScene(const _ADiffuseLightIntensity: Single): TVector3f;
begin
  Result := Self.DiffuseColor.Scale(_ADiffuseLightIntensity * Self.Albedo.X)
end;

function TMaterial.CalculateReflectColorInScene(const _AReflectColor: TVector3f): TVector3f;
begin
  Result := _AReflectColor.Scale(Self.Albedo.Z);
end;

function TMaterial.CalculateRefractColorInScene(const _ARefractColor: TVector3f): TVector3f;
begin
  Result := _ARefractColor.Scale(Self.Albedo.W);
end;

function TMaterial.CalculateSpecularColorInScene(const _ASpecularLightIntensity: Single): TVector3f;
begin
  Result := TVector3f.Create(1, 1, 1).Scale(_ASpecularLightIntensity * Self.Albedo.Y);
end;

procedure TMaterial.Initialize;
begin
  DiffuseColor := TVector3f.Create(0.0, 0.0, 0.0);
  Albedo := TVector4f.Create(1, 0, 0, 0);
  SpecularExponent := 50;
  RefractiveIndex := 1;
end;

end.
