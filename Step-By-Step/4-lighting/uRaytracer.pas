unit uRaytracer;

interface

uses
  Vcl.Graphics;

type
  TVector3f = record
    X: Single;
    Y: Single;
    Z: Single;

    constructor Create(_AX, _AY, _AZ: Single);
    function Add(_AVector: TVector3f): TVector3f;
    function Subtract(_AVector: TVector3f): TVector3f;
    function Scale(_AFactor: Single): TVector3f;
    function DotProduct(_AVector: TVector3f): Single;
    function Magnitude: Single;
    function Normalize: TVector3f;
  end;

  TLight = record
    Position: TVector3f;
    Intensity: Single;
    constructor Create(_APosition: TVector3f; _AIntensity: Single);
  end;

  TMaterial = record
    DiffuseColor: TVector3f; //uma cor deve ser um tcolor
  end;

  TSphere = record
    Center: TVector3f;
    Radius: Single;
    Material: TMaterial;

    constructor Create(_ACenter: TVector3f; _ARadius: Single; _AMaterial: TMaterial);
    function RayIntersect(_AOrig, _ADir: TVector3f; out _At0: Single): Boolean;
  end;

  function GetColor(R, G, B: Single): TColor; overload;
  function GetColor(_AVectorColor: TVector3f): TColor; overload; //Remover isto

implementation

uses
  Winapi.Windows, System.SysUtils, Math;

function GetColor(R, G, B: Single): TColor;
var
  AR,
  AG,
  AB: Byte;
begin
  if (R < 0) or (R > 1) or
     (G < 0) or (G > 1) or
     (B < 0) or (B > 1) then
     raise Exception.Create('Invalid parameter.');

  AR := Trunc(R * 255);
  AG := Trunc(G * 255);
  AB := Trunc(B * 255);
  Result := RGB(AR, AG, AB);
end;

function GetColor(_AVectorColor: TVector3f): TColor;
var
  AR,
  AG,
  AB: Byte;
begin
  AR := Trunc(_AVectorColor.X * 255);
  AG := Trunc(_AVectorColor.Y * 255);
  AB := Trunc(_AVectorColor.Z * 255);

  if (AR < 0) or (AR > 255) or
     (AG < 0) or (AG > 255) or
     (AB < 0) or (AB > 255) then
     raise Exception.Create('Invalid parameter.');

  Result := RGB(AR, AG, AB);
end;

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

{ TVector3f }

function TVector3f.Add(_AVector: TVector3f): TVector3f;
begin
  Result := TVector3f.Create(X + _AVector.X,
                             Y + _AVector.Y,
                             Z + _AVector.Z);
end;

constructor TVector3f.Create(_AX, _AY, _AZ: Single);
begin
  X := _AX;
  Y := _AY;
  Z := _AZ;
end;

function TVector3f.DotProduct(_AVector: TVector3f): Single;
begin
  Result := X * _AVector.X +
            Y * _AVector.Y +
            Z * _AVector.Z;
end;

function TVector3f.Magnitude: Single;
begin
  Result := Sqrt(X * X +
                 Y * Y +
                 Z * Z);
end;

function TVector3f.Normalize: TVector3f;
var
  AMag: Single;
begin
  AMag := Magnitude;
  Result := TVector3f.Create(X / AMag,
                             Y / AMag,
                             Z / AMag);
end;

function TVector3f.Scale(_AFactor: Single): TVector3f;
begin
  Result := TVector3f.Create(X * _AFactor,
                             Y * _AFactor,
                             Z * _AFactor);
end;

function TVector3f.Subtract(_AVector: TVector3f): TVector3f;
begin
  Result := TVector3f.Create(X - _AVector.X,
                             Y - _AVector.Y,
                             Z - _AVector.Z);
end;

{ TLight }

constructor TLight.Create(_APosition: TVector3f; _AIntensity: Single);
begin
  Position := _APosition;
  Intensity := _AIntensity;
end;

end.
