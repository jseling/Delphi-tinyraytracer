unit uVectorTypes;

interface

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
    function Magnitude(): Single;
    function Normalize(): TVector3f;
    function Reflect(_ANormal: TVector3f): TVector3f;
    function Refract(_AN: TVector3f; eta_t: Single; eta_i: Single = 1): TVector3f;
  end;

  TVector4f = record
    X: Single;
    Y: Single;
    Z: Single;
    W: Single;
    constructor Create(_AX, _AY, _AZ, _AW: Single);
  end;

implementation

uses
  System.Math, System.Math.Vectors;

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

function TVector3f.Magnitude(): Single;
begin
  Result := Sqrt(X * X +
                 Y * Y +
                 Z * Z);
end;

function TVector3f.Normalize(): TVector3f;
var
  AMag: Single;
begin
  AMag := Self.Magnitude();
  Result := TVector3f.Create(X / AMag,
                             Y / AMag,
                             Z / AMag);
end;

function TVector3f.Reflect(_ANormal: TVector3f): TVector3f;
var
  AIDotN: Single;
  ANScale2: TVector3f;
begin
  //Result := I - N*2.f*(I*N);
  ANScale2 := _ANormal.Scale(2);
  AIDotN := Self.DotProduct(_ANormal);

  Result := Self.Subtract(ANScale2.Scale(AIDotN));
end;

function TVector3f.Refract(_AN: TVector3f;
  eta_t: Single; eta_i: Single = 1): TVector3f;
var
  cosi: Single;
  eta: Single;
  k: Single;
begin
  cosi := Max(-1, Min(1, Self.DotProduct(_AN))) * (-1);

  if (cosi < 0) then
  begin
    result := Self.Refract(_AN.Scale(-1), eta_i, eta_t);
    exit;
  end;

  eta := eta_i / eta_t;
  k := 1 - eta * eta * (1 - cosi * cosi);

  if (k < 0) then
    result := TVector3f.Create(1, 0, 0)
  else
    result := Self.Scale(eta).Add(_AN.Scale(eta * cosi - sqrt(k)));
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

{ TVector4f }

constructor TVector4f.Create(_AX, _AY, _AZ, _AW: Single);
begin
  X := _AX;
  Y := _AY;
  Z := _AZ;
  W := _AW;
end;

end.
