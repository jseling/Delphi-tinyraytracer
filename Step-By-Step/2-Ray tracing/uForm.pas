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
    procedure Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer;_ASphere: TSphere);
    function CastRay(_AOrig, _ADir: TVector3f; _ASphere: TSphere): TColor;
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
begin
  ABitmap := TBitmap.Create;
  try
    ABitmap.Height := 480;
    ABitmap.Width := 640;
    Render(ABitmap.Canvas, ABitmap.Width, ABitmap.Height, TSphere.Create(TVector3f.Create(-3, 0, -16), 2));
    Image.Picture.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TForm1.Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer; _ASphere: TSphere);
var
  j, i: Integer;
  x, y: Single;
  AFOV: Single;
  Adir: Tvector3f;
begin
  AFOV := Pi/2;
  for j := 0 to _AHeight - 1 do
    for i := 0 to _AWidth - 1 do
    begin
      _ACanvas.Pixels[i, j] := GetColor(j/_AHeight, i/_AWidth, 0);

      x :=  (2*(i + 0.5)/_AWidth  - 1)*tan(AFOV/2)*_AWidth/_AHeight;
      y := -(2*(j + 0.5)/_AHeight - 1)*tan(AFOV/2);

      Adir := TVector3f.Create(x, y, -1).Normalize;
      _ACanvas.Pixels[i, j] := CastRay(TVector3f.Create(0,0,0), Adir, _ASphere);

    end;
end;

function TForm1.CastRay(_AOrig, _ADir: TVector3f; _ASphere: TSphere): TColor;
var
  ASphereDist: Single;
begin
  ASphereDist := MaxInt;

  if (not _ASphere.RayIntersect(_AOrig, _ADir, ASphereDist)) then
  begin
    Result := GetColor(0.2, 0.7, 0.8); // background color
    exit;
  end;

  Result := GetColor(0.4, 0.4, 0.3);
end;
end.
