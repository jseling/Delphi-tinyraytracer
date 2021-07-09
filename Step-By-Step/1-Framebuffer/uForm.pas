unit uForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetColor(R, G, B: Single): TColor;
    procedure Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);
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
    Render(ABitmap.Canvas, ABitmap.Width, ABitmap.Height);
    Image.Picture.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

function TForm1.GetColor(R, G, B: Single): TColor;
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

procedure TForm1.Render(_ACanvas: TCanvas; _AWidth, _AHeight: Integer);
var
  j, i: Integer;
begin
  for j := 0 to _AHeight - 1 do
    for i := 0 to _AWidth - 1 do
      _ACanvas.Pixels[i, j] := GetColor(j/_AHeight, i/_AWidth, 0);
end;

end.
