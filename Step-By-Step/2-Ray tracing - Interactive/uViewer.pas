unit uViewer;

interface

uses
  uRaytracer, Vcl.Graphics;

type
  TViewer = class
  private
    FHeight: Integer;
    FWidth: Integer;
    FFrameBuffer: TArray<TVector3f>;
    FZBuffer: TArray<Single>;
  public
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
    property FrameBuffer: TArray<TVector3f> read FFrameBuffer;
    property ZBuffer: TArray<Single> read FZBuffer;

    constructor Create(_AHeight, _AWidth: Integer);
    procedure SetPixel(x, y: Integer; color: TVector3f);
    function GetPixel(x, y: Integer): TVector3f;

    function GetZValue(x, y: Integer): Single;
    procedure SetZValue(x, y: Integer; _AValue: Single);

    procedure SaveToBitmap(_AFileName: String);
    function ToBitmap: Vcl.Graphics.TBitmap;
  end;

implementation

uses
  Winapi.Windows;

{ TViewer }

constructor TViewer.Create(_AHeight, _AWidth: Integer);
var
	i: Integer;
  s: Single;
begin
	FHeight := _AHeight;
	FWidth := _AWidth;
  s := MaxInt * (-1);

	SetLength(FFrameBuffer, _AWidth * _AHeight);
	SetLength(FZBuffer, _AWidth * _AHeight);

  for i := Low(FZBuffer) to High(FZBuffer) do
    FZBuffer[i] := s;
end;

function TViewer.GetPixel(x, y: Integer): TVector3f;
begin
  Result := FFrameBuffer[x + y * FWidth];
end;

function TViewer.GetZValue(x, y: Integer): Single;
begin
  Result := FZBuffer[x + y * FWidth];
end;

procedure TViewer.SaveToBitmap(_AFileName: String);
var
  ABitmap: Vcl.Graphics.TBitmap;
begin
  ABitmap := Self.ToBitmap;
  try
    ABitmap.SaveToFile(_AFileName);
  finally
    ABitmap.Free;
  end;
end;

procedure TViewer.SetPixel(x, y: Integer; color: TVector3f);
begin
  FFrameBuffer[x + y * FWidth] := color;
end;

procedure TViewer.SetZValue(x, y: Integer; _AValue: Single);
begin
  FZBuffer[x + y * FWidth] := _AValue;
end;

function TViewer.ToBitmap: Vcl.Graphics.TBitmap;
type
  TRGBTripleArray = ARRAY[Word] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray; // use a PByteArray for pf8bit color
var
  x, y: integer;
  AScanLine: pRGBTripleArray;
  AColor: TVector3f;
  ADelphiColor: TColor;
begin
  Result := Vcl.Graphics.TBitmap.Create;
  try
    Result.Height := Height;
    Result.Width := Width;
    Result.PixelFormat := pf24Bit;

     for y := 0 to Height - 1 do
      begin
        AScanLine := Result.ScanLine[y];
        for x := 0 to Width - 1 do
        begin
          AColor := GetPixel(x, Height - y);

          ADelphiColor := GetColor(AColor);

          AScanLine[x].rgbtRed := GetRValue(ADelphiColor);
          AScanLine[x].rgbtGreen := GetGValue(ADelphiColor);
          AScanLine[x].rgbtBlue := GetBValue(ADelphiColor);
        end;
      end;
  except
    Result.Free;
    raise;
  end;
end;

end.

