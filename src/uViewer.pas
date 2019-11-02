unit uViewer;

interface

uses
  Vcl.Graphics,
  uVectorTypes;

type
  TViewer = class
  private
    FHeight: Integer;
    FWidth: Integer;
    FFrameBuffer: TArray<TVector3f>;
    FZBuffer: TArray<Single>;
    function GetColor(_AVectorColor: TVector3f): TColor;
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
    function ToBitmap(_AFlipVertical: Boolean = False): Vcl.Graphics.TBitmap;
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

function TViewer.GetColor(_AVectorColor: TVector3f): TColor;
var
  AR, AG, AB: Integer;

  function FixMaxRange(_AValue: Integer): Integer;
  begin
    Result := _AValue;
    if _AValue > 255 then
      Result := 255;
  end;

  function FixMinRange(_AValue: Integer): Integer;
  begin
    Result := _AValue;
    if _AValue < 0 then
      Result := 0;
  end;

begin
  AR := Trunc(_AVectorColor.x * 255);
  AG := Trunc(_AVectorColor.y * 255);
  AB := Trunc(_AVectorColor.Z * 255);

  AR := FixMaxRange(AR);
  AR := FixMinRange(AR);

  AG := FixMaxRange(AG);
  AG := FixMinRange(AG);

  AB := FixMaxRange(AB);
  AB := FixMinRange(AB);

  Result := RGB(AR, AG, AB);
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

function TViewer.ToBitmap(_AFlipVertical: Boolean = False): Vcl.Graphics.TBitmap;
type
  TRGBTripleArray = ARRAY [Word] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray; // use a PByteArray for pf8bit color
var
  x, y: Integer;
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
        if _AFlipVertical then
          AColor := GetPixel(x, Height - y)
        else  
          AColor := GetPixel(x, y);

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
