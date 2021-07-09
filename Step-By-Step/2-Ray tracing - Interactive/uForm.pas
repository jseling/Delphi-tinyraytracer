unit uForm;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, Vcl.Forms, Vcl.Graphics,
  Vcl.Controls, Vcl.ExtCtrls, Math, uRaytracer, Vcl.ComCtrls, DateUtils,

  System.Threading,

  uViewer;

type
  TRGBTripleArray = ARRAY[Word] of TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray; // use a PByteArray for pf8bit color

  TForm1 = class(TForm)
    Image: TImage;
    TrackBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    { Private declarations }
    FViewer: TViewer;
    procedure Render(_AViewer: TViewer; _ASphere: TSphere);
    function CastRay(_AOrig, _ADir: TVector3f; _ASphere: TSphere): TVector3f;
    procedure SetColorToPixel(_APixel: PRGBTriple; _AColor: TColor);
  public
    { Public declarations }
  end;


var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FViewer := TViewer.Create(480, 640);
  Image.Picture.Bitmap.Height := 480;
  Image.Picture.Bitmap.Width := 640;
  Image.Picture.Bitmap.PixelFormat := pf24Bit;
  TrackBarChange(nil);
end;

procedure TForm1.Render(_AViewer: TViewer; _ASphere: TSphere);
var
  AFOV: Single;
  AWidth, AHeight: Integer;

  slices: integer;

 // AScanLine: pRGBTripleArray;
  //FSessaoCritica : TCriticalSection;
//  AMonitor: System.TMonitor;

//  corfrag: single;
begin
  AWidth := _AViewer.Width;
  AHeight := _AViewer.Height;

  AFOV := Pi/2;

//  for var j := 0 to AHeight - 1 do
//  begin
//    //AScanLine := _ABitmap.ScanLine[j];
//    for var i := 0 to AWidth - 1 do
//    begin
//      x :=  (2*(i + 0.5)/AWidth  - 1)*tan(AFOV/2)*AWidth/AHeight;
//      y := -(2*(j + 0.5)/AHeight - 1)*tan(AFOV/2);
//
//      Adir := TVector3f.Create(x, y, -1).Normalize;
//
//      AColor := CastRay(TVector3f.Create(0,0,0), Adir, _ASphere);
//      _AViewer.SetPixel(i, j, AColor);
//      //SetColorToPixel(@AScanLine[i], AColor);
//    end;
//  end;
//          exit;

//  for j := 0 to AHeight - 1 do
//  begin
//    AScanLine := _ABitmap.ScanLine[j];
//    TParallel.For(0, AWidth - 1,
//                procedure(i: integer)
//                begin
//                  x :=  (2*(i + 0.5)/AWidth  - 1)*tan(AFOV/2)*AWidth/AHeight;
//                  y := -(2*(j + 0.5)/AHeight - 1)*tan(AFOV/2);
//
//                  Adir := TVector3f.Create(x, y, -1).Normalize;
//
//                  AColor := CastRay(TVector3f.Create(0,0,0), Adir, _ASphere);
//
//                  SetColorToPixel(@AScanLine[i], AColor);
//                end);
//  end;

//FSessaoCritica := TCriticalSection.Create;

slices:=32;
  TParallel.For(1, slices,
  procedure(k: integer)
  var
    sliceIni, sliceFin: integer;

    x, y: Single;

    Adir: Tvector3f;

    AColor: TVector3f;
    corfrag: single;
  begin
//       1-120
//       121-240
//       241-360
//       361-480
      sliceIni := ((AHeight div slices) * k) - ((AHeight div slices) - 1);
      sliceFin := (AHeight div slices) * k;


      for var j := (sliceIni - 1) to (sliceFin - 1) do
      begin
        //AScanLine := _ABitmap.ScanLine[j];
        for var i := 0 to AWidth - 1 do
        begin
          x :=  (2*(i + 0.5)/AWidth  - 1)*tan(AFOV/2)*AWidth/AHeight;
          y := -(2*(j + 0.5)/AHeight - 1)*tan(AFOV/2);

          Adir := TVector3f.Create(x, y, -1).Normalize;

          AColor := CastRay(TVector3f.Create(0,0,0), Adir, _ASphere);

          corfrag := k/slices;

          AColor := AColor.Scale(corfrag );

//          AColor := TVector3f.Create(corfrag, corfrag, corfrag);

//          AColor := TVector3f.Create(k/slices, k/slices, k/slices);

          //FSessaoCritica.Enter;
//          AMonitor.Enter(_AViewer);
//          try
            _AViewer.SetPixel(i, j, AColor);
//          finally
//            AMonitor.Exit(_AViewer);
            //FSessaoCritica.Leave;
//          end;

          //SetColorToPixel(@AScanLine[i], AColor);
        end;
      end;

  end);



end;

procedure TForm1.SetColorToPixel(_APixel: PRGBTriple; _AColor: TColor);
begin
  _APixel.rgbtRed := GetRValue(_AColor);
  _APixel.rgbtGreen := GetGValue(_AColor);
  _APixel.rgbtBlue := GetBValue(_AColor);
end;

procedure TForm1.TrackBarChange(Sender: TObject);
var
  ABitmap: TBitmap;
var
  AStart, AEnd: TDateTime;
begin
  AStart := now;


  Render(FViewer, TSphere.Create(TVector3f.Create(TrackBar.Position, 0, -16), 2));

  ABitmap := FViewer.ToBitmap;
  try
    Image.Picture.Bitmap.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
//  FViewer.SaveToBitmap('c:\tmp\ult.bmp');

  Image.Invalidate;


  AEnd := now;
  caption := IntToStr(MillisecondsBetween(AStart, AEnd)) + ' ms';
end;

function TForm1.CastRay(_AOrig, _ADir: TVector3f; _ASphere: TSphere): TVector3f;
var
  ASphereDist: Single;
begin
  ASphereDist := MaxInt;

  if (not _ASphere.RayIntersect(_AOrig, _ADir, ASphereDist)) then
  begin
    Result := TVector3f.Create(0.2, 0.7, 0.8); // background color
    exit;
  end;

  Result := TVector3f.Create(0.4, 0.4, 0.3);
end;
end.
