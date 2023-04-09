unit frmImageProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, LclType, ActnList;

type

  { TForm1 }

  TForm1 = class(TForm)
    actGrayScale: TAction;
    actBlur: TAction;
    actEdge: TAction;
    ActionList1: TActionList;
    btnGrayscale: TButton;
    btnOpenImage: TButton;
    btnBlur: TButton;
    btnEdgeDetection: TButton;
    btnR: TButton;
    btnRotateOrig: TButton;
    Image1: TImage;
    ImageList1: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    pnlButtons: TPanel;
    Timer1: TTimer;
    procedure actBlurExecute(Sender: TObject);
    procedure actEdgeExecute(Sender: TObject);
    procedure actGrayScaleExecute(Sender: TObject);
    procedure btnOpenImageClick(Sender: TObject);
    procedure btnRClick(Sender: TObject);
    procedure btnRotateOrigClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FiOrigBitmap : TBitmap;
    FdRotation : Double;
  public
    procedure AfterConstruction; override;

  end;

  TSiRGB = packed record
    B, G, R, A: byte;
  end;

  PSiRGB = ^TSiRGB;

  TRGBTripleArray = array[0..4095] of TSiRGB;
  PRGBTripleArray = ^TRGBTripleArray;

var
  Form1: TForm1;

implementation

uses
  LazLogger, unEdgeStats, Types;

{$R *.lfm}


{ TForm1 }



procedure TForm1.actGrayScaleExecute(Sender: TObject);
var
  x, y, avg: integer;
  AImageBitmap: TBitmap;
  Line: PRGBTripleArray;
begin

  AImageBitmap := Image1.Picture.Bitmap;
  for y := 1 to Image1.Picture.Height - 2 do
  begin
    Line := PRGBTripleArray(AImageBitmap.ScanLine[y]);
    for x := 1 to Image1.Picture.Width - 2 do
    begin
      avg := (Line^[x].r + Line^[x].g + Line^[x].b) div 3;

      Line^[x].r := avg;
      Line^[x].G := avg;
      Line^[X].B := avg;

    end;
  end;
  FiOrigBitmap.Assign(Image1.Picture.Bitmap);

end;



procedure TForm1.actBlurExecute(Sender: TObject);
var
  x, y, avg: integer;
  dConvColor: double;
  ABitmap: TBitmap;
  AImageBitmap: TBitmap;
  thisLine, priorLine, nextLine, targetLine: PRGBTripleArray;
begin
  ABitmap := TBitmap.Create;
  ABitmap.Width := Image1.Picture.Width;
  ABitmap.Height := Image1.Picture.Height;
  ABitmap.PixelFormat := Image1.Picture.Bitmap.PixelFormat;
  try
    AImageBitmap := Image1.Picture.Bitmap;
    for y := 1 to Image1.Picture.Height - 2 do
    begin

      thisLine := PRGBTripleArray(AImageBitmap.ScanLine[y]);
      priorLine := PRGBTripleArray(AImageBitmap.ScanLine[y - 1]);
      nextLine := PRGBTripleArray(AImageBitmap.ScanLine[y + 1]);

      targetLine := PRGBTripleArray(ABitmap.ScanLine[y]);
      for x := 1 to Image1.Picture.Width - 2 do
      begin

        dConvColor := (priorLine^[x - 1].R * 0.1) + (priorLine^[x].R * 0.1) +
          (priorLine^[x + 1].R * 0.1) + (thisLine^[x - 1].R * 0.1) +
          (thisLine^[x].R * 0.2) + (thisLine^[x + 1].R * 0.1) +
          (nextLine^[x - 1].R * 0.1) + (nextLine^[x].R * 0.1) +
          (nextLine^[x + 1].R * 0.1);

        avg := Round(dConvColor);
        targetLine^[x].R := avg;
        targetLine^[x].G := avg;
        targetLine^[x].B := avg;

      end;
    end;
    Image1.Picture.Pixmap.Canvas.Draw(0, 0, ABitmap);
    Image1.Refresh;
    FiOrigBitmap.Assign(Image1.Picture.Bitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TForm1.actEdgeExecute(Sender: TObject);
var
  x, y, avg: integer;
  dConvColorX, dConvColorY, dConvColor: double;
  ABitmap: TBitmap;
  AImageBitmap: TBitmap;
  thisLine, priorLine, nextLine, targetLine: PRGBTripleArray;
begin
  ABitmap := TBitmap.Create;
  ABitmap.Width := Image1.Picture.Width;
  ABitmap.Height := Image1.Picture.Height;
  ABitmap.PixelFormat := Image1.Picture.Bitmap.PixelFormat;

  try
    AImageBitmap := Image1.Picture.Bitmap;
    for y := 1 to Image1.Picture.Height - 2 do
    begin

      thisLine := PRGBTripleArray(AImageBitmap.ScanLine[y]);
      priorLine := PRGBTripleArray(AImageBitmap.ScanLine[y - 1]);
      nextLine := PRGBTripleArray(AImageBitmap.ScanLine[y + 1]);

      targetLine := PRGBTripleArray(ABitmap.ScanLine[y]);
      for x := 1 to Image1.Picture.Width - 2 do
      begin

        dConvColorX := (priorLine^[x - 1].R * 1) + (priorLine^[x + 1].R * -1) +
          (thisLine^[x - 1].R * 2) + (thisLine^[x + 1].R * -2) +
          (nextLine^[x - 1].R * 1) + (nextLine^[x + 1].R * -1);

        dConvColorY := (priorLine^[x - 1].R) + (priorLine^[x].R * 2) +
          (priorLine^[x + 1].R) + (nextLine^[x - 1].R * -1) +
          (nextLine^[x].R * -2) + (nextLine^[x + 1].R * -1);

        dConvColor := Sqrt((dConvColorX * dConvColorX) + (dConvColorY * dConvColorY));

        avg := Round(dConvColor);
        ;
        targetLine^[x].R := avg;
        targetLine^[x].G := avg;
        targetLine^[x].B := avg;

      end;
    end;
    Image1.Picture.Pixmap.Canvas.Draw(0, 0, ABitmap);
    Image1.Refresh;
    FiOrigBitmap.Assign(Image1.Picture.Bitmap);
  finally
    ABitmap.Free;
  end;
end;


procedure TForm1.btnOpenImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
  end;
end;

procedure TForm1.btnRClick(Sender: TObject);
begin
    Timer1.Enabled := not Timer1.Enabled;
end;



procedure TForm1.btnRotateOrigClick(Sender: TObject);
var
  X, Y: integer;
  ABitmap, ASourceBitmap: TBitmap;
  sourceLine, targetLine: PRGBTripleArray;
  targetX, targetY, CentreX, CentreY,deltaX, deltaY : double;
  radius, angle: double;
const
  ROTATION = 0.03;
begin
  FdRotation := FdRotation + ROTATION;
  ASourceBitmap := FiOrigBitmap;
  CentreX := ASourceBitmap.Width / 2;
  CentreY := ASourceBitmap.Height / 2;
  ABitmap := TBitmap.Create;
  ABitmap.Width := ASourceBitmap.Width;
  ABitmap.Height := ASourceBitmap.Height;
  ABitmap.PixelFormat := ASourceBitmap.PixelFormat;
  try
    for Y := 0 to pred(ASourceBitmap.Height) do
    begin
      sourceLine := ASourceBitmap.ScanLine[y];
      for X := 0 to pred(ASourceBitmap.Width) do
      begin
        deltaX := Double(X) - CentreX;
        deltaY := double(Y) - CentreY;
        radius := Sqrt(sqr(deltaX) + Sqr(deltaY));

        if (DeltaX < 0) AND (deltaY <0)then
        begin
          angle := ArcTan(deltaY / deltaX);
          targetX := -(radius * cos(angle + FdRotation)) + CentreX;
          targetY := -(radius * sin(angle + FdRotation)) + CentreY;
        end
        else if (deltaX > 0) AND (deltaY <0) then
          begin
             angle :=- ((pi/2) +  ArcTan(deltax / deltaY));
             targetX := (radius * cos(angle + FdRotation)) + CentreX;
             targetY := (radius * sin(angle + FdRotation)) + CentreY;
          end

      else if (deltaX < 0) AND (deltaY > 0) then
      begin

             angle := ArcTan(deltax / deltaY);
             targetX := (radius * sin(angle- FdRotation)) + CentreX;
             targetY := (radius * cos(angle - FdRotation)) + CentreY;

      end
      else if (deltax >0) AND (deltaY > 0) then
      begin
        angle := ArcTan(deltax / deltaY);
             targetX := (radius * sin(angle- FdRotation)) + CentreX;
             targetY := (radius * cos(angle - FdRotation)) + CentreY;
      end;



          if PtInRect(Rect(0, 0, ABitmap.Width, ABitmap.Height),
            Point(round(targetX), round(targetY))) then
          begin
            targetLine := ABitmap.ScanLine[round(targetY)];
            targetLine^[round(targetX)].R := sourceLine^[x].R;
            targetLine^[round(targetX)].G := sourceLine^[x].G;
            targetLine^[round(targetX)].B := sourceLine^[x].B;
          end;


      end;
    end;
    Image1.Picture.Pixmap.Canvas.Draw(0, 0, ABitmap);
  finally
    ABitmap.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FiOrigBitmap := TBitmap.Create;
end;

procedure TForm1.AfterConstruction;
begin
  inherited AfterConstruction;
  FiOrigBitmap.Assign(Image1.Picture.Bitmap);
end;


end.
