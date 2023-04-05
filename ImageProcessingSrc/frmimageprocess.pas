unit frmImageProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, LclType;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnGrayscale: TButton;
    btnOpenImage: TButton;
    btnBlur: TButton;
    btnEdgeDetection: TButton;
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    pnlButtons: TPanel;
    procedure btnBlurClick(Sender: TObject);
    procedure btnEdgeDetectionClick(Sender: TObject);
    procedure btnGrayscaleClick(Sender: TObject);
    procedure btnOpenImageClick(Sender: TObject);
  private

  public

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


{$R *.lfm}


{ TForm1 }




procedure TForm1.btnGrayscaleClick(Sender: TObject);
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

end;

procedure TForm1.btnBlurClick(Sender: TObject);
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

        dConvColor := (priorLine^[x - 1].R * 0.1) +
          (priorLine^[x].R * 0.1) +
          (priorLine^[x + 1].R * 0.1) +
          (thisLine^[x - 1].R * 0.1) +
          (thisLine^[x].R * 0.2) +
          (thisLine^[x + 1].R * 0.1) +
          (nextLine^[x - 1].R * 0.1) +
          (nextLine^[x].R * 0.1) +
          (nextLine^[x + 1].R * 0.1);

        avg := Round(dConvColor);
        targetLine^[x].R := avg;
        targetLine^[x].G := avg;
        targetLine^[x].B := avg;

      end;
    end;
    Image1.Picture.Pixmap.Canvas.Draw(0, 0, ABitmap);
    Image1.Refresh;
  finally
    ABitmap.Free;
  end;

end;

procedure TForm1.btnEdgeDetectionClick(Sender: TObject);
var
    x, y, avg: integer;
  dConvColorX, dConvColorY, dConvColor: double;
  ABitmap : TBitmap;
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

        dConvColorX := (priorLine^[x - 1].R * 1) +
          (priorLine^[x + 1].R * -1) +
          (thisLine^[x - 1].R * 2) +
          (thisLine^[x + 1].R * -2) +
          (nextLine^[x - 1].R * 1) +
          (nextLine^[x + 1].R * -1);

        dConvColorY := (priorLine^[x - 1].R) +
          (priorLine^[x].R * 2) +
          (priorLine^[x + 1].R) +
          (nextLine^[x - 1].R * -1) +
          (nextLine^[x].R * -2) +
          (nextLine^[x+1].R * -1);

        dConvColor := Sqrt((dConvColorX  * dConvColorX) + (dConvColorY * dConvColorY));

        avg := Round(dConvColor);;
        targetLine^[x].R := avg;
        targetLine^[x].G := avg;
        targetLine^[x].B := avg;

      end;
    end;
    Image1.Picture.Pixmap.Canvas.Draw(0, 0, ABitmap);
    Image1.Refresh;
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


end.
