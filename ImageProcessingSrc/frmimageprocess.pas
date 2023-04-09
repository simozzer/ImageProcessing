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
    actToggleRotation: TAction;
    actRotate: TAction;
    ActionList1: TActionList;
    btnGrayscale: TButton;
    btnOpenImage: TButton;
    btnBlur: TButton;
    btnEdgeDetection: TButton;
    btnToggleRotation: TButton;
    btnRotateOrig: TButton;
    Image1: TImage;
    ImageList1: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    pnlButtons: TPanel;
    Timer1: TTimer;
    procedure actBlurExecute(Sender: TObject);
    procedure actEdgeExecute(Sender: TObject);
    procedure actGrayScaleExecute(Sender: TObject);
    procedure actRotateExecute(Sender: TObject);
    procedure actToggleRotationExecute(Sender: TObject);
    procedure btnOpenImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOrigBitmap: TBitmap;
    FdRotation: double;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

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
  Types;

{$R *.lfm}


{ TForm1 }



procedure TForm1.actGrayScaleExecute(Sender: TObject);
var
  iX, iY, iAvg: integer;
  AImageBitmap: TBitmap;
  pLine: PRGBTripleArray;
begin

  AImageBitmap := Image1.Picture.Bitmap;
  for iY := 1 to Image1.Picture.Height - 2 do
  begin
    pLine := PRGBTripleArray(AImageBitmap.ScanLine[iY]);
    for iX := 1 to Image1.Picture.Width - 2 do
    begin
      iAvg := (pLine^[iX].R + pLine^[iX].G + pLine^[iX].B) div 3;
      pLine^[iX].R := iAvg;
      pLine^[iX].G := iAvg;
      pLine^[iX].B := iAvg;
    end;
  end;
  FOrigBitmap.Assign(AImageBitmap);
  Image1.Canvas.Draw(0,0,AImageBitmap);

end;

procedure TForm1.actRotateExecute(Sender: TObject);
var
  iX, iY, iCenterX, iCenterY, iTargetX, iTargetY, iDeltaX, iDeltaY: integer;
  ABitmap, ASourceBitmap: TBitmap;
  pSourceLine, pTargetLine: PRGBTripleArray;
  dRadius, dAngle: double;
const
  ROTATION = 0.03;
begin
  FdRotation := FdRotation + ROTATION;
  ASourceBitmap := FOrigBitmap;
  iCenterX := ASourceBitmap.Width div 2;
  iCenterY := ASourceBitmap.Height div 2;
  ABitmap := TBitmap.Create;
  ABitmap.Width := ASourceBitmap.Width;
  ABitmap.Height := ASourceBitmap.Height;
  ABitmap.PixelFormat := ASourceBitmap.PixelFormat;
  try
    for iY := 0 to pred(ASourceBitmap.Height) do
    begin
      pSourceLine := ASourceBitmap.ScanLine[iY];
      for iX := 0 to pred(ASourceBitmap.Width) do
      begin
        iDeltaX := iX - iCenterX;
        iDeltaY := iY - iCenterY;
        dRadius := Sqrt(sqr(iDeltaX) + Sqr(iDeltaY));

        if (iDeltaX = 0) then
        begin
          if (iDeltaY < 0) then
            dAngle := -pi / 2
          else if (iDeltaY > 0) then
            dAngle := pi / 2;
        end
        else if (iDeltaY = 0) then
        begin
          if (iDeltaX < 0) then
            dAngle := 0
          else if (iDeltaX > 0) then
            dAngle := pi;
        end
        else if (iDeltaX < 0) and (iDeltaY < 0) then
          dAngle := pi + ArcTan(iDeltaY / iDeltaX)
        else if (iDeltaX > 0) and (iDeltaY < 0) then
          dAngle := -((pi / 2) + ArcTan(iDeltaX / iDeltaY))
        else if (iDeltaX < 0) and (iDeltaY > 0) then
          dAngle := pi + ArcTan(iDeltaY / iDeltaX)
        else if (iDeltax > 0) and (iDeltaY > 0) then
          dAngle := ArcTan(iDeltaY / iDeltax);

        iTargetX := Round(dRadius * cos(dAngle + FdRotation)) + iCenterX;
        iTargetY := Round(dRadius * sin(dAngle + FdRotation)) + iCenterY;


        if PtInRect(Rect(0, 0, ABitmap.Width, ABitmap.Height),
          Point(round(iTargetX), round(iTargetY))) then
        begin
          pTargetLine := ABitmap.ScanLine[round(iTargetY)];
          pTargetLine^[round(iTargetX)].R := pSourceLine^[iX].R;
          pTargetLine^[round(iTargetX)].G := pSourceLine^[iX].G;
          pTargetLine^[round(iTargetX)].B := pSourceLine^[iX].B;
        end;

      end;
    end;
    Image1.Picture.Pixmap.Canvas.Draw(0, 0, ABitmap);
  finally
    ABitmap.Free;
  end;

end;

procedure TForm1.actToggleRotationExecute(Sender: TObject);
begin
  if not timer1.Enabled then
  begin
    Timer1.enabled := true;
    TAction(Sender).Caption := 'Stop Rotation';
  end
  else
  begin
    Timer1.enabled := False;
    TAction(Sender).Caption := 'Start Rotation';
  end;
end;



procedure TForm1.actBlurExecute(Sender: TObject);
var
  iX, iY, iAvg: integer;
  dConvColor: double;
  ABitmap: TBitmap;
  AImageBitmap: TBitmap;
  pThisLine, pPriorLine, pNextLine, pTargetLine: PRGBTripleArray;
begin
  ABitmap := TBitmap.Create;
  ABitmap.Width := Image1.Picture.Width;
  ABitmap.Height := Image1.Picture.Height;
  ABitmap.PixelFormat := Image1.Picture.Bitmap.PixelFormat;
  try
    AImageBitmap := Image1.Picture.Bitmap;
    for iY := 1 to Image1.Picture.Height - 2 do
    begin

      pThisLine := PRGBTripleArray(AImageBitmap.ScanLine[iY]);
      pPriorLine := PRGBTripleArray(AImageBitmap.ScanLine[iY - 1]);
      pNextLine := PRGBTripleArray(AImageBitmap.ScanLine[iY + 1]);

      pTargetLine := PRGBTripleArray(ABitmap.ScanLine[iY]);
      for iX := 1 to Image1.Picture.Width - 2 do
      begin

        dConvColor := (pPriorLine^[iX - 1].R * 0.1) + (pPriorLine^[iX].R * 0.1) +
          (pPriorLine^[iX + 1].R * 0.1) + (pThisLine^[iX - 1].R * 0.1) +
          (pThisLine^[iX].R * 0.2) + (pThisLine^[iX + 1].R * 0.1) +
          (pNextLine^[iX - 1].R * 0.1) + (pNextLine^[iX].R * 0.1) +
          (pNextLine^[iX + 1].R * 0.1);

        iAvg := Round(dConvColor);
        pTargetLine^[iX].R := iAvg;
        pTargetLine^[iX].G := iAvg;
        pTargetLine^[iX].B := iAvg;

      end;
    end;
    Image1.Picture.Pixmap.Canvas.Draw(0, 0, ABitmap);
    Image1.Refresh;
    FOrigBitmap.Assign(Image1.Picture.Bitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TForm1.actEdgeExecute(Sender: TObject);
var
  iX, iY, iAvg: integer;
  dConvColorX, dConvColorY, dConvColor: double;
  ABitmap: TBitmap;
  AImageBitmap: TBitmap;
  pThisLine, pPriorLine, pNextLine, pTargetLine: PRGBTripleArray;
begin
  ABitmap := TBitmap.Create;
  ABitmap.Width := Image1.Picture.Width;
  ABitmap.Height := Image1.Picture.Height;
  ABitmap.PixelFormat := Image1.Picture.Bitmap.PixelFormat;

  try
    AImageBitmap := Image1.Picture.Bitmap;
    for iY := 1 to Image1.Picture.Height - 2 do
    begin

      pThisLine := PRGBTripleArray(AImageBitmap.ScanLine[iY]);
      pPriorLine := PRGBTripleArray(AImageBitmap.ScanLine[iY - 1]);
      pNextLine := PRGBTripleArray(AImageBitmap.ScanLine[iY + 1]);

      pTargetLine := PRGBTripleArray(ABitmap.ScanLine[iY]);
      for iX := 1 to Image1.Picture.Width - 2 do
      begin

        dConvColorX := (pPriorLine^[iX - 1].R * 1) + (pPriorLine^[iX + 1].R * -1) +
          (pThisLine^[iX - 1].R * 2) + (pThisLine^[iX + 1].R * -2) +
          (pNextLine^[iX - 1].R * 1) + (pNextLine^[iX + 1].R * -1);

        dConvColorY := (pPriorLine^[iX - 1].R) + (pPriorLine^[iX].R * 2) +
          (pPriorLine^[iX + 1].R) + (pNextLine^[iX - 1].R * -1) +
          (pNextLine^[iX].R * -2) + (pNextLine^[iX + 1].R * -1);

        dConvColor := Sqrt((dConvColorX * dConvColorX) + (dConvColorY * dConvColorY));

        iAvg := Round(dConvColor);

        pTargetLine^[iX].R := iAvg;
        pTargetLine^[iX].G := iAvg;
        pTargetLine^[iX].B := iAvg;

      end;
    end;
    Image1.Picture.Pixmap.Canvas.Draw(0, 0, ABitmap);
    Image1.Refresh;
    FOrigBitmap.Assign(Image1.Picture.Bitmap);
  finally
    ABitmap.Free;
  end;
end;


procedure TForm1.btnOpenImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    FOrigBitmap.Assign(Image1.Picture.Bitmap);
  end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  FOrigBitmap := TBitmap.Create;
end;

procedure TForm1.AfterConstruction;
begin
  inherited AfterConstruction;
  FOrigBitmap.Assign(Image1.Picture.Bitmap);
end;

destructor TForm1.Destroy;
begin
   FOrigBitmap.Free;
   inherited Destroy;
end;


end.
