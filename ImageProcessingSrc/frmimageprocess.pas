unit frmImageProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, LclType, ActnList, unSiImageProcessingTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    actGrayScale: TAction;
    actBlur: TAction;
    actEdge: TAction;
    actThreadedRotate: TAction;
    actRotatePlus: TAction;
    actToggleRotation: TAction;
    ActionList1: TActionList;
    btnGrayscale: TButton;
    btnOpenImage: TButton;
    btnBlur: TButton;
    btnEdgeDetection: TButton;
    btnToggleRotation: TButton;
    btnRotatePlus: TButton;
    btnThreadRotate: TButton;
    Image1: TImage;
    ImageList1: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    pnlButtons: TPanel;
    Timer1: TTimer;
    procedure actBlurExecute(Sender: TObject);
    procedure actEdgeExecute(Sender: TObject);
    procedure actGrayScaleExecute(Sender: TObject);
    procedure actRotatePlusExecute(Sender: TObject);
    procedure actThreadedRotateExecute(Sender: TObject);
    procedure actToggleRotationExecute(Sender: TObject);
    procedure btnOpenImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOrigBitmap, FBackBitmap: TBitmap;
    FdRotation: TSiFloat;
    procedure IncreaseRotation(const dAngle : TSiFloat);
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

  end;



var
  Form1: TForm1;

implementation

uses
  Types, unThreadRotation, unSiTrigonometry;

{$R *.lfm}

const
  TWO_PI = Pi * 2;


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
  Image1.Canvas.Draw(0, 0, AImageBitmap);

end;


procedure TForm1.actRotatePlusExecute(Sender: TObject);
var
  iX, iY, iCenterX, iCenterY, iSourceX, iSourceY, iDeltaX, iDeltaY: integer;
  ASourceBitmap: TBitmap;
  pSourceLine, pTargetLine: PRGBTripleArray;
  dRadius, dAngle: TSiFloat;
const
  ROTATION = 0.01;
begin
  IncreaseRotation(ROTATION);
  ASourceBitmap := FOrigBitmap;
  iCenterX := ASourceBitmap.Width div 2;
  iCenterY := ASourceBitmap.Height div 2;
  if FBackBitmap = nil then
  begin
    FBackBitmap := TBitmap.Create;
  end;

  FBackBitmap.Clear;
  FBackBitmap.Width := ASourceBitmap.Width;
  FBackBitmap.Height := ASourceBitmap.Height;
  FBackBitmap.PixelFormat := ASourceBitmap.PixelFormat;


  for iY := 0 to pred(ASourceBitmap.Height) do
  begin
    pTargetLine := FBackBitmap.ScanLine[iY];
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
          dAngle := pi
        else if (iDeltaX > 0) then
          dAngle := 0;
      end
      else if (iDeltaX < 0) and (iDeltaY < 0) then
        dAngle := pi + ArcTan(iDeltaY / iDeltaX)
      else if (iDeltaX > 0) and (iDeltaY < 0) then
        dAngle := -((pi / 2) + ArcTan(iDeltaX / iDeltaY))
      else if (iDeltaX < 0) and (iDeltaY > 0) then
        dAngle := pi + ArcTan(iDeltaY / iDeltaX)
      else if (iDeltax > 0) and (iDeltaY > 0) then
        dAngle := ArcTan(iDeltaY / iDeltax);

      iSourceX := Round(dRadius * TSiTrig.getCosine(dAngle - FdRotation)) + iCenterX;
      iSourceY := Round(dRadius * TSiTrig.GetSine(dAngle - FdRotation)) + iCenterY;


      if PtInRect(Rect(0, 0, FBackBitmap.Width, FBackBitmap.Height),
        Point(round(iSourceX), round(iSourceY))) then
      begin
        pSourceLine := ASourceBitmap.ScanLine[round(iSourceY)];
        pTargetLine^[round(iX)].R := pSourceLine^[iSourceX].R;
        pTargetLine^[round(iX)].G := pSourceLine^[iSourceX].G;
        pTargetLine^[round(iX)].B := pSourceLine^[iSourceX].B;
      end;

    end;
  end;
  Image1.Picture.Pixmap.Canvas.Draw(0, 0, FBackBitmap);

end;

procedure TForm1.actThreadedRotateExecute(Sender: TObject);
var
  iY, iCenterX, iCenterY: integer;
  ASourceBitmap: TBitmap;
const
  ROTATION = 0.03;
begin
  IncreaseRotation(ROTATION);
  ASourceBitmap := FOrigBitmap;
  iCenterX := round(ASourceBitmap.Width / 3.5);
  iCenterY := round(ASourceBitmap.Height / 5);
  if FBackBitmap = nil then
  begin
    FBackBitmap := TBitmap.Create;
  end;

  FBackBitmap.Clear;
  FBackBitmap.Width := ASourceBitmap.Width;
  FBackBitmap.Height := ASourceBitmap.Height;
  FBackBitmap.PixelFormat := ASourceBitmap.PixelFormat;

  FdRotation := FdRotation + ROTATION;
  for iY := 0 to 7 do
  begin
    TRotationThread.Create(ASourceBitmap, FBackBitmap, iCenterX,
      iCenterY, iY, FdRotation, Self.Handle);

  end;
  Image1.Picture.Bitmap.Canvas.Draw(0, 0, FBackBitmap);

end;

procedure TForm1.actToggleRotationExecute(Sender: TObject);
begin
  if not timer1.Enabled then
  begin
    Timer1.Enabled := True;
    TAction(Sender).Caption := 'Stop Rotation';
  end
  else
  begin
    Timer1.Enabled := False;
    TAction(Sender).Caption := 'Start Rotation';
  end;
end;



procedure TForm1.actBlurExecute(Sender: TObject);
var
  iX, iY, iAvg: integer;
  dConvColor: TSiFloat;
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
  dConvColorX, dConvColorY, dConvColor: TSiFloat;
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
  FBackBitmap := nil;
end;

procedure TForm1.IncreaseRotation(const dAngle: TSiFloat);
begin
  FdRotation := FdRotation + dAngle;
  while (FdRotation > TWO_PI) do
  begin
    FdRotation -= TWO_PI;
  end;
  while (FdRotation < 0) do
  begin
    FdRotation += TWO_PI;
  end;
end;


procedure TForm1.AfterConstruction;
begin
  inherited AfterConstruction;
  FOrigBitmap.Assign(Image1.Picture.Bitmap);
end;

destructor TForm1.Destroy;
begin
  FOrigBitmap.Free;
  FBackBitmap.Free;
  inherited Destroy;
end;


end.
