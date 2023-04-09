unit unThreadRotation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type

  { TRotationThread }

  TRotationThread = class(TThread)
  private
    FBmpSource: TBitmap;
    FBmpTarget: TBitmap;
    FiCx: integer;
    FiCy: integer;
    FiLine: integer;
    FiWidth: integer;
    FiHeight: integer;
    FdAngle: double;
  public
    constructor Create(ABmpSource, ABmpTarget: TBitmap; iCx, iCy, iLine: integer;
      dAngle: double);
    procedure Execute; override;
  end;

implementation

{ TRotationThread }

uses
  unSiImageProcessingTypes, Types;

procedure TRotationThread.Execute;
var
  iX, iDeltaX, iDeltaY, iSourceX, iSourceY: integer;
  dRadius, dAngle: double;
  pTargetLine, pSourceLine: PRGBTripleArray;
begin
  for iX := 0 to pred(FiWidth) do
  begin
    pTargetLine := FBmpTarget.ScanLine[FiLine];

    iDeltaX := iX - FiCx;
    iDeltaY := FiLine - FiCy;
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

    iSourceX := Round(dRadius * cos(dAngle - FdAngle)) + FiCx;
    iSourceY := Round(dRadius * sin(dAngle - FdAngle)) + FiCy;


    if PtInRect(Rect(0, 0, FiWidth, FiHeight),
      Point(round(iSourceX), round(iSourceY))) then
    begin
      pSourceLine := FBmpSource.ScanLine[round(iSourceY)];
      pTargetLine^[round(iX)].R := pSourceLine^[iSourceX].R;
      pTargetLine^[round(iX)].G := pSourceLine^[iSourceX].G;
      pTargetLine^[round(iX)].B := pSourceLine^[iSourceX].B;
    end;
  end;
end;

constructor TRotationThread.Create(ABmpSource, ABmpTarget: TBitmap;
  iCx, iCy, iLine: integer; dAngle: double);
begin
  inherited Create(True);
  FreeOnTerminate := true;
  FBmpSource := ABmpSource;
  FBmpTarget := ABmpTarget;
  FiCx := iCx;
  FiCy := iCy;
  FiLine := iLine;
  FdAngle := dAngle;
  FiWidth := ABmpTarget.Width;
  FiHeight := ABmpTarget.Height;
  Execute;
end;

end.
