unit unThreadRotation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, unSiImageProcessingTypes;

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
    FdAngle: TSiFloat;
    FMainThreadHandle: THandle;
  public
    constructor Create(ABmpSource, ABmpTarget: TBitmap; iCx, iCy, iLine: integer;
      dAngle: TSiFloat; MainThreadHandle: THandle);
    procedure Execute; override;
  end;

implementation

{ TRotationThread }

uses
  Types, unSiTrigonometry;

procedure TRotationThread.Execute;
var
  iX, iDeltaX, iDeltaY, iSourceX, iSourceY: integer;
  pTargetLine, pSourceLine: PRGBTripleArray;
  iLine: integer;
begin

  iLine := FiLine;
  while (iLine < FiHeight) do
  begin
    for iX := 0 to pred(FiWidth) do
    begin
      pTargetLine := FBmpTarget.ScanLine[iLine];

      iDeltaX := iX - FiCx;
      iDeltaY := iLine - FiCy;
      iSourceX := FiCx + Round((iDeltaX * TSiTrig.getCosine(FdAngle)) -
        (iDeltaY * TSiTrig.getSine(FdAngle)));

      iSourceY := FiCy + Round((iDeltaX * TSiTrig.GetSine(FdAngle)) +
        (iDeltaY * TSiTrig.getCosine(FdAngle)));

      if PtInRect(Rect(0, 0, FiWidth, FiHeight),
        Point(round(iSourceX), round(iSourceY))) then
      begin
        pSourceLine := FBmpSource.ScanLine[round(iSourceY)];
        pTargetLine^[round(iX)].R := pSourceLine^[iSourceX].R;
        pTargetLine^[round(iX)].G := pSourceLine^[iSourceX].G;
        pTargetLine^[round(iX)].B := pSourceLine^[iSourceX].B;
      end;
    end;
    iLine += 16;
  end;
  Terminate;
end;

constructor TRotationThread.Create(ABmpSource, ABmpTarget: TBitmap;
  iCx, iCy, iLine: integer; dAngle: TSiFloat; MainThreadHandle: THandle);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FBmpSource := ABmpSource;
  FBmpTarget := ABmpTarget;
  FiCx := iCx;
  FiCy := iCy;
  FiLine := iLine;
  FdAngle := dAngle;
  FiWidth := ABmpTarget.Width;
  FiHeight := ABmpTarget.Height;
  FMainThreadHandle := MainThreadHandle;
  Execute;
end;

end.
