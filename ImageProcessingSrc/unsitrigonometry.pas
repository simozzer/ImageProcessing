unit unSiTrigonometry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  MAX_ENTRIES = 36000;
  TWO_PI: Double = 2 * pi;
  LOOKUP_INC: DOUBLE = (2 * pi) / MAX_ENTRIES;
  LOOKUP_FACTOR: Double = MAX_ENTRIES / ( 2 * pi);

type

  { TSiFastSin }

  { TSiFastTrig }

  TSiFastTrig = class
  private
    FdSineLookupTable: array [0..MAX_ENTRIES] of double;
    FdCosineLookupTable: array [0..MAX_ENTRIES] of double;
    procedure BuildLookups;
    function ForceRange(const dAngle: Double): Double;
    function GetSine(const dAngle: Double): Double;
    function GetCosine(const dAngle: Double): Double;
  public
    property Sine[const index: Double]: Double read GetSine;
    property Cosine[const index: Double]: double read GetCosine;
    constructor Create;
  end;


  { TSiTrig }

  TSiTrig = class
    public
      class function GetSine(const index : Double): Double;
      class function getCosine(const iIndex : Double): Double;
  end;

implementation

uses Math;

var
  FFastTrig : TSiFastTrig;

{ TSiTrig }

class function TSiTrig.GetSine(const index: Double): Double;
begin
  RESULT := FFastTrig.Sine[index];
end;

class function TSiTrig.getCosine(const iIndex: Double): Double;
begin
  RESULT := FFastTrig.Cosine[iIndex];
end;

{ TSiFastTrig }

procedure TSiFastTrig.BuildLookups;
var
  i : Integer;
  dAngle : Double;
begin
  dAngle := 0.0;
  for i := 0 to pred(MAX_ENTRIES) do
  begin
    FdSineLookupTable[I] := Sin(dAngle);
    FdCosineLookupTable[I] := Cos(dAngle);
    dAngle += LOOKUP_INC;
  end;
end;

function TSiFastTrig.ForceRange(const dAngle: Double): Double;
var
  dReturnAngle: Double;
begin
  dReturnAngle := dAngle;
  while (dReturnAngle > TWO_PI) do
  begin
    dReturnAngle -= TWO_PI;
  end;
  while (dReturnAngle < 0) do
  begin
    dReturnAngle += TWO_PI;
  end;
  RESULT := dReturnAngle;
end;

function TSiFastTrig.GetSine(const dAngle: Double): Double;
begin
  // TODO ... do not exceed 2* pi!
  RESULT := FdSineLookupTable[round(LOOKUP_FACTOR * ForceRange(dAngle))];
end;

function TSiFastTrig.GetCosine(const dAngle: Double): Double;
begin
    // TODO ... do not exceed 2* pi!
  RESULT := FdCosineLookupTable[round(LOOKUP_FACTOR * ForceRange(dAngle))];
end;

constructor TSiFastTrig.Create;
begin
  BuildLookups;
end;

initialization
  FFastTrig := TSiFastTrig.Create;

end.

