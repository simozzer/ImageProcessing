unit unSiTrigonometry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, unSiImageProcessingTypes;

const
  MAX_ENTRIES = 3600;
  TWO_PI: TSiFloat = 2 * pi;
  LOOKUP_INC: TSiFloat = (2 * pi) / MAX_ENTRIES;
  LOOKUP_FACTOR: Double = MAX_ENTRIES / ( 2 * pi);

type

  { TSiFastSin }

  { TSiFastTrig }

  TSiFastTrig = class
  private
    FdSineLookupTable: array [0..MAX_ENTRIES] of TSiFloat;
    FdCosineLookupTable: array [0..MAX_ENTRIES] of TSiFloat;
    procedure BuildLookups;
    function ForceRange(const dAngle: TSiFloat): TSiFloat;
    function GetSine(const dAngle: TSiFloat): TSiFloat;
    function GetCosine(const dAngle: TSiFloat): TSiFloat;
  public
    property Sine[const index: TSiFloat]: TSiFloat read GetSine;
    property Cosine[const index: TSiFloat]: TSiFloat read GetCosine;
    constructor Create;
  end;


  { TSiTrig }

  TSiTrig = class
    public
      class function GetSine(const index : TSiFloat): TSiFloat;
      class function getCosine(const iIndex : TSiFloat): TSiFloat;
  end;

implementation

uses Math;

var
  FFastTrig : TSiFastTrig;

{ TSiTrig }

class function TSiTrig.GetSine(const index: TSiFloat): TSiFloat;
begin
  RESULT := FFastTrig.Sine[index];
end;

class function TSiTrig.getCosine(const iIndex: TSiFloat): TSiFloat;
begin
  RESULT := FFastTrig.Cosine[iIndex];
end;

{ TSiFastTrig }

procedure TSiFastTrig.BuildLookups;
var
  i : Integer;
  dAngle : TSiFloat;
begin
  dAngle := 0.0;
  for i := 0 to pred(MAX_ENTRIES) do
  begin
    FdSineLookupTable[I] := Sin(dAngle);
    FdCosineLookupTable[I] := Cos(dAngle);
    dAngle += LOOKUP_INC;
  end;
end;

function TSiFastTrig.ForceRange(const dAngle: TSiFloat): TSiFloat;
var
  dReturnAngle: TSiFloat;
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

function TSiFastTrig.GetSine(const dAngle: TSiFloat): TSiFloat;
begin
  RESULT := FdSineLookupTable[round(LOOKUP_FACTOR * ForceRange(dAngle))];
end;

function TSiFastTrig.GetCosine(const dAngle: TSiFloat): TSiFloat;
begin
  RESULT := FdCosineLookupTable[round(LOOKUP_FACTOR * ForceRange(dAngle))];
end;

constructor TSiFastTrig.Create;
begin
  BuildLookups;
end;

initialization
  FFastTrig := TSiFastTrig.Create;

end.

