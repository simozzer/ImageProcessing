unit unSiImageProcessingTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
TSiRGB = packed record
  B, G, R, A: byte;
end;

PSiRGB = ^TSiRGB;

TRGBTripleArray = array[0..4095] of TSiRGB;
PRGBTripleArray = ^TRGBTripleArray;



implementation

end.

