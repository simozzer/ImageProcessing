unit unEdgeStats;

{$mode ObjFPC}{$H+}

interface

uses
    Classes;

type

    TEdgeDetails = class(TObject)
      private
             iXEdge : integer;
             iYEdge : integer;
             function getEdgeValue: Integer;
             function getAngle: Double;
      public
            property XEdge : integer read iXEdge write iXEdge;
            property YEdge : integer read iYEdge write iYEdge;
            property Value : Integer read GetEdgeValue;
            property Angle : Double read GetAngle;
    end;

  TEdgeStats = class(TObject)
  end;


implementation

uses
  Math;

function TEdgeDetails.getEdgeValue: Integer;
begin
  RESULT := Trunc(Sqrt( (Self.iXEdge * self.iXEdge) + (Self.iYEdge * self.iYEdge) ));
end;

function TEdgeDetails.getAngle: Double;
begin
     RESULT := 0.00; // TODO
end;





end.

