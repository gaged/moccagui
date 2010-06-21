unit logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

procedure LoggerAddPoint(x,y,z: double);
procedure LoggerClear;
procedure LoggerCall;

implementation

uses
  gl;

const
  MaxPoints = 10000;

type
  T3DPt = record
    x,y,z: double;
  end;

type
  TPtArray = array[0..MaxPoints - 1] of T3dPt;

var
  Pts: TPtArray;
  Current,Last: integer;

procedure LoggerClear;
var
  i: integer;
begin
  for i:= 0 to MaxPoints - 1 do
    begin
      Pts[i].x:= 0;
      Pts[i].y:= 0;
      Pts[i].z:= 0;
      Current:= 0;
      Last:= 0;
    end;
end;

procedure LoggerAddPoint(x,y,z: double);
var
  P: T3dPt;
begin
  P.x:= x;
  P.y:= y;
  P.z:= z;
  Pts[Current]:= P;
  inc(Current);
  if Current > MaxPoints - 1 then
    Current:= 0;
  if Last < Current then Last:= Current;
end;

procedure LoggerCall;
var
  i: integer;
begin
  if Last < 2 then Exit;
  // glLineWidth(3);
  glBegin(GL_LINE_STRIP);
  // glColor3f(0,1,0);
  i:= Current + 1;
  while i < Last do
    begin
      glVertex3f(Pts[i].x,Pts[i].y,Pts[i].z+0.002);
      inc(i);
    end;
  i:= 0;
  while i < Current do
    begin
      glVertex3f(Pts[i].x,Pts[i].y,Pts[i].z+0.002);
      inc(i);
    end;
  glEnd();
  glLineWidth(1);
end;

end.

