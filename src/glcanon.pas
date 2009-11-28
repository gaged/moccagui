unit glcanon;

{$MODE objfpc}
{$H+}

{$define DEBUG_SIM}

{$link simcanon.o}
{$link /home/gtom/emc/lib/libemcini.so.0}
{$link /home/gtom/emc/lib/librs274.so.0}
{$linklib stdc++}

{$define PRINT_CANON}

interface

const
  INTP_OK   = 0;
  INTP_EXIT = 1;
  INTP_EXECUTE_FINISH = 2;
  INTP_ENDFILE = 3;
  INTP_FILE_NOT_OPEN = 4;

var
  Metric: boolean; external name 'metric';

function ParseGCode(FileName: string; UseMetric: Boolean): integer;
function GetGCodeError(code: integer): string;

function ToInternalUnits(Value: Double): Double;

implementation

uses
  math, sysutils,
  emc2pas,  // LINELEN
  gllist;   // MyGlList

type
  TTool = packed record
    id: integer;
    zoffset,
    xoffset,
    diameter,
    frontangle, 
    backangle: double;
    orientation: integer;
  end;
  
var
  FeedRate: Double;
  FirstMove: Boolean; 
  lo: tlo;
  offset: tlo;
  Text: string;
  MinExtents: array[0..2] of double;
  MaxExtents: array[0..2] of double;
  InArc: Boolean;
  xo,zo,wo: Double;
  Suppress: integer;
  DwellTime: Double;
  lineno: integer;

var
  Plane: integer; external name 'plane';
  last_sequence_number: integer; external name 'last_sequence_number';
  maxerror: integer; external name 'maxerror';
  savedError: array[0..LINELEN] of char; external name 'savedError';
  dummy_tool: TTool; external name 'dummy_tool';

procedure initgcode; cdecl; external;
function parsefile(filename,unitcode,initcode: PChar): integer; cdecl; external;
function converterror(Err: integer): integer; cdecl; external;

function ToInternalUnits(Value: Double): Double;
begin
  if True then
    Result:= Value / 25.4
  else
    Result:= Value;
end;

function GetGCodeError(Code: integer): string;
begin
  Result:= '';
  if converterror(Code) <> 0 then
    if savedError[0] <> #0 then
      Result:= PChar(savedError);
end;

procedure Init;
begin
  FeedRate:= 1;
  //MinExtents:= (0,0,0);
  //MaxExtents:= [0,0,0];
  FirstMove:= True;
  xo:= 0;
  zo:= 0;
  wo:= 0;
  DwellTime:= 0;
  Suppress:= 0;
  inArc:= False;
  lineno:= 0;
  Plane:= 1;
  Metric:= False;
end;

function ParseGCode(FileName: string; UseMetric: Boolean): integer;
var
  UnitCode: string;
begin
  Result:= -1;
  if not Assigned(MyGlList) then Exit;
  Init;
  if maxerror < 0 then
    initgcode;
  MyGlList.Clear;
  if UseMetric then
    UnitCode:= 'G21'
  else
    UnitCode:= 'G20';
  Result:= parsefile(PChar(FileName),PChar(UnitCode),nil);
end;

procedure AppendTraverse(l: tlo);
begin
  {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Traverse ',l.x,l.y,l.z]));
  {$endif}
  if Assigned(MyGlList) then
    MyGlList.AddTraverse(lineno,lo,l);
end;

procedure AppendFeed(l: tlo);
begin
 {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Feed ',l.x,l.y,l.z]));
 {$endif}
 if Assigned(MyGlList) then
   MyGlList.AddFeed(lineno,lo,l);
end;

procedure AppendArcFeed(l: tlo);
begin
 {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Arcfeed ',l.x,l.y,l.z]));
 {$endif}
  if Assigned(MyGlList) then
    MyGlList.AddArcFeed(lineno,lo,l);
end;

procedure AppendDwell(x,y,z: double);
begin
 {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Dwell ',x,y,z]));
 {$endif}
 if Assigned(MyGlList) then
   MyGlList.AddDwell(lineno,x,y,z);
end;
  
procedure nextline; cdecl; export;
begin
  lineno:= last_sequence_number;
end;

function checkabort: boolean; cdecl; export;
begin
  result:= false;
end;

procedure tooloffset(zt, xt, wt: double); cdecl; export;
begin        
  FirstMove:= True;
  lo.x:= lo.x - xt + xo;
  lo.z:= lo.z - zt + zo;
  lo.w:= lo.w - wt - wo;
  xo:= xt;
  zo:= zt;
  wo:= wt;
end;

procedure setoriginoffsets(x,y,z,a,b,c,u,v,w: double); cdecl; export;
begin
  offset.x:= x;
  offset.y:= y;
  offset.z:= z;
  {self.offset_a = offset_a self.offset_b = offset_b self.offset_c = offset_c
   self.offset_u = offset_u self.offset_v = offset_v self.offset_w = offset_w}
end;

procedure setplane(pl: integer); cdecl; export;
begin
  Plane:= pl;
end;

procedure changetool(Tool: integer); cdecl; export;
begin
  FirstMove:= True;
end;

function gettool(Tool: integer): integer; cdecl; export;
begin
  with dummy_tool do
    begin
      id:= Tool;
      zoffset:= 0.75;
      xoffset:= 0.0625;
      diameter:= 0.5;
      frontangle:= 0;
      backangle:= 0;
      orientation:= 0;
    end;
  Result:= 1;
end;

procedure setspindlerate(rate: double); cdecl; export;
begin
end;

procedure setfeedrate(rate: double); cdecl; export;
begin
  FeedRate:= rate / 60;
end;

procedure settraverserate(rate: double); cdecl; export;
begin
end;

procedure straighttraverse(x,y,z,a,b,c,u,v,w: double); cdecl; export;
var
  l: Tlo;
begin        
  if Suppress > 0 then
    Exit;
  SetCoords(l,x + offset.x,y + offset.y,z + offset.z,a,b,c,u,v,w);
  if not FirstMove then
    AppendTraverse(l);
  lo:= l;
end;

procedure arc2segments(x1,y1,cx,cy: Double;rot: integer;  z1, a, b, c, u, v, w: double);
var
  n: tlo;
  o: tlo;
  p: tlo;
  ccx,ccy: double;
  Steps: integer;
  i: integer;
  theta1: Double;
  theta2: Double;
  theta: Double;
  rad: Double;
  
  function interp(L,H: Double): Double;
  begin
    Result:= L + (H-L) * i / Steps;
  end;

begin

  o:= lo;

  if Plane = 1 then // XY Plane
    begin
     SetCoords(n,x1+offset.x,y1+offset.y,z1+offset.z, a, b, c, u, v, w);
     ccx:= cx + offset.x;
     ccy:= cy + offset.y;
     // xyz = [0,1,2]
     theta1:= arctan2(o.y - cy, o.x - cx);
     theta2:= arctan2(n.y - cy, n.x - cx);
     rad:= hypot(o.x - cx, o.y - cy);
    end
  else
  if Plane = 3 then
    begin
      SetCoords(n,y1+offset.x,z1+offset.y,x1+offset.z, a, b, c, u, v, w);
      ccx:=cx + offset.z;
      ccy:=cy + offset.x;
      // xyz = [2,0,1]
     theta1:= arctan2(o.x - cy, o.z - cx);
     theta2:= arctan2(n.x - cy, n.z - cx);
     rad:= hypot(o.z - cx, o.x - cy);
    end
  else
    begin
      SetCoords(n,z1+offset.x,x1+offset.y,y1+offset.z, a, b, c, u, v, w);
      ccx:=cx + offset.y;
      ccy:=cy + offset.z;
      theta1:= arctan2(o.z - cy, o.y - cx);
      theta2:= arctan2(n.z - cy, n.y - cx);
      rad:= hypot(o.y - cx, o.z - cy);
    end;

  if rot < 0 then
    begin
      if theta2 >= theta1 then
        theta2 -= pi * 2;
    end
  else
    begin
      if theta2 <= theta1 then
      theta2 += pi * 2;
    end;

  steps:= max(8, round(int(128 * abs(theta1 - theta2) / pi)));
  
   //      p = [0] * 9
  for i:= 1 to steps do
    begin
      theta:= interp(theta1, theta2);
      if Plane = 1 then // x,y,z
        begin
          p.x:= cos(theta) * rad + cx;
          p.y:= sin(theta) * rad + cy;
          p.z:= interp(o.z, n.z);
        end
      else
      if Plane = 3 then // z,x,y
        begin
          p.z:= cos(theta) * rad + cx;
          p.x:= sin(theta) * rad + cy;
          p.y:= interp(o.y, n.y);
        end
      else
        begin // y,z,x
          p.y:= cos(theta) * rad + cx;
          p.z:= sin(theta) * rad + cy;
          p.x:= interp(o.x, n.x);
        end;
      p.a:= interp(o.a, n.a);
      p.b:= interp(o.b, n.b);
      p.c:= interp(o.c, n.c);
      p.u:= interp(o.u, n.u);
      p.v:= interp(o.v, n.v);
      p.w:= interp(o.w, n.w);

      FirstMove:= False;
      AppendArcFeed(p);
      lo:= p;
    end;

  AppendArcFeed(n);
  lo:= n;
end;

procedure arcfeed(end1,end2,axis1,axis2: double;
  rot: integer; endp,a,b,c,u,v,w: double); cdecl; export;
begin
  if Suppress > 0 then Exit;
  FirstMove:= False;
  InArc:= True;
  try
    Arc2Segments(end1,end2,axis1,axis2,rot,endp,a,b,c,u,v,w);
  finally
    InArc:= False;
  end;
end;

procedure straightarcsegment(x,y,z,a,b,c,u,v,w: double); cdecl; export;
var
  l: Tlo;
begin
  FirstMove:= False;
  SetCoords(l,x,y,z,a,b,c,u,v,w);
  AppendArcFeed(l);
  lo:= l;
end;

procedure rigidtap(x,y,z: double); cdecl; export;
var
  l: tlo;
begin
  if Suppress > 0 then
    Exit;
  FirstMove:= False;
  SetCoords(l,x + offset.x,y + offset.y,z + offset.z, lo.a,lo.b,lo.c,lo.u,lo.v,lo.w);
  AppendFeed(l);
  AppendDwell(x + offset.x, y + offset.y, z + offset.z);
  AppendFeed(l);
end;

procedure straightfeed(x,y,z,a,b,c,u,v,w: double); cdecl; export;
var
  l: tlo;
begin       
  if Suppress > 0 then Exit;
  FirstMove:= False;
  SetCoords(l,x + offset.x,y + offset.y,z + offset.z,
    a + offset.a, b + offset.b, c + offset.c,
    u + offset.u, v + offset.v, w + offset.w);
  AppendFeed(l);
  lo:= l
end;
    
procedure straightprobe(x,y,z,a,b,c,d,u,v,w: double); cdecl; export;
begin
  straightfeed(x,y,z,a,b,c,u,v,w);
end;

procedure userdefinedfunction(i: integer; p,q: double); cdecl; export;
begin        
  if Suppress > 0 then Exit;
  //color = self.colors['m1xx']
  AppendDwell(lo.x,lo.y,lo.z  {self.state.plane/10-17)} )
end;
        
procedure dwell(arg: double); cdecl; export;
begin
  if Suppress > 0 then Exit;
  DwellTime:= DwellTime + arg;
  // color = self.colors['dwell']
  AppendDwell(lo.x,lo.y,lo.z {self.state.plane/10-17)})
end;

function getblockdelete: integer; cdecl; export;
begin
  Result:= integer(true);
end;

function toolalongw: integer; cdecl; export;
begin
  result:= 0;
end;

procedure setcomment(const msg: PChar); cdecl; export;
begin
end;

procedure setmessage(const msg: PChar); cdecl; export;
begin
end;


end.



