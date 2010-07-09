unit glcanon;

{$MODE objfpc}
{$H+}

{$link simcanon.o}

{$I mocca.inc}

interface

uses
  mocglb;

const
  INTP_OK = 0;
  INTP_EXIT = 1;
  INTP_EXECUTE_FINISH = 2;
  INTP_ENDFILE = 3;
  INTP_FILE_NOT_OPEN = 4;

// from rs274ngc.hh
const
  ACTIVE_G_CODES_MAX = 16;
  ACTIVE_M_CODES_MAX = 10;
  ACTIVE_SETTINGS_MAX = 3;

  DEFAULT_TOOL_DIA = 0.254;
  DEFAULT_TOOL_LENGTH = 2.54;

function ParseGCode(FileName: string; UnitCode,InitCode: string): integer;

function interpreter_init: longint; cdecl; external;
function interpreter_reset: longint; cdecl; external;
function interpreter_exec(const Command: PChar): longint; cdecl; external;
function interpreter_synch: longint; cdecl; external;
procedure interpreter_codes; cdecl; external;

function GetGCodeError(code: integer): string;

function ToCanonUnits(Value: Double): Double;
function ToCanonPos(Value: double; Axis: integer): double;

procedure GetCanonOffset(var Ofs: tlo);
procedure CanonInitOffsets;

function GetToolDiameter(i: integer): double;
function GetToolLength(i: integer): double;

function GCodeToStr(i: integer): string;
function MCodeToStr(i: integer): string;
function GetActiveFeed: Double;
function GetActiveSpindle: Double;

implementation

uses
  math, sysutils,
  emc2pas,  // LINELEN
  gllist;   // MyGlList

var
  FirstMove: Boolean;
  lo: tlo;
  offset: tlo;
  xo,zo,wo: Double;
  DwellTime: Double;
  lineno: integer;

var
  Plane: integer; external name 'plane';
  last_sequence_number: integer; external name 'last_sequence_number';
  maxerror: integer; external name 'maxerror';
  savedError: array[0..LINELEN] of char; external name 'savedError';
  CanonTool: TTool; external name 'canontool';
  ParameterFileName: array[0..LINELEN] of Char; external name '_parameter_file_name';
  glMetric: boolean; external name 'metric';
  axisMask: integer; external name 'axis_mask';

  glSettings: array[0..ACTIVE_SETTINGS_MAX-1] of double; external name 'settings';
  glGCodes: array[0..ACTIVE_G_CODES_MAX-1] of integer; external name 'gcodes';
  glMCodes: array[0..ACTIVE_M_CODES_MAX-1] of integer; external name 'mcodes';

{$ifdef VER_23}
procedure initgcode; cdecl; external;
{$endif}
function parsefile(filename,unitcode,initcode: PChar): integer; cdecl; external;
function converterror(Err: integer): integer; cdecl; external;
function goto_line(line_no: integer; filename,unitcode,initcode: PChar): integer; cdecl; external;


function GCodeToStr(i: integer): string;
var
  V,V1,V2: integer;
  S: string;
begin
  Result:= '';
  S:= '';
  if (i > 0) and (i < ACTIVE_G_CODES_MAX) then
    begin
      v:= glGCodes[i];
      if V > 100 then
        begin
          V1:= Trunc(V/10);
          V2:= V - (V1 * 10);
          S:= 'G' + IntToStr(V1);
          if V2 > 0 then
            S:= S + '.' + IntToStr(V2);
        end;
    end;
  Result:= S;
end;

function MCodeToStr(i: integer): string;
var
  V: integer;
  S: string;
begin
  Result:= '';
  S:= '';
  if (i > 0) and (i < ACTIVE_M_CODES_MAX) then
    begin
      V:= glMCodes[i];
      if V > 0 then S:= 'M' + IntToStr(V);
    end;
  Result:= S;
end;

function GetActiveFeed: Double;
begin
  Result:= glSettings[1];
end;

function GetActiveSpindle: Double;
begin
  Result:= glSettings[2];
end;

function ToCanonUnits(Value: Double): Double;
begin
  if Vars.Metric then
    Result:= Value / 25.4
  else
    Result:= Value;
end;


function ToCanonPos(Value: double; Axis: integer): double;
var
  P: double;
begin
  if Axis = 0 then P:= offset.x else
    if Axis = 1 then P:= offset.y else
      P:= offset.z;
 if Vars.Metric then
    Result:= (Value / 25.4) + P
  else
    Result:= Value + P;
end;

function GetGCodeError(Code: integer): string;
begin
  Result:= '';
  if converterror(Code) <> 0 then
    if savedError[0] <> #0 then
      Result:= PChar(savedError);
end;

function GetToolDiameter(i: integer): double;
var
  d: double;
begin
  // writeln('gettooldiameter ' + IntToStr(i));
  if (i < 0) or (i > CANON_TOOL_MAX) then
    d:= DEFAULT_TOOL_DIA
  else
    d:= ToCanonUnits(Tools[i].diameter);
  if d < DEFAULT_TOOL_DIA then
    d:= DEFAULT_TOOL_DIA;
  Result:= d;
end;

function GetToolLength(i: integer): double;
var
  d: double;
begin
  if (i < 0) or (i > CANON_TOOL_MAX) then
    d:= DEFAULT_TOOL_LENGTH
  else
    d:= ToCanonUnits(Tools[i].zoffset);
  if d < 0.5 then
    d:= 0.5;
  Result:= d;
end;

procedure InitOffsets;
var
  x,y,z: double;
begin
  x:= ToCanonUnits(GetOrigin(0));
  y:= ToCanonUnits(GetOrigin(1));
  z:= ToCanonUnits(GetOrigin(2));
  SetCoords(Offset,x,y,z,0,0,0,0,0,0);
  writeln(Format('%s %f %f %f',['Canon Offsets: ',x,y,z]));
end;

procedure CanonInitOffsets;
begin
  InitOffsets;
end;

procedure GetCanonOffset(var Ofs: tlo);
begin
  ofs:= offset;
end;

procedure Init;
var
  s: string;
begin
  FirstMove:= True;
  xo:= 0;
  zo:= 0;
  wo:= 0;
  DwellTime:= 0;
  lineno:= 0;
  Plane:= 1;
  glMetric:= False;
  axisMask:= trajAxisMask;
  s:= Vars.IniPath + 'moc.var';
  ParameterFileName:= PChar(s);
  InitOffsets;
end;

function ParseGCode(FileName: string; UnitCode,InitCode: string): integer;
begin
  Result:= -1;
  if not Assigned(Renderer) then
    begin
      writeln('glcanon: gl_renderer = nil!');
      Exit;
    end;
  Init;
  {$ifdef VER_23}
  if maxerror < 0 then
    InitGCode;
  {$endif}
  // Renderer.Clear(False);
  Result:= parsefile(PChar(FileName),PChar(UnitCode),PChar(InitCode));
  if linearUnitConversion = LINEAR_UNITS_MM then
    writeln('Units are mm') else writeln('Units are Inches');
  if glMetric then
    writeln('Canon is mm') else writeln('Canon is Inch');
end;

procedure AppendTraverse(l: tlo);
begin
  {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Traverse ',l.x,l.y,l.z]));
  {$endif}
  if Assigned(Renderer) then
    Renderer.Traverse(lineno,lo,l);
end;

procedure AppendFeed(l: tlo);
begin
  {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Feed ',l.x,l.y,l.z]));
  {$endif}
 if Assigned(Renderer) then
   Renderer.Feed(lineno,lo,l);
end;

procedure AppendArcFeed(l: tlo);
begin
  {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Arcfeed ',l.x,l.y,l.z]));
  {$endif}
  if Assigned(Renderer) then
    Renderer.ArcFeed(lineno,lo,l);
end;

procedure AppendDwell(x,y,z: double);
begin
  {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Dwell ',x,y,z]));
  {$endif}
  if Assigned(Renderer) then
    Renderer.Dwell(lineno,x,y,z);
end;

{$ifdef VER_24}
procedure set_xy_rotation(t: double); cdecl; export;
begin
end;

procedure use_tool_length_offset(x,y,z,a,b,c,u,v,w: double); cdecl; export;
begin
  // writeln('use_tool_length_offset');
end;

{$endif}
  
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
  {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['Tooloffset: ',zt,xt,wt]));
  {$endif}
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
  SetCoords(offset,offset.x-x,offset.y-y,offset.z-z,a,b,c,u,v,w);
  {$ifdef PRINT_CANON}
  writeln(Format('%s %n %n %n',['set_origin_offsets: ',x,y,z]));
  {$endif}
end;

procedure setplane(pl: integer); cdecl; export;
begin
  Plane:= pl;
end;

function gettool(i: integer): integer; cdecl; export;
begin
  if (i < 0) or (i >= CANON_TOOL_MAX) then
    result:= 0
  else
    with CanonTool do
      begin
        toolno:= Tools[i].toolno;
        xoffset:= ToCanonUnits(Tools[i].xoffset);
        {$ifdef VER_24}
        yoffset:= ToCanonUnits(Tools[i].yoffset);
        {$endif}
        zoffset:= ToCanonUnits(Tools[i].zoffset);
        {$ifdef VER_24}
        aoffset:= ToCanonUnits(Tools[i].aoffset);
        boffset:= ToCanonUnits(Tools[i].boffset);
        coffset:= ToCanonUnits(Tools[i].coffset);
        uoffset:= ToCanonUnits(Tools[i].uoffset);
        voffset:= ToCanonUnits(Tools[i].voffset);
        woffset:= ToCanonUnits(Tools[i].woffset);
        {$endif}
        diameter:= ToCanonUnits(Tools[i].diameter);
        frontangle:= ToCanonUnits(Tools[i].frontangle);
        backangle:= ToCanonUnits(Tools[i].backangle);
        orientation:= Tools[i].orientation;
        Result:= 1;
      end;
end;

procedure changetool(Tool: integer); cdecl; export;
begin
  FirstMove:= True;
  {$ifdef VER_24}
  if (Tool > 0) and (Tool < CANON_TOOL_MAX) then
    begin
      Tools[0]:= Tools[Tool];
      if Assigned(Renderer) then
        Renderer.SetTool(ToCanonUnits(Tools[0].diameter));
    end;
  {$endif}
  {$ifdef PRINT_CANON}
  writeln(Format('%s %d',['change_tool: ',Tool]));
  {$endif}
end;

procedure changetoolnumber(Tool: integer); cdecl; export;
begin
  FirstMove:= True;
  {$ifdef PRINT_CANON}
  writeln(Format('%s %d',['change_tool_number: ',Tool]));
  {$endif}
end;

procedure selecttool(tool: integer); cdecl; export;
begin
  {$ifdef PRINT_CANON} 
  writeln('Select tool: ' + IntToStr(tool));
  {$endif}
end;

procedure setspindlerate(rate: double); cdecl; export;
begin
end;

procedure setfeedrate(rate: double); cdecl; export;
begin
end;

procedure settraverserate(rate: double); cdecl; export;
begin
end;

procedure straighttraverse(x,y,z,a,b,c,u,v,w: double); cdecl; export;
var
  l: Tlo;
begin        
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
      cx:= cx + offset.x;
      cy:= cy + offset.y;
      theta1:= arctan2(o.y - cy, o.x - cx);
      theta2:= arctan2(n.y - cy, n.x - cx);
      rad:= hypot(o.x - cx, o.y - cy);
    end
  else
  if Plane = 3 then
    begin
      SetCoords(n,y1+offset.x,z1+offset.y,x1+offset.z, a, b, c, u, v, w);
      cx:= cx + offset.z;
      cy:= cy + offset.x;
      theta1:= arctan2(o.x - cy, o.z - cx);
      theta2:= arctan2(n.x - cy, n.z - cx);
      rad:= hypot(o.z - cx, o.x - cy);
    end
  else
    begin
      SetCoords(n,z1+offset.x,x1+offset.y,y1+offset.z, a, b, c, u, v, w);
      cx:= cx + offset.y;
      cy:= cy + offset.z;
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
  FirstMove:= False;
  Arc2Segments(end1,end2,axis1,axis2,rot,endp,a,b,c,u,v,w);
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
  FirstMove:= False;
  SetCoords(l,x + offset.x,y + offset.y,z + offset.z,
    a + offset.a, b + offset.b, c + offset.c,
    u + offset.u, v + offset.v, w + offset.w);
  AppendFeed(l);
  lo:= l
end;
    
procedure straightprobe(x,y,z,a,b,c,u,v,w: double); cdecl; export;
begin
  straightfeed(x,y,z,a,b,c,u,v,w);
end;

procedure userdefinedfunction(i: integer; p,q: double); cdecl; export;
begin        
  AppendDwell(lo.x,lo.y,lo.z)
end;
        
procedure dwell(arg: double); cdecl; export;
begin
  DwellTime:= DwellTime + arg;
  AppendDwell(lo.x,lo.y,lo.z)
end;

function getblockdelete: integer; cdecl; export;
begin
  Result:= integer(State.BlockDel);
end;

function toolalongw: integer; cdecl; export;
begin
  result:= integer(State.TloAlongW);
end;

procedure setcomment(const msg: PChar); cdecl; export;
begin
  // writeln(msg);
end;

procedure setmessage(const msg: PChar); cdecl; export;
begin
  // writeln(msg);
end;


end.



