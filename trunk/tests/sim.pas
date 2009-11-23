unit sim;

{$MODE objfpc}
{$H+}

{$define DEBUG_SIM}

interface

uses
  CTypes,SysUtils,Classes;
  
const
  INTP_OK   = 0;
  INTP_EXIT = 1;
  INTP_EXECUTE_FINISH = 2;
  INTP_ENDFILE = 3;
  INTP_FILE_NOT_OPEN = 4;

type

  PLineItem = ^TLineItem;
  TLineItem = record
    ltype: Byte;
    x,y,z,a,b,c: double;
  end;
  
const
  ltStraight = 1;
  ltFeed     = 2;
  ltArc      = 3;

// this is set to true if there is an error building the list of moves
// or set by the user to "true" to abort interpreting
var
  DoAbortBuildList: Boolean;
  
// lineitems is a TList that stores the moves of the machine
// it contains lineitems,arcitems...
var
  LineItems : TList;
  
var
  probe_a: double; external name 'probe_position_a';
  probe_b: double; external name 'probe_position_b';
  probe_c: double; external name 'probe_position_c';
  probe_x: double; external name 'probe_position_x';
  probe_y: double; external name 'probe_position_y';
  probe_z: double; external name 'probe_position_z';
  oa: double; external name 'program_origin_a';
  ob: double; external name 'program_origin_b';
  oc: double; external name 'program_origin_c';
  ox: double; external name 'program_origin_x';
  oy: double; external name 'program_origin_y';
  oz: double; external name 'program_origin_z';
  pa: double; external name 'program_position_a';
  pb: double; external name 'program_position_b';
  pc: double; external name 'program_position_c';
  px: double; external name 'program_position_x';
  py: double; external name 'program_position_y';
  pz: double; external name 'program_position_z';

  active_plane: cint; external name 'active_plane';
  active_slot: integer; external name 'active_slot';

type
  TIntArray = array of integer;
  PIntArray = ^TIntArray;
  TDoubleArray = array of double;
  PDoubleArray = ^TDoubleArray;

function interp_init: cint; cdecl; external; // get ready to run
function interp_execute: cint; cdecl; external;
function interp_executecmd(cmd: PChar; LineNo: cint): cint; cdecl; external;   //used for MDI calls to specify the pseudo MDI line number
function interp_exit: integer; cdecl; external; // stop running
function interp_load_tool_table: integer; cdecl; external;  // load a tool table
function interp_open(const filename: PChar): integer; cdecl; external;  // open a file of NC code
function interp_close: integer; cdecl; external;
function interp_read(const mdi: PChar): integer; cdecl; external;  // read the mdi or the next line of the open NC code file
function interp_reset: integer; cdecl; external; // reset yourself
function interp_restor_params(const filename: PChar): integer; cdecl; external; // restore interpreter variables from a file
function interp_save_params(const filename: PChar; const Params: Pointer): integer; cdecl; external; // save interpreter variables to file
function interp_synch: integer; cdecl; external; // synchronize your internal model with the external world
procedure interp_gcodes(const gcodes: PIntArray); cdecl; external; // copy active G codes into array [0]..[15]
procedure interp_mcodes(const mcodes: PIntArray); cdecl; external; // copy active M codes into array [0]..[9]
procedure interp_settings(const settings: PDoubleArray) cdecl; external; // copy active F, S settings into array [0]..[2]
procedure interp_errortext(error_code: integer; buffer: PChar; max_size: integer); cdecl; external;
// *fixme function interp_setError(const char *fmt, ...);
// *fixme function interp_getfilename(char *file_name, int max_size);
function interp_line_length: integer; cdecl; external;
procedure interp_line_text(Buffer: PChar; MaxSize: integer); cdecl; external;
procedure setrotationxy(line: integer; r: double); cdecl; export;
procedure setoriginoffsets(line: integer; x,y,z,a,b,c: Double); cdecl; export;
procedure settraverserate(rate: integer); cdecl; export;
procedure straigthtraverse(line: integer; x,y,z,a,b,c: double); cdecl; export;
procedure setUnits_inch; cdecl; export;
procedure setUnits_mm; cdecl; export;
procedure selectplane(in_plane: integer); cdecl; export;
procedure splinefeed2(line: integer; x1,y1,x2,y2: double); cdecl; export;
procedure splinefeed3(line: integer; x1,y1,x2,y2,x3,y3: double); cdecl; export;
procedure arcfeed(line: integer; end1,end2,ax1,ax2: Double; rot: integer; end3: Double;
  a,b,c,u,v,w: double); cdecl; export;
procedure straightfeed(line: integer; x,y,z,a,b,c,u,v,w: Double); cdecl; export;
procedure changetoolnumber(slot: integer); cdecl; export;
procedure changetool(slot: integer); cdecl; export;
procedure selecttool(slot: integer); cdecl; export;
procedure straightprobe(line: integer; x,y,z,a,b,c: Double); cdecl; export;
procedure rigidtap(x,y,z: double); cdecl; export;
procedure selectpocket(pocket: integer); cdecl; export;

function read_tool_file(const filename: PChar): integer; cdecl; external;
function settool(slot,id: integer; z,dia,xofs,fang,bang: double;
  orient: integer): integer; cdecl; external;

procedure ResetAll;

var
 maxx,minx: Double;
 maxy,miny: Double;
 maxz,minz: Double;
 midx,midy: Double;

implementation

uses
  math;
  
procedure ResetAll;
begin
  maxx:= 0; minx:= 0;
  maxy:= 0; miny:= 0;
  maxz:= 0; minz:= 0;
  pa:= 0; pb:= 0; pc:= 0; px:= 0; py:= 0; pz:= 0;
  oa:= 0; ob:= 0; oc:= 0; ox:= 0; oy:= 0; oz:= 0;
end;

type
  TArgs = Array[0..8] of Double;
  PArgs = ^TArgs;

function AddLineItem(t: Byte; x,y,z,a,b,c: double): Boolean;
var
  Item: PLineItem;
begin
  // check extents
  if x < minx then minx:= x;
  if x > maxx then maxx:= x;
  if y < miny then miny:= y;
  if y > maxy then maxy:= y;
  if z < minz then minz:= z;
  if z > maxz then maxz:= z;

  // create a new linitem an add it to the list
  Item:= New(PLineItem);
  if Item <> nil then
    begin
      Item^.ltype:= t;
      // Item^.line:= n;
      Item^.x:= x;
      Item^.y:= y;
      Item^.z:= z;
      Item^.a:= a;
      Item^.b:= b;
      Item^.c:= c;
      LineItems.Add(Item);
    end
  else
    DoAbortBuildList:= true;
end;


// this is the pascal implementation of the original axis arcstosegments
// maybe this can be simplified
procedure ArcToSeg1(x1,y1,cx1,cy1: Double; rot: integer; z1,a,b,c,u,v,w: Double);

var
  Steps: integer;
  i: integer;
  x,y,z: Double;
  nx,ny,nz: Double;
  cx,cy: double;
  theta1: Double;
  theta2: Double;
  theta: Double;
  rad: Double;
  
function inter(L,H: Double): Double;
begin
  Result:= L + (H-L) * i / Steps;
end;
  
begin
   if active_plane = 1 then  // XY
     begin
       nx:= x1 + ox;
       ny:= y1 + oy;
       nz:= z1 + oz;
       cx:= cx1 + ox;
       cy:= cy1 + oy;
       theta1:= arctan2(py-cy,px-cx);
       theta2:= arctan2(ny-cy,nx-cx);
       rad:= hypot(px-cx,py-cy);
       if rot < 0 then
         begin
           if theta2 >= theta1 then
             theta2 -= pi * 2
         end
       else
         begin
           if theta2 <= theta1 then
             theta2 += pi * 2
         end;
       steps:= max(8,Round(128 * abs(theta1 - theta2) / pi));
       for i:=  1 to steps do
         begin
           theta:= inter(theta1, theta2);
           x:= cos(theta) * rad + cx;
           y:= sin(theta) * rad + cy;
           z:= inter(pz,nz);
           AddLineItem(ltArc,x,y,z,a,b,c);
           if DoAbortBuildList then
             Break;
         end;
     end
   else
   if active_plane = 3 then   // XZ
     begin
       nx:= y1 + ox;
       ny:= z1 + oy;
       nz:= x1 + oz;
       cx:= cx1 + oz;
       cy:= cy1 + ox;
       theta1:= arctan2(px-cy,pz-cx);
       // theta2:= arctan2(nx-cy,nz-cx);
       theta2:= arctan2(nz-cy,nx-cx);  // test
       rad:= hypot(pz-cx,px-cy);
       
       if rot < 0 then
         begin
           if theta2 >= theta1 then
             theta2 -= pi * 2
         end
       else
         begin
           if theta2 <= theta1 then
             theta2 += pi * 2
         end;
       steps:= max(8,Round(128 * abs(theta1 - theta2) / pi));
       for i:=  1 to steps do
         begin
           theta:= inter(theta1, theta2);
           z:= cos(theta) * rad + cx;  // z
           x:= sin(theta) * rad + cy;  // x
           y:= py; // inter(px,nx);           // ny?
           AddLineItem(ltArc,x,y,z,a,b,c);
           if DoAbortBuildList then
             Break;
         end;
     end
   else
     begin  // YZ
       nx:= z1 + ox;
       ny:= x1 + oy;
       nz:= y1 + oz;
       cx:= cx1 + oy;
       cy:= cy1 + oz;
       theta1:= arctan2(pz-cy,py-cx);
       theta2:= arctan2(nz-cy,ny-cx);
       rad:= hypot(py-cx,pz-cy);
       if rot < 0 then
         begin
           if theta2 >= theta1 then
             theta2 -= pi * 2
         end
       else
         begin
           if theta2 <= theta1 then
             theta2 += pi * 2
         end;
       steps:= max(8,Round(128 * abs(theta1 - theta2) / pi));
       for i:=  1 to steps do
         begin
           theta:= inter(theta1, theta2);
           y:= cos(theta) * rad + cx;  // z
           z:= sin(theta) * rad + cy;  // x
           x:= px; // inter(px,nx);           // y
           AddLineItem(ltArc,x,y,z,a,b,c);
           if DoAbortBuildList then
             Break;
         end;
     end;
   { n[3]:= a; n[4]:= b; n[5]:= c; n[6]:= u; n[7]:= v; n[8]:= w;}
   
   {$ifdef DEBUG_SIM}
   writeln('arcsegments1');
   {$endif}
end;

procedure ArcToSeg2(x1,y1,cx1,cy1: Double; rot: integer; z1,a,b,c,u,v,w: Double);

var
  Steps: integer;
  i: integer;
  x,y,z: Double;
  nx,ny,nz: Double;
  cx,cy: double;
  theta1: Double;
  theta2: Double;
  theta: Double;
  rad: Double;
  n: array[0..2] of Double;
  p: array[0..2] of Double;
  o: array[0..2] of Double;
  xyz: array[0..2] of Byte;

function inter(L,H: Double): Double;
begin
  Result:= L + (H-L) * i / Steps;
end;

begin

  {$ifdef DEBUG_SIM}
    writeln('origin' + Format('%n %n %n',[ox,oy,oz]));
  {$endif}


   if active_plane = 1 then
     begin
       n[0]:= x1 + ox; n[1]:= y1 + oy; n[2]:= z1 + oz;
       cx:= cx1 + ox;
       cy:= cy1 + oy;
       xyz[0]:= 0; xyz[1]:= 1; xyz[2]:= 2;
     end
   else
   if active_plane = 3 then
     begin
       n[0]:= y1 + ox; n[1]:= z1 + oy; n[2]:= x1 + oz;
       cx:= cx1 + oz;
       cy:= cy1 + ox;
       xyz[0]:= 2; xyz[1]:= 0; xyz[2]:= 1;
     end
   else
     begin
       n[0]:= z1 + ox; n[1]:= x1 + oy; n[2]:= y1 + oz;
       cx:= cx1 + oy;
       cy:= cy1 + ox;
       xyz[0]:= 1; xyz[1]:= 2; xyz[2]:= 0;
     end;

   o[0]:= px; o[1]:= py; o[2]:= pz;

   theta1:= arctan2(o[xyz[1]]-cy,o[xyz[0]]-cx);
   theta2:= arctan2(n[xyz[1]]-cy,n[xyz[0]]-cx);
   rad:= hypot(p[xyz[0]]-cx,p[xyz[1]]-cy);
   { n[3]:= a; n[4]:= b; n[5]:= c; n[6]:= u; n[7]:= v; n[8]:= w;}

   if rot < 0 then
     begin
       if theta2 >= theta1 then
         theta2:= theta2 - (pi * 2);
     end
   else
     begin
       if theta2 <= theta1 then
         theta2:= theta2 + (pi * 2)
     end;

   steps:= max(8,Round(int(128 * abs(theta1 - theta2) / pi)));

   for i:=  1 to steps do
     begin
       theta:= inter(theta1, theta2);
       p[xyz[0]]:= cos(theta) * rad + cx;
       p[xyz[1]]:= sin(theta) * rad + cy;
       p[xyz[2]]:= inter(o[xyz[2]],n[xyz[2]]);
      { p[3]:= inter(o[3], n[3]);
       p[4]:= inter(o[4], n[4]);
       p[5]:= inter(o[5], n[5]);
       p[6]:= inter(o[6], n[6]);
       p[7]:= inter(o[7], n[7]);
       p[8]:= inter(o[8], n[8]); }
       AddLineItem(ltArc,p[0],p[1],p[2],0,0,0);
       if DoAbortBuildList then
         Break;
     end;
   //if not DoAbortBuildList then
   //  AddLineItem(ltArc,n[0],n[1],n[2],0,0,0);
   {$ifdef DEBUG_SIM}
   writeln('arcsegments2');
   {$endif}
end;

procedure setrotationxy(line: integer;r: double); cdecl; export;  
begin
  {$ifdef DEBUG_SIM}
    writeln('setrotationxy' );
  {$endif}
end;

procedure setoriginoffsets(line: integer; x,y,z,a,b,c: Double); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('setoffset' + Format('%d %n %n %n',[line,x,y,z]));
  {$endif}
end;

procedure settraverserate(rate: integer); cdecl; export; 
begin
end;

procedure straigthtraverse(line: integer; x,y,z,a,b,c: double); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('straighttraverse: ' + Format('%d %n %n %n',[line,x,y,z]));
  {$endif}
  AddLineItem(ltStraight,x,y,z,a,b,c);
end;

procedure setUnits_inch; cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('setunits inch');
  {$endif}
end;

procedure setUnits_mm; cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('setunits mm');
  {$endif}
end;

procedure selectplane(in_plane: integer); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('selectplane '+Format('%d',[in_plane]));
  {$endif}

end;

procedure splinefeed2(line: integer; x1,y1,x2,y2: double); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('splinefeed2 ' + Format('%d %n %n %n %n',[line,x1,y1,x2,y2]));
  {$endif}
end;

procedure splinefeed3(line: integer; x1,y1,x2,y2,x3,y3: double); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('splinefeed3: '+Format('%d %n %n %n %n %n %n',[line,x1,y1,x2,y2,x3,y3]));
  {$endif}
end;

// Arcfeed:
// first_end, second_end, first_axis, second_axis, int rot, axis_end_point
// a,b,c,u,v,w);
procedure arcfeed(line: integer; end1,end2,ax1,ax2: Double; rot: integer;
  end3,a,b,c,u,v,w: double); cdecl; export;
begin
  {$ifdef DEBUG_SIM}
    writeln('arcfeed: '+Format('%d %n %n %n %n %n %d',[line,end1,end2,end3,ax1,ax2,rot]));
  {$endif}
  {$ifdef DEBUG_SIM}
    writeln('arcfeed at: '+Format('%n %n %n',[px,py,pz]));
  {$endif}
   ArcToSeg1(end1,end2,ax1,ax2,rot,end3,a,b,c,u,v,w);

end;

procedure straightfeed(line: integer; x,y,z,a,b,c,u,v,w: Double); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('feed: '+Format('%d %n %n %n',[line,x,y,z]));
  {$endif}
  AddLineItem(ltFeed,x,y,z,a,b,c);
end;

procedure changetoolnumber(slot: integer); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('changetoolnumber '+Format('%d',[slot]));
  {$endif}
end;

procedure changetool(slot: integer); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('changetool ' + Format('%d',[slot]));
  {$endif}
end;

procedure selecttool(slot: integer); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('selecttool '+Format('%d',[slot]));
  {$endif}
end;

procedure straightprobe(line: integer; x,y,z,a,b,c: Double); cdecl; export;
begin
  {$ifdef DEBUG_SIM}
    writeln('probe '+Format('%d %n %n %n',[line,x,y,z]));
  {$endif}
end;

procedure rigidtap(x,y,z: double); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('tap '+Format('%n %n %n',[x,y,z]));
  {$endif}
end;

procedure selectpocket(pocket: integer); cdecl; export; 
begin
  {$ifdef DEBUG_SIM}
    writeln('selectpocket '+Format('%d',[pocket]));
  {$endif}
end;

end.
