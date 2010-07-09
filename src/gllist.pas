unit gllist;

{$I mocca.inc}

interface

uses
  Classes, SysUtils,
  mocglb, glu,gl;
  
type
  TListItemType = (ltFeed,ltArcFeed,ltTraverse,ltDwell);

type
  TSingle2dArray = array of array of Single;

type
  PListItem = ^TListItem;
  TListItem = record
    line: integer;
    ltype: TListItemType;
    l1,l2: tlo;
  end;

type
  TArrayDef = record
    CountX,CountY: integer; // The dimensions of the array
    MinX: Double; // the left position of the x0 array item
    MinY: Double; // the bottom position of the y0 array item
    P: TSingle2DArray; // the 2d array of single- values for the z-height map
  end;

type
  TGlRenderer = class
    constructor Create;
    destructor Destroy;
  private
    nTraverse: integer;
    nFeeds: Integer;
    nArcFeeds: integer;
    nDwells: integer;
    ItemList: TList;
    Current: Integer;
    SA: TArrayDef;
    procedure First;
    function  Get: PListItem;
    procedure ArrayToPos(i,k: integer; var x,y: double);
    procedure CreateTool(ADia: Single);
    procedure CutLine(n1,n2: tlo);
    procedure CutAt(x,y: integer; z: double);
  public
    Is3D: Boolean;
    procedure Clear(Reset: Boolean);
    procedure Traverse(line: integer; n1,n2: tlo);
    procedure Feed(line: integer; n1,n2: tlo);
    procedure ArcFeed(line: integer; n1,n2: tlo);
    procedure Dwell(line: integer; x,y,z: double);
    procedure SetTool(Dia: Single);
    function  GetExtents(var E: TExtents): Boolean;
    function  GetInfo: string;
    procedure MakeList(var ListL: gluInt);
    procedure Make3DList(var ListL: gluInt);
    procedure Render3D(SampleRate: integer; AToolDia: single);
  end;
  
var
  Renderer: TGlRenderer;

procedure SetGlColor3(const c: TGlColorItem);
procedure SetGlColor4(const c: TGlColorItem);


var
  RenderState: string;
  RenderReady: Boolean;

implementation

uses
  math;

var
  OutOfMemory: Boolean;

var
  Res: Double;
  LightPosition: Array[0..3] of glFloat;

type
  PPlaneArray = ^TPlaneArray;
  TPlaneArray = Array[-1024..1024] of Integer;

var
  ToolDef : record
    Size: integer;
    P: PPlaneArray;
  end;

procedure TGlRenderer.CreateTool(ADia: Single);
var
  i: integer;
  x,y: integer;
  r: integer;
begin
  r:= Round((ADia/2) / Res);
  if r < 1 then r:= 1;
  i:= r*2;
  if i > 1024 then raise
    Exception.Create('Tool is too big.');
  if ToolDef.P <> nil then
    FreeMem(ToolDef.P);
  ToolDef.P:= nil;
  ToolDef.Size:= i;
  GetMem(ToolDef.P,SizeOf(Integer) * (i + 1));
  if ToolDef.P = nil then
    raise Exception.Create('Not enough memory to create tool');
  with ToolDef do
    begin
      P^[0]:= r;
      for y:= 0 to r do
        begin
          x:= Round(sqrt((r*r) - (y*y)));
          if x > r then x:= r;
          P^[y]:= x;
          P^[-y]:= x;
          writeln('Test: ',x);
        end;
    end;
end;

procedure SetGlColor3(const c: TGlColorItem);
begin
  glColor3f(c.r,c.g,c.b);
end;

procedure SetGlColor4(const c: TGlColorItem);
begin
  glColor4f(c.r,c.g,c.b,c.a);
end;

procedure TGlRenderer.ArrayToPos(i,k: integer; var x,y: double);
begin
  x:= (i * Res) + SA.MinX;
  y:= (k * Res) + SA.MinY;
end;

procedure TGlRenderer.CutAt(x,y: integer; z: double);
var
  c,i,k: integer;
  OldZ: Single;
  ix,iy: integer;
  tx,ty: integer;
  r: integer;
begin
 with ToolDef do
   begin
     if P = nil then
       raise Exception.create('Cannot Render without a defined tool');
     r:= Size div 2;
     for iy:= -r to r do
       begin
         c:= P^[iy];
         if (c > 0) and (c <= r) then
           for ix:= -c to c do
             begin
               tx:= ix + x;
               ty:= iy + y;
               if (tx >= 0) and (ty >= 0) then
                 if (tx < SA.CountX) and (ty < SA.CountY) then
                   begin
                     OldZ:= SA.P[tx,ty];
                     if z < OldZ then
                     SA.P[tx,ty]:= z;
                   end;
             end;
       end;
  end;
end;


procedure TGlRenderer.CutLine(n1,n2: tlo);
var
  x1,y1,x2,y2: integer;
  dx,dy,dz: double;
  t1: double;
  nx,ny,t: integer;
  ToolRad: integer;
begin
  writeln('begin cut loop');
  x1:= Round((n1.x - SA.MinX) / res);
  y1:= Round((n1.y - SA.MinY) / res);
  x2:= Round((n2.x - SA.MinX) / res);
  y2:= Round((n2.y - SA.MinY) / res);

  if(x1 = x2) and (y1 = y2) then
    begin
      CutAt(x1,y1,Min(n1.z,n2.z));
      Exit;
    end;

  dx:= x2 - x1;
  dy:= y2 - y1;
  dz:= n2.z - n1.z;
  t1:= max(abs(dx), abs(dy));

  dx:= dx / t1;
  dy:= dy / t1;
  dz:= dz / t1;

  t:= 0;

  while t <= t1 do
    begin
      nx:= Round(t*dx) + x1;
      ny:= Round(t*dy) + y1;
      CutAt(nx,ny,n1.z + t*dz);
      inc(t);
    end;
  writeln('end cut loop');
 end;

// Render3D: This is a full 3d preview for XYZ mills.
// Render3D can only be called if a file was already loaded and the list was build.
// So first open a file, call parse_file in simclient.pas then call Render3D

procedure TGlRenderer.Render3D(SampleRate: integer; AToolDia: single);
var
  E: TExtents;
  ExtX,ExtY,ExtZ: double;
  ExtMax: Double;
  x,y: integer;
begin
  writeln('Initializing...');
  // RenderProgress:= 0;
  Is3D:= False;
  if ItemList.Count < 1 then
    Exit;
  E:= SetExtents(0,0,0,0,0,0);
  GetExtents(E);
  ExtX:= (E.maxX - E.minX);
  ExtY:= (E.maxY - E.minY);
  ExtZ:= (E.maxZ - E.minZ);
  ExtMax:= 1;
  if ExtX > ExtMax then ExtMax:= ExtX;
  if ExtY > ExtMax then ExtMax:= ExtY;
  Res:= ExtMax / SampleRate;
  writeln('Sample-Res:' + FloatToStrF(Res,ffFixed,8,4));
  // set initial Tool
  CreateTool(AToolDia);
  writeln('Toolsize: ' + IntToStr(ToolDef.Size));
  SA.CountX:= Round(Extx / Res);
  SA.CountY:= Round(Exty / Res);
  if SA.CountX < 1 then SA.CountX:= 1;
  if SA.CountY < 1 then SA.CountY:= 1;
  SA.MinX:= E.MinX;
  SA.MinY:= E.MinY;
  writeln('Array Dimensions: ' + IntToStr(SA.CountX) + 'x' + IntToStr(SA.CountY));
  SetLength(SA.P,SA.CountX,SA.CountY);
  RenderState:= 'clearing Heightmap...';
  for x:= 0 to SA.CountX - 1 do
    for y:= 0 to SA.CountY - 1 do
      SA.P[X,Y]:= E.MaxZ;
  Is3D:= True;
  writeln('Switched to 3D mode');
  // calc the light position
  LightPosition[0]:= E.MinX + (ExtX/ 2);
  LightPosition[1]:= E.MinY + (ExtY / 2);
  LightPosition[2]:= E.MaxZ + ExtMax;
  LightPosition[3]:= 1; // position light (0 = directional light)
end;

procedure triangle(x1,y1,z1,x2,y2,z2,x3,y3,z3: double);
var
  dx1,dy1,dz1,dx2,dy2,dz2: double;
  nx,ny,nz: double;
begin
  dx1 := x2-x1;
  dx2 := x3-x1;
  dy1 := y2-y1;
  dy2 := y3-y1;
  dz1 := z2-z1;
  dz2 := z3-z1;
  nx := dy1 * dz2 - dz1 * dy2;
  ny := dz1 * dx2 - dx1 * dz2;
  nz := dx1 * dy2 - dy1 * dx2;
  glNormal3f(nx, ny, nz);
  glVertex3f(x3,y3,z3);
  glVertex3f(x2,y2,z2);
  glVertex3f(x1,y1,z1);
end;

procedure TGlRenderer.Make3DList(var ListL: gluInt);
var
  i,k:integer;
  x1,y1,x2,y2,x3,y3: double;
  z1,z2,z3: double;
const
  mat_ambient: Array[0..3] of glFLoat = (0.2, 0.2, 0.2,0);
  light_amb: Array[0..3] of glFloat = (0.4,0.3,0.2,1);
  light_dif: Array[0..3] of glFloat = (0.8,0.8,0.6,0);
begin
  if (not is3D) or (SA.P = nil) then
    Exit;
  writeln('building triangles...');
  glDeleteLists(ListL,1);
  glNewList(ListL,GL_COMPILE);
  //glEnable(GL_LIGHTING);
  //glLightfv(GL_LIGHT0, GL_POSITION,LightPosition);
  ////glLightfv(GL_LIGHT1, GL_AMBIENT,light_amb);
  ////glLightfv(GL_LIGHT1, GL_DIFFUSE,light_amb);
  //glEnable(GL_LIGHT0);

  //glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,mat_Ambient);
  ////glDisable(GL_POLYGON_OFFSET_FILL);
  ////glEnable(GL_CULL_FACE);

  glEnable(GL_NORMALIZE);
  glbegin(gl_triangles);
  glcolor3f(255,0,0);
  for i:=0 to SA.CountX - 2 do
    for k := 0 to SA.CountY -2 do
      begin
        //RenderState:= 'scanline: ' + IntToStr(i) + ':' + IntToStr(k);
        ArrayToPos(i,k,x1,y1);
        ArrayToPos(i+1,k,x2,y2);
        ArrayToPos(i+1,k+1,x3,y3);
        z1:= SA.P[i,k];
        z2:= SA.P[i+1,k];
        z3:= SA.P[i+1,k+1];
        triangle(x1,y1,z1,x2,y2,z2,x2,y3,z3);
        ArrayToPos(i,k,x1,y1);
        ArrayToPos(i+1,k+1,x2,y2);
        ArrayToPos(i,k+1,x3,y3);
        z1:= SA.P[i,k];
        z2:= SA.P[i+1,k+1];
        z3:= SA.P[i,k+1];
        triangle(x1,y1,z1,x2,y2,z2,x3,y3,z3);
      end;
  glend;
  // glcolor3f(255,255,255);
  glDisable(GL_NORMALIZE);
  //glDisable(GL_LIGHTING);
  glEndList;
  writeln('...finished!');
end;

procedure TGlRenderer.MakeList(var ListL: gluInt);
var
  P: PListItem;
begin
  if Is3d then Exit;
  glDeleteLists(ListL,1);
  glNewList(ListL,GL_COMPILE);
  First;
  P:= Get;
  glLineWidth(3);
  while (P <> nil) do
    begin
      if (P^.ltype = ltFeed) or (P^.ltype = ltArcFeed) then
        begin
          glBegin(GL_LINES);
          SetGlColor3(GlColors.feed);
          glVertex3f(P^.l1.x,P^.l1.y,P^.l1.z);
          glVertex3f(P^.l2.x,P^.l2.y,P^.l2.z);
          glEnd();
        end;
          P:= Get;
    end;
  First;
  Get;
  glLineWidth(1);
  while (P <> nil) do
    begin
      if (P^.ltype = ltTraverse) or (P^.ltype = ltDwell) then
        begin
          glBegin(GL_LINES);
          SetGlColor3(GlColors.traverse);
          glVertex3f(P^.l1.x,P^.l1.y,P^.l1.z);
          glVertex3f(P^.l2.x,P^.l2.y,P^.l2.z);
          glEnd();
        end;
      P:= Renderer.Get;
    end;
  glLineWidth(1);
  glEndList;
end;

function NewListItem(ItemType: TListItemType; ln: integer; n1,n2: tlo): PListItem;
var
  P: PListItem;
begin
  if OutOfMemory then
    Exit;
  P:= New(PListItem);
  if P <> nil then
    begin
      P^.line:= ln;
      P^.ltype:= ItemType;
      P^.l1:= n1;
      P^.l2:= n2;
    end
  else
    begin
      OutOfMemory:= True;
      writeln('list out of memory');
    end;
  Result:= P;
end;

constructor TGlRenderer.Create;
begin
  Is3D:= False;
  OutOfMemory:= False;
  ItemList:= TList.Create;
  Clear(False);
end;

destructor TGlRenderer.Destroy;
begin
  Clear(True);
  ItemList.Free;
  OutOfMemory:= False;
end;

procedure TGlRenderer.First;
begin
  Current:= -1;
end;

function TGlRenderer.GetExtents(var E: TExtents): Boolean;
var
  P: PListItem;
  
  procedure Check(l: tlo);
  begin
    if l.x < E.MinX then E.MinX:= l.x;
    if l.x > E.MaxX then E.MaxX:= l.x;
    if l.y < E.MinY then E.MinY:= l.y;
    if l.y > E.MaxY then E.MaxY:= l.y;
    if l.z < E.MinZ then E.MinZ:= l.z;
    if l.z > E.MaxZ then E.MaxZ:= l.z;
  end;
  
begin
  Result:= False;
  E:= SetExtents(0,0,0,0,0,0);
  if ItemList.Count < 1 then
    Exit;
  First;
  P:= Get;
  if P <> nil then
  begin
    E:= SetExtents(P^.l1.x,P^.l1.x,P^.l1.y,P^.l1.y,P^.l1.z,P^.l1.z);
    Check(P^.l2);
    P:= Get;
  end;
  while P <> nil do
    begin
      Check(P^.l1);
      Check(P^.l2);
      P:= Get;
    end;
  Result:= True;
end;

function TGlRenderer.Get: PListItem;
begin
  if ItemList.Count < 1 then
    begin
      Result:= nil;
      Exit;
    end;
  inc(Current);
  if Current < ItemList.Count then
    Result:= PListItem(ItemList[Current])
  else
    Result:= nil;
end;

function TGlRenderer.GetInfo: string;
begin
  Result:= Format('%s %d %d %d %d',['GlList',nTraverse,nFeeds,nArcFeeds,nDwells]);
end;

procedure TGlRenderer.Clear(Reset: Boolean);
begin
  ItemList.Clear;
  nTraverse:= 0;
  nFeeds:= 0;
  nArcFeeds:= 0;
  nDwells:= 0;
  Current:= -1;
  if Reset then
    begin
      Is3D:= False;
      SetLength(SA.P,0);
    end;
end;

procedure TGlRenderer.SetTool(Dia: Single);
var
  i: integer;
begin
  if Is3D then
    begin
      writeln('set render tool');
      i:= Round(Dia / Res);
      if i < 2 then i:= 2;
      if i <> ToolDef.Size then
        CreateTool(Dia);
      writeln('render tool: ', ToolDef.Size);
    end;
end;

procedure TGlRenderer.Traverse(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
  if Is3D then
    begin
      CutLine(n1,n2);
      Exit;
    end;
  P:= NewListItem(ltTraverse,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nTraverse);
end;

procedure TGlRenderer.Feed(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
  if Is3D then
    begin
      CutLine(n1,n2);
      Exit;
    end;
  P:= NewListItem(ltFeed,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nFeeds);
end;

procedure TGlRenderer.ArcFeed(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
  if Is3D then
    begin
      CutLine(n1,n2);
      Exit;
    end;
  P:= NewListItem(ltArcFeed,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nArcFeeds);
end;

procedure TGlRenderer.Dwell(line: integer; x,y,z: double);
var
  P: PListItem;
  n1,n2: tlo;
begin
  if Is3D then Exit;
  SetCoords(n1,x,y,z,0,0,0,0,0,0);
  SetCoords(n2,0,0,0,0,0,0,0,0,0);
  P:= NewListItem(ltDwell,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nDwells);
end;

initialization

Renderer:= nil;
ToolDef.Size:= 0;
ToolDef.P:= nil;

end.

