unit simulator;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Menus, StdCtrls, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, ExtCtrls,
  glu,gl, mocglb;

type
  PHeightMapArray = ^THeightMapArray;
  THeightMapArray = array[0..0] of Double;

type
  PPlaneArray = ^TPlaneArray;
  TPlaneArray = Array[-1024..1024] of Integer;

type
  TToolDef = record
  Size: integer;
  PA: PPlaneArray;
end;

type
  { TSimulatorDlg }

TSimulatorDlg = class(TForm)
  Bevel1: TBevel;
  BtnRender: TButton;
  BtnCancel: TButton;
  cbSample: TComboBox;
  EditTool: TEdit;
  Label1: TLabel;
  Label2: TLabel;
  Label3: TLabel;
  LabelStatus: TLabel;
  ProgressBar: TProgressBar;
  procedure BtnCancelClick(Sender: TObject);
  procedure BtnRenderClick(Sender: TObject);
  procedure FormShow(Sender: TObject);
  procedure UpdateEdit;
  public

end;

function Show3DPreviewDlg(var AList: gluInt): Boolean;

implementation

uses
  math,gllist;

var
  CountX,CountY: integer;            // The dimensions of the array
  MinX: Double;                      // the left position of the x0 array item
  MinY: Double;                      // the bottom position of the y0 array item
  HeightMap: PHeightMapArray;        // the 2d array of single- values for the z-height map
  Res: Double;                       // resolution of heightmap
  ToolDef: TToolDef;                 // Tool heightmap
  DefaultToolDia: double;
  iList: gluInt;
  // Status: string;
  // DoAbort: Boolean;

function Show3DPreviewDlg(var AList: gluInt): Boolean;
var
  Dlg: TSimulatorDlg;
begin
  Result:= False;
  if not Assigned(Renderer) then
  begin
    writeln('Renderer = nil!');
    Exit;
  end;
  Application.CreateForm(TSimulatorDlg,Dlg);
  iList:= AList;
  try
    Result:= Dlg.ShowModal <> mrCancel;
  finally
    Dlg.Free;
  end;
end;

function NA(x,y: integer): single;
begin
  Result:= HeightMap^[(x*CountX) + y];
end;

procedure CutAt(x,y: integer; z: double);
var
  c: integer;
  OldZ: double;
  ix,iy: integer;
  tx,ty: integer;
  r: integer;
begin
  with ToolDef do
  begin
    if PA = nil then
     begin
       raise Exception.create('Cannot Render without a defined tool')
     end;
    r:= Size div 2;
    for iy:= -r to r do
    begin
      c:= PA^[iy];
      if (c > 0) and (c <= r) then
      begin for ix:= -c to c do
        begin
          tx:= ix + x;
          ty:= iy + y;
          if (tx >= 0) and (ty >= 0) then
          begin
            if (tx < CountX) and (ty < CountY) then
              begin
                OldZ:= HeightMap^[(tx*CountX) + ty];
                if z < OldZ then
                  HeightMap^[(tx*CountX) + ty]:= z;
              end
          end;
        end
      end;
    end;
  end;
end;

procedure CutLine(n1,n2: tlo);
var
  x1,y1,x2,y2: integer;
  dx,dy,dz: double;
  t1: double;
  nx,ny,t: integer;
  // ToolRad: integer;
begin
  x1:= Round((n1.x - MinX) / res);
  y1:= Round((n1.y - MinY) / res);
  x2:= Round((n2.x - MinX) / res);
  y2:= Round((n2.y - MinY) / res);
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
    Inc(t);
  end;
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

procedure ArrayToPos(i,k: integer; var x,y: double);
begin
  x:= (i * Res) + MinX;
  y:= (k * Res) + MinY;
end;

procedure MakeList;
var
  i,k:integer;
  x1,y1,x2,y2,x3,y3: double;
  z1,z2,z3: double;
  //tc: integer;
begin
  writeln('building triangles...');
  //ProgressBar.Max:= CountX * CountY;
  //ProgressBar.Position:= 0;
  glDeleteLists(iList,1);
  glNewList(iList,GL_COMPILE);
  glEnable(GL_NORMALIZE);
  glbegin(gl_triangles);
  for i:=0 to CountX - 2 do
    for k := 0 to CountY -2 do
      begin
        ArrayToPos(i,k,x1,y1);
        ArrayToPos(i+1,k,x2,y2);
        ArrayToPos(i+1,k+1,x3,y3);
        z1:= NA(i,k);
        z2:= NA(i+1,k);
        z3:= NA(i+1,k+1);
        triangle(x3,y3,z3,x2,y2,z2,x1,y1,z1);
        ArrayToPos(i,k,x1,y1);
        ArrayToPos(i+1,k+1,x2,y2);
        ArrayToPos(i,k+1,x3,y3);
        z1:= NA(i,k);
        z2:= NA(i+1,k+1);
        z3:= NA(i,k+1);
        triangle(x3,y3,z3,x2,y2,z2,x1,y1,z1);
      end;
  glend;
  glDisable(GL_NORMALIZE);
  glEndList;
end;

procedure CreateTool(ADia: Single);
var
  i: integer;
  x,y: integer;
  r: integer;
  d: double;
begin
  if ADia = 0 then
    begin
      d:= DefaultToolDia;
    end
  else
    d:= ADia;
  writeln('Tool dia: ' + FloatToStrF(d,ffFixed,6,2));
  if State.LinearUnits = 1 then
    d:= d / 25.4;
  r:= Round((d / 2) / Res);
  if r < 1 then r:= 1;
  i:= r*2;
  if ToolDef.Size = i then
    Exit;
  if i > 1024 then
    raise Exception.Create('Tool is too big.');
  if ToolDef.PA <> nil then
    begin
      writeln('Freeing Tooldef');
      FreeMem(ToolDef.PA)
    end;
  ToolDef.PA:= nil;
  ToolDef.PA:= AllocMem(SizeOf(Integer) * (i + 1));
  if ToolDef.PA = nil then
    raise Exception.Create('Not enough memory to create tool');
  ToolDef.Size:= i;
  with ToolDef do
  begin
    PA^[0]:= r;
    for y:= 0 to r do
     begin
       x:= Round(sqrt((r*r) - (y*y)));
       if x > r then x:= r;
       PA^[y]:= x;
       PA^[-y]:= x;
     end;
  end;
end;

procedure Render3D(SampleRate: Integer);
var
  E: TExtents;
  ExtX,ExtY: double;
  ExtMax: Double;
  x,y: integer;
  P: PListItem;
  tc: integer;
  i: integer;
begin
  ToolDef.Size:= 0;
  ToolDef.PA:= nil;
  if not Renderer.GetExtents(E) then
    begin
      writeln('3D- preview: nothing to draw!');
      Exit;
    end;
  ExtX:= (E.maxX - E.minX);
  ExtY:= (E.maxY - E.minY);
  // ExtZ:= (E.maxZ - E.minZ);
  ExtMax:= 1;
  if ExtX > ExtMax then
    ExtMax:= ExtX;
  if ExtY > ExtMax then
    ExtMax:= ExtY;
  Res:= ExtMax / SampleRate;
  writeln('Sample-Res:' + FloatToStrF(Res,ffFixed,8,4));
  CreateTool(0);
  CountX:= Round(Extx / Res);
  CountY:= Round(Exty / Res);
  if CountX < 1 then
    CountX:= 1;
  if CountY < 1 then
    CountY:= 1;
  MinX:= E.MinX;
  MinY:= E.MinY;
  writeln('Heightmap dimensions: ' + IntToStr(CountX) + 'x' + IntToStr(CountY));
  HeightMap:= AllocMem(SizeOf(Double) * (CountX * CountY));
  if HeightMap = nil then
    begin
      writeln('Noz enough memory for heightmap!');
      Exit;
    end;
  writeln('Heightmap allocated...');
  for x:= 0 to CountX - 1 do
    for y:= 0 to CountY - 1 do
      HeightMap^[(x*CountX) + y]:= E.MaxZ;
  writeln('Heightmap cleared...');
  Renderer.First;
  P:= Renderer.Get;
  if P = nil then
    begin
      writeln('Renderer: First list item is nil!');
      Exit;
    end;
  i:= 0;
  tc:= 0;
  writeln('cutting along toolpath...');
  while P <> nil do
    begin
      if P^.ltype = ltTool then
        CreateTool(P^.l1.x)
      else
      if P^.ltype <> ltDwell then
        CutLine(P^.l1,P^.l2);
      inc(i);
      inc(tc);
      //if tc > 50 then
      //  begin
      //    tc:= 0;
      //    // ProgressBar.Position:= i;
      //   Application.ProcessMessages;
      //  end;
      P:= Renderer.Get;
    end;
  MakeList;
  FreeMem(HeightMap);
  FreeMem(ToolDef.PA);
  writeln('Done!');
end;

procedure TSimulatorDlg.UpdateEdit;
var
  i: integer;
  s: string;
  v: single;
begin
  s:= EditTool.Text;
  i:= Pos(',',s);
  if i > 0 then s[i]:= '.';
  try
    v:= StrToFloat(s);
   if (v <= 0) then
     raise Exception.Create('Invalid Tool Diameter');
  DefaultToolDia:= v;
  except
    on E: Exception do
      begin
       DefaultToolDia:= 0;
       ShowMessage(E.Message);
       EditTool.SetFocus;
      end;
  end;
end;


procedure TSimulatorDlg.BtnRenderClick(Sender: TObject);
var
  iSample: integer;
begin
  UpdateEdit;
  if cbSample.ItemIndex = 0 then iSample:= 100 else
  if cbSample.ItemIndex = 1 then iSample:= 200 else
    iSample:= 300;
  Render3D(iSample);
  ModalResult:= mrOk;
end;

procedure TSimulatorDlg.FormShow(Sender: TObject);
begin
  if Sender = nil then ;
  {$IFDEF LCLGTK2}
  DoBringToFront(Self);
  {$ENDIF}
end;

procedure TSimulatorDlg.BtnCancelClick(Sender: TObject);
begin
  // DoAbort:= True;
end;

initialization
  {$I simulator.lrs}

end.

