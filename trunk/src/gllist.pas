unit gllist;

{$I mocca.inc}

interface

uses
  Classes, SysUtils,
  mocglb, glu,gl;
  
type
  TListItemType = (ltFeed,ltArcFeed,ltTraverse,ltDwell,ltTool);

type
  PListItem = ^TListItem;
  TListItem = record
    line: integer;
    ltype: TListItemType;
    l1,l2: tlo;
  end;

type
  TGlRenderer = class
    constructor Create;
    destructor Destroy; override;
  private
    nTraverse: integer;
    nFeeds: Integer;
    nArcFeeds: integer;
    nDwells: integer;
    ItemList: TList;
    Current: Integer;
  public
    procedure First;
    function  Get: PListItem;
    procedure Clear;
    procedure Traverse(line: integer; n1,n2: tlo);
    procedure Feed(line: integer; n1,n2: tlo);
    procedure ArcFeed(line: integer; n1,n2: tlo);
    procedure Dwell(line: integer; x,y,z: double);
    procedure SetTool(Dia: Single);
    function  GetExtents(var E: TExtents): Boolean;
    function  GetInfo: string;
    function  GetCount: integer;
    procedure MakeList(var ListL: gluInt);
  end;
  
var
  Renderer: TGlRenderer;

procedure SetGlColor3(const c: TGlColorItem);
procedure SetGlColor4(const c: TGlColorItem);

implementation

var
  OutOfMemory: Boolean;

procedure SetGlColor3(const c: TGlColorItem);
begin
  glColor3f(c.r,c.g,c.b);
end;

procedure SetGlColor4(const c: TGlColorItem);
begin
  glColor4f(c.r,c.g,c.b,c.a);
end;

function  TGlRenderer.GetCount: integer;
begin
  Result:= ItemList.Count;
end;

procedure TGlRenderer.MakeList(var ListL: gluInt);
var
  P: PListItem;
begin
  if ListL <> 0 then
    glDeleteLists(ListL,1);
  ListL:= glGenLists(1);
  glNewList(ListL, GL_COMPILE);
  First;
  P:= Get;
  //glLineWidth(3);
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
  //glLineWidth(1);
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
      P:= Get;
    end;
  //glLineWidth(1);
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
  inherited Create;
  OutOfMemory:= False;
  ItemList:= TList.Create;
  Clear;
end;

destructor TGlRenderer.Destroy;
begin
  Clear;
  ItemList.Free;
  OutOfMemory:= False;
  inherited;
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
  while P <> nil do
  begin
    if P^.ltype <> ltTool then
      begin
        E:= SetExtents(P^.l1.x,P^.l1.x,P^.l1.y,P^.l1.y,P^.l1.z,P^.l1.z);
        Check(P^.l2);
        Break;
      end;
    P:= Get;
  end;
  P:= Get;
  while P <> nil do
    begin
      if P^.ltype <> ltTool then
        begin
          Check(P^.l1);
          Check(P^.l2);
        end;
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

procedure TGlRenderer.Clear;
begin
  ItemList.Clear;
  nTraverse:= 0;
  nFeeds:= 0;
  nArcFeeds:= 0;
  nDwells:= 0;
  Current:= -1;
end;

procedure TGlRenderer.SetTool(Dia: Single);
var
  P: PListItem;
  n1,n2: tlo;
begin
  SetCoords(n1,Dia,0,0,0,0,0,0,0,0);
  SetCoords(n2,0,0,0,0,0,0,0,0,0);
  P:= NewListItem(ltTool,0,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  writeln('Settool', FloatToStr(Dia));
end;

procedure TGlRenderer.Traverse(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
  P:= NewListItem(ltTraverse,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nTraverse);
end;

procedure TGlRenderer.Feed(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
  P:= NewListItem(ltFeed,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nFeeds);
end;

procedure TGlRenderer.ArcFeed(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
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
  SetCoords(n1,x,y,z,0,0,0,0,0,0);
  SetCoords(n2,0,0,0,0,0,0,0,0,0);
  P:= NewListItem(ltDwell,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nDwells);
end;

initialization

Renderer:= nil;

end.

