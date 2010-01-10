unit gllist;

{$I mocca.inc}

interface

uses
  Classes, SysUtils,
  mocglb;
  
type
  TListItemType = (ltFeed,ltArcFeed,ltTraverse,ltDwell);

type
  PListItem = ^TListItem;
  TListItem = record
    line: integer;
    ltype: TListItemType;
    l1,l2: tlo;
  end;

type
  TGlList = class
    constructor Create;
    destructor Destroy;
  private
    nTraverse: integer;
    nFeeds: Integer;
    nArcFeeds: integer;
    nDwells: integer;
    ItemList: TList;
    Current: Integer;
  public
    procedure Clear;
    procedure AddTraverse(line: integer; n1,n2: tlo);
    procedure AddFeed(line: integer; n1,n2: tlo);
    procedure AddArcFeed(line: integer; n1,n2: tlo);
    procedure AddDwell(line: integer; x,y,z: double);
    procedure GetExtents(var E: TExtents);
    function  GetInfo: string;
    procedure First;
    function  Get: PListItem;
  end;
  
var
  MyGlList: TGlList;
  


implementation

var
  OutOfMemory: Boolean;

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

constructor TGlList.Create;
begin
  ItemList:= TList.Create;
  OutOfMemory:= False;
  Clear;
end;

destructor TGlList.Destroy;
begin
  Clear;
  ItemList.Free;
  OutOfMemory:= False;
end;

procedure TGlList.First;
begin
  Current:= -1;
end;

procedure TGlList.GetExtents(var E: TExtents);
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
  with E do
    begin
      MinX:= 0; MaxX:= 0;
      MinY:= 0; MaxY:= 0;
      MinZ:= 0; MaxZ:= 0;
    end;
  if ItemList.Count < 1 then
    Exit;
  First;
  P:= Get;
  while P <> nil do
    begin
      Check(P^.l1);
      Check(P^.l2);
      P:= Get;
    end;
end;

function TGlList.Get: PListItem;
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

function TGlList.GetInfo: string;
begin
  Result:= Format('%s %d %d %d %d',['GlList',nTraverse,nFeeds,nArcFeeds,nDwells]);
end;

procedure TGlList.Clear;
begin
  ItemList.Clear;
  nTraverse:= 0;
  nFeeds:= 0;
  nArcFeeds:= 0;
  nDwells:= 0;
  Current:= -1;
end;

procedure TGlList.AddTraverse(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
  P:= NewListItem(ltTraverse,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nTraverse);
end;

procedure TGlList.AddFeed(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
  P:= NewListItem(ltFeed,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nFeeds);
end;

procedure TGlList.AddArcFeed(line: integer; n1,n2: tlo);
var
  P: PListItem;
begin
  P:= NewListItem(ltArcFeed,line,n1,n2);
  if P <> nil then
    ItemList.Add(P);
  inc(nArcFeeds);
end;

procedure TGlList.AddDwell(line: integer; x,y,z: double);
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

  MyGlList:= nil;

end.

