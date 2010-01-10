unit mocglb;

{$I mocca.inc}

interface

uses
  Graphics,
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, ExtCtrls,
  ExtDlgs, ComCtrls,
  mocemc,mocbtn;

{$I mocglb.inc}

var
  Emc2Home: string;  // the path of emc2  /usr/share !?
  LastError: string;
  UpdateLock: Boolean;
  Vars: TEmcVars;
  State: TEmcState;
  GlSettings: TGlSettings;
  MocBtns: Array[0..NumTotalButtons - 1] of TMocButton; // the soft buttons
  GlobalImageList: TImageList;


procedure SetButtonEnabled(ACmd: integer; Enable: Boolean);
procedure SetButtonDown(ACmd: integer; SetDown: Boolean);
procedure SetButtonText(ACmd: integer; AText: string);
procedure SetButtonMap(B: PButtonArray; ObjClick: TOnClick);
procedure SetButtonMapFromTo(B: PButtonArray; iFrom,iTo: integer; ObjCLick: TOnClick);

procedure SetCoords(var l: Tlo; x,y,z,a,b,c,u,v,w: double);
function SetExtents(xa,xb,ya,yb,za,zb: double): TExtents;

procedure RaiseError(const Msg: string);

implementation

procedure SetCoords(var l: Tlo; x,y,z,a,b,c,u,v,w: double);
begin
  l.x:= x; l.y:= y; l.z:= z; l.a:= a;
  l.b:= b; l.c:= c; l.u:= u; l.v:= v;
  l.w:= w;
end;

function SetExtents(xa,xb,ya,yb,za,zb: double): TExtents;
begin
  Result.MinX:= xa; Result.MaxX:= xb;
  Result.MinY:= ya; Result.MaxY:= yb;
  Result.MinZ:= za; Result.MaxZ:= zb;
end;

procedure RaiseError(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure SetButtonMap(B: PButtonArray; ObjClick: TOnClick);
var
  i: Integer;
begin
  if B = nil then Exit;
  for i:= 0 to NumTotalButtons - 1 do
  begin
    if B^[i].T < 0 then
      MocBtns[i].OnClick:= nil
    else
      MocBtns[i].OnClick:= ObjClick;
    if B^[i].G < 0 then
      MocBtns[i].Glyph:= nil
    else
      GlobalImageList.GetBitmap(B^[i].G,MocBtns[i].Glyph);
    MocBtns[i].Tag:= B^[i].T;
    MocBtns[i].Caption:= B^[i].S;
    MocBtns[i].Enabled:= not (B^[i].T < 0);
    MocBtns[i].Down:= False;
  end;
end;

procedure SetButtonMapFromTo(B: PButtonArray; iFrom,iTo: integer; ObjCLick: TOnClick);
var
  i: Integer;
begin
  if B = nil then Exit;
  if (iFrom < 0) or (iFrom > NumTotalButtons - 1) or
    (iTo < 0) or (iTo > NumTotalButtons - 1) or
    (iFrom > iTo) then
      Exit;
  for i:= iFrom to iTo do
  begin
    if B^[i].T < 0 then
      MocBtns[i].OnClick:= nil
    else
      MocBtns[i].OnClick:= ObjClick;
    if B^[i].G < 0 then
      MocBtns[i].Glyph:= nil
    else
      GlobalImageList.GetBitmap(B^[i].G,MocBtns[i].Glyph);
    MocBtns[i].Tag:= B^[i].T;
    MocBtns[i].Caption:= B^[i].S;
    MocBtns[i].Enabled:= not (B^[i].T < 0);
    MocBtns[i].Down:= False;
  end;
end;

procedure SetButtonEnabled(ACmd: integer; Enable: Boolean);
var
  i: integer;
begin
  for i:= 0 to NumTotalButtons - 1 do
    if Assigned(MocBtns[i]) then
      if MocBtns[i].Tag = ACmd then
        begin
          MocBtns[i].Enabled:= Enable;
          Break;
        end;
end;

procedure SetButtonDown(ACmd: integer; SetDown: Boolean);
var
  i: integer;
begin
  for i:= 0 to NumTotalButtons - 1 do
    if Assigned(MocBtns[i]) then
      if MocBtns[i].Tag = ACmd then
        begin
          MocBtns[i].Down:= SetDown;
          Break;
        end;
end;

procedure SetButtonText(ACmd: integer; AText: string);
var
  i: integer;
begin
  for i:= 0 to NumTotalButtons - 1 do
    if Assigned(MocBtns[i]) then
      if MocBtns[i].Tag = ACmd then
        begin
          MocBtns[i].Caption:= AText;
          Break;
        end;
end;

initialization

GlobalImageList:= nil;
LastError:= '';
end.

