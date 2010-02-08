unit mocglb;

{$I mocca.inc}

interface

uses
  Graphics,Classes, SysUtils, LResources, Forms, Controls, Dialogs, ExtCtrls,
  ExtDlgs, ComCtrls,
  mocemc,mocbtn;

{$I mocglb.inc}

var
  LastError: string;

  UpdateLock: Boolean;

  ScriptRunning: Boolean;

  {$IFDEF LCLGTK2}
  IsFullScreen: Boolean;
  {$ENDIF}

  Vars: TEmcVars;
  State: TEmcState;
  GlSettings: TGlSettings;

  MocBtns: Array[0..NumAllButtons - 1] of TMocButton; // the buttons

  HasScripts: Boolean;
  MocScripts: Array[0..NumSButtons - 1] of TScriptDef;

  BtnDefScripts: TSButtonArray;
  StrListScript: TStringList;

  GlobalImageList: TImageList;
  GlobalFontWidth: integer;
  GlobalFontHeight: integer;

  GlobalErrors: TStringList;

  EditorBeginFile: string;
  EditorEndFile: string;

var
  PartBaseCoord : record
    x,y,z: double;
    IsSet: Boolean;
  end;

procedure SetButtonEnabled(ACmd: integer; Enable: Boolean);
procedure SetButtonDown(ACmd: integer; SetDown: Boolean);
procedure SetButtonText(ACmd: integer; AText: string);
procedure SetButtonMap(M: PMButtonArray; S: PSButtonArray; ObjClick: TOnClick);

procedure SetCoords(var l: Tlo; x,y,z,a,b,c,u,v,w: double);
function  SetExtents(xa,xb,ya,yb,za,zb: double): TExtents;

procedure RaiseError(const Msg: string);

{$IFDEF LCLGTK2}
procedure FullScreen(WinControl: TWinControl);
procedure UnFullScreen(WinControl: TWinControl);
{$ENDIF}

procedure CallEditor;

implementation


{$IFDEF LCLGTK2}
uses
  GtkDef, gdk2x, glib2, gdk2, gtk2, Gtk2Int,
  Process;
{$ENDIF}

procedure CallEditor;
var
  Process: TProcess;
begin
  if Vars.Editor = '' then
    raise Exception.Create('No Editor defined!');
  Process:= TProcess.Create(nil);
  try
    Process.CommandLine:= Vars.Editor + #32 + Vars.ProgramFile;
    Process.Execute;
  finally
    Process.Free;
  end;
end;

{$IFDEF LCLGTK2}
procedure FullScreen(WinControl: TWinControl);
begin
  gtk_window_fullscreen(PGtkWindow(WinControl.Handle));
  IsFullScreen:= True;
end;

procedure UnFullScreen(WinControl: TWinControl);
begin
  gtk_window_unfullscreen(PGtkWindow(WinControl.Handle));
  IsFullScreen:= False;
end;
{$ENDIF}

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

procedure SetButtonMap(M: PMButtonArray; S: PSButtonArray; ObjClick: TOnClick);
var
  i,ii: Integer;
begin
  if M <> nil then
    for i:= 0 to NumMButtons - 1 do
      begin
        if M^[i].T < 0 then
          MocBtns[i].OnClick:= nil
        else
          MocBtns[i].OnClick:= ObjClick;
        //if M^[i].G < 0 then MocBtns[i].Glyph:= nil
        //else
        //  GlobalImageList.GetBitmap(M^[i].G,MocBtns[i].Glyph);
        MocBtns[i].Tag:= M^[i].T;
        MocBtns[i].Caption:= M^[i].S;
        MocBtns[i].Enabled:= not (M^[i].T < 0);
        MocBtns[i].ShowClicks:= (M^[i].T > 99);
        MocBtns[i].Down:= False;
      end;
  if S <> nil then
    for i:= 0 to NumSButtons - 1 do
      begin
        ii:= i + NumMButtons;
        if S^[i].T < 0 then
          MocBtns[ii].OnClick:= nil
        else
          MocBtns[ii].OnClick:= ObjClick;
        //if S^[i].G < 0 then
        //  MocBtns[ii].Glyph:= nil
        //else
        //  GlobalImageList.GetBitmap(S^[i].G,MocBtns[ii].Glyph);
        MocBtns[ii].Tag:= S^[i].T;
        MocBtns[ii].Caption:= S^[i].S;
        MocBtns[ii].Enabled:= not (S^[i].T < 0);
        MocBtns[ii].ShowClicks:= (S^[i].T > 99);
        MocBtns[ii].Down:= False;
      end;
end;

procedure SetButtonEnabled(ACmd: integer; Enable: Boolean);
var
  i: integer;
begin
  for i:= 0 to NumAllButtons - 1 do
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
  for i:= 0 to NumAllButtons - 1 do
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
  for i:= 0 to NumAllButtons - 1 do
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

