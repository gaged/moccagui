unit mocglb;

{$I mocca.inc}

interface

uses
  Graphics,Classes, SysUtils, LResources, Forms, Controls, Dialogs, ExtCtrls,
  ExtDlgs, ComCtrls, mocbtn;

{$I mocglb.inc}

const
  ShowGlPreview: Boolean = True;

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
  GlColors: TGlColors;
  HasScripts: Boolean;

  MocBtns: Array[0..NumButtons - 1] of TMocButton;
  MocScripts: Array[0..NumButtons - 1] of TScriptDef;

  BtnDefScripts: TButtonArray;
  StrListScript: TStringList;
  GlobalBitmaps: TStringList;
  GlobalErrors: TStringList;

  ConfigDir: string;

  BackGroundImage: string;

  ToolCfg : array[0..8] of ToolDef;

  PartBaseCoord : record
    x,y,z: double;
    IsSet: Boolean;
  end;

function PosToString(const Value: Double): string;

procedure SetButtonEnabled(ACmd: integer; Enable: Boolean);
procedure SetButtonDown(ACmd: integer; SetDown: Boolean);
procedure SetButtonText(ACmd: integer; AText: string);
procedure SetButtonMap(M: PButtonArray; ObjClick: TOnClick);
procedure SetCoords(var l: Tlo; x,y,z,a,b,c,u,v,w: double);
function  SetExtents(xa,xb,ya,yb,za,zb: double): TExtents;
procedure RaiseError(const Msg: string);

function AddBitmap(const AName: string): integer;

{$IFDEF LCLGTK2}
procedure FullScreen(WinControl: TWinControl);
procedure UnFullScreen(WinControl: TWinControl);
{$ENDIF}

procedure CallEditor;

procedure ReadStyle(const Form: TForm);

function GetCmdNumber(const C: string): integer;

implementation


uses
 {$IFDEF LCLGTK2}
 GtkDef, gdk2x, gtk2,
 {$ENDIF}
 Process,
 stylereader;

function PosToString(const Value: Double): string;
var
  s: string;
begin
  Result:= '';
  if Value >= 0 then S:= '+';
  if Vars.Metric then
    S:= S + FloatToStrF(Value, ffFixed, 8, 3)
  else
    S:= S + FloatToStrF(Value, ffFixed, 8, 4);
  Result:= S;
end;

function GetCmdNumber(const C: string): integer;
var
  i: integer;
begin
  Result:= -2;
  for i:= 0 to CmdNamesMax do
    begin
      if C = CmdNames[i].S then
        begin
          Result:= CmdNames[i].i;
          Exit;
        end;
    end;
end;

procedure ReadStyle(const Form: TForm);
var
  FileName: string;
  FormName: string;
begin
  if not Assigned(Form) then
    begin
      writeln('Invalid call to "readstyle", Form = nil!');
      Exit;
    end;
  if ConfigDir = '' then
    begin
      writeln('Error reading layout for: ' + Form.Name);
      writeln('Path to xml-styles not set');
      Exit;
    end;
  FormName:= LowerCase(Form.Name);
  FileName:= ConfigDir + FormName + '.xml';
  writeln('Reading layout ' + FileName);
  try
    ReadXMLStyle(Form,FileName);
  except
    writeln('Error reading style: ' + Filename);
  end;
end;

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

procedure SetButtonMap(M: PButtonArray; ObjClick: TOnClick);
var
  i,ii: Integer;
  iBmp: Integer;
begin
  if M <> nil then
    for i:= 0 to NumButtons - 1 do
      begin
        if M^[i].T < 0 then
          MocBtns[i].OnClick:= nil
        else
          MocBtns[i].OnClick:= ObjClick;
        MocBtns[i].Tag:= M^[i].T;
        MocBtns[i].Caption:= M^[i].S;
        MocBtns[i].Enabled:= not (M^[i].T < 0);
        MocBtns[i].ShowClicks:= (M^[i].T > 99);
        MocBtns[i].Down:= False;
        MocBtns[i].Glyph:= nil;
        iBmp:= M^[i].G;
        if (iBmp >= 0) and Assigned(GlobalBitmaps) then
          if Assigned(GlobalBitmaps.Objects[iBmp]) then
            MocBtns[i].Glyph.Assign(TBitmap(GlobalBitmaps.Objects[iBmp]));
      end;
end;

procedure SetButtonEnabled(ACmd: integer; Enable: Boolean);
var
  i: integer;
begin
  for i:= 0 to NumButtons - 1 do
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
  for i:= 0 to NumButtons - 1 do
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
  for i:= 0 to NumButtons - 1 do
    if Assigned(MocBtns[i]) then
      if MocBtns[i].Tag = ACmd then
        begin
          MocBtns[i].Caption:= AText;
          Break;
        end;
end;

function AddBitmap(const AName: string): integer;
var
  i: integer;
  B: TBitmap;
  S,S1: string;
begin
  Result:= -1;
  S1:= Trim(LowerCase(AName));
  if S1 = '' then Exit;
  S:= ConfigDir + S1;
  if not Assigned(GlobalBitmaps) then
    GlobalBitmaps:= TStringList.Create;
  i:= GlobalBitmaps.IndexOf(AName);
  if (i >= 0) then
    begin
      {$ifdef DEBUG_CONFIG}
      writeln('Bitmap already exists in cache.');
      {$endif}
      Result:= i;
      Exit;
    end;
  B:= TBitmap.Create;
  if not Assigned(B) then
    begin
      writeln('Error: cannot create bitmap.');
      Exit;
    end;
  try
    B.LoadFromFile(S);
  except
    B.Free;
    writeln('error loading Bitmap: ' + S);
    Exit;
  end;
  i:= GlobalBitmaps.Add(AName);
  if i < 0 then Exit;
  GlobalBitmaps.Objects[i]:= B;
  Result:= i;
end;


initialization

GlobalBitmaps:= nil;
LastError:= '';
BackGroundImage:= '';

GlSettings.UseDirect:= True;
GlSettings.UseDoubleBuffered:= True;
GlSettings.UseRGBA:= True;

with GlColors do
  begin
    feed.r:= 0; feed.g:= 0; feed.b:= 1;
    traverse.r:= 0.5; traverse.g:= 0.5; traverse.b:= 0.5;
    cone.r:= 1; cone.g:= 0.5; cone.b:= 0; cone.a:= 0.5;
    limits.r:= 0.7; limits.g:= 0.7; limits.b:= 0.6;
    bg.r:= 1; bg.g:= 1; bg.b:= 1;
    table.r:= 0; table.g:= 0; table.b:= 0.9; table.a:= 0.5;
  end;

with Vars.JogIncrements[0] do
  begin
    Value:= 0;
    Text:= 'Continous';
  end;

end.
