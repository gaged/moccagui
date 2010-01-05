unit simclient;

{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  {$IFNDEF OWNGL}
  OpenGlContext;
  {$ELSE}
  glcontext;
  {$ENDIF}

type

  { TSimClientForm }

  TSimClientForm = class(TForm)
    Bevel: TBevel;
    sbH: TScrollBar;
    sbV: TScrollBar;
    BtnP: TSpeedButton;
    BtnX: TSpeedButton;
    BtnY: TSpeedButton;
    BtnZ: TSpeedButton;
    procedure BevelResize(Sender: TObject);
    procedure BtnPClick(Sender: TObject);
    procedure BtnXClick(Sender: TObject);
    procedure BtnYClick(Sender: TObject);
    procedure BtnZClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GlPanelResize(Sender: TObject);
    procedure sbHChange(Sender: TObject);
    procedure sbVChange(Sender: TObject);
  private
    FShowLivePlot: Boolean;
    FInitialized: Boolean;
    {$IFNDEF OWNGL}
    ogl: TOpenGlControl;
    {$ELSE}
    ogl: TGlControl;
    {$ENDIF}
  public
    procedure LoadPreview(FileName,UnitCode,InitCode: string);
    procedure UpdateLimits;
    procedure ClearPreview;
    procedure UpdateSelf;
    property ShowLivePlot: Boolean read FShowLivePlot write FShowLivePlot;
  end;
  
var
  clSim: TSimClientForm;

implementation

uses
  mocjoints,emc2pas,
  mocglb, glView,glList,glCanon;

procedure TSimClientForm.LoadPreview(FileName,UnitCode,InitCode: string);
var
  Error: integer;
begin
  if (not Assigned(MyGlList)) or (not Assigned(MyGlView)) then
    Exit;
  UpdateLimits;
  Error:= ParseGCode(FileName,UnitCode,InitCode);
  if Error <> 0 then
    LastError:= GetGCodeError(Error);
  MyGlView.UpdateView;
  MyGlView.Invalidate;
end;

procedure TSimClientForm.ClearPreview;
begin
  if (not Assigned(MyGlList)) or (not Assigned(MyGlView)) then
    Exit;
 // Add code here to clear the preview...
end;

procedure TSimClientForm.UpdateLimits;
var
  Axis: TAxis;
  E: TExtents;
begin
  if not Assigned(Joints) then
    Exit;
  with Joints do
    begin
      Axis:= GetAxis(AxisByChar('X'));
      if Axis <> nil then
        begin
          E.MinX:= ToCanonUnits(Axis.MinPosLimit);
          E.MaxX:= ToCanonUnits(Axis.MaxPosLimit);
        end;
      Axis:= GetAxis(AxisByChar('Y'));
      if Axis <> nil then
        begin
          E.MinY:= ToCanonUnits(Axis.MinPosLimit);
          E.MaxY:= ToCanonUnits(Axis.MaxPosLimit);
        end;
      Axis:= GetAxis(AxisByChar('Z'));
      if Axis <> nil then
        begin
          E.MinZ:= ToCanonUnits(Axis.MinPosLimit);
          E.MaxZ:= ToCanonUnits(Axis.MaxPosLimit);
         end;
    end;
  if Assigned(MyGlView) then
    MyGlView.SetMachineLimits(E);
end;

procedure TSimClientForm.UpdateSelf;
var
  ix,iy,iz: integer;
  X,Y,Z: double;
begin
  if FShowLivePlot then
    if Assigned(Joints) then
      if Assigned(MyGlView) then
        begin
          ix:= Joints.AxisByChar('X');
          iy:= Joints.AxisByChar('Y');
          iz:= Joints.AxisByChar('Z');
          if ix >= 0 then X:= GetRelPos(ix) else Exit;
          if iy >= 0 then Y:= GetRelPos(iy) else Exit;
          if iz >= 0 then Z:= GetRelPos(iz) else Exit;
          MyGlView.MoveCone(x,y,z);
        end;
end;

procedure TSimClientForm.sbHChange(Sender: TObject);
begin
  MyGlView.RotateZ(sbH.Position);
end;

procedure TSimClientForm.sbVChange(Sender: TObject);
begin
  MyGlView.RotateX(sbV.Position);
end;

procedure TSimClientForm.FormCreate(Sender: TObject);
var
  E: TExtents;
begin
  E.MinX:= 0; E.MaxX:= 10;
  E.MinY:= 0; E.MaxY:= 10;
  E.MinZ:= 0; E.MaxZ:= 10;
  if not Assigned(MyGlList) then
    MyGlList:= TGlList.Create;
  if not Assigned(MyGlList) then
    RaiseError('could not create gllist');

  if not Assigned(ogl) then
    {$IFNDEF OWNGL}
    ogl:= TOpenGlControl.Create(Self);
    {$ELSE}
    GlRGBA:= GlSettings.UseRGBA;
    GlDirect:= GlSettings.UseDirect;
    GlDoubleBuffered:= GlSettings.UseDoubleBuffered;
    ogl:= TGlControl.Create(Self);
    {$ENDIF}

  if not Assigned(ogl) then
    RaiseError('could not create opengl-control');

  ogl.Parent:= Self;

  if not Assigned(MyGlView) then
    MyGlView:= TGlView.Create(ogl);
  if not Assigned(MyGlView) then
    RaiseError('could not create glview');

  MyGlView.SetMachineLimits(E);
  MyGlView.ResetView;
  sbV.setParams(InitialRotX,-90,90);
  sbH.SetParams(InitialRotZ,-90,90);
  FShowLivePlot:= True;
end;

procedure TSimClientForm.BtnPClick(Sender: TObject);
begin
  if Assigned(MyGlView) then
    MyGlView.ViewMode(0);
end;

procedure TSimClientForm.BevelResize(Sender: TObject);
begin
 if Assigned(MyGlView) and Assigned(ogl) then
   MyGlView.SetBounds(Bevel.Left,Bevel.Top,Bevel.Width,Bevel.Height);
end;

procedure TSimClientForm.BtnXClick(Sender: TObject);
begin
  if Assigned(MyGlView) then
    MyGlView.ViewMode(1);
end;

procedure TSimClientForm.BtnYClick(Sender: TObject);
begin
  if Assigned(MyGlView) then
    MyGlView.ViewMode(2);
end;

procedure TSimClientForm.BtnZClick(Sender: TObject);
begin
  if Assigned(MyGlView) then
    MyGlView.ViewMode(3);
end;

procedure TSimClientForm.FormDestroy(Sender: TObject);
begin
  if Assigned(MyGlView) then
    FreeAndNil(MyGlView);
  if Assigned(MyGlList) then
    FreeAndNil(MyGlList);
end;

procedure TSimClientForm.FormResize(Sender: TObject);
begin
end;

procedure TSimClientForm.FormShow(Sender: TObject);
begin
  if not FInitialized then
    begin
      UpdateLimits;
      FInitialized:= True;
    end;
end;

procedure TSimClientForm.GlPanelResize(Sender: TObject);
begin
end;

initialization
  {$I simclient.lrs}

end.

