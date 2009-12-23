unit simclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, openglcontext, SysUtils, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type

  { TSimClientForm }

  TSimClientForm = class(TForm)
    OpenGLControl: TOpenGLControl;
    sbH: TScrollBar;
    sbV: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbHChange(Sender: TObject);
    procedure sbVChange(Sender: TObject);
  private
    FShowLivePlot: Boolean;
  public
    procedure LoadPreview(FileName,UnitCode,InitCode: string);
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

var
  E: TExtents;

procedure TSimClientForm.LoadPreview(FileName,UnitCode,InitCode: string);
var
  Error: integer;
begin
  if (not Assigned(MyGlList)) or (not Assigned(MyGlView)) then
    Exit;
  Error:= ParseGCode(FileName,UnitCode,InitCode);
  if Error <> 0 then
    LastError:= GetGCodeError(Error);
  MyGlView.UpdateView;
  MyGlView.GetLimits(E);
  MyGlView.Invalidate;
end;

procedure TSimClientForm.ClearPreview;
begin
  if (not Assigned(MyGlList)) or (not Assigned(MyGlView)) then
    Exit;
 // Add code here to clear the preview...
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
begin
  E.MinX:= 0; E.MaxX:= 10;
  E.MinY:= 0; E.MaxY:= 10;
  E.MinZ:= 0; E.MaxZ:= 10;
  if not Assigned(MyGlList) then
    MyGlList:= TGlList.Create;
  if not Assigned(MyGlView) then
    MyGlView:= TGlView.Create(OpenGlControl);
  MyGlView.SetMachineLimits(E);
  MyGlView.ResetView;
  sbV.setParams(InitialRotX,-90,90);
  sbH.SetParams(InitialRotZ,-90,90);
  FShowLivePlot:= True;
end;

procedure TSimClientForm.FormDestroy(Sender: TObject);
begin
  if Assigned(MyGlView) then
    MyGlView.Free;
  if Assigned(MyGlList) then
    MyGlList.Free;
end;

initialization
  {$I simclient.lrs}

end.

