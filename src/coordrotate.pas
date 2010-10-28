unit coordrotate; 

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs;

type

  { TCoordRotDlg }

  TCoordRotDlg = class(TForm)
    BtnXR: TButton;
    BtnYR: TButton;
    BtnZR: TButton;
    BtnRef: TButton;
    BtnReset: TButton;
    Button1: TButton;
    Button2: TButton;
    EditRX: TEdit;
    EditX: TEdit;
    EditRY: TEdit;
    EditY: TEdit;
    EditRZ: TEdit;
    EditZ: TEdit;
    Label1: TLabel;
    LabelRotX: TLabel;
    LabelRotY: TLabel;
    LabelRotZ: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure BtnRefClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure BtnXRClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure InitControls;
    procedure UpdateLabels;
  public
    { public declarations }
  end; 



function DoCoordRotate: boolean;

implementation

{ TCoordRotDlg }

uses
  math,emc2pas,mocglb,mocjoints,hal;

var
  rx,ry,rz: double;
  px,py: double;

function DoCoordRotate: boolean;
var
  Dlg: TCoordRotDlg;
begin
  Result:= False;
  Application.CreateForm(TCoordRotDlg,Dlg);
  if Assigned(Dlg) then
    begin
      Dlg.InitControls;
      Result:= Dlg.ShowModal = mrOk;	
      Dlg.Free;
    end;
  if Result then SetHalRotation(rx,ry,rz);
end;

function EditVal(const Edit: TEdit; var Value: double): Boolean;
var
  s: string;
begin
  Result:= False;
  if not Assigned(Edit) then Exit;
  s:= Edit.Text;
  if s = '' then
    begin
      Result:= False;
      Value:= 0;
      Exit;
    end;
  try
    Value:= StrToFloat(s);
    Result:= True;
  except
    Edit.Text:= '?';
    Value:= 0;
  end;
end;

procedure TCoordRotDlg.BtnRefClick(Sender: TObject);
begin
  if Sender = nil then ;
  with CoordRef do
    begin
      EditVal(EditRX,x);
      EditVal(EditRY,y);
      EditVal(EditRZ,z);
      IsSet:= True;
    end;
  InitControls;
end;

procedure TCoordRotDlg.BtnResetClick(Sender: TObject);
begin
  if Sender = nil then ;
  with CoordRef do
    begin
      isSet:= False;
      x:= 0; y:= 0; z:= 0;
      rx:= 0; ry:= 0; rz:= 0;
      InitControls;
    end;
end;

procedure CalcRotationX(x1,y1,x2,y2: double);
var
  dx,dy: double;
begin
  dx:= x2 - x1;
  dy:= y2 - y1;
  if dx <> 0 then
    begin
      rz:= RadToDeg(arctan(dy/dx));
      rx:= 0;
      ry:= 0;
    end
  else
    begin
      ShowMessage('Cannot rotate the Z-Axis with a x-Distance of 0.00');
      rx:= 0;
      ry:= 0;
      rz:= 0;
    end;
end;

procedure CalcRotationY(x1,y1,x2,y2: double);
var
  dx,dy: double;
begin
  dx:= x2 - x1;
  dy:= y2 - y1;
  if dy <> 0 then
    begin
      rz:= RadToDeg(arctan(dx/dy));
      rx:= 0;
      ry:= 0;
    end
  else
    begin
      ShowMessage('Cannot rotate the Z-Axis with a y-Distance of 0.00');
      rx:= 0;
      ry:= 0;
      rz:= 0;
    end;
end;

procedure TCoordRotDlg.BtnXRClick(Sender: TObject);
begin
  if Sender = nil then ;
  EditVal(EditX,px);
  EditVal(EditY,py);
  CalcRotationX(CoordRef.x,CoordRef.y,px,py);
  UpdateLabels;
end;

procedure TCoordRotDlg.FormActivate(Sender: TObject);
begin
  if Sender = nil then ;
  {$ifdef LCLGTK2}
  DoBringToFront(Self);
  {$endif}
end;

function PosToStringAxis(Axis: Char): string;
var
  i: integer;
begin
  Result:= '';
  i:= Pos(Axis,Vars.CoordMap) - 1;
  if i < 0 then Exit;
  Result:= PosToString(GetRelPos(i));
end;

procedure TCoordRotDlg.UpdateLabels;
begin
  LabelRotX.Caption:= FloatToStrF(rx, ffFixed, 3,10) + '°';
  LabelRotY.Caption:= FloatToStrF(ry, ffFixed, 3,10) + '°';
  LabelRotZ.Caption:= FloatToStrF(rz, ffFixed, 3,10) + '°';
end;

procedure TCoordRotDlg.InitControls;
begin
  if CoordRef.IsSet then
    begin
      EditRX.Text:= PosToString(CoordRef.x);
      EditRY.Text:= PosToString(CoordRef.y);
      EditRZ.Text:= PosToString(CoordRef.z);
      EditX.Text:= PosToStringAxis('X');
      EditY.Text:= PosToStringAxis('Y');
      EditZ.Text:= PosToStringAxis('Z');
      EditX.Enabled:= True;
      EditY.Enabled:= True;
      EditZ.Enabled:= True;
    end
  else
    begin
      EditRX.Text:= PosToStringAxis('X');
      EditRY.Text:= PosToStringAxis('Y');
      EditRZ.Text:= PosToStringAxis('Z');
      EditX.Text:= '';
      EditY.Text:= '';
      EditZ.Text:= '';
      EditX.Enabled:= False;
      EditY.Enabled:= False;
      EditZ.Enabled:= False;
    end;
  BtnRef.Enabled:= not CoordRef.IsSet;
end;

procedure TCoordRotDlg.FormCreate(Sender: TObject);
begin
  if Sender = nil then ;
  ReadStyle(self,'coordrotate.xml');
end;

initialization
  {$I coordrotate.lrs}

end.

