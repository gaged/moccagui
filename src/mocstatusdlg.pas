unit mocstatusdlg;

interface

{$I mocca.inc}

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TStatusDlgForm }

  TStatusDlgForm = class(TForm)
    Button1: TButton;
    LbStat: TListBox;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    procedure AddDouble(s: string; d: double);
    procedure AddInt(s: string; i: longint);
    procedure AddBool(s: string; b: Boolean);
    procedure AddStr(s1: string; s2: string);
  public
    procedure GetEmcStatus;
  end; 

procedure ShowStatusDlg;

implementation

{ TStatusDlgForm }

uses
  emc2pas;

var
  Dlg: TStatusDlgForm;

procedure ShowStatusDlg;
begin
  if not Assigned(Dlg) then
    Application.CreateForm(TStatusDlgForm,Dlg);
  Dlg.Show;
end;

procedure TStatusDlgForm.AddDouble(s: string; d: double);
begin
  AddStr(s,FloatToStr(d));
end;

procedure TStatusDlgForm.AddInt(s: string; i: longint);
begin
  AddStr(s,IntToStr(i));
end;

procedure TStatusDlgForm.AddBool(s: string; b: Boolean);
begin
  if b then
    AddStr(s,'true')
  else
    AddStr(s,'false');
end;

procedure TStatusDlgForm.AddStr(s1: string; s2: string);
var
  line: string;
begin
  line:= s1;
  while Length(line) < 26 do line:= line + #32;
  line:= line + s2;
  LbStat.Items.Add(line);
end;

function UnitsToStr(lu: integer): string;
begin
  case lu of
    LINEAR_UNITS_CUSTOM: Result:= 'CUSTOM';
    LINEAR_UNITS_AUTO: Result:= 'AUTO';
    LINEAR_UNITS_MM: Result:= 'MM';
    LINEAR_UNITS_INCH: Result:= 'INCH';
    LINEAR_UNITS_CM: Result:= 'CM';
 else
   Result:= 'UNKNOWN';
 end;
end;

procedure TStatusDlgForm.GetEmcStatus;
var
  i,nAxes: integer;
begin
  AddStr('Trajectory:','');
  AddInt('Mode ',trajMode);
  AddStr('Linearunits',UnitsToStr(linearunitconversion));
  AddDouble('Traj Linearunits',trajlinearUnits);
  AddDouble('Angular Units',trajangularUnits);
  AddDouble('Scale',trajScale);
  AddDouble('Spindle Scale',trajSpindleScale);
  AddDouble('Velocity',trajVel);
  AddDouble('Accel',trajAcceleration);
  AddDouble('Max Velocity',trajMaxVel);
  AddDouble('Distance to go',trajDtg);
  AddDouble('Current velocity',trajCurrentVel);
  AddBool('Feedoverride enabled',trajFeedORideEnabled);
  AddBool('Spindleoverride enabled',trajSpindleORideEnabled);
  AddBool('Adaptive Feed enabled ',trajAdaptiveFeedEnabled);
  AddBool('Feedhold enabled',trajFeedHoldEnabled);
  AddBool('Probing',trajProbing);
  AddBool('Probe tripped',trajProbeTripped);
  AddStr('Task:','');
  AddInt('Task mode',taskMode);
  AddInt('Task state',taskState);
  AddInt('Interpreter state',taskInterpState);
  AddInt('Motionline',taskMotionline);
  AddInt('Currentline',taskCurrentLine);
  AddInt('Readline',taskReadLine);
  // AddDouble('Rotation XY',taskRotationXY);
  {$ifdef VER_23}
  AddBool('Tlo is along W',taskTloIsAlongW);
  {$endif}
  AddInt('Program units',taskProgramUnits);
  AddInt('Interpreter Error',taskInterpErrorCode);
  // AddDouble('Delay left',taskDelayLeft);
  AddBool('Block Delete is On',taskBlockDelete);
  AddBool('Opt. Stop is On',taskOptStop);
  //AddBool('Canon Metric', glMetric);
  AddInt('Program Units',TaskProgramUnits);

  nAxes:= trajAxes;
  if nAxes > 0 then for
    i:= 0 to nAxes - 1 do
      begin
        AddStr('AXIS' + IntToStr(i),'');
        AddInt('Axistype',AxisAxisType(i));
      end;
end;

procedure TStatusDlgForm.FormShow(Sender: TObject);
begin
  GetEmcStatus;
end;

initialization
  {$I mocstatusdlg.lrs}

end.
