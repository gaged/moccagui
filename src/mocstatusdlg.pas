unit mocstatusdlg;

{$mode objfpc}{$H+}

interface

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

var
  StatusDlgForm: TStatusDlgForm;

implementation

{ TStatusDlgForm }

uses
  emc2pas;
  
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

procedure TStatusDlgForm.GetEmcStatus;
begin
  AddStr('Trajectory:','');
  AddInt('Mode ',trajMode);
  AddDouble('Linear Units',trajlinearUnits);
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
  AddDouble('Rotation XY',taskRotationXY);
  AddBool('Tlo is along W',taskTloIsAlongW);
  AddInt('Program units',taskProgramUnits);
  AddInt('Interpreter Error',taskInterpErrorCode);
  AddDouble('Delay left',taskDelayLeft);
  AddBool('Block Delete is On',taskBlockDelete);
  AddBool('Opt. Stop is On',taskOptStop);

end;

procedure TStatusDlgForm.FormShow(Sender: TObject);
begin
  GetEmcStatus;
end;

initialization
  {$I mocstatusdlg.lrs}

end.

