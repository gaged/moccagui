unit mocmain;

{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs, ComCtrls,
  mocglb,mocjoints,jogclient,runclient,mdiclient
  {$IFDEF USEGL}
  ,simclient{$ENDIF};

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList: TImageList;
    LabelFText: TLabel;
    LabelF: TLabel;
    LabelMCodes: TLabel;
    LabelGCodes: TLabel;
    LabelMaxVel: TLabel;
    LabelS: TLabel;
    LabelTool: TLabel;
    LabelUnits: TLabel;
    LabelVText: TLabel;
    LabelFeed: TLabel;
    lbMessages: TListBox;
    PanelInf: TPanel;
    PanelMaster: TPanel;
    PanelDRO: TPanel;
    PanelSoftBtns: TPanel;
    PanelMainBtns: TPanel;
    PanelBars: TPanel;
    rgOrigin: TRadioGroup;
    rgCoords: TRadioGroup;
    sbFeed: TScrollBar;
    sbMaxVel: TScrollBar;
    Timer: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);

    procedure FormShow(Sender: TObject);

    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure OnTimer(Sender: TObject);

    procedure PanelMasterResize(Sender: TObject);
    procedure PanelMainBtnsResize(Sender: TObject);
    procedure PanelSoftBtnsResize(Sender: TObject);
    procedure PanelDROResize(Sender: TObject);
    procedure rgCoordsClick(Sender: TObject);
    procedure rgOriginClick(Sender: TObject);

    procedure sbFeedChange(Sender: TObject);
    procedure sbMaxVelChange(Sender: TObject);

  private

    OldFeed: integer;
    OldMaxVel: integer;
    OldTool: integer;

    procedure InitPanels;
    procedure InitButtons;

    procedure UpdateState;
    procedure UpdateButtons;

    procedure TaskModeChanged;  // this is called after the taskmode was changed

    procedure HandleCommand(Cmd: integer);
 end;

var
  MainForm: TMainForm;
  
implementation

{ TMainForm }

uses
  emc2pas,
  mocemc,mocbtn;

procedure TMainForm.HandleCommand(Cmd: integer);
begin
  Emc.HandleCommand(Cmd);
end;

procedure TMainForm.TaskModeChanged;  // called by MainForm.UpdateTaskMode
begin
  clJog.Visible:= (State.TaskMode = TaskModeManual);
  clRun.Visible:= (State.TaskMode = TaskModeAuto);
  clMdi.Visible:= (State.TaskMode = TaskModeMDI);
  if Assigned(Joints) then
    Joints.ShowBox:= (State.TaskMode = TaskModeManual);
  case State.TaskMode of
    TaskModeManual: clJog.ActivateSelf;
    TaskModeAuto: clRun.ActivateSelf;
    TaskModeMDI: clMDI.ActivateSelf;
  end;
end;

procedure TMainForm.UpdateButtons; // called by Main_Form.UpdateState
begin
  MocBtns[0].Down:= State.EStop;
  MocBtns[1].Down:= State.Machine;
end;

procedure TMainForm.UpdateState;
var
  i: integer;
  d,l,Scale: double;
  UpdateMsg: Boolean;
begin
  if EMC.UpdateState then  // returns true if the taskmode changed
    TaskModeChanged;  // mainform.taskmodechanged

  if LastError <> '' then
    begin
      lbMessages.Items.Add(LastError);
      UpdateMsg:= True;
      LastError:= '';
    end;

  if ErrorStr[0] <> #0 then
    begin
      lbMessages.Items.Add(PChar(ErrorStr));
      UpdateMsg:= True;
      ErrorStr[0]:= #0;
    end;

  UpdateButtons;  // Update the Buttons;
  Joints.Update;  // update the joints

  LabelGCodes.Caption:= TrimLeft(PChar(ACTIVEGCODES));
  LabelMCodes.Caption:= TrimLeft(PChar(ACTIVEMCODES));
  LabelF.Caption:= TrimLeft(PChar(ACTIVEFWORDS));
  LabelS.Caption:= TrimLeft(PChar(ACTIVESWORDS));

  {$IFDEF USEGL}
  clSim.UpdateSelf;
  {$ENDIF}

  if OperatorTextStr[0] <> #0 then
    begin
      lbMessages.Items.Add(PChar(OperatorTextStr));
      UpdateMsg:= True;
      OperatorTextStr[0]:= #0;
    end;

  if OperatorDisplayStr[0] <> #0 then
    begin
      lbMessages.Items.Add(PChar(OperatorDisplayStr));
      UpdateMsg:= True;
      OperatorDisplayStr[0]:= #0;
    end;

  if State.TaskMode = TASKMODEMANUAL then
    clJog.UpdateSelf
  else
  if State.TaskMode = TASKMODEAUTO then
    clRun.UpdateSelf
  else
    clMDI.UpdateSelf;

  if State.UnitsChanged then
    begin
      OldMaxVel:= 0;
      if LinearUnitConversion = linear_units_mm then
        LabelUnits.Caption:= 'Einheiten: mm'
      else
        LabelUnits.Caption:= 'Einheiten: Inch';
      State.UnitsChanged:= False;
    end;

  if OldFeed <> State.ActFeed then
    begin
      LabelFeed.Caption:= IntToStr(State.ActFeed) + '%';
      OldFeed:= State.ActFeed;
    end;

  if OldMaxVel <> State.ActVel then
    begin
      i:= Round(Emc.ToLinearUnits(State.ActVel));
      LabelMaxVel.Caption:= IntToStr(i) + Vars.UnitVelStr;
      OldMaxVel:= State.ActVel;
    end;

  if OldTool <> State.CurrentTool then
    begin
      OldTool:= State.CurrentTool;
      if Vars.Metric then Scale:= 1 else Scale:= 25.4;
      if (OldTool < 0) or (OldTool > CANON_TOOL_MAX) then
        begin
          d:= 0;
          l:= 0;
        end
      else
        begin
          d:= Tools[OldTool].diameter / Scale;
          l:= Tools[OldTool].zoffset / Scale;
        end;
      LabelTool.Caption:= Format('%s %d %s %n %s %n',
        ['Tool: ',OldTool,' Dia: ',d,' Z: ',l]);
      if Assigned(clSim) then
        clSim.SetTool(State.CurrentTool);
    end;

    if UpdateMsg then
      begin
        lbMessages.ItemIndex:= lbMessages.Items.Count - 1;
        lbMessages.MakeCurrentVisible;
        UpdateMsg:= False;
      end;
 end;

procedure TMainForm.InitPanels;  // init the panels, clients, joints
var
  h: integer;
begin
  PanelDro.Font.Size:= DroFontSize; // from mocglb.pas
  Joints:= TJoints.Create(PanelDRO);  // create the joints here
  if not Assigned(Joints) then
    RaiseError('joints not initialized.');
  Joints.CreateJoints(Vars.CoordNames,Vars.NumAxes);  // setup joints
  h:= (Joints.BorderWidth + Abs(PanelDro.Font.Height)); // size the joints
  Joints.ShowActual:= Vars.ShowActual;
  Joints.ShowRelative:= Vars.ShowRelative;

  with Vars,State do  // setup mainforms controls
    begin
      ActFeed:= 100;
      sbFeed.Min:= 1;
      sbFeed.Max:= MaxFeedOverride;
      sbFeed.Position:= ActFeed;
      sbMaxVel.Min:= 0;
      sbMaxVel.Max:= MaxVel;
      sbMaxVel.Position:= ActVel;
      if ShowActual then
        rgCoords.ItemIndex:= 0
      else
        rgCoords.ItemIndex:= 1;
      if ShowRelative then
        rgOrigin.ItemIndex:= 0
      else
        rgOrigin.ItemIndex:= 1;
    end;

  OldMaxVel:= 0;
  OldFeed:= 0;
  OldTool:= -1;
  State.UnitsChanged:= True;
end;

procedure TMainForm.InitButtons; // This creates the buttons used by mocca
var
  S: TMocButton;
  i: Integer;
begin
  for i:= 0 to NumTotalButtons - 1 do  // create the buttons
    begin
      S:= TMocButton.Create(self);
      if not Assigned(S) then
        raiseError('Error creating buttons');
      if i < NumSideButtons then
        S.Parent:= PanelMainBtns
      else
        S.Parent:= PanelSoftBtns;
      //S.AllowAllUp:= True;
      //S.GroupIndex:= i+1;
      S.Width:= 64;
      S.Height:= 64;
      S.Tag:= -1;
      S.Transparent:= False;
      S.OnClick:= nil;
      //S.Layout:= blGlyphTop;
      S.Spacing:= 0;
      MocBtns[i]:= S;
    end;
  PanelSoftBtns.ClientHeight:= GlobalButtonSize;  // size the panel
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  State.TaskMode:= 0;  // trigger a taskmodechanged
  UpdateLock:= True; // prevent from update in on_idle

  GlobalImageList:= Self.ImageList;  // assign the imagelist to the global imagelist

  Emc:= TEmc.Create;
  if not Assigned(Emc) then
    RaiseError('Cannot create the emc-class');

  clJog:= TJogClientForm.Create(self); // create the client forms
  if not Assigned(clJog) then
    RaiseError('Error create class "jogclient"');
  clJog.Parent:= PanelMaster;
  clJog.Visible:= False;

  clMDI:= TMDIClientForm.Create(self);
  if not Assigned(clMDI) then
    RaiseError('Error create class "mdiclient"');
  clMDI.Parent:= PanelMaster;
  clMDI.Visible:= False;

  clRun:= TRunClientForm.Create(self);
  if not Assigned(clRun) then
    RaiseError('Error create class "runclient"');
  clRun.Parent:= PanelMaster;
  clRun.Visible:= False;

  {$IFDEF USEGL}
  clSim:= TSimClientForm.Create(self);
  if not Assigned(clSim) then
    RaiseError('Error create class "simclient"');
  clSim.Parent:= PanelMaster;
  clSim.Visible:= False;
  {$ENDIF}

  InitPanels;
  InitButtons;

  Timer.Interval:= Vars.CycleDelay;
  Timer.OnTimer:= @Self.OnTimer;

  Timer.Enabled:= True;
  UpdateLock:= False;

  try
  Emc.LoadTools;
  except
    LastError:= 'Could not load Toolfile';
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  UpdateLock:= True;  // prevent from updates during destroy
  Timer.Enabled:= False;
  if Assigned(clJog) then FreeAndNil(clJog);
  if Assigned(clMDI) then FreeAndNil(clMDI);
  if Assigned(clRun) then FreeAndNil(clRun);
  {$IFDEF USEGL}
  if Assigned(clSim) then FreeAndNil(clSim);
  {$ENDIF}
  for i:= 0 to NumTotalButtons - 1 do
    if Assigned(MocBtns[i]) then FreeAndNil(MocBtns[i]);
  if Assigned(EMC) then FreeAndNil(Emc);
  if Assigned(Joints) then FreeAndNil(Joints);
  GlobalImageList:= nil;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  lbMessages.Height:= 80;
  PanelDroResize(nil);
  PanelMasterResize(nil);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; // all keydowns get here
  Shift: TShiftState);

procedure DoAction(cmd: integer);
begin
  if Assigned(Emc) then
    Emc.HandleCommand(Cmd);
end;

begin
  if Key = 27 then  // handle the escape key first
    begin
      DoAction(cmESTOP);
      Key:= 0;
      Exit;
    end;

  if (Key = 32) and (ssCtrl in Shift) then
    lbMessages.Items.Clear;

  if (State.TaskMode = TaskModeManual) then
    clJog.FormKeyDown(nil,Key,Shift);
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (State.TaskMode = TaskModeManual) then
    if Assigned(clJog) then
      clJog.FormKeyUp(nil,Key,Shift)
end;

procedure TMainForm.OnTimer(Sender: TObject);
begin
  if not UpdateLock then Self.UpdateState;
end;

procedure TMainForm.PanelMasterResize(Sender: TObject);  // the master for the clients
var
  w,h: integer;
begin
  w:= PanelMaster.ClientWidth {$IFDEF USEGL} div 2 {$ENDIF};
  h:= PanelMaster.ClientHeight;
  if Assigned(clJog) then clJog.SetBounds(0,0,w,h);
  if Assigned(clMDI) then clMDI.SetBounds(0,0,w,h);
  if Assigned(clRun) then clRun.SetBounds(0,0,w,h);
  {$IFDEF USEGL}
  if Assigned(clSim) then
    begin
      clSim.SetBounds(w,0,w,h);
      if not clSim.Visible then
        clSim.Visible:= True;
    end;
  {$ENDIF}
end;

procedure TMainForm.PanelSoftBtnsResize(Sender: TObject);
var
  x,i,dx: integer;
  w: single;
begin
  w:= PanelSoftBtns.ClientWidth / NumSoftButtons;
  if w < GlobalButtonSize then
    w:= GlobalButtonSize;
  x:= 0;
  dx:= trunc(w);
  for i:= 0 to NumSoftButtons - 1 do
    if Assigned(MocBtns[i+NumSideButtons]) then
      begin
        x:= round(w * i);
        MocBtns[i+NumSideButtons].SetBounds(x,0,dx,GlobalButtonSize);
      end;
  PanelMainBtns.Left:= x;
  PanelMainBtns.ClientWidth:= dx;
end;

procedure TMainForm.PanelMainBtnsResize(Sender: TObject);
var
  y,i,w: integer;
  h: integer;
begin
  h:= PanelMainBtns.ClientHeight div NumSideButtons;
  if h < GlobalButtonSize then
    h := GlobalButtonSize;
  y:= (h div 2) - (GlobalButtonSize div 2);
  if y < 0 then y:= 0;
  w:= PanelMainBtns.ClientWidth;
  for i:= 0 to NumSideButtons - 1 do
    if Assigned(MocBtns[i]) then
      begin
        MocBtns[i].SetBounds(0,y,w,GlobalButtonSize);
        y:= y + h;
      end;
end;

procedure TMainForm.PanelDROResize(Sender: TObject);
begin
  if Assigned(Joints) then
    Joints.DoResize(nil);
end;

procedure TMainForm.rgCoordsClick(Sender: TObject);
begin
  if Assigned(Joints) then
    Joints.ShowActual:= rgCoords.ItemIndex = 0;
end;

procedure TMainForm.rgOriginClick(Sender: TObject);
begin
  if Assigned(Joints) then
    Joints.ShowRelative:= rgOrigin.ItemIndex = 0;
end;

procedure TMainForm.sbFeedChange(Sender: TObject);
begin
  if UpdateLock then Exit;
  Emc.SetFeedORide(sbFeed.Position);
end;

procedure TMainForm.sbMaxVelChange(Sender: TObject);
begin
  if UpdateLock then Exit;
  Emc.SetMaxVel(sbMaxVel.Position);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PanelDroResize(nil);  // does not work, here for compatibility, doesnt hurt
end;

initialization
  {$I mocmain.lrs}

end.

