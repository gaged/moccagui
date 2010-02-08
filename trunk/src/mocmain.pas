unit mocmain;

{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs, ComCtrls,
  mocglb,mocjoints,jogclient,runclient,mdiclient,
  emcmsgbox
  {$IFDEF USEGL}
  ,simclient
  {$ENDIF};

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    ImageList: TImageList;
    LabelMsg: TLabel;
    LabelR3: TLabel;
    LabelR1: TLabel;
    LabelR2: TLabel;
    LabelR0: TLabel;
    LabelMaxVel: TLabel;
    LabelFeed: TLabel;
    LabelSpORide: TLabel;
    LabelSpVel: TLabel;
    LabelVel: TLabel;
    LabelV1: TLabel;
    LabelDTG: TLabel;
    LabelV0: TLabel;
    LabelL0: TLabel;
    LabelL2: TLabel;
    LabelL1: TLabel;
    LabelCoords: TLabel;
    LabelF: TLabel;
    LabelGCodes: TLabel;
    LabelMCodes: TLabel;
    LabelS: TLabel;
    LabelTool: TLabel;
    LabelUnits: TLabel;
    LabelView: TLabel;
    Panel1: TPanel;
    PanelBars: TPanel;
    PanelDRO: TPanel;
    PanelInf: TPanel;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    PanelSoftBtns: TPanel;
    PanelMainBtns: TPanel;
    SbVel: TScrollBar;
    SbFeed: TScrollBar;
    SbSpORide: TScrollBar;
    ShAtSpeed: TShape;
    Timer: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure LabelCoordsClick(Sender: TObject);
    procedure LabelMsgClick(Sender: TObject);
    procedure LabelUnitsClick(Sender: TObject);
    procedure LabelViewClick(Sender: TObject);

    procedure OnTimer(Sender: TObject);
    procedure PanelDROResize(Sender: TObject);

    procedure PanelLeftResize(Sender: TObject);
    procedure PanelMainBtnsResize(Sender: TObject);
    procedure PanelRightResize(Sender: TObject);
    procedure PanelSoftBtnsResize(Sender: TObject);

    procedure sbFeedChange(Sender: TObject);
    procedure SbSpORideChange(Sender: TObject);
    procedure sbVelChange(Sender: TObject);

  private

    OldFeed: integer;
    OldMaxVel: integer;
    OldDtg: double;
    OldVel: double;
    OldTool: integer;
    // OldLube: Boolean;
    OldFlood: Boolean;
    OldMist: Boolean;
    OldSpDir: integer;
    OldSpVel: double;
    OldAtSpeed: Boolean;
    OldSpORide: integer;
    UpdateCoords: Boolean;

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
  mocemc,mocbtn,
  setup;
  //editorclient;

procedure TMainForm.HandleCommand(Cmd: integer);
begin
  Emc.HandleCommand(Cmd);
end;

procedure TMainForm.TaskModeChanged;  // called by MainForm.UpdateTaskMode
begin
  if ScriptRunning then Exit;
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
  s: string;
  UpdateMsg: Boolean;
begin

  if EMC.UpdateState then
    TaskModeChanged;

  UpdateMsg:= False;

  if LastError <> '' then
    begin
      GlobalErrors.Add(LastError);
      LabelMsg.Caption:= LastError;
      UpdateMsg:= True;
      LastError:= '';
    end;

  if ErrorStr[0] <> #0 then
    begin
      GlobalErrors.Add(PChar(ErrorStr));
      LabelMsg.Caption:= PChar(ErrorStr);
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
      GlobalErrors.Add(PChar(OperatorTextStr));
      UpdateMsg:= True;
      OperatorTextStr[0]:= #0;
    end;

  if OperatorDisplayStr[0] <> #0 then
    begin
      GlobalErrors.Add(PChar(OperatorDisplayStr));
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
        LabelUnits.Caption:= 'mm'
      else
        LabelUnits.Caption:= 'Inch';
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
          s:= 'Kein Werkzeug';
        end
      else
        begin
          d:= Tools[OldTool].diameter / Scale;
          l:= Tools[OldTool].zoffset / Scale;
          s:= PChar(ToolComments[OldTool]);
          if Length(s) < 1 then s:= 'Werkzeug?';
        end;
      LabelTool.Caption:= Format('%d %s %n %s %n %s',
        [OldTool,' D: ',d,' L: ',l,s]);
      {$IFDEF USEGL}
      if Assigned(clSim) then
        clSim.SetTool(State.CurrentTool);
      {$ENDIF}
    end;

  if OldDTG <> State.Dtg then
    begin
      LabelDtg.Caption:= FloatToStrF(State.Dtg, ffFixed, 6, 3) +
        Vars.UnitStr;
      OldDtg:= State.Dtg;
    end;

  if OldVel <> State.Vel then
    begin
      LabelVel.Caption:= FloatToStrF(State.Vel, ffFixed, 6, 3) +
        Vars.UnitVelStr;
      OldVel:= State.Vel;
    end;

  if OldFlood <> State.Flood then
    begin
      SetButtonDown(cmFLOOD,State.Flood);
      OldFlood:= State.Flood;
    end;

  if OldMist <> State.Mist then
    begin
      SetButtonDown(cmMIST,State.Mist);
      OldMist:= State.Mist;
    end;

  if (OldSpVel <> State.SpSpeed) then
    begin
      LabelSpVel.Caption:= FloatToStr(State.SpSpeed) + ' U/min';
      OldSpVel:= State.SpSpeed;
    end;

  if (OldSpORide <> State.ActSpORide) then
    begin
      LabelSpORide.Caption:= IntToStr(State.ActSpORide) + '%';
      OldSpORide:= State.ActSpORide;
    end;

  if OldAtSpeed <> (State.SpIncreasing = 0) then
    begin
      OldAtSpeed:= (State.SpIncreasing = 0);
      if OldAtSpeed then
        ShAtSpeed.Brush.Color:= clGreen
      else
        ShAtSpeed.Brush.Color:= clRed;
    end;

  if OldSpDir <> State.SpDir then
    begin
      SetButtonDown(cmSPCW,State.SpDir > 0);
      SetButtonDown(cmSPCCW,State.SpDir < 0);
      OldSpDir:= State.SpDir;
    end;

  if UpdateMsg then
    begin
      LabelMsgClick(nil);
      UpdateMsg:= False;
    end;

  if UpdateCoords then
    begin
      if Assigned(Joints) then
      with Joints do
        begin
        if ShowActual then
          LabelView.Caption:= 'Aktuell' else
          LabelView.Caption:= 'Befohlen';
        if ShowRelative then
          LabelCoords.Caption:= 'Relativ' else
          LabelCoords.Caption:= 'Absolut';
        end;
      UpdateCoords:= False;
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
      SbFeed.SetParams(ActFeed,1,MaxFeedOverride,1);
      SbVel.SetParams(ActVel,0,MaxVel,1);
      if Vars.MaxSpORide > 100 then
        SbSpORide.SetParams(ActSpORide,MinSpORide,MaxSpORide,1)
      else
        SbSpoRide.Enabled:= False;
     end;
  OldMaxVel:= 0;
  OldFeed:= 0;
  OldTool:= -1;
  OldVel:= -1;
  OldDtg:= -1;
  OldSpDir:= State.SpDir - 1;
  OldSpVel:= -1;
  OldSpORide:= -1;
  State.UnitsChanged:= True;
  UpdateCoords:= True;
end;

procedure TMainForm.InitButtons; // This creates the buttons used by mocca
var
  S: TMocButton;
  i: Integer;
begin
  for i:= 0 to NumAllButtons - 1 do  // create the buttons
    begin
      S:= TMocButton.Create(self);
      if not Assigned(S) then
        raiseError('Error creating buttons');
      if i < NumSButtons then
        S.Parent:= PanelMainBtns
      else
        S.Parent:= PanelSoftBtns;
      S.Width:= 64;
      S.Height:= 64;
      S.Tag:= -1;
      S.OnClick:= nil;
      MocBtns[i]:= S;
    end;
  PanelSoftBtns.ClientHeight:= GlobalButtonSize;  // size the panel
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

  ScriptRunning:= False;
  UpdateLock:= True; // prevent from update in on_idle

  Self.Position:= poScreenCenter;

  if (Vars.WindowSize = 0) then
    Self.WindowState:= wsNormal
  else
  if (Vars.WindowSize = 1) then
    Self.WindowState:= wsMaximized else
  if (Vars.WindowSize = 2) then
    begin
      Self.WindowState:= wsNormal;
      FullScreen(Self);
    end;

  Caption:='Mocca ' + Vars.Machine;

  GlobalFontWidth:= 0;
  GlobalFontHeight:= 0;
  GlobalImageList:= Self.ImageList;  // assign the imagelist to the global imagelist

  GlobalErrors:= TStringList.Create;
  LabelMsg.Caption:= '';

  {$IFDEF LCKGTK2}
  if MainFontSize > 0 then
    Self.Font.Size:= MainFontSize;
  if MainFontBold then
    Self.Font.Style:= [fsBold];
  {$ENDIF}

  State.TaskMode:= 0;  // trigger a taskmodechanged

  MsgForm:= TMsgForm.Create(Self);
  MsgForm.Parent:= Self;

  Emc:= TEmc.Create;
  if not Assigned(Emc) then
    RaiseError('Cannot create the emc-class');

  clJog:= TJogClientForm.Create(self); // create the client forms
  if not Assigned(clJog) then
    RaiseError('Error create class "jogclient"');
  clJog.Parent:= PanelLeft;
  clJog.Visible:= False;

  clMDI:= TMDIClientForm.Create(self);
  if not Assigned(clMDI) then
    RaiseError('Error create class "mdiclient"');
  clMDI.Parent:= PanelLeft;
  clMDI.Visible:= False;

  clRun:= TRunClientForm.Create(self);
  if not Assigned(clRun) then
    RaiseError('Error create class "runclient"');
  clRun.Parent:= PanelLeft;
  clRun.Visible:= False;

  {
  clEditor:= TEditorClient.Create(Self);
  if not Assigned(clEditor) then
    RaiseError('Error create class "editorclient"');
  clEditor.Parent:= PanelLeft;
  clEditor.Visible:= False;
  }

  {$IFDEF USEGL}
  clSim:= TSimClientForm.Create(self);
  if not Assigned(clSim) then
    RaiseError('Error create class "simclient"');
  clSim.Parent:= PanelRight;
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
  // if Assigned(clEditor) then FreeAndNil(clEditor);
  {$IFDEF USEGL}
  if Assigned(clSim) then FreeAndNil(clSim);
  {$ENDIF}
  for i:= 0 to NumAllButtons - 1 do
    if Assigned(MocBtns[i]) then FreeAndNil(MocBtns[i]);
  if Assigned(EMC) then FreeAndNil(Emc);
  if Assigned(Joints) then FreeAndNil(Joints);
  GlobalImageList:= nil;
  if Assigned(GlobalErrors) then GlobalErrors.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  GlobalFontHeight:= Self.Canvas.TextHeight('Üy');
  GlobalFontWidth:= Self.Canvas.TextWidth('X');
  if Assigned(Joints) then
    Joints.DoResize(nil);
  PanelRightResize(nil);
  PanelInf.Width:= (Self.ClientWidth - PanelMainBtns.Width) div 2;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if State.TaskMode = TASKMODEMANUAL then
    begin
      if not Assigned(Joints) then Exit;
      case Key of
        'u'..'z': Joints.SetActiveChar(Key);
      end;
      Key:= #0;
    end;
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
      DoAction(cmABORT);
      ScriptRunning:= false;
      Key:= 0;
      Exit;
    end
  else
  if  (ssCtrl in Shift) then
    begin
      if (Key = 32) then
        begin
          GlobalErrors.Clear;
          LabelMsg.Caption:= '';
          if Assigned(MsgForm) then
          MsgForm.Hide;

        end
      else
      if (Key = 83) then
        EditSetup(Self)
      {$IFDEF LCLGTK2}
      else
      if (Key = 123) then
        begin
          if IsFullScreen then
            UnFullScreen(Self)
          else
            FullScreen(Self);
        end
      {$ENDIF};
    end;

  if (State.TaskMode = TaskModeManual) then
    clJog.FormKeyDown(nil,Key,Shift);
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (State.TaskMode = TaskModeManual) then
    if Assigned(clJog) then
      clJog.FormKeyUp(nil,Key,Shift)
end;

procedure TMainForm.LabelCoordsClick(Sender: TObject);
begin
   if Assigned(Joints) then
    Joints.ShowRelative:= not Joints.ShowRelative;
  UpdateCoords:= True;
end;

procedure TMainForm.LabelMsgClick(Sender: TObject);
begin
  if GlobalErrors.Count > 0 then
    if Assigned(MsgForm) then
      begin
        MsgForm.PopMessages;
        MsgForm.BringToFront;
        Self.Activate;
      end;
end;

procedure TMainForm.LabelUnitsClick(Sender: TObject);
begin
  Emc.HandleCommand(cmUNITS);
end;

procedure TMainForm.LabelViewClick(Sender: TObject);
begin
if Assigned(Joints) then
    Joints.ShowActual:= not Joints.ShowActual;
  UpdateCoords:= True;
end;

procedure TMainForm.OnTimer(Sender: TObject);
begin
  //if not UpdateLock then
  Self.UpdateState;
end;

procedure TMainForm.PanelDROResize(Sender: TObject);
begin
  PanelLeft.Width:= PanelDro.Width;
end;

procedure TMainForm.PanelLeftResize(Sender: TObject);
var
  w,h: integer;
begin
  if Sender <> nil then ;
  h:= PanelDro.Top - 1;
  w:= PanelLeft.ClientWidth;
  if Assigned(clJog) then clJog.SetBounds(0,0,w,h);
  if Assigned(clMDI) then clMDI.SetBounds(0,0,w,h);
  if Assigned(clRun) then clRun.SetBounds(0,0,w,h);
  // if Assigned(clEditor) then clEditor.SetBounds(0,0,w,h);
end;

procedure TMainForm.PanelSoftBtnsResize(Sender: TObject);
var
  x,i,dx: integer;
  w: single;
begin
  w:= PanelSoftBtns.ClientWidth / NumSButtons;
  if w < GlobalButtonSize then
    w:= GlobalButtonSize;
  x:= 0;
  dx:= trunc(w);
  for i:= 0 to NumSButtons - 1 do
    if Assigned(MocBtns[i+NumMButtons]) then
      begin
        x:= round(w * i);
        MocBtns[i+NumMButtons].SetBounds(x,0,dx,GlobalButtonSize);
      end;
  PanelMainBtns.Left:= x;
  PanelMainBtns.ClientWidth:= dx;
end;

procedure TMainForm.PanelMainBtnsResize(Sender: TObject);
var
  y,i,w: integer;
  h: integer;
begin
  h:= PanelMainBtns.ClientHeight div NumMButtons;
  if h < GlobalButtonSize then
    h := GlobalButtonSize;
  y:= (h div 2) - (GlobalButtonSize div 2);
  if y < 0 then y:= 0;
  w:= PanelMainBtns.ClientWidth;
  for i:= 0 to NumMButtons - 1 do
    if Assigned(MocBtns[i]) then
      begin
        MocBtns[i].SetBounds(0,y,w,GlobalButtonSize);
        y:= y + h;
      end;
end;

procedure TMainForm.PanelRightResize(Sender: TObject);
begin
  {$IFDEF USEGL}
  if Assigned(clSim) then
    begin
      clSim.SetBounds(0,0,PanelRight.ClientWidth,PanelRight.ClientHeight);
      if not clSim.Visible then
        clSim.Visible:= True;
    end;
  if Assigned(MsgForm) then
    begin
      MsgForm.Left:= PanelRight.Left;
      MsgForm.Top:= PanelBars.Top - MsgForm.Height;
      MsgForm.Width:= PanelRight.Width;
    end;
  {$ELSE}
  if Assigned(MsgForm) then
    begin
      MsgForm.Left:= PanelBars.Left;
      MsgForm.Top:= PanelBars.Top - MsgForm.Height;
      MsgForm.Width:= PanelBars.Width;
    end;
  {$ENDIF}
end;

procedure TMainForm.sbFeedChange(Sender: TObject);
begin
  if UpdateLock then Exit;
  Emc.SetFeedORide(sbFeed.Position);
end;

procedure TMainForm.SbSpORideChange(Sender: TObject);
begin
  if UpdateLock then Exit;
  Emc.SetSpORide(SbSpORide.Position);
end;

procedure TMainForm.sbVelChange(Sender: TObject);
begin
  if UpdateLock then Exit;
  Emc.SetMaxVel(sbVel.Position);
end;

initialization
  {$I mocmain.lrs}

end.

