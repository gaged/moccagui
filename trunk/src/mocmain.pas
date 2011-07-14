unit mocmain;

{$I mocca.inc}

interface

uses
  Buttons, Classes, Menus, mocbtn, mocled, mocslider, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, mocglb,
  mocjoints, jogclient, runclient, mdiclient, emcmsgbox, simclient;

type

  { TMainForm }

  TMainForm = class(TForm)
    BgImage: TImage;
    ButtonShowDim: TMocButton;
    ButtonView1: TMocButton;
    ButtonView2: TMocButton;
    ButtonView3: TMocButton;
    ButtonToolPath: TMocButton;
    ButtonClear: TMocButton;
    ButtonView4: TMocButton;
    ButtonViewMinus: TMocButton;
    ButtonViewPlus: TMocButton;
    MenuItemHal: TMenuItem;
    MenuItemScope: TMenuItem;
    MenuItemHalmeter: TMenuItem;
    MenuItemStatus: TMenuItem;
    OEMLed1: TMocLed;
    OEMLabel11: TLabel;
    LedFOREnabled: TMocLed;
    LedSOREnabled: TMocLed;
    ButtonView0: TMocButton;
    OEMLabel1: TLabel;
    OEMLabel2: TLabel;
    OEMLabel3: TLabel;
    OEMLabel4: TLabel;
    OEMLabel5: TLabel;
    LabelFile: TLabel;
    OEMLabel7: TLabel;
    OEMLabel9: TLabel;
    Label2: TLabel;
    OEMLabel10: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OEMLabel12: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabelCurrentVel: TLabel;
    LabelF: TLabel;
    LabelGCodes: TLabel;
    LabelMCodes: TLabel;
    LabelS: TLabel;
    LabelTool: TLabel;
    LabelToolDia: TLabel;
    LabelToolLen: TLabel;
    LabelToolNo: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    LabelMsg: TLabel;
    Label11: TLabel;
    LabelSpVel: TLabel;
    OEMButton1: TMocButton;
    OEMButton2: TMocButton;
    ButtonFeedReset: TMocButton;
    ButtonSpindlebrake: TMocButton;
    ButtonUnitsMM: TMocButton;
    ButtonModeMDI: TMocButton;
    ButtonCoolant: TMocButton;
    ButtonMist: TMocButton;
    ButtonSpReverse: TMocButton;
    ButtonSpindle: TMocButton;
    ButtonEStop: TMocButton;
    ButtonMachine: TMocButton;
    ButtonModeRun: TMocButton;
    ButtonModeAuto: TMocButton;
    ButtonShowRelative: TMocButton;
    ButtonShowDtg: TMocButton;
    LedSpindlebrake: TMocLed;
    LedShowMM: TMocLed;
    LedModeMDI: TMocLed;
    LedEStop: TMocLed;
    LedFlood: TMocLed;
    LedMist: TMocLed;
    LedSpindleCCW: TMocLed;
    LedSpindleOn: TMocLed;
    LedMachineOn: TMocLed;
    LedModeAuto: TMocLed;
    LedModeManual: TMocLed;
    LedShowRelative: TMocLed;
    LedShowDtg: TMocLed;
    OEMLed2: TMocLed;
    OEMLed3: TMocLed;
    OEMLed4: TMocLed;
    OEMLed0: TMocLed;
    PanelDlg: TPanel;
    PanelDRO: TPanel;
    PanelMsg: TPanel;
    PanelPreview: TPanel;
    PanelMaster: TPanel;
    PanelButtons: TPanel;
    PopupMenuMain: TPopupMenu;
    SliderFeed: TSlider;
    SliderVel: TSlider;
    SliderSOR: TSlider;
    Timer: TTimer;

    procedure ButtonClick(Sender: TObject);
    procedure BtnSpCCWClick(Sender: TObject);
    procedure BtnSpClick(Sender: TObject);
    procedure BtnSpReverseClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonEStopClick(Sender: TObject);
    procedure ButtonShowDimClick(Sender: TObject);
    procedure ButtonToolPathClick(Sender: TObject);
    procedure ButtonUnitsMMClick(Sender: TObject);
    procedure ButtonViewClick(Sender: TObject);
    procedure ButtonViewMinusClick(Sender: TObject);
    procedure ButtonViewPlusClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);

    procedure LabelMsgClick(Sender: TObject);
    procedure MenuItemHalClick(Sender: TObject);
    procedure MenuItemHalmeterClick(Sender: TObject);
    procedure MenuItemScopeClick(Sender: TObject);
    procedure MenuItemStatusClick(Sender: TObject);

    procedure OnTimer(Sender: TObject);

    procedure PanelMasterResize(Sender: TObject);
    procedure PanelPreviewResize(Sender: TObject);
    procedure PanelButtonsResize(Sender: TObject);

    procedure SliderFeedPositionChanged(Sender: TObject; NewPos: integer);
    procedure SliderSORPositionChanged(Sender: TObject; NewPos: integer);
    procedure SliderVelPositionChanged(Sender: TObject; NewPos: integer);

  private
    FEStop: Boolean;
    FOn: Boolean;
    FFeed: integer;
    FMaxVel: integer;
    FDtg: double;
    FAcc: double;
    FTool: integer;
    FFlood: Boolean;
    FMist: Boolean;
    FSpDir: integer;
    FSpVel: double;
    FSpORide: integer;
    FSpReverse: Boolean;
    FShowRelative: Boolean;
    FShowDtg: Boolean;
    FCurrentVel: Double;
    FFOREnabled: Boolean;
    FSOREnabled: Boolean;
    FFile: string;

    procedure InitPanels;
    procedure InitButtons;

    procedure UpdateState;

    procedure UpdateSpindle;
    procedure UpdateView(Mode: integer);

    procedure TaskModeChanged;  // this is called after the taskmode was changed

    procedure HandleCommand(Cmd: integer);
 end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }

uses
  lcltype,
  emc2pas,mocemc,hal,startctrl;

procedure TMainForm.HandleCommand(Cmd: integer);
begin
  Emc.HandleCommand(Cmd);
end;

procedure TMainForm.TaskModeChanged;  // called by MainForm.UpdateTaskMode
begin
  // UpdateHalTaskMode(State.Mode);
  if ScriptRunning then
    begin
      if Verbose > 0 then
        writeln('Scrip is running: ignore taskmode');
      Exit;
    end;
  clJog.Visible:= (State.Mode = TaskModeManual);
  clRun.Visible:= (State.Mode = TaskModeAuto);
  clMdi.Visible:= (State.Mode = TaskModeMDI);
  if Assigned(Dro) then
    Dro.ShowBox:= (State.Mode = TaskModeManual);
  case State.Mode of
    TaskModeManual:
      clJog.ActivateSelf;
    TaskModeAuto:
      clRun.ActivateSelf;
    TaskModeMDI:
      clMDI.ActivateSelf;
  end;
  LedModeManual.IsOn:= State.Mode = TaskModeManual;
  LedModeAuto.IsOn:= State.Mode = TaskModeAuto;
  LedModeMDI.IsOn:= State.Mode = TaskModeMDI;
end;

procedure TMainForm.UpdateState;
var
  i: integer;
  d,l: double;
  s: string;
begin

  if not UpdateLock then
    begin
      {i:= GetHalTaskMode;
      if i  <> State.Mode then
        begin
          if i = TaskModeManual then Emc.HandleCommand(cmJOG) else
          if i = TaskModeAuto then Emc.HandleCommand(cmAuto) else
          if i = TaskModeMdi then Emc.HandleCommand(cmMDI);
        end;
      }
      if EMC.UpdateState then
        TaskModeChanged;
    end;

  if LastError <> '' then
    begin
      GlobalErrors.Add(LastError);
      LabelMsg.Caption:= LastError;
      LastError:= '';
    end;

  if ErrorStr[0] <> #0 then
    begin
      GlobalErrors.Add(PChar(ErrorStr));
      LabelMsg.Caption:= PChar(ErrorStr);
      ErrorStr[0]:= #0;
    end;

  Joints.Update;
  if ShowGlPreview then
    clSim.UpdateSelf;

  if OperatorTextStr[0] <> #0 then
    begin
      GlobalErrors.Add(PChar(OperatorTextStr));
      LabelMsg.Caption:= PChar(OperatorTextStr);
      OperatorTextStr[0]:= #0;
    end;

  if OperatorDisplayStr[0] <> #0 then
    begin
      GlobalErrors.Add(PChar(OperatorDisplayStr));
      LabelMsg.Caption:= PChar(OperatorDisplayStr);
      OperatorDisplayStr[0]:= #0;
    end;

  LabelGCodes.Caption:= TrimLeft(PChar(ACTIVEGCODES));
  LabelMCodes.Caption:= TrimLeft(PChar(ACTIVEMCODES));
  LabelF.Caption:= TrimLeft(PChar(ACTIVEFWORDS));
  LabelS.Caption:= TrimLeft(PChar(ACTIVESWORDS));

  if State.UnitsChanged then
    begin
      LedShowMM.IsOn:= Vars.ShowMetric;
      FCurrentVel:= -1; // trigger update fpr Label Currentvel
      FMaxVel:= -1;
    end;

  if FFeed <> Emc.FeedOverride then
    begin
      i:= Emc.FeedOverride;
      SliderFeed.Caption:= IntToStr(i) + '%';
      SliderFeed.Position:= i;
      FFeed:= i;
    end;

  if FMaxVel <> Emc.MaxVelocity then
    begin
      i:= Emc.MaxVelocity;
      SliderVel.Caption:= IntToStr(Round(Emc.ToDisplayUnits(i))) + Vars.UnitVelStr;
      SliderVel.Position:= i;
      FMaxVel:= i;
    end;

  if FTool <> State.CurrentTool then
    begin
      FTool:= State.CurrentTool;
      if (FTool < 0) or (FTool > CANON_TOOL_MAX) then
        begin
          d:= 0;
          l:= 0;
          s:= 'ToolNo out of range: ' + IntToStr(FTool);
        end
      else
        begin
          d:= Tools[FTool].diameter;
          l:= Tools[FTool].zoffset;
          s:= '';
          if FTool > 0 then
            begin
              s:= PChar(ToolComments[FTool]);
              if Length(s) < 1 then
                s:= 'WKZ' + IntToStr(FTool);
            end
          else
            s:= 'Kein Wkz.';
        end;
      LabelTool.Caption:= s;
      LabelToolDia.Caption:= FloatToStr(d);
      LabelToolLen.Caption:= FloatToStr(l);
      LabelToolNo.Caption:= IntToStr(FTool);
      if ShowGlPreview then
        if Assigned(clSim) then
          clSim.UpdateTool(FTool);
    end;

  if FFile <> Vars.ProgramFile then
    begin
      FFile:= Vars.ProgramFile;
      LabelFile.Caption:= FFile;
    end;

  if FCurrentVel <> State.CurrentVel then
    begin
      FCurrentVel:= State.CurrentVel;
      if Vars.ShowMetric then
        d:= State.CurrentVel
      else
        d:= State.CurrentVel / 25.4;
      s:= IntToStr(Round(d * 60)) + #32 + Vars.UnitStr + '/min';
      LabelCurrentVel.Caption:=  s;
    end;

  if FFOREnabled <> State.FeedORideEnabled then
    begin
      FFOREnabled:= State.FeedORideEnabled;
      LedFOREnabled.IsOn:= FFOREnabled;
    end;

  if FSOREnabled <> State.SpindleORideEnabled then
    begin
      FSOREnabled:= State.SpindleORideEnabled;
      LedSOREnabled.IsOn:= FSOREnabled;
    end;

  if FShowRelative <> Dro.Relative then
    begin
      FShowRelative:= Dro.Relative;
      LedShowRelative.IsOn:= not FShowRelative;
    end;

  if FShowDtg <> Dro.Dtg then
    begin
      FShowDtg:= Dro.Dtg;
      LedShowDtg.IsOn:= FShowDtg;
    end;

  if FFlood <> State.Flood then
    begin
      FFlood:= State.Flood;
      LedFlood.IsOn:= FFlood;
    end;

  if FMist <> State.Mist then
    begin
      FMist:= State.Mist;
      LedMist.IsOn:= FMist;
    end;

   if (FSpVel <> State.SpSpeed) then
    begin
      if State.SpDir <> 0 then
        d:= State.SpSpeed * (State.ActSpORide / 100)
      else
        d:= emcSpindleDefaultSpeed * (State.ActSpORide / 100);
      FSpVel:= State.SpSpeed;
      LabelSpVel.Caption:= FloatToStrF(d,ffFixed,8,3) + Vars.UnitRotStr;
    end;

  if (FSpORide <> State.ActSpORide) then
    begin
      SliderSOR.Caption:= IntToStr(State.ActSpORide) + '%';
      SliderSOR.Position:= State.ActSpORide;
      FSpORide:= State.ActSpORide;
      FSpVel:= -1; // trigger a update for LabelSpVel...
    end;

  if FSpDir <> State.SpDir then
    begin
      FSpDir:= State.SpDir;
      if FSpDir <> 0 then
        FSpReverse:= (FSpDir < 0);
      UpdateSpindle;
    end;

  if FEStop <> (State.State = STATE_ESTOP) then
    begin
      FEStop:= (State.State = STATE_ESTOP);
      LedEStop.IsOn:= not FEStop;
    end;

  if FOn <> (State.State = STATE_ON) then
    begin
      FOn:= (State.State = STATE_ON);
      LedMachineOn.IsOn:= FOn;
    end;

  OemLed0.IsOn:= GetHalLedState(0);
  OemLed1.IsOn:= GetHalLedState(1);
  OemLed2.IsOn:= GetHalLedState(2);
  OemLed3.IsOn:= GetHalLedState(3);
  OemLed4.IsOn:= GetHalLedState(4);

  if UpdateLock then Exit;

  if State.Mode = TASKMODEMANUAL then
    clJog.UpdateSelf
  else
  if State.Mode = TASKMODEAUTO then
    clRun.UpdateSelf
  else
    clMDI.UpdateSelf;

  for i:= HalButtonMin to HalButtonMax do
    begin
      if GetHalButtonDown(i) then
        if Assigned(MocBtns[i]) then
          if MocBtns[i].Tag > 0 then
            begin
              if TaskMode = TASKMODEMANUAL then
                clJog.HandleCommand(MocBtns[i].Tag)
              else
                Emc.HandleCommand(MocBtns[i].Tag);
            end;
    end;

  if State.UnitsChanged then
    State.UnitsChanged:= False;

 end;

procedure TMainForm.UpdateSpindle;
begin
  LedSpindleCCW.IsOn:= FSpReverse;
  LedSpindleOn.IsOn:= FSpDir <> 0;
end;

procedure TMainForm.InitPanels;  // init the panels, clients, joints
var
  i: integer;
  B: TMocButton;
begin
  if Assigned(MsgForm) then
    begin
      MsgForm.Left:= PanelMsg.Left;
      i:= PanelMsg.Top - MsgForm.Height;
      if i < Self.Top then
        i:= Self.Top;
      MsgForm.Top:= i;
      MsgForm.Width:= PanelMsg.Width;
    end;
  Joints:= TJoints.Create(PanelDRO);  // create the joints here
  if not Assigned(Joints) then
    RaiseError('joints not initialized.');
  Joints.CreateJoints;  // setup joints
  Dro.Relative:= Vars.ShowRelative;

  with Vars,State do  // setup mainforms controls
    begin
      ActFeed:= 100;
      SliderFeed.SetParams(1,MaxFeedOverride,ActFeed);
      SliderVel.SetParams(0,MaxVel,ActVel);
      if Vars.MaxSpORide > 99 then
        SliderSOR.SetParams(MinSpORide,MaxSpORide,ActSpORide)
      else
        SliderSOR.Enabled:= False;
    end;

  if Assigned(clSim) then
    with clSim do
      begin
        ButtonToolPath.Down:= ShowLivePlot;
        ButtonShowDim.Down:= ShowDimensions;
        ButtonView0.Down:= ViewMode = vmPerspective;
        ButtonView1.Down:= ViewMode = vmZ;
        ButtonView2.Down:= ViewMode = vmY;
        ButtonView3.Down:= ViewMode = vmX;
        ButtonView4.Down:= ViewMode = vmZDown;
      end;

  for i:= 0 to Self.ComponentCount - 1 do
    begin
      if Self.Components[i] is TMocButton then
        begin
          B:= TMocButton(Self.Components[i]);
          if Assigned(B) then
            if B.Command <> '' then
              B.Tag:= GetCmdNumber(B.Command);
        end;
    end;

  FOn:= not (State.State = STATE_ON);
  FMaxVel:= 0;
  FFeed:= 0;
  FTool:= -1;
  FAcc:= -1;
  FDtg:= -1;
  FSpDir:= State.SpDir - 1;
  FSpVel:= -1;
  FSpORide:= -1;
  FShowRelative:= not Dro.Relative;
  State.UnitsChanged:= True;

  InitHalPins;

end;

procedure TMainForm.InitButtons; // This creates the buttons used by mocca
var
  S: TMocButton;
  i: Integer;
begin
  for i:= 0 to NumButtons - 1 do  // create the buttons
    begin
      S:= TMocButton.Create(self);
      if not Assigned(S) then
        raiseError('Error creating buttons');
      S.Parent:= PanelButtons;
      S.Tag:= -1;
      S.OnClick:= nil;
      MocBtns[i]:= S;
    end;
  PanelButtonsResize(nil);
end;

procedure TMainForm.UpdateView(Mode: integer);
var
  OldMode: TViewModes;
begin
  if (Mode < Integer(vmPerspective)) or (Mode > Integer(vmZDown)) then
    Exit;
  if Assigned(clSim) then
    begin
      OldMode:= clSim.ViewMode;
      if OldMode = TViewModes(Mode) then
        Exit;
      clSim.ViewMode:= TViewModes(Mode);
      ButtonView0.Down:= TViewModes(Mode) = vmPerspective;
      ButtonView1.Down:= TViewModes(Mode) = vmZ;
      ButtonView2.Down:= TViewModes(Mode) = vmY;
      ButtonView3.Down:= TViewModes(Mode) = vmX;
      ButtonView4.Down:= TViewModes(Mode) = vmzDown;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  cw,ch: integer;
begin
  if Sender = nil then ;
  MainForm:= Self;
  if not UseDefaultLayout then
    ReadStyle(Self,'mocca.xml')
  else
    writeln('Using default layout for "mainform".');

  ScriptRunning:= False;
  UpdateLock:= True; // prevent from update in on_idle

  {$IFDEF LCLGTK2}
  if InitialFullScreen then
    begin
      cw:= Screen.Width;
      ch:= Screen.Height;
      if (Self.Width < cw) and (Self.Height < ch) then
        InitialFullScreen:= False;
    end;
  if InitialFullscreen then
    FullScreen(Self);
  {$ENDIF};

  Caption:='Mocca ' + Vars.Machine;

  GlobalErrors:= TStringList.Create;
  LabelMsg.Caption:= '';

  // Load the User_Error messages (if available)
  UserErrors:= TStringList.Create;
  if Assigned(UserErrors) then
    try
      UserErrors.LoadFromFile(ConfigDir + 'usererror.txt' );
      if Verbose > 0 then
        writeln('User Errors loaded from ',ConfigDir + 'usererror.txt');
    except
      writeln('Error loading user error file');
    end;

  State.Mode:= 0;  // trigger a taskmodechanged

  MsgForm:= TMsgForm.Create(Self);
  MsgForm.Parent:= Self;

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

  if ShowGlPreview then
    begin
      clSim:= TSimClientForm.Create(self);
      if not Assigned(clSim) then
        RaiseError('Error create class "simclient"');
      clSim.Parent:= PanelPreview;
      clSim.Visible:= False;
    end
  else
    clSim:= nil;

  InitPanels;
  InitButtons;

  FormResize(nil);

  Timer.Interval:= Vars.CycleDelay;
  Timer.OnTimer:= @Self.OnTimer;

  Timer.Enabled:= True;
  UpdateLock:= False;

  // Load Toolfile
  try
    Emc.LoadTools;
  except
    LastError:= 'Could not load Toolfile';
    raise;	
  end;

  // Load Postgui halfile
  LoadPostGuiHal;

end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin
  if Verbose > 0 then writeln('deactivating mainform');
  if State.Mode = TaskModeManual then
    if Assigned(Joints) then Joints.CheckJogExit;
end;

procedure TMainForm.BtnSpCCWClick(Sender: TObject);
begin
  if Sender = nil then ;
  if FSpDir <> 0 then Exit;
  FSpReverse:= True;
  UpdateSpindle;
end;

procedure TMainForm.BtnSpClick(Sender: TObject);
begin
  if Sender = nil then ;
  if FSpReverse then
    HandleCommand(cmSPCCW)
  else
    HandleCommand(cmSPCW);
end;

procedure TMainForm.BtnSpReverseClick(Sender: TObject);
begin
  if Sender = nil then ;
  if FSpDir <> 0 then Exit;
  FSpReverse:= not FSpReverse;
  UpdateSpindle;
end;

procedure TMainForm.ButtonClearClick(Sender: TObject);
begin
  if Sender = nil then ;
  if Assigned(clSim) then clSim.ClearPlot;
end;

procedure TMainForm.ButtonEStopClick(Sender: TObject);
begin
  Emc.HandleCommand(cmESTOP);
end;

procedure TMainForm.ButtonShowDimClick(Sender: TObject);
begin
  if Sender = nil then ;
  if not Assigned(clSim) then Exit;
  clSim.ShowDimensions:= not clSim.ShowDimensions;
  ButtonShowDim.Down:= clSim.ShowDimensions;
  clSim.InvalidateView;
end;

procedure TMainForm.ButtonToolPathClick(Sender: TObject);
begin
  if Sender = nil then ;
  if not Assigned(clSim) then Exit;
  clSim.ShowLivePlot:= not clSim.ShowLivePlot;
  ButtonToolPath.Down:= clSim.ShowLivePlot;
  if not clSim.ShowLivePlot then
    clSim.ClearPlot;
end;

procedure TMainForm.ButtonUnitsMMClick(Sender: TObject);
begin
  if Sender = nil then ;
  Vars.ShowMetric:= not Vars.ShowMetric;
end;

procedure TMainForm.ButtonViewClick(Sender: TObject);
begin
  if Sender <> nil then
    with Sender as TMocButton do
      UpdateView(Tag);
end;

procedure TMainForm.ButtonViewMinusClick(Sender: TObject);
begin
  if Sender = nil then ;
  if Assigned(clSim) then clSim.Zoom(-1);
end;

procedure TMainForm.ButtonViewPlusClick(Sender: TObject);
begin
  if Sender = nil then ;
  if Assigned(clSim) then clSim.Zoom(1);
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  //ShowStartDlg;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  writeln('mainform close');
  CloseAction:= caFree;
  UpdateLock:= True;  // prevent from updates during close
  Timer.OnTimer:= nil;
  Timer.Enabled:= False;  //turn off Timer
end;

procedure TMainForm.MenuItemHalClick(Sender: TObject);
begin
  ExecHalShow;
end;

procedure TMainForm.MenuItemHalmeterClick(Sender: TObject);
begin
  ExecHalMeter;
end;

procedure TMainForm.MenuItemScopeClick(Sender: TObject);
begin
  ExecHalScope;
end;

procedure TMainForm.MenuItemStatusClick(Sender: TObject);
begin
  ExecEmcTop;
end;

procedure TMainForm.ButtonClick(Sender: TObject);
var
  Cmd: integer;
begin
  if Sender = nil then ;
  with Sender as TMocButton do
    Cmd:= Tag;
  if Cmd > 0 then
    Emc.HandleCommand(Cmd);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  if Sender = nil then ;
  if Verbose > 0 then writeln('destroying mainform');
  UpdateLock:= True;  // prevent from updates during destroy
  Timer.Enabled:= False;  //turn off Timer
  // weit message queue
  //sleep(20);
  if Assigned(clJog) then FreeAndNil(clJog);
  if Assigned(clMDI) then FreeAndNil(clMDI);
  if Assigned(clRun) then FreeAndNil(clRun);
  if Assigned(clSim) then FreeAndNil(clSim);
  for i:= 0 to NumButtons - 1 do
    if Assigned(MocBtns[i]) then FreeAndNil(MocBtns[i]);
  if Assigned(EMC) then FreeAndNil(Emc);
  if Assigned(Joints) then FreeAndNil(Joints);
  if Assigned(GlobalBitmaps) then
    FreeBitmapList;
  if Assigned(GlobalErrors) then GlobalErrors.Free;
  if Assigned(UserErrors) then UserErrors.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  if Sender = nil then ;
  if Assigned(Dro) then
    Dro.Size;
  PanelPreviewResize(nil);
  PanelMasterResize(nil);
  PanelDlg.Left:= 0; PanelDlg.Top:= 0;
  PanelDlg.Width:= Self.ClientWidth;
  PanelDlg.Height:= Self.ClientHeight - PanelButtons.Height;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Sender = nil then ;
  case Key of
    '$': begin
           if Assigned(Dro) then
             Dro.JointMode:= not Dro.JointMode;
           Key:= #0;
           Exit;
         end;
  end;
  if State.Mode = TASKMODEMANUAL then
    clJog.FormKeyPress(nil,Key)
  else
  if (State.Mode = TASKMODEAUTO) then
    clRun.FormKeyPress(nil,Key)
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; // all keydowns get here
  Shift: TShiftState);

procedure DoAction(cmd: integer);
begin
  if Assigned(Emc) then
    Emc.HandleCommand(Cmd);
end;

begin
  if Sender = nil then ;
  case Key of
    VK_ESCAPE:
      begin
        if (ssCtrl in Shift) then
          begin
            DoAction(cmESTOP);
            Key:= 0;
            Exit;
          end
        else
          begin
            DoAction(cmABORT);
            ScriptRunning:= false;
            Key:= 0;
            Exit;
          end;
      end;
    VK_SPACE:
      if (ssCtrl in Shift) then
        begin
          GlobalErrors.Clear;
          LabelMsg.Caption:= '';
          if Assigned(MsgForm) then
          MsgForm.Hide;
        end;
    {$IFDEF LCLGTK2}
    VK_F:
      if (ssCtrl in Shift) then
        begin
          if IsFullScreen then
            UnFullScreen(Self)
          else
            FullScreen(Self);
        end;
    {$endif}
    VK_R: if (ssCtrl in Shift) then
      Emc.ResetInterpreter;
    VK_S: if (ssCtrl in Shift) then
      ExecEmcTop;
    VK_F1:  DoAction(cmMACHINE);
    VK_F2:  DoAction(cmJOG);
    VK_F3:  DoAction(cmAUTO);
    VK_F4:  DoAction(cmMDI);
    VK_F5:  DoAction(cmMIST);
    VK_F6:  DoAction(cmFLOOD);
    VK_F8:  DoAction(cmSPBRAKE);
    VK_F9:  DoAction(cmSPCCW);
    VK_F10: DoAction(cmSPMINUS);
    VK_F11: DoAction(cmSPPLUS);
    VK_F12: DoAction(cmSPCW);
  end;
  if (State.Mode = TaskModeManual) then
    clJog.FormKeyDown(nil,Key,Shift) else
  if (State.Mode = TaskModeAuto) then
    clRun.FormKeyDown(nil,Key,Shift) else
  if (State.Mode = TaskModeMDI) then
    clMDI.FormKeyDown(nil,Key,Shift);
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Sender = nil then ;
  if (State.Mode = TaskModeManual) then
    if Assigned(clJog) then
      clJog.FormKeyUp(nil,Key,Shift)
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if Sender = nil then ;
  if Assigned(clSim) then
    clSim.FormResize(nil);
  {$ifdef LCLGTK2}
  if InitialFullScreen then
    begin
      writeln('mocca is setup for fullscreen.');
      FullScreen(Self);
    end;
  {$endif}
  Self.Resize;
end;

procedure TMainForm.LabelMsgClick(Sender: TObject);
begin
  if Sender = nil then ;
  if GlobalErrors.Count > 0 then
    if Assigned(MsgForm) then
      begin
        MsgForm.PopMessages;
        MsgForm.BringToFront;
        Self.Activate;
      end;
end;

procedure TMainForm.OnTimer(Sender: TObject);
begin
  if Sender = nil then ;
  Self.UpdateState;
end;

procedure TMainForm.PanelMasterResize(Sender: TObject);
var
  w,h: integer;
begin
  if Sender <> nil then ;
  h:= PanelMaster.ClientHeight;
  w:= PanelMaster.ClientWidth;
  if Assigned(clJog) then clJog.SetBounds(0,0,w,h);
  if Assigned(clMDI) then clMDI.SetBounds(0,0,w,h);
  if Assigned(clRun) then clRun.SetBounds(0,0,w,h);
end;

procedure TMainForm.PanelButtonsResize(Sender: TObject);
const
  BtnSpace = 2;
var
  w,h,x,i: integer;
begin
  if Sender = nil then ;
  //if PanelButtons.Width > PanalButtons.Height then
  //  begin
      w:= PanelButtons.ClientWidth div NumButtons;
      h:= PanelButtons.ClientHeight;
      if w < 20 then w:= 20;
      if h < 20 then h:= 20;
      x:= (PanelButtons.ClientWidth - (w * NumButtons)) div 2;
  for i:= 0 to NumButtons - 1 do
    if Assigned(MocBtns[i]) then
      begin
        MocBtns[i].SetBounds(x+BtnSpace,BtnSpace,w-BtnSpace,h-BtnSpace);
        x:= x + w;
      end;
end;

procedure TMainForm.PanelPreviewResize(Sender: TObject);
begin
  if Sender = nil then ;
  if ShowGlPreview then
    if Assigned(clSim) then
      begin
        clSim.SetBounds(0,0,PanelPreview.ClientWidth,PanelPreview.ClientHeight);
        if not clSim.Visible then
          clSim.Visible:= True;
        clSim.FormResize(nil);
      end;
end;

procedure TMainForm.SliderFeedPositionChanged(Sender: TObject; NewPos: integer);
begin
  if Sender = nil then ;
  if UpdateLock then Exit;
  Emc.FeedOverride:= NewPos;
end;

procedure TMainForm.SliderSORPositionChanged(Sender: TObject; NewPos: integer);
begin
  if Sender = nil then ;
  if UpdateLock then Exit;
  Emc.SpindleOverride:= NewPos;
end;

procedure TMainForm.SliderVelPositionChanged(Sender: TObject; NewPos: integer);
begin
  if Sender = nil then ;
  if UpdateLock then Exit;
  Emc.MaxVelocity:= NewPos;
end;

initialization
{$I mocmain.lrs}

  MocButtonFrameWidth:= 3;
end.

