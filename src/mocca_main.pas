unit mocca_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs, Grids, Buttons, mocglobal, ComCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnMachine: TSpeedButton;
    BtnExecMDI: TButton;
    BtnFileOpen: TButton;
    BtnRun: TButton;
    BtnStop: TButton;
    cbJog: TComboBox;
    cbORideLimits: TCheckBox;
    CheckBox1: TCheckBox;
    EditMDI: TEdit;
    ImgSmall: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    LabelActVel: TLabel;
    LabelJogVel: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    LabelC2: TLabel;
    LabelFWords: TLabel;
    LabelSWords: TLabel;
    LabelY1: TLabel;
    LabelY2: TLabel;
    LabelZ1: TLabel;
    LabelZ2: TLabel;
    LabelB1: TLabel;
    LabelB2: TLabel;
    LabelC1: TLabel;
    LabelYDes: TLabel;
    LabelZDes: TLabel;
    LabelBDes: TLabel;
    LabelCDes: TLabel;
    LabelXDtg: TLabel;
    LabelX2: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelX1: TLabel;
    Label6: TLabel;
    LabelXDes: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LabelGCodes: TLabel;
    LabelMCodes: TLabel;
    LabelOpDisp: TLabel;
    LabelOpText: TLabel;
    LabelTest: TLabel;
    LabelVel: TLabel;
    LabelFeed: TLabel;
    LabelTaskMode: TLabel;
    LabelUnits: TLabel;
    LabelYDtg: TLabel;
    LabelZDtg: TLabel;
    LabelBDtg: TLabel;
    LabelCDtg: TLabel;
    lbHistory: TListBox;
    lbProgram: TListBox;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PanelMocca: TPanel;
    PanelHeader: TPanel;
    PanelInfo: TPanel;
    PanelButtons: TPanel;
    PanelBars: TPanel;
    PanelBigDro: TPanel;
    PanelTop: TPanel;
    PanelBottom: TPanel;
    sbFeedOride: TScrollBar;
    sbJogSpeed: TScrollBar;
    sbMaxVel: TScrollBar;
    BtnOffset: TSpeedButton;
    BtnHome: TSpeedButton;
    BtnUnhome: TSpeedButton;
    BtnRes3: TSpeedButton;
    BtnToolChg: TSpeedButton;
    BtnRes2: TSpeedButton;
    BtnEStop: TSpeedButton;
    BtnSpindleOff: TSpeedButton;
    BtnSpindleOn: TSpeedButton;
    BtnM09: TSpeedButton;
    TabManual: TTabSheet;
    TabMDI: TTabSheet;
    TabRun: TTabSheet;

    procedure BtnEStopClick(Sender: TObject);
    procedure BtnExecMDIClick(Sender: TObject);
    procedure BtnHomeClick(Sender: TObject);
    procedure BtnMachineClick(Sender: TObject);
    procedure BtnOffsetClick(Sender: TObject);
    procedure BtnRes3Click(Sender: TObject);
    procedure BtnRes2Click(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
    procedure BtnUnhomeClick(Sender: TObject);
    procedure BtnFileOpenClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure sbFeedOrideChange(Sender: TObject);
    procedure sbJogSpeedChange(Sender: TObject);
    procedure sbMaxVelChange(Sender: TObject);
  private

    FBigDro: Array[0..4] of TLabel;
    FSmDro: Array[0..4] of TLabel;
    FDtg: Array[0..4] of TLabel;
    FDes: Array[0..4] of TLabel;

    procedure AssignDrosToLabels;

    procedure InitPanels;

    procedure UpdateTaskMode;
    procedure UpdateState;
    
    procedure AfterTskModeChange;  // this is called after the taskmode was changed
    procedure BeforeTskModeChange(var Allow: Boolean);
    
    procedure JogStart(Axis: Char; Dir: Integer);
    procedure JogStop(Axis: Char);
    procedure HandleJogKeys(var Key: Word; Down: Boolean);

    procedure ShowMsg(const Msg: string);
    
    procedure ToggleEStop;
    procedure ToggleMachine;
    
    procedure DoAbort;

  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;
  
implementation

{ TMainForm }

uses
  emc2pas,
  StatusDlg,
  HomeDlg;

procedure tMainForm.AfterTskModeChange;
begin
end;

procedure tMainForm.BeforeTskModeChange(var Allow: Boolean);
begin
  Allow:= True;
end;

procedure TMainForm.AssignDrosToLabels;
begin
  FBigDro[0]:= LabelX1; FBigDro[1]:= LabelY1; FBigDro[2]:= LabelZ1;
  FBigDro[3]:= LabelB1; FBigDro[4]:= LabelC1;
  FSmDro[0]:= LabelX2; FSmDro[1]:= LabelY2; FSmDro[2]:= LabelZ2;
  FSmDro[3]:= LabelB2; FSmDro[4]:= LabelC2;
  FDtg[0]:= LabelXDtg; FDtg[1]:= LabelYDtg; FDtg[2]:= LabelZDtg;
  FDtg[3]:= LabelBDtg; FDtg[4]:= LabelCDtg;
  FDes[0]:= LabelXDes; FDes[1]:= LabelYDes; FDes[2]:= LabelZDes;
  FDes[3]:= LabelBDes; FDes[4]:= LabelCDes;
end;

procedure TMainForm.ToggleEStop;
begin
  if EStopped then
    SendEStopReset
  else
    SendEStop;
  EStopped:= not EStopped;
end;

procedure TMainForm.ToggleMachine;
begin
  if MachineOn then
    SendMachineOff
  else
    SendMachineOn;
  MachineOn:= not MachineOn;
end;

procedure TMainForm.DoAbort;
begin
  sendAbort();
end;

procedure TMainForm.ShowMsg(const Msg: string);
begin
  UpdateLock:= True;
  ShowMessage(Msg);
  UpdateLock:= False;
end;
  
procedure TMainForm.JogStart(Axis: Char; Dir: Integer);
var
  iAx: Integer;
begin
  iAx:= AxisId(Axis);
  if iAx < 0 then Exit;
  sendJogCont(iAx,ActJogSpeed * Dir);
end;

procedure TMainForm.JogStop(Axis: Char);
var
  iAx: Integer;
begin
  iAx:= AxisId(Axis);
  if iAx < 0 then Exit;
  sendJogStop(iAx);
end;

procedure TMainForm.UpdateTaskMode;
var
  NewTaskMode: integer;
  s: string;
begin
  NewTaskMode:= taskMode;   // get new taskmode
  if (NewTaskMode <> ActTaskMode) then
    begin
      ActTaskMode:= NewTaskMode;
      case ActTaskMode of
        TaskModeManual: s:= 'Manueller Betrieb';
        TaskModeAuto: s:= 'Programmlauf Satzfolge';
        TaskModeMDI: s:= 'Positionieren mit Handeingabe';
      else
        Exit; // unknown Taskmode, Exit!
      end; // case
      LabelTaskMode.Caption:= s;
      if ActTaskMode > 0 then
        PageControl.TabIndex:= ActTaskMode - 1;
        
      BtnSpindleOn.Enabled:= ActTaskMode = TaskModeManual;
      BtnSpindleOff.Enabled:= ActTaskMode = TaskModeManual;
      BtnM09.Enabled:= ActTaskMode = TaskModeManual;
    end;
end;

procedure TMainForm.UpdateState;
var
  i: integer;
  b: Boolean;
begin

  EStopped:= GetEStop;
  MachineOn:= GetMachineOn;
  BtnEStop.Down:= EStopped;
  BtnMachine.Down:= MachineOn;

  taskActiveCodes;  // update active G,MCodes, FWords, SWords;

  LabelGCodes.Caption:= PChar(ACTIVEGCODES);
  LabelMCodes.Caption:= PChar(ACTIVEMCODES);
  LabelFWords.Caption:= PChar(ACTIVEFWORDS);
  LabelSWords.Caption:= PChar(ACTIVESWORDS);
  LabelOPText.Caption:= PChar(OperatorTextStr);
  LabelOpDisp.Caption:= PChar(OperatorDisplayStr);

  // LabelActVel.Caption:= FloatToStr(trajVel);
  
  if ActTaskMode = TaskModeManual then  // only update jogstuff when manual mode
    begin
      if OldJogSpeed <> ActJogSpeed then
        begin
          OldJogSpeed:= ActJogSpeed;
          LabelJogVel.Caption:= IntToStr(ActJogSpeed)  +  'mm/min';
        end;
    end
  else
    if ActTaskMode = TaskModeAuto then
      begin
        if taskInterpState <> EMC_TASK_INTERP_IDLE then
          begin
            i:= taskMotionLine;
            LabelTest.Caption:= intToStr(i);
            if i < lbProgram.Items.Count then
              lbProgram.ItemIndex:= i;
              if not lbProgram.ItemFullyVisible(i) then
                lbProgram.MakeCurrentVisible;
          end;
      end;

  if OldFeedORide <> ActFeedORide then
    begin
      OldFeedORide:= ActFeedORide;
      LabelFeed.Caption:= IntToStr(ActFeedOride) + '%';
      sendFeedOverride(ActFeedOride / 100);
    end;
    
  if ActMaxVelocity <> OldMaxVelocity then
    begin
      OldMaxVelocity:= ActMaxVelocity;
      LabelActVel.Caption:= FloatToStrF(ActMaxVelocity, ffFixed, 4, 2) + 'mm/min';
      // sendMaxVelocity(ActMaxVelocity/60);
    end;

  // update DROs
  for i:= 0 to E.NumAxes - 1 do
  begin
    FDtg[i].Caption:= PosToString(getDtgPos(i));
    if E.ShowRelative then
      begin
        FBigDro[i].Caption:= PosToString(getRelPos(i));
        FSmDro[i].Caption:= PosToString(getAbsPos(i));
      end
    else
      begin
        FSmDro[i].Caption:= PosToString(getRelPos(i));
        FBigDro[i].Caption:= PosToString(getAbsPos(i));
      end;
    b:= AxisHomed(i);
    if b <> Joints[i].Homed then
      begin
        Joints[i].Homed:= b;
        if b then
          FDes[i].Font.Color:= clGreen
        else
          FDes[i].Font.Color:= clRed;
      end;
  end;
end;

procedure TMainForm.InitPanels;
var
  i: integer;
  c: Char;
begin
  AssignDrosToLabels;
  for i:= 0 to E.NumAxes - 1 do
    begin
      c:= E.CoordNames[i+1];
      FDes[i].Caption:= c;
    end;

  if E.NumAxes < MaxAxes then
    for i:= E.NumAxes to MaxAxes - 1 do
      begin
        FDes[i].Caption:= '-';
        FBigDro[i].Caption:= '------.---';
        FSmDro[i].Caption:= '------.---';
        FDtg[i].Caption:= '------.---';
      end;
      
  ActFeedORide:= 100;
  sbFeedOride.Min:= 0;
  sbFeedOride.Max:= E.MaxFeedOverride;
  sbFeedORide.Position:= ActFeedORide;

  sbMaxVel.Min:= 0;
  sbMaxVel.Max:= MaxMaxVelocity;
  sbMaxVel.Position:= ActMaxVelocity;

  sbJogSpeed.Min:= 0;
  sbJogSpeed.Max:= MaxJogSpeed;
  sbJogSpeed.Position:= OldJogSpeed;
  OldJogSpeed:= 0;

  LabelActVel.Caption:= E.UnitVelStr;
  LabelUnits.Caption:= E.UnitStr;

  cbJog.Items.Clear;
  for i:= 0 to JogIncCount - 1 do
    cbJog.Items.Add(JogInc[i].Txt);
  JogActiveInc:= 0;
  cbJog.ItemIndex:= 0;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateLock:= True;
  InitPanels;
  Application.AddOnIdleHandler(@OnAppIdle);
  UpdateLock:= False;
end;

procedure TMainForm.BtnEStopClick(Sender: TObject);
begin
  toggleEStop;
end;

procedure TMainForm.BtnExecMDIClick(Sender: TObject);
var
  s: string;
begin
  s:= EditMDI.Text;
  if s <> '' then
    begin
      sendMDICmd(PChar(s));  // send mdi command
      lbHistory.Items.Add(s); // save command to history
    end;
end;

procedure TMainForm.BtnHomeClick(Sender: TObject);
begin
  HomeDlgForm.ShowModal;
end;

procedure TMainForm.BtnMachineClick(Sender: TObject);
begin
  toggleMachine;
end;

procedure TMainForm.BtnOffsetClick(Sender: TObject);
begin
  SendHome(0);
end;

procedure TMainForm.BtnRes3Click(Sender: TObject);
var
 i: integer;
begin
  for i := 0 to 2 do sendHome(i);
end;

procedure TMainForm.BtnRes2Click(Sender: TObject);
begin
  StatusDlgForm.ShowModal;
end;

procedure TMainForm.BtnStopClick(Sender: TObject);
begin
  sendProgramPause;
end;

procedure TMainForm.BtnUnhomeClick(Sender: TObject);
begin
  sendUnHome(-1);
end;

procedure TMainForm.BtnFileOpenClick(Sender: TObject);
var
  s: string;
  FileName: string;
begin
  if OpenDialog.Execute then
    begin
      FileName:= OpenDialog.FileName;
      lbProgram.Items.LoadFromFile(FileName);
      if Length(FileName) > 0 then
        sendProgramOpen(PChar(FileName));
    end;
end;

procedure TMainForm.BtnRunClick(Sender: TObject);
begin
  sendProgramRun(1);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TMainForm.HandleJogKeys(var Key: Word; Down: Boolean);

procedure Jog(Letter: Char; Dir: Integer);
begin
  Key:= 0;
  JogStart(Letter,Dir);
  // LabelTest.Caption:= IntToStr(ti);
end;

begin
  if Down then
    begin
      case Key of
        33: Jog('Z',-1);
        34: Jog('Z', 1);
        37: Jog('X',-1);
        38: Jog('Y',-1);
        39: Jog('X', 1);
        40: Jog('Y', 1);
      end;
    end
  else
    begin
      case Key of
        33,34: JogStop('Z');
        37,39: JogStop('X');
        38,40: JogStop('Y');
      end // case
    end;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Handle abort,estop stuff first
  if Key = 27 then
    begin
      SendAbort;
      Exit;
    end;
  case Key of
    112: ToggleEStop;
    113: ToggleMachine;
    114: sendManual;
    116: sendMDI;
    115: SendAuto;
  end;
  if TaskMode = TaskModeManual then
    HandleJogKeys(Key,true);
end;


procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  HandleJogKeys(Key,false);
end;

{$define HAS_SLEEP}

procedure TMainForm.OnAppIdle(Sender: TObject; var Done: Boolean);
begin
  Done:= False;
  if UpdateLock then
    Exit;
  if UpdateStatus <> 0 then
    begin
      UpdateLock:= True;
      ShowMessage('Bad result in updatestatus');
      Exit;
    end;
  if UpdateError <> 0 then
    begin
      UpdateLock:= True;
      ShowMessage('Bad result in updateerror');
      Exit;
    end;
  if CheckError then
    begin
      ShowMsg(LastError);
      LastError:= '';
    end;
  UpdateTaskMode;
  UpdateState;
  Sleep(E.CycleDelay);
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
end;

procedure TMainForm.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Aviod switching tabs if mode does not allow this;
  // only change tab if interp state = idle
  AllowChange:= (taskInterpState = EMC_TASK_INTERP_IDLE)
end;

procedure TMainForm.sbFeedOrideChange(Sender: TObject);
begin
  if UpdateLock then
    Exit;
  ActFeedORide:= sbFeedOride.Position;
end;

procedure TMainForm.sbJogSpeedChange(Sender: TObject);
begin
  ActJogSpeed:= sbJogSpeed.Position;
end;

procedure TMainForm.sbMaxVelChange(Sender: TObject);
begin
  if UpdateLock then
    Exit;
  ActMaxVelocity:= sbMaxVel.Position;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // initial setup
  //BtnHome.Enabled:= E.HomingOrderDefined;
  //BtnUnhome.Enabled:= E.HomingOrderDefined;
  sbJogSpeed.Min:= MinJogSpeed;
  sbJogSpeed.Max:= MaxJogSpeed;
end;


initialization
  {$I mocca_main.lrs}

end.
