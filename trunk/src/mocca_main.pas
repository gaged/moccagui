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
    Button1: TButton;
    Button2: TButton;
    cbJog: TComboBox;
    cbORideLimits: TCheckBox;
    EditMDI: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    LabelJogVel: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    LabelC2: TLabel;
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
    MemoFile: TMemo;
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
    sbFeed: TScrollBar;
    sbJogSpeed: TScrollBar;
    sbVel: TScrollBar;
    BtnProbe: TSpeedButton;
    BtnHomeAll: TSpeedButton;
    BtnUnhomeAll: TSpeedButton;
    BtnRes1: TSpeedButton;
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
    procedure BtnHomeAllClick(Sender: TObject);
    procedure BtnMachineClick(Sender: TObject);
    procedure BtnProbeClick(Sender: TObject);
    procedure BtnUnhomeAllClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure sbFeedChange(Sender: TObject);
    procedure sbJogSpeedChange(Sender: TObject);
  private

    FBigDro: Array[0..4] of TLabel;
    FSmDro: Array[0..4] of TLabel;
    FDtg: Array[0..4] of TLabel;
    FDes: Array[0..4] of TLabel;
    
    procedure AssignDrosToLabels;

    procedure InitPosPanels;
    procedure InitStatePanel;

    procedure UpdateTaskMode;
    procedure UpdatePosPanels;
    procedure UpdateStatePanel;

    procedure UpdateFeed;

    procedure JogStart(Axis: Char; Dir: Integer);
    procedure JogStop(Axis: Char);
    procedure HandleJogKeys(var Key: Word; Down: Boolean);

    procedure ShowMsg(const Msg: string);
    
    procedure ToggleEStop;
    procedure ToggleMachine;
    
    procedure Home(Axis: integer);
    procedure UnHome(Axis: integer);

    procedure DoAbort;
    
    

  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;
  
implementation

{ TMainForm }

uses
  emc2pas;

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

procedure TMainForm.Home(Axis: Integer);
begin
end;

procedure TMainForm.UnHome(Axis: Integer);
begin
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
  if iAx < 0 then
    Exit;
  sendJogCont(iAx,100 * Dir);
end;

procedure TMainForm.JogStop(Axis: Char);
var
  iAx: Integer;
begin
  iAx:= AxisId(Axis);
  if iAx < 0 then
    Exit;
  sendJogStop(iAx);
end;

procedure TMainForm.UpdateFeed;
begin
  LabelFeed.Caption:= IntToStr(FeedOverride) + '%';
end;

procedure TMainForm.UpdateTaskMode;
var
  TskMode: integer;
  s: string;
begin
  TskMode:= GetTaskMode;
  if (TskMode <> TaskMode) then
    begin
      TaskMode:= Tskmode;
      case TaskMode of
        TaskModeManual: s:= 'Manueller Betrieb';
        TaskModeAuto: s:= 'Programmlauf Satzfolge';
        TaskModeMDI: s:= 'Positionieren mit Handeingabe';
      else
        s:= 'Unbekannte Betriebsart';
      end; // case
      LabelTaskMode.Caption:= s;
      if TaskMode > 0 then
        PageControl.TabIndex:= TaskMode - 1;
    end;
end;

procedure TMainForm.UpdateStatePanel;
var
  tmp: Integer;
begin
  EStopped:= GetEStop;
  MachineOn:= GetMachineOn;
  BtnEStop.Down:= EStopped;
  BtnMachine.Down:= MachineOn;

  getActiveGCodes;
  getActiveMCodes;

  LabelOPText.Caption:= PChar(OperatorTextStr);
  LabelOpDisp.Caption:= PChar(OperatorDisplayStr);
  LabelGCodes.Caption:= PChar(ActiveGCodes);
  LabelMCodes.Caption:= PChar(ActiveMCodes);
  
  // Jogspeed
  if sbJogSpeed.Position <> jogSpeed then
    sbJogSpeed.Position:= jogSpeed;
  LabelJogVel.Caption:= IntToStr(jogSpeed);
        
end;

procedure TMainForm.UpdatePosPanels;
var
  i: integer;
begin
  for i:= 0 to NumAxes - 1 do
  begin
    FDtg[i].Caption:= PosToString(getDtgPos(i));
    if ShowRelative then
      begin
        FBigDro[i].Caption:= PosToString(getRelPos(i));
        FSmDro[i].Caption:= PosToString(getAbsPos(i));
      end
    else
      begin
        FSmDro[i].Caption:= PosToString(getRelPos(i));
        FBigDro[i].Caption:= PosToString(getAbsPos(i));
      end;
    if GetJointHomed(i) then
      FDes[i].Font.Color:= clGreen
    else
  end;

end;

procedure TMainForm.InitPosPanels;
var
  i: integer;
  c: Char;
begin
  AssignDrosToLabels;
  for i:= 0 to NumAxes - 1 do
    begin
      c:= CoordNames[i+1];
      FDes[i].Caption:= c;
    end;
  if NumAxes < MaxAxes then
    for i:= NumAxes to MaxAxes - 1 do
      begin
        FDes[i].Caption:= '-';
        FBigDro[i].Caption:= '------.---';
        FSmDro[i].Caption:= '------.---';
        FDtg[i].Caption:= '------.---';
      end;
end;

procedure TMainForm.InitStatePanel;
var
  i: integer;
begin
  sbFeed.Min:= 0;
  sbFeed.Max:= MaxFeedOverride;
  FeedOverride:= 100;
  LabelVel.Caption:= UnitVelStr;
  LabelUnits.Caption:= UnitStr;
  cbJog.Items.Clear;
  for i:= 0 to JogIncCount - 1 do
    cbJog.Items.Add(JogInc[i].Txt);
  JogActiveInc:= 0;
  cbJog.ItemIndex:= 0;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateLock:= True;
  InitPosPanels;
  InitStatePanel;
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

procedure TMainForm.BtnHomeAllClick(Sender: TObject);
var
  i: integer;
begin
  sendHome(1);
end;

procedure TMainForm.BtnMachineClick(Sender: TObject);
begin
  toggleMachine;
end;

procedure TMainForm.BtnProbeClick(Sender: TObject);
begin
  SendHome(0);
end;

procedure TMainForm.BtnUnhomeAllClick(Sender: TObject);
begin
  sendhome(2);
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  ProgFile: string;
begin
  if OpenDialog.Execute then
    begin
      ProgFile:= OpenDialog.FileName;
      MemoFile.Lines.LoadFromFile(ProgFile);
    end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TMainForm.HandleJogKeys(var Key: Word; Down: Boolean);

procedure Jog(Letter: Char; Dir: Integer);
begin
  Key:= 0;
  JogStart(Letter,Dir);
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
  LabelTest.Caption:= IntToStr(Key);
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
  UpdatePosPanels;
  UpdateStatePanel;
  UpdateTaskMode;
  Sleep(CycleDelay);
end;

procedure TMainForm.PageControlChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Aviod switching tabs if mode does not allow this;
  //AllowChange:= False;
end;

procedure TMainForm.sbFeedChange(Sender: TObject);
begin
  FeedOverride:= sbFeed.Position;
  UpdateFeed;
end;

procedure TMainForm.sbJogSpeedChange(Sender: TObject);
begin
  jogSpeed:= sbJogSpeed.Position;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
end;


initialization
  {$I mocca_main.lrs}

end.

