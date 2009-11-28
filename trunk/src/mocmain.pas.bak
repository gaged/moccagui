unit mocmain;

{$mode objfpc}
{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs, Buttons, ComCtrls,
  mocglb,mocjoints,
  jogclient,runclient,mdiclient,simclient;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList: TImageList;
    Label1: TLabel;
    LabelMaxVel: TLabel;
    LabelFWords: TLabel;
    LabelSWords: TLabel;
    Label3: TLabel;
    LabelGCodes: TLabel;
    LabelMCodes: TLabel;
    LabelOpDisp: TLabel;
    LabelOpText: TLabel;
    LabelVel: TLabel;
    LabelFeed: TLabel;
    PanelMaster: TPanel;
    PanelDRO: TPanel;
    PanelSoftBtns: TPanel;
    PanelMainBtns: TPanel;
    PanelInfo: TPanel;
    PanelBars: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    sbFeed: TScrollBar;
    sbMaxVel: TScrollBar;
    Timer: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    //procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    procedure OnTimer(Sender: TObject);

    procedure PanelMasterResize(Sender: TObject);
    procedure PanelMainBtnsResize(Sender: TObject);
    procedure PanelSoftBtnsResize(Sender: TObject);
    procedure PanelDROResize(Sender: TObject);

    procedure sbFeedChange(Sender: TObject);
    procedure sbMaxVelChange(Sender: TObject);

    procedure MainClick(Sender: TObject);

  private
    procedure MapButtons;
    procedure InitPanels;
    procedure InitButtons;

    procedure UpdateState;
    procedure UpdateButtons;
    procedure TaskModeChanged;  // this is called after the taskmode was changed

    procedure ShowMsg(const Msg: string);
    procedure DoAction(Cmd: integer);
 end;

var
  MainForm: TMainForm;
  
implementation

{ TMainForm }

uses
  emc2pas,
  mocemc,mocStatusDlg;
  
// is called by a Buttonclick or by KeyDown ESC,F1-12
// returns true if the command was handled

procedure TMainForm.DoAction(Cmd: integer);
begin
  {$ifdef DEBUG_EMC}
    writeln('Command' + IntToStr(Cmd));
  {$endif}
  if not Emc.HandleCommand(Cmd) then   // returns true if the cmd was handled
    begin
      case Cmd of
        cmNCOPEN: clRun.OpenFile;
      end; // case;
    end;
end;

procedure TMainForm.TaskModeChanged;  // called by MainForm.UpdateTaskMode
begin

  clJog.Visible:= (emcState.TaskMode = TaskModeManual);
  clRun.Visible:= (emcState.TaskMode = TaskModeAuto);
  clMdi.Visible:= (emcState.TaskMode = TaskModeMDI);
  // FESimClient.Visible:= False;

  {$ifdef DEBUG_EMC}
    writeln('taskmode changed to' + IntToStr(emcState.TaskMode));
  {$endif}

  case emcState.TaskMode of
    TaskModeManual: clJog.ActivateSelf;
    TaskModeAuto: clRun.ActivateSelf;
    TaskModeMDI: clMDI.ActivateSelf;
  end;

  if emcState.TaskMode = TASKMODEAUTO then
    begin   // Disable the buttons we do not need in this mode
      EnableMainBtn(cmSPCW,False);
      EnableMainBtn(cmSPCCW,False);
      EnableMainBtn(cmSPPLUS,False);
      EnableMainBtn(cmSPMINUS,False);
      EnableMainBtn(cmFLOOD,False);
      EnableMainBtn(cmMIST,False);
    end
  else
  if emcState.TaskMode = TASKMODEMANUAL then
    begin
      EnableMainBtn(cmSPCW,true);
      EnableMainBtn(cmSPCCW,true);
      EnableMainBtn(cmSPPLUS,true);
      EnableMainBtn(cmSPMINUS,true);
      EnableMainBtn(cmFLOOD,true);
      EnableMainBtn(cmMIST,true);
    end
  else
    begin
      EnableMainBtn(cmSPCW,False);
      EnableMainBtn(cmSPCCW,False);
      EnableMainBtn(cmSPPLUS,true);
      EnableMainBtn(cmSPMINUS,true);
      EnableMainBtn(cmFLOOD,False);
      EnableMainBtn(cmMIST,False);
    end;
end;

procedure TMainForm.ShowMsg(const Msg: string);
begin
  UpdateLock:= True;
  ShowMessage(Msg);
  UpdateLock:= False;
end;

procedure TMainForm.UpdateButtons; // called by Main_Form.UpdateState
begin
  MainBtns[0].Down:= emcState.EStop;
  MainBtns[1].Down:= emcState.Machine;
  MainBtns[2].Down:= emcState.TaskMode = TASKMODEMANUAL;
  MainBtns[3].Down:= emcState.TaskMode = TASKMODEAUTO;
  MainBtns[4].Down:= emcState.TaskMode = TASKMODEMDI;
  MainBtns[5].Down:= emcState.SpInc < 0; // Fixme: its a state button
  MainBtns[6].Down:= emcState.SpDir > 0; // spindle cw
  MainBtns[7].Down:= emcState.SpDir < 0; // spindle ccw
  MainBtns[8].Down:= emcState.SpInc > 0; // Fixme: its a state button
  MainBtns[9].Down:= emcState.Flood;  // Flood
end;

procedure TMainForm.UpdateState;
begin
  if EMC.UpdateState then  // returns true if the taskmode changed
    TaskModeChanged;  // mainform.taskmodechanged

  UpdateButtons;
  Joints.Update;  // update the joints

  LabelGCodes.Caption:= PChar(ACTIVEGCODES);
  LabelMCodes.Caption:= PChar(ACTIVEMCODES);
  LabelFWords.Caption:= PChar(ACTIVEFWORDS);
  LabelSWords.Caption:= PChar(ACTIVESWORDS);
  LabelOPText.Caption:= PChar(OperatorTextStr);
  LabelOpDisp.Caption:= PChar(OperatorDisplayStr);

  clSim.Update;

  if emcState.TaskMode = TASKMODEMANUAL then
    begin
      clJog.UpdateSelf;
    end
  else
  if emcState.TaskMode = TASKMODEAUTO then
    begin
      clRun.UpdateSelf;
    end
  else
    begin
      clMDI.UpdateSelf;
    end;
end;

procedure TMainForm.InitPanels;  // init the panels, clients, joints
var
  h,i: integer;
begin
  Joints:= TJoints.Create(PanelDRO);  // create the joints here
  Joints.CreateJoints(emcVars.CoordNames,emcVars.NumAxes);  // setup joints
  if Assigned(Joints) then
    begin
      h:= (Joints.BorderWidth + Abs(PanelDro.Font.Height)); // size the joints
      // PanelDro.ClientHeight:= h * emcVars.NumAxes;   // size the dro panel
    end
  else
    Exit; // fixme: serious error, need to terminate
    
  if Assigned(clJog) then
    clJog.InitControls;  // init the clients

  with emcVars,emcState do  // setup mainforms controls
    begin
      ActFeed:= 100;
      sbFeed.Min:= 1;
      sbFeed.Max:= MaxFeedOverride;
      sbFeed.Position:= ActFeed;
      LabelFeed.Caption:= IntToStr(ActFeed) + '%';
      sbMaxVel.Min:= 0;
      sbMaxVel.Max:= MaxVel;
      sbMaxVel.Position:= ActVel;
      LabelMaxVel.Caption:= IntToStr(ActVel) + UnitVelStr;
    end;
end;

procedure TMainForm.MapButtons;  // load the buttons as defined in mocglb
var
  i,Id,iTag: Integer;
begin
  for i:= 0 to NumMainButtons - 1 do
    begin
      Id:= cmdMainGlyphs[i];
      iTag:= cmdMainTags[i];
      if (Id < 0) or (iTag < 0) then
        begin
          MainBtns[i].Glyph:= nil;
          MainBtns[i].Enabled:= False;
          MainBtns[i].Tag:= -1;
        end
      else
        begin
          ImageList.GetBitmap(Id,MainBtns[i].Glyph);
          MainBtns[i].Tag:= iTag;
          MainBtns[i].Enabled:= True;
        end;
    end;
end;

procedure TMainForm.InitButtons; // This creates the buttons used by mocca
var
  S: TSpeedButton;
  i: Integer;
begin
  for i:= 0 to NumMainButtons - 1 do  // create the main buttons
    begin
      S:= TSpeedButton.Create(self);
      S.Parent:= PanelMainBtns;
      S.AllowAllUp:= True;
      S.GroupIndex:= i+1;
      S.Width:= 64;
      S.Height:= 64;
      S.Tag:= -1;
      S.Transparent:= False;
      S.OnClick:= @MainClick;
      MainBtns[i]:= S;
    end;
  for i:= 0 to NumSoftButtons - 1 do  // create the soft buttons
    begin
      S:= TSpeedButton.Create(self);
      S.Parent:= PanelSoftBtns;
      S.AllowAllUp:= True;
      S.GroupIndex:= i + NumMainButtons + 1;
      S.Width:= 64;
      S.Height:= 64;
      S.Tag:= -1;
      S.OnClick:= nil;
      SoftBtns[i]:= S;
    end;

  MapButtons;  // load the main buttons..
  PanelSoftBtns.ClientHeight:= GlobalButtonSize;  // size the panel
  
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateLock:= True; // prevent from update in on_idle

  GlobalImageList:= ImageList;  // assign the imagelist to the global imagelist
  Emc:= TEmc.Create;

  clJog:= TJogClientForm.Create(self); // create the client forms
  clJog.Parent:= PanelMaster;
  clJog.Visible:= False;

  clMDI:= TMDIClientForm.Create(self);
  clMDI.Parent:= PanelMaster;
  clMDI.Visible:= False;

  clRun:= TRunClientForm.Create(self);
  clRun.Parent:= PanelMaster;
  clRun.Visible:= False;

  clSim:= TSimClientForm.Create(self);
  clSim.Parent:= PanelMaster;
  clSim.Visible:= False;

  InitPanels;
  InitButtons;

  // Application.AddOnIdleHandler(@OnAppIdle);  // set the idle handler to onappidle
  Timer.Interval:= emcVars.CycleDelay;
  Timer.OnTimer:= @Self.OnTimer;
  Timer.Enabled:= True;
  UpdateLock:= False;  //allow updates by on_idle
end;

procedure TMainForm.MainClick(Sender: TObject); // all mainbutton clicks get here
begin
  if Assigned(Sender) then
    with Sender as TSpeedButton do
      begin
        if Tag > 100 then Down:= False;  // tags > 100 are non state buttons
        DoAction(Tag);  // handle the command (tag)
      end;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  PanelDroResize(nil);  // bug in lazarus, need activate to get a resize on panels
  PanelMasterResize(nil);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  UpdateLock:= true;  // prevent from updates during destroy
  if Assigned(clJog) then clJog.Free;
  if Assigned(clMDI) then clMDI.Free;
  if Assigned(clRun) then clRun.Free;
  if Assigned(clSim) then clSim.Free;
  for i:= 0 to NumSoftButtons - 1 do
    if SoftBtns[i] <> nil then SoftBtns[i].Free;
  for i:= 0 to NumMainButtons - 1 do
    if MainBtns[i] <> nil then MainBtns[i].Free;
  if Assigned(EMC) then Emc.Free;
  if Assigned(Joints) then Joints.Free;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; // all keydowns get here
  Shift: TShiftState);
begin
  {$ifdef DEBUG_EMC}
  writeln('mainform.keydown: ' + IntToStr(Key));
  {$endif}
  if Key = 27 then  // handle the escape key first
    begin
      DoAction(cmESTOP);
      Key:= 0;
      Exit;
    end;

  if (emcState.TaskMode = TaskModeManual) then  // if taskmode manual
    begin
      if clJog.HandleJogKeys(Key,True) then Exit; // send keys to jogclient
      case Key of
        36: if (ssShift in  Shift) then  // Pos1 Key
          DoAction(cmREFACT)  // sendhome(activeaxis)
        else
          DoAction(cmREFALL); // sendhome(all)
        116: DoAction(cmSPMINUS);  // key F5
        117: DoAction(cmSPCW);     // key F6
        118: DoAction(cmSPCCW);    // key F7
        119: DoAction(cmSPPLUS);   // key F8
        120: DoAction(cmFLOOD);    // key F9
      end;
    end;
  case Key of
    112: DoAction(cmMACHINE); // key F1
    113: DoAction(cmJOG);
    114: DoAction(cmAUTO);
    115: DoAction(cmMDI); // key F4
  end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (emcState.TaskMode = TaskModeManual) then
    begin
      if clJog.HandleJogKeys(Key,False) then Exit;
    end;
end;

//procedure TMainForm.OnAppIdle(Sender: TObject; var Done: Boolean);
procedure TMainForm.OnTimer(Sender: TObject);
begin
  // Done:= False;
  if UpdateLock then Exit;  // do not update
  if UpdateStatus <> 0 then   // fixme: its a serious error, terminate!
    begin
      ShowMsg('Bad result in updatestatus');
      Exit;
    end;
  if UpdateError <> 0 then
    begin
      ShowMsg('Bad result in updateerror');
      Exit;
    end;
  if CheckError then  // fixme: show errors as a list, similar to "axis"
    begin
      ShowMsg(LastError);
      LastError:= '';
    end;
  UpdateState;  // the mainform.updatestate.
  // Sleep(emcVars.CycleDelay);  // sleep for ms (cycledelay)
end;

procedure TMainForm.PanelMasterResize(Sender: TObject);  // the master for the clients
var
  w,h: integer;
begin
  w:= PanelMaster.ClientWidth div 2;
  h:= PanelMaster.ClientHeight;
  clJog.SetBounds(0,0,w,h);
  clMDI.SetBounds(0,0,w,h);
  clRun.SetBounds(0,0,w,h);
  clSim.SetBounds(w,0,w,h);
  if not clSim.Visible then
    clSim.Visible:= True;
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
    if SoftBtns[i] <> nil then
      begin
        x:= round(w * i);
        SoftBtns[i].SetBounds(x,0,dx,GlobalButtonSize);
      end;
  PanelMainBtns.Left:= x;
  PanelMainBtns.ClientWidth:= dx;
end;

procedure TMainForm.PanelMainBtnsResize(Sender: TObject);
var
  y,i,w: integer;
  h: integer;
begin
  h:= PanelMainBtns.ClientHeight div NumMainButtons;
  if h < GlobalButtonSize then
    h := GlobalButtonSize;
  y:= (h div 2) - (GlobalButtonSize div 2);
  if y < 0 then y:= 0;
  w:= PanelMainBtns.ClientWidth;
  for i:= 0 to NumMainButtons - 1 do
    if MainBtns[i] <> nil then
      begin
        MainBtns[i].SetBounds(0,y,w,GlobalButtonSize);
        y:= y + h;
      end;
end;

procedure TMainForm.PanelDROResize(Sender: TObject);
begin
  if Assigned(Joints) then  // joints do resize in a own proc, see mocjoints.pas
    Joints.DoResize(nil);
end;


procedure TMainForm.sbFeedChange(Sender: TObject);
begin
  if UpdateLock then Exit;
  if Emc.SetFeedORide(sbFeed.Position) then
    LabelFeed.Caption:= IntToStr(emcState.ActFeed) + '%';
end;

procedure TMainForm.sbMaxVelChange(Sender: TObject);
var
  s: string;
begin
  if UpdateLock then Exit;
  if Emc.SetMaxVel(sbMaxVel.Position) then
    LabelMaxVel.Caption:= IntToStr(emcState.ActVel)+emcVars.UnitVelStr;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PanelDroResize(nil);  // does not work, here for compatibility, doesnt hurt
end;


initialization
  {$I mocmain.lrs}

end.

