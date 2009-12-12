unit mocmain;

{$mode objfpc}
{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs, Buttons, ComCtrls,
  mocglb,mocjoints,jogclient,runclient,mdiclient,simclient;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList: TImageList;
    LabelFText: TLabel;
    LabelMaxVel: TLabel;
    LabelFWords: TLabel;
    LabelSWords: TLabel;
    LabelVText: TLabel;
    LabelGCodes: TLabel;
    LabelMCodes: TLabel;
    LabelFeed: TLabel;
    lbMessages: TListBox;
    PanelMaster: TPanel;
    PanelDRO: TPanel;
    PanelSoftBtns: TPanel;
    PanelMainBtns: TPanel;
    PanelInfo: TPanel;
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

    OldSpindleSpeed: Double;
    OldFeed: Double;

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
  emc2pas,mocemc,mocStatusDlg;

procedure TMainForm.HandleCommand(Cmd: integer);
begin
  Emc.HandleCommand(Cmd);
end;

procedure TMainForm.TaskModeChanged;  // called by MainForm.UpdateTaskMode
begin
  clJog.Visible:= (emcState.TaskMode = TaskModeManual);
  clRun.Visible:= (emcState.TaskMode = TaskModeAuto);
  clMdi.Visible:= (emcState.TaskMode = TaskModeMDI);
  case emcState.TaskMode of
    TaskModeManual: clJog.ActivateSelf;
    TaskModeAuto: clRun.ActivateSelf;
    TaskModeMDI: clMDI.ActivateSelf;
  end;
end;

procedure TMainForm.UpdateButtons; // called by Main_Form.UpdateState
begin
  MocBtns[0].Down:= emcState.EStop;
  MocBtns[1].Down:= emcState.Machine;
end;

procedure TMainForm.UpdateState;
begin
  if EMC.UpdateState then  // returns true if the taskmode changed
    TaskModeChanged;       // mainform.taskmodechanged

  UpdateButtons;           // update the buttons;

  Joints.Update;           // update the joints
  clSim.UpdateSelf;        // update the preview

  if (emcState.TaskMode = TASKMODEMANUAL) then clJog.UpdateSelf else
  if (emcState.TaskMode = TASKMODEAUTO) then clRun.UpdateSelf else
    clMDI.UpdateSelf;

  LabelGCodes.Caption:= PChar(ACTIVEGCODES);
  LabelMCodes.Caption:= PChar(ACTIVEMCODES);
  LabelFWords.Caption:= PChar(ACTIVEFWORDS);
  LabelSWords.Caption:= PChar(ACTIVESWORDS);

  if OperatorTextStr[0] <> #0 then
    begin
      lbMessages.Items.Add(PChar(OperatorTextStr));
      OperatorTextStr[0]:= #0;
    end;

  if OperatorDisplayStr[0] <> #0 then
    begin
      lbMessages.Items.Add(PChar(OperatorDisplayStr));
      OperatorDisplayStr[0]:= #0;
    end;
 end;

procedure TMainForm.InitPanels;  // init the panels, clients, joints
var
  h,i: integer;
begin
  PanelDro.Font.Size:= DroFontSize; // from mocglb.pas

  Joints:= TJoints.Create(PanelDRO);  // create the joints here
  Joints.CreateJoints(emcVars.CoordNames,emcVars.NumAxes);  // setup joints

  if Assigned(Joints) then
    begin
      h:= (Joints.BorderWidth + Abs(PanelDro.Font.Height)); // size the joints
      Joints.ShowActual:= emcVars.ShowActual;
      Joints.ShowRelative:= emcVars.ShowRelative;
    end
  else
    raise Exception.Create('error creating joints');

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
      if ShowActual then
        rgCoords.ItemIndex:= 0
      else
        rgCoords.ItemIndex:= 1;
      if ShowRelative then
        rgOrigin.ItemIndex:= 0
      else
        rgOrigin.ItemIndex:= 1;
    end;
end;

procedure TMainForm.InitButtons; // This creates the buttons used by mocca
var
  S: TSpeedButton;
  i: Integer;
begin
  for i:= 0 to NumTotalButtons - 1 do  // create the buttons
    begin
      S:= TSpeedButton.Create(self);
      if i < NumSideButtons then
        S.Parent:= PanelMainBtns
      else
        S.Parent:= PanelSoftBtns;
      S.AllowAllUp:= True;
      S.GroupIndex:= i+1;
      S.Width:= 64;
      S.Height:= 64;
      S.Tag:= -1;
      S.Transparent:= False;
      S.OnClick:= nil;
      S.Layout:= blGlyphTop;
      S.Spacing:= 0;
      MocBtns[i]:= S;
    end;
  PanelSoftBtns.ClientHeight:= GlobalButtonSize;  // size the panel
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  emcState.TaskMode:= 0;  // trigger a taskmodechanged
  UpdateLock:= True;      // prevent from update in ontimer
  GlobalImageList:= Self.ImageList;  // assign the imagelist to the global imagelist
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
  InitPanels;                           // init the panels
  InitButtons;                          // init the buttons
  Timer.Interval:= emcVars.CycleDelay;  // setup timer
  Timer.OnTimer:= @Self.OnTimer;
  Timer.Enabled:= True;                 // switch timer on
  UpdateLock:= False;                   // allow updates (is running now)
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  UpdateLock:= True;                    // prevent from updates during destroy
  Timer.Enabled:= False;                // disable the timer
  if Assigned(clJog) then clJog.Free;
  if Assigned(clMDI) then clMDI.Free;
  if Assigned(clRun) then clRun.Free;
  if Assigned(clSim) then clSim.Free;
  for i:= 0 to NumTotalButtons - 1 do
    if Assigned(MocBtns[i]) then MocBtns[i].Free;
  if Assigned(EMC) then Emc.Free;
  if Assigned(Joints) then Joints.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  lbMessages.Height:= 80;
  PanelDroResize(nil);
  PanelMasterResize(nil);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; // all keydowns get here
  Shift: TShiftState);

procedure DoAction(Cmd: integer);
begin
  if Assigned(Emc) then Emc.HandleCommand(Cmd);
  Key:= 0;
end;

begin
  if (Key = 27) then DoAction(cmESTOP);   // EStop #27 first

  if (emcState.TaskMode = TaskModeManual) then  // clJog needs almost all keys
    clJog.FormKeyDown(nil,Key,Shift);           // send the keys

  if (Key = 32) and (ssCtrl in Shift) then
    begin
      lbMessages.Items.Clear;
      Key:= 0;
    end;
end;

procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (emcState.TaskMode = TaskModeManual) then
    clJog.FormKeyUp(nil,Key,Shift)
end;

procedure TMainForm.OnTimer(Sender: TObject);
begin
  if UpdateLock then Exit;    // do not update
  if UpdateStatus <> 0 then   // fixme: its a serious error, terminate!
    begin
      lbMessages.Items.Add('Bad result in updatestatus');
      Exit;
    end;
  if UpdateError <> 0 then
    begin
      lbMessages.Items.Add('Bad result in updateerror');
      Exit;
    end;
    
  if LastError <> '' then
    begin
      lbMessages.Items.Add(LastError);
      LastError:= '';
    end;
    
  if CheckError then
    begin
      if Length(LastError) > 0 then
        with lbMessages do
          begin
            Items.Add(LastError);
            ItemIndex:= Items.Count - 1;
            MakeCurrentVisible;
            LastError:= '';
        end;
    end;
  Self.UpdateState;  // the mainform.updatestate.
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
    if MocBtns[i+NumSideButtons] <> nil then
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
    if MocBtns[i] <> nil then
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
  Joints.ShowActual:= rgCoords.ItemIndex = 0;
end;

procedure TMainForm.rgOriginClick(Sender: TObject);
begin
  Joints.ShowRelative:= rgOrigin.ItemIndex = 0;
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

