unit jogclient;

{$mode objfpc}{$H+}
{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TJogClientForm }

  TJogClientForm = class(TForm)
    cbJog: TComboBox;
    Label1: TLabel;
    LabelCaption: TLabel;
    LabelJogVel: TLabel;
    Label3: TLabel;
    sbJogVel: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure sbJogVelChange(Sender: TObject);
    procedure cbJogChange(Sender: TObject);
    
    procedure Click(Sender: TObject);
    
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    function  HandleJogKeys(var Key: Word; Down: Boolean): Boolean;
  end;

var
  clJog: TJogClientForm;

implementation

{ TJogClientForm }

uses
  buttons,
  mocglb,mocjoints,
  emc2pas;
  
procedure TJogClientForm.ActivateSelf;
begin
  if emcState.TaskMode <> TASKMODEMANUAL then Exit;
  if not Visible then
    Visible:= true;
  MapButtons;
  {$ifdef DEBUG_EMC}
  writeln('jogclient activateself');
  {$endif}
end;

procedure TJogClientForm.UpdateSelf;
begin
end;

procedure TJogClientForm.MapButtons;
var
  i,c,Id,iTag: Integer;
  Tags: Array[0..NumSoftButtons - 1] of integer;
  Glyphs: Array[0..NumSoftButtons - 1] of integer;
begin
  Tags:= cmdJogTags;
  Glyphs:= cmdJogGlyphs;

  {$ifdef DEBUG_EMC}
  writeln('jogclient: map buttons');
  {$endif}

  for i:= 0 to NumSoftButtons - 1 do
    if (Tags[i] = cmREFALL) and (not emcVars.HomingOrderDefined) then
      begin
        Tags[i]:= -1;
        break;
      end;

  for i:= 0 to NumSoftButtons - 1 do
    begin
      Id:= Glyphs[i];
      iTag:= Tags[i];
      if (Id < 0) or (iTag < 0) then
        begin
          SoftBtns[i].Glyph:= nil;
          SoftBtns[i].Enabled:= False;
          SoftBtns[i].Tag:= -1;
          SoftBtns[i].OnClick:= nil;
        end
      else
        begin
          GlobalImageList.GetBitmap(Id,SoftBtns[i].Glyph);
          SoftBtns[i].Tag:= iTag;
          SoftBtns[i].Enabled:= True;
          SoftBtns[i].OnClick:= @Self.Click;
          {$ifdef DEBUG_EMC}
          writeln('jogclient mapped button' + intToStr(i));
          {$endif}
        end;
    end;
end;

procedure TJogClientForm.InitControls;
var
  i: integer;
begin
  with emcState,emcVars do
    begin
      sbJogVel.Min:= MinJogVel;
      sbJogVel.Max:= MaxJogVel;
      sbJogVel.Position:= ActJogVel;
      LabelJogVel.Caption:= IntToStr(ActJogVel) + UnitVelStr;
      cbJog.Items.Clear;
      for i:= 0 to JogIncMax do
        cbJog.Items.Add(JogIncrements[i].Text);
      cbJog.ItemIndex:= 0;
    end;
end;

function TJogClientForm.HandleJogKeys(var Key: Word; Down: Boolean): Boolean;

procedure Jog(Ch: Char; Dir: Integer);
begin
  {$ifdef DEBUG_EMC}
  writeln('jogclient.handlejogkeys: ' + IntToStr(Key));
  {$endif}
  Key:= 0;
  Sleep(10);
  Joints.JogStart(Ch,Dir);
end;

procedure JogStop(Ch: Char);
begin
  Key:= 0;
  Sleep(10);
  Joints.JogStop(Ch);
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
  Result:= (Key = 0);
end;

procedure TJogClientForm.Click(Sender: TObject);
begin
  if Assigned(Sender) then
    if Sender is TSpeedButton then
      with Sender as TSpeedButton do
        begin
          {$ifdef DEBUG_EMC}
          writeln('sender.tag: ' + intToStr(Tag));
          {$endif}
          if Tag > 100 then Down:= False;
          Emc.HandleCommand(Tag)
        end;
end;

procedure TJogClientForm.cbJogChange(Sender: TObject);
var
  i: integer;
  v: Double;
begin
  i:= cbJog.ItemIndex;
  if (i < 0) or (i > emcVars.JogIncMax) then
    Exit;
  emcVars.jogIncrement:= emcVars.JogIncrements[i].Value;
end;

procedure TJogClientForm.sbJogVelChange(Sender: TObject);
var
  Vel: integer;
begin
  if UpdateLock then Exit;
  Vel:= sbJogVel.Position;
  if Vel <> emcState.ActJogVel then
    begin
      emcState.ActJogVel:= Vel;
      LabelJogVel.Caption:= IntToStr(emcState.ActJogVel)+emcVars.UnitVelStr;
    end;
end;

procedure TJogClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEMANUAL;
end;


initialization
  {$I jogclient.lrs}
  
end.

