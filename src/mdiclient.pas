unit mdiclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMDIClientForm }

  TMDIClientForm = class(TForm)
    EdMDI: TEdit;
    Label1: TLabel;
    LabelCaption: TLabel;
    LB: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    function  HandleCommand(Cmd: integer): Boolean;
  end;
  
var
  clMdi: TMDIClientForm;

implementation

uses
  buttons,mocglb,mocjoints,
  emc2pas;

procedure TMDIClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEMDI;
end;

function TMDIClientForm.HandleCommand(Cmd: integer): Boolean;
var
  S: string;
begin
  Result:= True;
  case Cmd of
    cmMDIEXEC:
      begin
        S:= EdMDI.Text;
        if Length(S) > 0 then
          Emc.ExecMDI(S);
      end;
  else
    Result:= False;
  end; // case;
end;

procedure TMDIClientForm.ActivateSelf;
begin
  if emcState.TaskMode <> TASKMODEMDI then Exit;
  if not Visible then
    Visible:= true;
  MapButtons;
  SetButtonDown(cmMDI,True);
end;

procedure TMDIClientForm.UpdateSelf;
begin
end;

procedure TMDIClientForm.MapButtons;
begin
  SetButtonMap(@BtnDefMDI,@Self.Click);
end;

procedure TMDIClientForm.InitControls;
begin
end;

procedure TMDIClientForm.Click(Sender: TObject);
begin
  if Assigned(Sender) then
    with Sender as TSpeedButton do
      begin
        Down:= False;
        if not Self.HandleCommand(Tag) then
          Emc.HandleCommand(Tag);
      end;
end;

procedure TMDIClientForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    begin
      HandleCommand(cmMDIExec);
      Key:= 0;
    end;
end;


initialization
  {$I mdiclient.lrs}

end.

