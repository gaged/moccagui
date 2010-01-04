unit touchoff; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TTouchOffDlg }

  TTouchOffDlg = class(TForm)
    Button1: TButton;
    cbCoords: TComboBox;
    EditV: TEdit;
    procedure cbCoordsChange(Sender: TObject);
    procedure EditVKeyPress(Sender: TObject; var Key: char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FValue: double;
    FCurrentAxis: Char;
    FCoord: integer;
  public
    procedure InitControls;
  end;

procedure DoTouchOff;

implementation

uses
  emc2pas,mocglb,mocjoints,mocemc;

procedure DoTouchOff;
var
  Dlg: TTouchOffDlg;
begin
  Application.CreateForm(TTouchOffDlg,Dlg);
  if Assigned(Dlg) then
    begin
      Dlg.ShowModal;
      Dlg.Free;
    end;
end;

procedure TTouchOffDlg.FormCreate(Sender: TObject);
begin
  InitControls;
end;

procedure TTouchOffDlg.EditVKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = 8 then Exit;
  if Key = ',' then Key:= '.';
  if not (Key in ['0'..'9','.','-','+']) then
    begin
      Beep;
      Key:= #0;
    end;
end;

procedure TTouchOffDlg.cbCoordsChange(Sender: TObject);
begin

end;

procedure TTouchOffDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
    begin
      try
        CanClose:= False;
        if EditV.Text <> '' then
          FValue:= StrToFLoat(EditV.Text);
        FCoord:= cbCoords.ItemIndex;
        Emc.TouchOffAxis(FCurrentAxis,FCoord + 1,FValue);
        CanClose:= True;
      except
      end;
    end;
end;

procedure TTouchOffDlg.FormShow(Sender: TObject);
begin
  EditV.SetFocus;
end;

procedure TTouchOffDlg.InitControls;
var
  ToolAxis: Char;
  i: integer;
begin
  if not Assigned(Joints) then
    begin
      writeln('invalid call to joints.');
      Exit;
    end;
  if State.TloAlongW then
    ToolAxis:= 'W'
  else
    ToolAxis:= 'Z';

  FCurrentAxis:= Joints.GetAxisChar(Vars.ActiveAxis);

  if FCurrentAxis = #0 then Exit;

  FValue:= GetAbsPos(Vars.ActiveAxis);
  EditV.Text:= FloatToStr(FValue);

  cbCoords.Items.Clear;
  for i:= 0 to CoordSysMax do
    cbCoords.Items.Add(CoordSys[i]);

  FCoord:= Emc.GetActiveCoordSys;
  if FCoord < 0 then FCoord:= 0;
  cbCoords.ItemIndex:= FCoord;

  Caption:= 'Antasten Achse ' + FCurrentAxis;
end;

initialization
  {$I touchoff.lrs}

end.

 if State.TloIsAlongW then
    begin
      tool_offset_axes:= 'w'
  else
  if emcState.Lathe then
    tool_offset_axes:= 'xz'
  else
    tool_offset_axes = 'z'

  if (emcState.tool_in_spindle = 0) or vars.current_axis.get() not in tool_offset_axes:
            del systems[-1]


