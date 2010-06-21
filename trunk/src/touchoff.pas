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
    BtnCancel: TButton;
    cbCoords: TComboBox;
    EditV: TEdit;
    LabelPos: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelUnit: TLabel;
    procedure EditVKeyPress(Sender: TObject; var Key: char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    FValue: double;
    FAxisCh: Char;
    FAxisNo: integer;
    FCoord: integer;
  public
    procedure InitControls;
  end;

procedure DoTouchOff(Axis: Char);

implementation

uses
  emc2pas,mocglb,mocjoints,mocemc;

procedure DoTouchOff(Axis: Char);
var
  Dlg: TTouchOffDlg;
  i: integer;
begin
  if not Assigned(Joints) then
    begin
      writeln('touchoffdlg: joints = nil!');
      Exit;
    end;
  i:= Joints.AxisByChar(Axis);
  if i < 0 then
    Exit;
  Application.CreateForm(TTouchOffDlg,Dlg);
  if Assigned(Dlg) then
    begin
      Dlg.FAxisCh:= Axis;
      Dlg.FAxisNo:= i;
      Dlg.InitControls;
      Dlg.ShowModal;
      Dlg.Free;
    end;
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

procedure TTouchOffDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if ModalResult = mrOk then
    begin
      try
        CanClose:= False;
        if EditV.Text <> '' then
          FValue:= StrToFLoat(EditV.Text);
        FCoord:= cbCoords.ItemIndex;
        Emc.TouchOffAxis(FAxisCh,FCoord + 1,FValue);
        CanClose:= True;
      except
        CanClose:= False;
      end;
    end
  else
    CanClose:= True;
end;

procedure TTouchOffDlg.FormCreate(Sender: TObject);
begin
  ReadStyle(Self,'touchoffdialog.xml');
end;

procedure TTouchOffDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) then
    ModalResult:= mrOk
  else
  if (Key = 27) then
    ModalResult:= mrCancel;
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
  if State.TloAlongW then
    ToolAxis:= 'W'
  else
    ToolAxis:= 'Z';
  FValue:= GetRelPos(FAxisNo);
  EditV.Text:= '0.00';
  LabelPos.Caption:= PosToString(FValue);
  cbCoords.Items.Clear;
  for i:= 0 to CoordSysMax do
    cbCoords.Items.Add(CoordSys[i]);
  FCoord:= Emc.GetActiveCoordSys;
  if FCoord < 0 then FCoord:= 0;
  cbCoords.ItemIndex:= FCoord;
  LabelUnit.Caption:= Vars.UnitStr;
  Caption:= Caption + #32 + FAxisCh;
end;

initialization
  {$I touchoff.lrs}

end.
