unit touchoffwiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs;

type

  { TTouchOffWizDlg }

  TTouchOffWizDlg = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    cbCoords: TComboBox;
    EditX1: TEdit;
    EditY2: TEdit;
    EditX2: TEdit;
    EditY1: TEdit;
    EditZ: TEdit;
    EditDia: TEdit;
    Image1: TImage;
    Label1: TLabel;
    LabelUnit: TLabel;
    Label4: TLabel;
    procedure BtnOkClick(Sender: TObject);
    procedure EditDiaExit(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: char);
    procedure EditX1Change(Sender: TObject);
    procedure EditX1Exit(Sender: TObject);
    procedure EditX2Change(Sender: TObject);
    procedure EditX2Exit(Sender: TObject);
    procedure EditY1Change(Sender: TObject);
    procedure EditY1Exit(Sender: TObject);
    procedure EditY2Change(Sender: TObject);
    procedure EditY2Exit(Sender: TObject);
    procedure EditZExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    FAxisMask: string;
    FCoord: integer;
    FG10Code: string;
    procedure GetCode;
    procedure UpdateEdits;
    procedure InitControls;
  public
    property AxisMask: string write FAxisMask;
    property G10Code: string read FG10Code;
  end;

var
  TouchOffWizDlg: TTouchOffWizDlg;

function DoTouchOffWiz: string;

implementation

uses
  emc2pas,mocemc,MocGlb,MocJoints;

{ TTouchOffWizDlg }

var
  XOffset: double;
  YOffset: double;
  ZOffset: double;

  XDir,YDir,ZDir: integer;

function DoTouchOffWiz: string;
var
  Dlg: TTouchOffWizDlg;
begin
  Result:= '';
  if not Assigned(Joints) then
    begin
      writeln('Error on Touchoff- Wizzard: joints = nil!');
      Exit;
    end;
  Application.CreateForm(TTouchOffWizDlg,Dlg);
  try
    if Dlg.ShowModal = mrOk then
      Result:= Dlg.G10Code;
  finally
    Dlg.Free;
  end;
end;

function OffsetPos(Axis: Char; Position: double): double;
var
  i: integer;
  d: double;
begin
  i:= Joints.AxisByChar(Axis);
  if i < 0 then
    raise Exception.Create('Touchoff wizard: invalid Axis: ' + Axis);
  d:= GetAbsPos(i);
  Result:= d - Position;
end;

procedure TTouchOffWizDlg.GetCode;
var
  OffsetPosX,OffsetPosY,OffsetPosZ: double;
  IsInch: Boolean;
  Ef: double;
  HasX,HasY,HasZ: Boolean;
  S: string;
  Valid: Boolean;
begin

  UpdateEdits;
  FCoord:= cbCoords.ItemIndex + 1;

  IsInch:= Pos('G20',ActiveGCodes) > 0;

  HasX:= (Pos('X',FAxisMask) > 0) and (XDir > 0);
  HasY:= (Pos('Y',FAxisMask) > 0) and (YDir > 0);
  HasZ:= (Pos('Z',FAxisMask) > 0) and (ZDir > 0);

  Valid:= HasX or HasY or HasZ;
  if not Valid then
    begin
      ShowMessage('Error: Need at least 1 Value to perform a Touchoff command.');
      Exit;
    end;

  if HasX then OffsetPosX:= OffsetPos('X',XOffset);
  if HasY then OffsetPosY:= OffsetPos('Y',YOffset);
  if HasZ then OffsetPosZ:= OffsetPos('Z',ZOffset);

  Ef:= EdgeFinderDia / 2;

  if IsInch and Vars.Metric then
    begin
      OffsetPosX:= OffsetPosX / 25.4;
      OffsetPosY:= OffsetPosY / 25.4;
      OffsetPosZ:= OffsetPosZ / 25.4;
      Ef:= Ef / 25.4;
    end;

  if (not IsInch) and (not Vars.Metric) then
    begin
      OffsetPosX:= OffsetPosX * 25.4;
      OffsetPosY:= OffsetPosY * 25.4;
      OffsetPosZ:= OffsetPosZ * 25.4;
      Ef:= Ef * 25.4;
    end;

  if HasX then
    begin
      if XDir = 1 then
        OffsetPosX:= OffsetPosX - Ef
      else
        OffsetPosX:= OffsetPosX + Ef;
    end;

  if HasY then
    begin
      if YDir = 1 then
        OffsetPosY:= OffsetPosY - Ef
      else
        OffsetPosY:= OffsetPosY + Ef;
    end;

  S:= 'G10L2P' + IntToStr(FCoord);
  if HasX then
    S:= S + 'X' + FloatToStrF(OffsetPosX,ffFixed,8,4);
  if HasY then
    S:= S + 'Y' + FloatToStrF(OffsetPosY,ffFixed,8,4);
  if HasZ then
    S:= S + 'Z' + FloatToStrF(OffsetPosZ,ffFixed,8,4);

  FG10Code:= s;
end;

procedure TTouchOffWizDlg.EditKeyPress(Sender: TObject; var Key: char);
begin
  if Key > #31 then
    begin
      if Key = ',' then Key:= '.';
      if not (Key in ['0'..'9','.','+','-']) then
        begin
          Key:= #0;
          Beep;
        end;
    end;
end;

function GetEditValue(Edit: TEdit; var Value: double): Boolean;
begin
  Result:= False;
  if not Assigned(Edit) then Exit;
  if Edit.Text = '' then
    Exit;
  try
    Value:= StrToFloat(Edit.Text);
    Edit.Text:= PosToString(Value);
    Result:= True;
  except
    Edit.Text:= '?';
  end;
end;

procedure TTouchOffWizDlg.UpdateEdits;
begin
  XDir:= 0;
  if GetEditValue(EditX1,XOffset) then
    XDir:= 1
  else
  if GetEditValue(EditX2,XOffset) then
    XDir:= 2;
  YDir:= 0;
  if GetEditValue(EditY1,YOffset) then
    YDir:= 1
  else
  if GetEditValue(EditY2,YOffset) then
    YDir:= 2;
  ZDir:= 0;
  if GetEditValue(EditZ,ZOffset) then
    ZDir:= 1
end;

procedure TTouchOffWizDlg.BtnOkClick(Sender: TObject);
begin
end;

procedure TTouchOffWizDlg.EditDiaExit(Sender: TObject);
var
  d: double;
begin
  try
    d:= StrToFloat(EditDia.Text);
    EdgeFinderDia:= d;
    EditDia.Text:= PosToString(d);
  except
    EditDia.Text:= '?';
    EditDia.SetFocus;
  end;
end;

procedure TTouchOffWizDlg.EditX1Change(Sender: TObject);
begin
  if ActiveControl = Sender then
    if EditX2.Text <> '' then
      EditX2.Text:= '';
end;

procedure TTouchOffWizDlg.EditX1Exit(Sender: TObject);
begin
  if GetEditValue(EditX1,XOffset) then
    XDir:= 1;
end;

procedure TTouchOffWizDlg.EditX2Change(Sender: TObject);
begin
  if ActiveControl = Sender then
    if EditX1.Text <> '' then
      EditX1.Text:= '';
end;

procedure TTouchOffWizDlg.EditX2Exit(Sender: TObject);
begin
  if GetEditValue(EditX2,XOffset) then
    XDir:= 2;
end;

procedure TTouchOffWizDlg.EditY1Change(Sender: TObject);
begin
  if ActiveControl = Sender then
    if EditY2.Text <> '' then
      EditY2.Text:= '';
end;

procedure TTouchOffWizDlg.EditY1Exit(Sender: TObject);
begin
  if GetEditValue(EditY1,YOffset) then
    YDir:= 1;
end;

procedure TTouchOffWizDlg.EditY2Change(Sender: TObject);
begin
  if ActiveControl = Sender then
    if EditY1.Text <> '' then
      EditY1.Text:= '';
end;

procedure TTouchOffWizDlg.EditY2Exit(Sender: TObject);
begin
  if GetEditValue(EditY2,YOffset) then
    Ydir:= 2;
end;

procedure TTouchOffWizDlg.EditZExit(Sender: TObject);
begin
  GetEditValue(EditZ,ZOffset);
end;

procedure TTouchOffWizDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;

procedure TTouchOffWizDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean
  );
begin
  if ModalResult <> mrCancel then
    begin
       FG10Code:= '';
       GetCode;
       CanClose:= FG10Code <> '';
    end
  else
    CanClose:= True;
end;

procedure TTouchOffWizDlg.InitControls;
var
  i: integer;
  s: string;
begin
  //FValue:= GetRelPos(FAxisNo);
  // LabelPos.Caption:= PosToString(FValue);
  s:= '';
  if Pos('X',Vars.CoordNames) > 0 then s:= 'X';
  if Pos('Y',Vars.CoordNames) > 0 then s:= s + 'Y';
  if Pos('Z',Vars.CoordNames) > 0 then s:= s + 'Z';

  FAxisMask:= s;

  cbCoords.Items.Clear;
  for i:= 0 to CoordSysMax do
    cbCoords.Items.Add(CoordSys[i]);
  FCoord:= Emc.GetActiveCoordSys;
  if FCoord < 0 then FCoord:= 0;
  cbCoords.ItemIndex:= FCoord;
  LabelUnit.Caption:= Vars.UnitStr;
end;

procedure TTouchOffWizDlg.FormCreate(Sender: TObject);
begin
  Readstyle(Self,'touchoffwiz.xml');
  FAxisMask:= '';
  EditDia.Text:= PosToString(EdgeFinderDia);
  YDir:= 0;
  XDir:= 0;
  ZDir:= 0;
  InitControls;
end;

initialization
  {$I touchoffwiz.lrs}

end.

