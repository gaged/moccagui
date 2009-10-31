unit HomeDlg; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls;

type

  { THomeDlgForm }

  THomeDlgForm = class(TForm)
    BtnRefX: TBitBtn;
    BtnUnRefX: TBitBtn;
    BtnRefY: TBitBtn;
    BtnUnRefY: TBitBtn;
    BtnRefZ: TBitBtn;
    BtnRefB: TBitBtn;
    BtnUnRefZ: TBitBtn;
    BtnUnRefB: TBitBtn;
    BtnRefC: TBitBtn;
    BtnUnRefC: TBitBtn;
    Label0: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    PanelSide: TPanel;
    procedure BtnRefBClick(Sender: TObject);
    procedure BtnRefCClick(Sender: TObject);
    procedure BtnRefXClick(Sender: TObject);
    procedure BtnRefYClick(Sender: TObject);
    procedure BtnRefZClick(Sender: TObject);
    procedure BtnUnRefBClick(Sender: TObject);
    procedure BtnUnRefCClick(Sender: TObject);
    procedure BtnUnRefXClick(Sender: TObject);
    procedure BtnUnRefYClick(Sender: TObject);
    procedure BtnUnRefZClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure InitButtons;
  public
    { public declarations }
  end; 

var
  HomeDlgForm: THomeDlgForm;

implementation

{ THomeDlgForm }

uses
  emc2pas,mocglobal;
  
procedure THomeDlgForm.InitButtons;
var
  C: integer;
begin
  C:= E.NumAxes;
  if C > 4 then Label4.Caption:= E.CoordNames[5];
  if C > 3 then Label3.Caption:= E.CoordNames[4];
  if C > 2 then Label2.Caption:= E.CoordNames[3];
  if C > 1 then Label1.Caption:= E.CoordNames[2];
  if C > 0 then Label0.Caption:= E.CoordNames[1];

  if C < 5 then BtnRefC.Enabled:= False;
  if C < 4 then BtnRefB.Enabled:= False;
  if C < 3 then BtnRefZ.Enabled:= False;
  if C < 2 then BtnRefY.Enabled:= False;
  if C < 1 then BtnRefX.Enabled:= False;

  if C < 5 then BtnUnRefC.Enabled:= False;
  if C < 4 then BtnUnRefB.Enabled:= False;
  if C < 3 then BtnUnRefZ.Enabled:= False;
  if C < 2 then BtnUnRefY.Enabled:= False;
  if C < 1 then BtnUNRefX.Enabled:= False;
end;

procedure THomeDlgForm.FormCreate(Sender: TObject);
begin
  InitButtons;
end;

procedure THomeDlgForm.BtnRefXClick(Sender: TObject);
begin
  sendHome(AxisId('X'));
  Close;
end;

procedure THomeDlgForm.BtnRefBClick(Sender: TObject);
begin
  sendHome(3);
  Close;
end;

procedure THomeDlgForm.BtnRefCClick(Sender: TObject);
begin
  sendHome(4);
  Close;
end;

procedure THomeDlgForm.BtnRefYClick(Sender: TObject);
begin
  sendHome(AxisId('Y'));
  Close;
end;

procedure THomeDlgForm.BtnRefZClick(Sender: TObject);
begin
  sendHome(AxisId('Z'));
  Close;
end;

procedure THomeDlgForm.BtnUnRefBClick(Sender: TObject);
begin
  sendUnHome(3);
  Close;
end;

procedure THomeDlgForm.BtnUnRefCClick(Sender: TObject);
begin
  sendUnHome(4);
  Close;
end;

procedure THomeDlgForm.BtnUnRefXClick(Sender: TObject);
begin
  sendUnHome(AxisId('X'));
  Close;
end;

procedure THomeDlgForm.BtnUnRefYClick(Sender: TObject);
begin
  sendUnHome(AxisId('Y'));
  Close;
end;

procedure THomeDlgForm.BtnUnRefZClick(Sender: TObject);
begin
  sendUnHome(AxisId('Z'));
  Close;
end;


initialization
  {$I homedlg.lrs}

end.

