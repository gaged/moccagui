unit simulator;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, Menus, StdCtrls, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs;

type

  { TSimulatorDlg }

  TSimulatorDlg = class(TForm)
    BtnGen: TButton;
    BtnCancel: TButton;
    cbSample: TComboBox;
    EditTool: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    FTool: single;
    FRes: integer;
    procedure UpdateEdit;
  public
  end; 

function Show3DPreviewDlg(var Res: integer; var ToolDia: single): Boolean;

implementation

uses
  mocglb;

function Show3DPreviewDlg(var Res: integer; var ToolDia: single): Boolean;
var
  Dlg: TSimulatorDlg;
begin
  Result:= False;
  Res:= 200;
  ToolDia:= 2;
  Application.CreateForm(TSimulatorDlg,Dlg);
  try
    Result:= Dlg.ShowModal = mrOk;
    if Result then
      Res:= Dlg.FRes;
      ToolDia:= Dlg.FTool;
  finally
    Dlg.Free;
  end;
end;

{ TSimulatorDlg }

procedure TSimulatorDlg.UpdateEdit;
var
  i: integer;
  s: string;
  v: single;
begin
  s:= EditTool.Text;
  i:= Pos(',',s);
  if i > 0 then
    begin
      s[i]:= '.';
      EditTool.Text:= s;
    end;
  try
    FTool:= StrToFloat(EditTool.Text);
  except
    on E: Exception do
      begin
        ShowMessage(E.Message);
        EditTool.SetFocus;
      end;
  end;
end;

procedure TSimulatorDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  UpdateEdit;
  if Vars.Metric then
    FTool:= FTool /25.4;
  // if FTool < 0 then FTool:= 2;
  if cbSample.ItemIndex = 0 then FRes:= 200 else
  if cbSample.ItemIndex = 1 then FRes:= 400 else
    FRes:= 800;
  CanClose:= True;
end;

procedure TSimulatorDlg.FormCreate(Sender: TObject);
begin

end;

initialization
  {$I simulator.lrs}

end.

