unit startctrl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type
  TStartCtrlDlg = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end; 

procedure ShowStartDlg;

implementation

uses
  mocemc;

procedure ShowStartDlg;
var
  Dlg: TStartCtrlDlg;
begin
  Application.CreateForm(TStartCtrlDlg,Dlg);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

initialization
  {$I startctrl.lrs}
end.

