unit setup; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type

  { TSetupDlg }

  TSetupDlg = class(TForm)
    BtnFont: TButton;
    EdFont: TEdit;
    FontDlg: TFontDialog;
    Label1: TLabel;
    PageControl1: TPageControl;
    PgOpt: TTabSheet;
    PgGl: TTabSheet;
    procedure BtnFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    FForm: TWinControl;
  end; 

procedure EditSetup(AWinControl: TWinControl);

implementation

{ TSetupDlg }

procedure EditSetup(AWinControl: TWinControl);
var
  Dlg: TSetUpDlg;
begin
  Application.CreateForm(TSetupDlg,Dlg);
  Dlg.FForm:= AWinControl;
  Dlg.ShowModal;
end;

procedure TSetupDlg.FormCreate(Sender: TObject);
begin

end;

procedure TSetupDlg.BtnFontClick(Sender: TObject);
begin
  if FontDlg.Execute then
    begin
      EdFont.Font.Assign(FontDlg.Font);
      if Assigned(FForm) then
        FForm.Font.Assign(FontDlg.Font);
    end;
   // MainForm.Font.Assign(FontDlg.Font);
end;

initialization
  {$I setup.lrs}

end.

