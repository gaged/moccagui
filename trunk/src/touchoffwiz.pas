unit touchoffwiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs;

type

  { TTouchOffWizDlg }

  TTouchOffWizDlg = class(TForm)
    cbCoords: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label1: TLabel;
    Label4: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  TouchOffWizDlg: TTouchOffWizDlg;

implementation

initialization
  {$I touchoffwiz.lrs}

end.

