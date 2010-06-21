unit emcmsgbox; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TMsgForm }

  TMsgForm = class(TForm)
    LBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure LBoxClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure PopMessages;
  end;

var
  MsgForm: TMsgForm;

implementation

uses
  mocglb;

{ TMsgForm }

procedure TMsgForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #27) or (Key = #13) then
    Close
  else
    if (Key = ' ') then
      begin
        GlobalErrors.Clear;
        LBox.Items.Clear;
        Hide;
      end;
end;

procedure TMsgForm.FormCreate(Sender: TObject);
begin
  // ReadStyle(Self);
end;

procedure TMsgForm.LBoxClick(Sender: TObject);
begin
  Hide;
end;

procedure TMsgForm.PopMessages;
begin
  LBox.Items.Assign(GlobalErrors);
  Show;
end;

initialization

  {$I emcmsgbox.lrs}

end.

