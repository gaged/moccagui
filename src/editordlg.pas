unit editordlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs;

type

  { TEditorDlg }

  TEditorDlg = class(TForm)
    BtnClose: TButton;
    Memo: TMemo;
    procedure Click(Sender: TObject);
  private
  public
    procedure ShowEditor;
    procedure HideEditor;
    procedure ActivateSelf;
    // procedure UpdateSelf;
    // procedure InitControls
    function  HandleCommand(Cmd: integer): Boolean;
  end;

var
  EdDlg: TEditorDlg;

implementation

uses
  Buttons,mocglb;

function TEditorDlg.HandleCommand(Cmd: integer): Boolean;
begin
  Result:= False;
  if Cmd = 0 then;
end;

procedure TEditorDlg.Click(Sender: TObject);
begin
end;

procedure TEditorDlg.ActivateSelf;
begin
end;

procedure TEditorDlg.ShowEditor;
begin
  Self.Visible:= True;
end;

procedure TEditorDlg.HideEditor;
begin
  Self.Visible:= False;
end;

initialization
  {$I editordlg.lrs}

end.

