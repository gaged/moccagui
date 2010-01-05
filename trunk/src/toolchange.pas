unit toolchange;

{$mode objfpc}{$H+}

{$i mocca.inc}

interface

uses
  Classes, ComCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs;

type

  { TToolChgDlg }

  TToolChgDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    LbTools: TListBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    procedure InitControls;
  end;

function DoChangeTool: integer;

implementation

uses
  mocglb,emc2pas;

const
  Lathe: Boolean = False;

var
  iTool: integer;

function DoChangeTool: integer;
var
  Dlg: TToolChgDlg;
begin
  Result:= 0;
  iTool:= 0;
  if not ToolsInitialized then
    begin
      ShowMessage('No Toolfile set in ' + Vars.IniFile);
      Exit;
    end;
  Application.CreateForm(TToolChgDlg,Dlg);
  try
    if Dlg.ShowModal <> mrCancel then
      Result:= iTool;
  finally
    Dlg.Free;
  end;
end;

procedure TToolChgDlg.FormCreate(Sender: TObject);
begin
  InitControls;
end;

procedure TToolChgDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
 i,c: integer;
 s: string;
begin
 CanClose:= True;
 if ModalResult <> mrOk then Exit;
 i:= LbTools.ItemIndex;
 if i < 0 then Exit;
 s:= TrimLeft(LbTools.Items[i]);
 if Length(s) < 1 then Exit;
 c:= 1;
 while (s[c] in ['0'..'9']) and (c < Length(s)) do inc(c);
 s:= Copy(s,1,c);
 {$IFDEF DEBUG_EMC}
 writeln('Getting Tool: ' + s);
 {$ENDIF}
 try
   iTool:= StrToInt(Trim(s));
 except
   writeln('error in tool-file, got integer as: ' + s);
 end;
end;

procedure TToolChgDlg.FormShow(Sender: TObject);
begin
  LbTools.SetFocus;
end;

procedure TToolChgDlg.InitControls;
var
  i: integer;
  comment: string;
  S: string;
begin
  s:= '';
  LbTools.Clear;
  for i:= 1 to CANON_TOOL_MAX - 1 do  // ??? First Pocket is 1 ???
    with Tools[i] do
      if Tools[i].Id > 0 then
        begin
          Comment:= PChar(ToolComments[i]);
          if Lathe then
            S:= Format('%d %d %.4f %.4f %.4f %.4f %.4f %d %s',
              [i,id,ZOffset,XOffset,Diameter,FrontAngle,BackAngle,Orientation,Comment])
          else
            S:= Format('%2d %10.3f %10.3f %-20s',[id,ZOffset,Diameter,Comment]);
          LbTools.Items.Add(s);
        end;
end;

initialization
  {$I toolchange.lrs}

end.

