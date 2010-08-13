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
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
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

var
  iTool: integer;

function DoChangeTool: integer;
var
  Dlg: TToolChgDlg;
begin
  Result:= 0;
  iTool:= 0;
  if Vars.ToolFile = '' then
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
  if Sender = nil then ;
  ReadStyle(Self,'toolchange.xml');
  InitControls;
end;

procedure TToolChgDlg.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Sender = nil then ;
  if Key = #13 then ModalResult:= mrOk else
    if Key = #27 then ModalResult:= mrCancel;
end;

procedure TToolChgDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
 i,c: integer;
 s: string;
begin
  if Sender = nil then ;
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

procedure TToolChgDlg.FormActivate(Sender: TObject);
begin
  if Sender = nil then ;
  {$ifdef LCLGTK2}
  DoBringToFront(Self);
  {$endif}
end;

procedure TToolChgDlg.FormShow(Sender: TObject);
begin
  LbTools.SetFocus;
end;

procedure TToolChgDlg.InitControls;
var
  i: integer;
  s: string;
  Comment: string;
begin
  s:= '';
  LbTools.Clear;
  Comment:= '';
  for i:= 1 to CANON_TOOL_MAX - 1 do  // ??? First Pocket is 1 ???
    with Tools[i] do
      if Tools[i].ToolNo > 0 then
        begin
          Comment:= PChar(ToolComments[i]);
          if Vars.IsLathe then
            s:= Format('%3d %3d %8f %8f %8f %8f %8f %2d %s',
              [i,toolno,zoffset,xoffset,diameter,frontangle,backangle,
               orientation,Comment])
          else
            s:= Format('%3d %3d %8f %8f %s',[i,toolno,zoffset,diameter,Comment]);
          LbTools.Items.Add(s);
        end;
end;

initialization
  {$I toolchange.lrs}

end.

