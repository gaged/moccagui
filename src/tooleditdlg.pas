unit tooleditdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids;

type

  { TToolDlg }
  TToolDlg = class(TForm)
    Grid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridEditingDone(Sender: TObject);
  private
    procedure SetupGrid;
    procedure UpdateGrid;
  public
    procedure LoadTools;
  end;

procedure EditTools;

implementation

{ TToolDlg }

uses
  mocglb,emc2pas;

const Lathe: Boolean = false;

procedure EditTools;
var
  Dlg: TToolDlg;
begin
  if not ToolsInitialized then
    begin
      ShowMessage('No Toolfile set in ' + Vars.IniFile);
      Exit;
    end;
  Application.CreateForm(TToolDlg,Dlg);
  if Assigned(Dlg) then
    begin
      Dlg.ShowModal;
      Dlg.Free;
    end;
end;

procedure TToolDlg.FormCreate(Sender: TObject);
begin
  SetupGrid;
  UpdateGrid;
end;

procedure TToolDlg.FormDestroy(Sender: TObject);
begin
end;

procedure TToolDlg.GridEditingDone(Sender: TObject);
begin
end;

procedure TToolDlg.SetupGrid;
var
  w,i: integer;
begin
  if Lathe then
    begin
      Grid.ColCount:= 9;
      for i:= 0 to 8 do
        Grid.Cells[i,0]:= ToolsLathe[i];
    end
  else
    begin
      Grid.ColCount:= 5;
      for i:= 0 to 4 do
        Grid.Cells[i,0]:= ToolsMill[i];
     end;
  Grid.RowCount:= CANON_TOOL_MAX + 1;
  w:= Round(Abs(Grid.Font.Height) * 0.6);
  if w = 0 then w:= 10;
  if Lathe then
    begin
      for i:= 0 to Grid.ColCount - 1 do
        Grid.ColWidths[i]:= w * Length(ToolsLathe[i]);
    end
  else
    for i:= 0 to Grid.ColCount - 1 do
      Grid.ColWidths[i]:= w * Length(ToolsMill[i]);
  w:= 0;
  for i:= 0 to Grid.ColCount - 1 do
    w:= w + Grid.ColWidths[i] + Grid.GridLineWidth;
  Grid.ClientWidth:= w + Grid.GridLineWidth;
end;

procedure TToolDlg.UpdateGrid;
var
  i,r: integer;
  Comment: PChar;
begin
  r:= 0;
  for i:= 1 to CANON_TOOL_MAX - 1 do  // ??? First Pocket is 1 ???
    with Tools[i],Grid do
    begin
      if Tools[i].Id >= 0 then
        begin
          inc(r);
          Comment:= ToolComments[i];
          if Lathe then
            Cells[8,r]:= PChar(Comment)
          else
            Cells[4,r]:= PChar(Comment);
          Cells[0,r]:= IntToStr(i);
          Cells[1,r]:= IntToStr(Id);
          Cells[2,r]:= FloatToStr(ZOffset);
          if Lathe then
            begin
              Cells[3,r]:= FloatToStr(XOffset);
              Cells[4,r]:= FloatToStr(Diameter);
              Cells[5,r]:= FloatToStr(Frontangle);
              Cells[6,r]:= FloatToStr(Backangle);
              Cells[7,r]:= IntToStr(Orientation);
            end
          else
            Cells[3,r]:= FloatToStr(Diameter);
        end;
    end;
end;

procedure TToolDlg.LoadTools;
var
  FileName: PChar;
begin

end;

initialization
  {$I tooleditdlg.lrs}

end.

