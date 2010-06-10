unit tooleditdlg;

{$I mocca.inc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls;

type

  { TToolDlg }
  TToolDlg = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    Grid: TStringGrid;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    procedure SetupGrid;
    procedure UpdateGrid;
  end;

procedure EditTools;

implementation

{ TToolDlg }

uses
  mocglb,emc2pas;

procedure EditTools;
var
  Dlg: TToolDlg;
begin
  if  Vars.ToolFile = '' then
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

procedure TToolDlg.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if (Grid.Col > 3) or (Ord(Key) = 8) then Exit;  // BkSpace
  if Key = ',' then Key:= '.';
  if not (Key in ['0'..'9','.','-','+']) then
    begin
      Beep;
      Key:= #0;
    end;
end;

procedure TToolDlg.GridSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  if Value = '' then Exit;
  if (ACol < 1) or (ACol > 3) or (ARow < 1) then Exit;
  if (Value = '?') or (Value = '-') or (Value = '+') then Exit;
  try
    case ACol of
      1: Tools[ARow].toolno:= StrToInt(Value);
      2: Tools[ARow].ZOffset:= StrToFloat(Value);
      3: Tools[ARow].Diameter:= StrToFloat(Value);
    end;
  except
    Grid.Cells[ACol,ARow]:= '?';
    Beep;
  end;
end;

procedure TToolDlg.FormCreate(Sender: TObject);
begin
  // ReadStyle(Self);
  SetupGrid;
  Self.ClientWidth:= (2 *Grid.Left) + Grid.Width;
  UpdateGrid;
  Grid.Editor.OnKeyPress:= @Self.EditorKeyPress;
end;

procedure TToolDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
    SaveToolTable(PChar(Vars.ToolFile));
end;

procedure TToolDlg.SetupGrid;
var
  w,i: integer;
  S: string;
begin
  Grid.ColCount:= 5;
  Grid.RowCount:= CANON_TOOL_MAX + 1;
  for i:= 0 to 4 do
    begin
      S:= ToolCfg[i].Name;
      if S = '' then
        S:= 'Col' + IntToStr(i);
      Grid.Cells[i,0]:= S;
      w:= ToolCfg[i].Width;
      if w < 1 then
        w:= Grid.Canvas.TextExtent(S).cx;
      Grid.ColWidths[i]:= w + 4;
    end;
  w:= 0;
  for i:= 0 to Grid.ColCount - 1 do
    w:= w + Grid.ColWidths[i] + Grid.GridLineWidth;
  Grid.ClientWidth:= w + Grid.GridLineWidth;
end;

procedure TToolDlg.UpdateGrid;
var
  i,r: integer;
begin
  r:= 0;
  for i:= 1 to CANON_TOOL_MAX - 1 do  // ??? First Pocket is 1 ???
    with Tools[i],Grid do
    begin
      if Tools[i].ToolNo >= 0 then
        begin
          inc(r);
          Cells[0,r]:= IntToStr(i);
          Cells[1,r]:= IntToStr(ToolNo);
          Cells[2,r]:= FloatToStrF(ZOffset, ffFixed, 6, 3);
          Cells[3,r]:= FloatToStrF(Diameter, ffFixed, 6, 3);
          // Cells[4,r]:= PChar(Comment;
        end;
    end;
end;

initialization
  {$I tooleditdlg.lrs}

end.

