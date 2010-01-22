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
    procedure SaveTools;
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

procedure TToolDlg.SaveTools;
var
  i: integer;
  s: string;
  FileName: PChar;
begin
  if Length(Vars.ToolFile) < 1 then Exit;
  for i:= 1 to CANON_TOOL_MAX - 1 do
    begin
      s:= Grid.Cells[3,i];
      if ToolComments[i] <> nil then
        StrLCopy(ToolComments[i],PChar(s),CANON_TOOL_ENTRY_LEN - 1);
    end;
  FileName:= PChar(Vars.ToolFile);
  SaveToolTable(FileName);
end;

procedure TToolDlg.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if (Grid.Col > 2) or (Ord(Key) = 8) then Exit;  // BkSpace
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
  if (ACol < 1) or (ACol > 2) or (ARow < 1) then Exit;
  if (Value = '?') or (Value = '-') or (Value = '+') then Exit;
  try
    case ACol of
      1: Tools[ARow].ZOffset:= StrToFloat(Value);
      2: Tools[ARow].Diameter:= StrToFloat(Value);
    end;
  except
    Grid.Cells[ACol,ARow]:= '?';
    Beep;
  end;
end;

procedure TToolDlg.FormCreate(Sender: TObject);
begin
  SetupGrid;
  UpdateGrid;
  Grid.Editor.OnKeyPress:= @Self.EditorKeyPress;
end;

procedure TToolDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
    SaveTools;
end;

procedure TToolDlg.SetupGrid;
var
  w,i: integer;
begin
  Grid.ColCount:= 4;
  for i:= 0 to 3 do
    Grid.Cells[i,0]:= ToolsMill[i+1];
  Grid.RowCount:= CANON_TOOL_MAX + 1;
  w:= Grid.Canvas.TextWidth('X');
  if w < 8 then w:= 8;
  Grid.ColWidths[0]:= w * 3;
  Grid.ColWidths[1]:= w * 10;
  Grid.ColWidths[2]:= w * 10;
  Grid.ColWidths[3]:= w * 20;
  w:= 0;
  for i:= 0 to Grid.ColCount - 1 do
    w:= w + Grid.ColWidths[i]; //   + Grid.GridLineWidth;
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
      if Tools[i].Id >= 0 then
        begin
          inc(r);
          Cells[0,r]:= IntToStr(Id);
          Cells[1,r]:= FloatToStrF(ZOffset, ffFixed, 6, 3);
          Cells[2,r]:= FloatToStrF(Diameter, ffFixed, 6, 3);
          Cells[3,r]:= PChar(ToolComments[i]);
        end;
    end;
end;

initialization
  {$I tooleditdlg.lrs}

end.

