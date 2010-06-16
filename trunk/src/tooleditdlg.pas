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
    FToolNo: integer;
    procedure SetupGrid;
    //procedure UpdateGrid;
    //procedure UpdateRow(ARow: integer);
  end;

procedure EditTools;

implementation

{ TToolDlg }


uses
  mocglb,emc2pas;

var
  FakePocket : Array[0..CANON_TOOL_MAX] of integer;


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
  SetupGrid;
  Self.ClientWidth:= (2 *Grid.Left) + Grid.Width;
  //UpdateGrid;
  //Grid.Editor.OnKeyPress:= @Self.EditorKeyPress;
end;

procedure TToolDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
    SaveToolTable(PChar(Vars.ToolFile));
end;

procedure TToolDlg.SetupGrid;
var
  i,c: integer;
  MaxItem,MaxLen: integer;
  w,Item: integer;
  s: string;
begin
  if Vars.IsLathe then
    c:= ToolLatheCount
  else
    c:= ToolMillCount;
  Grid.ColCount:= c + 1;
  Grid.RowCount:= CANON_TOOL_MAX;
  for i:= 0 to c do
    begin
      if Vars.IsLathe then
        Item:= ToolIndicesLathe[i]
      else
        Item:= ToolIndicesMill[i];
      if (Item >= 0) and (Item <= ToolLatheCount) then
        begin
          s:= ToolHeaders[item];
          if s = '' then
            s:= 'Item' + IntToStr(item);
          w:= ToolHeaderWidths[item];
          if w < 10 then
            w:= Grid.Canvas.TextExtent(s).cx;
          Grid.Columns[i].Width:= w;
          Grid.Columns[i].Title.Caption:= s;
        end
      else
        begin
          writeln('Toolheader-index out of range.' + IntToStr(Item));
          Exit;
        end;
    end;
  if (MaxLen > 0) and (MaxItem > 0) then
    begin
      s:= ToolHeaders[item];
      w:= Grid.Canvas.TextExtent(s).cx;
      Grid.Columns[0].Width:= w;
    end;
end;

{
procedure TToolDlg.UpdateRow(ARow: integer);
var
  i,c,Pocket: integer;
  T: TTool;
  item: integer;
  iTool: integer;
begin
  if (ARow < 1) or (ARow > CANON_TOOL_MAX) then
    Exit;
  iTool:= ARow - 1;
  Pocket:= FakePockets[iTool];
  if (Pocket < 0) or (Pocket > CANON_TOOL_MAX) then
    begin
      for i:= 0 to Grid.ColCount - 1 do
        Grid.Cells[i,ARow]:= '';
      Exit;
    end;
  T:= Tools[Pocket];
  if Vars.IsLathe then
    c:= ToolLatheCount
  else
    c:= ToolMillCount;
  for i:= 0 to c do
    begin
      if Vars.IsLathe then
        Item:= ToolIndicesLathe[i]
      else
        Item:= ToolIndicesMill[i];
        if (Item < 0) or (Item > ToolHeadersMax) then
          break;
      case Item of
        0: Grid.Cells[i,Index]:= IntToStr(Index);
        1: Grid.Cells[i,Index]:= IntToStr(T.toolno);

          if Tool
        end;
    end;

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
}

initialization
  {$I tooleditdlg.lrs}

end.

