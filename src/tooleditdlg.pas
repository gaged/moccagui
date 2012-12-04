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
    BtnAddTool: TButton;
    BtnDeleteTool: TButton;
    Grid: TStringGrid;
    procedure BtnAddToolClick(Sender: TObject);
    procedure BtnChangePocClick(Sender: TObject);
    procedure BtnDeleteToolClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    procedure EditorEditingDone(Sender: TObject);
  private
    procedure InitGrid;
    procedure UpdateCell(ACol,ARow: integer);
    procedure UpdateRow(ARow: integer);
    procedure UpdateTool(ACol,ARow: integer);
    procedure AddTool;
    procedure DeleteTool;
    procedure ChangePocket;
    function  CheckToolNo: Boolean;
  end;

procedure EditTools;

implementation

{ TToolDlg }


uses
  mocglb,emc2pas;


type
  TToolItem = record
    Pocket: integer;
    Tool: TTool;
    Comment: string;
  end;

var
  T: Array[1..CANON_TOOL_MAX] of TToolItem;
  iPockets: integer;   // number of pockets
  Map: string;         // map of visible columns


procedure ClearTool(var Item: TToolItem);
begin
  Item.Pocket:= 0;
  Item.Comment:= '';
  with Item.Tool do
    begin
      toolno:= 0; zoffset:= 0; xoffset:= 0;
      {$ifndef VER_23}
      yoffset:= 0; aoffset:= 0; boffset:= 0; coffset:= 0;
      uoffset:= 0; voffset:= 0; woffset:= 0;
      {$endif}
      diameter:= 0; frontangle:= 0; backangle:= 0; orientation:= 0;
    end;
end;

procedure InitPockets;
var
  i: integer;
begin
  iPockets:= 0;
  for i:= 1 to CANON_TOOL_MAX - 1 do
    begin
      ClearTool(T[i]);
      if Tools[i].toolno > 0 then
        begin
          inc(iPockets);
          if iPockets > CANON_TOOL_MAX then
            break;
          T[iPockets].Pocket:= iPockets;
          T[iPockets].Tool:= Tools[i];
          T[iPockets].Comment:= PChar(ToolComments[i]);
        end;
    end;
end;

procedure SavePockets;
var
  i: integer;
begin
  for i:= 1 to CANON_TOOL_MAX - 1 do
    begin
      Tools[i]:= T[i].Tool;
      ToolComments[i]:= PChar(T[i].Comment);
    end;
  {$IFNDEF VER_26}
  SaveToolTable(PChar(Vars.ToolFile));
  {$ENDIF}
end;

procedure TToolDlg.UpdateCell(ACol,ARow: integer);
var
  s: string;
  Item: TToolItem;
  c: Char;
begin
  if (ARow < 1) or (ARow > CANON_TOOL_MAX) then
    begin
      writeln('Pocket out of range ' + IntToStr(ARow));
      Exit;
    end;
  if (ARow > iPockets) then Exit;
  if (ACol < 0) or (ACol >= Length(Map)) then
    begin
      writeln('Column out of range ' + IntToStr(ACol));
      Exit;
    end;
  Item:= T[ARow];
  s:= '';
  if Item.Pocket > 0 then
    begin
      c:= Map[ACol+1];
      case c of
        'P': s:= IntToStr(Item.Pocket);
        'T': s:= IntToStr(Item.Tool.toolno);
        'X': s:= FloatToStr(Item.Tool.xoffset);
        'Z': s:= FloatToStr(Item.Tool.zoffset);
        {$ifndef VER_23}
        'Y': s:= FloatToStr(Item.Tool.yoffset);
        'A': s:= FloatToStr(Item.Tool.aoffset);
        'B': s:= FloatToStr(Item.Tool.boffset);
        'C': s:= FloatToStr(Item.Tool.coffset);
        'U': s:= FloatToStr(Item.Tool.uoffset);
        'V': s:= FloatToStr(Item.Tool.voffset);
        'W': s:= FloatToStr(Item.Tool.woffset);
        {$endif}
        'D': s:= FloatToStr(Item.Tool.diameter);
        'F': s:= FloatToStr(Item.Tool.frontangle);
        'K': s:= FloatToStr(Item.Tool.backangle);
        'O': s:= IntToStr(Item.Tool.orientation);
        'Q': s:= Item.Comment;
      end;
    end;
  Grid.Cells[ACol,ARow]:= s;
end;


procedure TToolDlg.UpdateTool(ACol,ARow: integer);
var
  c: Char;
  s: string;
  i: integer;
begin
  if iPockets < 1 then Exit;
  if (ARow < 1) or (ARow > iPockets) then
    begin
      writeln('Tool-Update, Invalid row: ' + IntToStr(ARow));
      Exit;
    end;
  if (ACol < 1) or (ACol >= Length(Map)) then
    begin
      writeln('Tool-Update, Invalid Column: ' + IntToStr(ACol));
      Exit;
    end;
  s:= Grid.Cells[ACol,ARow];
  if s = '' then Exit;
  c:= Map[ACol+1];
  if c <> 'Q' then
    begin;
      i:= Pos(',',s);
      if i > 0 then s[i]:= '.';
    end;
  try
     case C of
        'T': T[ARow].Tool.toolno:= StrToInt(s);
        'X': T[ARow].Tool.xoffset:= StrToFloat(s);
        'Z': T[ARow].Tool.zoffset:= StrToFloat(s);
        {$ifndef VER_23}
        'Y': T[ARow].Tool.yoffset:= StrToFloat(s);
        'A': T[ARow].Tool.aoffset:= StrToFloat(s);
        'B': T[ARow].Tool.boffset:= StrToFloat(s);
        'C': T[ARow].Tool.coffset:= StrToFloat(s);
        'U': T[ARow].Tool.uoffset:= StrToFloat(s);
        'V': T[ARow].Tool.voffset:= StrToFloat(s);
        'W': T[ARow].Tool.woffset:= StrToFloat(s);
        {$endif}
        'D': T[ARow].Tool.diameter:= StrToFloat(s);
        'F': T[ARow].Tool.frontangle:= StrToFloat(s);
        'K': T[ARow].Tool.backangle:= StrToFloat(s);
        'O': T[ARow].Tool.orientation:= StrToInt(s);
        'Q': T[ARow].Comment:= s;
      end;
    UpdateCell(ACol,ARow);
  except
    ShowMessage('"' + s + '" is not a valid entry for ' + C);
    Grid.Cells[ACol,ARow]:= '';
    Grid.Col:= ACol;
    Grid.Row:= ARow;
  end;
end;

procedure TToolDlg.UpdateRow(ARow: integer);
var
  i: integer;
begin
  for i:= 1 to Length(Map) do
    UpdateCell(i-1,ARow);
end;

function TToolDlg.CheckToolNo: Boolean;
var
  Msg: string;
  i: integer;
begin
  if iPockets < 1 then
    begin
      Result:= True;
      Exit;
    end;
  Result:= False;
  Msg:= '';
  for i:= 1 to iPockets do
    begin
      if (T[i].Pocket < 1) or (T[i].Pocket > CANON_TOOL_MAX) then
        Msg:= 'Pocket No ' + IntToStr(i) + ' out of range'
      else
      if (T[i].Tool.toolno < 1) then
        Msg:= Format('%s %d %s %d %s',['Tool Number',T[i].Tool.toolno,'of Pocket',i,'is not valid']);
      if Msg <> '' then
        Break;
    end;
  Result:= Msg = '';
  if not Result then
    ShowMessage(Msg);
end;

procedure TToolDlg.AddTool;
begin
  if not CheckToolNo then
    Exit;
  if iPockets < CANON_TOOL_MAX - 1 then
    begin
      Grid.RowCount:= iPockets + 2;
      inc(iPockets);
      T[iPockets].Pocket:= iPockets;
      UpdateRow(iPockets);
    end
  else
    ShowMessage('Too many Tools, cannot add a new tool');
end;

procedure UpdatePocketNo(StartPos: integer);
var
  i: integer;
begin
  for i:= StartPos to iPockets do
    T[i].Pocket:= i;
end;

procedure TToolDlg.DeleteTool;
var
  i,Pocket: integer;
  Msg: string;
begin
  if iPockets < 1 then
    begin
      writeln('Nothing to delete!');
      Exit;
    end;
  Pocket:= Grid.Row;
  if (Pocket < 1) or (Pocket > CANON_TOOL_MAX -1) then
    begin
      writeln('Delete Tool, invalid pocket.');
      Exit;
    end;
  if T[Pocket].Tool.toolno > 0 then
    with T[Pocket] do
      begin
        Msg:= Format('%s %d %s %d',['Delete Pocket',Pocket,'with Tool no',Tool.ToolNo]);
        if MessageDlg(Msg,mtConfirmation,[mbOk,mbCancel],0) = mrCancel then
          Exit;
      end;
  if Pocket = iPockets then
    begin
      Writeln('Delete End');
      ClearTool(T[iPockets]);
      Dec(iPockets);
      Grid.RowCount:= iPockets + 1;
      UpdatePocketNo(Pocket);
    end
  else
    begin
      Writeln('Delete mid');
      for i:= Pocket + 1 to iPockets do
        T[i-1]:= T[i];
      ClearTool(T[iPockets]);
      Dec(iPockets);
      Grid.RowCount:= iPockets + 1;
      if (iPockets > 1) then
        begin
          UpdatePocketNo(Pocket);
          for i:= Pocket to iPockets do
            UpdateRow(i);
        end;
    end;
end;

procedure TToolDlg.ChangePocket;
begin
end;

procedure TToolDlg.InitGrid;
var
 i,n: integer;
 c: Char;
begin
  Map:= 'PTQD';
  if Vars.IsLathe then
    Map:= Map + 'FKO';
  if Pos('X',Vars.CoordNames) > 0 then Map:= Map + 'X';
  if Pos('Z',Vars.CoordNames) > 0 then Map:= Map + 'Z';
  {$ifndef VER_23}
  if Pos('Y',Vars.CoordNames) > 0 then Map:= Map + 'Y';
  if Pos('A',Vars.CoordNames) > 0 then Map:= Map + 'A';
  if Pos('B',Vars.CoordNames) > 0 then Map:= Map + 'B';
  if Pos('C',Vars.CoordNames) > 0 then Map:= Map + 'C';
  if Pos('U',Vars.CoordNames) > 0 then Map:= Map + 'U';
  if Pos('V',Vars.CoordNames) > 0 then Map:= Map + 'V';
  if Pos('W',Vars.CoordNames) > 0 then Map:= Map + 'W';
  {$endif}

  Writeln('columnmap: ', Map);
  Grid.ColCount:= Length(Map);

  writeln('colcount: ', grid.ColCount);
  if iPockets < 1 then
    Grid.RowCount:= 2
  else
    Grid.RowCount:= iPockets + 1;
  for i:= 0 to Grid.ColCount - 1 do
    begin
      c:= Map[i+1];
      n:= Pos(c,ToolColMap) - 1;
      if (n < 0) or (n > MaxToolColumns) then
        begin
          writeln('Error in Toolmap, illegal Column: ' + c);
          Exit;
        end;
      Grid.Cells[i,0]:= ToolColTitles[n];
      Grid.ColWidths[i]:= ToolColWidths[n];
    end;
  for i:= 1 to iPockets do
    UpdateRow(i);
end;

procedure EditTools;
var
  Dlg: TToolDlg;
begin
  if  Vars.ToolFile = '' then
    begin
      ShowMessage('No Toolfile set in ' + Vars.IniFile);
      Exit;
    end;
  InitPockets;
  Application.CreateForm(TToolDlg,Dlg);
  try
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TToolDlg.EditorEditingDone(Sender: TObject);
begin
  if Grid.Modified then
    begin
      UpdateTool(Grid.Col,Grid.Row);
    end;
end;

procedure TToolDlg.EditorKeyPress(Sender: TObject; var Key: char);
var
  i: integer;
begin
  i:= Grid.Col;
  if (i = MaxToolColumns) or (Ord(Key) = 8) then Exit;  // BkSpace
  if Key = ',' then Key:= '.';
  if not (Key in ['0'..'9','.','-','+']) then
    begin
      Beep;
      Key:= #0;
    end;
end;

procedure TToolDlg.FormCreate(Sender: TObject);
begin
  Readstyle(Self,'tooledit.xml');
  InitGrid;
end;

procedure TToolDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
    SavePockets;
end;

procedure TToolDlg.BtnAddToolClick(Sender: TObject);
begin
  AddTool;
end;

procedure TToolDlg.BtnChangePocClick(Sender: TObject);
begin
  ChangePocket;
end;

procedure TToolDlg.BtnDeleteToolClick(Sender: TObject);
begin
  DeleteTool;
end;

procedure TToolDlg.FormActivate(Sender: TObject);
begin
  if Sender = nil then ;
  {$ifdef LCLGTK2}
  DoBringToFront(Self);
  {$endif}
end;



initialization
  {$I tooleditdlg.lrs}

end.

