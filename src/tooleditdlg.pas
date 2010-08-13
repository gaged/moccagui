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
    procedure FormShow(Sender: TObject);
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
  TToolCol = record
    Title: string;
    Width: integer;
  end;

var
  iPockets: integer;
  ToolColumns: Array[0..MaxToolColumns] of TToolCol;

type
  TToolItem = record
    Pocket: integer;
    Tool: TTool;
    Comment: string;
  end;

var
  T: Array[1..CANON_TOOL_MAX] of TToolItem;

procedure ClearTool(var I: TToolItem);
begin
  I.Pocket:= 0;
  I.Comment:= '';
  with I.Tool do
    begin
      toolno:= 0; zoffset:= 0; xoffset:= 0;
      {$ifdef VER_24}
      yoffset:= 0; aoffset:= 0; boffset:= 0; coffset:= 0;
      uoffset:= 0; voffset:= 0; woffset:= 0;
      {$endif}
      diameter:= 0; frontangle:= 0; backangle:= 0; orientation:= 0;
    end;
end;

procedure InitPockets;
var
  i: integer;
  //Index: integer;
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
  for i:= 0 to MaxToolColumns do
    begin
      ToolColumns[i].Title:=  ToolColTitles[i];
      ToolColumns[i].Width:=  ToolColWidths[i];
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
  SaveToolTable(PChar(Vars.ToolFile));
end;

procedure TToolDlg.UpdateCell(ACol,ARow: integer);
var
  C: TToolCol;
  s: string;
  Item: TToolItem;
begin
  if (ARow < 1) or (ARow > CANON_TOOL_MAX) then
    begin
      writeln('Pocket out of range ' + IntToStr(ARow));
      Exit;
    end;
  if (ARow > iPockets) then
    Exit;
  if (ACol < 0) or (ACol > MaxToolColumns) then
    begin
      writeln('Column out of range ' + IntToStr(aCol));
      Exit;
    end;
  Item:= T[ARow];
  if Item.Pocket > 0 then
    begin
      case ACol of
        0: s:= IntToStr(Item.Pocket);
        1: s:= IntToStr(Item.Tool.toolno);
        2: s:= FloatToStr(Item.Tool.xoffset);
        4: s:= FloatToStr(Item.Tool.zoffset);
        {$ifdef VER_24}
        3: s:= FloatToStr(Item.Tool.yoffset);
        5: s:= FloatToStr(Item.Tool.aoffset);
        6: s:= FloatToStr(Item.Tool.boffset);
        7: s:= FloatToStr(Item.Tool.coffset);
        8: s:= FloatToStr(Item.Tool.uoffset);
        9: s:= FloatToStr(Item.Tool.voffset);
        10: s:= FloatToStr(Item.Tool.woffset);
        {$endif}
        11: s:= FloatToStr(Item.Tool.diameter);
        12: s:= FloatToStr(Item.Tool.frontangle);
        13: s:= FloatToStr(Item.Tool.backangle);
        14: s:= IntToStr(Item.Tool.orientation);
        15: s:= Item.Comment;
      end;
    end;
  Grid.Cells[ACol,ARow]:= s;
end;

procedure TToolDlg.UpdateTool(ACol,ARow: integer);
var
  C: TToolCol;
  s: string;
  // Item: TToolItem;
  i: integer;
begin
  if iPockets < 1 then Exit;
  if (ARow < 1) or (ARow > iPockets) then
    begin
      writeln('Tool-Update, Invalid row: ' + IntToStr(ARow));
      Exit;
    end;
  if (ACol < 1) or (ACol > MaxToolColumns) then
    begin
      writeln('Tool-Update, Invalid Column: ' + IntToStr(ACol));
      Exit;
    end;
  s:= Grid.Cells[ACol,ARow];
  if s = '' then Exit;
  if ACol < MaxToolColumns then
    begin;
      i:= Pos(',',s);
      if i > 0 then s[i]:= '.';
    end;
  try
     case ACol of
        1: T[ARow].Tool.toolno:= StrToInt(s);
        2: T[ARow].Tool.xoffset:= StrToFloat(s);
        4: T[ARow].Tool.zoffset:= StrToFloat(s);
        {$ifdef VER_24}
        3: T[ARow].Tool.yoffset:= StrToFloat(s);
        5: T[ARow].Tool.aoffset:= StrToFloat(s);
        6: T[ARow].Tool.boffset:= StrToFloat(s);
        7: T[ARow].Tool.coffset:= StrToFloat(s);
        8: T[ARow].Tool.uoffset:= StrToFloat(s);
        9: T[ARow].Tool.voffset:= StrToFloat(s);
        10: T[ARow].Tool.woffset:= StrToFloat(s);
        {$endif}
        11: T[ARow].Tool.diameter:= StrToFloat(s);
        12: T[ARow].Tool.frontangle:= StrToFloat(s);
        13: T[ARow].Tool.backangle:= StrToFloat(s);
        14: T[ARow].Tool.orientation:= StrToInt(s);
        15: T[ARow].Comment:= s;
      end;
      if i > 0 then
        UpdateCell(ACol,ARow);
  except
    ShowMessage('"' + s + '" is not a valid entry for ' + C.Title);
    Grid.Cells[ACol,ARow]:= '';
    Grid.Col:= ACol;
    Grid.Row:= ARow;
  end;
end;

procedure TToolDlg.UpdateRow(ARow: integer);
var
  i: integer;
begin
  for i:= 0 to MaxToolColumns do
    begin
      UpdateCell(i,ARow);
    end;
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
  i: integer;
begin
  Grid.ColCount:= MaxToolColumns + 1;
  if iPockets < 1 then
    Grid.RowCount:= 2
  else
    Grid.RowCount:= iPockets + 1;
  for i:= 0 to MaxToolColumns do
    begin
      Grid.Cells[i,0]:= ToolColumns[i].Title;
      Grid.ColWidths[i]:= ToolColumns[i].Width;
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

procedure TToolDlg.FormShow(Sender: TObject);
begin

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

