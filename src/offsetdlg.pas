unit offsetdlg;

{$I mocca.inc}

interface

uses
  Classes, ExtCtrls, Grids, StdCtrls, SysUtils, FileUtil,
  LResources, Forms, Controls, Graphics, Dialogs;

type

  { TOffsetsDlg }

  TOffsetsDlg = class(TForm)
    BtnCancel: TButton;
    BtnOk: TButton;
    Grid: TStringGrid;
    PanelBottom: TPanel;
    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
  private
    VarFile: TStringList;
    VOffs: integer;
    function GetFirstCoord: boolean;
    function GetValue(Coord, Axis: integer): Boolean;
    procedure SetValue(Coord, Axis: integer);
  public
    procedure SizeGrid;
    procedure InitControls;
    procedure LoadVars;
    procedure SaveVars;
  end;

procedure EditOffsets;

implementation

uses
  mocglb,emc2pas;

{ TOffsetsDlg }

type
  TOffsets = array[0..MaxAxes-1] of double;

var
  Offsets: array[0..CoordSysMax] of TOffsets;

procedure EditOffsets;
var
  Dlg: TOffsetsDlg;
begin
  Application.CreateForm(TOffsetsDlg,Dlg);
  if Assigned(Dlg) then
    begin
      Dlg.ShowModal;
      Dlg.Free;
    end;
end;


procedure TOffsetsDlg.FormCreate(Sender: TObject);
begin
  VarFile:= TStringList.Create;
  VOffs:= -1;
  InitControls;
  SizeGrid;
  PanelBottom.Width:= Grid.Width;
  PanelBottom.Left:= 0;
  PanelBottom.Top:= Grid.Height + 1;
  Self.ClientWidth:= Grid.Width;
  Self.ClientHeight:= Grid.Height + PanelBottom.Height;
  Grid.Editor.OnKeyPress:= @Self.EditorKeyPress;
  LoadVars;
end;

procedure TOffsetsDlg.BtnOkClick(Sender: TObject);
begin
  SaveVars;
  sendTaskPlanInit;
end;

procedure TOffsetsDlg.FormDestroy(Sender: TObject);
begin
  if Assigned(VarFile) then VarFile.Free;
end;

procedure TOffsetsDlg.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if (Grid.Col < 2) or (Ord(Key) = 8) then Exit;  // BkSpace
  // make german keyboards usable, this allows to use the keypad
  // as emc does not use the locales settings in the interpreter STRTOD
  // the german LC_NUMERIC does not work at all
  if Key = ',' then Key:= '.';
  if not (Key in ['0'..'9','.','-','+']) then
    begin
      Beep;
      Key:= #0;
    end;
end;

procedure TOffsetsDlg.GridSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  if (Value = '?') or (Value = '-') or (Value = '+') then Exit;
  if (ACol > 1) and (ARow > 0) and (Value <> '') then
    begin
      try
        Offsets[ARow-1][ACol-2]:= StrToFloat(Value);
      except
        Grid.Cells[ACol,ARow]:= '?';
        Beep;
      end;
    end;
end;

function TOffsetsDlg.GetFirstCoord: Boolean;
var
  s: string;
  i: integer;
begin
  VOffs:= -1;
  if VarFile.Count < 1 then Exit;
  for i:= 0 to VarFile.Count - 1 do
    begin
      s:= VarFile[i];
      if Length(s) > 4 then
        begin
          s:= copy(s,1,4);
          if s = CoordSysVar then
            VOffs:= i;
        end;
      if (VOffs <> -1) then Break;
    end;
  Result:= not (VOffs < 0);
end;

function TOffsetsDlg.GetValue(Coord, Axis: integer): Boolean;
var
  s: string;
  i: integer;
begin
  Result:= False;
  if VOffs < 0 then Exit;
  i:= VOffs + (Coord * CoordSysInc) + Axis;
  if (i > 0) and (i < VarFile.Count) then
    begin
      s:= VarFile[i];
      if Length(s) < 6 then Exit;
      s:= Trim(copy(s,5,length(s) - 5));
      try
        Offsets[Coord][Axis]:= StrToFloat(s);
        Result:= True;
      except
      end;
    end;
end;

procedure TOffsetsDlg.SetValue(Coord,Axis: integer);
var
  i: integer;
  s: string;
begin
  if VOffs < 0 then Exit;
  i:= VOffs + (Coord * CoordSysInc) + Axis;
  if (i > 0) and (i < VarFile.Count) then
    begin
       s:= VarFile[i];
       if Length(s) < 6 then Exit;
       s:= copy(s,1,4) +#9 + FloatToStr(Offsets[Coord][Axis]);
       VarFile[i]:= s;
     end;
end;

procedure TOffsetsDlg.LoadVars;
var
  a,c: integer;
begin
  VarFile.LoadFromFile(Vars.ParamFile);
  if VarFile.Count < 1 then
    begin
      LastError:= 'error loading file: ' + Vars.ParamFile;
      Exit;
    end;
  if not GetFirstCoord then
    begin
      LastError:= 'error reading var "' + CoordSysVar + '"';
      Exit;
    end;
  for c:= 0 to CoordSysMax do
    for a:= 0 to Vars.NumAxes - 1 do
      if GetValue(c,a) then
        Grid.Cells[a+2,c+1]:= FloatToStrF(Offsets[c][a],ffFixed,6,4);
end;

procedure TOffsetsDlg.SaveVars;
var
  a,c: integer;
begin
  if VOffs < 0 then Exit;
  for c:= 0 to CoordSysMax do
    for a:= 0 to Vars.NumAxes - 1 do
      SetValue(c,a);
  VarFile.SaveToFile(Vars.ParamFile);
end;

procedure TOffsetsDlg.SizeGrid;
var
  i,w,h: integer;
begin
  w:= trunc(Abs(Grid.Font.Height) * 0.6);
  if w > 1 then
    begin
      for i:= 2 to 4 do
        Grid.ColWidths[i]:= w * 14;
    end;
  with Grid do
    begin
      w:= GridLineWidth;
      h:= w;
      for i:= 0 to ColCount - 1 do
        w:= w + ColWidths[i] + GridLineWidth;
      for i:= 0 to RowCount - 1 do
        h:= h + RowHeights[i] + GridLineWidth;
      ClientWidth:= w;
      ClientHeight:= h;
    end;
end;

procedure TOffsetsDlg.InitControls;
var
  i: integer;
begin
  Grid.RowCount:= CoordSysMax + 2;
  Grid.ColCount:= Vars.NumAxes + 2;
  for i:= 0 to CoordSysMax do
    Grid.Cells[0,i+1]:= CoordSys[i];
  for i:= 0 to Vars.NumAxes - 1 do
    Grid.Cells[i+2,0]:= Vars.CoordNames[i+1];
  LoadVars;
end;



initialization
  {$I offsetdlg.lrs}

end.

