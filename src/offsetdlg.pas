unit offsetdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, Grids, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs;

type

  { TOffsetsDlg }

  TOffsetsDlg = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    Grid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Vars: TStringList;
    function GetVar(VarNo: integer; var Value: Double): Boolean;
    // function CompareText(const S1,S2: string): integer;
  public
    procedure InitControls;
    procedure LoadParamFile;
    procedure LoadVars;
    procedure SaveVars;
  end;


implementation

uses
  mocglb;

{ TOffsetsDlg }

var
  VOffs: array[1..9] of integer;

procedure TOffsetsDlg.FormCreate(Sender: TObject);
begin
  Vars:= TStringList.Create;
  LoadParamFile;
  InitControls;
end;

procedure TOffsetsDlg.FormDestroy(Sender: TObject);
begin
  if Assigned(Vars) then Vars.Free;
end;


function TOffsetsDlg.GetVar(VarNo: integer; var Value: Double): Boolean;
var
  s,svar,svalue: string;
  i,N: integer;
begin
  Result:= False;
  for i:= 0 to Vars.Count - 1 do
    begin
      N:= 0;
      s:= Vars[i];
      if Length(S) > 5 then
      begin
        svar:= Trim(copy(s,1,4));
        if Length(svar) = 4 then
          N:= StrToInt(svar);
      end;
      if N > 0 then
        if N = VarNo then
          break;
    end;
  if N > 0 then
    begin
      svalue:= Trim(copy(s,5,length(s) - 5));
      Value:= StrToFloat(sValue);
      Result:= True;
    end;
end;

procedure TOffsetsDlg.LoadVars;
var
  i,vi: integer;
  V: integer;
  Value: Double;
begin
  V:= G54Variable;
  for i:= 1 to G5SysMax do
    begin
      for vi:= 0 to emcVars.NumAxes - 1 do
        begin
          if GetVar(V + vi,Value) then
            Grid.Cells[vi+1,i]:= FloatToStr(Value)
          else
            Grid.Cells[vi+1,i]:= '?';
        end;
      V:= V + G54Inc;
    end;
end;

procedure TOffsetsDlg.SaveVars;
begin

end;

procedure TOffsetsDlg.InitControls;
var
  i: integer;
begin
  with Grid do
   begin
     RowCount:= G5SysMax + 1;
     ColCount:= emcVars.NumAxes + 1;
     for i:= 1 to G5SysMax do
       Cells[0,i]:= G5Systems[i];
     for i:= 1 to emcVars.NumAxes do
       Cells[i,0]:= emcVars.CoordNames[i];
     ClientWidth:= ColCount * DefaultColWidth + (ColCount * GridLineWidth);
     ClientHeight:= RowCount * DefaultRowHeight + (RowCount * GridLineWidth);
   end;
  LoadVars;
end;

procedure TOffsetsDlg.LoadParamFile;
begin
  Vars.LoadFromFile(emcVars.ParamFile);
end;

initialization
  {$I offsetdlg.lrs}

end.

