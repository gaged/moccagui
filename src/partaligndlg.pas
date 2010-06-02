unit partaligndlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls,Math;

type

  { TPartAlgnDlg }

  TPartAlgnDlg = class(TForm)
    BtnReset: TButton;
    BtnAlign: TButton;
    BtnOk: TButton;
    BtnCancel: TButton;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelZ: TLabel;
    LabelSkew: TLabel;
    PB: TPaintBox;
    rgBase: TRadioGroup;
    procedure BtnAlignClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PBPaint(Sender: TObject);
    procedure rgBaseClick(Sender: TObject);
  private
    FRect: TRect;
    FBase: integer;
    FTextHeight: integer;
    procedure DrawAxis(i: integer);
    procedure InitControls;
    procedure CalcSkew;
  public
    CurrentX,CurrentY,CurrentZ: double;
  end;

procedure DoPartAlign(XPos,YPos,ZPos: double);

implementation

uses
  mocglb;

type
  TSideText = record
    S1: string;
    S2: string;
  end;

const
  SidesTxt: Array[0..1] of TSideText =
   ((S1: 'Rechts'; S2: 'Links'),
    (S1: 'Hinten'; S2: 'Vorne'));

const
  BoxR = 6;

procedure DoPartAlign(XPos,YPos,ZPos: double);
var
  Dlg: TPartAlgnDlg;
begin
  Application.CreateForm(TPartAlgnDlg,Dlg);
  if Assigned(Dlg) then
    begin
      Dlg.CurrentX:= XPos;
      Dlg.CurrentY:= YPos;
      Dlg.CurrentZ:= ZPos;
      Dlg.InitControls;
      Dlg.ShowModal;
      Dlg.Free;
    end;
end;

{ TPartAlgnDlg }

procedure TPartAlgnDlg.InitControls;
var
  First: Boolean;
begin
  FBase:= 0;
  rgBase.ItemIndex:= FBase;
  First:= not PartBaseCoord.IsSet;
  BtnOk.Enabled:= First;
  BtnAlign.Enabled:= not First;
  BtnReset.Enabled:= not First;
  rgBase.Enabled:= not First;
  LabelX.Caption:= FloatToStrF(CurrentX, ffFixed, 8, 3);
  LabelY.Caption:= FloatToStrF(CurrentY, ffFixed, 8, 3);
  LabelZ.Caption:= FloatToStrF(CurrentZ, ffFixed, 8, 3);
  CalcSkew;
end;

procedure TPartAlgnDlg.DrawAxis(i: integer);
var
  X1,X2,Y1,Y2: integer;
begin
  with PB.Canvas do
    begin
      if (i = 0) then
       begin
         Y1:= FRect.Bottom; Y2:= Y1;
         X1:= FRect.Left; X2:= FRect.Right;
       end
     else
       begin
         X1:= FRect.Top; X2:= X1;
         Y1:= FRect.Bottom; Y2:= FRect.Top;
       end;
     if (i = FBase) then
       begin
         Brush.Style:= bsSolid;
         if PartBaseCoord.IsSet then
           begin
             Pen.Color:= clGreen;
             Brush.Color:= clGreen;
             EllipseC(X1,Y1,BoxR,BoxR);
             Pen.Color:= clRed;
             Brush.Color:= clRed;
             EllipseC(X2,Y2,BoxR,BoxR);
            end
          else
            begin
              Pen.Color:= clRed;
              Brush.Color:= clRed;
              EllipseC(X1,Y1,BoxR,BoxR);
            end;
       end;
     //TextOut(FRect.Left + 4,FRect.Top,'X');
     // TextOut(FRect.Right + 4,FRect.Bottom,'Y');
    end;
end;

procedure TPartAlgnDlg.PBPaint(Sender: TObject);
var
  i: integer;
  ts: TTextStyle;
begin
  if Sender = nil then ;
  if FTextHeight = 0 then
    begin
      FTextHeight:= PB.Canvas.TextHeight('X');
      ts.Alignment:= taCenter;
      ts.Layout:= tlCenter;
      PB.Canvas.TextStyle:= ts;
    end;
  with PB.Canvas do
    begin
      Brush.Color:= clWhite;
      Brush.Style:= bsClear;
      FillRect(ClientRect);
      Pen.Color:= clBlack;
      Pen.Width:= 1;
      Rectangle(FRect);
      for i:= 0 to 1 do DrawAxis(i);
    end;
end;

procedure TPartAlgnDlg.rgBaseClick(Sender: TObject);
begin
  if rgBase.ItemIndex <> FBase then
    begin
      FBase:= rgBase.ItemIndex;
      if FBase < 0 then FBase:= 0;
      PB.Invalidate;
    end;
end;

procedure TPartAlgnDlg.FormCreate(Sender: TObject);
begin
  FRect:= Rect(30,30,PB.Width-30,PB.Height-30);
end;

procedure TPartAlgnDlg.FormResize(Sender: TObject);
begin
  FRect:= Rect(30,30,PB.Width-30,PB.Height-30);
  PB.Invalidate;
end;

procedure TPartAlgnDlg.BtnResetClick(Sender: TObject);
begin
  PartBaseCoord.IsSet:= False;
  InitControls;
end;

procedure TPartAlgnDlg.BtnOkClick(Sender: TObject);
begin
  PartBaseCoord.x:= CurrentX;
  PartBaseCoord.y:= CurrentY;
  PartBaseCoord.z:= CurrentZ;
  PartBaseCoord.IsSet:= True;
  ModalResult:= mrOk;
end;

procedure TPArtAlgnDlg.CalcSkew;
var
  Skew,dx,dy: double;
begin
  dx:= PartBaseCoord.x - CurrentX;
  dy:= PartBaseCoord.y - CurrentY;
  if (dy = 0) or (dx = 0) then
    Skew:= 0
  else
    Skew:= (dy/dx);
  LabelSkew.Caption:= FloatToStrF(skew, ffFixed,3,7);
end;

procedure TPartAlgnDlg.BtnAlignClick(Sender: TObject);
begin
end;

initialization
  {$I partaligndlg.lrs}

end.

