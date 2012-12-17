unit moclister;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, LCLType, LCLProc, LCLIntf,
  GraphType, Graphics, ActnList, Controls, Forms,
  Themes, Buttons, Menus, LResources;

type
  TMocLister = class(TGraphicControl)
  private
    FItems: TStringList;
    FSelItem: integer;
    FNumItems: integer;
    FActiveColor: TColor;
    FTextStyle: TTextStyle;
    procedure SetSelItem(Value: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;
  published
    property Items: TStringList read FItems write FItems;
    property SelectedItem: integer read FSelItem write SetSelItem;
    property NumItems: integer read FNumItems;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Color default clGray;
    property ActiveColor: TColor read FActiveColor write FActiveColor;
    property Enabled;
    property Visible;
  end;

procedure Register;

implementation

constructor TMocLister.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems:= TStringList.Create;
  FSelItem:= 0;
  FActiveColor:= clGray;
  FTextStyle.Alignment:= taLeftJustify;
  FTextStyle.Layout:= tlCenter;
end;

destructor TMocLister.Destroy;
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);
  inherited;
end;

procedure TMocLister.SetSelItem(Value: integer);
begin
  if (Value < 0) or (Value >= FItems.Count) or (Value = FSelItem) then
    Exit;
  FSelItem:= Value;
  invalidate;
end;

procedure TMocLister.Paint;
var
  R: TRect;
  cm,i,idx,h: integer;
begin
  inherited Paint;
  R:= ClientRect;
  Canvas.Clipping:= True;
  Canvas.ClipRect:= R;
  Canvas.Brush.Color:= Color;
  // Canvas.Frame3D(R,2,bvLowered);
  h:= Canvas.TextHeight('Xy') + 2;
  if h < 1 then Exit;
  FNumItems:= clientheight div h;
  cm:= FNumItems div 2;
  R.Top:= (ClientHeight  - (FNumItems * h)) div 2;
  R.Bottom:= R.Top + h;
  for i:= 0 to FNumItems - 1 do
    begin
      idx:= FSelItem + i - cm;
      if (idx >= FItems.Count) or (idx < 0) then
        begin
          Canvas.Brush.Color:= Color;
          Canvas.FillRect(R);
        end
      else
        begin
           if idx = FSelItem then
            Canvas.Brush.Color:= FActiveColor
          else
            Canvas.Brush.Color:= Color;
          Canvas.FillRect(R);
          Canvas.TextRect(R,R.Left,R.Top,FItems[Idx]);
        end;
      inc(R.Top,h);
      inc(R.Bottom,h);
    end;
end;

procedure Register;
begin
  RegisterComponents('Mocca', [TMocLister]);
end;

initialization

end.

