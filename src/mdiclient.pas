unit mdiclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMDIClientForm }

  TMDIClientForm = class(TForm)
    EdMDI: TEdit;
    LabelCaption: TLabel;
    LB: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
  end;
  
var
  clMdi: TMDIClientForm;

implementation

uses
  mocglb,mocjoints,
  emc2pas;

procedure TMDIClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEMDI;
end;

procedure TMDIClientForm.ActivateSelf;
begin
  if emcState.TaskMode <> TASKMODEMDI then Exit;
  if not Visible then
    Visible:= true;
  MapButtons;
  {$ifdef DEBUG_EMC}
  writeln('mdiclient activateself');
  {$endif}
end;

procedure TMDIClientForm.UpdateSelf;
begin
end;

procedure TMDIClientForm.MapButtons;
var
  i,c,Id,iTag: Integer;
  Tags: Array[0..NumSoftButtons - 1] of integer;
  Glyphs: Array[0..NumSoftButtons - 1] of integer;
begin
  Tags:= cmdMDITags;
  Glyphs:= cmdMDIGlyphs;

  for i:= 0 to NumSoftButtons - 1 do
    begin
      Id:= Glyphs[i];
      iTag:= Tags[i];
      if (Id < 0) or (iTag < 0) then
        begin
          SoftBtns[i].Glyph:= nil;
          SoftBtns[i].Enabled:= False;
          SoftBtns[i].Tag:= -1;
          end
        else
          begin
            GlobalImageList.GetBitmap(Id,SoftBtns[i].Glyph);
            SoftBtns[i].Tag:= iTag;
            SoftBtns[i].Enabled:= True;
          end;
    end;
end;

procedure TMDIClientForm.InitControls;
begin
end;

initialization
  {$I mdiclient.lrs}

end.

