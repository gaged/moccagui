unit mdiclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMDIClientForm }

  TMDIClientForm = class(TForm)
    MDIEdit: TEdit;
    LabelCaption: TLabel;
    MDIHistListBox: TListBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MDIHistListBoxClick(Sender: TObject);
  private
    function StripMdiInput(inp: string): string;
    function FormatMdi(inp: string): string;
    procedure ExecuteMdi;
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    function  HandleCommand(Cmd: integer): Boolean;
  end;
  
var
  clMdi: TMDIClientForm;

const
  MDI_HIST_FILENAME = 'mocmdi.txt';

implementation

uses
  buttons,mocglb,mocemc,mocjoints,
  emc2pas;

procedure TMDIClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEMDI;
  try
    MDIHistListBox.Items.LoadFromFile(Vars.IniPath + MDI_HIST_FILENAME);
  except
    writeln('could not open: ' + Vars.IniPath + MDI_HIST_FILENAME);
  end;
end;

procedure TMDIClientForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
end;

function TMDIClientForm.HandleCommand(Cmd: integer): Boolean;
begin
  if Cmd = cmMDIExec then
    begin
      ExecuteMDI;
      Result:= True;
    end
  else
    Result:= False;
end;

procedure TMDIClientForm.ActivateSelf;
begin
  if State.TaskMode <> TASKMODEMDI then Exit;
  if not Visible then
    Visible:= true;
  MapButtons;
  initControls;
end;

procedure TMDIClientForm.UpdateSelf;
begin
end;

procedure TMDIClientForm.MapButtons;
begin
  SetButtonMap(@BtnDefMDI,@Self.Click);
end;

procedure TMDIClientForm.InitControls;
begin
  SetButtonDown(cmMDI,True);
  MDIEdit.SetFocus;
end;

procedure TMDIClientForm.Click(Sender: TObject);
begin
  if Assigned(Sender) then
    with Sender as TSpeedButton do
      begin
        Down:= False;
        if not Self.HandleCommand(Tag) then
          Emc.HandleCommand(Tag);
      end;
end;

procedure TMDIClientForm.FormDestroy(Sender: TObject);
begin
  MDIHistListBox.Items.SaveToFile(Vars.IniPath + MDI_HIST_FILENAME);
end;

procedure TMDIClientForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    begin
      HandleCommand(cmMDIExec);
      Key:= 0;
    end;
end;

procedure TMDIClientForm.MDIHistListBoxClick(Sender: TObject);
begin
  with MdiHistListbox do MdiEdit.Text:=items[itemindex];
end;

function TMDIClientForm.StripMdiInput(inp: string): string;
var
  i: integer;
  str1,str2 : string;
  c  : char;
begin
  result:= '';
  if inp = '' then
    Exit;

  // Leerzeichen strippen
  str1:='';
  for i:=1 to length(inp) do
    if inp[i]<>#32 then str1:=str1+upcase(inp[i]);

  // alphabetisch sortieren
  str2:='';
  for c:='A' to 'Z' do begin
    // Buchstaben suchen gehen
    i:=1;
    while (str1[i]<>c) and (i<=length(str1)) do inc(i);
    // einen gefunden? Dann bis zum nächsten Wortende oder zum Satzende suchen
    if i<length(str1) then begin
      repeat
        str2:=str2+str1[i];
        inc(i);
      until (str1[i] in ['A'..'Z']) or (i>length(str1))
    end;
  end;
  result:=str2;
end;

function TMDIClientForm.FormatMdi(inp: string): string;
var i: integer;
    str1, str2: string;
begin
  result:='';
  if inp='' then exit;

  // Leerzeichen strippen
  str1:='';
  for i:=1 to length(inp) do
    if inp[i]<>#32 then str1:=str1+upcase(inp[i]);

  // vor jedem Einzelbuchstaben ein Leerzeichen einfügen, ausser dem Ersten
  str2:=str1[1];
  for i:=2 to length(str1)-1 do begin
    if (not(str1[i-1] in ['A'..'Z'])) and
       (str1[i] in ['A'..'Z']) and
       (not(str1[i+1] in ['A'..'Z'])) then str2:=str2+#32;
    str2:=str2+str1[i];
  end;
  result:=str2+str1[i+1];
end;

procedure TMDIClientForm.ExecuteMdi;
var
  i: integer;
  stripped_mdi: string;
  mdi_in_list: boolean;
begin
  stripped_mdi:=StripMdiInput(MDIEdit.Text);
  // hier MDI senden:
  Emc.Execute(MDIEdit.Text);  // send mdi command
  Sleep(10);
  UpdateStatus;
  UpdateError;
  if ErrorStr[0] <> #0 then Exit;
  if MdiHistListbox.Count>0 then
    begin
      mdi_in_list:=false;
      i:=0;
      while (not mdi_in_list) and (i<MdiHistListbox.Count) do
        begin
          mdi_in_list:=stripped_mdi=StripMdiInput(MdiHistListbox.Items[i]);
          inc(i);
        end;
      if not mdi_in_list then MdiHistListbox.Items.Add(FormatMdi(MDIEdit.text));
    end
  else
    MdiHistListbox.Items.Add(FormatMdi(MDIEdit.text));
  MdiEdit.Text:= ''; // clear Edittext finally...
end;


initialization
  {$I mdiclient.lrs}

end.

