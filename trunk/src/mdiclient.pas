unit mdiclient;

{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMDIClientForm }

  TMDIClientForm = class(TForm)
    Label1: TLabel;
    MDIEdit: TEdit;
    MDIHistListBox: TListBox;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MDIEditKeyPress(Sender: TObject; var Key: char);
    procedure MDIHistListBoxClick(Sender: TObject);
  private
    function FormatMdi(sMdi: string): string;
    procedure ExecuteMdi;
    procedure UserClick(Sender: TObject);
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
  mocglb,mocemc,
  emc2pas,
  mocbtn;

const
  ERR_MDIHISTOPEN = 'Error loading MDI- history file: ';

procedure TMDIClientForm.FormCreate(Sender: TObject);
begin
  if Sender = nil then ;
  ReadStyle(Self,'mdi.xml');
  Self.Tag:= TASKMODEMDI;
  try
    MDIHistListBox.Items.LoadFromFile(Vars.IniPath + MDI_HIST_FILENAME);
  except
    writeln(ERR_MDIHISTOPEN + Vars.IniPath + MDI_HIST_FILENAME);
  end;
end;

procedure TMDIClientForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Sender = nil then ;
end;

procedure TMDIClientForm.FormActivate(Sender: TObject);
begin
  writeln('Activate');
end;

function TMDIClientForm.HandleCommand(Cmd: integer): Boolean;
begin
  Result:= True;
  case Cmd of
    cmMDIEXEC: ExecuteMDI;
    cmMDIHIST: MDIHistListBox.Clear;
  else
    Result:= False;
  end;
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
  SetButtonMap(@BtnDefMDI,@Self.UserClick);
end;

procedure TMDIClientForm.InitControls;
var
  i: integer;
begin
  i:= MDIHistListBox.Count - 1;
  if i >= 0 then
   if MDIHistListBox.Visible then
     begin
        MDIHistListBox.ItemIndex:= i;
        MDIHistListBox.MakeCurrentVisible;
     end;
  MDIEdit.SetFocus;
end;

procedure TMDIClientForm.UserClick(Sender: TObject);
begin
  if Assigned(Sender) then
    if Sender is TMocButton then
      with Sender as TMocButton do
        begin
          if not Self.HandleCommand(Tag) then
            Emc.HandleCommand(Tag);
        end;
end;

procedure TMDIClientForm.FormDestroy(Sender: TObject);
begin
  if Sender = nil then ;
  try
    MDIHistListBox.Items.SaveToFile(Vars.IniPath + MDI_HIST_FILENAME);
  except
    on E:Exception do
      writeln(E.Message);
  end;
end;

procedure TMDIClientForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Sender = nil then ;
  if Key = 13 then
    begin
      HandleCommand(cmMDIExec);
      Key:= 0;
    end;
end;

procedure TMDIClientForm.MDIEditKeyPress(Sender: TObject; var Key: char);
begin
  if Sender = nil then ;
  if (Key in ['a'..'c','f'..'k','m','p','r'..'z']) then
    Key:= UpCase(Key)
  else
    if (Key = ',') then Key:= '.';
end;

procedure TMDIClientForm.MDIHistListBoxClick(Sender: TObject);
begin
  if Sender = nil then ;
  with MdiHistListbox do
    begin
      if Items.Count < 1 then Exit;
      MdiEdit.Text:=items[itemindex];
    end;
end;

function TMDIClientForm.FormatMdi(sMdi: string): string;
var
  i: integer;
  S,SF: string;
 begin
  result:= '';
  if sMdi = '' then Exit;
  SF:= '';
  S:= '';
  for i:= 1 to Length(sMdi) do
    if SMdi[i] <> #32 then S:= S + UpCase(sMdi[i]);
  SF:= SF + S[1];
  if Length(S) > 1 then
    for i:= 2 to Length(S) do
      begin
        if S[i] in ['A'..'Z'] then
          SF:= SF + #32;
        SF:= SF + S[i];
      end;
  Result:= SF;
end;

procedure TMDIClientForm.ExecuteMdi;
var
  S,SF: string;
  NewOne: Boolean;
begin
  S:= MdiEdit.Text;
  if Length(S) < 1 then Exit;
  SF:= FormatMDI(S);
  // hier MDI senden:
  Emc.Execute(S);  // send mdi command
  Sleep(10);
  UpdateStatus;
  UpdateError;
  if ErrorStr[0] <> #0 then Exit;
  NewOne:= True;
  if MdiHistListbox.Count > 0 then
    NewOne:= MdiHistListBox.Items.IndexOf(SF) < 0;
  if NewOne then
    begin
      MdiHistListbox.Items.Add(SF);
      MDIHistListBox.ItemIndex:= MDIHistListBox.Count - 1;
      MDIHistListBox.MakeCurrentVisible;
    end;
  MdiEdit.Text:= '';
end;


initialization
  {$I mdiclient.lrs}

end.

