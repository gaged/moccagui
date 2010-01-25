unit editorclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs;

type

  { TEditorClient }

  TEditorClient = class(TForm)
    LabelCaption: TLabel;
    Memo: TMemo;
    procedure Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
  public
    procedure ActivateSelf;
    // procedure UpdateSelf;
    // procedure InitControls
    procedure CloseEditor;
    procedure MapButtons;
    procedure UpdateButtons;
    procedure LoadPreCode;
    procedure LoadFile;
    procedure LoadInternalFile(const S: TStrings; AName: string);
    function  HandleCommand(Cmd: integer): Boolean;
  end;

var
  clEditor: TEditorClient;

implementation

uses
  Buttons,mocglb,mocbtn,emc2pas,runclient;

procedure TEditorClient.LoadPreCode;
var
  Sl: TStringList;
  i: integer;
begin
  Sl:= TStringList.Create;
  if not Assigned(Sl) then
    begin
      LastError:= 'Error creating editor';
      Exit;
    end;
  try
    if EditorBeginFile <> '' then
      begin
        Sl.LoadFromFile(EditorBeginFile);
        if Sl.Count > 0 then
          for i:= 0 to Sl.Count - 1 do
            Memo.Lines.Add(Sl[i]);
        Sl.Clear;
      end
    else
      Memo.Lines.Add('G21');

    if EditorEndFile <> '' then
      begin
        Memo.Lines.Add('');
        Sl.LoadFromFile(EditorEndFile);
        if Sl.Count > 0 then
          for i:= 0 to Sl.Count - 1 do
            Memo.Lines.Add(Sl[i]);
        Sl.Clear;
      end
    else
      Memo.Lines.Add('M02');
  finally
    FreeAndNil(Sl);
  end;
end;

procedure TEditorClient.UpdateButtons;
begin

end;

procedure TEditorClient.LoadInternalFile(const S: TStrings; AName: string);
begin
  if Assigned(S) and (AName <> '') then
   begin
     FFileName:= AName;
     Memo.Lines.Assign(S);
   end;
end;

procedure TEditorClient.LoadFile;
var
  FileName: string;
begin
  if Assigned(clRun) then
    if Assigned(clRun.OpenDialog) then
      if clRun.OpenDialog.Execute then
        begin
          FileName:=  clRun.OpenDialog.FileName;
          if FileName = '' then Exit;
          FFileName:= FileName;
          Memo.Lines.LoadFromFile(FFileName);
        end;
end;

procedure TEditorClient.CloseEditor;
begin
  clRun.EditorMode(False);
end;

function TEditorClient.HandleCommand(Cmd: integer): Boolean;
begin
  Result:= False;
  case Cmd of
    cmEDOPEN: LoadFile;
    cmEDCLOSE: CloseEditor;
  end;
end;

procedure TEditorClient.Click(Sender: TObject);
begin
 if Assigned(Sender) then
    with Sender as TMocButton do
      Self.HandleCommand(Tag);
end;

procedure TEditorClient.FormCreate(Sender: TObject);
begin
  if Assigned(clRun) then
    Self.Memo.Font.Assign(clRun.LB.Font);
end;

procedure TEditorClient.MapButtons;
var
  i: integer;
begin
  for i:= 0 to NumMButtons - 1 do
    if Assigned(MocBtns[i]) then
      MocBtns[i].Enabled:= False;
  SetButtonMap(nil,@BtnDefEditor,@Self.Click);
end;

procedure TEditorClient.ActivateSelf;
begin
  MapButtons;
  if FFileName = '' then
    LoadPreCode;
  Visible:= true;
end;

initialization
  {$I editorclient.lrs}

end.

