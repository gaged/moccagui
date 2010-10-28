unit tssupport;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, mocbtn,
  X, XLib, KeySym;

type

  { TKeyboardForm }

  TKeyboardForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
  end;

var
  KeyboardForm: TKeyboardForm;

implementation

{ TKeyboardForm }

{$define HasX}

uses
  gdk2x, gtk2;

{$I tssupport.inc}

function SendKeyEvent(Key: word; Down: Boolean): Boolean;
var
  Ev: TXKeyEvent;
  XDisplay: PDisplay;
  XScreen: PScreen;
  XRootWindow,
  XWindow: TWindow;
  Widget: PGtkWidget;
begin
  XDisplay := gdk_display;
  if XDisplay = nil then
    begin
      writeln('display = nil');
      Exit;
    end;
  XScreen := XDefaultScreenOfDisplay(xdisplay);
  XRootWindow := XRootWindowOfScreen(xscreen);
  Widget:= PGtkWidget(Application.MainForm.Handle);
  if Widget^.window = nil then exit;
  XWindow:= gdk_window_xwindow(Widget^.window);
  if Down then
    Ev._type:= keypress
  else
    Ev._type:= keyrelease;
  Ev.serial:= 0;
  Ev.send_event:= TBool(True);
  Ev.display:= XDisplay;
  Ev.window:=  XWindow;
  Ev.root:= XRootWindow;
  Ev.subwindow:= NONE;
  Ev.time:= CurrentTime;
  Ev.x:= 1; Ev.y:= 1;
  Ev.x_root:= 1; Ev.y_root:= 1;
  Ev.state:= 0; // modifiers
  Ev.keycode:= XKeysymToKeycode(XDisplay,Key);
  Ev.same_screen:= TBool(True);
  XSendEvent(Ev.display,Ev.window,TRUE,KeyPressMask,@Ev);
  xflush(Ev.display);
end;

procedure DoXKeyPress(Key: Word);
begin
  SendKeyEvent(Key,True);
  SendKeyEvent(Key,False);
end;

procedure TKeyboardForm.FormCreate(Sender: TObject);
var
  x,y,i: integer;
  w,h: integer;
  B: TButton;

  procedure AddButton;
  begin
    if KeyMap[i].C = #0 then Exit;
    B:= TButton.Create(Self);
    B.Parent:= Self;
    B.Caption:= KeyMap[i].C;
    B.Tag:= KeyMap[i].Code;
    B.OnClick:= @Self.ButtonClick;
    B.SetBounds(x,y,w,h);
    x:= x + w;
  end;

begin
  w:= ClientWidth div 14;
  h:= ClientHeight div 4;
  y:= 0;
  x:= 0;
  for i:= 0 to 13 do
    AddButton;
  y:= y + h;
  x:= 0;
  for i:= 14 to 28 do
    AddButton;
  y:= y + h;
  x:= 0;
  for i:= 29 to 41 do
    AddButton;
end;

procedure TKeyboardForm.FormDestroy(Sender: TObject);
begin
end;

procedure TKeyboardForm.ButtonClick(Sender: TObject);
var
  KeySym: TKeySym;
begin
  if Sender is TButton then
    with Sender as TButton do
      begin
        if Tag < 1 then Exit;
        DoXKeypress(Tag);
      end;
end;

initialization
  {$I tssupport.lrs}

end.



