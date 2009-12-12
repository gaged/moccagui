unit touchoff; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs; 

type
  TForm1 = class(TForm)
  private
    { private declarations }
  public
    procedure Init;
  end;

var
  Form1: TForm1; 

implementation

initialization
  {$I unit1.lrs}

prcoedure TForm1.Init;
begin
  if emcState.tlo_is_along_w:
    tool_offset_axes:= 'w'
  else
  if emcState.Lathe then
    tool_offset_axes:= 'xz'
  else
    tool_offset_axes = 'z'

  if (emcState.tool_in_spindle = 0) or vars.current_axis.get() not in tool_offset_axes:
            del systems[-1]
            if defaultsystem.startswith("T"): defaultsystem = systems[0]
  linear_axis = vars.current_axis.get() in "xyzuvw"
  if linear_axis:
    if vars.metric.get(): unit_str = " " + _("mm")
    else: unit_str = " " + _("in")
            if lathe and vars.current_axis.get() == "x":
                if 80 in s.gcodes:
                    unit_str += _(" radius")
                else:
                    unit_str += _(" diameter")
        else: unit_str = _(u"\xb0")
        _prompt_float.__init__(self, title, text, default, unit_str)
        t = self.t
        f = Frame(t)
        self.c = c = StringVar(t)
        c.set(defaultsystem)
        l = Label(f, text=_("Coordinate System:"))
        mb = OptionMenu(f, c, *systems)
        mb.tk.call("size_menubutton_to_entries", mb)
        mb.configure(takefocus=1)
        l.pack(side="left")
        mb.pack(side="left")
        f.pack(side="top")
        self.buttons.tkraise()
        for i in [1,2,3,4,5,6,7,8,9]:
            t.bind("<Alt-KeyPress-%s>" % i, lambda event, system=systems[i-1]: c.set(system))
        if not (current_tool is None or vars.current_axis.get() not in tool_offset_axes):
            t.bind("<Alt-t>", lambda event: c.set(systems[9]))
            t.bind("<Alt-0>", lambda event: c.set(systems[9]))

    def result(self):
        if self.u.get(): return self.v.get(), self.c.get()
        return None, None

def prompt_touchoff(title, text, default, system=None):
    t = _prompt_touchoff(title, text, default, system)
    return t.run()


end.

