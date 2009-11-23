program preview;

{$mode objfpc}{$H+}
{$link /home/gtom/mocca/src/simmodule.o}
{$link /home/gtom/emc/lib/libemcini.so.0}
{$link /home/gtom/emc/lib/librs274.so.0}
{$linklib stdc++}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Sysutils,
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, preview1, LazOpenGLContext;
  

begin

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

