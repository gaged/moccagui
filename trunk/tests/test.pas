program test;

{$mode objfpc}{$H+}

{$link /home/gtom/mocca/src/simmodule.o}
{$link /home/gtom/emc/lib/libemcini.so.0}
{$link /home/gtom/emc/lib/librs274.so.0} 
{$linklib stdc++}

uses
  classes,glib2,interfaces,
  linecode
  ;

// function ParseFile(const AFileName: PChar): integer; cdecl; external 'liblcode.so';


const
  testfile: PChar = '/home/gtom/emc2-dev/nc_files/arcspiral.ngc';

begin
  // {$ifdef LC_ALL}
  ParseFile(testfile);
  writeln('press key...');
  readln;
end. 
