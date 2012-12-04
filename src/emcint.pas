unit emcint;

{$mode objfpc}{$H+}
{$I mocca.inc}

interface

uses
  Classes, SysUtils,
  dynlibs;

{$IFDEF LINUX_CNC}
const
  libemcini = 'libemcini.so'; // still emcini?
  libemc274 = 'librs274.so';
  libemchal = 'liblinuxcnchal.so';
{$ELSE}
const
  libemcini = 'libemcini.so';
  libemc274 = 'librs274.so';
  libemchal = 'libemchal.so';
{$ENDIF}

var
  Emc2Home: string;
  Emc2LibPath: string;
  Emc2Version: string;

{$ifndef VER_23}
  Emc2NmlFile: string;
{$endif}

function InitEmcEnvironment: boolean;
procedure DoneEmcEnvironment;

{$IFDEF LINUX_CNC}
{$linklib libemcini.so}
{$ELSE}
{$linklib libemcini.so}
{$ENDIF}

{$linklib librs274.so}


implementation

var
  EmcIniHandle: Pointer;
  Emc274Handle: Pointer;
  EmcHalHandle: Pointer;

function InitEmcEnvironment: boolean;
begin
  Result:= False;

  Emc2Home:= '';
  Emc2Home:= GetEnvironmentVariable({$IFDEF LINUX_CNC}'LINUXCNC_HOME'{$ELSE}'EMC2_HOME'{$ENDIF});
  if Length(Emc2Home) < 1 then
    begin
      writeln('Did not find the EMC2_HOME var');
      Exit;
    end;
  {$ifndef VER_23}
  Emc2NmlFile:= '';
  Emc2NmlFile:=  GetEnvironmentVariable('NMLFILE');
  if Length(Emc2NmlFile) < 1 then
    begin
      writeln('Did not find the NMLFILE var');
      Exit;
    end;
  {$endif}

  Emc2Version:= '';
  Emc2Version:= GetEnvironmentVariable({$IFNDEF LINUX_CNC}'EMC2VERSION'{$ELSE}'LINUXCNCVERSION'{$ENDIF});
  
  {$ifndef IGNORE_VERSION}
  {$ifdef VER_23}
  if Pos('2.3.',Emc2Version) < 1 
  {$endif}
  {$ifdef VER_24}
  {$info compiliert version 2.4ff}
  if Pos('2.4.',Emc2Version) < 1
  {$endif}
  {$ifdef VER_25}
  {$info compiliert version 2.5ff}
  if Pos('2.5.',Emc2Version) < 1
  {$endif}
  {$ifdef VER_26}
  {$info compiliert version 2.6ff}
  if Pos('2.6.',Emc2Version) < 1
  {$endif}
  then
    begin
      writeln('Wrong EMC2-Version Number, got Version ' + Emc2Version + ',' +#13);
      writeln('This Version of Mocca was build for EMC2-' +
        {$ifdef VER_23}'2.3'{$endif}
        {$ifdef VER_24}'2.4'{$endif}
        {$ifdef VER_25}'2.5'{$endif}
        {$ifdef VER_26}'2.6'{$endif});
      writeln('Please install the correct version of mocca.');
      Exit;
    end
  else
    writeln('mocca starts with correct LinuxCNC version: emc2-' + Emc2Version);
  {$else}
    writeln('mocca starts with LinuxCNC version: emc2-' + Emc2Version);
  {$endif}
  	
  Emc2LibPath:= Emc2Home + '/lib/';
  writeln('Library loaded from: ' + Emc2LibPath);
  EmcIniHandle:= Pointer(LoadLibrary(Emc2LibPath + libemcini));
  if EmcIniHandle = nil then
    begin
      writeln('Error loading libemcini.so from ',Emc2LibPath);
      Exit;
    end;
  Emc274Handle:= Pointer(LoadLibrary(Emc2LibPath + libemcini));
  if Emc274Handle = nil then
    begin
      writeln('Error loading librs274.so from ',Emc2LibPath);
      Exit;
    end;
  EmcHalHandle:= Pointer(LoadLibrary(Emc2LibPath + libemchal));
  if EmcHalHandle = nil then
    begin
      writeln('Error loading libemchal.so from ',Emc2LibPath);
      Exit;
    end;
  Result:= True;
end;

procedure DoneEmcEnvironment;
begin
  if EmcHalHandle <> nil then FreeLibrary(Cardinal(EmcHalHandle));
  if EmcIniHandle <> nil then FreeLibrary(Cardinal(EmcIniHandle));
  if Emc274Handle <> nil then FreeLibrary(Cardinal(Emc274Handle));
end;

initialization
  EmcHalHandle:= nil;
  EmcIniHandle:= nil;
  Emc274Handle:= nil;
end.

