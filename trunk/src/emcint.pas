unit emcint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  dynlibs;

//{$link libemcini.so.0}
//{$link librs274.so.0}

const
  libemcini = 'libemcini.so';
  libemc274 = 'librs274.so';
  libemchal = 'libemchal.so';

var
  Emc2Home: string;
  Emc2LibPath: string;

function InitEmcEnvironment: boolean;
procedure DoneEmcEnvironment;

{$link libemcini.so.0}
{$link librs274.so.0}

implementation

var
  EmcIniHandle: Pointer;
  Emc274Handle: Pointer;
  EmcHalHandle: Pointer;

function InitEmcEnvironment: boolean;
begin
  Result:= False;
  Emc2Home:= '';
  Emc2Home:= GetEnvironmentVariable('EMC2_HOME');
  if Length(Emc2Home) < 1 then
    begin
      writeln('Did not find the EMC2_HOME var');
      Exit;
    end;
  Emc2LibPath:= Emc2Home + '/lib/';
  EmcIniHandle:= Pointer(LoadLibrary(Emc2LibPath + libemcini));
  if EmcIniHandle = nil then
    begin
      writeln('error loading libemcini.so: ',Emc2LibPath);
      Exit;
    end;
  Emc274Handle:= Pointer(LoadLibrary(Emc2LibPath + libemcini));
  if Emc274Handle = nil then
    begin
      writeln('error loading librs272.so: ',Emc2LibPath);
      Exit;
    end;
  EmcHalHandle:= Pointer(LoadLibrary(Emc2LibPath + libemchal));
  if EmcHalHandle = nil then
    begin
      writeln('error loading libemchal.so: ',Emc2LibPath);
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

