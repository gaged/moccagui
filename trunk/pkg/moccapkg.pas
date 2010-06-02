{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit moccapkg; 

interface

uses
  mocslider, mocbtn, mocled, moclister, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('mocslider', @mocslider.Register); 
  RegisterUnit('mocbtn', @mocbtn.Register); 
  RegisterUnit('mocled', @mocled.Register);
  RegisterUnit('moclister', @moclister.Register); 
end; 

initialization
  RegisterPackage('moccapkg', @Register); 
end.
