unit editorclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs; 

type
  TEditorClientForm = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  EditorClientForm: TEditorClientForm;

implementation

initialization
  {$I editorclient.lrs}

end.

