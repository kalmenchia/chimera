program chimera.storage.test;

uses
  System.StartUpCopy,
  FMX.Forms,
  chimera.storage.test.forms.main in 'chimera.storage.test.forms.main.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
