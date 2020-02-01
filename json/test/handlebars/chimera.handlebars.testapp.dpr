program chimera.handlebars.testapp;

uses
  System.StartUpCopy,
  FMX.Forms,
  chimera.handlebars.test in 'chimera.handlebars.test.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
