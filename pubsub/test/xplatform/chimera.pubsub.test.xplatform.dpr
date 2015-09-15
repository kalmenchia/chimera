program chimera.pubsub.test.xplatform;

uses
  System.StartUpCopy,
  FMX.Forms,
  chimera.pubsub.test.xplatform.forms.main in 'chimera.pubsub.test.xplatform.forms.main.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
