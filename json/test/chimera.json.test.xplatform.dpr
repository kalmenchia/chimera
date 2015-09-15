program chimera.json.test.xplatform;

uses
  System.StartUpCopy,
  FMX.Forms,
  chimera.json.test.forms.main in 'chimera.json.test.forms.main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
