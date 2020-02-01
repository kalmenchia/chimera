unit chimera.handlebars.test;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, chimera.json;

type
  TForm4 = class(TForm)
    txtTemplate: TMemo;
    txtJSON: TMemo;
    txtResults: TMemo;
    tmrRefresh: TTimer;
    procedure txtJSONExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtTemplateChangeTracking(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
  private
    FJSON: IJSONObject;
    procedure RefreshResults;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses
  chimera.handlebars, chimera.handlebars.helpers.default;

{$R *.fmx}

procedure TForm4.FormShow(Sender: TObject);
begin
  txtJSONExit(Sender);
end;

procedure TForm4.RefreshResults;
begin
  txtResults.Text := THandlebars.Compile(txtTemplate.Text, FJSON);
end;

procedure TForm4.tmrRefreshTimer(Sender: TObject);
begin
  tmrRefresh.Enabled := False;
  RefreshResults;
end;

procedure TForm4.txtJSONExit(Sender: TObject);
begin
  FJSON := JSON(txtJSON.Text);
  RefreshResults;
end;

procedure TForm4.txtTemplateChangeTracking(Sender: TObject);
begin
  tmrRefresh.Enabled := False;
  tmrRefresh.Enabled := True;
end;

end.
