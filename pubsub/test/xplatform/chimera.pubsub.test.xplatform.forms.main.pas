unit chimera.pubsub.test.xplatform.forms.main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.WebBrowser,
  FMX.StdCtrls, chimera.json, chimera.bayeux.client, FMX.TabControl;

type
  TForm2 = class(TForm)
    txtUsername: TEdit;
    txtMessage: TEdit;
    web: TWebBrowser;
    btnSend: TButton;
    txtHost: TEdit;
    btnConnect: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure txtMessageKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FPubSub : TBayeuxClient;
    FMessages : TStringList;
    function NewMessage(const from : string; const msg : string) : IJSONObject;
    procedure AddErrorMessage(const error : string);
    procedure AddChatMessage(const from : string; const msg: string; timestamp : TDateTime);
  public
  end;

var
  Form2: TForm2;

implementation

uses System.DateUtils;

{$R *.fmx}

procedure TForm2.AddChatMessage(const from : string; const msg: string; timestamp : TDateTime);
var
  sl : TStringList;
  sPage : string;
begin
  TThread.Queue(TThread.Current,
    procedure
      function TimeString(timestamp : TDateTime) : string;
      begin
        DateTimeToString(Result, 'mmmm dd, yyyy hh:nn',TTimezone.Local.ToLocalTime(timestamp));
      end;
    begin
      FMessages.Insert(0,'<li class="media"><div class="media-body"><div class="media"><div class="media-body" >'+
        msg+'<br /><small class="text-muted">'+from+' | '+TimeString(timestamp)+'</small>'+
        '<hr /></div></div></div></li>');

      sl := TStringList.Create;
      try
        sl.LoadFromFile('..\..\chat.template.html');
        sPage := sl.Text.Replace('{%CHAT_MESSAGES%}',FMessages.Text);
      finally
        sl.Free;
      end;
      Web.LoadFromStrings(sPage, '/');
    end
  );
end;

procedure TForm2.AddErrorMessage(const error: string);
begin
  AddChatMessage('ERROR',error, Now);
end;

procedure TForm2.btnConnectClick(Sender: TObject);
begin
  txtUsername.Enabled := False;
  txtHost.Enabled := False;
  txtMessage.Enabled := True;
  btnConnect.Enabled := False;
  FPubsub := TBayeuxClient.Create(txtHost.Text);
  FPubsub.OnUnsuccessful :=
    procedure(const msg : IJSONObject)
    begin
      AddErrorMessage(msg.Strings['error']);
    end;
  FPubsub.Subscribe('/chatdemo',
    procedure(const msg : IJSONObject)
    begin
      AddChatMessage(msg.Strings['from'], msg.Strings['msg'], Now);
    end
  );
  //FPubsub.Publish('/chatdemo',NewMessage('system', 'User '+txtUsername.Text+' has connected.'));
end;

procedure TForm2.btnSendClick(Sender: TObject);
begin
  FPubsub.Publish('/chatdemo',NewMessage(txtUsername.Text, txtMessage.Text));
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FMessages := TStringList.Create;
  txtUsername.Text := 'User-'+Random(MaxLongInt).ToString;
  Web.Navigate('about:blank') ;
  while Web.IsUpdating do
    Application.ProcessMessages;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FMessages.Free;
  FPubSub.Free;
end;

function TForm2.NewMessage(const from, msg: string): IJSONObject;
var
  sDate : string;
begin
  Result := JSON;
  Result.Strings['from'] := from;
  Result.Strings['msg'] := msg;
  DateTimeToString(sDate, 'yyyy-mm-dd hh:nn', TTimezone.local.ToUniversalTime(Now));
  Result.Strings['timestamp'] := sDate;
end;

procedure TForm2.txtMessageKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  btnSend.Enabled := txtMessage.Text <> '';
end;

initialization
  Randomize;

end.
