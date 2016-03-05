unit chimera.json.test.forms.main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    txtMsg: TMemo;
    Layout1: TLayout;
    btnChangeFont: TButton;
    btnJWT: TButton;
    btnJWK: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnChangeFontClick(Sender: TObject);
    procedure btnJWTClick(Sender: TObject);
    procedure btnJWKClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure WriteLn(const msg : string);
  end;

var
  Form1: TForm1;

implementation

uses
  System.DateUtils, chimera.json.jwt, chimera.json.jwk,
  chimera.json, chimera.json.helpers;


{$R *.fmx}

procedure TForm1.btnChangeFontClick(Sender: TObject);
var
  jso : IJSONObject;
begin
  showmessage(txtMsg.TextSettings.Font.Size.ToString);
  jso := JSON();
  jso.Objects['TextSettings'] := JSON();
  jso.Objects['TextSettings'].Objects['Font'] := JSON();
  jso.Objects['TextSettings'].Objects['Font'].Numbers['Size'] := 34;
  jso.SaveToFile(ExtractFilePath(Paramstr(0))+'delta.json');
  jso := JSON();
  jso.LoadFromFile(ExtractFilePath(Paramstr(0))+'delta.json');
  txtMsg.AsJSONObject := jso;
  showmessage(txtMsg.TextSettings.Font.Size.ToString);

  txtMsg.AsJSONObject.Objects['TextSettings'].Objects['Font'].Numbers['Size'] := 22;
  showmessage(txtMsg.TextSettings.Font.Size.ToString);
end;

procedure TForm1.btnJWKClick(Sender: TObject);
var
  jwk : TJWK;
  jwkSet : TJWKSet;
begin
  jwkSet.Initialize;

  jwk.Initialize;
  jwk.kty := 'EC';
  jwk.use := 'enc';
  jwk.kid := '1';
  jwk.x := 'MKBCTNIcKUSDii11ySs3526iDZ8AiTo7Tu6KPAqv7D4';
  jwk.y := '4Etl6SRW2YiLUrN5vfvVHuhp7x8PxltmWWlbbM4IFyM';
  jwk.d := '870MB6gfuTJ4HtUnUvYMyJpr5eUZNP4Bk43bVdj3eAE';
  jwkSet.Add(jwk);

  jwk.Initialize;
  jwk.kty := 'RSA';
  jwk.alg := 'RS256';
  jwk.kid := '2011-04-29';
  jwk.n := '0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2aiAFbWhM78LhWx4'+
     'cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCiFV4n3oknjhMst'+
     'n64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65YGjQR0_FDW2Q'+
     'vzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n91CbOpbIS'+
     'D08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_xBniIqbw'+
     '0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw';
  jwk.e := 'AQAB';
  jwk.d := 'X4cTteJY_gn4FYPsXB8rdXix5vwsg1FLN5E3EaG6RJoVH-HLLKD9'+
     'M7dx5oo7GURknchnrRweUkC7hT5fJLM0WbFAKNLWY2vv7B6NqXSzUvxT0_YSfqij'+
     'wp3RTzlBaCxWp4doFk5N2o8Gy_nHNKroADIkJ46pRUohsXywbReAdYaMwFs9tv8d'+
     '_cPVY3i07a3t8MN6TNwm0dSawm9v47UiCl3Sk5ZiG7xojPLu4sbg1U2jx4IBTNBz'+
     'nbJSzFHK66jT8bgkuqsk0GjskDJk19Z4qwjwbsnn4j2WBii3RL-Us2lGVkY8fkFz'+
     'me1z0HbIkfz0Y6mqnOYtqc0X4jfcKoAC8Q';
  jwk.p := '83i-7IvMGXoMXCskv73TKr8637FiO7Z27zv8oj6pbWUQyLPQBQxtPV'+
     'nwD20R-60eTDmD2ujnMt5PoqMrm8RfmNhVWDtjjMmCMjOpSXicFHj7XOuVIYQyqV'+
     'WlWEh6dN36GVZYk93N8Bc9vY41xy8B9RzzOGVQzXvNEvn7O0nVbfs';
  jwk.q := '3dfOR9cuYq-0S-mkFLzgItgMEfFzB2q3hWehMuG0oCuqnb3vobLyum'+
     'qjVZQO1dIrdwgTnCdpYzBcOfW5r370AFXjiWft_NGEiovonizhKpo9VVS78TzFgx'+
     'kIdrecRezsZ-1kYd_s1qDbxtkDEgfAITAG9LUnADun4vIcb6yelxk';
  jwk.dp := 'G4sPXkc6Ya9y8oJW9_ILj4xuppu0lzi_H7VTkS8xj5SdX3coE0oim'+
     'YwxIi2emTAue0UOa5dpgFGyBJ4c8tQ2VF402XRugKDTP8akYhFo5tAA77Qe_Nmtu'+
     'YZc3C3m3I24G2GvR5sSDxUyAN2zq8Lfn9EUms6rY3Ob8YeiKkTiBj0';
  jwk.dq := 's9lAH9fggBsoFR8Oac2R_E2gw282rT2kGOAhvIllETE1efrA6huUU'+
     'vMfBcMpn8lqeW6vzznYY5SSQF7pMdC_agI3nG8Ibp1BUb0JUiraRNqUfLhcQb_d9'+
     'GF4Dh7e74WbRsobRonujTYN1xCaP6TO61jvWrX-L18txXw494Q_cgk';
  jwk.qi := 'GyM_p6JrXySiz1toFgKbWV-JdI3jQ4ypu9rbMWx3rQJBfmt0FoYzg'+
     'UIZEVFEcOqwemRN81zoDAaa-Bk0KWNGDjJHZDdDmFhW3AN7lI-puxk_mHZGJ11rx'+
     'yR8O55XLSe3SPmRfKwZI6yU24ZxvQKFYItdldUKGzO6Ia6zTKhAVRU';
  jwkSet.Add(jwk);
  txtMsg.Lines.Clear;
  txtMsg.Lines.Add(FormatJSON(jwkSet.AsJSON))
end;

procedure TForm1.btnJWTClick(Sender: TObject);
var
  jwt : TJWT;
  s224, s256, s384, s512 : string;
begin
  txtMsg.Lines.Clear;

  jwt.Initialize;
  jwt.Payload.sub := 'validation';
  jwt.Payload.iss := 'Chimera Test';
  jwt.Payload.exp := EncodeDate(2019,1,1);
  jwt.Payload.iat := EncodeDate(2016,1,1);
  jwt.Payload.aud := 'end users';
  s224 := jwt.SignHS224('secret');
  txtMsg.Lines.Add('HS224: '+s224);
  txtMsg.Lines.Add('Valid? '+BoolToStr(jwt.TryValidate(s224,'secret', JWT), true));
  txtMsg.Lines.Add('');
  s256 := jwt.SignHS256('secret');
  txtMsg.Lines.Add('HS256: '+s256);
  txtMsg.Lines.Add('Valid? '+BoolToStr(jwt.TryValidate(s256,'secret', JWT), true));
  txtMsg.Lines.Add('');
  s384 := jwt.SignHS384('secret');
  txtMsg.Lines.Add('HS384: '+s384);
  txtMsg.Lines.Add('Valid? '+BoolToStr(jwt.TryValidate(s384,'secret', JWT), true));
  txtMsg.Lines.Add('');
  s512 := jwt.SignHS512('secret');
  txtMsg.Lines.Add('HS512: '+s512);
  txtMsg.Lines.Add('Valid? '+BoolToStr(jwt.TryValidate(s512,'secret', JWT), true));
  txtMsg.Lines.Add('');
end;

procedure TForm1.FormShow(Sender: TObject);

var
  sl : TStringList;
  jso : IJSONObject;
  src : string;
  i, j: integer;
  dt : TDateTime;
  aryTimes : array[0..9] of integer;
  sb : TStringBuilder;
begin
  try
    sl := TStringList.Create;
    sb := TStringBuilder.Create;
    try
      sl.Text := txtMsg.AsJSON;
      sl.SaveToFile('..'+PathDelim+'..'+PathDelim+'Memo.json');
      WriteLn(sl.Text);
      sl.Clear;

      if FileExists(ExtractFilePath(ParamStr(0))+PathDelim+'test.json') then
        sl.LoadFromFile(ExtractFilePath(ParamStr(0))+PathDelim+'test.json')
      else
        sl.LoadFromFile('..'+PathDelim+'..'+PathDelim+'test.json');
      src := sl.Text;
      for i := 0 to 9 do
      begin
        sb.Clear;
        jso := nil;
        dt := Now;
        jso := JSON(src);
        for j := 0 to jso.Objects['d'].Arrays['results'].Count-1 do
        begin
          sb.AppendLine(jso.Objects['d'].Arrays['results'].Objects[j].Strings['Name']);
          sb.Append(jso.Objects['d'].Arrays['results'].Objects[j].Strings['Rating']+' ');
          sb.AppendLine(jso.Objects['d'].Arrays['results'].Objects[j].Integers['Runtime'].ToString);
        end;
        WriteLn(sb.ToString);
        //WriteLn(jso.AsJSON);
        aryTimes[i] := MillisecondsBetween(Now,dt);
        WriteLn(IntToStr(aryTimes[i]));
      end;
      WriteLn('Avg: '+FloatToStr(
        (aryTimes[0] +
        aryTimes[1] +
        aryTimes[2] +
        aryTimes[3] +
        aryTimes[4] +
        aryTimes[5] +
        aryTimes[6] +
        aryTimes[7] +
        aryTimes[8] +
        aryTimes[9]) / 10));
      sl.Text := jso.AsJSON;
      sl.SaveToFile('..\..\test.out.json');
      sl.LoadFromFile('..\..\Memo.json');
      jso := JSON(sl.Text);
      jso.Booleans['ReadOnly'] := True;
      txtMsg.AsJSONObject := jso;
      //ReadLn;
    finally
      sl.Free;
      sb.Free;
    end;
  except
    on E: Exception do
      Writeln('ERROR: '+E.ClassName +'-'+ E.Message);
  end;
end;

procedure TForm1.WriteLn(const msg: string);
begin
  txtMsg.Lines.Add(msg);
end;

end.
