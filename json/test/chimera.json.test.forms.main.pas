unit chimera.json.test.forms.main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    txtMsg: TMemo;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure WriteLn(const msg : string);
  end;

var
  Form1: TForm1;

implementation

uses
  System.DateUtils,
  chimera.json;


{$R *.fmx}

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
