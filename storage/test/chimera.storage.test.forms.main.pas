unit chimera.storage.test.forms.main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox,
  System.Generics.Collections, chimera.storage, chimera.json, chimera.storage.memory,
  chimera.storage.local;

type
  TForm3 = class(TForm)
    txtKey: TEdit;
    lblKey: TLabel;
    txtValue: TMemo;
    btnStore: TButton;
    btnLookup: TButton;
    lblValue: TLabel;
    cbEngines: TComboBox;
    cbAsync: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnStoreClick(Sender: TObject);
    procedure btnLookupClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FEngines : TDictionary<String, IStorageEngine>;
    function CurrentEngine : IStorageEngine;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses System.IOUtils;

procedure TForm3.btnLookupClick(Sender: TObject);
var
  eng : IStorageEngine;
  storage : IStorage;
begin
  txtValue.Text := '';
  eng := CurrentEngine;
  storage := eng.Acquire;
  try
    if cbAsync.IsChecked then
    begin
      storage.Lookup(txtKey.Text,
        procedure(const value : IJSONObject)
        begin
          txtValue.Text := value.asJSON;
        end,
        procedure(E: Exception; const Msg : String)
        begin
          ShowMessage('Exception: '+E.ClassName+#13#10+Msg);
        end
      );
    end else
      txtValue.Text := storage.Values[txtKey.Text].AsJSON;
  finally
    eng.Release(storage);
  end;
end;

procedure TForm3.btnStoreClick(Sender: TObject);
var
  eng : IStorageEngine;
  storage : IStorage;
begin
  eng := CurrentEngine;
  storage := eng.Acquire;
  try
    if cbAsync.IsChecked then
    begin
      storage.Store(txtKey.Text, JSON(txtValue.Text),
        procedure(const value : IJSONObject)
        begin
          txtValue.Text := '';
        end,
        procedure(E: Exception; const Msg : String)
        begin
          ShowMessage('Exception: '+E.ClassName+#13#10+Msg);
        end
      );
    end else
    begin
      try
        storage.Values[txtKey.Text] := JSON(txtValue.Text);
      except
        on e: exception do
          ShowMessage('Exception: '+E.ClassName+#13#10+E.Message);
      end;
      txtValue.Text := '';
    end;
  finally
    eng.Release(storage);
  end;
end;

function TForm3.CurrentEngine: IStorageEngine;
begin
  Result := FEngines[cbEngines.Items[cbEngines.ItemIndex]];
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  i: Integer;
  eng : IStorageEngine;
  config : IJSONObject;
begin
  FEngines := TDictionary<String, IStorageEngine>.Create;
  config := JSON;
  config.Strings['path'] := TPath.Combine(ExtractFilePath(ParamStr(0)),'data');

  for i := 0 to cbEngines.Count-1 do
  begin
    eng := TStorageEngines.Engines[cbEngines.Items[i]];
    eng.Configuration := config;
    FEngines.Add(cbEngines.Items[i],eng);
  end;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FEngines.Free;
end;

end.
