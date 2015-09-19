unit chimera.storage.local;

interface
  const LOCAL_STORAGE = 'LOCAL_STORAGE';

implementation

uses System.SysUtils, System.Classes, System.Generics.Collections, chimera.storage,
  chimera.storage.engine, chimera.json, System.IOUtils, IdHashSHA;

type
  TLocalStorage = class(TInterfacedObject, IStorage)
  strict private
    FRootPath: string;
    function ResolvePath(const Key : string) : String;
  private
    function GetValue(const Key : string) : IJSONObject;
    procedure SetValue(const Key : string; const Value : IJSONObject);
  public
    constructor Create(Configuration : IJSONObject);
    property Values[const Key : string] : IJSONObject read GetValue write SetValue;
    procedure Lookup(const Key : string; const ResultHandler : TResultHandler; const ErrorHandler : TErrorHandler = nil);
    procedure Store(const Key : string; const Value : IJSONObject; const FinishedHandler : TResultHandler = nil; const ErrorHandler : TErrorHandler = nil);
  end;

  TLocalStorageEngine = TStorageEngine<TLocalStorage>;

{ TLocalStorage }

constructor TLocalStorage.Create(Configuration: IJSONObject);
begin
  inherited Create;
  if Configuration.Has['path'] then
    FRootPath := Configuration.Strings['path']
  else
    FRootPath := TPath.Combine(TPath.GetDocumentsPath, 'storage');
end;

function TLocalStorage.GetValue(const Key: string): IJSONObject;
var
  sFile : string;
  fs : TFileStream;
  sl : TStringList;
begin
  sFile := ResolvePath(Key);
  if TFile.Exists(sFile) then
  begin
    fs := TFileStream.Create(sFile, fmOpenRead or fmShareDenyWrite);
    try
      sl := TStringList.Create;
      try
        sl.LoadFromStream(fs, TEncoding.UTF8);
        Result := JSON(sl.Text);
      finally
        sl.Free;
      end;
    finally
      fs.Free;
    end;
  end else
    raise EStorageException.Create(Key, 'Not found');
end;

procedure TLocalStorage.Lookup(const Key: string;
  const ResultHandler: TResultHandler; const ErrorHandler: TErrorHandler);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        ResultHandler(GetValue(Key));
      except
        on e: exception do
          if Assigned(ErrorHandler) then
            ErrorHandler(E, E.Message);
      end;
    end
  ).Start;
end;

function TLocalStorage.ResolvePath(const Key: string): String;
var
  SHA1: TIdHashSHA1;
  sHash, sPath : string;
begin
  SHA1 := TIdHashSHA1.Create;
  try
    sHash := SHA1.HashStringAsHex(Key);
    Result := TPath.Combine(TPath.Combine(TPath.Combine(TPath.Combine(FRootPath, sHash.Chars[0]), sHash.Chars[1]), sHash.Chars[2]), sHash.SubString(2) );
    sPath := ExtractFilePath(Result);
    if not TDirectory.Exists(sPath) then
      TDirectory.CreateDirectory(sPath);
  finally
    SHA1.Free;
  end;
end;

procedure TLocalStorage.SetValue(const Key: string; const Value: IJSONObject);
var
  sFile : string;
  fs : TFileStream;
  sl : TStringList;
begin
  sFile := ResolvePath(Key);
  fs := TFileStream.Create(sFile, fmOpenWrite or fmCreate or fmShareDenyWrite);
  try
    fs.Size := 0;
    sl := TStringList.Create;
    try
      sl.Text := Value.AsJSON;
      sl.SaveToStream(fs, TEncoding.UTF8);
    finally
      sl.Free;
    end;
  finally
    fs.Free;
  end;
end;

procedure TLocalStorage.Store(const Key: string; const Value: IJSONObject;
  const FinishedHandler: TResultHandler; const ErrorHandler: TErrorHandler);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        SetValue(Key, Value);
        if Assigned(FinishedHandler) then
          FinishedHandler(Value);
      except
        on e: exception do
          ErrorHandler(E, E.Message);
      end;
    end
  ).Start;
end;

initialization
  TStorageEngines.Engines[LOCAL_STORAGE] := TLocalStorageEngine.Create(
    function(Configuration : IJSONObject) : IStorage
    begin
      Result := TLocalStorage.Create(Configuration);
    end
  );
end.
