unit chimera.storage.memory;

interface
  const MEMORY_STORAGE = 'MEMORY_STORAGE';

implementation

uses System.SysUtils, System.Classes, System.Generics.Collections, chimera.storage,
  chimera.storage.engine, chimera.json;

type
  TMemoryStorage = class(TInterfacedObject, IStorage)
  strict private
    FStorage : TDictionary<String, IJSONObject>;
  private
    function GetValue(const Key : string) : IJSONObject;
    procedure SetValue(const Key : string; const Value : IJSONObject);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Values[const Key : string] : IJSONObject read GetValue write SetValue;
    procedure Lookup(const Key : string; const ResultHandler : TResultHandler; const ErrorHandler : TErrorHandler = nil);
    procedure Store(const Key : string; const Value : IJSONObject; const FinishedHandler : TResultHandler = nil; const ErrorHandler : TErrorHandler = nil);
  end;

  TMemoryStorageEngine = TStorageEngine<TMemoryStorage>;

{ TMemoryStorage }

constructor TMemoryStorage.Create;
begin
  inherited Create;
  FStorage := TDictionary<string, IJSONObject>.Create;
end;

destructor TMemoryStorage.Destroy;
begin
  FStorage.Free;
  inherited;
end;

function TMemoryStorage.GetValue(const Key: string): IJSONObject;
begin
  TMonitor.Enter(FStorage);
  try
    Result := FStorage[Key];
  finally
    TMonitor.Exit(FStorage);
  end;
end;

procedure TMemoryStorage.Lookup(const Key: string;
  const ResultHandler: TResultHandler; const ErrorHandler: TErrorHandler);
begin
  try
    ResultHandler(GetValue(Key));
  except
    on e: Exception do
      if Assigned(ErrorHandler) then
        ErrorHandler(e, e.Message);
  end;
end;

procedure TMemoryStorage.SetValue(const Key: string; const Value: IJSONObject);
begin
  TMonitor.Enter(FStorage);
  try
    FStorage.AddOrSetValue(Key,Value);
  finally
    TMonitor.Exit(FStorage);
  end;
end;

procedure TMemoryStorage.Store(const Key: string; const Value: IJSONObject;
  const FinishedHandler: TResultHandler; const ErrorHandler: TErrorHandler);
begin
  try
    SetValue(Key, Value);
    if Assigned(FinishedHandler) then
      FinishedHandler(Value);
  except
    on e: Exception do
      if Assigned(ErrorHandler) then
        ErrorHandler(e, e.Message);
  end;
end;

initialization
  TStorageEngines.Engines[MEMORY_STORAGE] := TMemoryStorageEngine.Create(
    function(Configuration : IJSONObject) : IStorage
    begin
      Result := TMemoryStorage.Create;
    end
  );

end.
