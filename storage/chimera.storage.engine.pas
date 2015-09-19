unit chimera.storage.engine;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections, chimera.storage,
  chimera.json;

type
  TCreateHandler = reference to function(Configuration : IJSONObject) : IStorage;

  TStorageEngine<T : IStorage> = class(TInterfacedObject, IStorageEngine)
  private
    FConfiguration : IJSONObject;
    FCreateHandler : TCreateHandler;
    FPool : TQueue<IStorage>;
    function GetConfiguration : IJSONObject;
    procedure SetConfiguration(const Value : IJSONObject);
  public
    constructor Create(StorageCreator : TCreateHandler); virtual;
    destructor Destroy; override;
    function Acquire: IStorage;
    procedure Release(const Storage: IStorage);
  end;

implementation

{ TStorageEngine<T> }

function TStorageEngine<T>.Acquire: IStorage;
begin
  TMonitor.Enter(FPool);
  try
    if FPool.Count = 0 then
    begin
      Result := FCreateHandler(FConfiguration);
    end else
      Result := FPool.Extract;
  finally
    TMonitor.Exit(FPool);
  end;
end;


constructor TStorageEngine<T>.Create(StorageCreator : TCreateHandler);
begin
  inherited Create;
  FCreateHandler := StorageCreator;
  FPool := TQueue<IStorage>.Create;
end;

destructor TStorageEngine<T>.Destroy;
begin
  FPool.Free;
  inherited;
end;

function TStorageEngine<T>.GetConfiguration: IJSONObject;
begin
  TMonitor.Enter(FPool);
  try
    result := FConfiguration;
  finally
    TMonitor.Exit(FPool);
  end;
end;

procedure TStorageEngine<T>.Release(const Storage: IStorage);
begin
  TMonitor.Enter(FPool);
  try
    FPool.Enqueue(Storage);
  finally
    TMonitor.Exit(FPool);
  end;
end;

procedure TStorageEngine<T>.SetConfiguration(const Value: IJSONObject);
begin
  TMonitor.Enter(FPool);
  try
    FConfiguration := Value;
    FPool.Clear;
  finally
    TMonitor.Exit(FPool);
  end;
end;

end.
