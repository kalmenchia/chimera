unit chimera.storage;

interface

uses System.SysUtils, System.Classes, chimera.json, System.Generics.Collections;

type
  EStorageException = class(Exception)
    constructor Create(AKey : string; AMsg : string);
  end;

  TResultHandler = reference to procedure(const Result : IJSONObject);
  TErrorHandler = reference to procedure(E : Exception; const Msg : String);

  IStorage = interface
    function GetValue(const Key : string) : IJSONObject;
    procedure SetValue(const Key : string; const Value : IJSONObject);
    property Values[const Key : string] : IJSONObject read GetValue write SetValue;
    procedure Lookup(const Key : string; const ResultHandler : TResultHandler; const ErrorHandler : TErrorHandler = nil);
    procedure Store(const Key : string; const Value : IJSONObject; const FinishedHandler : TResultHandler = nil; const ErrorHandler : TErrorHandler = nil);
  end;

  IStorageEngine = interface
    function GetConfiguration : IJSONObject;
    procedure SetConfiguration(const Value : IJSONObject);
    function Acquire : IStorage;
    procedure Release(const Storage : IStorage);
    property Configuration : IJSONObject read GetConfiguration write SetConfiguration;
  end;

  TStorageEngines = class
  strict private
    class var FEngines : TDictionary<String, IStorageEngine>;
    class procedure SetEngine(const ID : String; Engine : IStorageEngine); static;
    class function GetEngine(const ID : String) : IStorageEngine; static;
  public
    class constructor Create;
    class destructor Destroy;
    class property Engines[const ID : String] : IStorageEngine read GetEngine write SetEngine;
  end;

implementation

{ TStorageEngines }

class constructor TStorageEngines.Create;
begin
  FEngines := TDictionary<String, IStorageEngine>.Create;
end;

class destructor TStorageEngines.Destroy;
begin
  FEngines.Free;
end;

class function TStorageEngines.GetEngine(const ID: String): IStorageEngine;
begin
  Result := FEngines[ID];
end;

class procedure TStorageEngines.SetEngine(const ID: String;
  Engine: IStorageEngine);
begin
  FEngines.AddOrSetValue(ID, Engine);
end;

{ EStorageException }

constructor EStorageException.Create(AKey, AMsg: string);
begin
  inherited Create(AMsg+' (with Key "'+AKey+'")');
end;

end.
