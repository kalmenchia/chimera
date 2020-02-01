unit chimera.handlebars;

interface

uses System.SysUtils, System.Classes, Chimera.json, System.Generics.Collections;

type
  EHandlebarException = class(Exception) end;

  THelperHandler = reference to function(const Name : string; Params : TArray<string>) : string;
  TBlockHelperHandler = reference to function(const Name : string; Params : TArray<string>; const Block : string) : string;

  THandlebars = class
  strict private
    class var FBlockHelpers : TDictionary<string, TBlockHelperHandler>;
    class var FHelpers : TDictionary<string, THelperHandler>;
  strict private
    FContext : IJSONObject;
    FContextStack : TStack<IJSONObject>;
    FBlockStack : TStack<TPair<string, Cardinal>>;
    FOutput : TStringBuilder;
    FSource : String;
    FIndex : Cardinal;
    FLength : Cardinal;
    procedure ReadHandlebar;
    procedure ReadText;
    procedure ReadNext;
    function CallHelper(const Name : string; const Params : TArray<string>) : string;
    function ProcessValue(const Tag : string) : string;
  private
    procedure SetSource(const Value: string);
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create(const ASource : string; const AContext : IJSONObject); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;

    function Output : string;

    procedure Compile; overload;

    class function Compile(const Source : string; const Context : IJSONObject) : string; overload;
    class procedure RegisterHelper(const Name : string; Handler : THelperHandler); overload;
    class procedure RegisterHelper(const Name : string; Handler : TBlockHelperHandler); overload;

    property Source : string read FSource write SetSource;
    property Context : IJSONObject read FContext write FContext;
  end;

implementation

uses
  System.NetEncoding;

{ THandlebars }

class function THandlebars.Compile(const Source: string; const Context: IJSONObject): string;
var
  hb : THandlebars;
begin
  hb := THandlebars.Create(Source, Context);
  try
    hb.Compile;
    Result := hb.Output;
  finally
    hb.Free;
  end;
end;


constructor THandlebars.Create;
begin
  inherited Create;
  FOutput := TStringBuilder.Create;
  FContextStack := TStack<IJSONObject>.Create;
  FBlockStack := TStack<TPair<string, Cardinal>>.Create;
end;

constructor THandlebars.Create(const ASource: string; const AContext: IJSONObject);
begin
  Create;

  Source := ASource;
  FContext := AContext;
end;

destructor THandlebars.Destroy;
begin
  FBlockStack.Free;
  FContextStack.Free;
  FOutput.Free;
  inherited;
end;

class constructor THandlebars.Create;
begin
  FHelpers := TDictionary<string, THelperHandler>.Create;
  FBlockHelpers := TDictionary<string, TBlockHelperHandler>.Create;
end;

class destructor THandlebars.Destroy;
begin
  FHelpers.Free;
  FBlockHelpers.Free;
end;

function THandlebars.CallHelper(const Name : string; const Params: TArray<string>) : string;
var
  i : integer;
begin
  Result := '';
  if FHelpers.ContainsKey(Name) then
  begin
    for i := 0 to length(Params)-1 do
      Params[i] := ProcessValue(Params[i]);

    Result := FHelpers[Name](Name, Params);
  end;
end;

procedure THandlebars.Compile;
begin
  if FContext = nil then
    raise EHandlebarException.Create('No Context Provided');
  FOutput.Clear;
  ReadNext;
end;

function THandlebars.Output: string;
begin
  Result := FOutput.ToString;
end;

function THandlebars.ProcessValue(const Tag: string) : string;
var
  ary : TArray<string>;
  jso : IJSONObject;
  sProp, sObj : string;
  i, iPushCnt: Integer;
begin
  Result := '';
  ary := Tag.Split([' ']);
  sProp := ary[0];
  if length(ary) = 1 then
  begin
    jso := FContext;
    iPushCnt := 0;
    while sProp.Contains('.') do
    begin
      sObj := sProp.Substring(0,sProp.IndexOf('.'));
      if jso.Has[sObj] and (jso.Types[sObj] = TJSONValueType.&object) then
      begin
        inc(iPushCnt);
        FContextStack.Push(jso.Objects[sObj]);
        jso := jso.Objects[sObj];
        Delete(sProp,1, length(sObj)+1);
      end else
        exit;
    end;

    if jso.Has[sProp] then
    begin
      case jso.Types[sProp] of
        TJSONValueType.&string:
          Result := jso.Strings[sProp];
        TJSONValueType.number:
          Result := jso.Numbers[sProp].ToString;
        TJSONValueType.&array:
          Result := jso.Arrays[sProp].AsJSON;
        TJSONValueType.&object:
          Result := jso.Objects[sProp].AsJSON;
        TJSONValueType.boolean:
          Result := BoolToStr(jso.Booleans[sProp], True);
        TJSONValueType.null:;
        TJSONValueType.code:
          Result := jso.AsJSON;
      end;

      for i := 1 to iPushCnt do
        FContextStack.Pop;
    end;
  end else
  begin
    Delete(ary,0,1);
    Result := CallHelper(sProp, ary);
  end;
end;

procedure THandlebars.ReadHandlebar;
var
  sTag : string;
  sData : string;
  block : TPair<string, Cardinal>;
  iLocal : Cardinal;
begin
  iLocal := FIndex;
  repeat
    inc(iLocal);
  until (iLocal >= FLength-1) or ((FSource.Chars[iLocal] = '}') and (FSource.Chars[iLocal+1] = '}'));

  sTag := FSource.Substring(FIndex,iLocal-FIndex+2);
  case sTag.Chars[2] of
    '.': // parent context
      begin

      end;
    '#': // Block
      begin
        if sTag.Length > 5 then
        begin
          sTag := sTag.Substring(3, sTag.Length-5);
        end else
          exit; // invalid tag
                 
        FBlockStack.Push(TPair<string, Cardinal>.Create(sTag, iLocal+sTag.Length+6));
        FIndex := iLocal;
        ReadNext;
      end;
    '/': // end block
      begin
        if sTag.Length > 5 then
        begin
          sTag := sTag.Substring(3, sTag.Length-5);
          block
          if sTag = FBl then

          FIndex := iLocal;
          ReadNext;
        end;
      end;
    '!': // Comment
      begin
        if sTag.Length > 5 then
        begin
          if (sTag.Chars[3] = '-') and (sTag.Chars[4] = '-') then
            if (sTag.Chars[sTag.Length-3] <> '-') and (sTag.Chars[sTag.Length-4] <> '-') then
            begin
              repeat
                inc(iLocal);
              until (iLocal >= FLength-1) or
                    (  (FSource.Chars[iLocal] = '}') and
                       (FSource.Chars[iLocal+1] = '}') and
                       (FSource.Chars[iLocal-1] = '-') and
                       (FSource.Chars[iLocal-2] = '-')
                    );
              // ignore tag
            end;
          // ignore tag
        end;
      end;
    '>': // Partials
      begin

      end;
    '{': // Raw
      begin
        sTag := sTag.Substring(3,sTag.Length-6).Trim;
        FOutput.Append(ProcessValue(sTag));
        FIndex := iLocal+3;
      end;
    else
      begin
        sTag := sTag.Substring(2,sTag.Length-4).Trim;
        FOutput.Append(TNetEncoding.HTML.Encode(ProcessValue(sTag)));
        FIndex := iLocal+2;
      end;
  end;
  ReadNext;
end;

procedure THandlebars.ReadNext;
begin
  if FIndex >= FLength then
    exit;
  if FLength - FIndex > 5 then
  begin
    if (FSource.Chars[FIndex] = '{') and
       (FSource.Chars[FIndex+1] = '{') then
      ReadHandlebar
    else
      ReadText;
  end else
  begin
    FOutput.Append(FSource.Substring(FIndex))
  end;
end;

procedure THandlebars.ReadText;
var
  iLocal : Cardinal;
begin
  iLocal := FIndex;
  repeat
    inc(iLocal);
  until (iLocal >= FLength-1) or ((FSource.Chars[iLocal] = '{') and (FSource.Chars[iLocal+1] = '{'));

  if iLocal >= FLength-1 then
    inc(iLocal);

  FOutput.Append(FSource, FIndex, iLocal-FIndex);
  FIndex := iLocal;

  ReadNext;
end;

class procedure THandlebars.RegisterHelper(const Name: string;
  Handler: TBlockHelperHandler);
begin
  FBlockHelpers.AddOrSetValue(Name, Handler);
end;

class procedure THandlebars.RegisterHelper(const Name: string;
  Handler: THelperHandler);
begin
  FHelpers.AddOrSetValue(Name, Handler);
end;

procedure THandlebars.SetSource(const Value: string);
begin
  FSource := Value;
  FLength := FSource.Length;
  FIndex := 0;
end;

end.
