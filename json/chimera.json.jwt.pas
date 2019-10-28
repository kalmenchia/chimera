unit chimera.json.jwt;

interface

uses System.SysUtils, System.Classes, chimera.json;

type
  EJWTException = class(Exception)
  end;

  TJWTHeader = record
    Alg : string;
    Typ : string;
    function Default : TJWTHeader;
    function AsJSON : string;
    function AsBase64 : string;
  end;

  TJWTPayload = record
  private
    Data : IJSONObject;
    function GetISS: string;
    procedure SetISS(const Value: string);
    function GetAUD: string;
    function GetEXP: TDateTime;
    function GetNBF: TDateTime;
    function GetIAT: TDateTime;
    function GetSUB: string;
    function GetPRN: string;
    function GetJTI: string;
    procedure SetAUD(const Value: string);
    procedure SetEXP(const Value: TDateTime);
    procedure SetNBF(const Value: TDateTime);
    procedure SetIAT(const Value: TDateTime);
    procedure SetPRN(const Value: string);
    procedure SetJTI(const Value: string);
    procedure SetSUB(const Value: string);

  public
    property iss : string read GetISS write SetISS;
    property exp : TDateTime read GetEXP write SetEXP;
    property nbf : TDateTime read GetNBF write SetNBF;
    property iat : TDateTime read GetIAT write SetIAT;
    property prn : string read GetPRN write SetPRN;
    property jti : string read GetJTI write SetJTI;
    property sub : string read GetSUB write SetSUB;
    property aud : string read GetAUD write SetAUD;
    procedure Add(const Claim : string; const Value : string); overload;
    procedure Add(const Claim : string; const Value : Int64); overload;
    procedure Add(const Claim : string; const Value : Double); overload;
    procedure Add(const Claim : string; const Value : TDateTime); overload;
    procedure Add(const Claim : string; const Value : Boolean); overload;
    function AsJSON : string;
    function AsBase64 : string;
  end;

  TJWT = record
    Header : TJWTHeader;
    Payload : TJWTPayload;
    function Initialize(const JWT : string = '') : TJWT;
    function Validate(const JWT : string; const Secret : string; AllowNone : boolean = false) : TJWT;
    function ValidateHS224(const JWT : string; const Secret : string) : TJWT;
    function ValidateHS256(const JWT : string; const Secret : string) : TJWT;
    function ValidateHS384(const JWT : string; const Secret : string) : TJWT;
    function ValidateHS512(const JWT : string; const Secret : string) : TJWT;
    function TryValidate(const JWT : string; const Secret : string; var jwtResult : TJWT; AllowNone : boolean = false) : boolean;
    function TryValidateHS224(const JWT : string; const Secret : string; var jwtResult : TJWT) : boolean;
    function TryValidateHS256(const JWT : string; const Secret : string; var jwtResult : TJWT) : boolean;
    function TryValidateHS384(const JWT : string; const Secret : string; var jwtResult : TJWT) : boolean;
    function TryValidateHS512(const JWT : string; const Secret : string; var jwtResult : TJWT) : boolean;
    function AsString: String;
    function SignHS224(const Secret : string) : string;
    function SignHS256(const Secret : string) : string;
    function SignHS384(const Secret : string) : string;
    function SignHS512(const Secret : string) : string;
  end;

resourcestring
  S_INVALID_JWT_FORMAT = 'JWT String is not in a valid format.';
  S_UNKNOWN_ALGORITHM = 'Requested algorithm is not supported.';
  S_VALIDATION_FAILED = 'Signature validation failed.';

implementation

uses System.NetEncoding, System.Hash;

function Base64Decode(Data : string) : string; inline;
var
  i: Integer;
begin
  for i := 0 to Data.Length-1 do
    case Data.chars[i] of
      '_': Data[i+1] := '/';
      '+': Data[i+1] := '-';
    end;
  Result := TNetEncoding.Base64.Decode(Data);
end;

function Base64EncodeAsUTF8(const Data : string) : string; inline;
var
  enc : TBase64Encoding;
  i: Integer;
begin
  enc := TBase64Encoding.Create(High(Integer));
  try
    result := enc.EncodeBytesToString(TEncoding.UTF8.GetBytes(Data));
  finally
    enc.Free;
  end;
  for i := 0 to Result.length-1 do
    case result.chars[i] of
      '/': result[i+1] := '_';
      '+': result[i+1] := '-';
    end;
  result := result.Split(['='])[0];
end;

function Base64EncodeAsBytes(const Data : TArray<Byte>) : string;
var
  enc : TBase64Encoding;
  i: Integer;
begin
  enc := TBase64Encoding.Create(High(Integer));
  try
    result := enc.EncodeBytesToString(Data);
  finally
    enc.Free;
  end;
  for i := 0 to Result.length-1 do
    case result.chars[i] of
      '/': result[i+1] := '_';
      '+': result[i+1] := '-';
    end;
  result := result.Split(['='])[0];
end;

function ValidateHeader(const jso : IJSONObject) : IJSONObject; inline;
begin
  if (not jso.Has['typ']) or (not jso.Has['alg']) then
    raise EJWTException.Create(S_INVALID_JWT_FORMAT);
  Result := jso;
end;

function SplitJWT(const JWT : string; RequiredSections : integer) : TArray<string>; inline;
begin
  Result := JWT.Split(['.']);
  if Length(Result) <> RequiredSections then
    raise EJWTException.Create(S_INVALID_JWT_FORMAT);
end;

{ TJWT }

function TJWT.AsString: String;
begin
  Result := Self.Header.AsBase64+'.'+Self.Payload.AsBase64;
end;

function TJWT.Initialize(const JWT : string = ''): TJWT;
var
  ary : TArray<String>;
begin
  if JWT = '' then
  begin
    Self.Header.Default;
    Self.Payload.Data := JSON;
  end else
  begin
    ary := SplitJWT(JWT, 2);
    Self.Initialize;
    Self.Header.Alg := 'NONE';
    Self.Payload.Data := JSON(Base64Decode(ary[1]));
  end;
  Result := Self;
end;

function TJWT.SignHS224(const Secret: string): string;
begin
  Self.Header.Alg := 'HS224';

  Result := AsString+'.'+Base64EncodeAsBytes(THashSHA2.GetHMACAsBytes(TEncoding.UTF8.GetBytes(AsString),TEncoding.UTF8.GetBytes(Secret), THashSHA2.TSHA2Version.SHA224))
end;

function TJWT.SignHS256(const Secret: string): string;
begin
  Self.Header.Alg := 'HS256';

  Result := AsString+'.'+Base64EncodeAsBytes(THashSHA2.GetHMACAsBytes(TEncoding.UTF8.GetBytes(AsString),TEncoding.UTF8.GetBytes(Secret), THashSHA2.TSHA2Version.SHA256))
end;

function TJWT.SignHS384(const Secret: string): string;
begin
  Self.Header.Alg := 'HS384';

  Result := AsString+'.'+Base64EncodeAsBytes(THashSHA2.GetHMACAsBytes(TEncoding.UTF8.GetBytes(AsString),TEncoding.UTF8.GetBytes(Secret), THashSHA2.TSHA2Version.SHA384))
end;

function TJWT.SignHS512(const Secret: string): string;
begin
  Self.Header.Alg := 'HS512';

  Result := AsString+'.'+Base64EncodeAsBytes(THashSHA2.GetHMACAsBytes(TEncoding.UTF8.GetBytes(AsString),TEncoding.UTF8.GetBytes(Secret), THashSHA2.TSHA2Version.SHA512))
end;

function TJWT.TryValidate(const JWT, Secret: string; var jwtResult: TJWT;
  AllowNone: boolean): boolean;
var
  ary : TArray<String>;
  sAlg : string;
begin
  ary := SplitJWT(JWT, 3);
  sAlg := ValidateHeader(JSON(Base64Decode(ary[0]))).Strings['alg'];

  if sAlg = 'HS256' then
    Result := TryValidateHS256(JWT, Secret, jwtResult)
  else if sAlg = 'HS224' then
    Result := TryValidateHS224(JWT, Secret, jwtResult)
  else if sAlg = 'HS384' then
    Result := TryValidateHS384(JWT, Secret, jwtResult)
  else if sAlg = 'HS512' then
    Result := TryValidateHS512(JWT, Secret, jwtResult)
  else if (sAlg = 'NONE') and AllowNone then
  begin
    jwtResult := Initialize(JWT);
    Result := True;
  end else
    raise Exception.Create(S_UNKNOWN_ALGORITHM);

end;

function TJWT.TryValidateHS224(const JWT, Secret: string;
  var jwtResult: TJWT): boolean;
var
  ary : TArray<String>;
begin
  ary := SplitJWT(JWT, 3);
  Result := ary[2] = Base64EncodeAsBytes(THashSHA2.GetHMACAsBytes(TEncoding.UTF8.GetBytes(ary[0]+'.'+ary[1]),TEncoding.UTF8.GetBytes(Secret), THashSHA2.TSHA2Version.SHA224));
  if Result then
    jwtResult := jwtResult.Initialize(ary[0]+'.'+ary[1]);
end;

function TJWT.TryValidateHS256(const JWT, Secret: string;
  var jwtResult: TJWT): boolean;
var
  ary : TArray<String>;
begin
  ary := SplitJWT(JWT, 3);
  Result := ary[2] = Base64EncodeAsBytes(THashSHA2.GetHMACAsBytes(TEncoding.UTF8.GetBytes(ary[0]+'.'+ary[1]),TEncoding.UTF8.GetBytes(Secret), THashSHA2.TSHA2Version.SHA256));
  if Result then
    jwtResult := jwtResult.Initialize(ary[0]+'.'+ary[1]);
end;

function TJWT.TryValidateHS384(const JWT, Secret: string;
  var jwtResult: TJWT): boolean;
var
  ary : TArray<String>;
begin
  ary := SplitJWT(JWT, 3);
  Result := ary[2] = Base64EncodeAsBytes(THashSHA2.GetHMACAsBytes(TEncoding.UTF8.GetBytes(ary[0]+'.'+ary[1]),TEncoding.UTF8.GetBytes(Secret), THashSHA2.TSHA2Version.SHA384));
  if Result then
    jwtResult := jwtResult.Initialize(ary[0]+'.'+ary[1]);
end;

function TJWT.TryValidateHS512(const JWT, Secret: string;
  var jwtResult: TJWT): boolean;
var
  ary : TArray<String>;
begin
  ary := SplitJWT(JWT, 3);
  Result := ary[2] = Base64EncodeAsBytes(THashSHA2.GetHMACAsBytes(TEncoding.UTF8.GetBytes(ary[0]+'.'+ary[1]),TEncoding.UTF8.GetBytes(Secret), THashSHA2.TSHA2Version.SHA512));
  if Result then
    jwtResult := jwtResult.Initialize(ary[0]+'.'+ary[1]);
end;

function TJWT.Validate(const JWT, Secret: string; AllowNone : boolean = false): TJWT;
begin
  if not TryValidate(JWT, Secret, Result) then
    raise EJWTException.Create(S_VALIDATION_FAILED);
end;

function TJWT.ValidateHS224(const JWT, Secret: string): TJWT;
begin
  if not TryValidateHS224(JWT, Secret, Result) then
    raise EJWTException.Create(S_VALIDATION_FAILED);
end;

function TJWT.ValidateHS256(const JWT, Secret: string): TJWT;
begin
  if not TryValidateHS256(JWT, Secret, Result) then
    raise EJWTException.Create(S_VALIDATION_FAILED);
end;

function TJWT.ValidateHS384(const JWT, Secret: string): TJWT;
begin
  if not TryValidateHS384(JWT, Secret, Result) then
    raise EJWTException.Create(S_VALIDATION_FAILED);
end;

function TJWT.ValidateHS512(const JWT, Secret: string): TJWT;
begin
  if not TryValidateHS512(JWT, Secret, Result) then
    raise EJWTException.Create(S_VALIDATION_FAILED);
end;

{ TJWTHeader }

function TJWTHeader.AsBase64: string;
begin
  Result := Base64EncodeAsUTF8(AsJSON);
end;

function TJWTHeader.AsJSON: string;
var
  jso : IJSONObject;
begin
  jso := JSON;
  jso.Strings['alg'] := Self.Alg;
  jso.Strings['typ'] := Self.Typ;
  Result := jso.AsJSON(TWhitespace.compact);
end;

function TJWTHeader.Default : TJWTHeader;
begin
  Self.Alg := 'HS256';
  Self.Typ := 'JWT';
end;

{ TJWTPayload }

procedure TJWTPayload.Add(const Claim, Value : string);
begin
  Data.Strings[Claim] := Value;
end;

procedure TJWTPayload.Add(const Claim: string; const Value: Int64);
begin
  Data.Integers[Claim] := Value;
end;

procedure TJWTPayload.Add(const Claim: string; const Value: Double);
begin
  Data.Numbers[Claim] := Value;
end;

procedure TJWTPayload.Add(const Claim: string; const Value: TDateTime);
begin
  Data.Dates[Claim] := Value;
end;

procedure TJWTPayload.Add(const Claim: string; const Value: Boolean);
begin
  Data.Booleans[Claim] := Value;
end;

function TJWTPayload.AsBase64: string;
begin
  Result := Base64EncodeAsUTF8(AsJSON);
end;

function TJWTPayload.AsJSON: string;
begin
  Result := Data.AsJSON(TWhitespace.compact);
end;

function TJWTPayload.GetAUD: string;
begin
  Result := Data.Strings['aud'];
end;

function TJWTPayload.GetEXP: TDateTime;
begin
  Result := Data.IntDates['exp'];
end;

function TJWTPayload.GetIAT: TDateTime;
begin
  Result := Data.IntDates['iat'];
end;

function TJWTPayload.GetISS: string;
begin
  Result := Data.Strings['iss'];
end;

function TJWTPayload.GetJTI: string;
begin
  Result := Data.Strings['jti'];
end;

function TJWTPayload.GetNBF: TDateTime;
begin
  Result := Data.IntDates['nbf'];
end;

function TJWTPayload.GetPRN: string;
begin
  Result := Data.Strings['prn'];
end;

function TJWTPayload.GetSUB: string;
begin
  Result := Data.Strings['sub'];
end;

procedure TJWTPayload.SetAUD(const Value: string);
begin
  Data.Strings['aud'] := Value;
end;

procedure TJWTPayload.SetEXP(const Value: TDateTime);
begin
  Data.IntDates['exp'] := Value;
end;

procedure TJWTPayload.SetIAT(const Value: TDateTime);
begin
  Data.IntDates['iat'] := Value;
end;

procedure TJWTPayload.SetISS(const Value: string);
begin
  Data.Strings['iss'] := Value;
end;

procedure TJWTPayload.SetJTI(const Value: string);
begin
  Data.Strings['jti'] := Value;
end;

procedure TJWTPayload.SetNBF(const Value: TDateTime);
begin
  Data.IntDates['nbf'] := Value;
end;

procedure TJWTPayload.SetPRN(const Value: string);
begin
  Data.Strings['prn'] := Value;
end;

procedure TJWTPayload.SetSUB(const Value: string);
begin
  Data.Strings['sub'] := Value;
end;

end.
