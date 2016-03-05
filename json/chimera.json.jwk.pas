unit chimera.json.jwk;

interface

uses System.SysUtils, System.Classes, chimera.json;

type
  TJWK = record
  private
    Data : IJSONObject;
    function GetALG: string;
    function GetKeyOps: string;
    function GetKID: string;
    function GetKTY: string;
    function GetUSE: string;
    function GetX5C: string;
    function GetX5T: string;
    function GetX5TS256: string;
    function GetX5U: string;
    function GetN : string;
    function GetX : string;
    function GetY : string;
    function GetD : string;
    function GetE : string;
    function GetP : string;
    function GetQ : string;
    function GetDP : string;
    function GetDQ : string;
    function GetQI : string;
    procedure SetALG(const Value: string);
    procedure SetKeyOps(const Value: string);
    procedure SetKID(const Value: string);
    procedure SetKTY(const Value: string);
    procedure SetUSE(const Value: string);
    procedure SetX5C(const Value: string);
    procedure SetX5T(const Value: string);
    procedure SetX5TS256(const Value: string);
    procedure SetX5U(const Value: string);
    procedure SetN(const Value: string);
    procedure SetX(const Value: string);
    procedure SetY(const Value: string);
    procedure SetD(const Value: string);
    procedure SetE(const Value: string);
    procedure SetP(const Value: string);
    procedure SetQ(const Value: string);
    procedure SetDP(const Value: string);
    procedure SetDQ(const Value: string);
    procedure SetQI(const Value: string);
  public
    property kty : string read GetKTY write SetKTY;
    property use : string read GetUSE write SetUSE;
    property key_ops : string read GetKeyOps write SetKeyOps;
    property alg : string read GetALG write SetALG;
    property kid : string read GetKID write SetKID;
    property x5u : string read GetX5U write SetX5U;
    property x5c : string read GetX5C write SetX5C;
    property x5t : string read GetX5T write SetX5T;
    property x5tS256 : string read GetX5TS256 write SetX5TS256;
    property n : string read GetN write SetN;
    property x : string read GetX write SetX;
    property y : string read GetY write SetY;
    property d : string read GetD write SetD;
    property e : string read GetE write SetE;
    property p : string read GetP write SetP;
    property q : string read GetQ write SetQ;
    property dp : string read GetDP write SetDP;
    property dq : string read GetDQ write SetDQ;
    property qi : string read GetQI write SetQI;
    procedure Add(const Name : string; Value : string); overload;
    procedure Add(const Name: string; const Value : Int64); overload;
    procedure Add(const Name: string; const Value : Double); overload;
    procedure Add(const Name: string; const Value : TDateTime); overload;
    procedure Add(const Name: string; const Value : Boolean); overload;
    function Initialize : TJWK;
    function AsJSON : string;
  end;

  TJWKSet = record
  private
    Data : IJSONObject;
  public
    function Initialize : TJWKSet;
    procedure Add(jwk : TJWK);
    function AsJSON : string;
  end;

implementation

{ TJWK }

procedure TJWK.Add(const Name: string; const Value: TDateTime);
begin
  Data.IntDates[Name] := Value;
end;

procedure TJWK.Add(const Name: string; const Value: Int64);
begin
  Data.Integers[Name] := Value;
end;

procedure TJWK.Add(const Name: string; const Value: Double);
begin
  Data.Numbers[Name] := Value;
end;

procedure TJWK.Add(const Name: string; const Value: Boolean);
begin
  Data.Booleans[Name] := Value;
end;

function TJWK.AsJSON: string;
begin
  Result := Data.AsJSON(TWhitespace.compact);
end;

procedure TJWK.Add(const Name: string; Value: string);
begin
  Data.Strings[Name] := Value;
end;

function TJWK.GetALG: string;
begin
  Result := Data.Strings['alg'];
end;

function TJWK.GetD: string;
begin
  Result := Data.Strings['d'];
end;

function TJWK.GetDP: string;
begin
  Result := Data.Strings['dp'];
end;

function TJWK.GetDQ: string;
begin
  Result := Data.Strings['dq'];
end;

function TJWK.GetE: string;
begin
  Result := Data.Strings['e'];
end;

function TJWK.GetKeyOps: string;
begin
  Result := Data.Strings['key_ops'];
end;

function TJWK.GetKID: string;
begin
  Result := Data.Strings['kid'];
end;

function TJWK.GetKTY: string;
begin
  Result := Data.Strings['kty'];
end;

function TJWK.GetN: string;
begin
  Result := Data.Strings['n'];
end;

function TJWK.GetP: string;
begin
  Result := Data.Strings['p'];
end;

function TJWK.GetQ: string;
begin
  Result := Data.Strings['q'];
end;

function TJWK.GetQI: string;
begin
  Result := Data.Strings['qi'];
end;

function TJWK.GetUSE: string;
begin
  Result := Data.Strings['use'];
end;

function TJWK.GetX: string;
begin
  Result := Data.Strings['x'];
end;

function TJWK.GetX5C: string;
begin
  Result := Data.Strings['x5c'];
end;

function TJWK.GetX5T: string;
begin
  Result := Data.Strings['x5t'];
end;

function TJWK.GetX5TS256: string;
begin
  Result := Data.Strings['x5t#S256'];
end;

function TJWK.GetX5U: string;
begin
  Result := Data.Strings['x5u'];
end;

function TJWK.GetY: string;
begin
  Result := Data.Strings['y'];
end;

function TJWK.Initialize : TJWK;
begin
  Data := JSON;
  Result := Self;
end;

procedure TJWK.SetALG(const Value: string);
begin
  Data.Strings['alg'] := Value;
end;

procedure TJWK.SetD(const Value: string);
begin
  Data.Strings['d'] := Value;
end;

procedure TJWK.SetDP(const Value: string);
begin
  Data.Strings['dp'] := Value;
end;

procedure TJWK.SetDQ(const Value: string);
begin
  Data.Strings['dq'] := Value;
end;

procedure TJWK.SetE(const Value: string);
begin
  Data.Strings['e'] := Value;
end;

procedure TJWK.SetKeyOps(const Value: string);
begin
  Data.Strings['key_ops'] := Value;
end;

procedure TJWK.SetKID(const Value: string);
begin
  Data.Strings['kid'] := Value;
end;

procedure TJWK.SetKTY(const Value: string);
begin
  Data.Strings['kty'] := Value;
end;

procedure TJWK.SetN(const Value: string);
begin
  Data.Strings['n'] := Value;
end;

procedure TJWK.SetP(const Value: string);
begin
  Data.Strings['p'] := Value;
end;

procedure TJWK.SetQ(const Value: string);
begin
  Data.Strings['q'] := Value;
end;

procedure TJWK.SetQI(const Value: string);
begin
  Data.Strings['qi'] := Value;
end;

procedure TJWK.SetUSE(const Value: string);
begin
  Data.Strings['use'] := Value;
end;

procedure TJWK.SetX(const Value: string);
begin
  Data.Strings['x'] := Value;
end;

procedure TJWK.SetX5C(const Value: string);
begin
  Data.Strings['x5c'] := Value;
end;

procedure TJWK.SetX5T(const Value: string);
begin
  Data.Strings['x5t'] := Value;
end;

procedure TJWK.SetX5TS256(const Value: string);
begin
  Data.Strings['x5t#S256'] := Value;
end;

procedure TJWK.SetX5U(const Value: string);
begin
  Data.Strings['x5u'] := Value;
end;

procedure TJWK.SetY(const Value: string);
begin
  Data.Strings['y'] := Value;
end;

{ TJWKSet }

procedure TJWKSet.Add(jwk: TJWK);
begin
  Data.Arrays['keys'].Add(jwk.Data);
end;

function TJWKSet.AsJSON: string;
begin
  Result := Data.AsJSON(TWhitespace.compact);
end;

function TJWKSet.Initialize : TJWKSet;
begin
  Self.Data := JSON;
  Self.Data.Arrays['keys'] := JSONArray;
  result := Self;
end;

end.
