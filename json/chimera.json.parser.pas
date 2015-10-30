// *****************************************************************************
//
// chimera.json.parser;
//
// JSON Chimera project for Delphi
//
// Copyright (c) 2012 by Sivv LLC, All Rights Reserved
//
// Information about this product can be found at
// http://arcana.sivv.com/chimera
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
// *****************************************************************************

unit chimera.json.parser;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections,
  System.Types, System.Rtti, chimera.json;

type
{$SCOPEDENUMS ON}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

  EParseException = class(Exception)

  end;

  TParser = class(TObject)
  type
    TParseToken = (&String, Colon, OpenObject, CloseObject, OpenArray, CloseArray, Comma, EOF, MaxOp, Value);
  private
    FFmt : TFormatSettings;
    FText : string;
    FTextLength : integer;
    FIndex : integer;
    FToken : TParseToken;
    FTokenValue : TMultiValue;
    FOperatorStack : TStack<TParseToken>;
    FValueStack : TStack<TMultiValue>;
    FTmpValue : TStringBuilder;
    FTmpIdent : TStringBuilder;
    function GetToken : boolean;
    function ParseArray: IJSONArray; overload;
    function ParseObject: IJSONObject;
    procedure ParseObjectTo(const Obj : IJSONObject);
  protected
    function OperatorToStr(Token : TParseToken) : string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ExecuteTo(const AText : string; const Obj : IJSONObject);
    function Execute(const AText : string) : IJSONObject;
    function ExecuteForArray(const AText : string) : IJSONArray;
    class function Parse(const AText : string) : IJSONObject;
    class procedure ParseTo(const AText : string; const Obj : IJSONObject);
    class function ParseArray(const AText : string) : IJSONArray; overload;
  end;

implementation

uses System.Character, System.Variants;

{ TParser }

constructor TParser.Create;
begin
  inherited Create;
  FOperatorStack := TStack<TParseToken>.Create;
  FValueStack := TStack<TMultiValue>.Create;
  FFmt := TFormatSettings.Create('en-us');
  FTmpValue := TStringBuilder.Create;
  FTmpIdent := TStringBuilder.Create;
end;

destructor TParser.Destroy;
begin
  FOperatorStack.Free;
  FValueStack.Free;
  FTmpValue.Free;
  FTmpIdent.Free;
  inherited;
end;

function TParser.OperatorToStr(Token : TParseToken) : string;
begin
  case Token of
    TParser.TParseToken.String:
      Result := 'String';
    TParser.TParseToken.Colon:
      Result := 'Colon';
    TParser.TParseToken.OpenObject:
      Result := 'OpenObject';
    TParser.TParseToken.CloseObject:
      Result := 'CloseObject';
    TParser.TParseToken.OpenArray:
      Result := 'OpenArray';
    TParser.TParseToken.CloseArray:
      Result := 'CloseArray';
    TParser.TParseToken.Comma:
      Result := 'Comma';
    TParser.TParseToken.EOF:
      Result := 'EOF';
    TParser.TParseToken.MaxOp:
      Result := 'MaxOp';
    TParser.TParseToken.Value:
      Result := 'Value';
  end;
end;

function TParser.GetToken: boolean;
var
  d : Double;
  b : boolean;
  i : integer;
  iStart : integer;
begin
  FTmpIdent.Clear;
  while FIndex <= FTextLength do
  begin
    inc(FIndex);
    if CharInSet(FText.Chars[FIndex], [ '{', '}', '[', ']', ',', '"', ':']) then
    begin
      // Is an Operator
      FTmpIdent.Append(FText.Chars[FIndex]);
      break;
    end else if (FText.Chars[FIndex] <= Char($20)) then
      continue
    else if (FText.Chars[FIndex].IsLetterOrDigit) or (FText.Chars[FIndex]='-') then
    begin
      // Is an identifier or value
      iStart := FIndex;
      while (FIndex < FTextLength) do
      begin
        if ( not CharInSet(FText.Chars[FIndex], ['0'..'9', 'A'..'Z','a'..'z','.', '-'])) then //.isLetterOrDigit(FText[FIndex])) and (FText[FIndex] <> FFmt.DecimalSeparator)) then
          break;
        if (FIndex > iStart) then
        begin
          if ( not CharInSet(FText.Chars[FIndex-1], ['0'..'9', 'A'..'Z','a'..'z','.', '-'])) then //.isLetterOrDigit(FText[FIndex])) and (FText[FIndex] <> FFmt.DecimalSeparator)) then
        //if (FIndex > iStart) and (( not TCharacter.isLetterOrDigit(FText[FIndex-1])) and (FText[FIndex-1] <> FFmt.DecimalSeparator)) then
            break;
          FTmpIdent.Append(FText.Chars[FIndex-1]);
          FTmpIdent.Append(FText.Chars[FIndex]);
        end else
        begin
          FTmpIdent.Append(FText.Chars[FIndex]);
        end;
        inc(FIndex,2); // marginally faster to skip by twos, moreso on big tokens
      end;
      if (FIndex > iStart) and ( not CharInSet(FText.Chars[FIndex-1], ['0'..'9', 'A'..'Z','a'..'z','.', '-'])) then //.isLetterOrDigit(FText[FIndex])) and (FText[FIndex] <> FFmt.DecimalSeparator)) then
      //if (FIndex > iStart) and ( not TCharacter.isLetterOrDigit(FText[FIndex-1])) and (FText[FIndex-1] <> FFmt.DecimalSeparator) then
        dec(FIndex)
      else
      begin
        FTmpIdent.Append(FText.Chars[FIndex-1]);
      end;
      dec(FIndex);
      break;
    end;
    // Ignore Everything Else
  end;

  if FTmpIdent.Length > 0 then
  begin
    case FTmpIdent.Chars[0] of
      '{': FToken := TParseToken.OpenObject;
      '}': FToken := TParseToken.CloseObject;
      '[': FToken := TParseToken.OpenArray;
      ']': FToken := TParseToken.CloseArray;
      ',': FToken := TParseToken.Comma;
      ':': FToken := TParseToken.Colon;
      '"':
      begin
        FToken := TParseToken.String;
        FTmpValue.Clear;
        for i := FIndex+1 to FTextLength do
        begin
          if (FText.Chars[i] = '"') then
          begin
            if (FText.Chars[i-1] <> '\') then
              break;
            if (FText.Chars[i-2] = '\') then
              break;
            FTmpValue.Append('"');
          end else
          begin
            FTmpValue.Append(FText.Chars[i]);
          end;
        end;
        FIndex := i;
        FTokenValue.Initialize(FTmpValue.ToString);
      end;
      else
      begin
        FToken := TParseToken.Value;
        if TryStrToFloat(FTmpIdent.ToString,d, FFmt) then
          FTokenValue.Initialize(d)
        else if TryStrToBool(FTmpIdent.ToString,b) then
          FTokenValue.Initialize(b)
        else if FTmpIdent.ToString = 'null' then
          FTokenValue.InitializeNull
        else
          raise EParseException.Create('Unexpected Value');
      end;
    end;
  end else
  begin
    FToken := TParseToken.EOF;
  end;

  Result := False;
end;

function TParser.ParseArray : IJSONArray;
begin
  if FToken <> TParseToken.OpenArray  then
    raise Exception.Create('Array Expected');
  Result := JSONArray;
  GetToken;
  while FToken <> TParseToken.CloseArray do
  begin
    case FToken of
      TParser.TParseToken.String:
        Result.Add(@FTokenValue);
        //Result.Add(FTokenValue.StringValue);
      TParser.TParseToken.OpenObject:
        Result.Add(ParseObject);
      TParser.TParseToken.OpenArray:
        Result.Add(ParseArray);
      TParser.TParseToken.Value:
        case FTokenValue.ValueType of
          TJSONValueType.string:
            Result.Add(FTokenValue.StringValue);
          TJSONValueType.number:
            Result.Add(FTokenValue.NumberValue);
          TJSONValueType.array:
            Result.Add(FTokenValue.ArrayValue);
          TJSONValueType.object:
            Result.Add(FTokenValue.ObjectValue);
          TJSONValueType.boolean:
            Result.Add(FTokenValue.IntegerValue <> 0);
          TJSONValueType.null:
            Result.AddNull;
        end;
      TParser.TParseToken.CloseObject,
      TParser.TParseToken.CloseArray,
      TParser.TParseToken.Comma,
      TParser.TParseToken.EOF,
      TParser.TParseToken.MaxOp,
      TParser.TParseToken.Colon:
        if FToken <> TParseToken.Colon then
          raise Exception.Create('Value Expected');
    end;
    GetToken;
    if not (FToken in [TParseToken.Comma, TParseToken.CloseArray]) then
    begin
      raise Exception.Create('Comma or Close Array Expected');
    end;
    if FToken = TParseToken.Comma then
      GetToken;
  end;
end;

function TParser.ParseObject : IJSONObject;
begin
  Result := JSON;
  ParseObjectTo(Result);
end;

procedure TParser.ParseObjectTo(const Obj: IJSONObject);
var
  sName : String;
  p : Pointer;
begin
  if FToken <> TParseToken.OpenObject  then
    raise Exception.Create('Object Expected');
  GetToken;
  while FToken <> TParseToken.CloseObject do
  begin
    if FToken <> TParseToken.String then
      raise Exception.Create('String Expected');
    sName := FTokenValue.StringValue;
    GetToken;
    if FToken <> TParseToken.Colon then
      raise Exception.Create('Colon Expected');
    GetToken;
    case FToken of
      TParser.TParseToken.String:
        Obj.Raw[sName] := @FTokenValue;
      TParser.TParseToken.OpenObject:
        begin
          p := @obj;
          Obj.Objects[sName] := ParseObject;
          Obj.Objects[sName].OnChange :=
            procedure(const jso : IJSONObject)
            begin
              IJSONObject(p).DoChangeNotify;
            end;
        end;
      TParser.TParseToken.OpenArray:
        begin
          p := @obj;
          Obj.Arrays[sName] := ParseArray;
          Obj.Arrays[sName].OnChange :=
            procedure(const jsa : IJSONArray)
            begin
              IJSONObject(p).DoChangeNotify;
            end;
        end;
      TParser.TParseToken.Value:
        case FTokenValue.ValueType of
          TJSONValueType.string:
            Obj.Strings[sName] := FTokenValue.StringValue;
          TJSONValueType.number:
            Obj.Numbers[sName] := FTokenValue.NumberValue;
          TJSONValueType.array:
            Obj.Arrays[sName] := FTokenValue.ArrayValue;
          TJSONValueType.object:
            Obj.Objects[sName] := FTokenValue.ObjectValue;
          TJSONValueType.boolean:
            Obj.Booleans[sName] := FTokenValue.IntegerValue <> 0;
          TJSONValueType.null:
            Obj.AddNull(sName);
        end;
      TParser.TParseToken.CloseObject,
      TParser.TParseToken.CloseArray,
      TParser.TParseToken.Comma,
      TParser.TParseToken.EOF,
      TParser.TParseToken.MaxOp,
      TParser.TParseToken.Colon:
        if FToken <> TParseToken.Colon then
          raise Exception.Create('Value Expected');
    end;
    GetToken;
    if not (FToken in [TParseToken.Comma, TParseToken.CloseObject]) then
    begin
      raise Exception.Create('Comma or Close Object Expected');
    end;
    if FToken = TParseToken.Comma then
      GetToken;
  end;
end;

class procedure TParser.ParseTo(const AText: string;
  const Obj: IJSONObject);
var
  p : TParser;
begin
  p := TParser.Create;
  try
    p.ExecuteTo(AText, Obj);
  finally
    p.Free;
  end;

end;

function TParser.Execute(const AText: string): IJSONObject;
begin
  if Trim(AText) = '' then
  begin
    Result := JSON;
    exit;
  end;

  FIndex := -1;
  FText := AText;
  FTextLength := Length(AText);

  if GetToken then
    exit;
  Result := ParseObject;
end;

function TParser.ExecuteForArray(const AText : string) : IJSONArray;
begin
  if Trim(AText) = '' then
  begin
    Result := JSONArray;
    exit;
  end;

  FIndex := -1;
  FText := AText;
  FTextLength := Length(AText);

  if GetToken then
    exit;
  Result := ParseArray;
end;

procedure TParser.ExecuteTo(const AText: string; const Obj: IJSONObject);
begin
  if Trim(AText) = '' then
  begin
    Obj.Clear;
    exit;
  end;

  FIndex := -1;
  FText := AText;
  FTextLength := Length(AText);

  if GetToken then
    exit;
  ParseObjectTo(Obj);
end;

class function TParser.Parse(const AText: string): IJSONObject;
var
  p : TParser;
begin
  p := TParser.Create;
  try
    Result := p.Execute(AText);
  finally
    p.Free;
  end;
end;

class function TParser.ParseArray(const AText : string) : IJSONArray;
var
  p : TParser;
begin
  p := TParser.Create;
  try
    Result := p.ExecuteForArray(AText);
  finally
    p.Free;
  end;
end;

end.



