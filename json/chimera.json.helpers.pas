unit chimera.json.helpers;

interface

uses System.Classes, chimera.json, System.Rtti;

type
  TObjectHelper = class helper for TObject
  private
    function RTTITypeToJSONType(kind : TTypeKind) : TJSONValueType;
    function GetAsJSONObject : IJSONObject;
    procedure SetAsJSONObject(const Obj : IJSONObject);
    function GetAsJSON : string;
    procedure SetAsJSON(const Value : string);
    function GenerateJSONObject(obj: TObject): IJSONObject;
    function GenerateJSONArray(ary: TValue): IJSONArray;
    procedure ApplyJSONObject(jso : IJSONObject; obj : TObject);
    procedure ApplyJSONArray(jsa : IJSONArray; val : TValue);
  public
    property AsJSON : String read GetAsJSON write SetAsJSON;
    property AsJSONObject : IJSONObject read GetAsJSONObject write SetAsJSONObject;
  end;


implementation

uses System.TypInfo;

{ TPersistentHelper }

procedure TObjectHelper.ApplyJSONArray(jsa: IJSONArray; val: TValue);
var
  i : integer;
  iLen : Integer;
  p : Pointer;
begin
  iLen := jsa.Count;
  p := val.GetReferenceToRawData;
  DynArraySetLength(p,Pointer(val.TypeInfo), 1,PNativeInt(@iLen));
  for i := 0 to iLen-1 do
  begin
    case RTTITypeToJSONType(val.Kind) of
      TJSONValueType.String:
        begin
          if val.IsType<Boolean> then
            val.SetArrayElement(i,TValue.From<Boolean>(jsa.Booleans[i]))
          else if val.Kind = tkEnumeration then
            val.SetArrayElement(i,TValue.From<String>(jsa.Strings[i]))  // TODO: Handle enumerations better
          else
            val.SetArrayElement(i,TValue.From<String>(jsa.Strings[i]));
        end;
      TJSONValueType.Number:
        begin
          if val.Kind in [tkInteger, tkInt64] then
            val.SetArrayElement(i,TValue.From<Int64>(jsa.Integers[i]))
          else
            val.SetArrayElement(i,TValue.From<Extended>(jsa.Numbers[i]));
        end;
      TJSONValueType.Object:
        ApplyJSONObject(jsa.Objects[i],val.GetArrayElement(i).AsObject);

      TJSONValueType.Array:
        ApplyJSONArray(jsa.Arrays[i],val.GetArrayElement(i));
      else
        Continue;
    end;
  end;
end;

procedure TObjectHelper.ApplyJSONObject(jso: IJSONObject; obj: TObject);
var
  cxt : TRTTIContext;
begin
  cxt := TRTTIContext.Create;
  jso.Each(
    procedure(const name : string; const value : PMultiValue)
    var
      prop : TRttiProperty;
    begin
      prop := cxt.GetType(obj.ClassInfo).GetProperty(name);

      case RTTITypeToJSONType(prop.PropertyType.TypeKind) of
        TJSONValueType.String:
          begin
            if prop.PropertyType.TypeKind = tkEnumeration then
            begin
              if prop.GetValue(obj).IsType<boolean> then
                prop.SetValue(obj, TValue.From<Boolean>(value.IntegerValue=1))
              else
                prop.SetValue(obj, TValue.FromOrdinal(prop.PropertyType.Handle, GetEnumValue(prop.PropertyType.Handle,value.StringValue)));
            end else
              prop.SetValue(obj, TValue.From<String>(value.StringValue));
          end;
        TJSONValueType.Number:
          begin
            if prop.PropertyType.TypeKind in [tkInteger, tkInt64] then
              prop.SetValue(obj, TValue.From<Int64>(value.IntegerValue))
            else
              prop.SetValue(obj, TValue.From<Extended>(value.NumberValue));
          end;
        TJSONValueType.Object:
          ApplyJSONObject(value.ObjectValue, prop.GetValue(obj).AsObject);

        TJSONValueType.Array:
          ApplyJSONArray(value.ArrayValue, prop.GetValue(obj));
        else
          exit;
      end;

    end
  );
end;

function TObjectHelper.GenerateJSONArray(ary : TValue) : IJSONArray;
var
  iLen : Integer;
  i : integer;
  val : TValue;
begin
  Result := JSONArray();
  iLen := ary.GetArrayLength;
  for i := 0 to iLen-1 do
  begin
    val := ary.GetArrayElement(i);
    case RTTITypeToJSONType(val.Kind) of
      TJSONValueType.String:
        begin
          if val.Kind = tkEnumeration then
          begin
            if val.IsType<boolean> then
              Result.Booleans[i] := val.AsBoolean
            else
              Result.Strings[i] := val.ToString
          end else
            Result.Strings[i] := val.AsString;

          if val.Kind = tkEnumeration then
            Result.Strings[i] := val.AsString
          else
            Result.Strings[i] := val.AsString;
        end;
      TJSONValueType.Number:
        begin
          if val.Kind in [tkInteger, tkInt64] then
            Result.Integers[i] := val.AsInt64
          else
            Result.Numbers[i] := val.AsExtended;
        end;
      TJSONValueType.Object:
        Result.Objects[i] := GenerateJSONObject(val.AsObject);

      TJSONValueType.Array:
        Result.Arrays[i] := GenerateJSONArray(val);
      else
        Continue;
    end;

  end;
end;

function TObjectHelper.GenerateJSONObject(obj : TObject) : IJSONObject;
var
  cxt : TRTTIContext;
  ary : TArray<TRttiProperty>;
  prop : TRttiProperty;
  jsoResult: IJSONObject;
begin
  cxt := TRTTIContext.Create;
  Result := JSON();
  if obj = nil then
    exit;
  ary := cxt.GetType(obj.ClassInfo).GetProperties;
  for prop in ary do
  begin
    if (prop.IsReadable) and (prop.Visibility = TMemberVisibility.mvPublished) then
    begin
      case RTTITypeToJSONType(prop.PropertyType.TypeKind) of
        TJSONValueType.String:
          begin
            if prop.PropertyType.TypeKind = tkEnumeration then
            begin
              if prop.GetValue(obj).IsType<boolean> then
                Result.Booleans[prop.Name] := prop.GetValue(obj).AsBoolean
              else
                Result.Strings[prop.Name] := prop.GetValue(obj).ToString
            end else
              Result.Strings[prop.Name] := prop.GetValue(obj).AsString;
          end;
        TJSONValueType.Number:
          begin
            if prop.PropertyType.TypeKind in [tkInteger, tkInt64] then
              Result.Integers[prop.Name] := prop.GetValue(obj).AsInt64
            else
              Result.Numbers[prop.Name] := prop.GetValue(obj).AsExtended;
          end;
        TJSONValueType.Object:
          begin
            jsoResult := Result;
            jsoResult.Objects[prop.Name] := GenerateJSONObject(prop.GetValue(obj).AsObject);
            jsoResult.Objects[prop.Name].OnChange :=
              procedure(const jso : IJSONObject)
              begin
                jsoResult.DoChangeNotify;
              end;
          end;

        TJSONValueType.Array:
          begin
            jsoResult := Result;
            jsoResult.Arrays[prop.Name] := GenerateJSONArray(prop.GetValue(obj));
            jsoResult.Arrays[prop.Name].OnChange :=
              procedure(const jsa : IJSONArray)
              begin
                jsoResult.DoChangeNotify;
              end;
          end;
        else
          Continue;
      end;
    end;
  end;
end;

function TObjectHelper.GetAsJSON: string;
begin
  Result := GetAsJSONObject.AsJSON;
end;

function TObjectHelper.GetAsJSONObject: IJSONObject;
var
  jso : IJSONObject;
begin
  jso := GenerateJSONObject(Self);
  jso.OnChange :=
    procedure(const obj : IJSONObject)
    begin
      AsJSONObject := jso;
    end;
  Result := jso;
end;

function TObjectHelper.RTTITypeToJSONType(kind: TTypeKind): TJSONValueType;
begin
  case kind of
    tkUnknown,
    tkChar,
    tkString,
    tkWChar,
    tkLString,
    tkWString,
    tkUString:
      Result := TJSONValueType.String;

    tkInteger,
    tkInt64:
      Result := TJSONValueType.Number;

    tkEnumeration:
      Result := TJSONValueType.String;

    tkFloat:
      Result := TJSONValueType.Number;

    tkClassRef,
    tkClass:
      Result := TJSONValueType.Object;

    tkSet,
    tkPointer,
    tkInterface,
    tkVariant,
    tkMethod,
    tkRecord,
    tkProcedure:
      Result := TJSONValueType.Null;


    tkArray,
    tkDynArray:
      Result := TJSONValueType.Array;

    else
      Result := TJSONValueType.Null;
  end;
end;

procedure TObjectHelper.SetAsJSON(const Value: string);
begin
  SetAsJSONObject(JSON(Value));
end;

procedure TObjectHelper.SetAsJSONObject(const Obj: IJSONObject);
begin
  ApplyJSONObject(obj, self);
end;

end.
