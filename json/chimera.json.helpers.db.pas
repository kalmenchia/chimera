unit chimera.json.helpers.db;

interface

uses System.Classes, chimera.json, Data.DB;

type
  TDatasetHelper = class helper for TDataset
  private
    function GetAsJSONArray: IJSONArray;
  public
    property AsJSONArray : IJSONArray read GetAsJSONArray;
    procedure UpdateFields(obj : IJSONObject);
  end;

implementation

uses System.SysUtils;

{ TDatasetHelper }

function TDatasetHelper.GetAsJSONArray: IJSONArray;
var
  jso : IJSONObject;
  i: Integer;
  bs : TBytesStream;
begin
  Result := JSONArray;
  while not EOF do
  begin
    jso := JSON;
    for i := 0 to FieldCount-1 do
    begin
      if not Fields[i].IsNull then
      begin
        case Fields[i].DataType of
          ftUnknown,
          ftString,
          ftFixedChar,
          ftWideString,
          ftVariant,
          ftFixedWideChar,
          ftWideMemo:
            jso.Strings[Fields[i].FieldName] := Fields[i].AsWideString;

          ftGuid:
            jso.Guids[Fields[i].FieldName] := StringToGuid(Fields[i].AsString);

          TFieldType.ftSingle,
          TFieldType.ftExtended,
          ftCurrency,
          ftFloat,
          ftBCD:
            jso.Numbers[Fields[i].FieldName] := Fields[i].AsExtended;

          ftSmallint,
          ftInteger,
          ftWord,
          ftAutoInc,
          ftLargeint,
          ftOraInterval,
          ftLongWord,
          ftShortint,
          ftByte,
          ftTimeStampOffset:
            jso.Integers[Fields[i].FieldName] := Fields[i].AsLargeInt;

          ftBoolean:
            jso.Booleans[Fields[i].FieldName] := Fields[i].AsBoolean;

          ftDate,
          ftTime,
          ftDateTime,
          ftTimeStamp,
          ftOraTimeStamp:
            jso.Dates[Fields[i].FieldName] := Fields[i].AsDateTime;


          ftBytes,
          ftVarBytes:
            jso.Bytes[Fields[i].FieldName] := Fields[i].AsBytes;

          ftBlob,
          ftMemo,
          ftGraphic,
          ftFmtMemo:
          begin
            bs := TBytesStream.Create;
            try
              TBlobField(Fields[i]).SaveToStream(bs);
              bs.Position := 0;
              jso.Bytes[Fields[i].FieldName] := bs.Bytes;
            finally
              bs.Free;
            end;
          end;

          ftParadoxOle,
          ftDBaseOle,
          ftCursor,
          ftADT,
          ftArray,
          ftReference,
          ftDataSet,
          ftOraBlob,
          ftOraClob,
          ftInterface,
          ftIDispatch,
          ftFMTBcd,
          ftConnection,
          ftParams,
          ftStream,
          ftObject,
          ftTypedBinary:
            begin
              // unsupported and skip
              Next;
              Continue;
            end;
        end;
      end else
        jso.AddNull(Fields[i].FieldName);
    end;
    Result.Add(jso);
    Next;
  end;
end;

procedure TDatasetHelper.UpdateFields(obj: IJSONObject);
begin
  obj.Each(
    procedure(const Name : string; const Value : PMultiValue)
    begin
      case Value.ValueType of
        TJSONValueType.&string:
          FieldByName(Name).AsString := Value.StringValue;
        TJSONValueType.number:
          if (Value.NumberValue = Round(Value.NumberValue)) and
             (Value.NumberValue <> Value.IntegerValue) then
            FieldByName(Name).AsLargeInt := Value.IntegerValue
          else
            FieldByName(Name).AsExtended := Value.NumberValue;
        TJSONValueType.&array,
        TJSONValueType.&object:
          // Currently Unsupported, skip
          ;
        TJSONValueType.boolean:
          FieldByName(Name).AsBoolean := Value.IntegerValue <> 0;
        TJSONValueType.null:
          FieldByName(Name).Clear;
      end;

    end
  );
end;

end.