// *****************************************************************************
//
// chimera.json;
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

unit chimera.json;

interface

{$I chimera.inc}

uses System.SysUtils, System.Classes, System.JSON
  {$IFDEF USEFASTCODE}, FastStringBuilder{$ENDIF};

type
{$SCOPEDENUMS ON}
  TProcConst<T> = reference to procedure(const Arg1: T);
  TProcConst<T1,T2> = reference to procedure (const Arg1: T1; const Arg2: T2);

  TJSONValueType = (&string, number, &array, &object, boolean, null, code);

  IJSONObject = interface;
  IJSONArray = interface;

  TChangeObjectHandler = reference to procedure(const obj : IJSONObject);
  TChangeArrayHandler = reference to procedure(const ary : IJSONArray);
  TDuplicateResolution = (Skip, Overwrite);
  TDuplicateHandler = reference to function(const prop : string) : TDuplicateResolution;

  EChimeraException = class(Exception);

  EChimeraJSONException = class(EChimeraException);

  TWhitespace = (compact, standard);

  PMultiValue = ^TMultiValue;
  TMultiValue = record
    ValueType : TJSONValueType;
    StringValue : string;
    NumberValue : Double;
    IntegerValue : Int64;
    ObjectValue : IJSONObject;
    ArrayValue : IJSONArray;
    constructor Initialize(const Value : String; encode : boolean = false); overload;
    constructor Initialize(const Value : Double; encode : boolean = false); overload;
    constructor Initialize(const Value : Int64; encode : boolean = false); overload;
    constructor Initialize(const Value : Boolean; encode : boolean = false); overload;
    constructor Initialize(const Value : IJSONObject; encode : boolean = false); overload;
    constructor Initialize(const Value : IJSONArray; encode : boolean = false); overload;
    constructor Initialize(const Value : Variant; encode : boolean = false); overload;
    constructor Initialize(const Value : PMultiValue); overload;
    function InitializeNull : TMultiValue; inline;
    constructor InitializeCode(const Value : String);
    function AsJSON : string; overload;
    procedure AsJSON(var Result : string); overload;
    procedure AsJSON(Result : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder); overload;
    function ToVariant : Variant;
  end;

  IJSONArray = interface(IInterface)
    ['{2D496737-5D01-4332-B2C2-7328772E3587}']
    function GetOnChange: TChangeArrayHandler;
    procedure SetOnChange(const Value: TChangeArrayHandler);
    function GetRaw(const idx: integer): PMultiValue;
    procedure SetRaw(const idx: integer; const Value: PMultiValue);
    function GetBoolean(const idx: integer): Boolean;
    function GetCount: integer;
    function GetNumber(const idx: integer): Double;
    function GetDate(const idx: integer): TDateTime;
    function GetLocalDate(const idx: integer): TDateTime;
    function GetIntDate(const idx: integer): TDateTime;
    function GetInteger(const idx: integer): Int64;
    function GetItem(const idx: integer): Variant;
    function GetString(const idx: integer): string;
    function GetObject(const idx: integer): IJSONObject;
    function GetArray(const idx: integer): IJSONArray;
    function GetType(const idx: integer): TJSONValueType;
    function GetBytes(const idx : integer) : TArray<Byte>;
    function GetGUID(const idx : integer) : TGuid;
    function GetValue(const idx : integer) : PMultiValue;

    function GetBooleanDefaulted(const idx: integer): Boolean;
    function GetNumberDefaulted(const idx: integer): Double;
    function GetDateDefaulted(const idx: integer): TDateTime;
    function GetLocalDateDefaulted(const idx: integer): TDateTime;
    function GetIntDateDefaulted(const idx: integer): TDateTime;
    function GetIntegerDefaulted(const idx: integer): Int64;
    function GetStringDefaulted(const idx: integer): string;
    function GetObjectDefaulted(const idx: integer): IJSONObject;
    function GetArrayDefaulted(const idx: integer): IJSONArray;
    function GetBytesDefaulted(const idx : integer) : TArray<Byte>;
    function GetGUIDDefaulted(const idx : integer) : TGuid;

    procedure SetBoolean(const idx: integer; const Value: Boolean);
    procedure SetCount(const Value: integer);
    procedure SetNumber(const idx: integer; const Value: Double);
    procedure SetDate(const idx: integer; const Value: TDateTime);
    procedure SetLocalDate(const idx: integer; const Value: TDateTime);
    procedure SetIntDate(const idx: integer; const Value: TDateTime);
    procedure SetInteger(const idx: integer; const Value: Int64);
    procedure SetItem(const idx: integer; const Value: Variant);
    procedure SetString(const idx: integer; const Value: string);
    procedure SetArray(const idx: integer; const Value: IJSONArray);
    procedure SetObject(const idx: integer; const Value: IJSONObject);
    procedure SetType(const idx: integer; const Value: TJSONValueType);
    procedure SetBytes(const idx : integer; const Value : TArray<Byte>);
    procedure SetGuid(const idx : integer; const Value : TGUID);
    //procedure ParentOverride(parent : IJSONArray); overload;
    //procedure ParentOverride(parent : IJSONObject); overload;

    procedure Add(const value : PMultiValue); overload;
    procedure Remove(const value : PMultiValue); overload;
    property Raw[const idx : integer] : PMultiValue read GetRaw write SetRaw;

    procedure Add(const value : string); overload;
    procedure Add(const value : double); overload;
    procedure Add(const value : int64); overload;
    procedure Add(const value : boolean); overload;
    procedure Add(const value : IJSONArray); overload;
    procedure Add(const value : IJSONObject); overload;
    procedure Add(const value : Variant); overload;
    procedure Add(const value : TArray<Byte>); overload;
    procedure AddNull;
    procedure AddCode(const value : string);
    procedure Merge(const &Array : IJSONArray);
    procedure Delete(const idx : integer);
    procedure Clear;

    procedure Remove(const value : string); overload;
    procedure Remove(const value : double); overload;
    procedure Remove(const value : int64); overload;
    procedure Remove(const value : boolean); overload;
    procedure Remove(const value : IJSONArray); overload;
    procedure Remove(const value : IJSONObject); overload;
    procedure Remove(const value : Variant); overload;

    function IndexOf(const value : string) : integer; overload;
    function IndexOf(const value : double) : integer; overload;
    function IndexOf(const value : int64) : integer; overload;
    function IndexOf(const value : boolean) : integer; overload;
    function IndexOf(const value : IJSONArray) : integer; overload;
    function IndexOf(const value : IJSONObject) : integer; overload;

    property OnChange : TChangeArrayHandler read GetOnChange write SetOnChange;
    procedure BeginUpdates;
    procedure EndUpdates;
    procedure DoChangeNotify;

    function Equals(const obj : IJSONArray) : boolean;

    function AsJSON(Whitespace : TWhitespace = TWhitespace.Standard) : string; overload;
    procedure AsJSON(var Result : string; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    procedure AsJSON(Result : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    function CreateRTLArray : System.JSON.TJSONArray;

    function LoadFromStream(idx : integer; Stream : TStream; Encode : boolean) : IJSONObject; overload;
    function LoadFromStream(Stream : TStream; Encode : boolean) : IJSONObject; overload;
    procedure SaveToStream(Stream: TStream; Decode : boolean); overload;
    procedure SaveToStream(idx : integer; Stream: TStream; Decode : boolean); overload;

    procedure Each(proc : TProcConst<string>); overload;
    procedure Each(proc : TProcConst<double>); overload;
    procedure Each(proc : TProcConst<int64>); overload;
    procedure Each(proc : TProcConst<boolean>); overload;
    procedure Each(proc : TProcConst<IJSONObject>); overload;
    procedure Each(proc : TProcConst<IJSONArray>); overload;
    procedure Each(proc : TProcConst<Variant>); overload;
    procedure Each(proc : TProcConst<PMultiValue>); overload;

    //function ParentArray : IJSONArray;
    //function ParentObject : IJSONObject;

    property GUIDs[const idx : integer] : TGUID read GetGuid write SetGuid;
    property Bytes[const idx : integer] : TArray<Byte> read GetBytes write SetBytes;
    property Strings[const idx : integer] : string read GetString write SetString;
    property Numbers[const idx : integer] : Double read GetNumber write SetNumber;
    property Dates[const idx : integer] : TDateTime read GetDate write SetDate;
    property LocalDates[const idx : integer] : TDateTime read GetLocalDate write SetLocalDate;
    property IntDates[const idx : integer] : TDateTime read GetIntDate write SetIntDate;
    property Integers[const idx : integer] : Int64 read GetInteger write SetInteger;
    property Booleans[const idx : integer] : Boolean read GetBoolean write SetBoolean;
    property Objects[const idx : integer] : IJSONObject read GetObject write SetObject;
    property Arrays[const idx : integer] : IJSONArray read GetArray write SetArray;

    property GUIDsDefaulted[const idx : integer] : TGUID read GetGuidDefaulted write SetGuid;
    property BytesDefaulted[const idx : integer] : TArray<Byte> read GetBytesDefaulted write SetBytes;
    property StringsDefaulted[const idx : integer] : string read GetStringDefaulted write SetString;
    property NumbersDefaulted[const idx : integer] : Double read GetNumberDefaulted write SetNumber;
    property DatesDefaulted[const idx : integer] : TDateTime read GetDateDefaulted write SetDate;
    property LocalDatesDefaulted[const idx : integer] : TDateTime read GetLocalDateDefaulted write SetLocalDate;
    property IntDatesDefaulted[const idx : integer] : TDateTime read GetIntDateDefaulted write SetIntDate;
    property IntegersDefaulted[const idx : integer] : Int64 read GetIntegerDefaulted write SetInteger;
    property BooleansDefaulted[const idx : integer] : Boolean read GetBooleanDefaulted write SetBoolean;
    property ObjectsDefaulted[const idx : integer] : IJSONObject read GetObjectDefaulted write SetObject;
    property ArraysDefaulted[const idx : integer] : IJSONArray read GetArrayDefaulted write SetArray;

    property Items[const idx : integer] : Variant read GetItem write SetItem; default;
    property Types[const idx : integer] : TJSONValueType read GetType write SetType;
    property Values[const idx : integer] : PMultiValue read GetValue;

    property Count : integer read GetCount write SetCount;
  end;

  IJSONObject = interface(IInterface)
    ['{D99D532B-A21C-4135-9DF5-0FFC8538CED4}']
    function GetOnChange: TChangeObjectHandler;
    procedure SetOnChange(const Value: TChangeObjectHandler);
    function GetRaw(const name: string): PMultiValue;
    procedure SetRaw(const name: string; const Value: PMultiValue);
    function GetHas(const name : string): Boolean;
    function GetCount: integer;

    function GetBoolean(const name : string): Boolean;
    function GetNumber(const name : string): Double;
    function GetDate(const name : string): TDateTime;
    function GetLocalDate(const name : string): TDateTime;
    function GetIntDate(const name : string): TDateTime;
    function GetInteger(const name : string): Int64;
    function GetString(const name : string): string;
    function GetObject(const name : string): IJSONObject;
    function GetArray(const name : string): IJSONArray;
    function GetBytes(const name : string): TArray<Byte>;
    function GetGuid(const name : string) : TGuid;

    function GetBooleanDefaulted(const name : string): Boolean;
    function GetNumberDefaulted(const name : string): Double;
    function GetDateDefaulted(const name : string): TDateTime;
    function GetLocalDateDefaulted(const name : string): TDateTime;
    function GetIntDateDefaulted(const name : string): TDateTime;
    function GetIntegerDefaulted(const name : string): Int64;
    function GetStringDefaulted(const name : string): string;
    function GetObjectDefaulted(const name : string): IJSONObject;
    function GetArrayDefaulted(const name : string): IJSONArray;
    function GetBytesDefaulted(const name : string): TArray<Byte>;
    function GetGuidDefaulted(const name : string) : TGuid;

    function GetItem(const name : string): Variant;
    function GetType(const name : string): TJSONValueType;
    function GetName(const idx : integer): string;
    function GetValue(const name : string) : PMultiValue;

    procedure SetBoolean(const name : string; const Value: Boolean);
    procedure SetNumber(const name : string; const Value: Double);
    procedure SetDate(const name : string; const Value: TDateTime);
    procedure SetLocalDate(const name : string; const Value: TDateTime);
    procedure SetIntDate(const name : string; const Value: TDateTime);
    procedure SetInteger(const name : string; const Value: Int64);
    procedure SetItem(const name : string; const Value: Variant);
    procedure SetString(const name : string; const Value: string);
    procedure SetArray(const name : string; const Value: IJSONArray);
    procedure SetObject(const name : string; const Value: IJSONObject);
    procedure SetType(const name : string; const Value: TJSONValueType);
    procedure SetBytes(const name : string; const Value: TArray<Byte>);
    procedure SetGuid(const name : String; const Value : TGuid);
    //procedure ParentOverride(parent : IJSONArray); overload;
    //procedure ParentOverride(parent : IJSONObject); overload;

    procedure Each(proc : TProcConst<string, PMultiValue>); overload;
    procedure Add(const name : string; const value : PMultiValue); overload;
    property Raw[const name : string] : PMultiValue read GetRaw write SetRaw;

    procedure Each(proc : TProcConst<string, string>); overload;
    procedure Each(proc : TProcConst<string, double>); overload;
    procedure Each(proc : TProcConst<string, int64>); overload;
    procedure Each(proc : TProcConst<string, boolean>); overload;
    procedure Each(proc : TProcConst<string, IJSONObject>); overload;
    procedure Each(proc : TProcConst<string, IJSONArray>); overload;
    procedure Each(proc : TProcConst<string, Variant>); overload;

    procedure Add(const name : string; const value : string); overload;
    procedure Add(const name : string; const value : double); overload;
    procedure Add(const name : string; const value : Int64); overload;
    procedure Add(const name : string; const value : boolean); overload;
    procedure Add(const name : string; const value : IJSONArray); overload;
    procedure Add(const name : string; const value : IJSONObject); overload;
    procedure Add(const name : string; const value : Variant); overload;
    procedure Add(const name : string; const value : TArray<Byte>); overload;
    procedure Remove(const name : string);
    procedure AddNull(const name : string);
    procedure AddCode(const name : string; const value : string);

    procedure Merge(const &object : IJSONObject; OnDuplicate : TDuplicateHandler = nil);
    function SameAs(CompareTo : IJSONObject) : boolean;
    function AsSHA1(Whitespace : TWhitespace = TWhitespace.Standard) : string;
    function AsJSON(Whitespace : TWhitespace = TWhitespace.Standard) : string; overload;
    procedure AsJSON(var Result : string; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    procedure AsJSON(Result : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    function CreateRTLObject : System.JSON.TJSONObject;

    procedure Reload(const Source : string);
    procedure Clear;
    property OnChange : TChangeObjectHandler read GetOnChange write SetOnChange;
    procedure BeginUpdates;
    procedure EndUpdates;
    procedure DoChangeNotify;

    function Equals(const obj : IJSONObject) : boolean;
    function LoadFromStream(const Name : String; Stream : TStream; Encode : boolean) : IJSONObject; overload;
    function LoadFromStream(Stream : TStream) : IJSONObject; overload;
    function LoadFromFile(const Name, Filename : string; Encode : boolean) : IJSONObject; overload;
    function LoadFromFile(const Filename : string) : IJSONObject; overload;
    procedure SaveToStream(const name : string; Stream : TStream; Decode : boolean); overload;
    procedure SaveToStream(Stream : TStream; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    procedure SaveToFile(const Name, Filename : string; Decode : boolean); overload;
    procedure SaveToFile(const Filename : string; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    //function ParentArray : IJSONArray;
    //function ParentObject : IJSONObject;

    property GUIDs[const name : string] : TGuid read GetGuid write SetGuid;
    property Bytes[const name : string] : TArray<Byte> read GetBytes write SetBytes;
    property Strings[const name : string] : string read GetString write SetString;
    property Numbers[const name : string] : Double read GetNumber write SetNumber;
    property Dates[const name : string] : TDateTime read GetDate write SetDate;
    property LocalDates[const name : string] : TDateTime read GetLocalDate write SetLocalDate;
    property IntDates[const name : string] : TDateTime read GetIntDate write SetIntDate;
    property Integers[const name : string] : Int64 read GetInteger write SetInteger;
    property Booleans[const name : string] : Boolean read GetBoolean write SetBoolean;
    property Objects[const name : string] : IJSONObject read GetObject write SetObject;
    property Arrays[const name : string] : IJSONArray read GetArray write SetArray;

    property GUIDsDefaulted[const name : string] : TGuid read GetGuidDefaulted write SetGuid;
    property BytesDefaulted[const name : string] : TArray<Byte> read GetBytesDefaulted write SetBytes;
    property StringsDefaulted[const name : string] : string read GetStringDefaulted write SetString;
    property NumbersDefaulted[const name : string] : Double read GetNumberDefaulted write SetNumber;
    property DatesDefaulted[const name : string] : TDateTime read GetDateDefaulted write SetDate;
    property LocalDatesDefaulted[const name : string] : TDateTime read GetLocalDateDefaulted write SetLocalDate;
    property IntDatesDefaulted[const name : string] : TDateTime read GetIntDateDefaulted write SetIntDate;
    property IntegersDefaulted[const name : string] : Int64 read GetIntegerDefaulted write SetInteger;
    property BooleansDefaulted[const name : string] : Boolean read GetBooleanDefaulted write SetBoolean;
    property ObjectsDefaulted[const name : string] : IJSONObject read GetObjectDefaulted write SetObject;
    property ArraysDefaulted[const name : string] : IJSONArray read GetArrayDefaulted write SetArray;

    property Items[const name : string] : Variant read GetItem write SetItem; default;
    property Types[const name : string] : TJSONValueType read GetType write SetType;
    property Values[const name : string] : PMultiValue read GetValue;

    property Count : integer read GetCount;
    property Names[const idx : integer] : string read GetName;
    property Has[const name : string] : boolean read GetHas;
  end;

function JSON(const src : string = '') : IJSONObject;
function JSONArray(const src : string = '') : IJSONArray;
function FormatJSON(const src : string; Indent : byte = 3) : string;
function JSONEncode(const str : string) : string;
function JSONDecode(const str : string) : string;
function StringIsJSON(const str : string) : boolean;
function JSONValueTypeToString(t : TJSONValueTYpe) : string;

implementation

uses System.Variants, System.Generics.Collections, chimera.json.parser,
  System.StrUtils, System.DateUtils, System.TimeSpan, System.NetEncoding,
  System.Hash;

function JSONValueTypeToString(t : TJSONValueTYpe) : string;
begin
  case t of
    TJSONValueType.string:  Result := 'String';
    TJSONValueType.number:  Result := 'Number';
    TJSONValueType.array:   Result := 'Array';
    TJSONValueType.object:  Result := 'Object';
    TJSONValueType.boolean: Result := 'Boolean';
    TJSONValueType.null:    Result := 'Null';
    TJSONValueType.code:    Result := 'Code';
    else
      Result := '(Unknown)';
  end;
end;

type
  TJSONArray = class(TInterfacedObject, IJSONArray)
  private
    FUpdating: Boolean;
    FOnChangeHandler: TChangeArrayHandler; // IJSONArray
    //FParentObject: IJSONObject;
    //FParentArray: IJSONArray;

    function GetOnChange: TChangeArrayHandler;
    procedure SetOnChange(const Value: TChangeArrayHandler);
    function GetRaw(const idx: integer): PMultiValue;
    procedure SetRaw(const idx: integer; const Value: PMultiValue);

    function GetBoolean(const idx: integer): Boolean;
    function GetCount: integer;
    function GetNumber(const idx: integer): Double;
    function GetDate(const idx: Integer): TDateTime;
    function GetLocalDate(const idx: Integer): TDateTime;
    function GetIntDate(const idx: Integer): TDateTime;
    function GetInteger(const idx: integer): Int64;
    function GetItem(const idx: integer): Variant;
    function GetString(const idx: integer): string;
    function GetObject(const idx: integer): IJSONObject;
    function GetArray(const idx: integer): IJSONArray;
    function GetType(const idx: integer): TJSONValueType;
    function GetBytes(const idx : integer) : TArray<Byte>;
    function GetGuid(const idx : integer) : TGuid;
    function GetValue(const idx : integer) : PMultiValue;

    function GetBooleanDefaulted(const idx: integer): Boolean;
    function GetNumberDefaulted(const idx: integer): Double;
    function GetDateDefaulted(const idx: integer): TDateTime;
    function GetLocalDateDefaulted(const idx: integer): TDateTime;
    function GetIntDateDefaulted(const idx: integer): TDateTime;
    function GetIntegerDefaulted(const idx: integer): Int64;
    function GetStringDefaulted(const idx: integer): string;
    function GetObjectDefaulted(const idx: integer): IJSONObject;
    function GetArrayDefaulted(const idx: integer): IJSONArray;
    function GetBytesDefaulted(const idx : integer) : TArray<Byte>;
    function GetGUIDDefaulted(const idx : integer) : TGuid;

    procedure SetGuid(const idx : integer; const Value : TGUID);
    procedure SetBytes(const idx: integer; const Value: TArray<Byte>);
    procedure SetBoolean(const idx: integer; const Value: Boolean);
    procedure SetCount(const Value: integer);
    procedure SetNumber(const idx: integer; const Value: Double);
    procedure SetDate(const idx: Integer; const Value: TDateTime);
    procedure SetLocalDate(const idx: Integer; const Value: TDateTime);
    procedure SetIntDate(const idx: Integer; const Value: TDateTime);
    procedure SetInteger(const idx: integer; const Value: Int64);
    procedure SetItem(const idx: integer; const Value: Variant);
    procedure SetString(const idx: integer; const Value: string);
    procedure SetArray(const idx: integer; const Value: IJSONArray);
    procedure SetObject(const idx: integer; const Value: IJSONObject);
    procedure SetType(const idx: integer; const Value: TJSONValueType);
    //function ParentArray : IJSONArray;
    //function ParentObject : IJSONObject;
    //procedure ParentOverride(parent : IJSONArray); overload;
    //procedure ParentOverride(parent : IJSONObject); overload;
  private
    FValues : TList<PMultiValue>;
    procedure EnsureSize(const idx : integer);
  public // IJSONArray

    procedure Add(const value : PMultiValue); overload;
    procedure Remove(const value : PMultiValue); overload;
    property Raw[const idx: integer] : PMultiValue read GetRaw write SetRaw;

    function LoadFromStream(idx : integer; Stream : TStream; Encode : boolean) : IJSONObject; overload;
    function LoadFromStream(Stream : TStream; Encode : boolean) : IJSONObject; overload;
    procedure SaveToStream(Stream: TStream; Decode : boolean); overload;
    procedure SaveToStream(idx : integer; Stream: TStream; Decode : boolean); overload;

    procedure Each(proc : TProcConst<string>); overload;
    procedure Each(proc : TProcConst<double>); overload;
    procedure Each(proc : TProcConst<int64>); overload;
    procedure Each(proc : TProcConst<boolean>); overload;
    procedure Each(proc : TProcConst<IJSONObject>); overload;
    procedure Each(proc : TProcConst<IJSONArray>); overload;
    procedure Each(proc : TProcConst<Variant>); overload;
    procedure Each(proc : TProcConst<PMultiValue>); overload;

    procedure Add(const value : string); overload;
    procedure Add(const value : double); overload;
    procedure Add(const value : boolean); overload;
    procedure Add(const value : Int64); overload;
    procedure Add(const value : IJSONArray); overload;
    procedure Add(const value : IJSONObject); overload;
    procedure Add(const value : Variant); overload;
    procedure Add(const value : TArray<Byte>); overload;
    procedure AddNull;
    procedure AddCode(const value : string);
    procedure Merge(const &Array : IJSONArray);

    function AsJSON(Whitespace : TWhitespace = TWhitespace.Standard) : string; overload;
    procedure AsJSON(var Result : string; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    procedure AsJSON(Result : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    function CreateRTLArray: System.JSON.TJSONArray;

    procedure Delete(const idx: Integer);
    procedure Clear;

    property OnChange : TChangeArrayHandler read GetOnChange write SetOnChange;
    procedure BeginUpdates;
    procedure EndUpdates;
    procedure DoChangeNotify;

    procedure Remove(const value : string); overload;
    procedure Remove(const value : double); overload;
    procedure Remove(const value : int64); overload;
    procedure Remove(const value : boolean); overload;
    procedure Remove(const value : IJSONArray); overload;
    procedure Remove(const value : IJSONObject); overload;
    procedure Remove(const value : Variant); overload;

    function IndexOf(const value : string) : integer; overload;
    function IndexOf(const value : double) : integer; overload;
    function IndexOf(const value : int64) : integer; overload;
    function IndexOf(const value : boolean) : integer; overload;
    function IndexOf(const value : IJSONArray) : integer; overload;
    function IndexOf(const value : IJSONObject) : integer; overload;

    function Equals(const obj : IJSONArray) : boolean; reintroduce;

    property GUIDs[const idx : integer] : TGuid read GetGuid write SetGuid;
    property Bytes[const idx : integer] : TArray<Byte> read GetBytes write SetBytes;
    property Strings[const idx : integer] : string read GetString write SetString;
    property Numbers[const idx : integer] : Double read GetNumber write SetNumber;
    property Dates[const idx : integer] : TDateTime read GetDate write SetDate;
    property LocalDates[const idx : integer] : TDateTime read GetLocalDate write SetLocalDate;
    property IntDates[const idx : integer] : TDateTime read GetIntDate write SetIntDate;
    property Integers[const idx : integer] : Int64 read GetInteger write SetInteger;
    property Booleans[const idx : integer] : Boolean read GetBoolean write SetBoolean;
    property Objects[const idx : integer] : IJSONObject read GetObject write SetObject;
    property Arrays[const idx : integer] : IJSONArray read GetArray write SetArray;
    property Items[const idx : integer] : Variant read GetItem write SetItem; default;
    property Types[const idx : integer] : TJSONValueType read GetType write SetType;
    property Count : integer read GetCount write SetCount;

    constructor Create; overload; virtual;
    destructor Destroy; override;
  end;

  TJSONObject = class(TInterfacedObject, IJSONObject)
  private
    FUpdating: Boolean;
    FOnChangeHandler: TChangeObjectHandler; // IJSONObject

    function GetRaw(const name: string): PMultiValue;
    procedure SetRaw(const name: string; const Value: PMultiValue);

    //FParentObject: IJSONObject;
    //FParentArray: IJSONArray;
    function GetBoolean(const name : string): Boolean;
    function GetCount: integer;
    function GetItem(const name : string): Variant;
    function GetType(const name : string): TJSONValueType;
    function GetName(const idx : integer): string;

    function GetNumber(const name : string): Double;
    function GetDate(const name : string): TDateTime;
    function GetLocalDate(const name : string): TDateTime;
    function GetIntDate(const name : string): TDateTime;
    function GetInteger(const name : string): Int64;
    function GetString(const name : string): string;
    function GetObject(const name : string): IJSONObject;
    function GetArray(const name : string): IJSONArray;
    function GetBytes(const name : string): TArray<Byte>;
    function GetGuid(const name: string): TGuid;
    function GetValue(const name: string) : PMultiValue;

    function GetBooleanDefaulted(const name : string): Boolean;
    function GetNumberDefaulted(const name : string): Double;
    function GetDateDefaulted(const name : string): TDateTime;
    function GetLocalDateDefaulted(const name : string): TDateTime;
    function GetIntDateDefaulted(const name : string): TDateTime;
    function GetIntegerDefaulted(const name : string): Int64;
    function GetStringDefaulted(const name : string): string;
    function GetObjectDefaulted(const name : string): IJSONObject;
    function GetArrayDefaulted(const name : string): IJSONArray;
    function GetBytesDefaulted(const name : string): TArray<Byte>;
    function GetGuidDefaulted(const name : string) : TGuid;

    procedure SetGuid(const name: string; const Value: TGuid);
    procedure SetBytes(const name : string; const Value: TArray<Byte>);
    procedure SetBoolean(const name : string; const Value: Boolean);
    procedure SetNumber(const name : string; const Value: Double);
    procedure SetDate(const name : string; const Value: TDateTime);
    procedure SetLocalDate(const name : string; const Value: TDateTime);
    procedure SetIntDate(const name : string; const Value: TDateTime);
    procedure SetInteger(const name : string; const Value: Int64);
    procedure SetItem(const name : string; const Value: Variant);
    procedure SetString(const name : string; const Value: string);
    procedure SetArray(const name : string; const Value: IJSONArray);
    procedure SetObject(const name : string; const Value: IJSONObject);
    procedure SetType(const name : string; const Value: TJSONValueType);
    function GetHas(const name: string): boolean;
    //procedure ParentOverride(parent : IJSONArray); overload;
    //procedure ParentOverride(parent : IJSONObject); overload;
  private
    FValues : TDictionary<string, PMultiValue>;
    procedure DisposeOfValue(Sender: TObject; const Item: PMultiValue; Action: TCollectionNotification);
    function GetValueOf(const name: string): PMultiValue;
    function GetOnChange: TChangeObjectHandler;
    procedure SetOnChange(const Value: TChangeObjectHandler);
    property ValueOf[const name : string] : PMultiValue read GetValueOf;
  public  // IJSONObject
    procedure Each(proc : TProcConst<string, PMultiValue>); overload;
    procedure Add(const name : string; const value : PMultiValue); overload;
    property Raw[const name : string] : PMultiValue read GetRaw write SetRaw;
    procedure Reload(const Source : string);
    procedure Clear;

    procedure Each(proc : TProcConst<string, string>); overload;
    procedure Each(proc : TProcConst<string, double>); overload;
    procedure Each(proc : TProcConst<string, int64>); overload;
    procedure Each(proc : TProcConst<string, boolean>); overload;
    procedure Each(proc : TProcConst<string, IJSONObject>); overload;
    procedure Each(proc : TProcConst<string, IJSONArray>); overload;
    procedure Each(proc : TProcConst<string, Variant>); overload;

    procedure Add(const name : string; const value : string); overload;
    procedure Add(const name : string; const value : double); overload;
    procedure Add(const name : string; const value : Int64); overload;
    procedure Add(const name : string; const value : boolean); overload;
    procedure Add(const name : string; const value : IJSONArray); overload;
    procedure Add(const name : string; const value : IJSONObject); overload;
    procedure Add(const name : string; const value : Variant); overload;
    procedure Add(const name : string; const value : TArray<Byte>); overload;
    procedure AddNull(const name : string);
    procedure AddCode(const name : string; const value : string);

    procedure Merge(const &object : IJSONObject; OnDuplicate : TDuplicateHandler = nil);
    function SameAs(CompareTo : IJSONObject) : boolean;
    function AsSHA1(Whitespace : TWhitespace = TWhitespace.Standard) : string;
    function AsJSON(Whitespace : TWhitespace = TWhitespace.Standard) : string; overload;
    procedure AsJSON(var Result : string; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    procedure AsJSON(Result : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    function CreateRTLObject: System.JSON.TJSONObject;

    procedure Remove(const name: string);
    function LoadFromStream(const Name : String; Stream : TStream; Encode : boolean) : IJSONObject; overload;
    function LoadFromStream(Stream : TStream) : IJSONObject; overload;
    function LoadFromFile(const Name, Filename : string; Encode : boolean) : IJSONObject; overload;
    function LoadFromFile(const Filename : string) : IJSONObject; overload;
    procedure SaveToStream(const Name : string; Stream : TStream; Decode : boolean); overload;
    procedure SaveToStream(Stream : TStream; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    procedure SaveToFile(const Name, Filename : string; Decode : boolean); overload;
    procedure SaveToFile(const Filename : string; Whitespace : TWhitespace = TWhitespace.Standard); overload;
    property OnChange : TChangeObjectHandler read GetOnChange write SetOnChange;
    procedure BeginUpdates;
    procedure EndUpdates;

    procedure DoChangeNotify;
    //function ParentArray : IJSONArray;
    //function ParentObject : IJSONObject;

    function Equals(const obj : IJSONObject) : boolean; reintroduce;

    property GUIDs[const name : string] : TGuid read GetGuid write SetGuid;
    property Bytes[const name : string] : TArray<Byte> read GetBytes write SetBytes;
    property Strings[const name : string] : string read GetString write SetString;
    property Numbers[const name : string] : Double read GetNumber write SetNumber;
    property Dates[const name : string] : TDateTime read GetDate write SetDate;
    property LocalDates[const name : string] : TDateTime read GetLocalDate write SetLocalDate;
    property IntDates[const name : string] : TDateTime read GetIntDate write SetIntDate;
    property Integers[const name : string] : Int64 read GetInteger write SetInteger;
    property Booleans[const name : string] : Boolean read GetBoolean write SetBoolean;
    property Objects[const name : string] : IJSONObject read GetObject write SetObject;
    property Arrays[const name : string] : IJSONArray read GetArray write SetArray;
    property Items[const name : string] : Variant read GetItem write SetItem; default;
    property Types[const name : string] : TJSONValueType read GetType write SetType;
    property Count : integer read GetCount;
    property Names[const idx : integer] : string read GetName;
    property Has[const name : string] : boolean read GetHas;

    constructor Create; overload; virtual;
    destructor Destroy; override;
  end;

function WhiteChar(c : Char; Whitespace : TWhitespace) : String; inline;
begin
  case Whitespace of
    TWhitespace.compact: Result := c;
    TWhitespace.standard: Result := ' '+c+' ';
  end;
end;

function WhiteCharBefore(c : Char; Whitespace : TWhitespace) : String; inline;
begin
  case Whitespace of
    TWhitespace.compact: Result := c;
    TWhitespace.standard: Result := ' '+c;
  end;
end;

function WhiteCharAfter(c : Char; Whitespace : TWhitespace) : String; inline;
begin
  case Whitespace of
    TWhitespace.compact: Result := c;
    TWhitespace.standard: Result := c+' ';
  end;
end;

function IsValidLocalDate(Value : TDateTime) : boolean;
var
  TimeZone: TTimeZone;
begin
  TimeZone := TTimeZone.Local;
  Result := (Double(Value) * 1000) + TimeZone.UtcOffset.TotalMilliseconds >= 0;
end;

function StringIsJSON(const str : string) : boolean;
begin
  result := (str <> '') and (str[1] = '{') and (str[length(str)] = '}')
end;

function FormatJSON(const src : string; Indent : Byte = 3) : string;
  function Spaces(var Size : integer; iLevel : integer; indent : byte) : string;
  var
    i: Integer;
  begin
    Size := (iLevel*Indent);
    setlength(result,Size);
    for i := 1 to Size do
      Result[i] := ' ';
  end;
var
  i, iSize,  iLevel : integer;
  sb : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder;
  bInString : boolean;
begin
  iLevel := 0;
  sb := {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder.Create(src);
  try
    i := 0;
    bInString := False;
    while i < sb.length do
    begin
      case sb.Chars[i] of
        '{', '[':
        begin
          if not bInString then
          begin
            inc(iLevel);
            while (i < sb.Length - 1) and (sb.Chars[i+1] = ' ') do
              sb.Remove(i+1,1);
            if not CharInSet(sb.Chars[i],['}',']'])  then
            begin
              sb.Insert(i+1,#13#10+Spaces(iSize, iLevel,Indent));
              inc(i,iSize+2);
            end;
          end;
        end;
        ',':
        begin
          if not bInString then
          begin
            while (i < sb.Length - 1) and (sb.Chars[i+1] = ' ') do
              sb.Remove(i+1,1);
            sb.Insert(i+1,#13#10+Spaces(iSize, iLevel,Indent));
            inc(i,iSize+2);
          end;
        end;
        '"':
        begin
          if (sb.Chars[i-1] <> '/') then
            bInString := not bInString;
        end;
        '}',']':
        begin
          if not bInString then
          begin
            while (i < sb.Length - 1) and (sb.Chars[i+1] = ' ') do
              sb.Remove(i+1,1);
            if not CharInSet(sb.Chars[i-1],['{','['])  then
            begin
              dec(iLevel);
              sb.Insert(i,#13#10+Spaces(iSize, iLevel,Indent));
              inc(i,iSize+2);
            end;
          end;
        end;
      end;
      inc(i);
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function JSONEncode(const str : string) : string;
var
  i : integer;
  sb : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder;
begin
  sb := {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder.Create;
  try
    for i := 0 to str.Length-1 do
    begin
      case str.Chars[i] of
        '"',
        '\',
        '/': begin
          sb.append('\');
          sb.append(str.Chars[i]);
        end;
        #8: begin
          sb.append('\b');
        end;
        #9: begin
          sb.append('\t');
        end;
        #10: begin
          sb.append('\n');
        end;
        #12: begin
          sb.append('\f');
        end;
        #13: begin
          sb.append('\r');
        end;
        else if (Ord(str.Chars[i]) > 255) or (Ord(str.Chars[i]) < 32) then
        begin
          sb.append('\u'+IntToHex(Ord(str.Chars[i]),4));
        end else
          sb.append(str.Chars[i]);
      end;
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function JSONDecode(const str : string) : string;
var
  i : integer;
  ichar : integer;
  sb : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder;
begin
  sb := {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder.Create;
  try
    i := 0;
    while i < str.length-1 do
    begin
      if str.Chars[i] = '\' then
      begin
        case str.Chars[i+1] of
          '"',
          '\',
          '/':
            sb.Append(str.Chars[i+1]);
          'b': begin
            sb.append(#8);
          end;
          't': begin
            sb.append(#9);
          end;
          'n': begin
            sb.append(#10);
          end;
          'f': begin
            sb.append(#12);
          end;
          'r': begin
            sb.append(#13);
          end;
          'u': begin
            if TryStrToInt('$'+str.Substring(i+2,4),iChar) then
            begin
              sb.append(Char(iChar));
              inc(i,4);
            end else
              sb.insert(0, str[i+1]);
          end;
        end;
        inc(i);
      end else
        sb.Append(str.Chars[i]);
      inc(i);
    end;
    if i < str.length then
      sb.Append(str.Chars[i]);
    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure VerifyType(t1, t2 : TJSONValueType); inline;
begin
  if t1 <> t2 then
    if ((t1 = TJSONValueType.null) and
        not (t2 in [TJSONValueType.&string, TJSONValueType.number, TJSONValueType.&object])) or
       ((t2 = TJSONValueType.null) and
        not (t1 in [TJSONValueType.&string, TJSONValueType.number, TJSONValueType.&object])) then
    raise EChimeraJSONException.Create('Value is not of required type: '+JSonValueTypeToString(t1)+' <> '+JSONValueTypeToString(t2));
end;

function JSON(const src : string) : IJSONObject;
begin
  if src <> '' then
    Result := TParser.Parse(src)
  else
    Result := TJSONObject.Create;
end;

function JSONArray(const src : string) : IJSONArray;
begin
  if src <> '' then
    Result := TParser.ParseArray(src)
  else
    Result := TJSONArray.Create;
end;

{ TJSONArray }

procedure TJSONArray.Add(const value: double);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
end;

procedure TJSONArray.Add(const value: string);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
end;

procedure TJSONArray.Add(const value: Variant);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
end;

procedure TJSONArray.Add(const value: boolean);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
end;

procedure TJSONArray.Add(const value: IJSONObject);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
  //value.ParentOverride(Self);
end;

procedure TJSONArray.Add(const value: Int64);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
end;

procedure TJSONArray.Add(const value: IJSONArray);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
  //value.ParentOverride(self);
end;

procedure TJSONArray.AddNull;
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.InitializeNull;
  FValues.Add(pmv);
end;

procedure TJSONArray.AddCode(const value: string);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.InitializeCode(value);
  FValues.Add(pmv);
end;

function TJSONArray.AsJSON(Whitespace : TWhitespace = TWhitespace.Standard): string;
begin
  Result := '';
  AsJSON(Result, Whitespace);
end;

procedure TJSONArray.AsJSON(var Result : string; Whitespace : TWhitespace = TWhitespace.Standard);
var
  i: Integer;
begin
  Result := Result+'[';
  for i := 0 to FValues.Count-1 do
  begin
    if i > 0 then
      Result := Result+WhiteChar(',', Whitespace);
    FValues[i].AsJSON(Result);
  end;
  Result := Result+']';
end;

procedure TJSONArray.AsJSON(Result : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder; Whitespace : TWhitespace = TWhitespace.Standard);
var
  i: Integer;
begin
  Result.Append('[');
  for i := 0 to FValues.Count-1 do
  begin
    if i > 0 then
      Result.Append(WhiteChar(',', Whitespace));
    FValues[i].AsJSON(Result);
  end;
  Result.Append(']');
end;

procedure TJSONArray.BeginUpdates;
begin
  FUpdating := True;
end;

procedure TJSONArray.Clear;
begin
  while Count > 0 do
  begin
    if FValues[0].ObjectValue <> nil then
      FValues[0].ObjectValue.OnChange := nil;
    Delete(0);
  end;
end;

{constructor TJSONArray.Create(Parent : IJSONObject);
begin
  Create;
  FParentObject := Parent;
  FParentArray := nil;
end;

constructor TJSONArray.Create(Parent : IJSONArray);
begin
  Create;
  FParentObject := nil;
  FParentArray := Parent;
end;}

constructor TJSONArray.Create;
begin
  inherited Create;
  FValues := TList<PMultiValue>.Create;
end;

function TJSONArray.CreateRTLArray: System.JSON.TJSONArray;
begin
  Result := System.JSON.TJSONArray(System.JSON.TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AsJSON),0));
end;

destructor TJSONArray.Destroy;
begin
  Clear;
  FValues.Free;
  inherited;
end;

procedure TJSONArray.DoChangeNotify;
begin
  if Assigned(FOnChangeHandler) and (not FUpdating) then
    FOnChangeHandler(Self);
end;

procedure TJSONArray.Each(proc: TProcConst<int64>);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    proc(FValues[i].IntegerValue);
end;

procedure TJSONArray.Each(proc: TProcConst<double>);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    proc(FValues[i].NumberValue);
end;

procedure TJSONArray.Each(proc: TProcConst<string>);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    proc(strings[i]);
end;

procedure TJSONArray.Each(proc: TProcConst<boolean>);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    proc(FValues[i].IntegerValue <> 0);
end;

procedure TJSONArray.Each(proc: TProcConst<Variant>);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    proc(FValues[i].ToVariant);
end;

procedure TJSONArray.Each(proc: TProcConst<IJSONArray>);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    proc(FValues[i].ArrayValue);
end;

procedure TJSONArray.Each(proc: TProcConst<IJSONObject>);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    proc(FValues[i].ObjectValue);
end;

procedure TJSONArray.EndUpdates;
begin
  FUpdating := False;
  DoChangeNotify;
end;

procedure TJSONArray.EnsureSize(const idx: integer);
var
  pmv : PMultiValue;
begin
  while FValues.Count <= idx do
  begin
    New(pmv);
    pmv.InitializeNull;
    FValues.Add(pmv);
  end;
end;

function TJSONArray.Equals(const obj: IJSONArray): boolean;
var
  i : integer;
  j: Integer;
begin
  Result := obj.Count = Count;

  for i := 0 to FValues.Count-1 do
  begin
    if not Result then
      exit;
    case FValues[i].ValueType of
      TJSONValueType.string:
        Result := obj.IndexOf(Strings[i]) >= 0;
      TJSONValueType.number:
        Result := obj.IndexOf(Numbers[i]) >= 0;
      TJSONValueType.boolean:
        Result := obj.IndexOf(Booleans[i]) >= 0;
      TJSONValueType.array:
        Result := obj.IndexOf(Arrays[i]) >= 0;
      TJSONValueType.object:
        Result := obj.IndexOf(Objects[i]) >= 0;
      TJSONValueType.null:
        for j := 0 to obj.Count-1 do
        begin
          if obj.Raw[j].ObjectValue = nil then
          begin
            Result := True;
            continue;
          end;
        end;
      TJSONValueType.code:
        for j := 0 to obj.Count-1 do
        begin
          if obj.Raw[j].StringValue = FValues[i].StringValue then
          begin
            Result := True;
            continue;
          end;
        end;
    end;
  end;
end;

function TJSONArray.GetArray(const idx: integer): IJSONArray;
begin
  VerifyType(FValues.Items[idx].ValueType, TJSONValueType.&array);
  Result := FValues.Items[idx].ArrayValue;
end;

function TJSONArray.GetArrayDefaulted(const idx: integer): IJSONArray;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetArray(idx)
  else
    Result := JSONArray;
end;

function TJSONArray.GetBoolean(const idx: integer): Boolean;
begin
  VerifyType(FValues.Items[idx].ValueType, TJSONValueType.boolean);
  Result := FValues.Items[idx].IntegerValue <> 0;
end;

function TJSONArray.GetBooleanDefaulted(const idx: integer): Boolean;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetBoolean(idx)
  else
    Result := False;
end;

function TJSONArray.GetBytes(const idx: integer): TArray<Byte>;
begin
  Result := TNetEncoding.Base64.Decode(TEncoding.UTF8.GetBytes(Strings[idx]));
end;

function TJSONArray.GetBytesDefaulted(const idx: integer): TArray<Byte>;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetBytes(idx)
  else
    SetLength(Result,0);
end;

function TJSONArray.GetCount: integer;
begin
  Result := FValues.Count;
end;

function TJSONArray.GetDate(const idx: Integer): TDateTime;
var
  TempDate: TDateTime;
begin
  Result := 0.0;
  if TryISO8601ToDate(GetString(idx),TempDate) then
    Result := TempDate;
//  Result := ISO8601ToDate(GetString(idx));
end;

function TJSONArray.GetDateDefaulted(const idx: integer): TDateTime;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetDate(idx)
  else
    Result := 0;
end;

function TJSONArray.GetGuid(const idx: integer): TGuid;
begin
  Result := StringToGuid(Strings[idx]);
end;

function TJSONArray.GetGUIDDefaulted(const idx: integer): TGuid;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetGUID(idx)
  else
    Result := TGUID.Empty;
end;

function TJSONArray.GetNumber(const idx: integer): Double;
begin
  VerifyType(FValues.Items[idx].ValueType, TJSONValueType.number);
  Result := FValues.Items[idx].NumberValue;
end;

function TJSONArray.GetNumberDefaulted(const idx: integer): Double;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetNumber(idx)
  else
    Result := 0;
end;

function TJSONArray.GetIntDate(const idx: Integer): TDateTime;
begin
  Result := IncSecond(EncodeDate(1970,1,1),GetInteger(idx));
end;

function TJSONArray.GetIntDateDefaulted(const idx: integer): TDateTime;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetIntDate(idx)
  else
    Result := 0;
end;

function TJSONArray.GetInteger(const idx: integer): Int64;
begin
  VerifyType(FValues.Items[idx].ValueType, TJSONValueType.number);
  Result := FValues.Items[idx].IntegerValue;
end;

function TJSONArray.GetIntegerDefaulted(const idx: integer): Int64;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetInteger(idx)
  else
    Result := 0;
end;

function TJSONArray.GetItem(const idx: integer): Variant;
begin
  EnsureSize(idx);
  Result := FValues.Items[idx].ToVariant;
end;

function TJSONArray.GetLocalDate(const idx: Integer): TDateTime;
var
  TempDate: TDateTime;
begin
  Result := 0.0;
  if TryISO8601ToDate(GetString(idx),TempDate,false) then
    Result := TempDate;
  //Result := ISO8601ToDate(GetString(idx),false);
end;

function TJSONArray.GetLocalDateDefaulted(const idx: integer): TDateTime;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetLocalDate(idx)
  else
    Result := 0;
end;

function TJSONArray.GetObject(const idx: integer): IJSONObject;
begin
  VerifyType(FValues.Items[idx].ValueType, TJSONValueType.&object);
  Result := FValues.Items[idx].ObjectValue;
end;

function TJSONArray.GetObjectDefaulted(const idx: integer): IJSONObject;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetObject(idx)
  else
    Result := JSON;
end;

function TJSONArray.GetOnChange: TChangeArrayHandler;
begin
  Result := FOnChangeHandler;
end;

function TJSONArray.GetRaw(const idx: integer): PMultiValue;
begin
  Result := FValues.Items[idx];
end;

function TJSONArray.GetString(const idx: integer): string;
begin
  VerifyType(FValues.Items[idx].ValueType, TJSONValueType.string);
  Result := JSONDecode(FValues.Items[idx].StringValue);
end;

function TJSONArray.GetStringDefaulted(const idx: integer): string;
begin
  if (idx >= 0) and (idx < Count) then
    Result := GetString(idx)
  else
    Result := '';
end;

function TJSONArray.GetType(const idx: integer): TJSONValueType;
begin
  Result := FValues.Items[idx].ValueType;
end;

function TJSONArray.GetValue(const idx: integer): PMultiValue;
begin
  Result := FValues[idx];
end;

function TJSONArray.IndexOf(const value: int64): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FValues.Count-1 do
  begin
    if (Types[i] = TJSONValueType.Number) and (FValues[i].IntegerValue = value) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TJSONArray.IndexOf(const value: boolean): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FValues.Count-1 do
  begin
    if (Types[i] = TJSONValueType.Boolean) and (
        (Value and (FValues[i].IntegerValue = 1)) or
       ((not Value) and (FValues[i].IntegerValue = 0))) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TJSONArray.IndexOf(const value: string): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FValues.Count-1 do
  begin
    if (Types[i] = TJSONValueType.String) and (FValues[i].StringValue = value) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TJSONArray.IndexOf(const value: double): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FValues.Count-1 do
  begin
    if (Types[i] = TJSONValueType.Number) and (FValues[i].NumberValue = value) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TJSONArray.IndexOf(const value: IJSONArray): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FValues.Count-1 do
  begin
    if (Types[i] = TJSONValueType.Array) and (Arrays[i].Equals(value)) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TJSONArray.IndexOf(const value: IJSONObject): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FValues.Count-1 do
  begin
    if (Types[i] = TJSONValueType.Object) and (Objects[i].Equals(value)) then
    begin
      result := i;
      break;
    end;
  end;
end;

function TJSONArray.LoadFromStream(idx: integer; Stream: TStream;
  Encode: boolean): IJSONObject;
var
  data : TArray<Byte>;
  d : Double;
begin
  if not Encode then
    case Types[idx] of
      TJSONValueType.&string:
      begin
        SetLength(data, Stream.Size - Stream.Position);
        Stream.Read(data, Stream.Size - Stream.Position);
        Strings[idx] := TEncoding.UTF8.GetString(data);
      end;
      TJSONValueType.number:
        begin
          Stream.Read(d, SizeOf(Double));
          Numbers[idx] := d;
        end;
      TJSONValueType.&array:
        begin
          Raw[idx]^.ArrayValue.LoadFromStream(Stream, Encode);
        end;
      TJSONValueType.&object:
      begin
        SetLength(data, Stream.Size - Stream.Position);
        Stream.Read(data, Stream.Size - Stream.Position);
        Raw[idx]^.ObjectValue := JSON(TEncoding.UTF8.GetString(data));
      end;
      TJSONValueType.boolean:
        begin
          SetLength(data,1);
          Stream.Read(data,1);
          Booleans[idx] := data[0] <> 0;
        end
      else
        exit; // skip null, code
    end
  else
  begin
    SetLength(data, Stream.Size - Stream.Position);
    Stream.Read(data, Stream.Size - Stream.Position);
    Bytes[idx] := data;
  end;
end;

function TJSONArray.LoadFromStream(Stream: TStream;
  Encode: boolean): IJSONObject;
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
  begin
    LoadFromStream(i, Stream, Encode);
  end;
end;

procedure TJSONArray.Merge(const &Array: IJSONArray);
var
  i : integer;
begin
  for i := 0 to &Array.Count-1 do
  begin
    FValues.Add(&Array.Values[i]);
  end;
end;

{function TJSONArray.ParentArray: IJSONArray;
begin
  Result := FParentArray;
end;

function TJSONArray.ParentObject: IJSONObject;
begin
  Result := FParentObject;
end;

procedure TJSONArray.ParentOverride(parent: IJSONArray);
begin
  FParentObject := nil;
  FParentArray := parent;
end;

procedure TJSONArray.ParentOverride(parent: IJSONObject);
begin
  FParentObject := parent;
  FParentArray := nil;
end;}

procedure TJSONArray.Remove(const value: int64);
var
  i : integer;
begin
  for i := FValues.Count-1 downto 0 do
  begin
    if (FValues[i].ValueType = TJSONValueType.number) and (FValues[i].IntegerValue = value) then
      FValues.Delete(i);
  end;
end;

procedure TJSONArray.Remove(const value: boolean);
var
  i : integer;
  iBool : Int64;
begin
  if value then
    iBool := 0
  else
    iBool := 1;

  for i := FValues.Count-1 downto 0 do
  begin
    if (FValues[i].ValueType = TJSONValueType.boolean) and (FValues[i].IntegerValue = iBool) then
      FValues.Delete(i);
  end;
end;

procedure TJSONArray.Remove(const value: string);
var
  i : integer;
  sEncoded : string;
begin
  sEncoded := JSONEncode(value);
  for i := FValues.Count-1 downto 0 do
  begin
    if (FValues[i].ValueType = TJSONValueType.string) and (FValues[i].StringValue = sEncoded) then
      FValues.Delete(i);
  end;
end;

procedure TJSONArray.Remove(const value: double);
var
  i : integer;
begin
  for i := FValues.Count-1 downto 0 do
  begin
    if (FValues[i].ValueType = TJSONValueType.number) and (FValues[i].NumberValue = value) then
      FValues.Delete(i);
  end;
end;

procedure TJSONArray.Remove(const value: Variant);
var
  i, j : integer;
  bRemove : boolean;
  jso : IJSONObject;
  jsa : IJSONArray;
begin
  for i := FValues.Count-1 downto 0 do
  begin
    if VarIsType(Value,varUnknown) then
    begin
      if Supports(Value, IJSONObject, jso) then
      begin
        bRemove := FValues[i].ObjectValue.AsJSON = jso.AsJSON;
      end else if Supports(Value, IJSONArray, jsa) then
      begin
        bRemove := FValues[i].ArrayValue.AsJSON = jsa.AsJSON;
      end else
        raise EChimeraJSONException.Create('Unknown variant type.');
    end else
      case VarType(Value) of
        varSmallInt,
        varInteger,
        varSingle,
        varDouble,
        varCurrency,
        varShortInt,
        varByte,
        varWord,
        varLongWord,
        varInt64,
        varUInt64:
        begin
          bRemove := FValues[i].IntegerValue = VarAsType(Value,varDouble);
        end;

        varDate:
        begin
          bRemove := FValues[i].StringValue = DateToStr(Value,TFormatSettings.Create('en-us'))
        end;

        varBoolean:
        begin
          if VarAsType(Value,varBoolean) then
            bRemove := FValues[i].IntegerValue = 1
          else
            bRemove := FValues[i].IntegerValue = 0;
        end;

        varOleStr,
        varString,
        varUString:
        begin
          bRemove := FValues[i].StringValue = JSONEncode(VarAsType(Value,varString));
        end;

        varArray:
        begin
          jsa := TJSONArray.Create;
          for j := VarArrayLowBound(Value,1) to VarArrayHighBound(Value,1) do
            jsa[j] := Value[j];
          bRemove := FValues[i].ArrayValue.AsJSON = jsa.AsJSON;
        end;

        else
          raise EChimeraJSONException.Create('Unknown variant type.');
      end;
      if bRemove then
        FValues.Delete(i);
  end;
end;

procedure TJSONArray.Remove(const value: PMultiValue);
var
  i : integer;
  bRemove : boolean;
begin
  for i := FValues.Count-1 downto 0 do
  begin
    bRemove := false;
    if (FValues[i].ValueType = value.ValueType) then
    begin
      case FValues[i].ValueType of
        TJSONValueType.string   : bRemove := FValues[i].StringValue = value.StringValue;
        TJSONValueType.number   : bRemove := (FValues[i].NumberValue = value.NumberValue) and (FValues[i].IntegerValue = value.IntegerValue);
        TJSONValueType.&array   : bRemove := FValues[i].ArrayValue.AsJSON = value.ArrayValue.AsJSON;
        TJSONValueType.&object  : bRemove := FValues[i].ObjectValue.AsJSON = value.ObjectValue.AsJSON;
        TJSONValueType.boolean  : bRemove := FValues[i].IntegerValue = value.IntegerValue;
        TJSONValueType.null     : bRemove := true;
        TJSONValueType.code     : bRemove := FValues[i].StringValue = value.StringValue;
      end;
    end;
    if bRemove then
      FValues.Delete(i);
  end;
end;

procedure TJSONArray.Remove(const value: IJSONArray);
var
  i : integer;
begin
  for i := FValues.Count-1 downto 0 do
  begin
    if FValues[i].ArrayValue.AsJSON = value.AsJSON then
      FValues.Delete(i);
  end;
end;

procedure TJSONArray.Remove(const value: IJSONObject);
var
  i : integer;
begin
  for i := FValues.Count-1 downto 0 do
  begin
    if FValues[i].ObjectValue.AsJSON = value.AsJSON then
      FValues.Delete(i);
  end;
end;

procedure TJSONArray.Delete(const idx: Integer);
begin
  Dispose(FValues[idx]);
  FValues.Delete(idx);
end;

procedure TJSONArray.SaveToStream(Stream: TStream; Decode: boolean);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
  begin
    SaveToStream(i, Stream, Decode);
  end;
end;

procedure TJSONArray.SaveToStream(idx: integer; Stream: TStream;
  Decode: boolean);
var
  data : TArray<Byte>;
  d : Double;
begin
  if not Decode then
    case Types[idx] of
      TJSONValueType.&string:
        data := TEncoding.UTF8.GetBytes(Strings[idx]);
      TJSONValueType.number:
        begin
          SetLength(Data, SizeOf(Double));
          d := Raw[idx]^.NumberValue;
          Stream.WriteData(@d, SizeOf(Double));
          exit;
        end;
      TJSONValueType.&array:
        begin
          Raw[idx]^.ArrayValue.SaveToStream(Stream, Decode);
          exit;
        end;
      TJSONValueType.&object:
        data := TEncoding.UTF8.GetBytes(Objects[idx].AsJSON);
      TJSONValueType.boolean:
        begin
          SetLength(data,1);
          if Booleans[idx] then
            data[0] := 1
          else
            data[0] := 0;
        end
      else
        exit; // skip null, code
    end
  else
    data := TNetEncoding.Base64.Decode(TEncoding.UTF8.GetBytes(Strings[idx]));

  Stream.Write(data, length(data));
end;

procedure TJSONArray.SetArray(const idx: integer; const Value: IJSONArray);
var
  pmv : PMultiValue;
begin
  EnsureSize(idx);
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
  //Value.ParentOverride(Self);
  DoChangeNotify;
end;

procedure TJSONArray.SetBoolean(const idx: integer; const Value: Boolean);
var
  pmv : PMultiValue;
begin
  EnsureSize(idx);
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
  DoChangeNotify;
end;

procedure TJSONArray.SetBytes(const idx: integer; const Value: TArray<Byte>);
begin
  Strings[idx] := TEncoding.UTF8.GetString(TNetEncoding.Base64.Encode(Value));
end;

procedure TJSONArray.SetCount(const Value: integer);
begin
  EnsureSize(Value);
  while FValues.Count > Value do
    FValues.Delete(FValues.Count-1);
  DoChangeNotify;
end;

procedure TJSONArray.SetDate(const idx: Integer; const Value: TDateTime);
begin
  SetString(idx, DateToISO8601(Value));
end;

procedure TJSONArray.SetGuid(const idx: integer; const Value: TGUID);
begin
  Strings[idx] := GuidToString(Value);
end;

procedure TJSONArray.SetNumber(const idx: integer; const Value: Double);
var
  pmv : PMultiValue;
begin
  EnsureSize(idx);
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
  DoChangeNotify;
end;

procedure TJSONArray.SetIntDate(const idx: Integer; const Value: TDateTime);
begin
  SetInteger(idx, SecondsBetween(EncodeDate(1970,1,1),Value));
end;

procedure TJSONArray.SetInteger(const idx: integer; const Value: Int64);
var
  pmv : PMultiValue;
begin
  EnsureSize(idx);
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
  DoChangeNotify;
end;

procedure TJSONArray.SetItem(const idx: integer; const Value: Variant);
begin
  EnsureSize(idx);
  FValues.Items[idx].Initialize(Value);
  DoChangeNotify;
end;

procedure TJSONArray.SetLocalDate(const idx: Integer; const Value: TDateTime);
begin
  if IsValidLocalDate(Value) then
    SetString(idx, DateToISO8601(Value,false))
  else
    SetString(idx, DateToISO8601(0,True));
end;

procedure TJSONArray.SetObject(const idx: integer; const Value: IJSONObject);
var
  pmv : PMultiValue;
begin
  EnsureSize(idx);
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
  //value.ParentOverride(Self);
  DoChangeNotify;
end;

procedure TJSONArray.SetOnChange(const Value: TChangeArrayHandler);
begin
  FOnChangeHandler := Value;
end;

procedure TJSONArray.SetRaw(const idx: integer; const Value: PMultiValue);
var
  pmv : PMultiValue;
begin
  EnsureSize(idx);
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
end;

procedure TJSONArray.SetString(const idx: integer; const Value: string);
var
  pmv : PMultiValue;
begin
  EnsureSize(idx);
  New(pmv);
  pmv.Initialize(JSONEncode(value));
  FValues.Add(pmv);
  DoChangeNotify;
end;

procedure TJSONArray.SetType(const idx: integer; const Value: TJSONValueType);
begin
  EnsureSize(idx);
  if Value <> FValues.Items[idx].ValueType then
  begin
    case Value of
      TJSONValueType.string:
        FValues.Items[idx].Initialize('');
      TJSONValueType.number:
        FValues.Items[idx].Initialize(0);
      TJSONValueType.array:
        FValues.Items[idx].Initialize(TJSONArray.Create{(Self)});
      TJSONValueType.object:
        FValues.Items[idx].Initialize(TJSONObject.Create{(Self)});
      TJSONValueType.boolean:
        FValues.Items[idx].Initialize(False);
      TJSONValueType.null:
        FValues.Items[idx].InitializeNull;
    end;
    DoChangeNotify;
  end;
end;

procedure TJSONArray.Add(const value: PMultiValue);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(value);
  FValues.Add(pmv);
  DoChangeNotify;
end;

procedure TJSONArray.Add(const value: TArray<Byte>);
begin
  Add(TEncoding.UTF8.GetString(TNetEncoding.Base64.Encode(Value)));
end;

procedure TJSONArray.Each(proc: TProcConst<PMultiValue>);
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    proc(FValues[i]);
end;

{ TJSONObject }

procedure TJSONObject.Add(const name: string; const value: double);
begin
  Numbers[name] := value;
end;

procedure TJSONObject.Add(const name, value: string);
begin
  Strings[name] := value;
end;

procedure TJSONObject.Add(const name: string; const value: boolean);
begin
  Booleans[name] := value;
end;

procedure TJSONObject.Add(const name: string; const value: IJSONObject);
begin
  Objects[name] := value;
end;

procedure TJSONObject.Add(const name: string; const value: Int64);
begin
  Integers[name] := value;
end;

procedure TJSONObject.Add(const name: string; const value: IJSONArray);
begin
  Arrays[name] := value;
end;

procedure TJSONObject.AddNull(const name: string);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.InitializeNull;
  FValues.AddOrSetValue(Name, pmv);
  DoChangeNotify;
end;

procedure TJSONObject.AddCode(const name, value: string);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.InitializeCode(Value);
  FValues.AddOrSetValue(Name, pmv);
  DoChangeNotify;
end;

function TJSONObject.AsJSON(Whitespace : TWhitespace = TWhitespace.Standard): string;
var
  sb : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder;
begin
  sb := {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder.Create;
  try
    AsJSON(sb, Whitespace);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TJSONObject.AsJSON(var Result : string; Whitespace : TWhitespace = TWhitespace.Standard);
var
  item : TPair<string, PMultiValue>;
  bFirst : boolean;
begin
  Result := Result+'{';
  bFirst := True;
  for item in FValues do
  begin
    if not bFirst then
      Result := Result +WhiteChar(',', Whitespace)+'"'+item.Key+'"'+WhiteChar(':', Whitespace)
    else
      Result := Result+'"'+item.Key+'"'+WhiteChar(':', Whitespace);
    item.Value.AsJSON(Result);
    bFirst := False;
  end;
  Result := Result+'}';
end;

procedure TJSONObject.AsJSON(Result: {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder; Whitespace : TWhitespace = TWhitespace.Standard);
var
  item : TPair<string, PMultiValue>;
  bFirst : boolean;
begin
  Result.Append('{');
  bFirst := True;
  for item in FValues do
  begin
    if not bFirst then
      Result.Append(WhiteCharAfter(',', Whitespace)+'"'+item.Key+'"'+WhiteChar(':', Whitespace))
    else
      Result.Append('"'+item.Key+'"'+WhiteChar(':', Whitespace));
    item.Value.AsJSON(Result);
    bFirst := False;
  end;
  Result.Append('}');
end;


function TJSONObject.AsSHA1(Whitespace: TWhitespace): string;
var
  LSHA2: THashSHA1;
begin
  LSHA2 := THashSHA1.Create;
  LSHA2.Update(TEncoding.UTF8.GetBytes(AsJSON(Whitespace)));
  Result := LSHA2.HashAsString.ToUpper;
end;

procedure TJSONObject.BeginUpdates;
begin
 FUpdating := True;
end;

procedure TJSONObject.Clear;
var
  mv : TPair<string,PMultiValue>;
begin
  for mv in FValues do
    if mv.Value.ObjectValue <> nil then
      mv.Value.ObjectValue.OnChange := nil;

  FValues.Clear;
end;

constructor TJSONObject.Create;
begin
  inherited Create;
  FValues := TDictionary<string, PMultiValue>.Create;
  FValues.OnValueNotify := DisposeOfValue;
end;

function TJSONObject.CreateRTLObject: System.JSON.TJSONObject;
begin
  Result := System.JSON.TJSONObject(System.JSON.TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AsJSON),0));
end;

destructor TJSONObject.Destroy;
begin
  Clear;
  FValues.Free;
  inherited;
end;

procedure TJSONObject.DisposeOfValue(Sender: TObject; const Item: PMultiValue;
  Action: TCollectionNotification);
begin
  if Action = TCollectionNotification.cnRemoved then
    Dispose(Item);
end;

procedure TJSONObject.DoChangeNotify;
begin
  if Assigned(FOnChangeHandler) and (not FUpdating) then
    FOnChangeHandler(Self);
end;

procedure TJSONObject.Each(proc: TProcConst<string, int64>);
var
  item : TPair<string, PMultiValue>;
begin
  for item in FValues do
    proc(item.Key, item.Value.IntegerValue);
end;

procedure TJSONObject.Each(proc: TProcConst<string, boolean>);
var
  item : TPair<string, PMultiValue>;
begin
  for item in FValues do
    proc(item.Key, item.Value.IntegerValue <> 0);
end;

procedure TJSONObject.Each(proc: TProcConst<string, string>);
var
  item : TPair<string, PMultiValue>;
begin
  for item in FValues do
    proc(item.Key, JSONDecode(item.Value.StringValue));
end;

procedure TJSONObject.Each(proc: TProcConst<string, double>);
var
  item : TPair<string, PMultiValue>;
begin
  for item in FValues do
    proc(item.Key, item.Value.NumberValue);
end;

procedure TJSONObject.Each(proc: TProcConst<string, Variant>);
var
  item : TPair<string, PMultiValue>;
begin
  for item in FValues do
    proc(item.Key, item.Value.ToVariant);
end;

procedure TJSONObject.EndUpdates;
begin
  FUpdating := False;
  DoChangeNotify;
end;

procedure TJSONObject.Each(proc: TProcConst<string, PMultiValue>);
var
  item : TPair<string, PMultiValue>;
begin
  for item in FValues do
    proc(item.Key, item.Value);
end;

function TJSONObject.Equals(const obj: IJSONObject): boolean;
var
  item : TPair<string, PMultiValue>;
begin
  Result := True;
  for item in FValues do
  begin
    if (not obj.Has[item.Key]) or (obj.Types[item.key] <> item.Value.ValueType) then
    begin
      Result := False;
      exit;
    end;
    case item.Value.ValueType of
      TJSONValueType.string,
      TJSONValueType.number,
      TJSONValueType.boolean:
        Result := item.Value.ToVariant = obj[item.Key];
      TJSONValueType.array:
        Result := item.Value.ArrayValue.Equals(obj.Arrays[item.Key]);
      TJSONValueType.object:
        Result := item.Value.ObjectValue.Equals(obj.Objects[item.Key]);
      TJSONValueType.null:
        Result := True; // only one value option for this type so if equal above will match here.
      TJSONValueType.code:
        Result := item.Value.StringValue = obj.Raw[item.Key].StringValue;
    end;
    if not Result then
      exit;
  end;
end;

procedure TJSONObject.Each(proc: TProcConst<string, IJSONObject>);
var
  item : TPair<string, PMultiValue>;
begin
  for item in FValues do
    proc(item.Key, item.Value.ObjectValue);
end;

procedure TJSONObject.Each(proc: TProcConst<string, IJSONArray>);
var
  item : TPair<string, PMultiValue>;
begin
  for item in FValues do
    proc(item.Key, item.Value.ArrayValue);
end;

function TJSONObject.GetArray(const name: string): IJSONArray;
begin
  VerifyType(ValueOf[Name].ValueType, TJSONValueType.&array);
  Result := ValueOf[Name].ArrayValue;
end;

function TJSONObject.GetArrayDefaulted(const name: string): IJSONArray;
begin
  if Has[name] then
    Result := GetArray(name)
  else
    Result := JSONArray;
end;

function TJSONObject.GetBoolean(const name: string): Boolean;
begin
  VerifyType(ValueOf[Name].ValueType, TJSONValueType.boolean);
  Result := ValueOf[Name].IntegerValue <> 0;
end;

function TJSONObject.GetBooleanDefaulted(const name: string): Boolean;
begin
  if Has[name] then
    Result := GetBoolean(name)
  else
    Result := False;
end;

function TJSONObject.GetBytes(const name: string): TArray<Byte>;
begin
  Result := TNetEncoding.Base64.Decode(TEncoding.UTF8.GetBytes(Strings[name]));
end;

function TJSONObject.GetBytesDefaulted(const name: string): TArray<Byte>;
begin
  if Has[name] then
    Result := GetBytes(name)
  else
    SetLength(Result,0)
end;

function TJSONObject.GetCount: integer;
begin
  Result := FValues.Count;
end;

function TJSONObject.GetDate(const name: string): TDateTime;
var
  TempDate: TDateTime;
begin
  Result := 0.0;
  if TryISO8601ToDate(GetString(name),TempDate) then
    Result := TempDate;
  //Result := ISO8601ToDate(GetString(name));
end;

function TJSONObject.GetDateDefaulted(const name: string): TDateTime;
begin
  if Has[name] then
    Result := GetDate(name)
  else
    Result := 0;
end;

function TJSONObject.GetGuid(const name: string): TGuid;
begin
  Result := StringToGuid(Strings[name]);
end;

function TJSONObject.GetGuidDefaulted(const name: string): TGuid;
begin
  if Has[name] then
    Result := GetGuid(name)
  else
    Result := TGuid.Empty;
end;

function TJSONObject.GetHas(const name: string): boolean;
begin
  Result := FValues.ContainsKey(name);
end;

function TJSONObject.GetNumber(const name: string): Double;
begin
  VerifyType(ValueOf[Name].ValueType, TJSONValueType.number);
  Result := ValueOf[Name].NumberValue;
end;

function TJSONObject.GetNumberDefaulted(const name: string): Double;
begin
  if Has[name] then
    Result := GetNumber(name)
  else
    Result := 0;
end;

function TJSONObject.GetIntDate(const name: string): TDateTime;
begin
  Result := IncSecond(EncodeDate(1970,1,1),GetInteger(name));
end;

function TJSONObject.GetIntDateDefaulted(const name: string): TDateTime;
begin
  if Has[name] then
    Result := GetIntDate(name)
  else
    Result := 0;
end;

function TJSONObject.GetInteger(const name: string): Int64;
begin
  VerifyType(ValueOf[Name].ValueType, TJSONValueType.number);
  Result := ValueOf[Name].IntegerValue;
end;

function TJSONObject.GetIntegerDefaulted(const name: string): Int64;
begin
  if Has[name] then
    Result := GetInteger(name)
  else
    Result := 0;
end;

function TJSONObject.GetItem(const name: string): Variant;
begin
  Result := ValueOf[Name].ToVariant;
end;

function TJSONObject.GetLocalDate(const name: string): TDateTime;
var
  TempDate: TDateTime;
begin
  Result := 0.0;
  if TryISO8601ToDate(GetString(name),TempDate,false) then
    Result := TempDate;
//  Result := ISO8601ToDate(GetString(name),false);
end;

function TJSONObject.GetLocalDateDefaulted(const name: string): TDateTime;
begin
  if Has[name] then
    Result := GetLocalDate(name)
  else
    Result := 0;
end;

function TJSONObject.GetName(const idx: integer): string;
begin
  Result := FValues.Keys.ToArray[idx];
end;

function TJSONObject.GetObject(const name: string): IJSONObject;
begin
  if ValueOf[Name].ValueType <> TJSONValueType.null then
    VerifyType(ValueOf[Name].ValueType, TJSONValueType.&object);
  Result := ValueOf[Name].ObjectValue;
end;

function TJSONObject.GetObjectDefaulted(const name: string): IJSONObject;
begin
  if Has[name] then
    Result := GetObject(name)
  else
    Result := JSON
end;

function TJSONObject.GetOnChange: TChangeObjectHandler;
begin
  Result := FOnChangeHandler;
end;

function TJSONObject.GetRaw(const name: string): PMultiValue;
begin
  Result := FValues.Items[name];
end;

function TJSONObject.GetString(const name: string): string;
begin
  VerifyType(ValueOf[Name].ValueType, TJSONValueType.string);
  Result := JSONDecode(ValueOf[Name].StringValue);
end;

function TJSONObject.GetStringDefaulted(const name: string): string;
begin
  if Has[name] then
    Result := GetString(name)
  else
    Result := '';
end;

function TJSONObject.GetType(const name: string): TJSONValueType;
begin
  Result := ValueOf[Name].ValueType;
end;

function TJSONObject.GetValue(const name: string): PMultiValue;
begin
  Result := FValues[name];
end;

function TJSONObject.GetValueOf(const name: string): PMultiValue;
begin
  if FValues.ContainsKey(name) then
    result := FValues[name]
  else
    raise EChimeraJSONException.Create('Object is missing the "'+name+'" property.');

end;

function TJSONObject.LoadFromFile(const Filename: string) : IJSONObject;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

function TJSONObject.LoadFromFile(const Name, Filename: string;
  Encode: boolean): IJSONObject;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(Name, fs, Encode);
  finally
    fs.Free;
  end;
end;

function TJSONObject.LoadFromStream(const Name: String; Stream: TStream;
  Encode: boolean): IJSONObject;
var
  data : TArray<Byte>;
  d : Double;
begin
  if not Encode then
    case Types[Name] of
      TJSONValueType.&string:
      begin
        SetLength(data, Stream.Size - Stream.Position);
        Stream.Read(data, Stream.Size - Stream.Position);
        Strings[Name] := TEncoding.UTF8.GetString(data);
      end;
      TJSONValueType.number:
        begin
          Stream.Read(d, SizeOf(Double));
          Numbers[Name] := d;
        end;
      TJSONValueType.&array:
        begin
          Raw[Name]^.ArrayValue.LoadFromStream(Stream, Encode);
        end;
      TJSONValueType.&object:
      begin
        SetLength(data, Stream.Size - Stream.Position);
        Stream.Read(data, Stream.Size - Stream.Position);
        Raw[Name]^.ObjectValue := JSON(TEncoding.UTF8.GetString(data));
      end;
      TJSONValueType.boolean:
        begin
          SetLength(data,1);
          Stream.Read(data,1);
          Booleans[Name] := data[0] <> 0;
        end
      else
        exit; // skip null, code
    end
  else
  begin
    SetLength(data, Stream.Size - Stream.Position);
    Stream.Read(data, Stream.Size - Stream.Position);
    Bytes[Name] := data;
  end;
end;

function TJSONObject.LoadFromStream(Stream: TStream) : IJSONObject;
var
  ss : TStringStream;
begin
  ss := TStringStream.Create('', TEncoding.utf8);
  try
    ss.CopyFrom(Stream, Stream.Size-Stream.Position);
    Reload(ss.DataString);
  finally
    ss.Free;
  end;
  Result := Self;
end;


procedure TJSONObject.Merge(const &object: IJSONObject; OnDuplicate: TDuplicateHandler = nil);
begin
  &Object.Each(
    procedure(const Prop : string; const val : PMultiValue)
    var
      bOk : boolean;
    begin
      if Has[Prop] and Assigned(OnDuplicate) then
        bOK := OnDuplicate(Prop) = TDuplicateResolution.Overwrite
      else
        bOK := True;
      if bOK then
        case &Object.Types[Prop] of
          TJSONValueType.&string:
            Strings[Prop] := &Object.Strings[Prop];
          TJSONValueType.number:
            Numbers[Prop] := &Object.Numbers[Prop];
          TJSONValueType.&array:
            Arrays[Prop] := &Object.Arrays[Prop];
          TJSONValueType.&object:
            Objects[Prop] := &Object.Objects[Prop];
          TJSONValueType.boolean:
            Booleans[Prop] := &Object.Booleans[Prop];
          TJSONValueType.null:
          begin
            Remove(Prop);
            AddNull(Prop);
          end;
          TJSONValueType.code:
            Raw[Prop] := &Object.Raw[Prop];
        end;
    end
  );
end;

procedure TJSONObject.Reload(const Source: string);
begin
  Clear;
  TParser.ParseTo(Source, Self);
  DoChangeNotify;
end;

procedure TJSONObject.Remove(const name: string);
begin
  FValues.Remove(name);
end;

function TJSONObject.SameAs(CompareTo: IJSONObject): boolean;
begin
  Result := AsSHA1 = CompareTo.AsSHA1;
end;

procedure TJSONObject.SaveToFile(const Filename: string; Whitespace : TWhitespace = TWhitespace.Standard);
var
  fs : TFileStream;
begin
  if FileExists(Filename) then
    fs := TFileStream.Create(Filename, fmOpenWrite or fmShareDenyWrite)
  else
    fs := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
  fs.Size := 0;
  try
    SaveToStream(fs, Whitespace);
  finally
    fs.Free;
  end;
end;

procedure TJSONObject.SaveToFile(const Name, Filename: string; Decode : boolean);
var
  fs : TFileStream;
begin
  if FileExists(Filename) then
    fs := TFileStream.Create(Filename, fmOpenWrite or fmShareDenyWrite)
  else
    fs := TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
  fs.Size := 0;
  try
    SaveToStream(Name, fs, Decode);
  finally
    fs.Free;
  end;
end;

procedure TJSONObject.SaveToStream(const Name: string; Stream: TStream; Decode : boolean);
var
  data : TArray<Byte>;
  d : Double;
begin
  if not Decode then
    case Types[name] of
      TJSONValueType.&string:
        data := TEncoding.UTF8.GetBytes(Strings[name]);
      TJSONValueType.number:
        begin
          SetLength(Data, SizeOf(Double));
          d := Raw[Name]^.NumberValue;
          Stream.WriteData(@d, SizeOf(Double));
          exit;
        end;
      TJSONValueType.&array:
        begin
          Raw[Name]^.ArrayValue.SaveToStream(Stream, Decode);
          exit;
        end;
      TJSONValueType.&object:
        data := TEncoding.UTF8.GetBytes(Objects[name].AsJSON);
      TJSONValueType.boolean:
        begin
          SetLength(data,1);
          if Booleans[name] then
            data[0] := 1
          else
            data[0] := 0;
        end
      else
        exit; // skip null, code
    end
  else
    data := TNetEncoding.Base64.Decode(TEncoding.UTF8.GetBytes(Strings[name]));

  Stream.Write(data, length(data));
end;

procedure TJSONObject.SaveToStream(Stream: TStream; Whitespace : TWhitespace = TWhitespace.Standard);
var
  sb : {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder;
  bytes : TArray<Byte>;
begin
  sb := {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder.Create;
  try
    AsJSON(sb, Whitespace);
    bytes := TEncoding.UTF8.GetBytes(sb.ToString);
    Stream.Write(bytes,length(bytes));
  finally
    sb.Free;
  end;
end;

procedure TJSONObject.SetArray(const name: string; const Value: IJSONArray);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(Value);
  FValues.AddOrSetValue(Name, pmv);
  //value.ParentOverride(Self);
  DoChangeNotify;
end;

procedure TJSONObject.SetBoolean(const name: string; const Value: Boolean);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(Value);
  FValues.AddOrSetValue(Name, pmv);
  DoChangeNotify;
end;

procedure TJSONObject.SetBytes(const name: string; const Value: TArray<Byte>);
begin
  Strings[name] := TEncoding.UTF8.GetString(TNetEncoding.Base64.Encode(Value));
end;

procedure TJSONObject.SetDate(const name: string; const Value: TDateTime);
begin
  SetString(name, DateToISO8601(Value));
end;

procedure TJSONObject.SetGuid(const name: string; const Value: TGuid);
begin
  Strings[name] := GuidToString(Value);
end;

procedure TJSONObject.SetNumber(const name: string; const Value: Double);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(Value);
  FValues.AddOrSetValue(Name, pmv);
  DoChangeNotify;
end;

procedure TJSONObject.SetIntDate(const name: string; const Value: TDateTime);
begin
  SetInteger(name, SecondsBetween(EncodeDate(1970,1,1),Value));
end;

procedure TJSONObject.SetInteger(const name: string; const Value: Int64);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(Value);
  FValues.AddOrSetValue(Name, pmv);
  DoChangeNotify;
end;

procedure TJSONObject.SetItem(const name: string; const Value: Variant);
var
  pmv : PMultiValue;
begin
  if not FValues.ContainsKey(name) then
  begin
    New(pmv);
    pmv.Initialize(Value,true);
    FValues.AddOrSetValue(Name, pmv);
  end else
    ValueOf[Name].Initialize(Value, true);
  DoChangeNotify;
end;

procedure TJSONObject.SetLocalDate(const name: string; const Value: TDateTime);
begin
  if IsValidLocalDate(Value) then
    SetString(name, DateToISO8601(Value,false))
  else
    SetString(name, DateToISO8601(0,True));
end;

procedure TJSONObject.SetObject(const name: string; const Value: IJSONObject);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(Value);
  FValues.AddOrSetValue(Name, pmv);
  //value.ParentOverride(Self);
  DoChangeNotify;
end;

procedure TJSONObject.SetOnChange(const Value: TChangeObjectHandler);
begin
  FOnChangeHandler := Value;
end;

procedure TJSONObject.SetRaw(const name: string; const Value: PMultiValue);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(Value);
  FValues.AddOrSetValue(Name, pmv);
  DoChangeNotify;
end;

procedure TJSONObject.SetString(const name, Value: string);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(JSONEncode(Value));
  FValues.AddOrSetValue(Name, pmv);
  DoChangeNotify;
end;

procedure TJSONObject.SetType(const name: string; const Value: TJSONValueType);
var
  mv : PMultiValue;
begin
  mv := ValueOf[Name];

  if mv.ValueType <> Value then
  begin
    case Value of
      TJSONValueType.string:
        SetString(Name, '');
      TJSONValueType.number:
        SetNumber(Name, 0);
      TJSONValueType.array:
        SetArray(Name, TJSONArray.Create);
      TJSONValueType.object:
        SetObject(Name, TJSONObject.Create);
      TJSONValueType.boolean:
        SetBoolean(Name, False);
      TJSONValueType.null:
        AddNull(Name);
    end;
  end;
  DoChangeNotify;
end;

procedure TJSONObject.Add(const name: string; const value: PMultiValue);
begin
  Raw[name] := value;
end;

procedure TJSONObject.Add(const name: string; const value: Variant);
var
  pmv : PMultiValue;
begin
  New(pmv);
  pmv.Initialize(Value);
  FValues.AddOrSetValue(Name, pmv);
  DoChangeNotify;
end;

procedure TJSONObject.Add(const name: string; const value: TArray<Byte>);
begin
  Add(name, TEncoding.UTF8.GetString(TNetEncoding.Base64.Encode(Value)));
end;

{ TMultiValue }

constructor TMultiValue.Initialize(const Value: Double; encode : boolean = false);
begin
  Self.ValueType := TJSONValueType.number;
  Self.NumberValue := Value;
  Self.IntegerValue := Round(Value);
end;

constructor TMultiValue.Initialize(const Value: String; encode : boolean = false);
begin
  Self.ValueType := TJSONValueType.string;
  if encode then
    Self.StringValue := JSONEncode(Value)
  else
    Self.StringValue := Value;
end;

function TMultiValue.AsJSON: string;
begin
  Result := '';
  AsJSON(Result);
end;

procedure TMultiValue.AsJSON(var result : string);
begin
  case Self.ValueType of
    TJSONValueType.code:
      Result := Result+Self.StringValue;
    TJSONValueType.string:
    begin
      Result := Result+'"'+Self.StringValue+'"';
    end;
    TJSONValueType.number:
      if (Self.NumberValue = Round(Self.NumberValue)) and
         (Self.NumberValue <> Self.IntegerValue) then
        Result := Result+IntToStr(Self.IntegerValue)
      else
        Result := Result+FloatToStr(Self.NumberValue);
    TJSONValueType.array:
    begin
      if Assigned(Self.ArrayValue) then
        Self.ArrayValue.AsJSON(Result)
      else
        Result := Result+'null';

    end;
    TJSONValueType.object:
    begin
      if Assigned(Self.ObjectValue) then
        Self.ObjectValue.AsJSON(Result)
      else
        Result := Result+'null';
    end;
    TJSONValueType.boolean:
      if Self.IntegerValue = 1 then
        Result := Result+'true'
      else
        Result := Result+'false';
    TJSONValueType.null:
      Result := Result+'null';
  end;
end;

constructor TMultiValue.Initialize(const Value: IJSONArray; encode : boolean = false);
begin
  Self.ValueType := TJSONValueType.&array;
  Self.ArrayValue := Value;
end;

constructor TMultiValue.Initialize(const Value: Int64; encode : boolean = false);
begin
  Self.ValueType := TJSONValueType.number;
  Self.NumberValue := Value;
  Self.IntegerValue := Value;
end;

constructor TMultiValue.Initialize(const Value: IJSONObject; encode : boolean = false);
begin
  Self.ValueType := TJSONValueType.&object;
  Self.ObjectValue := Value;
end;

function TMultiValue.InitializeNull : TMultiValue;
begin
  Self.ValueType := TJSONValueType.&null;
  Self.ObjectValue := nil;
  Self.IntegerValue := 0;
  Self.StringValue := '';
  Result := Self;
end;

constructor TMultiValue.InitializeCode(const Value: String);
begin
  Self.ValueType := TJSONValueType.code;
  Self.StringValue := Value;
end;

function TMultiValue.ToVariant: Variant;
var
  jsNull : IJSONObject;
begin
  case ValueType of
    TJSONValueType.string:
      result := JSONDecode(StringValue);
    TJSONValueType.number:
      result := NumberValue;
    TJSONValueType.&array:
      result := ArrayValue;
    TJSONValueType.&object:
      result := ObjectValue;
    TJSONValueType.boolean:
      result := IntegerValue <> 0;
    TJSONValueType.null:
    begin
      jsNull := nil;
      result := jsNull;
    end;
  end;
end;

constructor TMultiValue.Initialize(const Value: Boolean; encode : boolean = false);
begin
  Self.ValueType := TJSONValueType.boolean;
  if Value then
    Self.IntegerValue := 1
  else
    Self.IntegerValue := 0;
end;

constructor TMultiValue.Initialize(const Value: Variant; encode : boolean = false);
var
  jso : IJSONObject;
  jsa : IJSONArray;
  d : Double;
  s : string;
  b : Boolean;
  i : integer;
begin
  if VarIsType(Value,varUnknown) then
  begin
    if Supports(Value, IJSONObject, jso) then
    begin
      Initialize(jso)
    end else if Supports(Value, IJSONArray, jsa) then
    begin
      Initialize(jsa);
    end else
      raise EChimeraJSONException.Create('Unknown variant type.');
  end else
    case VarType(Value) of
      varSmallInt,
      varInteger,
      varSingle,
      varDouble,
      varCurrency,
      varShortInt,
      varByte,
      varWord,
      varLongWord,
      varInt64,
      varUInt64:
      begin
        d := VarAsType(Value,varDouble);
        Initialize(d);
      end;

      varDate:
      begin
        Initialize(DateToStr(Value,TFormatSettings.Create('en-us')));
      end;

      varBoolean:
      begin
        b := VarAsType(Value,varBoolean);
        Initialize(b);
      end;

      varOleStr,
      varString,
      varUString:
      begin
        s := VarAsType(Value,varString);
        if encode then
          s := JSONencode(s);
        Initialize(s);
      end;

      varArray:
      begin
        jsa := TJSONArray.Create;
        for i := VarArrayLowBound(Value,1) to VarArrayHighBound(Value,1) do
          jsa[i] := Value[i];

        Initialize(jsa);
      end;

      else
        raise EChimeraJSONException.Create('Unknown variant type.');
    end;
end;

procedure TMultiValue.AsJSON(Result: {$IFDEF USEFASTCODE}FastStringBuilder.{$ENDIF}TStringBuilder);
begin
  case Self.ValueType of
    TJSONValueType.code:
      Result.Append(Self.StringValue);
    TJSONValueType.string:
    begin
      Result.Append('"'+Self.StringValue+'"');
    end;
    TJSONValueType.number:
      if (Self.NumberValue = Round(Self.NumberValue)) and
         (Self.NumberValue <> Self.IntegerValue) then
        Result.Append(IntToStr(Self.IntegerValue))
      else
        Result.Append(FloatToStr(Self.NumberValue));
    TJSONValueType.array:
    begin
      if Assigned(Self.ArrayValue) then
         Self.ArrayValue.AsJSON(Result)
      else
        Result.Append('null');
    end;
    TJSONValueType.object:
    begin
      if Assigned(Self.ObjectValue) then
       Self.ObjectValue.AsJSON(Result)
     else
      Result.Append('null');
    end;

    TJSONValueType.boolean:
      if Self.IntegerValue = 1 then
        Result.Append('true')
      else
        Result.Append('false');
    TJSONValueType.null:
      Result.Append('null');
  end;
end;

constructor TMultiValue.Initialize(const Value: PMultiValue);
begin
  Self.ValueType := Value.ValueType;
  Self.StringValue := Value.StringValue;
  Self.NumberValue := Value.NumberValue;
  Self.IntegerValue := Value.IntegerValue;
  if Self.ValueTYpe = TJSONValueType.Object then
    Self.ObjectValue := Value.ObjectValue;
  if Self.ValueTYpe = TJSONValueType.Array then
    Self.ArrayValue := Value.ArrayValue;
end;

end.
