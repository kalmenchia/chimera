// *****************************************************************************
//
// chimera.bayeux.client;
//
// PubSub Chimera project for Delphi
//
// Copyright (c) 2014 by Sivv Corp, All Rights Reserved
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

unit chimera.bayeux.client;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections, chimera.json,
  System.Threading, chimera.utility.http, IdURI, IdCookieManager;

type
  TRetryMode = (retry, handshake, none);
  TMessageHandler = reference to procedure(const Msg : IJSONObject);
  TAuthenticateHandler = reference to procedure(var Username : String; var Password : string; var Authenticate : boolean);
  TBayeuxClient = class(TInterfacedObject)
  private const
    META_HANDSHAKE   = '/meta/handshake';
    META_CONNECT     = '/meta/connect';
    META_SUBSCRIBE   = '/meta/subscribe';
    META_UNSUBSCRIBE = '/meta/unsubscribe';
    META_DISCONNECT  = '/meta/disconnect';
  strict private
    FEndpoint: TIdURI;
    FClientID : string;
    FListener : TThread;
    FMessageID : Int64;
    FDispatcher : TDictionary<string, TMessageHandler>;
    FCookieManager : TIdCookieManager;
  private
    FRetryMode : TRetryMode;
    FVersion: string;
    FSupportedConnectionTypes: string;
    FTimeout: Int64;
    FAlternativeHosts: IJSONArray;
    FMultipleClients: Boolean;
    FOnAuthenticate: TAuthenticateHandler;
    FOnUnsuccessful : TMessageHandler;
    FExtension: IJSONObject;
    FInterval: Cardinal;
    FDeferConnect: Boolean;
    FOnHandshakeComplete: TProc;
    FOnLogMessage: TMessageHandler;
    FOnLogResponse: TMessageHandler;
    function Handshake(http : TIdHTTP) : boolean;
    function DoAuthenticate(var Username : string; var Password : string) : boolean;
    procedure SetupHTTP(http : TIdHTTP);
    procedure SynchronizeCookies(http : TIdHTTP);
    procedure ProcessResponseObject(const obj : IJSONObject);
    procedure DoHandshake;
  protected
    procedure StartListener(const OnReady : TProc = nil); virtual;
    function GenerateRandomID : string; virtual;
    function DoSendMessage(http : TIdHTTP; const Msg : IJSONObject) : IJSONObject; virtual;
    procedure SendMessage(const Msg : IJSONObject); virtual;
    function NextID : string; virtual;
  public
    constructor Create(const Endpoint : string; DeferConnect : boolean = false;
      const OnHandshakeComplete : TProc = nil; const OnLogMessage : TMessageHandler = nil;
      const OnLogResponse : TMessageHandler = nil);
    destructor Destroy; override;

    procedure Subscribe(const Channel : string; const OnMessage : TMessageHandler);
    procedure Unsubscribe(const Channel : string);
    procedure Publish(const Channel : string; const Msg : IJSONObject);
    property OnLogMessage : TMessageHandler read FOnLogMessage write FOnLogMessage;
    property OnLogResponse : TMessageHandler read FOnLogResponse write FOnLogResponse;
    property OnAuthenticate : TAuthenticateHandler read FOnAuthenticate write FOnAuthenticate;
    property OnUnsuccessful : TMessageHandler read FOnUnsuccessful write FOnUnsuccessful;
    property CookieManager : TIdCookieManager read FCookieManager;
    property ClientID : string read FClientID;
    property Extension : IJSONObject read FExtension;
    property Interval : Cardinal read FInterval write FInterval;
  end;

implementation

uses System.Hash, IdSSLOpenSSL, System.SyncObjs, chimera.utility;

type
  TListenerThread = class(TThread)
  strict private
    FOwner : TBayeuxClient;
    FOnReady : TProc;
  protected
    constructor Create(Owner : TBayeuxClient; OnReady : TProc);
    procedure OnChunkReceived(Sender : TObject; Chunk : TStream);
    procedure Execute; override;
  end;

{ TBayeuxClient }

constructor TBayeuxClient.Create(const Endpoint: string; DeferConnect : boolean = false;
      const OnHandshakeComplete : TProc = nil; const OnLogMessage : TMessageHandler = nil;
      const OnLogResponse : TMessageHandler = nil);
begin
  inherited Create;
  FMessageID := 0;
  FDispatcher := TDictionary<string, TMessageHandler>.Create;
  FEndpoint := TIdURI.Create(Endpoint);
  FDeferConnect := DeferConnect;
  FOnHandshakeComplete := OnHandshakeComplete;
  FOnLogMessage := OnLogMessage;
  FOnLogResponse := OnLogResponse;
  if not FDeferConnect then
    StartListener;
end;

destructor TBayeuxClient.Destroy;
begin
  if Assigned(FListener) then
  begin
    FListener.Terminate;
    Sleep(10);
    TerminateThread(FListener);
  end;

  FDispatcher.Free;
  FEndpoint.Free;
  inherited;
end;

function TBayeuxClient.DoAuthenticate(var Username, Password: string): boolean;
begin
  Result := Assigned(FOnAuthenticate);
  if Result then
    OnAuthenticate(Username, Password, Result);
end;

procedure TBayeuxClient.DoHandshake;
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      http : TIdHTTP;
    begin
      http := TIdHTTP.Create(nil);
      try
        SetupHTTP(http);

        Handshake(http);

        SynchronizeCookies(http);
      finally
        http.Free;
      end;
    end
  ).Start;
end;

function TBayeuxClient.DoSendMessage(http: TIdHTTP; const Msg: IJSONObject) : IJSONObject;
  function ProcessAsObject(ss : TStringStream) : IJSONObject;
  begin
    Result := JSON(ss.DataString);
    ProcessResponseObject(Result);
  end;
  function ProcessAsArray(ss : TStringStream) : IJSONObject;
  var
    ary : IJSONArray;
  begin
    ary := JSONArray(ss.DataString);
    if Ary.Count > 0 then
      result := Ary.Objects[0]
    else
      Result := nil;
    ary.Each(
      procedure(const obj : IJSONObject)
      begin
        ProcessResponseObject(obj);
      end
    );
  end;
var
  ssSource, ssResponse : TStringStream;
  c: Char;
  jsoError : IJSONObject;
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(msg);
  try
    ssSource := TStringStream.Create(msg.AsJSON, TEncoding.UTF8);
    ssResponse := TStringStream.Create('',TEncoding.UTF8);
    try
      http.post(FEndpoint.URI, ssSource, ssResponse);
      if (ssResponse.Size > 0) then
      begin
        c := ssResponse.DataString.Chars[0];
        case c of
          '[' : Result := ProcessAsArray(ssResponse);
          '{' : Result := ProcessAsObject(ssResponse);
        end;
      end else
        Result := nil;
    finally
      ssResponse.Free;
      ssSource.Free;
    end;
  except
    on e: exception do
    begin
      jsoError := JSON();
      jsoError.Booleans['successful'] := false;
      jsoError.Strings['error'] := 'Local Send Message Error: '+e.Message;
      ProcessResponseObject(jsoError);
      result := jsoError;
    end;
  end;
  if Assigned(FOnLogResponse) then
    FOnLogResponse(msg);
end;

function TBayeuxClient.GenerateRandomID: string;
var
  g : TGuid;
  sDate : string;
begin
  CreateGUID(g);
  DateTimeToString(sDate, 'yyyymmddhhnnsszzzz', Now);
  Result := THashSHA1.GetHMAC(GuidToString(g),sDate);
end;

function TBayeuxClient.Handshake(http: TIdHTTP) : boolean;
var
  jso : IJSONObject;
begin
  jso := JSON;
  jso.Strings['channel'] := META_HANDSHAKE;
  jso.Strings['version'] := '1.0';
  jso.Strings['minimumVersion'] := '1.0beta';
  jso.Arrays['supportedConnectionTypes'] := JSONArray();
  jso.Arrays['supportedConnectionTypes'].Add('long-polling');
  jso := DoSendMessage(http, jso);
  if jso <> nil then
    Result := jso.Booleans['successful']
  else
    Result := false;
  if Result then
  begin
    if Assigned(FOnHandshakeComplete) then
      FOnHandshakeComplete()
  end else
    FOnUnsuccessful(jso);
end;

function TBayeuxClient.NextID: string;
var
  id : UInt64;
begin
  id := TInterlocked.Increment(FMessageID);
  Result := id.ToString.PadLeft(32,'0');
end;

procedure TBayeuxClient.ProcessResponseObject(const obj: IJSONObject);
  procedure processAdvice(advice : IJSONObject);
  var
    s: string;
  begin
    if advice.Has['reconnect'] then
    begin
      s := advice.Strings['reconnect'];
      if s = 'retry' then
        FRetryMode := TRetryMode.retry
      else if s = 'handshake' then
        FRetryMode := TRetryMode.handshake
      else
        FRetryMode := TRetryMode.none;
    end;

    if advice.Has['timeout'] then
      FTimeout := advice.Integers['timeout'];

    if advice.Has['interval'] then
      FInterval := advice.Integers['interval'];

    if advice.Has['multiple-clients'] then
      FMultipleClients := advice.Booleans['multiple-clients'];

    if advice.Has['hosts'] then
      FAlternativeHosts := advice.Arrays['hosts'];
  end;
var
  sChannel : string;
begin
  if obj.Has['successful'] then
  begin
    if obj.Booleans['successful'] then
    begin
      sChannel := obj.Strings['channel'];
      if sChannel = META_HANDSHAKE then
      begin
        FVersion := obj.Strings['version'];
        FSupportedConnectionTypes := obj.Strings['supportedConnectionTypes'];
        FClientID := obj.Strings['clientId'];
        if obj.Has['advice'] then
          processAdvice(obj.Objects['advice']);
      end else if sChannel = META_CONNECT then
      begin
        if obj.Has['advice'] then
          processAdvice(obj.Objects['advice']);
      end else if sChannel = META_SUBSCRIBE then
      begin
        // Do Nothing
      end else if sChannel = META_UNSUBSCRIBE then
      begin
        TThread.Queue(TThread.Current,
          procedure
          begin
            FDispatcher.Remove(obj.Strings['subscription']);
          end
        );
      end else if sChannel = META_DISCONNECT then
      begin
        // Do Nothing
      end;
    end else if sChannel = META_SUBSCRIBE then
    begin
      TThread.Queue(TThread.Current,
        procedure
        begin
          FDispatcher.Remove(obj.Strings['subscription']);
        end
      );
      if Assigned(FOnUnsuccessful) then
        FOnUnsuccessful(obj);
    end else if Assigned(FOnUnsuccessful) then
      FOnUnsuccessful(obj);
  end else
  begin
    if obj.Has['channel'] and obj.Has['data'] then
    begin
      TThread.Queue(TThread.Current,
        procedure
        begin
          FDispatcher[obj.Strings['channel']](obj.Objects['data']);
        end
      );
    end;
  end;
end;

procedure TBayeuxClient.Publish(const Channel: string; const Msg: IJSONObject);
var
  jso: IJSONObject;
begin
  jso := JSON;
  jso.Strings['channel'] := Channel;
  jso.Objects['data'] := Msg;
  if FClientID <> '' then
    jso.Strings['clientId'] := FClientID;
  jso.Strings['id'] := NextID;
  if Assigned(FExtension)  then
    jso.Objects['ext'] := FExtension;
  SendMessage(jso);
end;

procedure TBayeuxClient.SendMessage(const Msg: IJSONObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      http : TIdHTTP;
    begin
      http := TIdHTTP.Create(nil);
      try
        SetupHTTP(http);

        DoSendMessage(http, Msg);

        SynchronizeCookies(http);
      finally
        http.Free;
      end;
    end
  ).Start;
end;

procedure TBayeuxClient.SetupHTTP(http: TIdHTTP);
var
  sUser, sPass : String;
begin
  http.AllowCookies := True;
  http.CookieManager := TIdCookieManager.Create(http);
  http.HandleRedirects := True;
  http.ProtocolVersion := TIdHTTPProtocolVersion.pv1_1;
  http.Request.ContentType := 'application/json';
  if DoAuthenticate(sUser, sPass) then
  begin
    http.Request.Username := sUser;
    http.Request.Password := sPass;
  end;
  http.Request.UserAgent := 'Chimera Bayeux Client';
  http.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(http);
  TThread.Synchronize(TThread.Current,
    procedure
    begin
      http.CookieManager.AddCookies(FCookieManager);
    end
  );

end;

procedure TBayeuxClient.StartListener(const OnReady : TProc = nil);
begin
  if not Assigned(FListener) then
  begin
    FListener := TListenerThread.Create(Self, OnReady);
  end else if Assigned(OnReady) then
    OnReady();
end;

procedure TBayeuxClient.Subscribe(const Channel: string;
  const OnMessage: TMessageHandler);
begin
  FDispatcher.AddOrSetValue(Channel, OnMessage);
  StartListener(
    procedure
    var
      jso : IJSONObject;
    begin
      jso := JSON;
      jso.Strings['clientId'] := FClientID;
      jso.Strings['channel'] := META_SUBSCRIBE;
      jso.Strings['subscription'] := Channel;
      if Assigned(FExtension) then
        jso.Objects['ext'] := FExtension;

      SendMessage(jso);
    end
  );
end;

procedure TBayeuxClient.SynchronizeCookies(http: TIdHTTP);
begin
{  TThread.Synchronize(TThread.Current,
    procedure
    begin
      FCookieManager.AddCookies(http.CookieManager);
    end
  );}
end;

procedure TBayeuxClient.Unsubscribe(const Channel: string);
begin
  StartListener(
    procedure
    var
      jso : IJSONObject;
    begin
      jso := JSON;
      jso.Strings['clientId'] := FClientID;
      jso.Strings['channel'] := META_UNSUBSCRIBE;
      jso.Strings['subscription'] := Channel;
      if Assigned(FExtension) then
        jso.Objects['ext'] := FExtension;

      SendMessage(jso);
      FDispatcher.Remove(Channel);
    end
  );
end;

{ TListenerThread }

constructor TListenerThread.Create(Owner : TBayeuxClient; OnReady : TProc) ;
begin
  inherited Create(False);
  FOwner := Owner;
  FreeOnTerminate := True;
  FOnReady := OnReady;
end;

procedure TListenerThread.Execute;
  function WaitInterval : boolean;
  var
    i : integer;
  begin
    Result := True;
    for i := FOwner.Interval*100 downto 0 do
    begin
      if Terminated then
      begin
        Result := False;
        exit;
      end;
      sleep(10);
    end;
  end;
  function Connect(http : TIdHTTP) : boolean;
  var
    jso : IJSONObject;
  begin
    jso := JSON;
    jso.Strings['channel'] := FOwner.META_CONNECT;
    jso.Strings['clientId'] := FOwner.ClientID;
    jso.Strings['connectionType'] := 'long-polling';
    if FOwner.Extension <> nil then
      jso.Objects['ext'] := FOwner.Extension;
    jso.Strings['id'] := FOwner.NextID;
    jso := FOwner.DoSendMessage(http, jso);
    if jso <> nil then
      Result := jso.Booleans['successful']
    else
      Result := true;
  end;
var
  http : TIdHTTP;
  sUser, sPass : string;
  ssSource, ssResponse : TStringStream;
begin
  http := TIdHTTP.Create(nil);
  try
    FOwner.SetupHTTP(http);
    http.OnChunkReceived := OnChunkReceived;
    if FOwner.Handshake(http) then
    begin
      http.Request.Connection := 'keep-alive';
      http.Request.TransferEncoding := 'chunked';
      repeat
        if Connect(http) then
        begin
          if Assigned(FOnReady) then
            FOnReady();
          FOnReady := nil;
        end;

        FOwner.SynchronizeCookies(http);

        if not WaitInterval then
          break;
      until (Terminated);

    end;
  finally
    http.Free;
  end;
end;

procedure TListenerThread.OnChunkReceived(Sender: TObject; Chunk: TStream);
begin
  FOwner.ProcessResponseObject(JSON().LoadFromStream(Chunk));
  Chunk.Size := 0;
end;

end.
