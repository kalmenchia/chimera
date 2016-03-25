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
  System.Threading, System.Net.HTTPClient, System.Net.URLClient, System.SyncObjs;

type
  TRetryMode = (retry, handshake, none);
  TMessageHandler = reference to procedure(const Msg : IJSONObject);
  TStringHandler = reference to procedure(const Msg : string);
  TAuthenticateHandler = reference to procedure(var Username : String; var Password : string; var Authenticate : boolean);
  TBayeuxClient = class(TInterfacedObject)
  private const
    META_HANDSHAKE   = '/meta/handshake';
    META_CONNECT     = '/meta/connect';
    META_SUBSCRIBE   = '/meta/subscribe';
    META_UNSUBSCRIBE = '/meta/unsubscribe';
    META_DISCONNECT  = '/meta/disconnect';
  strict private
    FEndpoint: TURI;
    FClientIDCS : TMultiReadExclusiveWriteSynchronizer;
    FClientID : string;
    FListener : TThread;
    FMessageID : Int64;
    FDispatcher : TDictionary<string, TMessageHandler>;
    FCookieManager : TCookieManager;
    FRetryMode : TRetryMode;
    FVersion: string;
    FSupportedConnectionTypes: string;
    FRetry : Cardinal;
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
    FOnLogVerbose: TStringHandler;
    function DoAuthenticate(var Username : string; var Password : string) : boolean;
    procedure DoHandshake;
    procedure AuthCallback(const Sender: TObject; AnAuthTarget: TAuthTargetType;
      const ARealm, AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean;
      var Persistence: TAuthPersistenceType);
  private
    procedure SetupHTTP(http : THTTPClient);
    function Handshake(http : THTTPClient) : boolean;
    procedure SynchronizeCookies(http : THTTPClient);
    procedure ProcessResponseObject(const obj : IJSONObject);
    function GetClientID: string;

    procedure SetClientID(const Value: string);
    procedure DoLogVerbose(const Msg: string);
    procedure DoOnUnsuccessful(const obj: IJSONObject);  protected
    procedure StartListener(const OnReady : TProc = nil); virtual;
    function GenerateRandomID : string; virtual;
    function DoSendMessage(http : THTTPClient; const Msg : IJSONObject) : IJSONObject; virtual;
    procedure SendMessage(const Msg : IJSONObject); virtual;
    function NextID : string; virtual;
  public
    constructor Create(const Endpoint : string; DeferConnect : boolean = false;
      const OnHandshakeComplete : TProc = nil; const OnLogMessage : TMessageHandler = nil;
      const OnLogResponse : TMessageHandler = nil; const InitialClientID : string = '');
    destructor Destroy; override;

    procedure Subscribe(const Channel : string; const OnMessage : TMessageHandler);
    procedure Unsubscribe(const Channel : string);
    procedure Publish(const Channel : string; const Msg : IJSONObject);
    property OnLogMessage : TMessageHandler read FOnLogMessage write FOnLogMessage;
    property OnLogResponse : TMessageHandler read FOnLogResponse write FOnLogResponse;
    property OnAuthenticate : TAuthenticateHandler read FOnAuthenticate write FOnAuthenticate;
    property OnUnsuccessful : TMessageHandler read FOnUnsuccessful write FOnUnsuccessful;
    property CookieManager : TCookieManager read FCookieManager;
    property OnLogVerbose : TStringHandler read FOnLogVerbose write FOnLogVerbose;
    property Extension : IJSONObject read FExtension;
    property Interval : Cardinal read FInterval write FInterval;
    property Retry : Cardinal read FRetry write FRetry;
    property ClientID : string read GetClientID write SetClientID;
  end;

implementation

uses System.Hash;

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

procedure TBayeuxClient.AuthCallback(const Sender: TObject;
  AnAuthTarget: TAuthTargetType; const ARealm, AURL: string; var AUserName,
  APassword: string; var AbortAuth: Boolean;
  var Persistence: TAuthPersistenceType);
var
  sUser, sPass : String;
begin
  if DoAuthenticate(sUser, sPass) then
  begin
    AbortAuth := False;
    AUsername := sUser;
    APassword := sPass;
    Persistence := TAuthPersistenceType.Client;
  end;
end;

constructor TBayeuxClient.Create(const Endpoint: string; DeferConnect : boolean = false;
      const OnHandshakeComplete : TProc = nil; const OnLogMessage : TMessageHandler = nil;
      const OnLogResponse : TMessageHandler = nil; const InitialClientID : string  = '');
begin
  inherited Create;
  FClientIDCS := TMultiReadExclusiveWriteSynchronizer.Create;
  FCookieManager := TCookieManager.Create;
  FInterval := 0;
  FRetry := 5;
  FMessageID := 0;
  FDispatcher := TDictionary<string, TMessageHandler>.Create;
  FEndpoint := TURI.Create(Endpoint);
  FDeferConnect := DeferConnect;
  FOnHandshakeComplete := OnHandshakeComplete;
  FOnLogMessage := OnLogMessage;
  FOnLogResponse := OnLogResponse;
  FClientID := InitialClientID;
  if not FDeferConnect then
    StartListener;
end;

destructor TBayeuxClient.Destroy;
begin
  if Assigned(FListener) then
  begin
    FListener.Terminate;
    Sleep(500);
    FreeAndNil(FListener);
  end;

  FClientIDCS.Free;
  FDispatcher.Free;
  FCookieManager.Free;
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
      http : THTTPClient;
    begin
      http := THTTPClient.Create;
      try
        SetupHTTP(http);

        Handshake(http);

        SynchronizeCookies(http);
      finally
        if Assigned(http.CookieManager) then
          http.CookieManager.Free;
        http.Free;
      end;
    end
  ).Start;
end;

procedure TBayeuxClient.DoLogVerbose(const Msg : string);
begin
  if Assigned(FOnLogVerbose) then
    FOnLogVerbose('Invalid Response: ' + Msg);
end;

function TBayeuxClient.DoSendMessage(http: THTTPClient; const Msg: IJSONObject) : IJSONObject;
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
  function WaitforRetry: boolean;
  var
    i : integer;
  begin
    Result := True;
    for i := Retry*100 downto 0 do
    begin
      if TListenerThread(TThread.Current).Terminated then
      begin
        Result := False;
        Abort;
      end;
      sleep(10);
    end;
    if TListenerThread(TThread.Current).Terminated then
      Abort;
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
      try
        http.post(FEndpoint.ToString, ssSource, ssResponse);
      except
        on e: exception do
        begin
          DoLogVerbose('HTTP Error "'+e.Message+'" waiting for Retry.');
          ssSource.Size := 0;  // TODO: to keep incremental memory growth we might want to freeandnil this instead;
          ssResponse.Size := 0;
          try
            WaitForRetry;
          except
            on e: EAbort do
              exit;
          end;

          Result := DoSendMessage(http, Msg);
          exit;
        end;
      end;
      if (ssResponse.Size > 0) then
      begin
        c := ssResponse.DataString.Chars[0];
        case c of
          '[' : Result := ProcessAsArray(ssResponse);
          '{' : Result := ProcessAsObject(ssResponse);
          else
          begin
            result := nil;
            DoLogVerbose(ssResponse.Datastring);
          end;
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

function TBayeuxClient.GetClientID: string;
begin
  FClientIDCS.BeginRead;
  try
    Result := FClientID;
  finally
    FClientIDCS.EndRead;
  end;
end;

function TBayeuxClient.Handshake(http: THTTPClient) : boolean;
var
  jso : IJSONObject;
begin
  if ClientID <> '' then
  begin
    result := True;
    exit;
  end;

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
  begin
    raise Exception.Create('Invalid Response from Server.');

  end;
  if Result then
  begin
    DoLogVerbose('Handshake Complete');
    if Assigned(FOnHandshakeComplete) then
      FOnHandshakeComplete();
  end else
  begin
    DoLogVerbose('Handshake Failed');
    FOnUnsuccessful(jso);
  end;
end;

function TBayeuxClient.NextID: string;
var
  id : UInt64;
begin
  id := TInterlocked.Increment(FMessageID);
  Result := id.ToString.PadLeft(32,'0');
end;

procedure TBayeuxClient.DoOnUnsuccessful(const obj: IJSONObject);
var
  iErrorCode : integer;
begin
  if obj.Has['error'] and TryStrToInt(obj.Strings['error'].Substring(0,3),iErrorCode) then
  begin
        case iErrorCode of
          401:
          begin
            DoLogVerbose('Connection Error: Unknown Client ID, Trying another Handshake.');
            FClientID := '';
          end else
           DoLogVerbose(obj.Strings['error']);
        end;

      end;

  if Assigned(FOnUnsuccessful) then
    FOnUnsuccessful(obj);
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

    if advice.Has['retry'] then
      FRetry := advice.Integers['retry'];

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
        ClientID := obj.Strings['clientId'];
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

      DoOnUnsuccessful(obj);
   end else
     DoOnUnsuccessful(obj);
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
  if ClientID <> '' then
    jso.Strings['clientId'] := ClientID;
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
      http : THTTPClient;
    begin
      http := THTTPClient.Create;
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

procedure TBayeuxClient.SetClientID(const Value: string);
begin
  if FClientIDCS.BeginWrite then
    try
      FClientID := Value;
    finally
      FClientIDCS.EndWrite;
    end
  else
    raise Exception.Create('Could not set ClientID');
end;

procedure TBayeuxClient.SetupHTTP(http: THTTPClient);
begin
  http.AllowCookies := True;
  http.CookieManager := TCookieManager.Create;
  http.HandleRedirects := True;
  //http.ProtocolVersion := TIdHTTPProtocolVersion.pv1_1;
  http.ContentType := 'application/json';
  http.AuthEvent := Self.AuthCallback;
  http.UserAgent := 'Chimera Bayeux Client';
  TThread.Synchronize(TThread.Current,
    procedure
    var
      cookie : TCookie;
    begin
      for cookie in FCookieManager.Cookies do
        http.CookieManager.AddServerCookie(cookie.ToString, '.');
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
      jso.Strings['clientId'] := ClientID;
      jso.Strings['channel'] := META_SUBSCRIBE;
      jso.Strings['subscription'] := Channel;
      if Assigned(FExtension) then
        jso.Objects['ext'] := FExtension;

      SendMessage(jso);
    end
  );
end;

procedure TBayeuxClient.SynchronizeCookies(http: THTTPClient);
begin
  TThread.Synchronize(TThread.Current,
    procedure
    var
      cookie : TCookie;
    begin
      for cookie in http.CookieManager.Cookies do
      begin
        FCookieManager.AddServerCookie(cookie.ToString, '.');
      end;
    end
  );
end;

procedure TBayeuxClient.Unsubscribe(const Channel: string);
begin
  StartListener(
    procedure
    var
      jso : IJSONObject;
    begin
      jso := JSON;
      jso.Strings['clientId'] := ClientID;
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
  FreeOnTerminate := False;
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
        exit;
      end;
      sleep(10);
    end;
  end;
  function Connect(http : THTTPClient) : boolean;
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
    begin
      Result := jso.Booleans['successful'];
      if not Result and (FOwner.ClientID = '') then
        if FOwner.Handshake(http) then
        begin
          Result := Connect(http);
          if Result then
            exit;
        end;
    end else
    begin
      Result := false;
    end;
  end;
var
  http : THTTPClient;
  sUser, sPass : string;
begin
  http := THTTPClient.Create;
  try
    FOwner.SetupHTTP(http);
    //http.OnChunkReceived := OnChunkReceived;
    if FOwner.Handshake(http) then
    begin
      http.CustomHeaders['Keep-Alive'] := 'max';
      //http.CustomHeaders['Connection'] := 'close';
      //http.TransferEncoding := 'chunked';
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
    if assigned(http.CookieManager) then
      http.CookieManager.Free;
    http.Free;
  end;
end;

procedure TListenerThread.OnChunkReceived(Sender: TObject; Chunk: TStream);
begin
  FOwner.ProcessResponseObject(JSON().LoadFromStream(Chunk));
  Chunk.Size := 0;
end;

end.
