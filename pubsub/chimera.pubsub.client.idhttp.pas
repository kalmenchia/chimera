// *****************************************************************************
//
// chimera.pubsub.client.idhttp;
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

unit chimera.pubsub.client.idhttp;

interface

uses System.SysUtils, System.Classes, IdHTTP, chimera.json,chimera.pubsub.interfaces,
  System.Generics.Collections {$IFDEF ANDROID},System.ByteStrings{$ENDIF};

type
  TDataEvent = procedure(Sender : TObject; const channel : IChannel<IJSONObject>; const Context : string; const Data : IJSONObject) of object;
  TMessageEvent = procedure(Sender : TObject; const Msg : IJSONObject) of object;

  TMessageHandler = reference to procedure(const Msg : IJSONObject);
  TSuccessHandler = reference to procedure(const channel : string; const Msg : IJSONObject);
  TPublishErrorHandler = reference to procedure(const channel : string; const Msg : IJSONObject; const E : Exception);
  TSubscribeErrorHandler = reference to procedure(const channel : string; const E : Exception; var Retry : boolean);

  TPubSubHTTPClient = class(TComponent)
  private
    FThreads : TDictionary<string, TThread>;
    FChannels: TStrings;
    FOnMessage: TMessageEvent;
    FPort: integer;
    FHost: string;
    FRootPath: string;
    FSynchronize: boolean;
    FOnClearMessage: TDataEvent;
    FOnStoreMessage: TDataEvent;
    procedure SetChannels(const Value: TStrings);
  protected
    procedure DoMessage(const msg : IJSONObject; handler : TMessageHandler); virtual;
    procedure DoMessages(const ary : IJSONArray; handler : TMessageHandler); virtual;
  public
    procedure Subscribe(const Channel : string); overload;
    procedure Subscribe(const Channel : string; const OnError : TSubscribeErrorHandler); overload;
    procedure Subscribe(const Channel : string; const OnMessage : TMessageHandler; const OnError : TSubscribeErrorHandler); overload;
    procedure Unsubscribe(const Channel : string);

    procedure Publish(const Channel : string; const Msg : IJSONObject; const OnSuccess : TSuccessHandler = nil; const OnError : TPublishErrorHandler = nil); overload;
    procedure Publish(const Channel : string; const SetupMsg : TMessageHandler; const OnSuccess : TSuccessHandler = nil; const OnError : TPublishErrorHandler = nil); overload;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Host : string read FHost write FHost;
    property Port : integer read FPort write FPort;
    property RootPath : string read FRootPath write FRootPath;
    property Channels : TStrings read FChannels write SetChannels;
    property OnMessage : TMessageEvent read FOnMessage write FOnMessage;
    property Syncronize : boolean read FSynchronize write FSynchronize default true;
  end;

procedure Register;

implementation

uses IdSSLOpenSSL, IdCookieManager;

procedure Register;
begin
  RegisterComponents('PubSub',[TPubSubHTTPClient]);
end;

{ TPubSubHTTPClient }

constructor TPubSubHTTPClient.Create(AOwner: TComponent);
begin
  inherited;
  FThreads := TDictionary<string, TThread>.Create;
  FChannels := TStringList.Create;
  FRootPath := '';
  FSynchronize := true;
end;

destructor TPubSubHTTPClient.Destroy;
var
  p: TPair<string, TThread>;
begin
  FChannels.Free;
  for p in FThreads do
  begin
    p.Value.Terminate;
  end;
  FThreads.Free;
  inherited;
end;

procedure TPubSubHTTPClient.DoMessage(const msg: IJSONObject; handler : TMessageHandler);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, msg);
  if Assigned(handler) then
    handler(msg);
end;

procedure TPubSubHTTPClient.DoMessages(const ary: IJSONArray; handler : TMessageHandler);
var
  i: Integer;
begin
  for i := 0 to ary.Count-1 do
  begin
    DoMessage(ary.Objects[i], handler);
  end;
end;

procedure TPubSubHTTPClient.Publish(const Channel: string;
  const SetupMsg: TMessageHandler; const OnSuccess: TSuccessHandler;
  const OnError: TPublishErrorHandler);
var
  msg : IJSONObject;
begin
  msg := JSON;
  SetupMsg(msg);
  publish(Channel, msg, OnSuccess, OnError);
end;

procedure TPubSubHTTPClient.Publish(const Channel: string;
  const Msg: IJSONObject; const OnSuccess: TSuccessHandler = nil;
  const OnError: TPublishErrorHandler = nil);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      http : TIdHTTP;
      sHost : string;
      ssPost : TStringStream;
    begin
      try
        http := TIdHTTP.Create(nil);
        try
          if FHost.ToLower.StartsWith('https://') or (FPort = 443) then
            http.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(http);
          http.AllowCookies := True;
          http.HandleRedirects := True;
          http.CookieManager := TIdCookieManager.Create(http);
          if FPort > 0 then
            sHost := FHost+':'+FPort.ToString+RootPath
          else
            sHost := FHost+RootPath;

          if not Channel.StartsWith('/') then
            sHost := sHost+'/';

          ssPost := TStringStream.Create(UTF8String(msg.AsJSON),TEncoding.UTF8);
          try
            http.Request.ContentType := 'application/json';
            http.Post(sHost+Channel, ssPost);
            if Assigned(OnSuccess) then
              OnSuccess(Channel, Msg);
          finally
            ssPost.Free;
          end;
        finally
          http.Free;
        end;
      except
        on E : Exception do
          if Assigned(OnError) then
            OnError(Channel, Msg, E);
      end;
    end
  ).Start;
end;

procedure TPubSubHTTPClient.SetChannels(const Value: TStrings);
begin
  FChannels.Assign(Value);
end;

procedure TPubSubHTTPClient.Subscribe(const Channel: string);
begin
  Subscribe(Channel,nil,nil);
end;

type
  TThreadHack = class(TThread);

procedure TPubSubHTTPClient.Subscribe(const Channel: string;
  const OnMessage: TMessageHandler; const OnError: TSubscribeErrorHandler);
var
  thread : TThread;
begin
  FChannels.Add(Channel);
  TMonitor.Enter(FThreads);
  try
    thread := TThread.CreateAnonymousThread(
      procedure
      var
        http : TIdHTTP;
        sHost : string;
        ssOut : TStringStream;
        jsa : IJSONArray;
        bRetry : boolean;
      begin
        http := TIdHTTP.Create(nil);
        try
          if FHost.ToLower.StartsWith('https://') or (FPort = 443) then
            http.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(http);
          http.AllowCookies := True;
          http.HandleRedirects := True;
          http.CookieManager := TIdCookieManager.Create(http);
          if FPort > 0 then
            sHost := FHost+':'+FPort.ToString+FRootPath
          else
            sHost := FHost+FRootPath;

          if not Channel.StartsWith('/') then
            sHost := sHost+'/';

          bRetry := True;

          while (not TThreadHack(TThread.CurrentThread).Terminated) and bRetry do
          begin
            try
              ssOut := TStringStream.Create;
              try
                http.Get(sHost+Channel,ssOut);
                if ssOut.DataString <> '' then
                begin
                  jsa := JSONArray(ssOut.DataString);
                  if FSynchronize then
                  begin
                    TThread.Synchronize(TThread.CurrentThread,
                      procedure
                      begin
                        DoMessages(jsa, OnMessage);
                      end
                    );
                  end else
                    DoMessages(jsa, OnMessage);
                end;

              finally
                ssOut.Free;
              end;
            except
              on e: exception do
                if Assigned(OnError) then
                  OnError(channel, E, bRetry);
            end;
          end;
        finally
          http.Free;
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
              FChannels.Delete(FChannels.IndexOf(Channel));
            end
          );
        end;
      end
    );
    FThreads.Add(Channel, thread);
    thread.Start;
  finally
    TMonitor.Exit(FThreads);
  end;
end;

procedure TPubSubHTTPClient.Subscribe(const Channel: string; const OnError : TSubscribeErrorHandler);
begin
  Subscribe(Channel,nil,OnError);
end;

procedure TPubSubHTTPClient.Unsubscribe(const Channel: string);
var
  thread : TThread;
begin
  TMonitor.Enter(FThreads);
  try
    if FThreads.TryGetValue(Channel, thread) then
    begin
      thread.Terminate;
      FThreads.ExtractPair(Channel);
    end;
  finally
    TMonitor.Exit(FThreads);
  end;
end;

end.
