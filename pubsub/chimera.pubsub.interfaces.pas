unit chimera.pubsub.interfaces;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections;

type
  TMessageHandler<T> = reference to procedure(const Msg : T);

  IChannel<T> = interface(IInterface)
      function GetName : string;

      procedure Subscribe(Handler : TMessageHandler<T>; const ID : string = '');
      procedure Unsubscribe(Handler : TMessageHandler<T>; const ID : string = '');
      procedure Publish(const Msg : T; const ID : string = '');
      procedure BeginContext(const Context : string; const ID : string = ''); overload;
      procedure BeginContext(const Context : string; const Prefill : TArray<T>; const ID : string = ''); overload;
      function BeginAndGetContext(const Context : string; const ID : string = '') : TQueue<T>; overload;
      function BeginAndGetContext(const Context : string; const Prefill : TArray<T>; const ID : string = '') : TQueue<T>; overload;
      function EndContext(const Context : string) : TArray<T>;
      function WaitOnContext(const Context : string; Timeout : integer; const ID : string = '') : TArray<T>;

      property Name : string read GetName;
  end;



implementation

end.
