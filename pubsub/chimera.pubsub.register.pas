unit chimera.pubsub.register;

interface

uses System.SysUtils, System.Classes, chimera.pubsub.producer,
  chimera.pubsub.client.idhttp, chimera.pubsub.server.idhttp;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Chimera',[TPubSubProducer,TPubSubHTTPClient]);
end;

end.
