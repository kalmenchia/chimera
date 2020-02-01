unit chimera.handlebars.helpers.default;

interface

uses
  System.SysUtils,
  System.Classes,
  chimera.handlebars;

implementation

initialization
  THandlebars.RegisterHelper('loud',
    function (const Name : string; Params : TArray<string>) : string
    var
      s: string;
    begin
      Result := '';
      for s in Params do
      begin
        Result := Result+s.ToUpper;
      end;
    end
  );

  THandlebars.RegisterHelper('log',
    function (const Name : string; Params : TArray<string>) : string
    begin
      Result := '';
      // TODO: implement logging
    end
  );

end.
