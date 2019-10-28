unit chimera.utility;

interface

uses System.SysUtils, System.Classes, System.Types;

type
  TLibraryLoader = class(TObject)
  private
    FHandle : HModule;
  public
    constructor Create(const ModuleName : string);
    destructor Destroy; override;
    function GetAddress(const ExportName : string) : Pointer;
    property Handle : HModule read FHandle;
  end;

const
{$IFDEF MSWINDOWS}
  PLATFORM_LIBRARY_EXT = '.dll';
{$ELSEIF ANDROID}
  PLATFORM_LIBRARY_EXT = '.so';
{$ELSEIF LINUX}
  PLATFORM_LIBRARY_EXT = '.so';
{$ELSEIF MACOS}
  PLATFORM_LIBRARY_EXT = '.dynlib';
{$ELSEIF IOS}
  PLATFORM_LIBRARY_EXT = '.dynlib';
{$ENDIF}

  S_LIBRARY_NOT_FOUND = 'Library "%s" not found.';

implementation

{$IF Defined(MSWINDOWS)}
uses Winapi.Windows;
{$ELSEIF Defined(POSIX)}
uses Posix.Dlfcn;
{$ENDIF}

{ TDLLLoader }

constructor TLibraryLoader.Create(const ModuleName: string);
begin
  {$IF Defined(MSWINDOWS)}
    FHandle := LoadLibrary(PChar(ModuleName));
  {$ELSEIF Defined(POSIX)}
    FHandle := DLOpen(@(TEncoding.UTF8.GetBytes(ModuleName)[0]), RTLD_LOCAL or RTLD_LAZY);
  {$ENDIF}
  if FHandle = 0 then
    raise Exception.Create(Format(S_LIBRARY_NOT_FOUND,[ModuleName]));
  inherited Create;
end;

destructor TLibraryLoader.Destroy;
begin
  {$IF Defined(MSWINDOWS)}
    FreeLibrary(FHandle);
  {$ELSEIF Defined(POSIX)}
    DLClose(FHandle);
  {$ENDIF}
  inherited;
end;

function TLibraryLoader.GetAddress(const ExportName: string): Pointer;
begin
  {$IF Defined(MSWINDOWS)}
    Result := GetProcAddress(FHandle, PChar(ExportName));
  {$ELSEIF Defined(POSIX)}
    Result := DLSym(FHandle, @(TEncoding.UTF8.GetBytes(ExportName)[0]));
  {$ENDIF}
end;

end.
