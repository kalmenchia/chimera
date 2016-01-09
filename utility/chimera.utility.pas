unit chimera.utility;

interface

uses System.SysUtils, System.Classes;

procedure TerminateThread(Thread : TThread);

implementation

{$IF Defined(MSWINDOWS)}
uses Winapi.Windows;
{$ELSEIF Defined(POSIX)}
uses Posix.Pthread;
{$ENDIF POSIX}

procedure TerminateThread(Thread : TThread);
begin
{$IF Defined(MSWINDOWS)}
  Winapi.Windows.TerminateThread(Thread.Handle, 0);
{$ELSEIF Defined(POSIX)}
  pthread_cancel(Thread.ThreadID);
{$ELSE}
  No Implementation At this time!
{$ENDIF POSIX}

end;

end.
