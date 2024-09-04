{$I ..\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit ShellApiEx;

{$MINENUMSIZE 4}

interface

uses Windows, Messages;

const
    NIF_INFO = $10;
    NIF_MESSAGE = 1;
    NIF_ICON = 2;
    NOTIFYICON_VERSION = 3;
    NIF_TIP = 4;
    NIM_SETVERSION = $00000004;
    NIM_SETFOCUS = $00000003;
    NIIF_INFO = $00000001;
    NIIF_WARNING = $00000002;
    NIIF_ERROR = $00000003;

    NIN_BALLOONSHOW = WM_USER + 2;
    NIN_BALLOONHIDE = WM_USER + 3;
    NIN_BALLOONTIMEOUT = WM_USER + 4;
    NIN_BALLOONUSERCLICK = WM_USER + 5;
    NIN_SELECT = WM_USER + 0;
    NINF_KEY = $1;
    NIN_KEYSELECT = NIN_SELECT or NINF_KEY;

type
    TDummyUnionName = record
      case Integer of
        0 : (uTimeout: UInt);
        1 : (uVersion: UInt);
    end; { TDummyUnionName }

    PTNotifyIconDataEx = ^TNotifyIconDataEx;
    TNotifyIconDataEx = record
      cbSize : DWord;
      Wnd : HWND;
      uID : UInt;
      uFlags : UInt;
      uCallbackMessage : UInt;
      hIcon : HICON;
      // Version 5.0 is 128 chars, old ver is 64 chars
      szTip : array [0..127] of Char;
      dwState : DWord; //Version 5.0
      dwStateMask : DWord; //Version 5.0
      szInfo : array [0..255] of Char; //Version 5.0
      DummyUnionName : TDummyUnionName;
      szInfoTitle : array [0..63] of Char; //Version 5.0
      dwInfoFlags : DWord;   //Version 5.0
    end; { TNotifyIconDataEx }

function RunElevated (const sCmdLine, sParams: String; var dwExitCode: DWord;
					  const sCurrentDirectory: String = '';
                      const iVisibility: Integer = sw_Show;
                      const bWait: Boolean = true) : Boolean; overload;

procedure RunElevated (const sCmdLine, sParams: String;
					   const sCurrentDirectory: String = '';
                       const iVisibility: Integer = sw_Show;
                       const bWait: Boolean = true); overload;

implementation

uses SysUtils, ShellApi,
     VerifyU;

(* ---- *)

// https://stackoverflow.com/questions/11586139/how-to-run-application-which-requires-admin-rights-from-one-that-doesnt-have-th
// https://stackoverflow.com/questions/17638674/how-to-wait-for-shellexecute-to-run
function RunElevated (const sCmdLine, sParams: String; var dwExitCode: DWord;
  					  const sCurrentDirectory: String = '';
                      const iVisibility: Integer = sw_Show;
                      const bWait: Boolean = true) : Boolean;

var
    SEI : TShellExecuteInfo;

begin
    Assert (sCmdLine <> '');

    FillChar (SEI{%H-}, SizeOf (TShellExecuteInfo), #0);

    SEI.cbSize := SizeOf (TShellExecuteInfo);

    SEI.fMask := SEE_MASK_NOCLOSEPROCESS;
    SEI.lpVerb := 'runas';
    SEI.lpFile := PChar (sCmdLine);

    if (sParams <> '') then
        SEI.lpParameters := PChar (sParams);

    if (sCurrentDirectory <> '') then
        SEI.lpDirectory := PChar (sCurrentDirectory);

    SEI.nShow := iVisibility;
    SEI.hInstApp := 0;

    if (ShellExecuteEx (@SEI)) then
    begin
        Result := true;

        if (bWait) then
        begin
            WaitForSingleObject (SEI.hProcess, INFINITE);
            VerifyApi (GetExitCodeProcess (SEI.hProcess, dwExitCode{%H-}));
            VerifyApi (CloseHandle (SEI.hProcess));
        end { if }
        else dwExitCode := 0;
    end { if }
    else Result := false;
end; { RunElevated }

(* ---- *)

procedure RunElevated (const sCmdLine, sParams: String;
					   const sCurrentDirectory: String = '';
                       const iVisibility: Integer = sw_Show;
                       const bWait: Boolean = true); overload;

var
    dwExitCode : DWord;

begin
    if not (RunElevated (sCmdLine, sParams, dwExitCode{%H-}, sCurrentDirectory,
                         iVisibility, bWait)) then
        RaiseLastOSError;
end; { RunElevated }

(* ---- *)

end.

