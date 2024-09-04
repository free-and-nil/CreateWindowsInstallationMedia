// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit CopyDataThreadU;

interface

uses
  Windows, Classes, SysUtils;

type
    TCopyDataThread = class (TThread)
      private
        FExitCode : DWord;
        sBatchFile, sParameters : String;

      protected
        procedure Execute; override;

      public
        constructor Create (const sBatchFile, sParameters: String;
                            const OnTerminateProc: TNotifyEvent);

        property ExitCode : DWord read FExitCode;
    end; { TCopyDataThread }

implementation

uses Win32ToolsU;

(* ---- *)

procedure TCopyDataThread.Execute;

var
    sCmdLine : String;

begin
(**
    sCmdLine := Format ('%s\cmd.exe /c "%s" %s',
                        [GetSystemDir, sBatchFile, sParameters]);
**)
    sCmdLine := Format ('"%s" %s', [sBatchFile, sParameters]);

    WinExecAndWait32 (sCmdLine, FExitCode);
end; { TCopyDataThread.Execute }

constructor TCopyDataThread.Create (const sBatchFile, sParameters: String;
                                    const OnTerminateProc: TNotifyEvent);
begin
    Assert (sBatchFile <> '');
    Assert (sParameters <> '');
    Assert (Assigned (OnTerminateProc));

    Self.sBatchFile := sBatchFile;
    Self.sParameters := sParameters;
    OnTerminate := OnTerminateProc;
    FreeOnTerminate := true;

    inherited Create (false);
end; { TCopyDataThread.Create }

end.

