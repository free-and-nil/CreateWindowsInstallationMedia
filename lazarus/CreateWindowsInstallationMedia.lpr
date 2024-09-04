// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I ..\switches.inc}

program CreateWindowsInstallationMedia;

{$mode objfpc}{$H+}

{$WARN UNIT_PLATFORM OFF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormU, DiskAndPartitionInfoU, MsgBoxU, Win32ToolsU, RegistryApiU,
  ShellApiEx, VCL_Tool, Win2000_ImportU, TaskDlgToolsU, TaskDlgU, VCL_Tools32U,
  Delphi32ToolsU, VerifyU, Delphi_T, PasTools, CopyDataThreadU
  { you can add units after this };

{$R *.res}

(* ---- *)

begin { CreateWindowsInstallationMedia }
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TCreateMediaForm, CreateMediaForm);
  Application.Run;
end.

