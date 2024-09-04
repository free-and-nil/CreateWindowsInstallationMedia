//
//  Original author: Olaf Hess
//  This work is published from: Germany.
//
//  To the extent possible under law, Olaf Hess has waived all copyright and
//  related or neighboring rights to this source code:
//  http://creativecommons.org/publicdomain/zero/1.0/
//
//  Unless expressly stated otherwise, the person who associated a work with
//  this deed makes no warranties about the work, and disclaims liability for
//  all uses of the work, to the fullest extent permitted by applicable law.
//

{$I ..\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

{$IFDEF DELPHI7_UP}
	{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

unit VCL_Tools32U;

interface

uses Windows, Controls, SysUtils, StdCtrls,
{$IFDEF FPC}
    MaskEdit,
{$ELSE}
    Mask,
{$ENDIF}
{$IFDEF DELPHI7_UP}
    ExtCtrls,
{$ENDIF}
	Forms;

{$IFNDEF FPC}
  {$IFDEF DELPHI6_UP}
procedure CD_Eject (chDrive : Char = #0);
{ CD aus dem Laufwerk auswerfen }
  {$ENDIF}
{$ENDIF}

function Check_IP (MaskEdit: TMaskEdit) : Boolean;

function CheckForEmptyText (const acControls: array of TWinControl) : Boolean;

procedure DisableWinControls (const aControls: array of TWinControl);
{$IFDEF SUPPORTS_OVERLOAD}
    overload;

procedure DisableWinControls (const Form: TCustomForm); overload;
{$ENDIF}

function EmptyEdit (const Edit: TCustomEdit;
					const ALabel:
{$IFDEF DELPHI6_UP}
                    TCustomLabel
{$ELSE}
					TLabel
{$ENDIF}
                    ) : Boolean;
{$IFDEF DELPHI7_UP}
	overload;

function EmptyEdit (const Edit: TLabeledEdit) : Boolean; overload;
{$ENDIF}

procedure EnableDisableControls (const aControls: array of TWinControl;
						         const bEnable: Boolean);
{$IFDEF SUPPORTS_OVERLOAD}
    overload;

procedure EnableDisableControls (const Form: TCustomForm;
								 const bEnable: Boolean); overload;
{$ENDIF}

procedure EnableWinControls (const aControls: array of TWinControl);
{$IFDEF SUPPORTS_OVERLOAD}
    overload;

procedure EnableWinControls (const Form: TCustomForm); overload;
{$ENDIF}

function ExecuteProcess (const applicationName, commandLine,
							   currentDirectory: String) : Boolean;

function IsForegroundWindow : Boolean;

function IsService : Boolean;

procedure WaitExecute (const sEXE: String;
                       const iVisibility: Integer = sw_Show);
{$IFDEF SUPPORTS_OVERLOAD}
    overload;

procedure WaitExecute (const sEXE: String; out dwExitCode: DWord;
                       const iVisibility: Integer = sw_Show);
	overload;
{$ENDIF}

implementation

uses Messages, Graphics, Dialogs,
{$IFDEF DELPHI6_UP}
  {$IFNDEF FPC}
	 MPlayer,
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI_XE3_UP}
	 System.UITypes,
{$ENDIF}
	 Delphi32ToolsU, IP_ToolsU, PasTools, VCL_Tool
    {$IFNDEF FPC}, Delphi_T {$ENDIF};

{$IFDEF DELPHI4_UP}
ResourceString
	cCheckForEmptyTextMsg = 'Please enter a text first!';
	cEmptyEditMsg = 'Please enter a "%s" first!';
	cInvalid_IP_Msg = 'IP not correct!';
{$ELSE}
	cCheckForEmptyTextMsg = 'Please enter a text first!';
	cEmptyEditMsg = 'Please enter a "%s" first!';
	cInvalid_IP_Msg = 'IP not correct!';
{$ENDIF}

(* ---- *)

{$IFNDEF FPC}
  {$IFDEF DELPHI6_UP}
procedure CD_Eject (chDrive : Char = #0);
{ CD aus dem Laufwerk auswerfen }

var
	MediaPlayer : TMediaPlayer;

begin
	if (chDrive = #0) then
		chDrive := GetCD_DriveLetter;

	if (chDrive <> #0) then
	begin
		chDrive := UpCase (chDrive);

		if (FilesExist (chDrive)) then
		begin
			MediaPlayer := TMediaPlayer.Create (Application.MainForm);

			with MediaPlayer do
				try
					Parent := Application.MainForm;

					Visible := false;
					DeviceType := dtCDAudio;
					Enabled := true;
					EnabledButtons := [btEject];

					Application.ProcessMessages;

					Open;
					Eject;
					Close;

					Sleep (500);

				finally
					Free;
				end; { try / finally }
		end; { if }
	end; { if }
end; { CD_Eject }
  {$ENDIF}
{$ENDIF}

(* ---- *)

function Check_IP (MaskEdit: TMaskEdit) : Boolean;
{ Überprüft, ob die IP-Adresse im Control "MaskEdit" gültig ist.
  ->> MaskEdit : Control, in dem die IP-Adresse steht.
  <<- Result : TRUE, wenn OK; sonst FALSE. }

var
    iErrorPos : integer;

begin
    with MaskEdit do
        if not (Valid_IP (Text, iErrorPos)) then
        begin
        	MessageDlg (cInvalid_IP_Msg, mtError, [mbOK], 0);

            Result := false;

            AutoSelect := false;
            SetFocus;
            AutoSelect := true;

            // SetCursor funktioniert nicht
            SendMessage (Handle, em_SetSel, iErrorPos, iErrorPos);
        end { if }
        else Result := true;
end; { Check_IP }

(* ---- *)

function CheckForEmptyText (const acControls: array of TWinControl) : Boolean;

var
	iIndex : Integer;
    sText : String;

begin
    Result := true;

    for iIndex := 0 to High (acControls) do
    begin
    	if (acControls [iIndex] is TCustomEdit) then
			sText := (acControls [iIndex] as TCustomEdit).Text
        else if (acControls [iIndex] is TMemo) then
			sText := (acControls [iIndex] as TMemo).Lines.Text
        else if (acControls [iIndex] is TComboBox) then
			sText := (acControls [iIndex] as TComboBox).Text
        else if (acControls [iIndex] is TMaskEdit) then
			sText := (acControls [iIndex] as TMaskEdit).Text
        else sText := '';

        if (Trim (sText) = '') then
        begin
            MessageDlg (cCheckForEmptyTextMsg, mtError, [mbOK], 0);
            acControls [iIndex].SetFocus;
			exit;
        end; { if }
    end; { for }

    Result := false;
end; { CheckForEmptyText }

(* ---- *)

procedure DisableWinControls (const aControls: array of TWinControl);
begin
    EnableDisableControls (aControls, false);
end; { DisableWinControls }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
procedure DisableWinControls (const Form: TCustomForm);
begin
	EnableDisableControls (Form, false);
end; { DisableWinControls }
{$ENDIF}

(* ---- *)

function EmptyEdit (const Edit: TCustomEdit;
					const ALabel:
{$IFDEF DELPHI6_UP}
                    TCustomLabel
{$ELSE}
					TLabel
{$ENDIF}
                    ) : Boolean;

var
	sLabel : String;

begin
	Result := Edit.Text = '';

    if (Result) then
        if (ALabel <> NIL) and (ALabel.Caption <> '') then
        begin
            sLabel := RemoveChars (ALabel.Caption, ['&', ':']);
            MessageDlg (Format (cEmptyEditMsg, [sLabel]), mtError, [mbOK], 0);
            Edit.SetFocus;
        end; { if }
end; { EmptyEdit }

(* ---- *)

{$IFDEF DELPHI6_UP}
function EmptyEdit (const Edit: TLabeledEdit) : Boolean;
begin
	Result := EmptyEdit (Edit, Edit.EditLabel);
end; { EmptyEdit }
{$ENDIF}

(* ---- *)

procedure EnableDisableControls (const aControls: array of TWinControl;
						         const bEnable: Boolean);

var
	iIndex : Integer;

begin
	if (Length (aControls) < 1) then
    	exit;

    for iIndex := 0 to High (aControls) do
{$IFDEF DELPHI7_UP}
        if (aControls [iIndex] is TCustomLabeledEdit) then
            EnableEdit (aControls [iIndex] as TLabeledEdit, bEnable)
        else
{$ENDIF}
        if (aControls [iIndex] is TCustomEdit) then
			EnableEdit (aControls [iIndex] as TEdit, bEnable)
		else aControls [iIndex].Enabled := bEnable;
end; { EnableDisableControls }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
procedure EnableDisableControls (const Form: TCustomForm;
								 const bEnable: Boolean);

var
	iIndex : Integer;
    Control : TControl;

begin
	for iIndex := 0 to Form.ControlCount - 1 do
    begin
    	Control := Form.Controls [iIndex];

        if (Control is TCustomStaticText) or
		   (Control is TCustomEdit) or
		   (Control is TButtonControl) or
		   (Control is TCustomComboBox) or
		   (Control is TCustomListBox) or
		   (Control is TCustomLabel) then
        begin
        	if (bEnable = false) and (Control.Enabled = false) then
            	Continue;

            if (bEnable = true) and (Control.Tag = 0) then
            	Continue;

            Control.Tag := iif (bEnable, 0, 1);

    {$IFDEF DELPHI7_UP}
            if (Control is TLabeledEdit) then
            	EnableEdit (Control as TLabeledEdit, bEnable)
            else
    {$ENDIF}
            if (Control is TEdit) then
            	EnableEdit (Control as TEdit, bEnable)
            else Control.Enabled := bEnable;
        end; { if }
    end; { for }
end; { EnableDisableControls }
{$ENDIF}

(* ---- *)

procedure EnableWinControls (const aControls: array of TWinControl);
begin
    EnableDisableControls (aControls, true);
end; { EnableWinControls }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
procedure EnableWinControls (const Form: TCustomForm);
begin
	EnableDisableControls (Form, true);
end; { EnableWinControls }
{$ENDIF}

(* ---- *)

function ExecuteProcess (const applicationName, commandLine,
							   currentDirectory: String) : Boolean;

var
  startupInfo: TStartupInfo;
  processInformation: TProcessInformation;
  createdOK: Boolean;

begin
  { Fill with known state }
  FillChar(startupInfo{%H-}, SizeOf(TStartupInfo), #0);
  FillChar(processInformation{%H-}, SizeOf(TProcessInformation), #0);
  startupInfo.cb := SizeOf(TStartupInfo);

  createdOK := CreateProcess(nil, PChar(applicationName + ' ' + commandLine),
	nil, nil, False, CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS,
	nil, PChar(currentDirectory), startupInfo, processInformation);

  { Check to see if successful and wait for completion if so }
  if createdOK then
  begin
	while WaitForSingleObject(processInformation.hProcess, 10) = WAIT_TIMEOUT do
	  Application.ProcessMessages;
	CloseHandle(processInformation.hProcess);
  end;

  Result := createdOK;
end; { ExecuteProcess }

(* ---- *)

function IsForegroundWindow : Boolean;
begin
    // https://stackoverflow.com/questions/3712551/how-can-i-check-if-my-application-has-focus
    Result := FindControl (GetForegroundWindow) <> NIL
end; { IsForegroundWindow }

(* ---- *)

function IsService : Boolean;
begin
    Result := false;

    if (Assigned (Application)) then
        Result := not (Application is TApplication);
end; { IsService }

(* ---- *)

procedure WaitExecute (const sEXE: String;
                       const iVisibility: Integer = sw_Show);

var
    dwExitCode : DWord;

begin
    WaitExecute (sEXE, dwExitCode, iVisibility);
end; { WaitExecute }

(* ---- *)

procedure WaitExecute (const sEXE: String; out dwExitCode: DWord;
                       const iVisibility: Integer = sw_Show);

var
    StartupInfo : TStartupInfo;
    ProcessInfo : TProcessInformation;
    dwRet : DWord;

begin
    FillChar (StartupInfo{%H-}, SizeOf (StartupInfo), #0);
    FillChar (ProcessInfo{%H-}, SizeOf (ProcessInfo), #0);
    StartupInfo.CB := SizeOf (StartupInfo);
    StartupInfo.wShowWindow := iVisibility;

    Win32Check (CreateProcess (NIL, PChar (sEXE), NIL, NIL, False,
                               NORMAL_PRIORITY_CLASS, NIL, NIL, StartupInfo,
                               ProcessInfo));

    repeat
        dwRet := MsgWaitForMultipleObjects (1, ProcessInfo.hProcess, false,
        								    INFINITE, QS_ALLINPUT);
        if (dwRet <> WAIT_OBJECT_0) then
            Application.ProcessMessages;
    until (dwRet = WAIT_OBJECT_0);

    GetExitCodeProcess (ProcessInfo.hProcess, dwExitCode{%H-});

    CloseHandle (ProcessInfo.hThread);
    CloseHandle (ProcessInfo.hProcess);
end; { WaitExecute }

(* ---- *)

end.
