// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

{$WARN UNIT_PLATFORM OFF}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit MainFormU;

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls,
  DiskAndPartitionInfoU;

type

  { TCreateMediaForm }

  TCreateMediaForm = class (TForm)
    HardDrivesCheck: TCheckBox;
    CreateMediaBtn: TButton;
    StatusBar: TStatusBar;
    TargetMediaLbl: TLabel;
    MediaCombo: TComboBox;
    RefreshTimer: TTimer;
    procedure CreateMediaBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure HardDrivesCheckChange(Sender: TObject);
    procedure MediaComboCloseUp(Sender: TObject);
    procedure MediaComboSelect(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);

  private
    chWinPeDrive, chDataDrive : Char;
    bUseDsmDepot, bBusy, bDebug : Boolean;
    iMinimumMediaSize : Integer;
    DiskDrives : TDiskDrives;
    TargetDisk : TDiskDrive;
    sDriversPath, sWindowsSourcesPath, sWinPE_Path, sCopyDataScript,
        sTargetMedia, sDsmDepotPath, sTempPath, sProgramDataPath : String;

    function CheckAndGetNetInstPath (out sPath: String) : Boolean;
    function CheckForWritePermissions : Boolean;
    function CheckMediaSize : Boolean;
    function CheckSourceFolders : Boolean;
    function CheckTempFolder : Boolean;
    function ConfirmMediaErase : Boolean;
    function CreateBatchFile (out sParameters: String) : Boolean;
    function CreateProgramDataFolder : Boolean;
    procedure EnableControls (const bEnable: Boolean = true);
    procedure FillMediaCombo;
    function GetTargetDriveLetters : Boolean;
    function LoadValuesFromIni : Boolean;
    procedure OnCopyDataThreadTerminate (Sender: TObject);
    function PartitionThumbDrive : Boolean;
    procedure ResetRefreshTimer;
    procedure SetMediaComboHint;
    procedure SetStatusBarText (const sText: String;
                                const iPanel: Integer = 0);

  public

  end; { TCreateMediaForm }

var
	CreateMediaForm : TCreateMediaForm;

implementation

{$R *.lfm}

uses SysConst, JwaWinIoctl, Jwadbt, lazstringutils,
     CopyDataThreadU,
     MsgBoxU, Win32ToolsU, RegistryApiU, ShellApiEx, VCL_Tool, Win2000_ImportU,
     {%H-}TaskDlgToolsU, {%H-}TaskDlgU, VCL_Tools32U, Delphi_T, PasTools;

ResourceString
	cAdminActiveMsg =
        'This program should not be executed with local administrative rights';
    cConfigFileNotFoundMsg = 'Required configuration file "%s" not found!';
    cConfirmCopyOperationMsg =
        'More than 5 GB of data will be copied to the device "%s". ' +
        'This can take a very long time.' +
        #13#10#13#10'Continue?';
    cConfirmMediaEraseMainMsg = 'Initializing the device "%s"';
    cConfirmMediaEraseMsg = 'All data is going to get erased.';
    cConfirmMediaEraseCheckMsg = 'Y&es, erase all data on the device';
    cCopyFailureMsg =
        'Unable to copy all data to the device "%s". Return code = %d.';
//  cCopyFileErrMsg = 'Unable to copy'#13#10'"%s"'#13#10'to "%s":'#13#10'%s';
    cCopyDataSuccessMsg = 'Data successfully copied to the device "%s"';
    cCopyFileErrorMsg = 'Unable to create the file "%s": %s';
    cCopyingMsg = 'Copying data to "%s"';
    cCreateDirErrMsg = 'Unable to create the folder "%s":'#13#10'%s';
    cDeleteFileErrMsg = 'Unable to delete the file "%s":'#13#10'%s';
    cDiskPartErrMsg =
    	'Initializing the media "%s" failed ("DiskPart" exit code = %d)';
    cDiskPartExecutionErrMsg = 'Unable to start "DiskPart.exe":'#13#10'%s';
    cDiskPartPromptMainMsg =
    	'"DiskPart.exe" must be executed with highest privileges';
    cDiskPartPromptContentMsg = 'Please specify an account with local admin ' +
                                'rights. Press "OK" to continue.';
	cFillingMediaListMsg = 'Filling media list ...';
    cInitializingMediaMsg = 'Initializing "%s"';
    cIniValueNotFoundMsg = 'Unable to read the value "%s" from the INI file!';
    cMinSizeInvalidMsg = 'The minimum media size must be greater than 0!';
    cNetInstFolderErrMsg = 'Unable to access the DSM source path "%s"';
    cNetInstRegistryErrMsg = 'Unable to retrieve the DSM server path';
    cNoFreeDriveLetterMsg =
    	'Unable to find a free drive letter; unable to continue';
    cNoRemovableMediaFoundMsg = 'No removeable media found';
    cProgramBusyMsg = 'The program is currently performing an operation.'#13#10 +
        			  'Please wait for it to finish.';
    cSourceDrivers = 'driver';
    cSourceLabel = 'Source: ';
    cSourceLocal = 'local';
    cSourcesPathNotFoundMsg =
        'Path'#13#10'%s'#13#10'to the %s sources not found!';
    cSourceWindows = 'Windows';
    cSourceWinPE = 'Windows PE';
    cTargetMediaSizeErrMsg = 'The capacity of the drive "%s" is too small!'#13#10 +
      						 'The minimum size is %d GB.';
    cUndefinedPathMsg = '[Undefined]';
//	cUserAbortMsg = 'Operation aborted by user';
    cWriteProtectionErrMsg = 'Unable to write data to the target media.'#13#10 +
                             #13#10'Aborting program operation.';

const
	cAppName = 'CreateWindowsInstallationMedia';
    cCopyDataBatchFileName = 'CopyRecoveryData.bat';
	cDsmKey = 'SOFTWARE\NetSupport\NetInstall';
    cPartitionTargetDrive = 'PartitionTargetDrive';
    cPartitionTargetDriveBat = cPartitionTargetDrive + '.bat';
    cPartitionTargetDriveDiskPartScript = cPartitionTargetDrive + '.txt';
    cServerSourcePath = 'ServerSourcePath';

var
    OrgWndProc : WNDPROC = NIL;

(* ---- *)

function DeleteExistingFile (const sFileName: String) : Boolean;
begin
    if (FileExists (sFileName)) then
    begin
        Result := DeleteFile (sFileName);

        if not (Result) then
            MsgBox (cDeleteFileErrMsg, [sFileName, GetLastErrorMsg], mb_IconStop)
    end { if }
    else Result := true;
end; { DeleteExistingFile }

function NewWndProc (Handle: HWND; iMsg: UINT;
					 WParam_: WPARAM; LParam_: LPARAM) : LRESULT; stdcall;
begin
    if (iMsg = wm_DeviceChange) and (wParam_ = DBT_DEVNODES_CHANGED) then
    begin
        Result := LResult (true);
        CreateMediaForm.ResetRefreshTimer;
    end { if }
    else Result := CallWindowProc (OrgWndProc, Handle, iMsg, WParam_, LParam_);
end; { NewWndProc }

procedure TCreateMediaForm.FormCreate (Sender: TObject);
begin
    @OrgWndProc := {%H-}Pointer (GetWindowLongPtr (Handle, GWLP_WNDPROC));
    Win32Check (SetWindowLongPtr (Handle, GWLP_WNDPROC,
                                  {%H-}LONG_PTR (@NewWndProc)) <> 0);

    sTempPath := GetTempDir + cAppName + '\';
    sCopyDataScript := sTempPath + cCopyDataBatchFileName;
    sProgramDataPath := GetEnvironmentVar ('ProgramData') + '\' + cAppName;

{$IFDEF DEBUG}
    bDebug := true;
    Caption := Caption + ' [DEBUG]';
{$ENDIF}
end; { TCreateMediaForm.FormCreate }

procedure TCreateMediaForm.FormCloseQuery (Sender: TObject;
                     					   var CanClose: Boolean);
begin
	if (bBusy) then
    begin
        CanClose := false;
    	MsgBox (cProgramBusyMsg, mb_IconInformation);
    end; { if }
end; { TCreateMediaForm.FormCloseQuery }

procedure TCreateMediaForm.FormDestroy (Sender: TObject);
begin
    DiskDrives.Free;
end; { TCreateMediaForm.FormDestroy }

procedure TCreateMediaForm.FormPaint (Sender: TObject);

var
    bAbort : Boolean = true;

begin
    OnPaint := NIL;

    Application.ProcessMessages;

    if (IsUserAnAdmin) then
        MsgBox (cAdminActiveMsg, mb_IconStop)
    else
        if (CreateProgramDataFolder) then
            if (CheckTempFolder) then
                if (LoadValuesFromIni) then
                    if (bUseDsmDepot) then
                    begin
                        if (CheckAndGetNetInstPath (sDsmDepotPath)) and
                           (CheckSourceFolders) then
                            bAbort := false;
                    end { if }
                    else
                    begin
                        SetStatusBarText (cSourceLabel + cSourceLocal, 1);
                        bAbort := false;
                    end; { else }

    if (bAbort) then
        Application.Terminate
    else ResetRefreshTimer;
end; { TCreateMediaForm.FormPaint }

procedure TCreateMediaForm.HardDrivesCheckChange (Sender: TObject);
begin
    FillMediaCombo;
end; { TCreateMediaForm.HardDrivesCheckChange }

procedure TCreateMediaForm.MediaComboCloseUp (Sender: TObject);
begin
    with MediaCombo do
        if (ItemIndex <> (-1)) and
           (Items [ItemIndex] <> cNoRemovableMediaFoundMsg) then
        CreateMediaBtn.SetFocus;
end; { TCreateMediaForm.MediaComboCloseUp }

procedure TCreateMediaForm.MediaComboSelect (Sender: TObject);
begin
    if (MediaCombo.Items.Count > 0) then
        CreateMediaBtn.Enabled := true;

    if (MediaCombo.Items.Count = 1) then
        CreateMediaBtn.SetFocus;

    SetMediaComboHint;
end; { TCreateMediaForm.MediaComboSelect }

procedure TCreateMediaForm.RefreshTimerTimer (Sender: TObject);
begin
	RefreshTimer.Enabled := false;

	FillMediaCombo;
end; { TCreateMediaForm.RefreshTimerTimer }

procedure TCreateMediaForm.CreateMediaBtnClick (Sender: TObject);

var
    sBatchParameters : String = '';

begin
    with MediaCombo do
		TargetDisk := TDiskDrive (Items.Objects [ItemIndex]);

    sTargetMedia := TargetDisk.DisplayName;

    if (CheckMediaSize) then
        if (MsgBox (cConfirmCopyOperationMsg, [sTargetMedia],
                    mb_IconQuestion or mb_YesNo) = idYes) then
            if (GetTargetDriveLetters) then
    	        if (ConfirmMediaErase) then
                    if (CreateBatchFile (sBatchParameters)) then
                    begin
                        if (PartitionThumbDrive) and
                           (CheckForWritePermissions) then
                        begin
                            if (bDebug) then
                                if (MsgBox ('Copy files now?',
                                            mb_IconQuestion or
                                                        mb_YesNo) <> idYes) then
                                    exit;

                            TCopyDataThread.Create (sCopyDataScript,
                                                    sBatchParameters,
                                                    OnCopyDataThreadTerminate);
                            bBusy := true;
                            SetStatusBarText (Format (cCopyingMsg,
                            						  [sTargetMedia]));
                            Screen.Cursor := crHourGlass;
                        end; { if }
                    end; { if }
end; { TCreateMediaForm.CreateMediaBtnClick }

function TCreateMediaForm.CheckAndGetNetInstPath (out sPath: String) : Boolean;

var
    sServer : String;
    SourcePathList : TStringList;

begin
    Result := false;

	sPath := RegReadStr (HKey_Local_Machine, cDsmKey, cServerSourcePath);

    if (sPath <> '') then
    begin
        if (DirectoryExists (sPath)) then
        begin
            Result := true;
	        sPath := IncludeTrailingBackslash (sPath);

            SourcePathList := TStringList (SplitString (sPath, '\'));
            sServer := SourcePathList [0];
            SourcePathList.Free;

            if (Pos ('.', sServer) > 0) then
                SetLength (sServer, Pred (Pos ('.', sServer)));

            SetStatusBarText (cSourceLabel + sServer, 1);
        end { if }
        else MsgBox (cNetInstFolderErrMsg, [sPath], mb_IconStop);
    end { if }
    else
    begin
        SetStatusBarText (cSourceLabel + cUndefinedPathMsg, 1);
        MsgBox (cNetInstRegistryErrMsg, mb_IconStop);
    end; { else }
end; { TCreateMediaForm.CheckAndGetNetInstPath }

function TCreateMediaForm.CheckForWritePermissions : Boolean;

const
    cFileName = ':\test.dat';

var
    FS : TFileStream;
    sFileName : String;

begin
    Result := false;

    sFileName := chDataDrive + cFileName;

    try
        FS := TFileStream.Create (sFileName, fmCreate);

        try
            Result := true;

        finally
            FS.Free;
        end; { try / finally }

        DeleteExistingFile (sFileName);

    except
        on E:Exception do
            MsgBox (cWriteProtectionErrMsg,
                    mb_IconStop or mb_SystemModal);
    end; { try / finally }
end; { TCreateMediaForm.CheckForWritePermissions }

function TCreateMediaForm.CheckMediaSize : Boolean;

var
    iMinSize : Int64;

begin
    iMinSize := Trunc ((Int64 (iMinimumMediaSize) * cOne_GB) * 0.9);
	Result := TargetDisk.DiskSize >= iMinSize;

	if not (Result) then
     	MsgBox (cTargetMediaSizeErrMsg, [sTargetMedia, iMinimumMediaSize],
        		mb_IconStop);
end; { TCreateMediaForm.CheckMediaSize }

function TCreateMediaForm.CheckSourceFolders : Boolean;

	(* ---- *)

	function CheckPath (const sDescription, sPath: String) : Boolean;
    begin
    	Assert (sPath <> '');

		Result := DirectoryExists (sPath);

        if not (Result) then
        	MsgBox (cSourcesPathNotFoundMsg, [sPath, sDescription], mb_IconStop)
    end; { CheckPath }

    (* ---- *)

begin { TCreateMediaForm.CheckSourceFolders }
    if not (bUseDsmDepot) then
        Exit (true);

    if (CheckPath (cSourceWinPE, sDsmDepotPath + sWinPE_Path)) and
       (CheckPath (cSourceWindows, sDsmDepotPath + sWindowsSourcesPath)) and
       (CheckPath (cSourceDrivers, sDsmDepotPath + sDriversPath)) then
    begin
        Result := true;

        sWinPE_Path := sDsmDepotPath + sWinPE_Path;
        sWindowsSourcesPath := sDsmDepotPath + sWindowsSourcesPath;
        sDriversPath := sDsmDepotPath + sDriversPath;
    end { if }
    else Result := false;
end; { TCreateMediaForm.CheckSourceFolders }

function TCreateMediaForm.CheckTempFolder : Boolean;

    (* ---- *)

    function EraseFiles : Boolean;

    var
        FileList : TStringList;
        sFile : String;

    begin
        Result := true;

        FileList := TStringList.Create;

        try
            FillFileList (FileList, sTempPath + '*.*');

            for sFile in FileList do
                if (ExtractFileExt (LowerCase (sFile)) <> '.log') then
                    if not (DeleteExistingFile (sFile)) then
                    begin
                        Result := false;
                        Break;
                    end; { if }

        finally
            FileList.Free;
        end; { try / finally }
    end; { EraseFiles }

    (* ---- *)

begin { TCreateMediaForm.CheckTempFolder }
    if (DirectoryExists (sTempPath)) then
        Result := EraseFiles
    else
    begin
        Result := CreateDirectory (PChar (sTempPath), NIL);

        if not (Result) then
            MsgBox (cCreateDirErrMsg, [sTempPath, GetLastErrorMsg], mb_IconStop)
    end; { else }
end; { TCreateMediaForm.CheckTempFolder }

function TCreateMediaForm.ConfirmMediaErase : Boolean;

var
    bChecked : Boolean = false;

begin
    Result := TaskDlgCheckBox (Format (cConfirmMediaEraseMainMsg,
                                       [sTargetMedia]),
                               cConfirmMediaEraseMsg, tdtWarning,
                               [tcbOk, tcbCancel], cConfirmMediaEraseCheckMsg,
                               bChecked, tcbOk) = tdrOk;
end; { TCreateMediaForm.ConfirmMediaErase }

function TCreateMediaForm.CreateBatchFile (out sParameters: String) : Boolean;

    (* ---- *)

    function GetDateAndTime : String;

    var
        wYear, wMonth, wDay, wHour, wMin, wSec, wMSec : Word;

    begin
        DecodeDate (Now, wYear, wMonth, wDay);
        DecodeTime (Now, wHour, wMin, wSec, wMSec);

        Result := Format ('%.2d%.2d%.2d_%.2d%.2d%.2d',
                          [wYear, wMonth, wDay, wHour, wMin, wSec, wMSec]);
    end; { GetDateAndTime }

    (* ---- *)

    function FP (const sPath: String) : String;
    begin
        if (Pos (' ', sPath) > 0) then
            Result := '"' + sPath + '"'
        else Result := sPath;
    end; { FP }

    (* ---- *)

const
    cSourceFileName = cCopyDataBatchFileName + '.txt';

var
    sSourceFile : String;

begin { TCreateMediaForm.CreateBatchFile }
    sSourceFile := ExtractFilePath (Application.ExeName) + cSourceFileName;

    if not (FileExists (sSourceFile)) then
    begin
        MsgBox (cConfigFileNotFoundMsg, [sSourceFile], mb_IconStop);
        Exit (false);
    end; { if}

    Result := CopyFile (PChar (sSourceFile), PChar (sCopyDataScript), false);

    if (Result) then
        sParameters := Format ('%s %s %s %s %s %s' + iif (bDebug, ' DEBUG', ''),
                               [FP (sWinPE_Path), FP (sWindowsSourcesPath),
                                FP (sDriversPath), chWinPeDrive, chDataDrive,
                                GetDateAndTime])
    else MsgBox (cCopyFileErrorMsg, [sCopyDataScript, GetLastErrorMsg],
                 mb_IconStop);
end; { TCreateMediaForm.CreateBatchFile }

(* ---- *)

function TCreateMediaForm.CreateProgramDataFolder : Boolean;
// The DiskPart script file must be saved to a directory which is accessibly
// by all users with or without admin rights!
begin
    Result := DirectoryExists (sProgramDataPath);

    if not (Result) then
    begin
        Result := CreateDir (sProgramDataPath);

        if not (Result) then
            MsgBox (GetLastErrorMsg, mb_IconStop);
    end; { if }
end; { TCreateMediaForm.CreateProgramDataFolder }

(* ---- *)

procedure TCreateMediaForm.EnableControls (const bEnable: Boolean);
begin
	TargetMediaLbl.Enabled := bEnable;
    MediaCombo.Enabled := bEnable;
    CreateMediaBtn.Enabled := bEnable;
end; { TCreateMediaForm.EnableControls }

(* ---- *)

procedure TCreateMediaForm.FillMediaCombo;

    (* ---- *)

    function GetMediaDriveLetters (const Disk: TDiskDrive) : String;

    var
        iCount : Integer = 0;
        Partition : TPartition;

    begin
        Result := '';

        if (Disk.PartitionCount > 0) then
            for Partition in Disk do
                if (Assigned (Partition.LogicalDisk)) then
                    if (Partition.LogicalDisk.DriveLetter <> '') then
                    begin
                        if (iCount > 0) then
                            Result := Result + ', ' +
                                      Partition.LogicalDisk.DriveLetter
                        else Result := Partition.LogicalDisk.DriveLetter;

                        Inc (iCount);
                    end; { if }
    end; { GetMediaDriveLetters }

    (* ---- *)

    function SupportedDisk (const Disk: TDiskDrive) : Boolean;
    begin
        if (Disk.MediaType = RemovableMedia) then
            Result := true
        else if (Disk.MediaType = FixedMedia) and
                (HardDrivesCheck.Checked) and (Disk.BusType = btUsb) then
            Result := true
        else Result := false;
    end; { SupportedDisk }

    (* ---- *)

var
    Disk : TDiskDrive;
    sDriveLetters, sInfo : String;
    iSize : Integer;

begin { TCreateMediaForm.FillMediaCombo }
    if (ActiveControl = CreateMediaBtn) then
        MediaCombo.SetFocus;

    CreateMediaBtn.Enabled := false;

    try
        if (Assigned (DiskDrives)) then
            DiskDrives.Free;

        DiskDrives := TDiskDrives.Create (true, true);

    finally
        Screen.Cursor := crDefault;
    end; { try / finally }

    if (MediaCombo.Items.Count > 0) then
        MediaCombo.Items.Clear;

    for Disk in DiskDrives do
        if (SupportedDisk (Disk)) then
        begin
            sDriveLetters := GetMediaDriveLetters (Disk);

            iSize := Disk.DiskSize div 1000000000;

            if (Odd (iSize)) then
                Inc (iSize);

            if (sDriveLetters <> '') then
            begin
                sInfo := Format ('%s [%d GB] [%s]',
                                 [Disk.DisplayName, iSize, sDriveLetters]);
                MediaCombo.Items.AddObject (sInfo, Disk)
            end { if }
            else MediaCombo.Items.AddObject (Format ('%s [%d GB]',
                                                     [Disk.DisplayName, iSize]),
                                             Disk);
        end; { if }

    StatusBar.Panels [0].Text := '';

    MediaCombo.Hint := '';

    if (MediaCombo.Items.Count = 0) then
    begin
        MediaCombo.Items.Add (cNoRemovableMediaFoundMsg);
        MediaCombo.ItemIndex := 0;
    end { if }
    else if (MediaCombo.Items.Count = 1) then
    begin
        MediaCombo.ItemIndex := 0;
        CreateMediaBtn.Enabled := true;
        CreateMediaBtn.SetFocus;
        SetMediaComboHint;
    end { else if }
    else
        if (IsForegroundWindow) then
            MediaCombo.DroppedDown := true;
end; { TCreateMediaForm.FillMediaCombo }

function TCreateMediaForm.GetTargetDriveLetters : Boolean;

	(* ---- *)

    function GetCurrentDriveLetters (out chDrive_1, chDrive_2: Char) : Boolean;

    var
        Partition : TPartition;

    begin
        chDrive_1 := #0;
        chDrive_2 := #0;

        if (TargetDisk.PartitionCount > 0) then
            for Partition in TargetDisk do
                if (Assigned (Partition.LogicalDisk)) then
                    if (Partition.LogicalDisk.DriveLetter <> '') then
                        if (chDrive_1 = #0) then
	                        chDrive_1 := Partition.LogicalDisk.DriveLetter [1]
                        else
                        begin
							chDrive_2 := Partition.LogicalDisk.DriveLetter [1];
                            Break;
	                    end; { if }

        Result := chDrive_1 <> #0;
    end; { GetCurrentDriveLetters }

    (* ---- *)

    function GetNextFreeDriveLetter (const chStart: Char;
                        			 out chNext: Char;
                                     const bShowError: Boolean = true) : Boolean;

    var
    	iIndex, iStart : Integer;

    begin
        if (chStart <> #0) and (CharInSet (UpCase (chStart), ['A'..'Z'])) then
        	iStart := Byte (UpCase (chStart))
        else iStart := Byte ('A');

        for iIndex := iStart to Byte ('Z') do
            if (GetDriveType (Char (iIndex)) = dtError) then
            begin
            	chNext := Char (iIndex);
                Exit (true);
            end; { if }

        Result := false;

        if (bShowError) then
        MsgBox (cNoFreeDriveLetterMsg, mb_IconStop);
    end; { GetNextFreeDriveLetter }

    (* ---- *)

begin { TCreateMediaForm.GetTargetDriveLetters }
	Result := false;

	GetCurrentDriveLetters (chWinPeDrive, chDataDrive);

    if (chWinPeDrive = #0) then
    	if not (GetNextFreeDriveLetter ('D', chWinPeDrive, false)) then
        	if not (GetNextFreeDriveLetter ('A', chWinPeDrive)) then
				exit;

    if (chDataDrive <> #0) then
    	Result := true
    else
        if (GetNextFreeDriveLetter (Char (Succ (Byte (chWinPeDrive))),
        						    chDataDrive, false)) or
           (GetNextFreeDriveLetter ('A', chDataDrive)) then
		    Result := true;
end; { TCreateMediaForm.GetTargetDriveLetters }

function TCreateMediaForm.LoadValuesFromIni : Boolean;

	(* ---- *)

	function ReadValue (const sIniFile, sValueName: String;
    					out sValue: String;
                        const bShowError: Boolean = true) : Boolean;
	begin
		sValue := IniReadStr (sIniFile, cAppName, sValueName);

        Result := sValue <> '';

        if (Result = false) and (bShowError) then
        	MsgBox (cIniValueNotFoundMsg, [sValueName], mb_IconStop);
    end; { ReadValue }

    (* ---- *)

const
    cDriversPath = 'DriversPath';
	cMinimumMediaSize = 'MinimumMediaSize';
    cUseDsmDepot = 'UseDsmDepot';
    cWindowsSourcesPath = 'WindowsSourcesPath';
    cWinPE_Path = 'WinPE_Path';
{$IFNDEF DEBUG}
    cDebug = 'Debug';
{$ENDIF}

var
	sIniFile, sUseDsmDepot, sSize : String;
{$IFNDEF DEBUG}
    sDebug : String = '';
{$ENDIF}

begin { TCreateMediaForm.LoadValuesFromIni }
    Result := false;

	sIniFile := ChangeFileExt (Application.ExeName, '.ini');

    if (FileExists (sIniFile)) then
	begin
{$IFNDEF DEBUG}
        if (ReadValue (sIniFile, cDebug, sDebug, false)) then
            bDebug := sDebug = '1'
        else bDebug := false;
{$ENDIF}

		if (ReadValue (sIniFile, cDriversPath, sDriversPath)) and
           (ReadValue (sIniFile, cMinimumMediaSize, sSize)) and
		   (ReadValue (sIniFile, cUseDsmDepot, sUseDsmDepot)) and
           (ReadValue (sIniFile, cWindowsSourcesPath, sWindowsSourcesPath)) and
           (ReadValue (sIniFile, cWinPE_Path, sWinPE_Path)) then
        begin
            bUseDsmDepot := sUseDsmDepot = '1';

    		iMinimumMediaSize := StrToIntDef (sSize, 0);

            if (iMinimumMediaSize > 0) then
            	Result := true
            else MsgBox (cMinSizeInvalidMsg, mb_IconStop);
        end; { if }
    end { try / finally }
    else MsgBox (cConfigFileNotFoundMsg, [sIniFile], mb_IconStop);
end; { TCreateMediaForm.LoadValuesFromIni }

procedure TCreateMediaForm.OnCopyDataThreadTerminate (Sender: TObject);

var
    dwExitCode : DWord;

begin
    StatusBar.Panels [0].Text := '';
    EnableControls;
    Screen.Cursor := crDefault;

    with Sender as TCopyDataThread do
        dwExitCode := ExitCode;

    if (dwExitCode = 0) then
    begin
        MsgBox (cCopyDataSuccessMsg, [sTargetMedia], mb_IconAsterisk);
        Application.Terminate;
    end { if }
    else MsgBox (cCopyFailureMsg, [sTargetMedia, dwExitCode], mb_IconStop);

    bBusy := false;
end; { TCreateMediaForm.OnCopyDataThreadTerminate }

function TCreateMediaForm.PartitionThumbDrive : Boolean;

	(* ---- *)

	function CallScript (const sBatchFile, sDiskPartScript: String) : Boolean;

    var
    	dwExitCode : DWord = 0;
        bResult : Boolean;
        sCmdExe, sParams : String;

    begin
        Result := false;

		if (bDebug) then
            if (MsgBox ('Call "DiskPart" now?',
        			    mb_YesNo or mb_IconWarning or
                                                   mb_DefButton2) <> idYes) then
        	    exit;

        TaskDlgDirect (cDiskPartPromptMainMsg, cDiskPartPromptContentMsg,
                       tdtShield, [tcbOk]);

        EnableControls (false);
        Screen.Cursor := crHourGlass;
        SetStatusBarText (Format (cInitializingMediaMsg, [sTargetMedia]));
        Application.ProcessMessages;

        try
            sCmdExe := GetSystemDir + '\cmd.exe';
            // No "" surrounding parameters, otherwise it fails!
            sParams := Format ('/c %s %s',
                               [ExtractShortPath (sBatchFile), sDiskPartScript]);

            if (bDebug) then
                ShowMessage (Format ('DiskPart params = '#13#10'"%s"',
                                     [sParams]));

            bResult := RunElevated (sCmdExe, sParams, dwExitCode);

        finally
            StatusBar.Panels [0].Text := '';
	        Screen.Cursor := crDefault;
        end; { try / finally }

        if (bResult) then
        begin
	        if (dwExitCode = 0) then
            	Result := true
            else MsgBox (cDiskPartErrMsg, [sTargetMedia, dwExitCode],
            			 mb_IconStop);
        end { if }
  		else MsgBox (cDiskPartExecutionErrMsg, [GetLastErrorMsg], mb_IconStop);

        if not (Result) then
			EnableControls (true);
    end; { CallScript }

    (* ---- *)

var
    sScriptData : String = '';
    sBatchFile, sDP_Script : String;

begin { TCreateMediaForm.PartitionThumbDrive }
	Result := false;

    sBatchFile := ExtractFilePath (Application.ExeName) +
                  cPartitionTargetDriveBat;

    if not (FileExists (sBatchFile)) then
    begin
        MsgBox (SFileNotFound + ': "%s"', [sBatchFile], mb_IconStop);
        exit;
    end; { if }

    sDP_Script := sProgramDataPath + '\' + cPartitionTargetDriveDiskPartScript;

    if not (DeleteExistingFile (sDP_Script)) then
        exit;

    if (LoadText (ExtractFilePath (Application.ExeName) +
                  cPartitionTargetDriveDiskPartScript, sScriptData)) then
        if (SaveText (sDP_Script,
            		  Format (sScriptData,
                              [TargetDisk.DiskIndex, chWinPeDrive,
                               chDataDrive]))) then
			Result := CallScript (sBatchFile, sDP_Script);
end; { TCreateMediaForm.PartitionThumbDrive }

procedure TCreateMediaForm.ResetRefreshTimer;
begin
    if (bBusy) then
        exit;

    if not (RefreshTimer.Enabled) then
    	SetStatusBarText (cFillingMediaListMsg);

	RefreshTimer.Enabled := false;
    RefreshTimer.Enabled := true;

    if (Screen.Cursor <> crHourGlass) then
		Screen.Cursor := crHourGlass;
end; { TCreateMediaForm.ResetRefreshTimer }

procedure TCreateMediaForm.SetMediaComboHint;
begin
    with MediaCombo do
        if (ItemIndex = (-1)) then
            ShowHint := false
        else
        begin
            ShowHint := true;
            Hint := Items [ItemIndex];
        end; { else }
end; { TCreateMediaForm.SetMediaComboHint }

procedure TCreateMediaForm.SetStatusBarText (const sText: String;
                                             const iPanel: Integer = 0);
begin
	Assert (sText <> '');

    StatusBar.Panels [iPanel].Text := ' ' + sText;
end; { TCreateMediaForm.SetStatusBarText }

end.

