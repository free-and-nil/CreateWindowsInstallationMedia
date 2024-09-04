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

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

unit TaskDlgToolsU;  // "TaskDlgU" must be included in the "uses" list as well!

interface

uses Windows, Types,
     TaskDlgU;

type
    TTaskDlgResult = (tdrUndefined, tdrOk, tdrYes, tdrNo, tdrCancel, tdrRetry,
                      tdrClose, tdrTimedOut);
    TTaskDlgType = (tdtNone, tdtWarning, tdtError, tdtInformation, tdtShield);

function TaskDlgDirect (const hParentWnd: HWnd;
                        const sMainInstruction, sContent, sWindowTitle: String;
                        const TaskDlgType: TTaskDlgType;
                        const Buttons: TTaskDlgCommonButtons) : TTaskDlgResult;
																	   overload;

function TaskDlgDirect (const sMainInstruction, sContent, sWindowTitle: String;
                        const TaskDlgType: TTaskDlgType;
                        const Buttons: TTaskDlgCommonButtons) : TTaskDlgResult;
																	   overload;

function TaskDlgDirect (const sMainInstruction, sContent: String;
                        const TaskDlgType: TTaskDlgType;
                        const Buttons: TTaskDlgCommonButtons) : TTaskDlgResult;
																	   overload;

function TaskDlg (const hParentWnd: HWnd;
                  const sMainInstruction, sContent, sWindowTitle: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const bTopMost: Boolean = false;
                  const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlg (const sMainInstruction, sContent, sWindowTitle: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const bTopMost: Boolean = false;
                  const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlg (const sMainInstruction, sContent: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const bTopMost: Boolean = false;
                  const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlg (const hParentWnd: HWnd;
                  const sMainInstruction, sContent, sWindowTitle: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const DefaultButton: TTaskDlgCommonButton;
                  const bTopMost: Boolean = false;
                  const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlg (const sMainInstruction, sContent, sWindowTitle: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const DefaultButton: TTaskDlgCommonButton;
                  const bTopMost: Boolean = false;
                  const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlg (const sMainInstruction, sContent: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const DefaultButton: TTaskDlgCommonButton;
                  const bTopMost: Boolean = false;
                  const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlgCheckBox (const hParentWnd: HWnd;
                          const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const ToggleButton: TTaskDlgCommonButton;
                          const bToggleButtonDisabled: Boolean = true;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlgCheckBox (const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const ToggleButton: TTaskDlgCommonButton;
                          const bToggleButtonDisabled: Boolean = true;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlgCheckBox (const sMainInstruction, sContent: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const ToggleButton: TTaskDlgCommonButton;
                          const bToggleButtonDisabled: Boolean = true;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlgCheckBox (const hParentWnd: HWnd;
                          const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlgCheckBox (const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;
                                                                       overload;

function TaskDlgRadioBtn (const hParentWnd: HWnd;
                          const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const asRadioButtons: TStringDynArray;
                          out uSelected: Cardinal;
                          const uPreSelected: UInt = 0;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true;
                          const RadioBtnClicked: TRadioBtnClickedEvent = NIL) :
                                                       TTaskDlgResult; overload;

function TaskDlgRadioBtn (const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const asRadioButtons: TStringDynArray;
                          out uSelected: Cardinal;
                          const uPreSelected: UInt = 0;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true;
                          const RadioBtnClicked: TRadioBtnClickedEvent = NIL) :
                                                       TTaskDlgResult; overload;

function TaskDlgRadioBtn (const sMainInstruction, sContent: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const asRadioButtons: TStringDynArray;
                          out uSelected: Cardinal;
                          const uPreSelected: UInt = 0;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true;
                          const RadioBtnClicked: TRadioBtnClickedEvent = NIL) :
                                                       TTaskDlgResult; overload;

implementation

uses SysUtils, {$IFNDEF NO_VCL} Forms, {$ENDIF}
     TaskDlgApiU;

const
    cRadioBtnOffset = 11;

type
    TTaskDlgBase = class (TTaskDlg)
      private
        FTimeOut : Boolean;
        uTimeOutSec : UInt;
        rTimeOutSec : Real;
        bProgressBar : Boolean;

    	procedure Timer (Sender: TObject; const {%H-}uElapsed: UInt;
        			 	 var {%H-}bResetTimer: Boolean);

      public
        constructor Create (const sMainInstruction, sContent,
                                                           sWindowTitle: String;
                            const TaskDlgType: TTaskDlgType;
                            const Buttons: TTaskDlgCommonButtons;
                            const bTopMost: Boolean; const uTimeOutSec: UInt;
                            const bProgressBar: Boolean);

        function Execute (const hParent: HWnd = 0) : TTaskDlgResult;

        property TimeOut : Boolean read FTimeOut;
    end; { TTaskDlgBase }

    { TTaskDlgCheckBox }

    TTaskDlgCheckBox = class (TTaskDlgBase)
	  private
        ToggletBtn : TTaskDlgCommonButton;
        bToggleBtnDisabled : Boolean;

        procedure DialogCreated (Sender: TObject);
        procedure VerificationClicked (Sender: TObject; const bChecked: Boolean);

      public
        constructor Create (const sMainInstruction, sContent,
                                                           sWindowTitle: String;
                            const TaskDlgType: TTaskDlgType;
                            const Buttons: TTaskDlgCommonButtons;
                            const bToggleBtnDisabled: Boolean;
                            const bCheckBoxChecked, bTopMost: Boolean;
                            const uTimeOutSec: UInt;
                            const bProgressBar: Boolean); overload;

        constructor Create (const sMainInstruction, sContent,
                                                           sWindowTitle: String;
                            const TaskDlgType: TTaskDlgType;
                            const Buttons: TTaskDlgCommonButtons;
                            const bTopMost: Boolean;
                            const uTimeOutSec: UInt;
                            const bProgressBar: Boolean); overload;
    end; { TTaskDlgCheckBox }

    { TTaskDlgRadioBtn }

    TTaskDlgRadioBtn = class (TTaskDlgBase)
      private
        RadioBtnClicked : TRadioBtnClickedEvent;

        procedure RadioBtnClickedInternal (Sender: TObject;
                                           const iButtonID: Integer);

      public
        constructor Create (const sMainInstruction, sContent,
                                                           sWindowTitle: String;
                            const TaskDlgType: TTaskDlgType;
                            const Buttons: TTaskDlgCommonButtons;
                            const asRadioButtons: TStringDynArray;
                            const uSelectedBtnIndex: UInt;
                            const bTopMost: Boolean;
                            const uTimeOutSec: UInt;
                            const bProgressBar: Boolean;
                            const RadioBtnClicked: TRadioBtnClickedEvent);
    end; { TTaskDlgRadioBtn }

(* ---- *)

function GetParentWindow : HWND;
begin
{$IFDEF NO_VCL}
    Result := GetForegroundWindow;

    if (Result = 0) then
        Result := GetDesktopWindow;

{$ELSE}
    if (Assigned (Screen)) and (Assigned (Screen.ActiveForm)) and
       (IsWindow (Screen.ActiveForm.Handle)) then
        Result := Screen.ActiveForm.Handle
    else Result := GetDesktopWindow;
{$ENDIF}
end; { GetParentWindow }

(* ---- *)

function GetParentWindowTitle : String;

{$IFDEF NO_VCL}
var
	hParent : HWnd;
    iLen : Integer;
{$ENDIF}

begin
{$IFDEF NO_VCL}
    hParent := GetForegroundWindow;

    if (hParent <> 0) then
    begin
        SetLength (Result, MAX_PATH);

        iLen := GetWindowText (hParent, PChar (Result), MAX_PATH);

        if (iLen > 0) then
        begin
            SetLength (Result, iLen);
            exit;
        end; { if }
    end; { if }

    Result := ExtractFileName (ParamStr (0));
{$ELSE}
    if (Assigned (Screen)) and (Assigned (Screen.ActiveForm)) then
        Result := Screen.ActiveForm.Caption
    else Result := Application.Title;
{$ENDIF}
end; { GetParentWindowTitle }

(* ---- *)

procedure TTaskDlgBase.Timer (Sender: TObject; const uElapsed: UInt;
                              var bResetTimer: Boolean);

const
    uLastElapsed : UInt = 0;

var
    uSecondsElapsed, uProgressBarPos : UInt;
    Button : TTaskDlgCommonButton;

begin
    if (uTimeOutSec > 0) then
    begin
        uSecondsElapsed := uElapsed div 1000;

        if (uSecondsElapsed > uTimeOutSec) then
        begin
            FTimeOut := true;

            if (tcbCancel in CommonButtons) then
                ClickButton (tcbCancel)
            else
                for Button in CommonButtons do
                begin
                    ClickButton (Button);  // Simulate button press
                    Break;
                end; { for }

            exit;
        end; { if }

        if (bProgressBar) and (uLastElapsed <> uSecondsElapsed) then
        begin
            uLastElapsed := uSecondsElapsed;
            uProgressBarPos := Round ((uSecondsElapsed / rTimeOutSec) * 100);

            if (ProgressBarPos <> uProgressBarPos) then
                ProgressBarPos := uProgressBarPos;
        end; { if }
    end; { if }

    if (TopMost) then
        SetWindowPos (hTaskDlg, hwnd_TopMost, 0, 0, 0, 0,
                      swp_NoSize or swp_NoMove);
end; { TTaskDlgBase.Timer }

(* ---- *)

function TTaskDlgBase.Execute (const hParent: HWnd) : TTaskDlgResult;
begin
    inherited Execute (hParent);

    if (TimeOut) then
        Result := tdrTimedOut
    else
        case ModalResult of
            idOK : Result := tdrOk;
            idYes : Result := tdrYes;
            idNo : Result := tdrNo;
            idCancel : Result := tdrCancel;
            idRetry : Result := tdrRetry;
            idClose : Result := tdrClose
            else ; Result := tdrUndefined;
        end; { case TDCB.ModalResult of }
end; { TTaskDlgBase.Execute }

(* ---- *)

constructor TTaskDlgBase.Create (const sMainInstruction, sContent,
                                                           sWindowTitle: String;
                                 const TaskDlgType: TTaskDlgType;
                                 const Buttons: TTaskDlgCommonButtons;
                                 const bTopMost: Boolean;
                                 const uTimeOutSec: UInt;
                                 const bProgressBar: Boolean);
begin
    Assert (Buttons <> []);

    inherited Create;

    if (tcbCancel in Buttons) then
        Flags := [tdfAllowDialogCancellation, tdfPositionRelativeToWindow]
    else Flags := [tdfPositionRelativeToWindow];

    MainInstruction := sMainInstruction;
    Content := sContent;
    WindowTitle := sWindowTitle;

    CommonButtons := Buttons;

    case TaskDlgType of
        tdtWarning : MainIcon := tdiWarning;
        tdtError : MainIcon := tdiError;
        tdtInformation : MainIcon := tdiInformation;
        tdtShield : MainIcon := tdiInformation;
    end; { TTaskDlgType }

    TopMost := bTopMost;
    Self.uTimeOutSec := uTimeOutSec;
    Self.bProgressBar := bProgressBar;

    if (uTimeOutSec > 0) or (bTopMost) then
    begin
        rTimeOutSec := uTimeOutSec * 1.0;
        Self.uTimeOutSec := uTimeOutSec;

        if (uTimeOutSec > 0) and (bProgressBar) then
			Flags := Flags + [tdfCallbackTimer, tdfShowProgressBar]
        else Flags := Flags + [tdfCallbackTimer];

        OnTimer := Timer;
        TopMost := bTopMost;
    end; { if }
end; { TTaskDlgBase.Create }

(* ---- *)

procedure TTaskDlgCheckBox.DialogCreated (Sender: TObject);
begin
    if (bToggleBtnDisabled) then
        EnableButton (ToggletBtn, false);
end; { TTaskDlgCheckBox.DialogCreated }

(* ---- *)

procedure TTaskDlgCheckBox.VerificationClicked (Sender: TObject;
                                                const bChecked: Boolean);
begin
    EnableButton (ToggletBtn, bChecked);
end; { TTaskDlgCheckBox.VerificationClicked }

(* ---- *)

constructor TTaskDlgCheckBox.Create (const sMainInstruction, sContent,
                                                           sWindowTitle: String;
                                     const TaskDlgType: TTaskDlgType;
                                     const Buttons: TTaskDlgCommonButtons;
                                     const bToggleBtnDisabled: Boolean;
                                     const bCheckBoxChecked, bTopMost: Boolean;
                                     const uTimeOutSec: UInt;
                                     const bProgressBar: Boolean);
begin
    inherited Create (sMainInstruction, sContent, sWindowTitle, TaskDlgType,
                      Buttons, bTopMost, uTimeOutSec, bProgressBar);

    if (bCheckBoxChecked) then
        Flags := Flags + [tdfVerificationFlagChecked];

    Self.bToggleBtnDisabled := bToggleBtnDisabled;

    OnDialogCreated := DialogCreated;
    OnVerificationClicked := VerificationClicked;
end; { TTaskDlgCheckBox.Create }

constructor TTaskDlgCheckBox.Create (const sMainInstruction, sContent,
                                                           sWindowTitle: String;
                                     const TaskDlgType: TTaskDlgType;
                                     const Buttons: TTaskDlgCommonButtons;
                                     const bTopMost: Boolean;
                                     const uTimeOutSec: UInt;
                                     const bProgressBar: Boolean);
begin
    inherited Create (sMainInstruction, sContent, sWindowTitle, TaskDlgType,
                      Buttons, bTopMost, uTimeOutSec, bProgressBar);
end;

(* ---- *)

procedure TTaskDlgRadioBtn.RadioBtnClickedInternal (Sender: TObject;
                                                    const iButtonID: Integer);
begin
    if (Assigned (RadioBtnClicked)) and (IsWindowVisible (hTaskDlg)) then
        RadioBtnClicked (Sender, iButtonID - cRadioBtnOffset);
end; { TTaskDlgRadioBtn.RadioBtnClickedInternal }

(* ---- *)

constructor TTaskDlgRadioBtn.Create (
                         const sMainInstruction, sContent, sWindowTitle: String;
                         const TaskDlgType: TTaskDlgType;
                         const Buttons: TTaskDlgCommonButtons;
                         const asRadioButtons: TStringDynArray;
                         const uSelectedBtnIndex: UInt;
                         const bTopMost: Boolean; const uTimeOutSec: UInt;
                         const bProgressBar: Boolean;
                         const RadioBtnClicked: TRadioBtnClickedEvent);

var
    iIndex : Integer;

begin
    inherited Create (sMainInstruction, sContent, sWindowTitle, TaskDlgType,
                      Buttons, bTopMost, uTimeOutSec, bProgressBar);

    if (Assigned (RadioBtnClicked)) then
    begin
        OnRadioBtnClicked := RadioBtnClickedInternal;
        Self.RadioBtnClicked := RadioBtnClicked;
    end; { if }

    for iIndex := 0 to High (asRadioButtons) do
        AddRadioButton (iIndex + cRadioBtnOffset, asRadioButtons [iIndex],
                        UInt (iIndex) = uSelectedBtnIndex);
end; { TTaskDlgRadioBtn.Create }

(* ---- *)

function TaskDlgDirect (const hParentWnd: HWnd;
                        const sMainInstruction, sContent, sWindowTitle: String;
                        const TaskDlgType: TTaskDlgType;
                        const Buttons: TTaskDlgCommonButtons) : TTaskDlgResult;

    (* ---- *)

    function GetCommonButtons : DWord;

    const
        cCommonButtons : array [TTaskDlgCommonButton] of Word =
            (TDCBF_OK_BUTTON, TDCBF_YES_BUTTON, TDCBF_NO_BUTTON,
             TDCBF_CANCEL_BUTTON, TDCBF_RETRY_BUTTON, TDCBF_CLOSE_BUTTON);

    var
        Button : TTaskDlgCommonButton;

    begin
        Result := 0;

        for Button := Low (TTaskDlgCommonButton) to
                                                  High (TTaskDlgCommonButton) do
            if (Button in Buttons) then
                Result := Result or cCommonButtons [Button];
    end; { GetCommonButtons }

    (* ---- *)

    function GetIcon : Word;
    begin
        case TaskDlgType of
            tdtWarning : Result := TD_WARNING_ICON;
            tdtError : Result := TD_ERROR_ICON;
            tdtInformation : Result := TD_INFORMATION_ICON;
            tdtShield : Result := TD_SHIELD_ICON;
            else Result := 0;
        end; { case }
    end; { GetIcon }

    (* ---- *)

var
	nButton : Integer;

begin { TaskDlgDirect }
	Win32Check (TaskDialog (hParentWnd, 0,
{$IFDEF UNICODE}
                PWideChar (sWindowTitle), PWideChar (sMainInstruction),
                PWideChar (sContent),
{$ELSE}
                PWideChar (WideString (sWindowTitle)),
                PWideChar (WideString (sMainInstruction)),
                PWideChar (WideString (sContent)),
{$ENDIF}
                GetCommonButtons, {%H-}PWideChar (GetIcon), nButton{%H-}) = S_OK);

    case nButton of
        idOK : Result := tdrOk;
        idYes : Result := tdrYes;
        idNo : Result := tdrNo;
        idCancel : Result := tdrCancel;
        idRetry : Result := tdrRetry;
        idClose : Result := tdrClose;
        else Result := tdrUndefined;
    end; { case }
end; { TaskDlgDirect }

(* ---- *)

function TaskDlgDirect (const sMainInstruction, sContent, sWindowTitle: String;
                        const TaskDlgType: TTaskDlgType;
                        const Buttons: TTaskDlgCommonButtons) : TTaskDlgResult;
begin
	Result := TaskDlgDirect (GetParentWindow, sMainInstruction, sContent,
    						 sWindowTitle, TaskDlgType, Buttons)
end; { TaskDlgDirect }

(* ---- *)

function TaskDlgDirect (const sMainInstruction, sContent: String;
                        const TaskDlgType: TTaskDlgType;
                        const Buttons: TTaskDlgCommonButtons) : TTaskDlgResult;
begin
	Result := TaskDlgDirect (GetParentWindow, sMainInstruction, sContent,
    						 GetParentWindowTitle, TaskDlgType, Buttons)
end; { TaskDlgDirect }

(* ---- *)

function TaskDlg (const hParentWnd: HWnd;
                  const sMainInstruction, sContent, sWindowTitle: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const bTopMost: Boolean = false; const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;

var
    TD : TTaskDlgBase;

begin
    TD := TTaskDlgBase.Create (sMainInstruction, sContent, sWindowTitle,
                               TaskDlgType, Buttons, bTopMost, uTimeOutSec,
                               bProgressBar);

    try
        Result := TD.Execute (hParentWnd);

    finally
        TD.Free;
    end; { try / finally }
end; { TaskDlg }

(* ---- *)

function TaskDlg (const sMainInstruction, sContent, sWindowTitle: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const bTopMost: Boolean = false; const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
begin
    Result := TaskDlg (GetParentWindow, sMainInstruction, sContent,
                       sWindowTitle, TaskDlgType, Buttons, bTopMost,
                       uTimeOutSec, bProgressBar)
end; { TaskDlg }

(* ---- *)

function TaskDlg (const sMainInstruction, sContent: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const bTopMost: Boolean = false; const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
begin
    Result := TaskDlg (GetParentWindow, sMainInstruction, sContent,
                       GetParentWindowTitle, TaskDlgType, Buttons, bTopMost,
                       uTimeOutSec, bProgressBar);
end; { TaskDlg }

(* ---- *)

function TaskDlg (const hParentWnd: HWnd;
                  const sMainInstruction, sContent, sWindowTitle: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const DefaultButton: TTaskDlgCommonButton;
                  const bTopMost: Boolean = false; const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
var
    TD : TTaskDlgBase;

begin
    Assert (DefaultButton in Buttons);

    TD := TTaskDlgBase.Create (sMainInstruction, sContent, sWindowTitle,
                               TaskDlgType, Buttons, bTopMost, uTimeOutSec,
                               bProgressBar);

    try
        TD.DefaultButton := DefaultButton;

        Result := TD.Execute (hParentWnd);

    finally
        TD.Free;
    end; { try / finally }
end; { TaskDlg }

(* ---- *)

function TaskDlg (const sMainInstruction, sContent, sWindowTitle: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const DefaultButton: TTaskDlgCommonButton;
                  const bTopMost: Boolean = false; const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
begin
    Result := TaskDlg (GetParentWindow, sMainInstruction, sContent,
                       sWindowTitle, TaskDlgType, Buttons,
                       DefaultButton, bTopMost, uTimeOutSec, bProgressBar);
end; { TaskDlg }

(* ---- *)

function TaskDlg (const sMainInstruction, sContent: String;
                  const TaskDlgType: TTaskDlgType;
                  const Buttons: TTaskDlgCommonButtons;
                  const DefaultButton: TTaskDlgCommonButton;
                  const bTopMost: Boolean = false; const uTimeOutSec: UInt = 0;
                  const bProgressBar: Boolean = true) : TTaskDlgResult;
begin
    Result := TaskDlg (GetParentWindow, sMainInstruction, sContent,
                       GetParentWindowTitle, TaskDlgType, Buttons,
                       DefaultButton, bTopMost, uTimeOutSec, bProgressBar);
end; { TaskDlg }

(* ---- *)

function TaskDlgCheckBox (const hParentWnd: HWnd;
                          const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const ToggleButton: TTaskDlgCommonButton;
                          const bToggleButtonDisabled: Boolean = true;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;

var
    TDCB : TTaskDlgCheckBox;

begin
    Assert (ToggleButton in Buttons, '"ToggleButton" not in "Buttons"');

    TDCB := TTaskDlgCheckBox.Create (sMainInstruction, sContent,
                                     sWindowTitle, TaskDlgType, Buttons,
                                     bToggleButtonDisabled, bCheckBoxChecked,
                                     bTopMost, uTimeOutSec, bProgressBar);

    try
        TDCB.VerificationText := sCheckBoxText;
        TDCB.ToggletBtn := ToggleButton;
        Result := TDCB.Execute (hParentWnd);

    finally
        TDCB.Free;
    end; { try / finally }
end; { TaskDlgCheckBox }

(* ---- *)

function TaskDlgCheckBox (const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const ToggleButton: TTaskDlgCommonButton;
                          const bToggleButtonDisabled: Boolean = true;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;
begin
    Result := TaskDlgCheckBox (GetParentWindow, sMainInstruction, sContent,
                               sWindowTitle, TaskDlgType, Buttons, sCheckBoxText,
                               bCheckBoxChecked, ToggleButton,
                               bToggleButtonDisabled, bTopMost, uTimeOutSec,
                               bProgressBar);
end; { TaskDlgCheckBox }

(* ---- *)

function TaskDlgCheckBox (const sMainInstruction, sContent: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const ToggleButton: TTaskDlgCommonButton;
                          const bToggleButtonDisabled: Boolean = true;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;
begin
    Result := TaskDlgCheckBox (GetParentWindow, sMainInstruction, sContent,
                               GetParentWindowTitle, TaskDlgType, Buttons,
                               sCheckBoxText, bCheckBoxChecked, ToggleButton,
                               bToggleButtonDisabled, bTopMost, uTimeOutSec,
                               bProgressBar);
end; { TaskDlgCheckBox }

(* ---- *)

function TaskDlgCheckBox (const hParentWnd: HWnd;
                          const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;

var
    TDCB : TTaskDlgCheckBox;

begin
    TDCB := TTaskDlgCheckBox.Create (sMainInstruction, sContent, sWindowTitle,
                                     TaskDlgType, Buttons, bTopMost,
                                     uTimeOutSec, bProgressBar);

    try
        TDCB.VerificationText := sCheckBoxText;

        if (bCheckBoxChecked) then
            TDCB.Flags := TDCB.Flags + [tdfVerificationFlagChecked];

        Result := TDCB.Execute (hParentWnd);

        bCheckBoxChecked := TDCB.VerificationFlagChecked;

    finally
        TDCB.Free;
    end; { try / finally }
end; { TaskDlgCheckBox }

(* ---- *)

function TaskDlgCheckBox (const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const sCheckBoxText: String;
                          var bCheckBoxChecked: Boolean;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true) : TTaskDlgResult;
begin
    Result := TaskDlgCheckBox (GetParentWindow, sMainInstruction, sContent,
                               sWindowTitle, TaskDlgType, Buttons, sCheckBoxText,
                               bCheckBoxChecked, bTopMost, uTimeOutSec,
                               bProgressBar);
end; { TaskDlgCheckBox }

(* ---- *)

function TaskDlgRadioBtn (const hParentWnd: HWnd;
                          const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const asRadioButtons: TStringDynArray;
                          out uSelected: Cardinal;  // 0 = first, -1 = non selected
                          const uPreSelected: UInt = 0;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true;
                          const RadioBtnClicked: TRadioBtnClickedEvent = NIL) :
                                                                 TTaskDlgResult;

var
    TDRB : TTaskDlgRadioBtn;

begin
    Assert (Length (asRadioButtons) > 1,
            '"asRadioButtons" must contain at least 2 entries');
    Assert (uPreSelected < Cardinal (Length (asRadioButtons)),
            'Selected item index larger than the count of RadioButtons');

    TDRB := TTaskDlgRadioBtn.Create (sMainInstruction, sContent, sWindowTitle,
    								 TaskDlgType, Buttons, asRadioButtons,
                                     uPreSelected, bTopMost,
                                     uTimeOutSec, bProgressBar, RadioBtnClicked);

    try
        Result := TDRB.Execute (hParentWnd);

        uSelected := TDRB.RadioButton - cRadioBtnOffset;

    finally
        TDRB.Free;
    end; { try / finally }
end; { TaskDlgRadioBtn }

(* ---- *)

function TaskDlgRadioBtn (const sMainInstruction, sContent, sWindowTitle: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const asRadioButtons: TStringDynArray;
                          out uSelected: Cardinal;  // 0 = first
                          const uPreSelected: UInt = 0;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true;
                          const RadioBtnClicked: TRadioBtnClickedEvent = NIL) :
                                                                 TTaskDlgResult;
begin
	Result := TaskDlgRadioBtn (GetParentWindow, sMainInstruction, sContent,
    						   sWindowTitle, TaskDlgType, Buttons,
                               asRadioButtons, uSelected, uPreSelected,
                               bTopMost, uTimeOutSec, bProgressBar,
                               RadioBtnClicked);
end; { TaskDlgRadioBtn }

(* ---- *)

function TaskDlgRadioBtn (const sMainInstruction, sContent: String;
                          const TaskDlgType: TTaskDlgType;
                          const Buttons: TTaskDlgCommonButtons;
                          const asRadioButtons: TStringDynArray;
                          out uSelected: Cardinal;  // 0 = first
                          const uPreSelected: UInt = 0;
                          const bTopMost: Boolean = false;
                          const uTimeOutSec: UInt = 0;
                          const bProgressBar: Boolean = true;
                          const RadioBtnClicked: TRadioBtnClickedEvent = NIL) :
                                                       TTaskDlgResult; overload;
begin
	Result := TaskDlgRadioBtn (GetParentWindow, sMainInstruction, sContent,
    						   GetParentWindowTitle, TaskDlgType, Buttons,
                               asRadioButtons, uSelected, uPreSelected,
                               bTopMost, uTimeOutSec, bProgressBar,
                               RadioBtnClicked)
end; { TaskDlgRadioBtn }

(* ---- *)

end.

