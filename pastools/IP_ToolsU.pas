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

unit IP_ToolsU;

interface

function CompareIPs (const sIP1, sIP2: String) : Integer;
function DotQuadFind (const sHTML: AnsiString) : AnsiString;
function HexStrToIP_Str (const sHex: String) : String;
function HostNameToIP (const sHostName: String;
					   var sIP, sError: String) : Boolean; overload;
function HostNameToIP (const sHostName: String) : String; overload;
function IP_To_Int (const sIP: String) : Int64;
function Valid_IP (sIP: String; out iErrorPos: Integer) : Boolean; overload;
function Valid_IP (const sIP: String) : Boolean; overload;

implementation

uses WinSock, Windows, SysUtils,
     PasTools;

(* ---- *)

function CompareIPs (const sIP1, sIP2: String) : Integer;

var
	a, b : TInAddr;

begin
{$IFDEF UNICODE}
    a.S_addr := inet_addr (PAnsiChar (AnsiString (sIP1)));
    b.S_Addr := inet_addr (PAnsiChar (AnsiString (sIP2)));
{$ELSE}
    a.S_addr := inet_addr (PChar (sIP1));
    b.S_Addr := inet_addr (PChar (sIP2));
{$ENDIF}

    if ntohl (a.S_addr) > ntohl (b.S_addr) then
    	Result := 1
    else if ntohl (b.S_addr) > ntohl (a.S_addr) then
    	Result := (-1)
    else Result := 0;
end; { CompareIPs }

(* ---- *)

function DotQuadFind (const sHTML: AnsiString) : AnsiString;

var
	sIP : AnsiString;

	(* ---- *)

	function Find_IP (const sLine: AnsiString; iByte: Integer = 0) : Boolean;

    var
    	iIndex, iLen, iResult : Integer;
        sByte : AnsiString;

    begin
    	Result := false;

    	if (sLine = '') then
        	exit;

        sByte := '';

    	inc (iByte);

        iIndex := 1;
        iLen := Length (sLine);

        while (iIndex <= iLen) do
        begin
        	if (sLine [iIndex] in ['0'..'9']) then
            begin
            	sByte := sByte + sLine [iIndex];

                if (Length (sByte) > 3) then
                	if (iByte > 1) then
	                	break { Zahl zu lang }
    	            else sByte := '';

                if (Length (sByte) = 3) then
	                if (Str2Int (sByte, iResult{%H-})) then
                    	if (iResult > 255) then
    	            		break; { Keine gültige IP }
            end { if }
            else if (sLine [iIndex] = '.') and (sByte <> '') and
            		(iByte < 4) then
            begin
                sIP := sIP + sByte + '.';

                if (Find_IP (Copy (sLine, iIndex + 1, iLen - iIndex),
                             iByte)) then
                begin
                    Result := true;
                    break;
                end { if }
                else
                    if (iByte > 1) then
                        break
                    else sByte := '';
            end { else if }
            else
            begin { Ein anderes Zeichen }
            	if (iByte = 1) then
                	sByte := ''
                else if (iByte in [2, 3]) then
                	Break
                else { iByte = 4 }
                begin
                    if (sByte <> '') then
                    begin
                        sIP := sIP + sByte;
                        Result := true;
                    end; { if }

                    Break;
                end; { else }
            end; { else }

            inc (iIndex);
        end; { for }

        if (iIndex > iLen) then { Ende der Zeile erreicht }
        	if (iByte = 4) and (sByte <> '') then
            begin
                sIP := sIP + sByte;
            	Result := true;
            end; { if }

        if (Result = false) then
        	sIP := '';
    end; { ScanLine }

    (* ---- *)

begin
	sIP := '';

    if (Find_IP (sHTML)) then
    	Result := sIP
    else Result := '';
end; { DotQuadFind }

(* ---- *)

function HexStrToIP_Str (const sHex: String) : String;
{ Eine IP-Adresse in hexadezimaler Form ($348F2C10) in das IP-Format
  172.16.2.8 umwandeln.
  ->> sHex : Adresse in hexadezimaler Form.
  <<- Result : IP-Adresse als Zeichenkette mit Zahlen. }

    (* ---- *)

    function GetHexValue (const chHex: Char) : Word;

    begin
        if (AnsiChar (chHex) in ['0'..'9']) then
            Result := Byte (chHex) - 48
        else if (AnsiChar (chHex) in ['A'..'F']) then
            Result := Byte (chHex) - 55
        else Result := Byte (chHex) - 87;
    end; { GetHexValue }

    (* ---- *)

var
    sTupple : String;
    i : Integer;

begin
    Result := '';

    i := 1;

    SetLength (sTupple{%H-}, 2);

    while (i < 9) do
    begin
        sTupple [1] := sHex [i];
        sTupple [2] := sHex [i + 1];

        Result := Result + Int2Str ((GetHexValue (sTupple [1]) * 16) +
                                    GetHexValue (sTupple [2])) + '.';

        inc (i, 2);
    end; { while }

    SetLength (Result, Length (Result) - 1);
end; { HexStrToIP_Str }

(* ---- *)

function HostNameToIP (const sHostName: String;
					   var sIP, sError: String) : Boolean;

type
	TArrayPInAddr = array [0..10] of PInAddr;
	PArrayPInAddr = ^TArrayPInAddr;

const
	cUnknown = 'Unknown error';

var
	InitData : TWSADATA;
	pHost: PHostEnt;
	iResult : Integer;
	pPtr : PArrayPInAddr;
    pszIP : PAnsiChar;

begin
	Result := false;
    sError := '';
    sIP := '';

    try
        iResult := WSAStartup ($101, InitData{%H-});

        if (iResult <> 0) then
        begin
            case iResult of
                WSASYSNOTREADY : sError := 'WSASysNotReady';
                WSAVERNOTSUPPORTED : sError := 'WSAVerNotSupported';
                WSAEINPROGRESS : sError := 'WSAEInProgress';
                WSAEPROCLIM : sError := 'WSAEProcLim';
                WSAEFAULT : sError := 'WSAEFault';
                else sError := cUnknown;
            end; { case }

            exit;
        end; { if }

        pHost := gethostbyname (PAnsiChar (AnsiString (sHostName)));

        if (pHost <> NIL) then
        begin
            pPtr := PArrayPInAddr(pHost^.h_addr_list);

            pszIP := inet_ntoa (pPtr^[0]^);

            if (pszIP <> NIL) then
            begin
                Result := true;
                sIP := String (pszIP);
            end { if }
            else sError := cUnknown;
        end { if }
        else
        begin
            iResult := WSAGetLastError;

            case iResult of
                WSANOTINITIALISED : sError := 'WSANotInitialised';
                WSAENETDOWN	 : sError := 'WSAENetDown';
                WSAHOST_NOT_FOUND : sError := 'WSAHost_Not_Found';
                WSATRY_AGAIN : sError := 'WSATry_Again';
                WSANO_RECOVERY : sError := 'WSANo_Recovery';
                WSANO_DATA : sError := 'WSANo_Data';
                WSAEINPROGRESS : sError := 'WSAEInProgress';
                WSAEFAULT : sError := 'WSAEFault';
                WSAEINTR : sError := 'WSAEIntr';
                else sError := cUnknown;
            end; { case }
        end; { else }

        if (sError <> '') then
            sError := String (Format ('WinSock error: %s', [sError]));

    finally
        WSACleanUp;
    end; { try / finally }
end; { HostNameToIP }

(* ---- *)

function HostNameToIP (const sHostName: String) : String; overload;

var
	sError : String;

begin
	HostNameToIp (sHostName, Result{%H-}, sError{%H-});
end; { HostNameToIP }

(* ---- *)

function IP_To_Int (const sIP: String) : Int64;
{ Eine als Zeichenkette eingegebene IP-Adresse in eine LongInt-Zahl umwandeln.
  Die Zahl der IP-Adresse muß immer am Anfang stehen, e.g. 172.16.2.8
  ->> sIP : Die IP-Adresse.
  <<- Result : IP-Adresse als Zahl. }

var
    abyBytes : array [1..4] of Byte;
    i, j, iPos, iResult : Integer;
    sNo : String;
    bBreak : Boolean;

begin
    if (Length (sIP) = 0) then
    begin
        Result := 0;
        exit;
    end; { if }

    SetLength (sNo{%H-}, 3);

    iPos := 1;

    for i := 1 to 4 do
    begin
        SetLength (sNo, 0);
        bBreak := false;

        for j := 1 to 4 do
        begin
            if (AnsiChar (sIP [iPos]) in ['0'..'9']) then
                sNo := sNo + sIP [iPos];

            if (sIP [iPos] = '.') or (iPos = Length (sIP)) then
                bBreak := true;

            inc (iPos);

            if (bBreak) then
                break;
        end; { for }

        if (Length (sNo) = 0) then
            sNo := '0'; { Sonst gibt's eine Exception }

        if (Str2Int (sNo, iResult{%H-})) then
        	abyBytes [i] := Byte (iResult)
        else
        begin
            MessageBox (GetDesktopWindow,
                        'Exception EConvertError in "IP_To_Int"!',
                        PChar (ParamStr (0)), mb_IconStop or mb_OK);
            abyBytes [i] := 0;
        end; { if }
    end; { for }

    { Wenn Überlaufprüfung eingeschaltet ist, ($Q+), gibt's hier eine
      Exception }
    Result {%H-}:= (LongInt (abyBytes [1]) * 16777216) +
              	   (LongInt (abyBytes [2]) * 65536) +
                   (LongInt (abyBytes [3]) * 256) +
                   LongInt (abyBytes [4]);
end; { IP_To_Int }

(* ---- *)

function Valid_IP (sIP: String; out iErrorPos: Integer) : Boolean;
{ Überprüft, ob eine IP-Adresse gültig ist.
  ->> sIP : IP-Adresse als Text.
  <<- Result : TRUE, wenn gültig, sonst FALSE. }

var
    iOrgLen, iByte, iPos, iIP : Integer;
    sByte : String;

begin
 	Assert (sIP = Trim (sIP));

	Result := false;

    if (sIP = '') then
    begin
		iErrorPos := 1;
        exit;
    end; { if }

    SetLength (sByte{%H-}, 3);

    iOrgLen := Length (sIP);
    iErrorPos := 1;
    iByte := 0;

    while (sIP <> '') do
    begin
        sByte := '';

        Inc (iByte);
        iPos := Pos ('.', sIP);

        if (iPos > 0) then
        begin
            sByte := Copy (sIP, 1, iPos - 1);
            Delete (sIP, 1, iPos);
        end { if }
        else
        begin
	        sByte := sIP;
            sIP := '';
        end; { else }

        iErrorPos := iOrgLen - Length (sIP);

        if (sByte = '') or (Length (sByte) > 3) then
        	exit;

        if (iByte > 4) then
        	exit;

        Str2Int (sByte, iIP{%H-});

        if (iIP < 0) or (iIP > 255) then
            exit;
    end; { for }

    if (iByte < 4) then
    	exit;

    Result := true;
end; { Valid_IP }

(* ---- *)

function Valid_IP (const sIP: String) : Boolean;

var
	iErrorPos : Integer;

begin
    Result := Valid_IP (sIP, iErrorPos);
end; { Valid_IP }

(* ---- *)

end.
