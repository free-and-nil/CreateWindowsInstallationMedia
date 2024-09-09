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

{$WARN SYMBOL_PLATFORM OFF}

unit DiskAndPartitionInfoU;

interface

uses Classes, Windows,
{$IFDEF FPC}
    JwaWinIoctl;
{$ELSE}
    DeviceIoControlU;
{$ENDIF}

const
    cOne_MB = 1048576;
    c10_MB  = 10485760;
    cOne_GB = 1073741824;

{$IFDEF FPC}
	PARTITION_BASIC_DATA_GUID : TGUID = (
    	// EBD0A0A2-B9E5-4433-87C0-68B6B72699C7
    	D1:$EBD0A0A2; D2:$B9E5; D3:$4433;
        D4:($87, $C0, $68, $B6, $B7, $26, $99, $C7));
{$ENDIF}

	PARTITION_MSFT_RECOVERY_GUID : TGUID = (
		// DE94BBA4-06D1-4D40-A16A-BFD50179D6AC
    	D1:$DE94BBA4; D2:$06D1; D3:$4D40;
        D4:($A1, $6A, $BF, $D5, $01, $79, $D6, $AC));

type
    TBusType = (btUnknown, btScsi, btAtapi, btAta, bt1394, btSsa, btFibre,
                btUsb, btRAID, btiScsi, btSas, btSata, btSd, btMmc, btVirtual,
                btFileBackedVirtual, btSpaces, btNvme, btSCM, btUfs, btMax);

    TPartitionStyle = (psMBR, psGPT, psRaw);

	TDiskDrive = class;
    TPartition = class;

    TaDiskDrives = array of TDiskDrive;
    TaPartitions = array of TPartition;

    TLogicalDisk = class
      private
      	FDriveLetter : String;
        FFileSystem : String;
        FFreeSpace : Int64;
        FParentPartition : TPartition;
        FLogicalDiskSize : Int64;
        FVolumeDirty : Boolean;
        FVolumeName : String;

      public
        constructor Create (const Partition: TPartition);

        property DriveLetter : String read FDriveLetter;
        property FileSystem : String read FFileSystem;
        property FreeSpace : Int64 read FFreeSpace;
        property ParentPartition : TPartition read FParentPartition;
        property LogicalDiskSize : Int64 read FLogicalDiskSize;
        property VolumeDirty : Boolean read FVolumeDirty;
        property VolumeName : String read FVolumeName;
    end; { TLogicalDisk }

	TPartition = class
      private
        FLogicalDisk : TLogicalDisk;
        FParentDrive : TDiskDrive;
        FPartitionNumber : Integer;
        FPartitionSize : Int64;

      public
        constructor Create (const Drive: TDiskDrive);
        destructor Destroy; override;

        property LogicalDisk : TLogicalDisk read FLogicalDisk;
        property PartitionNumber : Integer read FPartitionNumber;
        property ParentDrive : TDiskDrive read FParentDrive;
        property PartitionSize : Int64 read FPartitionSize;
    end; { TPartition }

    TGptAttribute = (gaPlatformRequired, gaNoDriveLetter, gaHidden,
    				 gaShadowCopy, gaReadOnly);
    TGptAttributes = set of TGptAttribute;

    TPartition_GPT = class (TPartition)
      private
        FPartitionType : TGuid;
        FPartitionId : TGuid;
        FPartitionAttributes : DWord64;
        FPartitionName : String;
        FGptAttributes : TGptAttributes;

        function GetPartitionId : String;
        function GetPartitionType : String;

      public
        function GetGptAttributes : String;

        property PartitionType : String read GetPartitionType;
        property PartitionTypeGuid : TGuid read FPartitionType;
        property PartitionId : String read GetPartitionId;
        property PartitionAttributes : DWord64 read FPartitionAttributes;
        property GptAttributes : TGptAttributes read FGptAttributes;
        property PartitionName : String read FPartitionName;
    end; { TPartition_GPT }

    TPartition_MBR = class (TPartition)
      private
      	FBootable : Boolean;
        FPartitionType : String;
		FExtendedPartition : Boolean;
        FPartitionTypeByte : Byte;

      public
      	property Bootable : Boolean read FBootable;
        property PartitionType : String read FPartitionType;
		property ExtendedPartition : Boolean read FExtendedPartition;
        property PartitionTypeByte : Byte read FPartitionTypeByte;
    end; { TPartition_MBR }

    TPartition_MBR_Unknown = class (TPartition)
      private
        FPartitionType : String;
        FPartitionTypeByte : Byte;

      public
        property PartitionType : String read FPartitionType;
        property PartitionTypeByte : Byte read FPartitionTypeByte;
    end; { TPartition_MBR_Unknown }

    TPartition_GPT_Unknown = class (TPartition)
      private
      public
    end; { TPartition_GPT_Unknown }

    TDiskDriveEnumerator = class
      private
        FIndex: Integer;
        FDiskDrive : TDiskDrive;

      public
        constructor Create (const DiskDrive: TDiskDrive);
        function GetCurrent : TPartition;
        function MoveNext : Boolean;
        property Current : TPartition read GetCurrent;
    end; { TDiskDriveEnumerator }

	TDiskDrive = class
      private
        FaPartitions : TaPartitions;
        FBusType : TBusType;
        FDiskIndex : Integer;
		FMediaType : TMediaType;
        FDiskSize : Int64;
        FPartitionStyle : TPartitionStyle;
        FDisplayName : String;
		FSerialNumber: String;
        FDiskInterface : String;
        FVendorId: String;
        FProductId: String;
        FProductRevision: String;
        FDevicePath : String;

        function GetPartionCount : Integer;
        function GetPartition (iIndex: Integer) : TPartition;
        procedure GetPartitionInfo (const hDisk: THandle); virtual; abstract;

      public
      	constructor Create (const iDiskIndex: Integer);
        destructor Destroy; override;

        function GetEnumerator : TDiskDriveEnumerator;
        function GetMediaType : String;
        function GetPartitionStyle (const Style: TPartitionStyle) : String;

        property BusType : TBusType read FBusType;
        property DevicePath : String read FDevicePath;
        property DiskIndex : Integer read FDiskIndex;
        property DiskInterface : String read FDiskInterface;
        property DiskSize : Int64 read FDiskSize;
        property DisplayName : String read FDisplayName;
        property MediaType : TMediaType read FMediaType;
        property PartitionCount : Integer read GetPartionCount;
        property Partitions [iIndex: Integer] : TPartition read GetPartition;
                                                                        default;
        property PartitionStyle : TPartitionStyle read FPartitionStyle;
        property ProductId : String read FProductId;
        property ProductRevision : String read FProductRevision;
        property SerialNumber : String read FSerialNumber;
        property VendorId : String read FVendorId;
    end; { TDiskDrive }

    TDiskDrive_GPT = class (TDiskDrive)
      private
        FDiskId : TGuid;

        function GetDiskId : String;
        procedure GetPartitionInfo (const hDisk: THandle); override;

      public
        constructor Create (const iDiskIndex: Integer);

        property DiskId : String read GetDiskId;
    end; { TDiskDrive_GPT }

    TDiskDrive_MBR = class (TDiskDrive)
      private
        FDiskSignature : String;

        procedure GetPartitionInfo (const hDisk: THandle); override;
        procedure GetPartitionInfo_NT4_W2K (const hDisk: THandle);
        procedure GetPartitionInfo_XP_and_Up (const hDisk: THandle);
        function GetPartitionType (const byType: Byte;
        						   out sType: String) : Boolean;
        function IsExtended (const byPartitionType: Byte) : Boolean;

      public
        constructor Create (const iDiskIndex: Integer);

        property DiskSignature : String read FDiskSignature;
    end; { TDiskDrive_MBR }

    TDiskDrive_RAW = class (TDiskDrive)
      private
        procedure GetPartitionInfo (const {%H-}hDisk: THandle); override;

      public
		constructor Create (const iDiskIndex: Integer);
      end; { TDiskDrive_RAW }

    TDiskDrives = class;

    TDiskDrivesEnumerator = class
      private
        FIndex: Integer;
        FDiskDrives : TDiskDrives;

      public
        constructor Create (const DiskDrives: TDiskDrives);
        function GetCurrent : TDiskDrive;
        function MoveNext : Boolean;
        property Current : TDiskDrive read GetCurrent;
    end; { TDiskDrivesEnumerator }

    TDiskDrives = class
      private
      	FDrives : TaDiskDrives;
        FLastError : String;
        FIncludeRemovableDisks : Boolean;

        bCheckForAdminRights : Boolean;

        procedure FillDiskDrivesArray (const bUserIsAdmin: Boolean);
        procedure FillVolumeList (const bUserIsAdmin: Boolean);
        function GetCount : Integer;
        function GetDrive (iIndex: Integer) : TDiskDrive;
        function GetDiskGeometry_NT4_W2K (const hDisk: THandle;
									      out iDiskSize: Int64;
                                          out MediaType: TMediaType) : Boolean;
        function GetDiskGeometry_XP_and_Up (const hDisk: THandle;
                                            out iSize: Int64;
                                            out Style: TPartitionStyle;
                                            out MediaType: MEDIA_TYPE) : Boolean;
        function GetDiskDetails (const hDrive: THandle;
        						 var DD: TDiskDrive) : Boolean;

      public
      	constructor Create (const bIncludeRemovableDisks: Boolean = false;
        					const bRefreshNow: Boolean = false;
                            const bCheckForAdminRights: Boolean = true);
        destructor Destroy; override;

        procedure Refresh;
        function GetEnumerator : TDiskDrivesEnumerator;

        property Count : Integer read GetCount;
      	property Disks [iIndex: Integer] : TDiskDrive read GetDrive; default;
        property IncludeRemovableDisks : Boolean read FIncludeRemovableDisks
        										 write FIncludeRemovableDisks;
        property LastError : String read FLastError;
    end; { TDiskDrives }

function FormatSize (const iSize: Int64) : String;

implementation

uses SysUtils, SysConst,
{$IFNDEF FPC}
    {$IFNDEF DELPHI7_UP}
	     , D7_CompatibilityU,
    {$ENDIF}
{$ENDIF}
	 VerifyU, Win32ToolsU, PasTools;

ResourceString
	cBusTypeUnknown = 'Unknown';                    // BusTypeUnknown = 0
    cBusTypeScsi 	= 'SCSI';                       // BusTypeScsi = 1
    cBusTypeAtapi 	= 'ATAPI';                      // BusTypeAtapi = 2
    cBusTypeAta 	= 'ATA';                        // BusTypeAta = 3
    cBusType1394 	= 'IEEE-1394';                  // BusType1394 = 4
    cBusTypeSsa 	= 'SSA';                        // BusTypeSsa = 5
    cBusTypeFibre 	= 'Fibre Channel';              // BusTypeFibre = 6
    cBusTypeUsb 	= 'USB';                        // BusTypeUsb = 7
    cBusTypeRAID 	= 'RAID';                       // BusTypeRAID = 8
    cBusTypeiScsi 	= 'iSCSI';                      // BusTypeiScsi = 9
    cBusTypeSas		= 'Serial Attached SCSI (SAS)'; // BusTypeSas = 10
    cBusTypeSata 	= 'SATA';                       // BusTypeSata = 11
    cBusTypeSd		= 'SD';                         // BusTypeSd = 12
    cBusTypeMmc 	= 'MMC';                        // BusTypeMmc = 13
    cBusTypeVirtual = 'Virtual';                    // BusTypeVirtual = 14
	cBusTypeFileBackedVirtual = 'FileBackedVirtual'; // BusTypeFileBackedVirtual = 15
    cBusTypeSpaces 	= 'Spaces';  					// BusTypeSpaces = 16
    cBusTypeNvme 	= 'Nvme';  						// BusTypeNvme = 17
    cBusTypeSCM 	= 'SCM';  						// BusTypeSCM = 18
    cBusTypeUfs	    = 'Ufs';  						// BusTypeUfs = 19
    cBusTypeMax		= 'Max'; 						// BusTypeMax = 20
//  cBusTypeMaxReserved = $7F;

    cPlatformMsg = '"TDiskDrives": Windows 9x / ME are not supported';

{$IFDEF FPC}
const
    // Max number of drives assuming primary/secondary, master/slave topology
	MAX_IDE_DRIVES = 16;

    GPT_BASIC_DATA_ATTRIBUTE_NO_DRIVE_LETTER = DWORD64($8000000000000000);
	IOCTL_STORAGE_QUERY_PROPERTY = $2D1400;
    PARTITION_Windows_RE	    = $27; // Windows RE hidden partition

type
    STORAGE_PROPERTY_QUERY = packed record
      PropertyId: DWord;
      QueryType: DWord;
      AdditionalParameters: array[0..3] of Byte;
    end;

    STORAGE_DEVICE_DESCRIPTOR = packed record
      Version: ULONG;
      Size: ULONG;
      DeviceType: Byte;
      DeviceTypeModifier: Byte;
      RemovableMedia: Boolean;
      CommandQueueing: Boolean;
      VendorIdOffset: ULONG;
      ProductIdOffset: ULONG;
      ProductRevisionOffset: ULONG;
      SerialNumberOffset: ULONG;
      STORAGE_BUS_TYPE: DWord;
      RawPropertiesLength: ULONG;
      RawDeviceProperties: array[0..511] of Byte;
    end;
{$ENDIF}

(* ---- *)

function FormatSize (const iSize: Int64) : String;

begin
	if (iSize = 0) then
    	Result := ''
	else if (iSize >= cOne_GB) then
        Result := Format ('%.1f GB', [iSize / (cOne_GB * 1.0)])
    else if (iSize > c10_MB) then
    	Result := Format ('%d MB', [iSize div cOne_MB])
    else Result := Format ('%.1f MB', [iSize / (cOne_MB * 1.0)])
end; { FormatSize }

(* ---- *)

function GetBusType (const dwBusType: DWord) : TBusType;

begin
	case dwBusType of
        1  : Result := btScsi;
        2  : Result := btAtapi;
        3  : Result := btAta;
        4  : Result := bt1394;
        5  : Result := btSsa;
        6  : Result := btFibre;
        7  : Result := btUsb;
        8  : Result := btRAID;
        9  : Result := btiScsi;
        10 : Result := btSas;
        11 : Result := btSata;
        12 : Result := btSd;
        13 : Result := btMmc;
        14 : Result := btVirtual;
        15 : Result := btFileBackedVirtual;
        16 : Result := btSpaces;
        17 : Result := btNvme;
        18 : Result := btSCM;
        19 : Result := btUfs;
        20 : Result := btMax;
    	else Result := btUnknown;
    end; { case True of }
end; { GetBusType }

(* ---- *)

function GetBusTypeString (const BusType: TBusType) : String;

begin
	case BusType of
        btScsi  : Result := cBusTypeScsi;
        btAtapi  : Result := cBusTypeAtapi;
        btAta  : Result := cBusTypeAta;
        bt1394  : Result := cBusType1394;
        btSsa  : Result := cBusTypeSsa;
        btFibre  : Result := cBusTypeFibre;
        btUsb  : Result := cBusTypeUsb;
        btRAID  : Result := cBusTypeRAID;
        btiScsi : Result := cBusTypeiScsi;
        btSas : Result := cBusTypeSas;
        btSata : Result := cBusTypeSata;
        btSd : Result := cBusTypeSd;
        btMmc : Result := cBusTypeMmc;
        btVirtual : Result := cBusTypeVirtual;
        btFileBackedVirtual : Result := cBusTypeFileBackedVirtual;
        btSpaces : Result := cBusTypeSpaces;
        btNvme : Result := cBusTypeNvme;
        btSCM : Result := cBusTypeSCM;
        btUfs : Result := cBusTypeUfs;
        btMax : Result := cBusTypeMax;
    	else Result := cBusTypeUnknown;
    end; { case True of }
end; { GetBusTypeString }

(* ---- *)

function SerialNoToStr (const sSerialNo: String) : String;

var
	iStrLen, iIndex, iHex : Integer;
//    chTemp : Char;

begin
    Result := '';

    iStrLen := Length (sSerialNo);

    if (Odd (iStrLen)) then
    	exit;

    iIndex := 1;

    while (iIndex < iStrLen) do
    begin
        iHex := StrToIntDef ('$' + Copy (sSerialNo, iIndex, 2), (-1));

        if (iHex = (-1)) then
        begin
			Result := sSerialNo;
            Break;
        end; { if }

        Result := Result {%H-}+ Chr (iHex);
        Inc (iIndex, 2);
    end; { while }

    Result := Trim (Result);

    for iIndex := 1 to Length (Result) do
    	if (Byte (Result [iIndex]) < 32) or (Byte (Result [iIndex]) > 127) then
        begin
			Result := '';
            exit;
        end; { if }

(**
    iIndex := 1;

    while (iIndex < Length (Result)) do
    begin
        chTemp := Result [iIndex];
        Result [iIndex] := Result [iIndex + 1];
        Result [iIndex + 1] := chTemp;
        Inc(iIndex, 2);
    end; { while }
**)
end; { SerialNoToStr }

(* ---- *)

constructor TLogicalDisk.Create (const Partition: TPartition);
begin
	Inherited Create;

    FParentPartition := Partition;
end; { TLogicalDisk.Create }

(* ---- *)

constructor TPartition.Create (const Drive: TDiskDrive);
begin
	Inherited Create;

    FParentDrive := Drive;
end; { TPartition.Create }

(* ---- *)

destructor TPartition.Destroy;
begin
	if (FLogicalDisk <> NIL) then
    	FLogicalDisk.Free;

	inherited;
end; { TPartition.Destroy }

(* ---- *)

function TPartition_GPT.GetGptAttributes : String;

	(* ---- *)

    procedure CheckAttribute (const Attribute: TGptAttribute;
    						  const sAttribute: String);
    begin
    	Assert (sAttribute <> '');

		if (Attribute in FGptAttributes) then
        	if (Result = '') then
            	Result := sAttribute
            else Result := Result + ', ' + sAttribute;
    end; { CheckAttribute }

    (* ---- *)

begin { TPartition_GPT.GetGptAttributes }
    Result := '';

	CheckAttribute (gaPlatformRequired, 'PlatformRequired');
	CheckAttribute (gaNoDriveLetter, 'gaNoDriveLetter');
	CheckAttribute (gaHidden, 'gaHidden');
	CheckAttribute (gaShadowCopy, 'ShadowCopy');
	CheckAttribute (gaReadOnly, 'ReadOnly');
end; { TPartition_GPT.GetGptAttributes }

(* ---- *)

function TPartition_GPT.GetPartitionId : String;
begin
	Result := GuidToString (FPartitionId);
end; { TPartition_GPT.GetPartitionId }

(* ---- *)

function TPartition_GPT.GetPartitionType : String;
begin
	Result := GuidToString (FPartitionType);
end; { TPartition_GPT.GetPartitionType }

(* ---- *)

constructor TDiskDriveEnumerator.Create (const DiskDrive: TDiskDrive);
begin
    inherited Create;

    FIndex := (-1);
    FDiskDrive := DiskDrive;
end; { TDiskDriveEnumerator.Create }

(* ---- *)

function TDiskDriveEnumerator.GetCurrent : TPartition;
begin
	Result := FDiskDrive.FaPartitions [FIndex];
end; { TDiskDriveEnumerator.GetCurrent }

(* ---- *)

function TDiskDriveEnumerator.MoveNext : Boolean;
begin
	if (FIndex < High (FDiskDrive.FaPartitions)) then
    begin
        Result := true;
    	Inc (FIndex);
    end { if }
    else Result := false;
end; { TDiskDriveEnumerator.MoveNext }

(* ---- *)

function TDiskDrive.GetPartition (iIndex: Integer) : TPartition;
begin
	if (iIndex < (0)) or (iIndex >= Length (FaPartitions)) then
    	raise ERangeError.Create (SRangeError);

	Result := FaPartitions [iIndex];
end; { TDiskDrive.GetPartition }

(* ---- *)

function TDiskDrive.GetPartionCount : Integer;
begin
	Result := Length (FaPartitions);
end; { TDiskDrive.GetPartionCount }

(* ---- *)

constructor TDiskDrive.Create (const iDiskIndex: Integer);
begin
	inherited Create;

    FDiskIndex := iDiskIndex;
end; { TDiskDrive.Create }

(* ---- *)

destructor TDiskDrive.Destroy;

var
	iPartition : Integer;

begin
    if (Length (FaPartitions) > 0) then
        for iPartition := 0 to High (FaPartitions) do
            FaPartitions [iPartition].Free;

	inherited;
end; { TDiskDrive.Destroy }

(* ---- *)

function TDiskDrive.GetEnumerator : TDiskDriveEnumerator;
begin
	Result := TDiskDriveEnumerator.Create (Self);
end; { TDiskDrive.GetEnumerator }

(* ---- *)

function TDiskDrive.GetMediaType : String;
begin
	case FMediaType of
      	Unknown : Result := 'Unknown';
        RemovableMedia : Result := 'Removable Media';
        FixedMedia : Result := 'Fixed Hard Disk';
	    else Result := 'Floppy';
    end; { case }
end; { TDiskDrive.GetMediaType }

(* ---- *)

function TDiskDrive.GetPartitionStyle (const Style: TPartitionStyle) : String;
begin
    case Style of
        psMBR : Result := 'MBR';
        psGPT : Result := 'GPT';
        psRaw : Result := 'RAW';
        else Result := '';
    end; { case PartitionStyle of }
end; { TDiskDrive.GetPartitionStyle }

(* ---- *)

function TDiskDrive_GPT.GetDiskId : String;
begin
	Result := GUIDToString (FDiskId);
end; { TDiskDrive_GPT.GetDiskId }

(* ---- *)

procedure TDiskDrive_GPT.GetPartitionInfo (const hDisk: THandle);

	(* ---- *)

    procedure SetGptAttribues (const Partition_GPT: TPartition_GPT;
    						   const dwAttr: DWord64);
    begin
    	if ((dwAttr and GPT_ATTRIBUTE_PLATFORM_REQUIRED) <> 0) then
        	Include (Partition_GPT.FGptAttributes, gaPlatformRequired);

    	if ((dwAttr and GPT_BASIC_DATA_ATTRIBUTE_NO_DRIVE_LETTER) <> 0) then
        	Include (Partition_GPT.FGptAttributes, gaNoDriveLetter);

    	if ((dwAttr and GPT_BASIC_DATA_ATTRIBUTE_HIDDEN) <> 0) then
        	Include (Partition_GPT.FGptAttributes, gaHidden);

    	if ((dwAttr and GPT_BASIC_DATA_ATTRIBUTE_SHADOW_COPY) <> 0) then
        	Include (Partition_GPT.FGptAttributes, gaShadowCopy);

    	if ((dwAttr and GPT_BASIC_DATA_ATTRIBUTE_READ_ONLY) <> 0) then
        	Include (Partition_GPT.FGptAttributes, gaReadOnly);
    end; { SetGptAttribues }

    (* ---- *)

(**
const
	cRecoveryPartition = '{DE94BBA4-06D1-4D40-A16A-BFD50179D6AC}';
**)

var
    iIndex, iSize, iCount : Integer;
    pDriveLayoutInfo : PDriveLayoutInformationEx;
    dwBytesReturned : DWord;
    Partition_GPT : TPartition_GPT;
    PE : PPARTITION_INFORMATION_EX;

begin { TDiskDrive_GPT.GetPartitionInfo }
    iSize := SizeOf (TDriveLayoutInformationEx) +
             (MAX_IDE_DRIVES * SizeOf (TPartitionInformationGpt));

    GetMem (pDriveLayoutInfo, iSize);

    try
        Win32Check (DeviceIoControl (hDisk, IOCTL_DISK_GET_DRIVE_LAYOUT_EX, NIL,
        							 0, pDriveLayoutInfo, iSize,
                                     dwBytesReturned{%H-}, NIL));

        Assert (pDriveLayoutInfo^.PartitionStyle = DWord (PARTITION_STYLE_GPT));

        FDiskId := pDriveLayoutInfo^.Union.Gpt.DiskId;

        if (pDriveLayoutInfo^.PartitionCount > 0) then
        begin
            SetLength (FaPartitions, pDriveLayoutInfo^.PartitionCount);
            iCount := 0;

            for iIndex := 0 to pDriveLayoutInfo^.PartitionCount - 1 do
            begin
            	Partition_GPT := TPartition_GPT.Create (Self);
{$IFDEF DEBUG}
	{$R-}
{$ENDIF}
				PE := @pDriveLayoutInfo^.PartitionEntry [iIndex];

                Partition_GPT.FPartitionNumber := PE^.PartitionNumber;
                Partition_GPT.FPartitionSize := PE^.PartitionLength.QuadPart;
                Partition_GPT.FPartitionType := PE^.GPT.PartitionType;
                Partition_GPT.FPartitionId := PE^.GPT.PartitionId;
                Partition_GPT.FPartitionAttributes := PE^.GPT.Attributes;
                Partition_GPT.FPartitionName := String (PE^.GPT.Name);

				SetGptAttribues (Partition_GPT, PE^.GPT.Attributes);
{$IFDEF DEBUG}
	{$R+}
{$ENDIF}
                FaPartitions [iCount] := Partition_GPT;

				Inc (iCount);
			end; { for }

            SetLength (FaPartitions, iCount);
        end; { if }

    finally
    	FreeMem (pDriveLayoutInfo, iSize);
    end; { try / finally }
end; { TDiskDrive_GPT.GetPartitionInfo }

(* ---- *)

constructor TDiskDrive_GPT.Create (const iDiskIndex: Integer);
begin
	inherited;

    FPartitionStyle := psGPT;
end; { TDiskDrive_GPT.Create }

(* ---- *)

procedure TDiskDrive_MBR.GetPartitionInfo (const hDisk: THandle);
begin
	if (CheckWin32Version (5, 1)) then
    	GetPartitionInfo_XP_and_Up (hDisk)  // XP and better
    else GetPartitionInfo_NT4_W2K (hDisk);
end; { TDiskDrive_MBR.GetPartitionInfo }

(* ---- *)

procedure TDiskDrive_MBR.GetPartitionInfo_NT4_W2K (const hDisk: THandle);

const
    cSize = SizeOf (TDriveLayoutInformation) +
            (MAX_IDE_DRIVES * SizeOf (PARTITION_INFORMATION));

var
    pDriveLayoutInfo : PDriveLayoutInformation;
    dwBytesReturned, dwNumber : DWord;
    iCount, iIndex : Integer;
    Partition_MBR : TPartition_MBR;
    Partition_Unknown : TPartition_MBR_Unknown;
    sPartitionType : String;

begin
    GetMem (pDriveLayoutInfo, cSize);

    try
        FillChar (pDriveLayoutInfo^, cSize, #0);

        Win32Check (DeviceIoControl (hDisk, IOCTL_DISK_GET_DRIVE_LAYOUT, NIL, 0,
                                 	 pDriveLayoutInfo, cSize,
                                     dwBytesReturned{%H-}, NIL));

        FDiskSignature := IntToHex (pDriveLayoutInfo^.Signature, 8);

        if (pDriveLayoutInfo^.PartitionCount > 0) then
        begin
            SetLength (FaPartitions, pDriveLayoutInfo^.PartitionCount);
            iCount := 0;

            for iIndex := 0 to pDriveLayoutInfo^.PartitionCount - 1 do
            begin
{$IFDEF DEBUG}
	{$R-}
{$ENDIF}
                with pDriveLayoutInfo^.PartitionEntry [iIndex] do
                begin
                	if (PartitionType = PARTITION_ENTRY_UNUSED) then
                    	Continue;

                    dwNumber := PartitionNumber;

                    if (GetPartitionType (PartitionType, sPartitionType)) then
                    begin
                        Partition_MBR := TPartition_MBR.Create (Self);
                        Partition_MBR.FPartitionTypeByte := PartitionType;

                        with Partition_MBR do
                        begin
                            FBootable := BootIndicator;
                            FPartitionType := sPartitionType;
                            FPartitionNumber := dwNumber;
                            FPartitionSize := PartitionLength.QuadPart;
                            FExtendedPartition :=
                            			 PartitionTypeByte = PARTITION_EXTENDED;
                        end; { with }

						FaPartitions [iCount] := Partition_MBR;
                    end { if }
                    else
                    begin
                    	Partition_Unknown :=
                        				   TPartition_MBR_Unknown.Create (Self);

                        Partition_Unknown.FPartitionTypeByte := PartitionType;

                        with Partition_Unknown do
                        begin
	                        FPartitionSize := PartitionLength.QuadPart;
                            FPartitionNumber := dwNumber;
                            FPartitionType := sPartitionType;
                        end; { with }

						FaPartitions [iCount] := Partition_Unknown;
                    end; { else }

                    Inc (iCount);
                end; { with }
{$IFDEF DEBUG}
	{$R+}
{$ENDIF}
			end; { for }

            SetLength (FaPartitions, iCount);
        end; { if }

    finally
        FreeMem (pDriveLayoutInfo, cSize);
    end; { try / finally }
end; { TDiskDrive_MBR.GetPartitionInfo_NT4_W2K }

(* ---- *)

procedure TDiskDrive_MBR.GetPartitionInfo_XP_and_Up (const hDisk: THandle);

	(* ---- *)

    function EntryForPartitionExists (const iNumber: Integer) : Boolean;

    var
    	iIndex : Integer;

    begin
    	Result := false;

    	for iIndex := 0 to High (FaPartitions) do
        	if (FaPartitions [iIndex] = NIL) then
        		Break
            else
                if (FaPartitions [iIndex].FPartitionNumber = iNumber) then
                begin
                    Result := true;
                    Break;
                end; { if }
    end; { EntryForPartitionExists }

    (* ---- *)

const
    cSize = SizeOf (TDriveLayoutInformationEx) +
            SizeOf (TDriveLayoutInformationGpt) +
             (MAX_IDE_DRIVES *
              (SizeOf (TPartitionInformationEx) +
               SizeOf (TPartitionInformationGpt)));

var
    iIndex, iCount : Integer;
    pDriveLayoutInfo : PDriveLayoutInformationEx;
    dwSize, dwBytesReturned : DWord;
    Partition_MBR : TPartition_MBR;
    Partition_Unknown : TPartition_MBR_Unknown;
    PE : PPARTITION_INFORMATION_EX;
    sPartitionType : String;
    bRecognizedPartition : Boolean;

begin { TDiskDrive_MBR.GetPartitionInfo_XP_and_Up }
    iIndex := 1;
    dwSize := iIndex * cSize;
    pDriveLayoutInfo := AllocMem (dwSize);

    try
        while (DeviceIoControl (hDisk, IOCTL_DISK_GET_DRIVE_LAYOUT_EX, NIL,
                                0, pDriveLayoutInfo, dwSize, dwBytesReturned{%H-},
                                NIL) = false) do
            if (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
            begin
                FreeMem (pDriveLayoutInfo);
                Inc (iIndex);
                dwSize := iIndex * cSize;
                pDriveLayoutInfo := AllocMem (dwSize);
            end { if }
            else RaiseLastOSError;

        Assert (pDriveLayoutInfo^.PartitionStyle = DWord (PARTITION_STYLE_MBR));

        FDiskSignature := IntToHex (pDriveLayoutInfo^.Union.Mbr.Signature, 8);

        if (pDriveLayoutInfo^.PartitionCount > 0) then
        begin
            SetLength (FaPartitions, pDriveLayoutInfo^.PartitionCount);
            iCount := 0;

            for iIndex := 0 to pDriveLayoutInfo^.PartitionCount - 1 do
            begin
{$IFDEF DEBUG}
	{$R-}
{$ENDIF}
				PE := @pDriveLayoutInfo^.PartitionEntry [iIndex];

                if (PE^.Mbr.PartitionType = PARTITION_ENTRY_UNUSED) then
                    Continue;

                if (EntryForPartitionExists (PE^.PartitionNumber)) then
                	Continue;

                GetPartitionType (PE^.Mbr.PartitionType, sPartitionType);

                bRecognizedPartition := PE^.Mbr.RecognizedPartition;

                if not (bRecognizedPartition) then
                	if (PE^.PartitionNumber = 0) and
                       (IsExtended (PE^.Mbr.PartitionType)) then
                       	bRecognizedPartition := true;

                if (bRecognizedPartition) then
                begin
                    Partition_MBR := TPartition_MBR.Create (Self);

                    with Partition_MBR do
                    begin
                        FBootable := PE^.Mbr.BootIndicator;
                        FPartitionNumber := PE^.PartitionNumber;
                        FPartitionSize := PE^.PartitionLength.QuadPart;
                        FPartitionType := sPartitionType;
                        FPartitionTypeByte := PE^.Mbr.PartitionType;
                        FExtendedPartition := IsExtended (PartitionTypeByte);
                    end; { with }

                    FaPartitions [iCount] := Partition_MBR;
                end { if }
                else
                begin
                    Partition_Unknown := TPartition_MBR_Unknown.Create (Self);

                    with Partition_Unknown do
                    begin
                        FPartitionSize := PE^.PartitionLength.QuadPart;
                        FPartitionNumber := PE^.PartitionNumber;
                        FPartitionType := sPartitionType;
						FPartitionTypeByte := PE^.Mbr.PartitionType;
                    end; { with }

                    FaPartitions [iCount] := Partition_Unknown;
                end; { else }

                Inc (iCount);
{$IFDEF DEBUG}
	{$R+}
{$ENDIF}
			end; { for }

            SetLength (FaPartitions, iCount);
        end; { if }

    finally
    	FreeMem (pDriveLayoutInfo);
    end; { try / finally }
end; { TDiskDrive_MBR.GetPartitionInfo_XP_and_Up }

(* ---- *)

function TDiskDrive_MBR.GetPartitionType (const byType: Byte;
										  out sType: String) : Boolean;
begin
	Result := true;

    case byType of
        PARTITION_ENTRY_UNUSED : sType := 'unused';
        PARTITION_FAT_12 : sType := 'FAT 12';
        PARTITION_FAT_12 + $10 : sType := 'FAT 12 Hidden';
        PARTITION_XENIX_1 : sType := 'Xenix 1';
        PARTITION_XENIX_2 : sType := 'Xenix 2';
        PARTITION_FAT_16 : sType := 'FAT 16';
        PARTITION_FAT_16 + $10 : sType := 'FAT 12';
        PARTITION_EXTENDED : sType := 'Extended';
        PARTITION_HUGE : sType := 'Huge';
        PARTITION_HUGE + $10 : sType := 'Huge Hidden';
        PARTITION_IFS : sType := 'NTFS';
        PARTITION_IFS + $10 : sType := 'NTFS Hidden';
        PARTITION_OS2BOOTMGR : sType := 'OS2 Boot Manager';
        PARTITION_FAT32 : sType := 'FAT 32';
        PARTITION_FAT32 + $10 : sType := 'FAT 32 Hidden';
        PARTITION_FAT32_XINT13 : sType := 'FAT 32 Int13';
        PARTITION_FAT32_XINT13 + $10 : sType := 'FAT 32 Int13 Hidden';
        PARTITION_XINT13 : sType := 'Int13';
        PARTITION_XINT13 + $10 : sType := 'Int13 Hidden';
        PARTITION_XINT13_EXTENDED : sType := 'Int13 Extended';
        PARTITION_XINT13_EXTENDED + $10 : sType := 'Int13 Extended Hidden';
        PARTITION_Windows_RE : sType := 'Windows RE hidden partition';
        PARTITION_PREP : sType := 'Prep';
        PARTITION_LDM : sType := 'LDM';
        PARTITION_UNIX : sType := 'Unix';
        else
        begin
        	Result := false;
        	sType := Format ('unknown (0x%x)', [byType]);
        end; { else }
    end; { case }
end; { TDiskDrive_MBR.GetPartitionType }

(* ---- *)

function TDiskDrive_MBR.IsExtended (const byPartitionType: Byte) : Boolean;
begin
	Result := byPartitionType in [PARTITION_EXTENDED, PARTITION_XINT13,
    							  PARTITION_XINT13_EXTENDED];
end; { TDiskDrive_MBR.IsExtended }

(* ---- *)

constructor TDiskDrive_MBR.Create (const iDiskIndex: Integer);
begin
	inherited;

    FPartitionStyle := psMBR;
end; { TDiskDrive_MBR.Create }

(* ---- *)

procedure TDiskDrive_RAW.GetPartitionInfo (const hDisk: THandle);
begin
	{ Tut nix }
end; { TDiskDrive_RAW.GetPartitionInfo }

(* ---- *)

constructor TDiskDrive_RAW.Create (const iDiskIndex: Integer);
begin
	inherited;

    FPartitionStyle := psRaw;
end; { TDiskDrive_RAW.Create }

(* ---- *)

constructor TDiskDrivesEnumerator.Create (const DiskDrives: TDiskDrives);
begin
	inherited Create;

    FIndex := (-1);
    FDiskDrives := DiskDrives;
end; { TDiskDrivesEnumerator.Create }

(* ---- *)

function TDiskDrivesEnumerator.GetCurrent : TDiskDrive;
begin
	Result := FDiskDrives.FDrives [FIndex];
end; { TDiskDrivesEnumerator.GetCurrent }

(* ---- *)

function TDiskDrivesEnumerator.MoveNext : Boolean;
begin
	if (FIndex < High (FDiskDrives.FDrives)) then
    begin
		Result := true;
        Inc (FIndex);
    end { if }
    else Result := false;
end; { TDiskDrivesEnumerator.MoveNext }

(* ---- *)

procedure TDiskDrives.FillDiskDrivesArray (const bUserIsAdmin: Boolean);

	(* ---- *)

    function GetInfo_W2K_NT4 (const iDisk: Integer; const hDisk: THandle;
    						  out DiskDrive: TDiskDrive) : Boolean;

    var
		iDiskSize : Int64;
        MediaType : TMediaType;

    begin
    	Result := false;

        if (GetDiskGeometry_NT4_W2K (hDisk, iDiskSize, MediaType)) then
        begin
        	if (MediaType <> FixedMedia) and
               (FIncludeRemovableDisks = false) then
            	exit;

	    	DiskDrive := TDiskDrive_MBR.Create (iDisk);
            DiskDrive.FDiskSize := iDiskSize;
            DiskDrive.FMediaType := MediaType;

            try
                if (bUserIsAdmin) then
                    DiskDrive.GetPartitionInfo (hDisk);

				Result := true;

            except
                on E: Exception do
                begin
                    DiskDrive.Free;
                    raise;
                end; { on E: Exception do }
            end; { try / except }
        end; { if }
    end; { GetInfo_W2K_NT4 }

    (* ---- *)

    function GetInfo_XP_and_Up (const iDisk: Integer; const hDisk: THandle;
    							out DiskDrive: TDiskDrive) : Boolean;

    var
		iDiskSize : Int64;
        PartitionStyle : TPartitionStyle;
		MediaType : MEDIA_TYPE;

    begin
    	Result := false;

    	DiskDrive := NIL;

		if (GetDiskGeometry_XP_and_Up (hDisk, iDiskSize, PartitionStyle,
        							   MediaType)) then
        begin
        	if (MediaType <> FixedMedia) and
               (FIncludeRemovableDisks = false) then
            	exit;

            case PartitionStyle of
                psMBR : DiskDrive := TDiskDrive_MBR.Create (iDisk);
                psGPT : DiskDrive := TDiskDrive_GPT.Create (iDisk);
                psRaw : DiskDrive := TDiskDrive_Raw.Create (iDisk);
            end; { case PartitionStyle of }

            DiskDrive.FDiskSize := iDiskSize;
            DiskDrive.FMediaType := MediaType;

            try
                DiskDrive.GetPartitionInfo (hDisk);
				Result := true;

            except
                on E: Exception do
                begin
                    DiskDrive.Free;
                    raise;
                end; { on E: Exception do }
            end; { try / except }
        end; { if }
    end; { GetInfo_XP_and_Up }

    (* ---- *)

const
	cFileTemplate = '\\.\PHYSICALDRIVE%d';

var
	iIndex, iCount : Integer;
	sDisk : String;
    hDisk : THandle;
    bResult : Boolean;
    DiskDrive : TDiskDrive;

begin { TDiskDrives.FillDiskDrivesArray }
    iCount := 0;
    SetLength (FDrives, MAX_IDE_DRIVES);

    try
        for iIndex := 0 to MAX_IDE_DRIVES - 1 do
        begin
            sDisk := Format (cFileTemplate, [iIndex]);

            hDisk := CreateFile (PChar (sDisk),
                                 iif (bUserIsAdmin, GENERIC_READ, 0),
                                 FILE_SHARE_WRITE, NIL, OPEN_EXISTING,
                                 FILE_ATTRIBUTE_NORMAL or FILE_FLAG_NO_BUFFERING,
                                 0);

            if (hDisk <> INVALID_HANDLE_VALUE) then
                try
                    if (CheckWin32Version (5, 1)) then
                        bResult := GetInfo_XP_and_Up (iIndex, hDisk, DiskDrive)
                    else bResult := GetInfo_W2K_NT4 (iIndex, hDisk, DiskDrive);

                    if (bResult) then
                    begin
                    	DiskDrive.FDevicePath := Copy (sDisk, 5,
                                                       Length (sDisk) - 4);

                        if (Win32MajorVersion >= 5) then
                        	GetDiskDetails (hDisk, DiskDrive);

                    	FDrives [iCount] := DiskDrive;
                        Inc (iCount);
                    end; { if }

                finally
                    VerifyApi (CloseHandle (hDisk));
                end { try / finally }
            else
                // Code 2 = 'The system cannot find the file specified'
                if (GetLastError <> 2) then
                    RaiseLastOSError;
        end; { for }

    finally
	    SetLength (FDrives, iCount);
    end; { try / finally }
end; { TDiskDrives.FillDiskDrivesArray }

(* ---- *)

procedure TDiskDrives.FillVolumeList (const bUserIsAdmin: Boolean);

	(* ---- *)

    procedure RetrieveFixedDrives (const VolumeList: TStringList);

    var
        iIndex : Integer;
        sDrive : String;
        bAddDrive : Boolean;

    begin
    	for iIndex := Byte ('A') to Byte ('Z') do
        begin
			sDrive := Format ('%s:', [Char (iIndex)]);

            if (FIncludeRemovableDisks) then
            	bAddDrive := GetDriveType (Char (iIndex)) in
                										  [dtFixed, dtRemovable]
            else bAddDrive := GetDriveType (Char (iIndex)) = dtFixed;

        	if (bAddDrive) then
            	VolumeList.Add (sDrive);
        end; { for }
    end; { RetrieveFixedDrives }

    (* ---- *)

    function GetDeviceNumber (const sDrive: String;
    						  out iDeviceNo, iPartitionNo: Integer) : Boolean;

    const
    	cFileName = '\\.\%s';

    var
        hDevice : THandle;
        dwBytesReturned : DWord;
        StorageDeviceNo : TStorageDeviceNumber;

    begin
    	Assert (sDrive <> '');

    	Result := false;

        hDevice := CreateFile (PChar (Format (cFileName, [sDrive])),
        					   iif (bUserIsAdmin, GENERIC_READ, 0),
                               FILE_SHARE_READ or FILE_SHARE_WRITE,
                               NIL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN,
                               0);

        if (hDevice = INVALID_HANDLE_VALUE) then
        	exit;

        FillChar (StorageDeviceNo{%H-}, SizeOf (TStorageDeviceNumber), #0);

        if (DeviceIoControl (hDevice, IOCTL_STORAGE_GET_DEVICE_NUMBER, NIL, 0,
                     		 @StorageDeviceNo, SizeOf (TStorageDeviceNumber),
                     		 dwBytesReturned{%H-}, NIL)) then
        begin
			Result := true;
            iDeviceNo := StorageDeviceNo.DeviceNumber;
            iPartitionNo := StorageDeviceNo.PartitionNumber;
        end; { if }

        VerifyApi (CloseHandle (hDevice));
    end; { GetDeviceNumber }

    (* ---- *)

	function GetVolumeInfo (var LogicalDisk: TLogicalDisk) : Boolean;

	var
		dwVolumeSerialNo, lpMaximumComponentLength, lpFileSystemFlags : DWord;

	begin
		with LogicalDisk do
		begin
			SetLength (FVolumeName, MAX_PATH);
			SetLength (FFileSystem, MAX_PATH);

			Result := GetVolumeInformation (PChar (FDriveLetter + '\'),
											PChar (FVolumeName), MAX_PATH,
											PDWORD (@dwVolumeSerialNo),
											lpMaximumComponentLength{%H-},
											lpFileSystemFlags{%H-},
											PChar (FFileSystem), MAX_PATH);

			if (Result) then
			begin
				SetLength (FVolumeName, lstrlen (PChar (FVolumeName)));
				SetLength (FFileSystem, lstrlen (PChar (FFileSystem)));

				FLogicalDiskSize := DiskSize (Byte (FDriveLetter [1]) - 64);
				FFreeSpace := DiskFree (Byte (FDriveLetter [1]) - 64);
			end { if }
			else
			begin
				FVolumeName := '';
				FFileSystem := '';

				FLogicalDiskSize := (-1);
				FFreeSpace := (-1);
			end; { else }
		end; { with }
	end; { GetVolumeInfo }

	(* ---- *)

	procedure SetLogicalDisk (const sDrive: String;
							  const iDisk, iPartition: Integer);

    	(* ---- *)

        function GetPartitionIndex (const aPartitions: TaPartitions;
        							out iIndex: Integer) : Boolean;
        begin
            Result := false;

			iIndex := 0;

            while (iIndex <= High (aPartitions)) do
            begin
                if (aPartitions [iIndex].FPartitionNumber = iPartition) then
                begin
                	Result := true;
                    Break;
                end; { if }

                Inc (iIndex);
            end; { while }
        end; { GetPartitionIndex }

        (* ---- *)

        function GetDiskIndex (out iIndex: Integer) : Boolean;
        begin
            Result := false;

			iIndex := 0;

            while (iIndex <= High (FDrives)) do
            begin
                if (FDrives [iIndex].FDiskIndex = iDisk) then
                begin
                	Result := true;
                    Break;
                end; { if }

                Inc (iIndex);
            end; { while }
        end; { GetDiskIndex }

        (* ---- *)

	var
		LogicalDisk : TLogicalDisk;
		Partition : TPartition;
        iPartitionIndex, iDiskIndex : Integer;
        aPartitions : TaPartitions;

	begin { SetLogicalDisk }
		Assert (sDrive <> '');

        SetLength (aPartitions{%H-}, 0);

        if not (GetDiskIndex (iDiskIndex)) then
        	exit;

        aPartitions := FDrives [iDiskIndex].FaPartitions;

        if not (GetPartitionIndex (aPartitions, iPartitionIndex)) then
        	exit;

		Partition := aPartitions [iPartitionIndex];

        if (Partition is TPartition_MBR) then
            if ((Partition as TPartition_MBR).FExtendedPartition) then
			    exit;

		LogicalDisk := TLogicalDisk.Create (Partition);

		Partition.FLogicalDisk := LogicalDisk;

		LogicalDisk.FDriveLetter := sDrive;

		if (GetVolumeInfo (LogicalDisk) = false) then
            if (Partition is TPartition_MBR) then
                with LogicalDisk do
                begin
                    FVolumeName := '';
                    FFileSystem := (Partition as TPartition_MBR).FPartitionType;

                    if (Partition.PartitionSize > 0) then
                        FLogicalDiskSize := Partition.PartitionSize;
                end; { with }
	end; { SetLogicalDisk }

	(* ---- *)

	procedure MapLogicalDisksToPartitions (const VolumeList: TStringList);

	var
		iDeviceNo, iPartitionNo, iIndex : Integer;

	begin
        for iIndex := 0 to VolumeList.Count - 1 do
            if (GetDeviceNumber (VolumeList [iIndex],
            					 iDeviceNo, iPartitionNo)) then
                SetLogicalDisk (VolumeList [iIndex], iDeviceNo, iPartitionNo);
    end; { TDiskDrives.MapLogicalDisksToPartitions }

    (* ---- *)

var
	VolumeList : TStringList;

begin { TDiskDrives.FillVolumeList }
	if (Length (FDrives) = 0) then
    	exit;

	VolumeList := TStringList.Create;

    try
    	RetrieveFixedDrives (VolumeList);

        if (VolumeList.Count > 0) then
        	MapLogicalDisksToPartitions (VolumeList);

    finally
        VolumeList.Free;
    end; { try / finally }
end; { TDiskDrives.FillVolumeList }

(* ---- *)

function TDiskDrives.GetCount : Integer;
begin
	Result := Length (FDrives);
end; { TDiskDrives.GetCount }

(* ---- *)

function TDiskDrives.GetDrive (iIndex: Integer) : TDiskDrive;
begin
	if (iIndex < (0)) or (iIndex >= Length (FDrives)) then
    	raise ERangeError.Create (SRangeError);

    Result := FDrives [iIndex];
end; { TDiskDrives.GetDrive }

(* ---- *)

function TDiskDrives.GetDiskGeometry_NT4_W2K (const hDisk: THandle;
									       out iDiskSize: Int64;
                                           out MediaType: TMediaType) : Boolean;

var
    dwBytesReturned : DWord;
    DiskGeometry : TDiskGeometry;
    iCylinderSize : Int64;

begin
	Result := false;

    FillChar (DiskGeometry{%H-}, SizeOf (TDiskGeometry), #0);

    if (DeviceIoControl (hDisk, IOCTL_DISK_GET_DRIVE_GEOMETRY, NIL, 0,
    					 @DiskGeometry, SizeOf (TDiskGeometry), dwBytesReturned{%H-},
                         NIL)) then
    begin
    	Result := true;

        with DiskGeometry do
        begin
            iCylinderSize := Int64 (TracksPerCylinder) *
                             Int64 (SectorsPerTrack)  * Int64 (BytesPerSector);
            iDiskSize := Cylinders.QuadPart * iCylinderSize;
        end; { with }

        MediaType := DiskGeometry.MediaType;
    end { if }
    else
        if (GetLastError <> 21) then  // 21 = "The device is not ready"
            RaiseLastOSError;
end; { TDiskDrives.GetDiskGeometry_NT4_W2K }

(* ---- *)

function TDiskDrives.GetDiskGeometry_XP_and_Up (const hDisk: THandle;
										   out iSize: Int64;
                                           out Style: TPartitionStyle;
                                           out MediaType: MEDIA_TYPE) : Boolean;

var
    dwBytesReturned : DWord;
    pDiskGeometry : PDiskGeometryEx;
    dwBufSize : DWord;

begin
	Result := false;

	dwBufSize := SizeOf (DISK_GEOMETRY_EX) + SizeOf (DISK_PARTITION_INFO) +
    			 SizeOf (DISK_DETECTION_INFO);

    GetMem (pDiskGeometry, dwBufSize);

    try
        FillChar (pDiskGeometry^, dwBufSize, #0);

        if (DeviceIoControl (hDisk, IOCTL_DISK_GET_DRIVE_GEOMETRY_EX, NIL, 0,
        					 pDiskGeometry, dwBufSize, dwBytesReturned{%H-},
                             NIL)) then
        begin
        	Result := true;

            iSize := pDiskGeometry^.DiskSize.QuadPart;
            MediaType := pDiskGeometry^.Geometry.MediaType;

            with DiskGeometryGetPartition (pDiskGeometry)^ do
                case PartitionStyle of
                    PARTITION_STYLE_MBR : Style := psMBR;
                    PARTITION_STYLE_GPT : Style := psGPT;
                    else Style := psRaw;
                end; { case True of }
        end { if }
        else
            if (GetLastError <> 21) then  // 21 = "The device is not ready"
            	RaiseLastOSError;

    finally
    	FreeMem (pDiskGeometry, dwBufSize);
    end; { try / finally }
end; { TDiskDrives.GetDiskGeometry_XP_and_Up }

(* ---- *)

function TDiskDrives.GetDiskDetails (const hDrive: THandle;
									 var DD: TDiskDrive) : Boolean;

	(* ---- *)

    function GetDisplayName : String;
    begin
    	if (DD.FVendorId = '') then
        	Result := DD.FProductId
        else if (DD.FProductId = '') then
        	Result := DD.FVendorId
        else
        	if (CharInSet (UpCase (DD.FVendorId [Length (DD.FVendorId)]),
        				   ['A'..'Z', '0'..'9'])) then
            	Result := DD.FVendorId + ' ' + DD.FProductId
            else Result := DD.FVendorId + DD.FProductId;
    end; { GetDisplayName }

    (* ---- *)

type
	PCharArray = ^TCharArray;
  	TCharArray = array [0..32767] of AnsiChar;

var
    PropQuery : STORAGE_PROPERTY_QUERY;
    DevDescriptor : STORAGE_DEVICE_DESCRIPTOR;
    dwBytesReturned : DWord;
  	PCh : PAnsiChar;

begin { TDiskDrives.GetDiskDetails }
	Result := false;

    FillChar (PropQuery{%H-}, SizeOf (STORAGE_PROPERTY_QUERY), #0);
    FillChar (DevDescriptor{%H-}, SizeOf (STORAGE_DEVICE_DESCRIPTOR), #0);

    DevDescriptor.Size := SizeOf (STORAGE_DEVICE_DESCRIPTOR);

    if not (DeviceIoControl (hDrive, IOCTL_STORAGE_QUERY_PROPERTY, @PropQuery,
    					 	 SizeOf (STORAGE_PROPERTY_QUERY), @DevDescriptor,
                         	 DevDescriptor.Size, dwBytesReturned{%H-}, NIL)) then
    	exit;

    Result := true;

    DD.FBusType := GetBusType (DevDescriptor.STORAGE_BUS_TYPE);
    DD.FDiskInterface := GetBusTypeString (DD.FBusType);

    if (DevDescriptor.VendorIdOffset <> 0) then
    begin
        PCh := @PCharArray (@DevDescriptor)^[DevDescriptor.VendorIdOffset];
{$WARNINGS OFF}
        DD.FVendorId := TrimRight (String (PCh));
{$WARNINGS ON}
    end; { if }

    if (DevDescriptor.ProductIdOffset <> 0) then
    begin
        PCh := @PCharArray (@DevDescriptor)^[DevDescriptor.ProductIdOffset];
{$WARNINGS OFF}
        DD.FProductId := TrimRight (String (PCh));
{$WARNINGS ON}
    end; { if }

    if (DevDescriptor.ProductRevisionOffset <> 0) then
    begin
    	PCh := @PCharArray (@DevDescriptor)^[DevDescriptor.ProductRevisionOffset];
{$WARNINGS OFF}
      	DD.FProductRevision := TrimRight (String (PCh));
{$WARNINGS ON}
    end; { if }

    if (DevDescriptor.SerialNumberOffset <> 0) then
    begin
        PCh := @PCharArray (@DevDescriptor)^[DevDescriptor.SerialNumberOffset];
{$WARNINGS OFF}
        DD.FSerialNumber := SerialNoToStr (TrimRight (String (PCh)));
{$WARNINGS ON}
    end; { if }

    DD.FDisplayName := GetDisplayName;
end; { TDiskDrives.GetDiskDetails }

(* ---- *)

constructor TDiskDrives.Create (const bIncludeRemovableDisks: Boolean = false;
        						const bRefreshNow: Boolean = false;
                                const bCheckForAdminRights: Boolean = true);
begin
	inherited Create;

    FIncludeRemovableDisks := bIncludeRemovableDisks;
    Self.bCheckForAdminRights := bCheckForAdminRights;

    if (bRefreshNow) then
    	Refresh;
end; { TDiskDrives.Create }

(* ---- *)

destructor TDiskDrives.Destroy;

var
	iDisk : Integer;

begin
	for iDisk := 0 to High (FDrives) do
    	FDrives [iDisk].Free;

	Inherited;
end; { TDiskDrives.Destroy }

(* ---- *)

function TDiskDrives.GetEnumerator : TDiskDrivesEnumerator;
begin
	Result := TDiskDrivesEnumerator.Create (Self);
end; { TDiskDrives.GetEnumerator }

(* ---- *)

procedure TDiskDrives.Refresh;

var
    bElevated, bUserIsAdmin : Boolean;

begin
	if not (Win32Platform = VER_PLATFORM_WIN32_NT) then
    	raise Exception.Create (cPlatformMsg);

    if (bCheckForAdminRights) then
	    bUserIsAdmin := IsUserLocalAdmin (bElevated) and (bElevated)
    else bUserIsAdmin := false; 

    FillDiskDrivesArray (bUserIsAdmin);
    FillVolumeList (bUserIsAdmin);
end; { TDiskDrives.Refresh }

(* ---- *)

end.
