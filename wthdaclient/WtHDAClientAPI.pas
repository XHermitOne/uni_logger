{

	Callback Definitions

	Used by the DLL to pass control back to the Client Application
	Each callback must be explicitly enabled by calling one of the following
	exported API functions:
				EnableHDAErrorNotification (HDAERRORPROC lpCallback);
				EnableHDAShutdownNotification (HANDLE hConnect, HDASHUTDOWNPROC lpCallback);
				EnableHDAEventMsgs (HDAEVENTMSGPROC lpCallback);



	HDAERRORPROC
		Signals the application when an error is detected by the dll.  If this callback
		is not used  errors will generate a modal MessageBox which must be acknowledged
		by the user)
		prototype for the callback function is as follows:
		void CALLBACK EXPORT ErrorMsgCallback (DWORD hResult, char *pMsg)
		(the buffer supplied by the dll as pMsg is a temporary and data should be
		copied to a permanant buffer by the application)
	HDASHUTDOWNPROC
		Signals the application if the connected server requests a disconnect;
		The HANDLE parameter in the shutdown callback procedure identifies the connection.
	HDAEVENTMSGPROC
		Establishes a callback to the application for displaying
		Debug DCOM Event Messages.  A character buffer is returned as a parameter
		that contains a textual description of the message.
}

unit WtHDAClientAPI;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF windows}
  Windows, ActiveX, ComObj;
  {$ENDIF}

const
  { UpdateType Literals }
  HDAINSERT: Integer = 1;
  HDAREPLACE: Integer = 2;
  HDAINSERTORREPLACE: Integer =	4;

type

  HDAERRORPROC = procedure(); stdcall;
  HDASHUTDOWNPROC = procedure(); stdcall;
  HDAEVENTMSGPROC = procedure(); stdcall;

  VB_DATE = Double;
  PVB_DATE = ^VB_DATE;

  {
  WtHDAClientRevision() - This function simply returns a WORD revision indication
  }
  WtHDAClientRevision = function(): WORD; stdcall;

  {
   WtHDAClientCoInit() - This function initializes DCOM using default security settings
  }
  WtHDAClientCoInit = function(): Boolean; stdcall;

  {
   NumberOfHDAServers(...)
   returns the number of available OPC-HDA servers
		the server list is obtained from the OPCENUM component as
		supplied by OPC Foundation and MachineName may be used to
		obtain the list of servers from a remote machine
  }
  NumberOfHDAServers = function(MachineName: LPCSTR): Integer; stdcall;

  {
   GetHDAServerName(...)
   Used to iterate through the server list obtained with NumberOfHDAServers()
   User Buffer pointed to by pBuf is filled with the Server name at index of the Server List
   A returned value of FALSE indicates that the index is invalid.
  }
  GetHDAServerName = function(index: Integer; pBuff: PChar; BufSize: Integer): Boolean; stdcall;

  {
   ConnectHDA (...)
   Establishes an OPC-HDA Connection with the specified server
   INVALID_HANDLE_VALUE (-1) id returned if the connection cannot be established.
  }
  ConnectHDA = function(MachineName: LPCSTR; ServerName: LPCSTR): HANDLE; stdcall;

  {
   DisconnectHDA(...)
   Used to shutdown an OPC-HDA Connection
  }
  DisconnectHDA = procedure(hConnect: HANDLE); stdcall;

  {
   NumberOfHDAAttributes(...)
   Returns the number of Item Attributes supported by the server
   This function fills an internal array of Attribute Descriptions
   which may then be accessed via GetHDAAttributeDescr().
  }
  NumberOfHDAAttributes = function(hConnect: HANDLE): Integer; stdcall;

  {
   GetHDAAttributeDescr(...)
   Allows user to iterate through the list of Attribute Descriptions obtained from
   NumberOfHDAAttributes().
  }
  GetHDAAttributeDescr = function(hConnect: HANDLE; index: Integer; pAttrId: PDWORD; pVT: TVarType; pNameBuf: PChar; nBufSize: Integer; pDescBuf: PChar; dBufSize: Integer): Boolean; stdcall;

  {
   NumberOfHDAAggregates(...)
   Returns the number of Item Aggregates supported by the server
   This function fills an internal array of Aggregate Descriptions
   which may then be accessed via GetHDAAggregateDescr().
  }
  NumberOfHDAAggregates = function(hConnect: HANDLE): Integer; stdcall;

  {
   GetHDAAggregateDescr(...)
   Allows user to iterate through the list of Aggregate Descriptions obtained from
   NumberOfHDAAttributes().
  }
  GetHDAAggregateDescr = function(hConnect: HANDLE; index: Integer; pAttrId: PDWORD; pNameBuf: PChar; nBufSize: Integer; pDescBuf: PChar; dBufSize: Integer): Boolean; stdcall;

  {
   NumberOfHDAItems(...)
   Returns the number of OPC HDA Items from the Browse Interface of the designated
   Server connection.  If the server does not support Browsing, a value of xero
   is returned.  This function fills an internal array of itemnames which may
   then be accessed via GetHDAItemName().

   This function is equivalent to calling the HDA Browser using a
   universal filter, (OPCHDA_ITEMID+OPCHDA_EQUAL+'*'), for  HDA Items
   using OPC_FLAT from the Root position.
  }
  NumberOfHDAItems = function(hConnect: HANDLE): Integer; stdcall;

  {
   GetHDAItemName(...)
   Allows user to iterate through the list of item names obtained from
   NumberOfHDAItems().
  }
  GetHDAItemName = function(hConnect: HANDLE; index: Integer; pBuf: PChar; BufSize: Integer): Boolean; stdcall;

  {
   GetHDAItemHandle(...)
   Returns the server handle for the defined item.
   The returned server handle must be used in all requests to read or update history.
  }
  GetHDAItemHandle = function(hConnect: HANDLE; ItemName: LPCSTR; ClientHandle: DWORD): HANDLE; stdcall;

  {
   ReleaseHDAItemHandle(...)
   Releases the HDA Server Handle for the connected item.
  }
  ReleaseHDAItemHandle = function(hConnect: HANDLE; hServer: HANDLE): Boolean; stdcall;

  {
   ReadHDAItemAttributes (...)
   Allows the application to read Attribute values for a connected HDA Item
   over the specifiied period of time.  The return value represents the
   number of Attribute values read.  If the MSB of the return value is set, it
   indicates that more data was available than would fit in the allocated buffer.
  }
  ReadHDAItemAttributes = function(hConnect: HANDLE; hItem: HANDLE; AttrID: DWORD; aStart, aEnd: FILETIME; MaxValues: DWORD; pTimeStamps: PFILETIME; pValues: PVARIANT): DWORD; stdcall;
  ReadHDAItemAttributesVB = function(hConnect: HANDLE; hItem: HANDLE; AttrID: DWORD; pStart, pEnd: PVB_DATE; NumValues: DWORD; pTimeStamps: PVB_DATE; pValues: PVARIANT): DWORD; stdcall;
  ReadHDAItemAttributesVBnet = function(hConnect: HANDLE; hItem: HANDLE; AttrID: DWORD; pStart, pEnd: PVB_DATE; NumValues: DWORD; ppTimeStamps: PPSAFEARRAY; pValues: PSAFEARRAY): DWORD; stdcall;

  {
   ReadHDAItemValues (...)
   Allows the application to read item values from the server
   over the specifiied period of time.  The return value represents the
   number of values read.  If the MSB of the return value is set, it
   indicates that more data was available than would fit in the allocated buffer.
  }
  ReadHDAItemValues = function(hConnect: HANDLE; hItem: HANDLE; GetBoundingValues: Boolean;  aStart, aEnd: FILETIME; MaxValues: DWORD; pTimeStamps: PFILETIME; pValues: PVARIANT; pQualities: PDWORD): DWORD; stdcall;
  ReadHDAItemValuesVB = function(hConnect: HANDLE; hItem: HANDLE; GetBoundingValues: Boolean;  pStart, pEnd: PVB_DATE; NumValues: DWORD; pTimeStamps: PVB_DATE; pValues: PVARIANT; pQualities: PDWORD): DWORD; stdcall;
  ReadHDAItemValuesVBnet = function(hConnect: HANDLE; hItem: HANDLE; GetBoundingValues: Boolean;  pStart, pEnd: PVB_DATE; NumValues: DWORD; ppTimeStamps: PPSAFEARRAY; ppValues: PPSAFEARRAY; ppQualities: PPSAFEARRAY): DWORD; stdcall;

  {
   WriteHDAItemValues (...)
   Allows the application to write item values to the server.
   This uses an optional Interface that may not be supported by all
   HDA Servers.  The return code indicates the HRESULT as returned
   from the server application..

   Please refer to the OPC HDA Specification for a description of
   the SyncUpdate::Insert, ::Replace, and ::InsertReplace operations
  }
  WriteHDAItemValues = function(hConnect: HANDLE; hItem: HANDLE; UpdateType: DWORD; NumValues: DWORD; pTimeStamps: PFILETIME; pValues: PVARIANT; pQualities: PDWORD; pResults: PHRESULT): HRESULT; stdcall;
  WriteHDAItemValuesVB = function(hConnect: HANDLE; hItem: HANDLE; UpdateType: DWORD; MaxValues: DWORD; pTimeStamps: PVB_DATE; pValues: PVARIANT; pQualities: PDWORD; pResults: PHRESULT): DWORD; stdcall;
  WriteHDAItemValuesVBnet = function(hConnect: HANDLE; hItem: HANDLE; UpdateType: DWORD; MaxValues: DWORD; ppTimeStamps: PPSAFEARRAY; ppValues: PPSAFEARRAY; ppQualities: PPSAFEARRAY; ppResults: PPSAFEARRAY): DWORD; stdcall;

  {
   ReadHDAProcessedItemValues (...)
   Allows the application to read calculated item values from the server
   over the specifiied period of time.  The return value represents the
   number of values read.  If the MSB of the return value is set, it
   indicates that more data was available than would fit in the allocated buffer.
  }
  ReadHDAProcessedItemValues = function(hConnect: HANDLE; hItem: HANDLE; AggregateID: DWORD; aStart, aEnd: FILETIME; ReSampleInterval: FILETIME; MaxValues: DWORD; pTimeStamps: PFILETIME; pValues: PVARIANT; pQualities: PDWORD): DWORD; stdcall;
  ReadHDAProcessedItemValuesVB = function(hConnect: HANDLE; hItem: HANDLE; AggregateID: DWORD; pStart, pEnd: PVB_DATE; pReSampleSeconds: PDWORD; MaxValues: DWORD; pTimeStamps: PVB_DATE; pValues: PVARIANT; pQualities: PDWORD): DWORD; stdcall;
  ReadHDAProcessedItemValuesVBnet = function(hConnect: HANDLE; hItem: HANDLE; AggregateID: DWORD; pStart, pEnd: PVB_DATE; pReSampleSeconds: PDWORD; MaxValues: DWORD; ppTimeStamps: PPSAFEARRAY; ppValues: PPSAFEARRAY; ppQualities: PPSAFEARRAY): DWORD; stdcall;

  {
   GetHDASvrStatus (...)
   Allows the controlling application to interrogate the running
   status of an attached server.  pSvrStatus points to a structure containing
   a pointer to a buffer which is to receive a VendorInfo string (WSTR).
   VendorInfoBufSize defines the length of this buffer to keep the dll from overrunning.
   GetSvrStatus may be called with pSvrStatus = NULL, in which case a return
   value of TRUE indicates that the server processed the interface call, but
   no data was returned.
  }
  GetHDASvrStatus = function(hConnect: HANDLE; pStatus: PWORD; pCurrentTime: PFILETIME; pStartTime: PFILETIME; pwMajorVersion, pwMinorVersion, pwBuildNumber: PWORD; pdwMaxReturnValues: PDWORD; pStatusString: PChar; StatusBufSize: Integer; pVendorInfo: PChar; VendorInfoBufSize: Integer): Boolean; stdcall;

  {
   EnableHDAErrorNotification(...)
   Establishes a callback function in the user application which will receive
   control when an error is detected by the dll.  If this callback is not used
   errors will generate a modal MessageBox which must be acknowledged by the user)

   prototype for the callback function is as follows:
		void CALLBACK EXPORT HDAErrorMsgCallback (DWORD hResult, char *pMsg)
   (the buffer supplied by the dll as pMsg is a temporary and data should be
    copied to a permanant buffer by the application)
  }
  EnableHDAErrorNotification = function(lpCallback: HDAERRORPROC): Boolean; stdcall;

  {
   EnableHDAShutdownNotification(...)
   Establishes a callback to the application if the connected
   server requests a disconnect;
  }
  EnableHDAShutdownNotification = function(hConnect: HANDLE; lpCallback: HDASHUTDOWNPROC): Boolean; stdcall;

  {
   EnableHDAEventMsgs(...)
   Establishes a callback to the application for displaying
   Debug DCOM Event Messages
  }
  EnableHDAEventMsgs = function(lpCallback: HDASHUTDOWNPROC): Boolean; stdcall;


/////////////				Undocumented Functions						  ////////////////
//																						//
//				Undocumented function to Disable Demo Timer								//
//																						//
//////////////////////////////////////////////////////////////////////////////////////////

  DisableHDA30MinTimer = function(Authorization: LPCSTR): Boolean; stdcall;


implementation

end.

