{*******************************************************}
{       OPC Data Access 3.0  Custom Interface           }
{       Delphi Sample - Callback Interface              }

{  Переработано                                         }
{  Copyright (C) 2013 Чигрин В.Н. vchigrin@mail.ru      }
{*******************************************************}
unit OPCCallback;

{$mode objfpc}{$H+}

interface

uses
  Windows, ActiveX, SysUtils,
  OPCDA, OPCTypes,
  tag_list, opc_client;

type
  // class to receive IConnectionPointContainer data change callbacks

  { TOPCDataCallback }

  TOPCDataCallback = class(TInterfacedObject, IOPCDataCallback)
  public
    constructor Create(const aOPCClient : TOPCClient);
    function OnDataChange(dwTransid : DWORD; hGroup : OPCHANDLE;
      {%H-}hrMasterquality : HResult; {%H-}hrMastererror : HResult;
      dwCount : DWORD; phClientItems : POPCHANDLEARRAY;
      pvValues : POleVariantArray; pwQualities : PWordArray;
      {%H-}pftTimeStamps : PFileTimeArray; pErrors : PResultList) : HResult; stdcall;
    function OnReadComplete(dwTransid : DWORD; hGroup : OPCHANDLE;
      hrMasterquality : HResult; hrMastererror : HResult;
      dwCount : DWORD; phClientItems : POPCHANDLEARRAY;
      pvValues : POleVariantArray; pwQualities : PWordArray;
      pftTimeStamps : PFileTimeArray; pErrors : PResultList) : HResult; stdcall;
    function OnWriteComplete({%H-}dwTransid : DWORD; {%H-}hGroup : OPCHANDLE;
      {%H-}hrMastererr : HResult; {%H-}dwCount : DWORD; {%H-}pClienthandles : POPCHANDLEARRAY;
      {%H-}pErrors : PResultList) : HResult; stdcall;
    function OnCancelComplete({%H-}dwTransid : DWORD;
      {%H-}hGroup : OPCHANDLE) : HResult; stdcall;
  private
    myOPCClent  : TOPCClient;
  end;


implementation

// TOPCDataCallback methods
constructor TOPCDataCallback.Create(const aOPCClient : TOPCClient);
begin
  myOPCClent := aOPCClient;
end;

function TOPCDataCallback.OnDataChange(dwTransid : DWORD; hGroup : OPCHANDLE;
  hrMasterquality : HResult; hrMastererror : HResult; dwCount : DWORD;
  phClientItems : POPCHANDLEARRAY; pvValues : POleVariantArray;
  pwQualities : PWordArray; pftTimeStamps : PFileTimeArray;
  pErrors : PResultList) : HResult; stdcall;
var
  i :      integer;
  myTagList : TTagList;
  ClientItems : POPCHANDLEARRAY;
  Values : POleVariantArray;
  Qualities : PWORDARRAY;
  Errors : PResultList;
  myTag :  TTagItem;
  TagPosition : TTagPosition;
begin
  Result := S_OK;
  if (dwTransid <> 0) and not Assigned(myOPCClent) then Exit;
  myTagList := myOPCClent.TagList;
  if not Assigned(myTagList) then Exit;

  Values      := POleVariantArray(pvValues);
  ClientItems := POPCHANDLEARRAY(phClientItems);
  Qualities   := PWORDARRAY(pwQualities);
  Errors      := PResultList(pErrors);

  with TagPosition do
    begin
      NGroup := hGroup - 1;
      for i := 0 to dwCount - 1 do
        if Assigned(myTagList[NGroup]) then
          begin
            NTag := ClientItems^[i] - 1;
            myTag := myTagList[NGroup][NTag];
            if Assigned(myTag) then
              begin
                myTag.Quality := Qualities^[i];
                myTag.Value.SetValue(Values^[i]);
                myTag.Errors := Errors^[i];
                // Вызов обработчика изменения тега
                // myOPCClent.TagChange(TagPosition,myTag);
              end;
          end;
    end;
end;

function TOPCDataCallback.OnReadComplete(dwTransid : DWORD; hGroup : OPCHANDLE;
  hrMasterquality : HResult; hrMastererror : HResult; dwCount : DWORD;
  phClientItems : POPCHANDLEARRAY; pvValues : POleVariantArray;
  pwQualities : PWordArray; pftTimeStamps : PFileTimeArray;
  pErrors : PResultList) : HResult; stdcall;
begin
  Result := OnDataChange(dwTransid, hGroup, hrMasterquality, hrMastererror,
    dwCount, phClientItems, pvValues, pwQualities, pftTimeStamps, pErrors);
end;

function TOPCDataCallback.OnWriteComplete(dwTransid : DWORD;
  hGroup : OPCHANDLE; hrMastererr : HResult; dwCount : DWORD;
  pClienthandles : POPCHANDLEARRAY; pErrors : PResultList) : HResult; stdcall;
begin
  // we don't use this facility
  Result := S_OK;
end;

function TOPCDataCallback.OnCancelComplete(dwTransid : DWORD;
  hGroup : OPCHANDLE) : HResult; stdcall;
begin
  // we don't use this facility
  Result := S_OK;
end;

end.















