{******************************************************************************}
{* This Source Code Form is subject to the terms of the Mozilla Public        *}
{* License, v. 2.0. If a copy of the MPL was not distributed with this        *}
{* file, You can obtain one at http://mozilla.org/MPL/2.0/.                   *}
{*                                                                            *}
{* Copyright (C) George J. Vavoulogiannis                                     *}
{*                                                                            *}
{* Source code is available at:                                               *}
{*    https://github.com/G-Vavoulogiannis/Delphi-7-Libraries                  *}
{******************************************************************************}

{===============================================================================
 Description.....: Datasnap utily to manage communication via OwnerData
 Developer/Author: George J. Vavoulogiannis, georgev@hol.gr
                   http://g-vavoulogiannis.blogspot.com

===============================================================================}

unit UtilDataSnap;

interface

uses SysUtils, DB, Variants;

type
  TOwnerData = record
  private
    FData: OleVariant;
  public
    constructor Create(var aData: OleVariant);
    function Add(const Name: string; Value: Variant; FldType: TFieldType=ftUnknown): Boolean;
    function Get(const Name: string): Variant;
    property Data: OleVariant read FData write FData;
  end;
{ OwnerData structure:
  string (TParams)
         DSCOMM_REQUESTDATA (TParams)
               'VALIDATE_RECORD', 'RESULT_RECORD', 'RESULT_MESSAGES'
         DSCOMM_CLIENTFILTERS (TParams)
               'YY', 'AA'
}

function OwnerData_Add(const Name: string; FldType: TFieldType; Value: Variant;
                       var OwnerData: OleVariant): Boolean; overload;
function OwnerData_Add(const Name: string; Value: Variant;
                       var OwnerData: OleVariant): Boolean; overload;
function OwnerData_Get(const Name: string;
                       OwnerData: OleVariant): Variant;

{@ Add to Dataset Properties VarArray}
function DSVarData_Add(Name: string; Value: Variant; IncludeInDelta: Boolean;
                       var VarData: OleVariant): integer;
{@ Get from Dataset Properties VarArray (GetOptionalParam)}
function DSVarData_Get(Name: string;
                       VarData: OleVariant): Variant;

const
{@ Datasnap communication constants (optionalparams, ownerdata}
{@}DSCOMM_LOOKUPRECS = '*LOOKUP_INFORECS';
{@}DSCOMM_TABLENAME = '*TABLE_NAME';
{@}DSCOMM_DATASETNAME = '*DATASET_NAME';
{@}DSCOMM_DATASETDESCRIPTION = '*DATASET_DESCRIPTION';
{@}DSCOMM_DATABASENAME = '*DATABASE_NAME'; {???BDE}
{@}DSCOMM_REQUESTDATA = '*REQUESTDATA';
{@}DSCOMM_RESULTSET = '*RESULTSET';
{@}DSCOMM_CLIENTFILTERS = '*CLIENTFILTERS';
{@}DSCOMM_SQLTEXT = '*SQLTEXT';
{@}DSCOMM_SQLPARAMS = '*SQLPARAMS';

implementation

uses StrUtils, UtilFun, UtilDB;

function OwnerData_Add(const Name: string; FldType: TFieldType; Value: Variant;
                       var OwnerData: OleVariant): Boolean;
begin
  if (VarIsEmpty(OwnerData) or VarIsNull(OwnerData)) then
    OwnerData := '';
  if not (VarIsStr(OwnerData) and
     ((OwnerData = '') or IsParamsString(OwnerData)) ) then
    raise Exception.Create('OwnerData has invalid format (not TParams).');
  OwnerData := UpdateParam(OwnerData, ArrayFromStrings(Unstring(Name,[';','\','/'],[])),Value,FldType);
  Result := VarIsClear(OwnerData);
end;

function OwnerData_Add(const Name: string; Value: Variant;
                       var OwnerData: OleVariant): Boolean;
begin
  Result := OwnerData_Add(Name,ftUnknown,Value,OwnerData);
end;

function OwnerData_Get(const Name: string;
                       OwnerData: OleVariant): Variant;
begin
  if (VarIsEmpty(OwnerData) or VarIsNull(OwnerData))
     or
     ( not (VarIsStr(OwnerData) and
     ((OwnerData = '') or IsParamsString(OwnerData)) )) then
    Exit;
  Result := ExtractParam(OwnerData,ArrayFromStrings(Unstring(Name,[';','\','/'],[])));
end;

{ TOwnerData }

function TOwnerData.Add(const Name: string; Value: Variant; FldType: TFieldType): Boolean;
begin
  Result := OwnerData_Add(Name,FldType,Value,self.FData);
end;

constructor TOwnerData.Create(var aData: OleVariant);
begin
  FData := aData;
end;

function TOwnerData.Get(const Name: string): Variant;
begin
  Result := OwnerData_Get(Name,self.FData);
end;

{@ Add to Dataset Properties VarArray}
function DSVarData_Add(Name: string; Value: Variant; IncludeInDelta: Boolean; var VarData: OleVariant): integer;
var i: integer;
begin
 i := -1;
 if varIsNull(VarData) or varIsEmpty(VarData) then
    begin
    VarData := VarArrayCreate([0,0], varVariant);
    i:=0;
    end
 else
    begin
    if not VarIsArray(VarData) then Exit;
    i:=VarArrayHighBound(VarData,1);
    Inc(i,1);
    VarArrayRedim(VarData,i);
    end;
  VarData[i] := VarArrayOf([Name, Value, IncludeInDelta]);
  Result := i;
end;

{@ Get from Dataset Properties VarArray (GetOptionalParam)}
function DSVarData_Get(Name: string;
                       VarData: OleVariant): Variant;
var i: integer;
begin
 Result := Unassigned;
 if varIsNull(VarData) or varIsEmpty(VarData) or (not VarIsArray(VarData)) then Exit;
 for I := 0 to VarArrayHighBound(VarData,0) do
   if VarIsArray(VarData[i]) and VarIsStr(VarData[i,0]) and SameText(VarData[i,0],Name) then
      Result := VarData[i,1];
end;

end.
