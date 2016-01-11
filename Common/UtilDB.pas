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
 Description.....: Various Database related utily functions
 Developer/Author: George J. Vavoulogiannis, georgev@hol.gr
                   http://g-vavoulogiannis.blogspot.com

===============================================================================}

  { --- DONE HISTORY ----------------------------------------------------------}

  {HISTORY
26/12/99 split of database routines from UtilFun
15/01/2000 change to SetFieldLabels parameter from TTable to TDataset
           and code insight recognizes TTable/TADOTable.Tablename
           same to LockTableIndex, CheckTableKeys, GetPrimaryIndex,
           LockTableKeys, WriteVisibleFields, ReadVisibleFields,
           WriteTableState, ReadTableState
           New function 'DatasetTableName'
15/02/2000 New function DatasetRecordType
19/02/2000 New function GetIndexForFields
25/02/2000 New Function GetCutrrentIndex
25/02/2000 Change to GetPrimaryIndex so it returns TIndexDef
           New function GetIndexDefs
           New function GetUniqueIndex
           New function CloneField
03/03/2000 new function DBSessionName that finds SessionName for a given Databasename
09/03/2000 change to DatasetTableName for query uses DBCommon.GetTableNamefromSQL
20/03/2000 MoveCorr changed to function that returns the number of fields copied
10/04/2000 CreateTablesFmDef know checks for existing DDSFile file parameter
           and creates OpenDialog if needed.
11/05/2000 New constant DEFSTATEINI for write/read tablestate in different
           file than DEFVARSINI.
01/11/2000 new procedure OpenFooDatasets that searches for 'Foo' named
           datasets under a component and Opens them.
08/11/2000 new function FieldsStr concats array of field names into a fields string
           like FieldNames property.
25/11/2000 new function BuildLookupInfo that builds an array of information
           for lookup fields of a dataset & it's detail/nested datasets
22/03/2001 new procedures Write/ReadGridColumns
27/03/2001 new functions StrToFieldAttr & FieldAttrToStr to breakdown fieldlabel data
           to/from string
...
08/02/2007 RtvFieldData can be used with Dataset=nil
}

unit UtilDB;

interface

uses SysUtils, WinProcs, Forms, Graphics, Controls, Contnrs, Dialogs,
   DB, IniFiles, Classes, UtilFun, TypInfo, StrUtils,
   DBClient, Provider, DBCommon, Variants;

type
  TLookupInfoRec = record
    FieldName, KeyFields, Provider, LookupKeyFields, LookupResultField: WideString;
    FieldKind: TFieldKind;
    ReadOnly, Visible: Boolean;
    end;
  TLookupInfoRecs = WideString;

  TFieldAttributes = record
    DisplayLabel, DisplayFormat, EditFormat, EditMask, DefaultValue: string;
    DisplayWidth: integer;
    BDEAttr: string;
    Alignment: SmallInt;
    end;
  TFieldLabelAttr = (ldDisplayLabel, ldDisplayFormat, ldEditFormat, ldEditMask, ldDefaultValue,
    ldDisplayWidth, ldBDEAttr, ldAlignment);

  TFieldLabelAttrProc = procedure(var IniF, IniS; Dataset: TDataset; Done: Boolean) of object;

  {@ DragDrop Operation for fields }
  TDragFieldObject = class(TObject)
  public
   ObjField: TField;
   ObjDataSource: TDataSource;
   ObjComponent: TComponent;
  end;
  TDragFieldList = class(TDragObjectEx)
  private
    FFieldList: TObjectList; //TDragFieldObject item
    function GetItem(Index: Integer): TField;
    function GetDataSource(Index: Integer): TDataSource;
    function GetComponent(Index: Integer): TComponent;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Field: TField; DataSource: TDataSource=nil; Component: TComponent=nil): integer;
    function FieldCount: integer;
    property Field[Index: Integer]: TField read GetItem; default;
    property DataSource[Index: Integer]: TDataSource read GetDataSource;
    property Component[Index: Integer]: TComponent read GetComponent;
  end;

  TDragDataset = class(TDragObjectEx)
  private
    CDS: TClientDataset;
  public
    TableName: string;
    constructor Create(Fields: TFields; FieldMap: TStrings=nil);
    destructor Destroy; override;
    property Dataset: TClientDataset read CDS;
  end;

  TParamsStorage = class(TComponent)
  protected
   FParams: TParams;
  published
   property Params: TParams read FParams write FParams;
  end;

  TMoveCorrPair = record
    SourceField, DestField: TField;
  end;
  TMoveCorrMapping = class
  private
    FPairs: array of TMoveCorrPair;
    function GetCount: integer;
    function GetPair(index: integer): TMovecorrPair;
  public
    constructor Create;
    destructor Destroy; override;
    function AddPair(aSrcFld, aDstFld: TField): integer;
    property Count: integer read GetCount;
    property Pair[index: integer]: TMovecorrPair read GetPair;
  end;

{@}  TStrFormat = record
{@}    Name: string;
{@}    Format: string;
{@}    Width: integer;
{@}    Alignment: TAlignment;
{@}    UpperCase: Boolean;
{@}    PadChar: Char;
{@}    Prefix: string;
{@}    Suffix: string;
{@}    Value: string;
{@}    DataType: TFieldType;
{@}    NUMValue: Extended;
{@}    DTEValue: TDateTime;
{@}    BITValue: Boolean;
{@}  end;

{@}  TParamProps = record
{@}    Name: string;
{@}    DataType: TFieldType;
{@}    Value: variant;
{@}  end;


{*** DATABASE ***}
function AskDatasetPost(Dataset: TDataset; Msg: string=''): Word;

function INI2CDS(IniF: string): TClientDataset;
procedure CDS2INI(CDS: TClientDataset; IniF: string);

function SetFieldLabels(Dataset: TDataSet; Fnames: TFileName; LBLPROC: TFieldLabelAttrProc=nil):string ;
function GetFilterDef(Dataset: TDataSet; Fnames: TFileName): string;
function GetFilterID(FilterBase: string; Fnames: TFileName): string;
procedure SetFilterID(FilterBase: string; Fnames: TFileName; FilterID: string);
function GetSortFields(Dataset: TDataSet; Fnames: TFileName): string;
function RtvFieldData(Dataset: TDataSet; Fnames: TFileName; FieldName: string; Data: TFieldLabelAttr): string;
function StrToFieldAttr(Value: string): TFieldAttributes;
function FieldAttrToStr(Value: TFieldAttributes): string;

procedure GetIniFS(const FNames: string; var IniF, IniS: string);
procedure LockTableKeys(Dataset: TDataSet; Lck: Boolean);
procedure CheckTableKeys(Dataset: TDataSet);
function GetIndexDefs(Dataset: TDataset): TIndexDefs;
function GetIndexList(Dataset: TDataset): TStringList;
function GetPrimaryIndex(Dataset: TDataSet): TIndexDef;
function GetUniqueIndex(Dataset: TDataSet): TIndexDef;
function GetIndexForFields(Dataset: TDataSet; const Fields: string): TIndexDef;
function GetCurrentIndex(Dataset: TDataSet): TIndexDef;
function GetCurrentIndexFields(Dataset: TDataSet): string;
function GetPrimaryIndexFields(Dataset: TDataSet): string;
function GetFieldDefs(Dataset: TDataset): string;
procedure LockTableIndex(Dataset: TDataSet; Lck: Boolean);
{procedure LockEditKey(var EDIT: TDBEdit);}
procedure SwapFieldValue(f1, f2: TField);
function MoveCorr(Params: TParams; Dataset: TDataset): integer; overload;
function MoveCorr(Source, Dest: TDataSet; MoveNested: Boolean=True; CorrMap: TMoveCorrMapping=nil): integer; overload;
function MoveCorrMap(Source, Dest: TDataSet; FMap: TStringList=nil; CorrMap: TMoveCorrMapping=nil): integer;

function UpdateRecords(fromDataset, ToDataset: TDataset; KeyFields: string;
         CanAdd, CanUpdate: Boolean; FMap: TStringList): integer;
function ChangeFieldValue(Dataset: TDataset; FieldNames, FindValue, NewValue: string): integer;

procedure WriteTableState(Dataset: TDataSet; IniF: TFileName; IniS: string);
procedure WriteVisibleFields(Dataset: TDataSet; IniF: TFileName; IniS: string);
procedure ReadTableState(Dataset: TDataSet; IniF: TFileName; IniS: string);
procedure ReadVisibleFields(Dataset: TDataSet; IniF: TFileName; IniS: string);
function StringFromFields(Dataset: TDataset; sFields: string; separator: string='';
         whenblank: string=''; ignoreblanks: boolean=false): string;
function VisibleFieldsList(Dataset: TDataset): TStringList;

function FieldInFieldString(FieldName, FieldString: string): Boolean;
function FieldNoInFields(FieldName, FieldString: string): integer;
function FieldTypeByName(sFieldType: string): TFieldType;
function FieldsStr(const Names: array of string): string;

function QuotedFieldValue(Field: TField): string;
function QuotedParamValue(Param: TParam): string;
function GetQuotedValue(s: string; f: TFieldType): string;

function DatasetTableName(Dataset: TDataSet; RemoveDot: Boolean): string;
function DatasetRecordType(Dataset: TDataset): TStringList;

procedure FieldsToParams(Fields: TFields; Params: TParams; doAdd: Boolean=False);
procedure CopyFields(Fields: TFields; ToDataset: TDataset; doAdd: Boolean=False;
                     FieldMap: TStrings=nil; FieldKinds: TFieldKinds=[]);
procedure CopyFFields(Fields, ToFields: TFields; doAdd: Boolean=False;
                     FieldMap: TStrings=nil; FieldKinds: TFieldKinds=[]);
function CreateFieldsFromDefs(Dataset: TDataset): integer;
procedure CreateFieldDefsFromFields(Fields: TFields; ToFieldDefs: TFieldDefs);
function CloneField(Source: TField; AOwner: TComponent; NewClass: TFieldClass=nil): TField;
procedure Wide2StringField(Dataset: TDataset);

procedure OpenFooDatasets(UnderComponent: TComponent; FooID: string='');

function AutoMapFields(Source, Dest: TDataset): TStringList;

function SQLDateFmTo(fieldname: string; DateFm, DateTo: TDateTime): string;
function GetDatasetSQLText(Q: TDataset; var sqltext: string): Boolean;
function SetDatasetSQLText(Q: TDataset; sqltext: string): Boolean;
function CreateSQLText(ForTable: string; KeyFields: string; KeyValues: variant; OrderFields: string;
                       InSQL: TStrings): integer; overload;
function CreateSQLText(ForTable: string; KeyFields: string; KeyValues: variant; OrderFields: string;
                       out Text: String): integer; overload;

function IsDatasetCanModify(Dataset: TDataset; DefReadOnly: Boolean=False): Boolean;
function IsDatasetQuery(Dataset: TDataset): Boolean;

function ParseSQL(SQL: string; Section: TSQLToken; ParseList: TStrings=nil): string;
function ChangeSQLWhere(Sql, sWhere: string; DoAdd: Boolean): string;

function CreateTParams(const aParams: array of TParamProps): TParams;
function UpdateParam(Params: TParams; const Path: array of string;
                     Value: Variant; FldType: TFieldType): Boolean; overload;
function UpdateParam(strParams: string; const Path: array of string;
                     Value: Variant; FldType: TFieldType=ftUnknown): string; overload;
function ParamsToString(Params: TParams): string;
procedure StringToParams(Value: string; Params: TParams); overload;
function StringToParams(Value: string): TParams; overload;

function FormatFields(Fields: TFields; StrFormats: array of TStrFormat): string;
function FormatField(Field: TField; StrFormat: TStrFormat): string;
function FormatParams(Params: TParams; StrFormats: array of TStrFormat): string;
function FormatParam(Param: TParam; StrFormat: TStrFormat): string;
function StrFormatValue(Value: Variant; DataType: TFieldType; StrFormat: TStrFormat): string;

{*** DATASETPROVIDER / REMOTEDATAMODULE ***}
procedure ProvideDataSetProperties(DataSet: TDataSet; var Properties: OleVariant);
function IsEmptyInfoRecs(Value: TLookupInfoRecs): Boolean;
function UnstringInfoRecs(Value: TLookupInfoRecs): TStringList;
function LookupInfoRec(Value: WideString): TLookupInfoRec;
function LookupInfoVar(Value: TLookupInfoRec): WideString;
function BuildLookupInfo(ForDataset: TDataset): TLookupInfoRecs;

function EncryptCDS(CDS: TClientDataset; decrypt: boolean; c: integer; FileName: string): boolean;

const
  FIELDSDELIMITER = ';';
  SSDEFVARSEC = '*GLOBAL*';
  DEFSTATEINI = 'DBSTATE.CFG';
  FOOSTRING = 'Foo'; //@ marks readonly/autopen helper datasets
  QOOSTRING = 'Qoo'; //@ marks readonly/noautopen helper datasets
  QUERYSTRING = 'Query'; //@ marks readonly/noautopen helper datasets
  
  TXT_EXT = '.TXT'; //@ TXT Disk File extension
  CSV_EXT = '.CSV'; //@ CSV Disk File extension
  XML_EXT = '.XML'; //@ XML Disk File extension
  INI_EXT = '.INI'; //@ INI Disk File extension
  CFG_EXT = '.CFG'; //@ CFG Disk File extension
  DFM_EXT = '.DFM'; //@ DFM Disk File extension
  CDS_EXT = '.CDS'; //@ ClientDataset Disk File extension
  MDW_EXT = '.MDW'; //@ Access Workgroup File extension

  DBDDSFILE = 'DB';
  DBDDSEXT = '.DDS';
  secFIELDDEFS = '-FIELDDEFS';
  secINDEXDEFS = '-INDEXDEFS';
  secFIELDS = '-FIELDS';
  secTABLESDIRECTORY = 'TABLES DIRECTORY';
  idTABLEDESCRIPTION = '*DESCRIPTION';
  idTABLEFILTERID    = '*FILTERID';
  idTABLEFILTERDEF   = '*FILTERDEF';
  idTABLESORTFIELDS  = '*SORTFIELDS';

  TrueExpr = '0=0';

  { Server Date formats used with FormatDateTime }
  sdfStandard16 = '''"''mm''/''dd''/''yyyy''"'''; {"mm/dd/yyyy"}
  sdfStandard32 = '''''''dd/mm/yyyy''''''';       {'dd/mm/yyyy'}
  sdfOracle     = '"TO_DATE(''"dd/mm/yyyy"'', ''DD/MM/YYYY'')"';
  sdfInterbase  = '"CAST(''"mm"/"dd"/"yyyy"'' AS DATE)"';
  sdfMSSQL      = '"CONVERT(datetime, ''"mm"/"dd"/"yyyy"'', 103)"';
  //sdfMSAccess   = 'DateValue(''%s'')';

  LOOKREC_SEP = '|';
  LOOKFLD_SEP = '#';

{@ Provider optional parameter names}
{@}PROVIDERPROPERTY_LOOKUPRECS = '*LOOKUP_INFORECS';
{@}PROVIDERPROPERTY_TABLENAME = '*TABLE_NAME';
{@}PROVIDERPROPERTY_DATASETNAME = '*DATASET_NAME';
{@}PROVIDERPROPERTY_DATASETDESCRIPTION = '*DATASET_DESCRIPTION';
{@}PROVIDERPROPERTY_DATABASENAME = '*DATABASE_NAME'; {???BDE}

{@ DatasetStates array with TDatasetState names (Obsolet. can use TypInfo.GetEnumName)}
{@}  DatasetStates: array[TDataSetState] of string =
{@}    ('InActive', 'Browsing', 'Editing', 'Inserting', 'Setting key',
{@}     'Calculating', 'Filtering', 'NewValue', 'OldValue', 'CurValue' ,
{@}     'BlockRead', 'InternalCalc', 'Opening');
{@}  DatasetStatesGR: array[TDataSetState] of string =
{@}    ('Ανενεργό αρχείο', 'Προβολή', 'Αλλαγή Εγγραφής', 'Νέα Εγγραφή', 'Επιλογή Κλειδιού',
{@}     'Υπολογισμός', 'Φίλτρο', 'Νέα Τιμή', 'Παλαιά Τιμή', 'Τρέχουσα Τιμή' ,
{@}     'Ομαδική Ανάγνωση', 'Εσωτερικός Υπολογισμός', 'Ενεργοποίηση Αρχείου');

{@}  UpdateKinds: array[TUpdateKind] of string =
{@}    ('Modify', 'Insert', 'Delete');
{@}  UpdateStatuses: array[TUpdateStatus] of string =
{@}    ('UnModified', 'Modified', 'Inserted', 'Deleted');

{@}  ftStringTypes = [ftString, ftFixedChar, ftWideString];
{@}  ftDateTimeTypes = [ftDate, ftTime, ftDateTime];
{@}  ftMemoTypes = [ftMemo, ftFmtMemo];
{@}  ftBooleanTypes = [ftBoolean];
{@}  ftNumberTypes = [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftLargeint, ftBCD];

var
  DEFVARSINI: string = 'DBLBL.CFG';
  FormatForDateValue: string;
  Global_LBLPROC: TFieldLabelAttrProc = nil;

implementation

{==============================================================================}
{@ DATABASE COMMON CLASSES                                                     }
{==============================================================================}

{@ TDragFieldList}
constructor TDragFieldList.Create;
begin
 inherited Create;
 FFieldList := TObjectList.Create(True);
end;

destructor TDragFieldList.Destroy;
begin
 FFieldList.Free;
 inherited;
end;

function TDragFieldList.GetItem(Index: Integer): TField;
begin
  Result := (FFieldList.Items[Index] as TDragFieldObject).ObjField;
end;
function TDragFieldList.GetDataSource(Index: Integer): TDataSource;
begin
  Result := (FFieldList.Items[Index] as TDragFieldObject).ObjDataSource;
end;
function TDragFieldList.GetComponent(Index: Integer): TComponent;
begin
  Result := (FFieldList.Items[Index] as TDragFieldObject).ObjComponent;
end;
function TDragFieldList.Add(Field: TField; DataSource: TDataSource; Component: TComponent): integer;
var item: TDragFieldObject;
begin
  item := TDragFieldObject.Create;
  item.ObjField :=Field;
  item.ObjDataSource := DataSource;
  item.ObjComponent := Component;
  Result := FFieldList.Add(item);
end;
function TDragFieldList.FieldCount: integer;
begin
  Result := FFieldList.Count;
end;


{ TDragDataset }

constructor TDragDataset.Create(Fields: TFields; FieldMap: TStrings);
begin
  inherited Create;
  CDS := TClientDataset.Create(nil);
  if not Assigned(FieldMap) then CopyFields(Fields,CDS)
  else
   raise exception.Create('TDragDataset copy fields mapping NOT YET IMPLEMENTED');
  CDS.CreateDataSet;
end;

destructor TDragDataset.Destroy;
begin
  CDS.Free;
  inherited;
end;

{==============================================================================}
{@ DATABASE FUNCTIONS                                                          }
{==============================================================================}

{ TODO : WriteFieldLabels function which creates fieldlabels from Dataset.Fields }

{@}function AskDatasetPost(Dataset: TDataset; Msg: string=''): Word;
begin
  if Msg = '' then Msg := 'Post changes?';
  Result := MessageDlg(Msg, mtConfirmation, mbYesNoCancel, 0);
  case Result of
   mrYes: Dataset.Post;
   mrNo: Dataset.Cancel;
   end;
end;

{@}function INI2CDS(IniF: string): TClientDataset;
var Ini: TIniFile;
    sS, sI: TStringList;
    i,k: integer;
begin
  Result := TClientDataset.Create(nil);
  Result.FieldDefs.Add('Section',ftString,200,True);
  Result.FieldDefs.Add('Ident',ftString,200,True);
  Result.FieldDefs.Add('Value',ftString,200,True);
  Result.IndexFieldNames := 'Section;Ident';
  Result.CreateDataSet;
  Result.LogChanges := False;
  Result.FilterOptions := [foCaseInsensitive];
  if not FileExists(IniF) then
   if FileExists(ExtractFilePath(Application.ExeName)+IniF) then
     IniF := ExtractFilePath(Application.ExeName)+IniF;
  Ini := TIniFile.Create(IniF);
  sS := TStringList.Create;
  sI := TStringList.Create;
  try
  Ini.ReadSections(sS);
  for i:=0 to sS.Count-1 do
   begin
   Ini.ReadSectionValues(sS[i],sI);
   for k:=0 to sI.Count-1 do
     Result.InsertRecord([sS[i],sI.Names[k],sI.Values[sI.Names[k]]]);
   end;
  finally Ini.Free;
   sS.Free;
   sI.Free;
  end;
end;

{@}procedure CDS2INI(CDS: TClientDataset; IniF: string);
var Ini: TIniFile;
    sS: TStringList;
    i: integer;
begin
  if not FileExists(IniF) then
   if FileExists(ExtractFilePath(Application.ExeName)+IniF) then
     IniF := ExtractFilePath(Application.ExeName)+IniF;
  Ini := TIniFile.Create(IniF);
  try
   sS := TStringList.Create;
   try Ini.ReadSections(sS);
     for i:=0 to sS.Count-1 do
      Ini.EraseSection(sS[i]);
   finally sS.Free;
   end;
   CDS.First;
   while not CDS.Eof do
    begin
    Ini.WriteString(CDS.FieldByName('Section').AsString,CDS.FieldByName('Ident').AsString,
                    CDS.FieldByName('Value').AsString);
    CDS.Next;
    end;
  finally Ini.Free;
  end;
end;

{@Set a Table's Field labels according to 'var.ini' settings
 and return dataset's description
 call on 'AfterOpen' the Table}
{@}function SetFieldLabels(Dataset: TDataSet; Fnames: TFileName; LBLPROC: TFieldLabelAttrProc): string;
var i,j: integer;
    IniFile: TIniFile;
    IniF: string;
    IniS: string;
    s: string;
    fa: TFieldAttributes;
  procedure testAlias(var s: string);
  var ss, sr: string;
  begin
    ss := StringBetween(s,'<','>',True);
    while (ss<>'') do begin
     sr := IniFile.ReadString(SSDEFVARSEC,ss,'');
     s := StringReplace(s,'<'+ss+'>',sr,[rfReplaceAll,rfIgnoreCase]);
     ss := StringBetween(s,'<','>',True);
     end;
  end;
begin
  GetIniFS(Fnames, IniF, IniS);
  if Inis = '' then Inis := DatasetTableName(Dataset, True);

  { TODO : call back procedure for application specific IniF, IniS values
           "TFieldLabelAttrProc"
           see also "Global_LBLPROC" variable
           check also RtvFieldData and other GetIniFS functions }
  if (IniF = '') or (Inis = '') then Exit;//???
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);//???
  try j := Dataset.FieldCount-1;
  with Dataset do
   begin
   Result := IniFile.ReadString(IniS,idTABLEDESCRIPTION,'');
   for i:=0 to j do
    begin
    s := IniFile.ReadString(IniS,Fields[i].FieldName,'');
    if s = '' then s := IniFile.ReadString(SSDEFVARSEC,Fields[i].FieldName,'');
    if s = '' then Continue;
    testAlias(s);
    fa := StrToFieldAttr(s);
    if fa.DisplayLabel <> '' then Fields[i].DisplayLabel := fa.DisplayLabel;
    if fa.DisplayFormat <> '' then TypInfo.SetPropValue(Fields[i],'DisplayFormat',fa.DisplayFormat);
    if fa.EditFormat <> '' then TypInfo.SetPropValue(Fields[i], 'EditFormat',fa.EditFormat);
    if fa.EditMask <> '' then try TypInfo.SetPropValue(Fields[i], 'EditMask',fa.EditMask); except ; end;
    if fa.DefaultValue <> '' then Fields[i].DefaultExpression := fa.DefaultValue;
    if fa.DisplayWidth <> 0 then TypInfo.SetPropValue(Fields[i],'DisplayWidth',fa.DisplayWidth);
    if fa.BDEAttr <> '' then Fields[i].AttributeSet := fa.BDEAttr;
    if fa.Alignment <> 0 then try Fields[i].Alignment := TAlignment(fa.Alignment-1); except ; end;

{ TODO : SetFieldLabels convert string to MinValue/MaxValue }

    //if p>Length(s) then Continue;
    //sA := ExtractString(s,p,FIELDSDELIMITER); //MinValue
    //if sA <> '' then Fields[i].MinValue := sA;
    //if p>Length(s) then Continue;
    //sA := ExtractString(s,p,FIELDSDELIMITER); //MaxValue
    //if sA <> '' then Fields[i].MaxValue := sA;
    end;
   end;
  finally IniFile.Free;
  end;
end;

{@}function GetFilterDef(Dataset: TDataSet; Fnames: TFileName): string;
var IniFile: TIniFile;
    IniF: string;
    IniS: string;
begin
  GetIniFS(Fnames, IniF, IniS);
  if Inis = '' then Inis := DatasetTableName(Dataset, True);
  if (IniF = '') or (Inis = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
  Result := IniFile.ReadString(IniS,idTABLEFILTERDEF,'');
  finally IniFile.Free;
  end;
end;

{@}function GetFilterID(FilterBase: string; Fnames: TFileName): string;
var IniFile: TIniFile;
    IniF: string;
    IniS: string;
begin
  GetIniFS(Fnames, IniF, IniS);
  if Inis = '' then Inis := 'FILTERS';
  if (IniF = '') or (Inis = '') or (FilterBase = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
  Result := IniFile.ReadString(IniS,FilterBase,'');
  finally IniFile.Free;
  end;
end;

{@}procedure SetFilterID(FilterBase: string; Fnames: TFileName; FilterID: string);
var IniFile: TIniFile;
    IniF: string;
    IniS: string;
begin
  GetIniFS(Fnames, IniF, IniS);
  if Inis = '' then Inis := 'FILTERS';
  if (IniF = '') or (Inis = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
  IniFile.WriteString(IniS,FilterBase,FilterID);
  finally IniFile.Free;
  end;
end;

{@}function GetSortFields(Dataset: TDataSet; Fnames: TFileName): string;
var IniFile: TIniFile;
    IniF: string;
    IniS: string;
begin
  GetIniFS(Fnames, IniF, IniS);
  if Inis = EmptyStr then
     Inis := DatasetTableName(Dataset, True);
  if (IniF = '') or (Inis = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
  Result := IniFile.ReadString(IniS,idTABLESORTFIELDS,'');
  finally IniFile.Free;
  end;
end;

{@}function RtvFieldData(Dataset: TDataSet; Fnames: TFileName; FieldName: string; Data: TFieldLabelAttr): string;
var IniFile: TIniFile;
    IniF: string;
    IniS: string;
    s: string;
    fa: TFieldAttributes;
  procedure testAlias(var s: string);
  var ss, sr: string;
  begin
    ss := StringBetween(s,'<','>',True);
    while (ss<>'') do begin
     sr := IniFile.ReadString(SSDEFVARSEC,ss,'');
     s := StringReplace(s,'<'+ss+'>',sr,[rfReplaceAll,rfIgnoreCase]);
     ss := StringBetween(s,'<','>',True);
     end;
  end;
begin
  GetIniFS(Fnames, IniF, IniS);
  if (Inis = '') and Assigned(Dataset) then Inis := DatasetTableName(Dataset, True);
  if (IniF = '') or (Inis = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
    s := IniFile.ReadString(IniS,FieldName,'');
    if s = '' then s := IniFile.ReadString(SSDEFVARSEC,FieldName,'');
    testAlias(s);
    fa := StrToFieldAttr(s);
    Result := '';
    case Data of
      ldDisplayLabel:   Result := fa.DisplayLabel;
      ldDisplayFormat:  Result := fa.DisplayFormat;
      ldEditFormat:     Result := fa.EditFormat;
      ldEditMask:       Result := fa.EditMask;
      ldDefaultValue:   Result := fa.DefaultValue;
      ldDisplayWidth:   Result := IntToStr(fa.DisplayWidth);
      ldBDEAttr:        Result := fa.BDEAttr;
      ldAlignment:      try Result := TypInfo.GetEnumName(TypeInfo(TAlignment),fa.Alignment-1); except; end;
    end;
  finally IniFile.Free;
  end;
end;

{@}function StrToFieldAttr(Value: string): TFieldAttributes;
var p: integer;
   sA: string;
begin
  with Result do begin
    DisplayLabel  := '';
    DisplayFormat := '';
    EditFormat    := '';
    EditMask      := '';
    DefaultValue  := '';
    DisplayWidth  := 0;
    BDEAttr       := '';
    Alignment     := 0;
    end;
  p:=1;
  sA := ExtractString(Value,p,FIELDSDELIMITER); //Label
  if sA <> '' then Result.DisplayLabel := sA;
  sA := ExtractString(Value,p,FIELDSDELIMITER); //DisplayFormat
  if sA <> '' then Result.DisplayFormat := StringReplace(sA,'|',';',[rfReplaceAll]);
  sA := ExtractString(Value,p,FIELDSDELIMITER); //EditFormat
  if sA <> '' then Result.EditFormat := StringReplace(sA,'|',';',[rfReplaceAll]);
  sA := ExtractString(Value,p,FIELDSDELIMITER); //EditMask
  if sA <> '' then Result.EditMask := sA;
  sA := ExtractString(Value,p,FIELDSDELIMITER); //DefaultValue
  if sA <> '' then Result.DefaultValue := sA;
  sA := ExtractString(Value,p,FIELDSDELIMITER); //DisplayWidth
  if sA <> '' then try Result.DisplayWidth := StrToInt(sA); except ; end;
  sA := ExtractString(Value,p,FIELDSDELIMITER); //Attribute set from BDE data dictionary
  if sA <> '' then Result.BDEAttr := sA;
  sA := ExtractString(Value,p,FIELDSDELIMITER); //Alignment
  if sA <> '' then try Result.Alignment := Ord(TypInfo.GetEnumValue(TypeInfo(TAlignment),sA))+1;
                   except Result.Alignment := 0 ; end;
end;

{@}function FieldAttrToStr(Value: TFieldAttributes): string;
var sa: string;
begin
   try sa := TypInfo.GetEnumName(TypeInfo(TAlignment),Value.Alignment-1) except sa := ''; end;
   Result := StringFromStrings([Value.DisplayLabel,
                                StringReplace(Value.DisplayFormat,';','|',[rfReplaceAll]),
                                StringReplace(Value.EditFormat,';','|',[rfReplaceAll]),
                                Value.EditMask,
                                Value.DefaultValue,
                                Value.BDEAttr,
                                sa],
                                FIELDSDELIMITER);

end;

{@}procedure GetIniFS(const FNames: string; var IniF, IniS: string);
var p: integer;
begin
  P := 0;
  if Fnames = EmptyStr then IniF := DEFVARSINI
  else
     begin
     IniF := ExtractString(Fnames,P,',');
     IniS := ExtractString(Fnames,P,',');
     end;
  if IniF = '' then IniF := DEFVARSINI;    
end;

{@ Lock/Unlock table primary keys }
{@}procedure LockTableKeys(Dataset: TDataSet; Lck: Boolean);
var i: integer;
    ind: TIndexDef;
    s: string;
    f: TField;
begin
  if not (Dataset.Active) then Exit;
  ind := GetPrimaryIndex(Dataset);
  if not Assigned(ind) then Exit;
  s := ind.Fields;
  i:=1;
  while i <= Length(s) do
   begin
   f := Dataset.FindField(ExtractString(s,i,FIELDSDELIMITER));
   if f <> nil then f.ReadOnly :=  Lck;
   end;
end;

{@ Lock/Unlock table primary keys based on dsInsert}
{@}procedure CheckTableKeys(Dataset: TDataSet);
var i: integer;
    ind: TIndexDef;
    s: string;
    f: TField;
begin
  if not (Dataset.Active) then Exit;
  ind := GetPrimaryIndex(Dataset);
  if not Assigned(ind) then Exit;
  s := ind.Fields;
  i:=1;
  while i <= Length(s) do
   begin
   f := Dataset.FindField(ExtractString(s,i,FIELDSDELIMITER));
   if f <> nil then f.ReadOnly :=  not (Dataset.State in [dsInsert]);
  end;
end;

{@ get indexdefs if is valid dataset }
{@}function GetIndexDefs(Dataset: TDataset): TIndexDefs;
begin
  Result := nil;
  //ShowMessage('function <GetIndexDefs> not implemented. Please inform operator!');
  { TODO : GetIndexDefs   ΠΡΟΣΟΧΗ ΔΕΝ ΛΕΙΤΟΥΡΓΕΙ!!! με TBetterADODataset & TCustomadodataset
         εχει γίνει στο TTADODataset κοίταξε εκεί! }
  try Result := IProviderSupport(Dataset).PSGetIndexDefs; except ; end;
  {if TypInfo.IsPublishedProp(Dataset,'IndexDefs') then
     Result := TypInfo.GetObjectProp(Dataset,'IndexDefs') as TIndexDefs
  else Result := nil;}
  if Assigned(Result) then
    if (Result.Count = 0) and Dataset.Active then
    try Result.Update;
    except on exception do ;
    end;
end;

{@ get indexfieldnames for indexdefs in stringlist if is valid dataset }
{@}function GetIndexList(Dataset: TDataset): TStringList;
var i: integer;
    ind: TIndexDefs;
    indNames: string;
begin
  ind := nil; indNames := '';
  Result := TStringList.Create;
  if Assigned(Dataset) then begin
     try ind := GetIndexDefs(DataSet);  except  end;
     try indNames := TypInfo.GetPropValue(DataSet,'IndexFieldNames'); except  end;
     end;
  if not Assigned(ind) then begin
     if indNames <> '' then
        Result.Add(indNames);
     end
  else begin
    for i:= 1 to Ind.Count do
       Result.Add(Ind.Items[i-1].Fields);
    i:=Result.IndexOf(IndNames);
    if (i < 0) and (indNames <> '') then
       Result.Add(indNames);
    end;
  i:=Result.IndexOf('');
  if i>=0 then Result.Delete(i);
end;

{@ Get dataset's primary key fields if exists}
{@}function GetPrimaryIndex(Dataset: TDataSet): TIndexDef;
var i: integer;
    ind: TIndexDefs;
begin
  Result := nil;
  ind := GetIndexDefs(Dataset);
  if not Assigned(ind) then Exit;
  with Ind do
   begin
   for i := 0 to Count-1 do
   if ixPrimary in Items[i].Options then
      begin
      Result := Items[i];
      Break;
      end;
   end;
end;

{@ Get dataset's unique index if exists}
{@}function GetUniqueIndex(Dataset: TDataSet): TIndexDef;
var i: integer;
    ind: TIndexDefs;
begin
  Result := nil;
  ind := GetIndexDefs(Dataset);
  if not Assigned(ind) then Exit;
  with Ind do
   begin
   for i := 0 to Count-1 do
   if ixUnique in Items[i].Options then
      begin
      Result := Items[i];
      Break;
      end;
   end;
end;

function GetIndexForFields(Dataset: TDataSet; const Fields: string): TIndexDef;
var ind: TIndexDefs;
begin
  Result := nil;
  ind := GetIndexDefs(Dataset);
  if not Assigned(ind) then Exit;
  Result := ind.GetIndexForFields(Fields, True);
end;

{@ get current active index by index name or indexfields }
{@}function GetCurrentIndex(Dataset: TDataSet): TIndexDef;
var ind: TIndexDefs;
    s: string;
begin
  Result := nil;
  ind := GetIndexDefs(Dataset);
  if not Assigned(ind) then Exit;
  s := '';
  try s := AssignVar(TypInfo.GetPropValue(Dataset,'IndexName'),'');  except end;
  if s <> '' then Result := ind.Find(s)
  else
   begin
   try s := AssignVar(TypInfo.GetPropValue(Dataset,'IndexFieldNames'),''); except end;
   Result := ind.GetIndexForFields(s, True);
   end;
end;

{@ get current active index indexfieldnames by index name or indexfields }
{@}function GetCurrentIndexFields(Dataset: TDataSet): string;
var ind: TIndexDefs;
    s: string;
begin
  Result := '';
  if not Assigned(Dataset) then Exit;
  ind := GetIndexDefs(Dataset);
  if Assigned(ind) then begin
     s := '';
     try s := AssignVar(TypInfo.GetPropValue(Dataset,'IndexName'),''); except end;
     if s <> '' then Result := ind.Find(s).Fields
     end;
  if Result = '' then
   try Result := AssignVar(TypInfo.GetPropValue(Dataset,'IndexFieldNames'),''); except end;
end;

{@ get primary index indexfieldnames by index name or indexfields }
{@}function GetPrimaryIndexFields(Dataset: TDataSet): string;
var ind: TIndexDef;
begin
  Result := '';
  if not Assigned(Dataset) then Exit;
  ind := GetPrimaryIndex(Dataset);
  if Assigned(ind) then
     Result := ind.Fields;
end;

{@ Get table fields from fielddefs}
{@}function GetFieldDefs(Dataset: TDataset): string;
var i: integer;
begin
  Result := EmptyStr;
  with Dataset.FieldDefs do
  for i := 0 to Count-1 do
   Result := Result + IIF(Result = EmptyStr,EmptyStr,FIELDSDELIMITER) + Items[i].Name ;
end;

{@ Lock/Unlock table's current keys }
{@}procedure LockTableIndex(Dataset: TDataSet; Lck: Boolean);
var i: integer;
begin
  if (Dataset.Active) {and (not Dataset.ReadOnly)} then
     for i:=0 to Dataset.FieldCount-1 do
      if Dataset.Fields[i].IsIndexField then
         Dataset.Fields[i].ReadOnly := Lck;
end;

{@Lock a DBEdit if associated field is index and not inserting
 call in 'OnKeyDown' of the DBEdit}
{@}{procedure LockEditKey(var EDIT: TDBEdit);
begin
  EDIT.ReadOnly := not (EDIT.DataSource.State in [dsInsert]);
  if EDIT.ReadOnly then
     EDIT.Color := clBtnFace
  else
     EDIT.Color := clWindow;
end;}

{@}procedure SwapFieldValue(f1, f2: TField);
var msw: variant;
begin
  msw := f1.Value;
  f1.Value := f2.Value;
  f2.Value := msw;
end;

{@ Set corresponding field values from Source to Dest Dataset COBOL like }
function MoveCorr(Params: TParams; Dataset: TDataset): integer;
var i: integer;
    p: TParam;
begin
  Result := 0;
  with Dataset do
  begin
  for i := 0 to FieldCount -1 do
   if (Fields[i].FieldKind = fkData) and (not Fields[i].ReadOnly) then
    begin
    p := Params.FindParam(Fields[i].FieldName);
    if Assigned(p) then //fieldparam may not exists!
      if p.Bound then
      //check for equal fields - problem on ado master/detail when a masterfield changes
      //the detail is requeried!!!
      if not VarEquals(Fields[i].Value, p.Value) then
       begin
       if VarIsEmpty(p.Value) or VarIsNull(p.Value) then
         Fields[i].Clear
       else
         Fields[i].Value := p.Value;
       Inc(Result);
       end;
    end;
  end;
end;

{@}function MoveCorr(Source, Dest: TDataSet; MoveNested: Boolean; CorrMap: TMoveCorrMapping): integer;

 procedure MoveNestedData(nSrcFld, nDstFld: TDatasetField);
 var NestMap: TMoveCorrMapping;
 begin
   NestMap := TMoveCorrMapping.Create;
   with nSrcFld do
     try
       NestedDataset.First;
       while not NestedDataset.Eof do
        begin
        nDstFld.NestedDataSet.Append;
        MoveCorr(NestedDataSet, nDstFld.NestedDataSet, True,NestMap);
        nDstFld.NestedDataSet.Post;
        NestedDataset.Next;
        end;
       finally
        FreeAndNil(NestMap);
       end;
 end;

var i: integer;
    fld: TField;
begin
 Result := 0;
 if not (Source.Active and Dest.Active) then Exit;
 if Assigned(CorrMap) then
    if (CorrMap.Count>0) then
     begin
     for i:=0 to CorrMap.Count-1 do
      if (CorrMap.Pair[i].SourceField is TDatasetField) and
         (CorrMap.Pair[i].DestField is TDatasetField) then
         begin
         MoveNestedData(TDatasetField(CorrMap.Pair[i].SourceField), TDatasetField(CorrMap.Pair[i].DestField));
         Inc(Result);
         end
      else
      if not VarEquals(CorrMap.Pair[i].SourceField.Value, CorrMap.Pair[i].DestField.Value) then
         begin
         CorrMap.Pair[i].DestField.Value := CorrMap.Pair[i].SourceField.Value;
         Inc(Result);
         end;
     Exit;
     end;

 for i := 0 to Dest.FieldCount -1 do
  if Dest.Fields[i].CanModify then
  begin
  fld := Source.FindField(Dest.Fields[i].FieldName);
  if (fld <> nil) then
  if fld is TObjectField then
    begin
    if (fld is TDatasetField) and (Dest.Fields[i] is TDatasetField) and
     TDatasetField(fld).NestedDataSet.CanModify and
     TDatasetField(Dest.Fields[i]).NestedDataSet.CanModify and
     MoveNested then
     //with (fld as TDatasetField) do
      begin
      if Assigned(CorrMap) then CorrMap.AddPair(fld,Dest.Fields[i]);
      MoveNestedData(TDatasetField(fld),TDatasetField(Dest.Fields[i]));
      {NestMap := TMoveCorrMapping.Create;
      try
       NestedDataset.First;
       while not NestedDataset.Eof do
        begin
        TDatasetField(Dest.Fields[i]).NestedDataSet.Append;
        MoveCorr(NestedDataSet, TDatasetField(Dest.Fields[i]).NestedDataSet, True,NestMap);
        TDatasetField(Dest.Fields[i]).NestedDataSet.Post;
        NestedDataset.Next;
        end;
       finally
        FreeAndNil(NestMap);
       end;}
      Inc(Result);
      end;
    end
  else
    begin
    if Assigned(CorrMap) then CorrMap.AddPair(fld,Dest.Fields[i]);
    if not VarEquals(fld.Value, Dest.Fields[i].Value) then
       begin
       Dest.Fields[i].Value := fld.Value;
       Inc(Result);
       end;
    end;
  end;
end;

{@}function MoveCorrMap(Source, Dest: TDataSet; FMap: TStringList; CorrMap: TMoveCorrMapping): integer;
var i,p: integer;
    s, s1, s2: String;
    sf, df: TField;
begin
  if not Assigned(FMap) or (Assigned(FMap) and (FMap.Text = '')) then begin
    Result := MoveCorr(Source, Dest); Exit;
    end;
 Result := 0;
 if not (Source.Active and Dest.Active) then Exit;
 for i := 0 to FMap.Count -1 do
  begin
  s := FMap[i]; p:=1;
  if (s = '*') then begin
     MoveCorr(Source, Dest);
     Continue;
     end;
  s1 := ExtractString(s,p,'='); s2 := ExtractString(s,p,'=');
  if (s2='') then s2 := s1;
  sf := Source.FindField(s1); df := Dest.FindField(s2);
  if Assigned(sf) and Assigned(df) then
  if not ((sf is TObjectField) or (df is TObjectField)) then 
   if df.CanModify and not VarEquals(sf.Value, df.Value) then
       begin
       df.Value := sf.Value;
       Inc(Result);
       end;
  end;
end;
{@ Update records from/to dataset}
{@}function UpdateRecords(fromDataset, ToDataset: TDataset; KeyFields: string;
            CanAdd, CanUpdate: Boolean; FMap: TStringList): integer;
var fldMoved: integer;
    bExists: Boolean;
    sKeyFields: string;
begin
  Result := -1;
  sKeyFields := KeyFields;
  if sKeyFields = '' then Exit;
  Result := 0;
  FromDataset.First;
  while not fromDataset.Eof do
   begin
   bexists := ToDataset.Locate(sKeyFields,fromDataset[sKeyFields],[loCaseInsensitive]);
   if bExists then
      begin
      if CanUpdate then ToDataset.Edit;
      end
   else
      begin
      if CanAdd then ToDataset.Append;
      end;
   if ToDataset.State in dsEditModes then
      begin
      fldMoved := MoveCorrMap(fromDataset,ToDataset,FMap);
      {try/except ???}
      ToDataset.Post;//errors can be trapped using ToDataset.OnPostError
      Inc(Result);
      end;
   fromDataset.Next;
   end;
end;

{@ Change field(s) value based on FindValue/NewValue in field's string}
{@}function ChangeFieldValue(Dataset: TDataset; FieldNames, FindValue, NewValue: string): integer;
var i: integer;
    sValue: string;
    flds: TList;
begin
  Result := 0;
  flds := Tlist.Create;
  try
   Dataset.GetFieldList(flds,FieldNames);
   if flds.Count = 0 then Exit;
   Dataset.First;
   while not Dataset.Eof do
    begin
    for i := 0 to flds.Count-1 do
     begin
     sValue := TField(flds.Items[i]).AsString;
     if AnsiContainsText(svalue,FindValue) then
        begin
        Dataset.Edit;
        if TField(flds.Items[i]).CanModify then
           begin
           try TField(flds.Items[i]).AsString :=
             StringReplace(sValue,FindValue,NewValue,[rfReplaceAll, rfIgnoreCase]);
            Inc(Result); 
            except on Exception do ; end;
           end;
        end;
     end;
    if Dataset.State in dsEditModes then Dataset.Post;
    Dataset.Next;
    end;
   finally flds.Free;
   end;
end;

{@ Write Dataset state to inifile }
{@}procedure WriteTableState(Dataset: TDataSet; IniF: TFileName; IniS: string);
var i: integer;
    IniFile: TIniFile;
    S, s1: string;
begin
  if IniF = '' then IniF := DEFSTATEINI;
  S := DatasetTableName(Dataset, True)+IniS;
  if (IniF = '') or (S = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
   IniFile.WriteString(S,'Filter',Dataset.Filter);
   IniFile.WriteBool(S,'Filtered',Dataset.Filtered);
   try s1 := AssignVar(TypInfo.GetPropValue(Dataset,'IndexFieldNames'),''); except end;
   IniFile.WriteString(S,'IndexFieldNames',s1);
   for i:=0 to Dataset.FieldCount-1 do
    IniFile.WriteBool(S,Dataset.Fields[i].FieldName, Dataset.Fields[i].Visible);
  finally IniFile.Free;
  end;
end;

{@ Write Dataset Visible fields to inifile }
{@}procedure WriteVisibleFields(Dataset: TDataSet; IniF: TFileName; IniS: string);
var i: integer;
    IniFile: TIniFile;
    S: string;
begin
  if IniF = EmptyStr then IniF := DEFSTATEINI;
  S := DataSetTableName(Dataset,True)+IniS;
  if (IniF = '') or (S = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
   for i:=0 to Dataset.FieldCount-1 do
    IniFile.WriteBool(S,Dataset.Fields[i].FieldName, Dataset.Fields[i].Visible);
  finally IniFile.Free;
  end;
end;

{@ Read Table state from inifile }
{@}procedure ReadTableState(Dataset: TDataSet; IniF: TFileName; IniS: string);
var i: integer;
    IniFile: TIniFile;
    S: string;
begin
  if IniF = EmptyStr then IniF := DEFSTATEINI;
  S := DataSetTableName(Dataset,True)+IniS;
  if (IniF = '') or (S = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
   Dataset.Filter := IniFile.ReadString(S,'Filter',Dataset.Filter);
   Dataset.Filtered := IniFile.ReadBool(S,'Filtered',Dataset.Filtered);
   TypInfo.SetPropValue(Dataset,'IndexFieldNames',
           IniFile.ReadString(S,'IndexFieldNames',EmptyStr));
   for i:=0 to Dataset.FieldCount-1 do
    Dataset.Fields[i].Visible := IniFile.ReadBool(S,Dataset.Fields[i].FieldName, Dataset.Fields[i].Visible);
  finally IniFile.Free;
  end;
end;

{@ Read Table Visible fields from inifile }
{@}procedure ReadVisibleFields(Dataset: TDataSet; IniF: TFileName; IniS: string);
var i: integer;
    IniFile: TIniFile;
    S: string;
begin
  if IniF = EmptyStr then IniF := DEFSTATEINI;
  S := DataSetTableName(Dataset,True)+IniS;
  if (IniF = '') or (S = '') then Exit;
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + IniF);
  try
   for i:=0 to Dataset.FieldCount-1 do
    Dataset.Fields[i].Visible := IniFile.ReadBool(S,Dataset.Fields[i].FieldName, Dataset.Fields[i].Visible);
  finally IniFile.Free;
  end;
end;

{@ Concat string values of fields into one string }
{@}function StringFromFields(Dataset: TDataset; sFields: string; separator: string;
            whenblank: string; ignoreblanks: boolean): string;
var p: integer;
    s: string;
    fld: TField;
begin
 Result := EmptyStr;
 p := 1;
 while p <= Length(sFields) do
  begin
  s := ExtractString(sFields,p,FIELDSDELIMITER);
  fld := Dataset.FindField(s);
  if fld <> nil then
     if fld.IsNull or (fld.AsString = '') then
        if ignoreblanks then
           Continue
        else
           Result := Result + IIF(Result='','',separator) +whenblank
     else
         Result := Result + IIF(Result='','',separator) +fld.AsString
  else Result := Result + s;
  end;

end;

{@Get Dataset's visible fields list in a StringList}
{@}function VisibleFieldsList(Dataset: TDataset): TStringList;
var i: integer;
begin
 Result := TStringList.Create;
 if not Dataset.DefaultFields then
    for i := 0 to Dataset.FieldCount-1 do
     if Dataset.Fields[i].Visible then
        Result.Add(Dataset.Fields[i].FieldName);
end;

{@Check if a FieldName exists in a Fields string like MasterFields}
{@}function FieldInFieldString(FieldName, FieldString: string): Boolean;
var P: integer;
begin
 P:=0;
 Result := False;
 while P <= Length(FieldString) do
  if FieldName = ExtractString(FieldString,P,FIELDSDELIMITER) then
     begin
     Result := True;
     Break;
     end;
end;

{@Check if a FieldName exists in a Fields string like MasterFields
and return it's sequence number or 0 if not exists}
{@}function FieldNoInFields(FieldName, FieldString: string): integer;
var i,P: integer;
begin
 P:=0; i:=0;
 Result := 0;
 while P <= Length(FieldString) do
  begin
  inc(i);
  if FieldName = ExtractString(FieldString,P,FIELDSDELIMITER) then
     begin
     Result := i;
     Break;
     end;
  end;
end;

{@Get TFieldType value named in a string based on FieldTypeName array}
{@}function FieldTypeByName(sFieldType: string): TFieldType;
var i: TFieldType;
begin
 Result := ftUnknown;
 for i:=Low(FieldTypeNames) to High(FieldTypeNames) do
  if UpperCase(sFieldType) = Uppercase(FieldTypeNames[i]) then
   begin
   Result := i;
   Exit;
   end;
end;

{@ Concat field names into a fields string like FieldNames property }
{@}function FieldsStr(const Names: array of string): string;
var i, j: integer;
begin
  Result := '';
  j:=High(Names);
  for i:=0 to j do
    Result := Result + Format('%s%s',[Names[i],IIF(i<j,FIELDSDELIMITER,'')]);
end;

{@}function QuotedFieldValue(Field: TField): string;
begin
  Result := GetQuotedValue(Field.AsString,Field.DataType);
end;
{@}function QuotedParamValue(Param: TParam): string;
begin
  Result := GetQuotedValue(Param.AsString,Param.DataType);
end;
{@}function GetQuotedValue(s: string; f: TFieldType): string;
begin
  if (f in ftStringTypes) or (f in ftMemoTypes)then  Result := QuotedStr(s)
  else
  if (f in ftDateTimeTypes) then begin
      if FormatForDateValue <> '' then
         Result := Format(FormatForDateValue,[s])
      else
         Result := QuotedStr(s);
      end
  else
  if (f in ftBooleanTypes) then Result := s
  else
     Result := s;
end;

{@ Try retreive tablename from dataset using GetPropValue'TableName' or GetTablenameFromSQL}
{@}function DatasetTableName(Dataset: TDataSet; RemoveDot: Boolean): string;
var s: string;
    sl: TStrings;
    p: integer;
begin
 Result := '';
 Result := IProviderSupport(Dataset).PSGetTableName;
 if Result = '' then
 if TypInfo.IsPublishedProp(Dataset, 'TableName') then
    Result := TypInfo.GetPropValue(DataSet,'TableName')
 else
 if TypInfo.IsPublishedProp(Dataset, 'CommandText') then
    begin
    s := TypInfo.GetPropValue(DataSet,'CommandText');
    Result := GetTableNameFromSQL(s);
    if (Result = '') then
       Result := s;//??? provider/Filename
    end
 else
 if TypInfo.IsPublishedProp(Dataset, 'SQL') then
    begin
    sl := TypInfo.GetObjectProp(DataSet,'SQL') as TStrings;
    Result := GetTableNameFromSQL(sl.Text);
    end;
 if Result = '' then Result := DataSet.Name;
 if RemoveDOT then begin
   p := LastDelimiter('.', Result);
   if p > 0 then Delete(Result,1,p);
   end;
end;

{@ Create pascal record for fields of a dataset}
{@}function DatasetRecordType(Dataset: TDataset): TStringList;
var i: integer;
begin
 Result := TStringList.Create;

 Result.Add(format(' T%s = record',[Dataset.Name]));
 for i:=0 to Dataset.FieldCount-1 do
  with Dataset.Fields[i] do
  begin
  Result.Add(format('   %s : %s;',[FieldName,FieldTypeNames[DataType]]));
  end;
 Result.Add(' end;');

 Result.Add(format('function %s(Dataset: TDataset): T%s;',[Dataset.Name,Dataset.Name]));
 Result.Add('var f: TField;');
 Result.Add('begin');
 for i:=0 to Dataset.FieldCount-1 do
  with Dataset.Fields[i] do
  begin
  Result.Add(format('  f := Dataset.FindField(''%s'');',[FieldName]));
  Result.Add(format('  if f <> nil then Result.%s := f.value;',[FieldName]));
  end;
 Result.Add('end;');

end;

{@}procedure FieldsToParams(Fields: TFields; Params: TParams; doAdd: Boolean=False);
var i: integer;
begin
  if not doAdd then Params.Clear;
  for i:=0 to Fields.Count-1 do
   if not (Fields[i].DataType in ObjectFieldTypes) then
    Params.CreateParam(Fields[i].DataType, Fields[i].FieldName, ptUnknown);
end;

{@}procedure CopyFields(Fields: TFields; ToDataset: TDataset; doAdd: Boolean;
                        FieldMap: TStrings; FieldKinds: TFieldKinds);
//var i: integer;
//    Fld: TField;
begin
  CopyFFields(Fields,ToDataset.Fields,doAdd,FieldMap,FieldKinds);
//  if not doAdd then ToDataset.Fields.Clear;
//  for i:=0 to Fields.Count-1 do
//   if (Fields[i].FieldKind in FieldKinds) or (FieldKinds = []) then
//    begin
//    Fld := CloneField(Fields[i], ToDataset);
//    Fld.DataSet := ToDataset;
//    end;
end;

{@}procedure CopyFFields(Fields, ToFields: TFields; doAdd: Boolean;
                        FieldMap: TStrings; FieldKinds: TFieldKinds);
var i,p: integer;
    Fld: TField;
    dFld: string;
begin
  if not doAdd then ToFields.Clear;
  for i:=0 to Fields.Count-1 do
   if (Fields[i].FieldKind in FieldKinds) or (FieldKinds = []) then
    begin
    if Assigned(FieldMap) then
       begin
       dFld := '';
       p := FieldMap.IndexOfName(Fields[i].FieldName);
       if p <> -1 then
          begin
          dFld := FieldMap.Values[Fields[i].FieldName];
          end
       else
          begin
          p := FieldMap.IndexOf(Fields[i].FieldName);
          if p <> -1 then dFld := Fields[i].FieldName;
          end;
       if dfld <> '' then
          begin
          if Assigned(ToFields.FindField(Fields[i].FieldName)) then Continue;
          Fld := CloneField(Fields[i], ToFields.Dataset);
          Fld.FieldName := dfld;
          Fld.DataSet := ToFields.Dataset;
          end;
       end
    else
       begin
       if Assigned(ToFields.FindField(Fields[i].FieldName)) then Continue;
       Fld := CloneField(Fields[i], ToFields.Dataset);
       Fld.DataSet := ToFields.Dataset;
       end;
    end;
end;

{@}function CreateFieldsFromDefs(Dataset: TDataset): integer;
var
  I: Integer;
begin
 if Dataset.FieldCount > 0 then
  raise Exception.Create('CreateFieldsFromDefs: Dataset has existing fields!');
 with Dataset do begin
  if ObjectView then
  begin
    if FieldDefs.Count = 0 then FieldDefs.Update;
    Result := FieldDefs.Count;
    for I := 0 to FieldDefs.Count - 1 do
      with FieldDefs[I] do
        if (DataType <> ftUnknown) and
          not ((faHiddenCol in Attributes) and not FIeldDefs.HiddenFields) then
          CreateField(Dataset);
  end else
  begin
    if FieldDefList.Count = 0 then FieldDefList.Update;
    Result := FieldDefList.Count;
    for I := 0 to FieldDefList.Count - 1 do
      with FieldDefList[I] do
        if (DataType <> ftUnknown) and not (DataType in ObjectFieldTypes) and
          not ((faHiddenCol in Attributes) and not FIeldDefs.HiddenFields) then
          CreateField(Dataset, nil, FieldDefList.Strings[I]);
  end;
  //SetKeyFields;
 end;
end;

{@}procedure CreateFieldDefsFromFields(Fields: TFields; ToFieldDefs: TFieldDefs);
  procedure CreateFieldDefs(Fields: TFields; FieldDefs: TFieldDefs);
  var
    I: Integer;
    F: TField;
    FieldDef: TFieldDef;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      F := Fields[I];
      with F do
      if FieldKind = fkData then
      begin
        FieldDef := FieldDefs.AddFieldDef;
        FieldDef.Name := FieldName;
        FieldDef.DataType := DataType;
        FieldDef.Size := Size;
        if Required then
          FieldDef.Attributes := [faRequired];
        if ReadOnly then
          FieldDef.Attributes := FieldDef.Attributes + [faReadonly];
        if (DataType = ftBCD) and (F is TBCDField) then
          FieldDef.Precision := TBCDField(F).Precision;
        if F is TObjectField then
          CreateFieldDefs(TObjectField(F).Fields, FieldDef.ChildDefs);
      end;
    end;
  end;
begin
  { Create FieldDefs from persistent fields if needed }
  ToFieldDefs.Clear;
  if ToFieldDefs.Count = 0 then
  begin
    ToFieldDefs.BeginUpdate;
    try
      CreateFieldDefs(Fields, ToFieldDefs);
    finally
      ToFieldDefs.EndUpdate;
    end;
  end;
end;

{@ Create a new field same as Source field and assign source properties }
{@}function CloneField(Source: TField; AOwner: TComponent; NewClass: TFieldClass): TField;
  procedure SetProp(Name: string);
  var V: variant;
      PropInfo: PPropInfo;
  begin
  PropInfo := GetPropInfo(Source, Name);
  if PropInfo <> nil then begin
   try V := TypInfo.GetPropValue(Source,Name);
   if not VarIsNull(V) then TypInfo.SetPropValue(Result,Name,V); except ; end;
   end;
  end;
begin
  if Assigned(NewClass) then
     Result := NewClass.Create(AOwner)
  else
     Result := TFieldClass(Source.ClassType).Create(AOwner);

  Result.Alignment              := Source.Alignment;
  Result.AutoGenerateValue      := Source.AutoGenerateValue;
  Result.CustomConstraint       := Source.CustomConstraint;
  Result.ConstraintErrorMessage := Source.ConstraintErrorMessage;
  Result.DefaultExpression      := Source.DefaultExpression;
  Result.DisplayLabel           := Source.DisplayLabel;
  Result.DisplayWidth           := Source.DisplayWidth;
  Result.FieldKind              := Source.FieldKind;
  Result.FieldName              := Source.FieldName;
  Result.ImportedConstraint     := Source.ImportedConstraint;
  Result.LookupDataSet          := Source.LookupDataSet;
  Result.LookupKeyFields        := Source.LookupKeyFields;
  Result.LookupResultField      := Source.LookupResultField;
  Result.KeyFields              := Source.KeyFields;
  Result.LookupCache            := Source.LookupCache;
  Result.ProviderFlags          := Source.ProviderFlags;
  Result.ReadOnly               := Source.ReadOnly;
  Result.Required               := Source.Required;
  Result.Visible                := Source.Visible;

  SetProp('EditMask');
  SetProp('FixedChar');
  SetProp('Size');
  SetProp('Transliterate');
  SetProp('DisplayFormat');
  SetProp('EditFormat');
  SetProp('Currency');
  SetProp('MaxValue');
  SetProp('MinValue');
  SetProp('Precision');
  SetProp('DisplayValues');
  SetProp('BlobType');
  SetProp('ObjectType');
  SetProp('IncludeObjectField');
  SetProp('ReferenceTableName');
  SetProp('Active');
  SetProp('Expression');
  SetProp('GroupingLevel');
  SetProp('IndexName');
end;

procedure Wide2StringField(Dataset: TDataset);
var i: integer;
    FName: string;
    WF: TField;
    FLD: TField;
begin
  //if not Dataset.DefaultFields then Exit;
  for i:=Dataset.FieldCount-1 downto 0 do
   if (Dataset.Fields[i] is TWideStringField) and (Dataset.Fields[i].Owner = Dataset) then
     begin
     WF := Dataset.Fields[i];
     FName := WF.Name;
     FLD := CloneField(WF,Dataset,TStringField);
     Dataset.Fields[i].Free;
     FLD.Name := FName;
     FLD.DataSet := Dataset;
     end;
end;

{@ open 'Foo' named datasets in a compenet's (form/datamodule) component list}
{@}procedure OpenFooDatasets(UnderComponent: TComponent; FooID: string);
var i,j: integer;
    Foo: TDataset;
    sFoo: string;
begin
  if sFoo = '' then sFoo := FOOSTRING
  else sFoo := FooID;
  j := UnderComponent.ComponentCount-1;
  for i:=0 to j do
   if UnderComponent.Components[i] is TDataset then begin
      Foo := UnderComponent.Components[i] as TDataset;
      if AnsiStartsText(sFoo,Foo.Name) then
         if not Foo.Active then
            begin
            Foo.Open;
            Foo.First;
            end;
   end;
end;

{@ Create Fieldmappings between datasets for fields with same name}
{@}function AutoMapFields(Source, Dest: TDataset): TStringList;
var i,j: integer;
begin
 Result := TStringList.Create;
 j := Source.FieldCount-1;
 for i := 0 to j do
  if Assigned(Dest.FindField(Source.Fields[i].FieldName)) then
     Result.Add(Source.Fields[i].FieldName);
end;

function SQLDateFmTo(fieldname: string; DateFm, DateTo: TDateTime): string;
begin
  //-> ??? Format(FormatForDateValue,[sstring])
  Result := ' ('+fieldname+' >= '''+FormatDateTime('yyyymmdd',DateFm)+''' AND '+
                 fieldname+' < '''+FormatDateTime('yyyymmdd',DateTo+1)+''') ';
end;

{@ GetDatasetSQLText retreives sql statement from SQL/CommandText property
   WARNING: result may contain tablename, storedprocedure... and not sql statement}
{@}function GetDatasetSQLText(Q: TDataset; var sqltext: string): Boolean;
var sSQL: TStrings;
begin
 Result := True;
 if IsPublishedProp(Q, 'SQL') then begin
   sSQL := GetObjectProp(Q, 'SQL', TStrings) as TStrings;
   sqltext := sSQL.Text;
   end
 else
 if IsPublishedProp(Q, 'CommandText') then begin
   sqltext := GetPropValue(Q, 'CommandText');
   end
 else
   Result := False;
end;

{@ SetDatasetSQLText sets sql statement on SQL/CommandText property}
{@}function SetDatasetSQLText(Q: TDataset; sqltext: string): Boolean;
var sSQL: TStrings;
begin
 Result := True;
 if IsPublishedProp(Q, 'SQL') then begin
   sSQL := GetObjectProp(Q, 'SQL', TStrings) as TStrings;
   sSQL.Text := sqlText;
   end
 else
 if IsPublishedProp(Q, 'CommandText') then
   SetPropValue(Q, 'CommandText', sqltext)
 else
   Result := False;
end;

{@}function CreateSQLText(ForTable: string; KeyFields: string; KeyValues: variant;
{@}                       OrderFields: string; InSQL: TStrings): integer;
var p: integer;
    s, sAnd: string;
begin
    Result := 0;
    InSQL.Add('select * from '+ForTable);
    if KeyFields <> '' then
       begin
       InSQL.Add('where');
       p:=0; sAnd := '';
       while p<=Length(KeyFields) do begin
        s := ExtractString(KeyFields,p,FIELDSDELIMITER);
        InSQL.Add(Format('  %s %s = :%s',[sAnd, s, s]));
        sAnd := 'and';
        Inc(Result); //denote number of fields
        end;
       end;
    if OrderFields <> '' then
       InSQL.Add('order by ' + StringReplace(OrderFields,FIELDSDELIMITER,', ',[rfReplaceAll]));
  end;

{@}function CreateSQLText(ForTable: string; KeyFields: string; KeyValues: variant; OrderFields: string;
                       out Text: string): integer;
var p: integer;
    s,sv, sAnd: string;
    Param: TParam;
begin
    Result := 0; Text := '';
    Text := 'select * from '+ForTable;
    if KeyFields <> '' then
       begin
       Text := Text +'where';
       p:=0; sAnd := '';
       Param := TParam.Create(nil);
       try
        while p<=Length(KeyFields) do begin
          s := ExtractString(KeyFields,p,FIELDSDELIMITER);
          if (Result = 0) and not VarIsArray(KeyValues) then
           Param.Value := KeyValues
          else
           Param.Value := KeyValues[Result];
          sv := QuotedParamValue(Param);
          Text := Text + Format('  %s %s = %s',[sAnd, s, sv]);
          sAnd := 'and';
          Inc(Result); //denote number of fields
          end;
       finally Param.Free;
       end;
       end;
    if OrderFields <> '' then
       Text := Text + 'order by ' + StringReplace(OrderFields,FIELDSDELIMITER,', ',[rfReplaceAll]);
end;

{@}function IsDatasetCanModify(Dataset: TDataset; DefReadOnly: Boolean): Boolean;
begin
 Result := DefReadOnly;
 if Dataset.Active then Result := Dataset.CanModify
 else
 if IsPublishedProp(Dataset,'ReadOnly') then
   try Result := (GetOrdProp(Dataset,'ReadOnly') <> 1); except ; end
 else
 if IsPublishedProp(Dataset,'LockType') then
   try Result := (GetOrdProp(Dataset,'LockType') <> 1); except ; end //ADODB.TADOLockType.ltReadOnly
end;

{@}function IsDatasetQuery(Dataset: TDataset): Boolean;
begin
 Result := False;
 if IsPublishedProp(Dataset,'TableName') then Result := False
 else
 if IsPublishedProp(Dataset,'SQL') then Result := True
 else
 if IsPublishedProp(Dataset,'CommandType') then
  try Result := (GetOrdProp(Dataset,'CommandType')=1) except ; end; //ADODB.TCommandType.cmdText ???
end;

{@}function ParseSQL(SQL: string; Section: TSQLToken; ParseList: TStrings): string;
  var
    Start, sec1, sec2: PChar;
    FName: string;
    SQLToken, CurSection: TSQLToken;
    sSec, s: string;
begin
    Start := PChar(SQL);
    sec1 := Start;
    sec2 := Start;
    CurSection := stUnknown;
    repeat
      SQLToken := NextSQLToken(Start, FName, CurSection);
      if (SQLToken in SQLSections) or (SQLToken = stEnd) then
        begin
        if (Sec2 > Sec1) then
           begin
           SetString(s,Sec1,Sec2-Sec1);
           s := StringReplace(s,#10,' ',[rfReplaceAll]);
           s := StringReplace(s,#13,' ',[rfReplaceAll]);
           if Assigned(ParseList) then
              ParseList.Append(sSec+'='+s);
           if Section = CurSection then
              Result := s;
           end;
        CurSection := SQLToken;
        sSec := FName;
        sec1 := Start;
        sec2 := Start;
        end
      else
        Sec2 := Start;
    until SQLToken in [stEnd];
end;

{@}function ChangeSQLWhere(Sql, sWhere: string; DoAdd: Boolean): string;
var sL: TStringList;
   sw: string;
   i: integer;
begin
  sL := TStringList.Create;
  try
   Result := '';
   { TODO : ΠΡΟΣΟΧΗ!!! ΤΟ ChangeSQLWhere ΔΕΝ ΛΕΙΤΟΥΡΓΕΙ ΜΕ SUBSELECT!!! }
   sw := UtilDB.ParseSQL(sql,stWhere,sL);
   for i:=0 to sL.Count-1 do
    begin
    if AnsiSameText(sL.Names[i],'where') then
       begin
       if DoAdd then begin
          Result := Result + sL.Names[i] + ' '+ sL.Values[sL.Names[i]] + ' ';
          if sWhere <> '' then
             Result := Result + ' AND (' + sWhere + ') ';
          end
       else begin
          if sWhere <> '' then
          Result := Result + ' where ' + sWhere +' ';
          end;
       end
    else
    if AnsiSameText(sL.Names[i],'from') and (sw = '') then
       begin
       Result := Result + sL.Names[i] + ' '+ sL.Values[sL.Names[i]] + ' ';
       if sWhere <> '' then
          Result := Result + ' where ' + sWhere+ ' ';
       end
    else
       Result := Result + sL.Names[i] + ' '+ sL.Values[sL.Names[i]] + ' ';
    end;
  finally sL.Free;
  end;
end;

function CreateTParams(const aParams: array of TParamProps): TParams;
var i: integer;
begin
  Result := TParams.Create;
  for i:=0 to High(aParams) do
   Result.CreateParam(aParams[i].DataType,aParams[i].Name,ptUnknown).Value := aParams[i].Value;
end;

function UpdateParam(Params: TParams; const Path: array of string;
                     Value: Variant; FldType: TFieldType): Boolean;
var WP: TParams;
    P: TParam;
    A: array of string;
    i: integer;
begin
  if Length(Path) = 1 then
     begin
     P := Params.FindParam(Path[0]);
     if not Assigned(P) then
        P := Params.CreateParam(FldType,Path[0],ptUnKnown);
     P.Value := Value;
     Result := True;
     end
  else
     begin
     P := Params.FindParam(Path[0]);
     if not Assigned(P) then
        P := Params.CreateParam(ftString,Path[0],ptUnKnown);
     WP := TParams.Create;
     try if P.AsString <> '' then
           StringToParams(P.AsString, WP);
      SetLength(A,Length(Path)-1);
      for i:=0 to Length(A)-1 do A[i] := Path[i+1];
      Result := UpdateParam(WP,A,Value,FldType);
      P.AsString := UtilDB.ParamsToString(WP);
      finally WP.Free;
      end;
     end;
end;

function UpdateParam(strParams: string; const Path: array of string;
                     Value: Variant; FldType: TFieldType): string;
var wParams: TParams;
begin
  wParams := TParams.Create(nil);
  try
   if strParams<>'' then StringToParams(strParams,wParams);
   if not UpdateParam(wParams,Path,Value,FldType) then
      raise Exception.Create(StringFromStrings(Path,'/')+' .Update of param failed!');
   Result := ParamsToString(wParams);
   finally
    wParams.Free;
   end;
end;

function ParamsToString(Params: TParams): string;
var ps: TParamsStorage;
begin
  ps := TParamsStorage.Create(nil);
  try ps.Params := Params;
   Result := ComponentToString(ps);
  finally ps.Free;
  end;
end;
procedure StringToParams(Value: string; Params: TParams);
var ps: TParamsStorage;
begin
  ps := TParamsStorage.Create(nil);
  try ps.Params := Params;
   ps.Params.Clear;
   StringToComponent(Value,ps);
  finally ps.Free;
  end;
end;

function StringToParams(Value: string): TParams;
begin
  Result := TParams.Create;
  StringToParams(Value,Result);
end;

function FormatFields(Fields: TFields; StrFormats: array of TStrFormat): string;
var i: integer;
    f: TField;
begin
 Result := '';
 for i:=Low(StrFormats) to High(StrFormats) do
  begin
  if Assigned(Fields) then
     begin
     f := Fields.FindField(StrFormats[i].Name);
     if Assigned(f) then
        Result := Result + FormatField(f,StrFormats[i])
     else
        Result := Result + StrFormatValue(Null,ftUnKnown,StrFormats[i]);
     end
  else
     Result := Result + StrFormatValue(Null,ftUnKnown,StrFormats[i]);
  end;
end;

function FormatField(Field: TField; StrFormat: TStrFormat): string;
begin
 if StrFormat.Format = '' then
    Result := StrFormatValue(Field.DisplayText, ftString, StrFormat)
 else
    Result := StrFormatValue(Field.Value, Field.DataType, StrFormat);
end;

function FormatParams(Params: TParams; StrFormats: array of TStrFormat): string;
var i: integer;
    p: TParam;
begin
 Result := '';
 for i:=Low(StrFormats) to High(StrFormats) do
  begin
  if Assigned(Params) then
     begin
     p := Params.FindParam(StrFormats[i].Name);
     if Assigned(p) then
        Result := Result + FormatParam(p,StrFormats[i])
     else
        Result := Result + StrFormatValue(Null,ftUnKnown,StrFormats[i]);
     end
  else
     Result := Result + StrFormatValue(Null,ftUnKnown,StrFormats[i]);
  end;
end;

function FormatParam(Param: TParam; StrFormat: TStrFormat): string;
begin
 Result := StrFormatValue(Param.Value, Param.DataType, StrFormat);
end;

function StrFormatValue(Value: Variant; DataType: TFieldType; StrFormat: TStrFormat): string;
var V: Variant;
    s: string;
    p,w: integer;
begin
 if (VarIsNull(Value) or VarIsEmpty(Value)) then
    begin
    if DataType in ftDateTimeTypes then
       V := StrFormat.DTEValue
    else
    if DataType in ftBooleanTypes then
       V := StrFormat.BITValue
    else
    if DataType in ftNumberTypes then
       V := StrFormat.NUMValue
    else //??? DTE/NUM/BITValue ??? need datatype in TStrFormat
    if StrFormat.DataType in ftDateTimeTypes then
       V := StrFormat.DTEValue
    else
    if StrFormat.DataType in ftBooleanTypes then
       V := StrFormat.BITValue
    else
    if StrFormat.DataType in ftNumberTypes then
       V := StrFormat.NUMValue
    else
       V := StrFormat.Value;
    end
 else
    V := Value;

 if (VarIsNull(V) or VarIsEmpty(V)) then
    V := '';

 if StrFormat.Format = '' then
    s := Variants.VarAsType(V,varString)
 else
  if DataType in ftDateTimeTypes then
     DateTimeToString(s,StrFormat.Format,V)
  else
  if DataType in ftBooleanTypes then
     begin
     p:=1; s := UtilFun.ExtractString(StrFormat.Format,p,';');
     if not Variants.VarAsType(V,varBoolean) then s := UtilFun.ExtractString(StrFormat.Format,p,';')
     end
  else
  if DataType in ftNumberTypes then
     s := FormatFloat(StrFormat.Format, V)
  else
  if StrFormat.DataType in ftDateTimeTypes then
     DateTimeToString(s,StrFormat.Format,V)
  else
  if StrFormat.DataType in ftBooleanTypes then
     begin
     p:=1; s := UtilFun.ExtractString(StrFormat.Format,p,';');
     if not Variants.VarAsType(V,varBoolean) then s := UtilFun.ExtractString(StrFormat.Format,p,';')
     end
  else
  if StrFormat.DataType in ftNumberTypes then
     s := FormatFloat(StrFormat.Format, V)
  else
     s := Variants.VarAsType(V,varString);

 if StrFormat.Width > 0 then
    begin
    { TODO : REVIEW/CHECK SIZES STUFFSTRING WORKS UNEXPECTED}
    w := StrFormat.Width;
    Result := StringOfChar(' ',w);
    case StrFormat.Alignment of
      //taLeftJustify:  Result := StuffString(Result,1,w{Length(s)},Copy(s,1,w));
      taRightJustify: begin
                      p := Length(s);
                      if p > w then
                         Result := StuffString(Result,1,w,copy(s,p-w+1,w))
                      else
                         Result := StuffString(Result,w-p+1,p,s);
                      end;
      taCenter:       begin
                      p := Length(s);
                      if p > w then
                         Result := StuffString(Result,1,w,Copy(s,1,w))//???
                      else
                         Result := StuffString(Result,((w-p) div 2)+1,p,s);
                      end;
     else
        begin
        p := Length(s);
        if p >= w then
           Result := copy(s,1,w)
        else
           Result := StuffString(Result,1,p,s);
        end;
      //Result := StuffString(Result,1,w{Length(s)},Copy(s,1,w));
     end;
    end
 else Result := s;

 if StrFormat.UpperCase then Result := AnsiUppercase(Result);
 Result := StrFormat.Prefix+Result+StrFormat.Suffix;
end;


{==============================================================================}
{@ DATASETPROVIDER / REMOTEDATAMODULE                                          }
{==============================================================================}

{@ Build array of dataset properties for use by TTClientDataset}
{@}procedure ProvideDataSetProperties(DataSet: TDataSet; var Properties: OleVariant);
var LUA: TLookupInfoRecs;
    i: integer;
begin
 if varIsNull(Properties) or varIsEmpty(Properties) then begin
    Properties := VarArrayCreate([0,0], varVariant);
    i:=0
    end
   else begin
        i:=VarArrayHighBound(Properties,1);
        Inc(i,1);
        VarArrayRedim(Properties,i);
        end;
  Properties[i] := VarArrayOf([PROVIDERPROPERTY_TABLENAME, DatasetTableName(Dataset, True), False]);

  LUA := BuildLookupInfo(Dataset);
  if not IsEmptyInfoRecs(LUA) then begin
     i:=VarArrayHighBound(Properties,1);
     Inc(i,1);
     VarArrayRedim(Properties,i);
     Properties[i] := VarArrayOf([PROVIDERPROPERTY_LOOKUPRECS, LUA, False]);
     end;
end;

{@ LookupInfoRec from/to variant convertion functions }
{@}function IsEmptyInfoRecs(Value: TLookupInfoRecs): Boolean;
begin
 Result := Value = '';
end;
{@}function UnstringInfoRecs(Value: TLookupInfoRecs): TStringList;
begin
  Result := Unstring(Value,[LOOKREC_SEP],[]);
end;
{@}function LookupInfoRec(Value: WideString): TLookupInfoRec;
var p: integer;
begin
   p:=0;
   Result.FieldName          := ExtractString(Value,p,LOOKFLD_SEP);
   Result.KeyFields          := ExtractString(Value,p,LOOKFLD_SEP);
   Result.Provider           := ExtractString(Value,p,LOOKFLD_SEP);
   Result.LookupKeyFields    := ExtractString(Value,p,LOOKFLD_SEP);
   Result.LookupResultField  := ExtractString(Value,p,LOOKFLD_SEP);
   Result.FieldKind          := TFieldKind(GetEnumValue(TypeInfo(TFieldKind),ExtractString(Value,p,LOOKFLD_SEP)));
   Result.ReadOnly           := Boolean(StrToInt(ExtractString(Value,p,LOOKFLD_SEP)));
   Result.Visible            := Boolean(StrToInt(ExtractString(Value,p,LOOKFLD_SEP)));
end;
{@}function LookupInfoVar(Value: TLookupInfoRec): WideString;
begin
  Result := StringFromStrings(
         [Value.FieldName,Value.KeyFields,Value.Provider,
          Value.LookupKeyFields,Value.LookupResultField,
          GetEnumName(TypeInfo(TFieldKind),Ord(Value.FieldKind)),
          IIF(Value.ReadOnly,'1','0'), IIF(Value.Visible,'1','0') ]
         ,LOOKFLD_SEP);
end;

{@ Build LookupInfoRecs array of information for lookup fields of a dataset
   & it's detail/nested datasetsfor Dataset
   Is used in Provider.OnGetDatasetProperties event for passing lookup info to the
    remote clientdataset.
    }

{@}function BuildLookupInfo(ForDataset: TDataset): TLookupInfoRecs;

 function FindProviderName(c: TComponent; ForDset: TDataSet): string;
 var i, j: integer;
 begin
  Result := '';
  if not Assigned(c) then Exit;
  j:=c.ComponentCount-1;
  for i:=0 to j do
   if c.Components[i] is TDatasetProvider then
   with c.Components[i] as TDatasetProvider do
   if (Dataset = ForDset) and Exported then begin
      Result := Name;
      Break; end;
 end;

var i,j: integer;
    LU: TLookupInfoRec;
begin
  Result := '';
  j:=ForDataset.FieldCount-1;
  for i:=0 to j do
   with ForDataset.Fields[i] do
   if Assigned(LookupDataset) then //locate provider for LookupField
    begin
    if LookupDataSet is TClientDataset then
       LU.Provider := TClientDataset(LookupDataSet).ProviderName
    else
       begin
       LU.Provider := FindProviderName(LookupDataSet.Owner,LookupDataSet);
       if LU.Provider = '' then
          LU.Provider := FindProviderName(ForDataset.Owner,LookupDataSet);
       end;
    if LU.Provider <> '' then begin //provider found! build info record
       LU.FieldKind := FieldKind;
       LU.FieldName := FieldName;
       LU.KeyFields := KeyFields;
       LU.LookupKeyFields := LookupKeyFields;
       LU.LookupResultField := LookupResultField;
       LU.ReadOnly := ReadOnly;
       LU.Visible := Visible;
       Result := StringFromStrings([Result,LookupInfoVar(LU)],LOOKREC_SEP);
       end;
    end; //fklookup

  {@ NOTE: the event is fired first for all the detail datasets of the
   provider's dataset so we do not need to recurse }

   //else //check TDatasetField
   // if ForDataset.Fields[i] is TDatasetField then
   // with ForDataset.Fields[i] as TDatasetField do
   //  begin //recurse into NestedDataset
   //    LUA := BuildLookupInfo( NestedDataset );
   //    for k:=0 to High(LUA) do
   //        Result[High(Result)+1] := LUA[k];
   //  end; //TDatasetField

  //now check & recurse for detail datasets
  //dL := TList.Create;
  //try
  // ForDataset.GetDetailDataSets(dL);
  // for i:=0 to dL.Count-1 do begin
  //     LUA := BuildLookupInfo( TDataset(dl.Items[i]) );
  //     for k:=0 to High(LUA) do
  //         Result[High(Result)+1] := LUA[k];
  //     end;
  //finally dL.Free;
  //end;
end;

{@}function EncryptCDS(CDS: TClientDataset; decrypt: boolean; c: integer; FileName: string): boolean;
var s: TMemoryStream;
begin
 Result := False;
 s := TMemoryStream.Create;
 try
  if decrypt then
   begin
   if CDS.Active then raise exception.Create('CDS is active!!!');
   s.LoadFromFile(FileName);
   s.Seek(0,soFromBeginning);
   EncryptStream(s,true,c);
   s.Seek(0,soFromBeginning);
   CDS.LoadFromStream(s);
   Result := True;
   end
  else
   begin
   if not CDS.Active then raise exception.Create('CDS is not active!!!');
   s.Seek(0,soFromBeginning);
   CDS.SaveToStream(s,dfXML);
   s.Seek(0,soFromBeginning);
   EncryptStream(s,false,c);
   s.Seek(0,soFromBeginning);
   s.SaveToFile(FileName);
   Result := True;
   end;
 finally s.free;
 end;
end;

{ TMoveCorrMapping }

constructor TMoveCorrMapping.Create;
begin
  inherited Create;
end;

destructor TMoveCorrMapping.Destroy;
begin
  SetLength(FPairs,0);
  inherited;
end;

function TMoveCorrMapping.AddPair(aSrcFld, aDstFld: TField): integer;
var aPair: TMoveCorrPair;
begin
  SetLength(FPairs,Count+1);
  aPair := FPairs[High(FPairs)];
  aPair.SourceField := aSrcFld;
  aPair.DestField := aDstFld;
end;

function TMoveCorrMapping.GetCount: integer;
begin
  Result := Length(FPairs);
end;

function TMoveCorrMapping.GetPair(index: integer): TMovecorrPair;
begin
  if (index < 0) or (index >= Count) then
    raise Exception.Create('Pair Index out of bounds ('+IntToStr(index)+').')
  else
    Result := FPairs[index];
end;

initialization
 FormatForDateValue := '';
 RegisterClass(TParamsStorage);
finalization
 UnregisterClass(TParamsStorage);


end.
