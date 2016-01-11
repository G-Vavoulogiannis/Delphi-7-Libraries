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
 Description.....: Various utily functions
 Developer/Author: George J. Vavoulogiannis, georgev@hol.gr
                   http://g-vavoulogiannis.blogspot.com

===============================================================================}


  { --- DONE HISTORY ----------------------------------------------------------}

{HISTORY
17/3/97 SetFieldLabels: IniFile to open as parameter or default to 'VAR.INI'
28/3/97 SetFieldLabels: section of IniFile to access or deafault to tablename
                        parameter in the form 'inifile,section'
28/3/97 FontToStr & FontFmStr : improved functions with pitch and code enhancements
31/3/97 ReplaceStr: correction to for loop which raised ElistOutofRange error
6/4/97 Added functions 'GetParentWindowNameAt', 'AddBitmapToMenuItem',
        'OpenObject', 'DeleteFileWithUndo', 'RunOnStartup'
8/4/97 'GradientFill' procedure added
17/4/97 AddBitmapToMenuItem turned to function
21/5/97 added utility dialog functions 'ConfirmYesNoCancel' & 'ConfirmYesNo'
30/6/97 new procedure 'FieldPickDate'
1/12/97 new function 'IIF'
30/12/97 new procedure 'MoveCorr' fieldvaluess from one table to another
2/1/98  procedure WriteText copied from DBGrids.pas
18/1/98  procedure Frame3D copied from ExtCtrls.pas
01/02/98 procedures 'WriteTableState' & 'ReadTableState' that
         save & load properties (Filter, IndexFields, Visible Fields)
         of a Table in IniFile
11/03/98 StringFromVariant function to concat variant values into one string
14/03/98 StringFromFields function to concat field values into one string
29/03/98 IsModuleRunning function to check if a named module is currently running
19/07/98 VisibleFieldsList function that returns a string list of all visible fields
         of Dataset
26/07/98 new function "FieldInFieldString" that searches a fields string like
         MasterFields property for a given fieldname and returns true if it is found.
11/10/98 GetPrimaryIndex does not check for active table nor
         updates IndexDefs before accessing them.
11/10/98 new function 'GetFieldDefs' that returns a string containing all fieldname
         of a dataset fielddefs property delimited by FIELDSDELIMITER.
26/10/98 new function 'FieldTypeByName' that returns the field type from
         it's string name (based on array 'FieldTypeName').
         new functions 'WriteTableDef' and 'ReadTabledef' that writes and read
         table definition (tablename, type, fielddefs, indexdefs) in a .Def
         (.Ini) file.
         new function 'BackupTable';
         new procedure 'CreateTablesFromDef' that creates tables defined in
         a Tables.Def file (.Ini oriented) which has been created from 'WriteTableDef'.
01/11//98 procedures 'WriteVisibleFields' & 'ReadVisibleFields' that
         save & load Visible Fields of a Table in IniFile.
         New section for Date Functions.
         New function IncreaseMonth.
08/11/98 New function "FieldNoInFields" to check if a FieldName exists in a Fields
         string like MasterFields and return it's sequence number or 0 if not exists.
         Correction to function ReplaceStr wich added replace string at the end
         of the resulting string.
09/11/98 New function "StringBetween" that extracts a string between two other strings
13/11/98 New function "GetInsertedAa" to quickly retrieve the next available
         record key field (next no) for index, like autoincrement.
18/11/98 New function AssignVar that checks a variant for null value
         and if so returns the default value parameter.
18/12/98 correction in 'WriteTableDef' and 'ReadTableDef' for single field
         index names. Write function did not write the valuekey and read function
         did not read correctly the section values.
24/12/98 New procedure 'CloneComponent' that copies a component properties to
         an existing one of same class using streaming.
         New function 'ReplicateComponent' that creates a new component with
         of the same class as the given and copies all properties using streaming.
30/06/99 table definitions file & extension in constants
         new parameter 'Description' in Writetabledef
27/09/99 "ExploreWeb" new function to open web pages
26/12/99 new "LeftStr", "RightStr", "StrSplit" string functions
26/12/99 split of database routines from UtilFun
25/01/00 New function DateSer like VB
23/02/2000 Error & ErrorFmt renamed to ExceptionXXXXXX
14/03/2000 Recovered TRectWH type, RectWH & CenterInWH functions from PRTOUT.pas,
08/05/2000 KillMessage procedure from DBGrids
12/05/2000 new function Greeklish that converts a Greek characters in a string
           to Greeklish
31/05/2000 New function VarEquals copied from DBCtrls
09/11/2000 new function CopyComponent
12/07/2001 new function FuncAvail & IsInternetConnected
24/02/2007 changes to StringBetween/Pos for caseinsensitive search
24/10/2011 function AFMCheck(AFM: string): Boolean;
}

unit Utilfun;

interface

uses SysUtils, WinTypes, WinProcs, ShellAPI, Messages, Forms, Graphics, Controls,
Classes, Registry, Dialogs, TypInfo, ExtCtrls, StdCtrls, checklst, StrUtils,
variants;

type
{@}  TRectWH = record
{@}    Left, Top, Width, Height: integer;
{@}  end;
     TStringArray = array of string;

{*** STRING ***}
function ExtractString(const Fields: string; var Pos: Integer; const Delim: Char): string;
procedure Unstring(sStr: string;
                   Items: TStrings;
                   const DelimOut, DelimIn: array of string); overload;
function  Unstring(sStr: string;
                   const DelimOut, DelimIn: array of string): TStringList; overload;
procedure FontFmStr(var Font: TFont; sFont: string);
function FontToStr(const Font: TFont): string;
function LTrim(s: string): string;
function RTrim(s: string): string;
function AllTrim(s: string): string;
function sBreakApart(BaseString, BreakString: string; StringList: TStringList): TStringList;
function pBreakApart(BaseString, BreakString: PChar; StringList: TStringList): TStringList;
function ReplaceStr(BaseString, ReplaceThis, WithThis: string): string;
function StringBetween(SearchStr, FromThis, ToThis: string; CaseIns: Boolean=False; FromPos: integer=0): string;
function StringBetweenPos(SearchStr, FromThis, ToThis: string; CaseIns: Boolean; var StrPos: integer): string;
function QuickStrings(const Ss: array of string): TStringList; overload;
function QuickStrings(const Ss: array of string; const Data: array of Pointer): TStringList; overload;
function ArrayFromStrings(Value: TStrings; ValOrName: integer=0): TStringArray;
function ArrayFromFields(Value: string): TStringArray;
function StringFromStrings(const Value: array of string; Separator: AnsiString = ''): string;
function StringFromVariant(Value: Variant; Separator: AnsiString = ''; WhenEmpty: AnsiString = '*???*'): string;
procedure StrSplit(SrcStr : String; BreakDownPos : Integer;
	var S1, S2 : String);
function LeftStr(S : String; ToPos : Integer) : String;
function RightStr(S : String; ToPos : Integer) : String;
function Greeklish(s: string;grPfx:string='';grSfx:string=''): string;
function StrInArray(Str: string; const sArray: array of string; CaseIns: Boolean=False): Boolean;
function ValidIdentStr(const Ident: string; Replstr: string=''): String;
function IsNumericString(str: string): Boolean;
function IsHEXString(str: string): Boolean;
function CheckAndFormat(Text: string): string;

function StrPosInArray(Str: string; const sArray: array of string; iLow: integer; CaseIns: Boolean=False): Integer;
function ElfHash(const Value: string): Integer;
function GetHashCode(Str: PChar): Integer;

{*** DATE ***}
function IncreaseMonth(Value: TDateTime; Incr: Integer): TDateTime;
function  EndOfMonth(D: TDateTime): TDateTime;
function DateSer(y,m,d: Integer): TDateTime;

{*** SYSTEM ***}
procedure DragMoveControl(Ctl: TControl);
function GetParentWindowNameAt(X, Y : integer ) : string;
procedure OpenObject(sObjectPath : string );
procedure PrintObject( sObjectPath : string );
procedure ExploreWeb(handle:HWND ; page:PChar);
function DeleteFileWithUndo(sFileName: string) : boolean;
procedure RunOnStartup(sProgTitle, sCmdLine: string; bRunOnce: boolean);
function IsModuleRunning(ModuleName: string): Boolean;
procedure KillMessage(Wnd: HWnd; Msg: Integer);
function FuncAvail(VLibraryname, VFunctionname: string; var VPointer: pointer): boolean;
function IsInternetConnected: Boolean;

{*** GRAPHICS ***}
procedure GradientFill(DC: HDC; FBeginColor, FEndColor: TColor; R: TRect; VertFill: Boolean);
procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment);
procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);
function DisplayWidthToPixels(Canvas: TCanvas; DisplayWidth: integer; Font: TFont=nil): integer;
function RectWH(ALeft,ATop,AWidth,AHeight: integer): TRectWH;
function CenterInWH(InWidth, InHeight, AWidth, AHeight: integer): TRectWH;

{*** DIALOGS ***}
function ConfirmYesNo(const Msg: string): Word;
function ConfirmYesNoCancel(const Msg: string): Word;
//function ListSelect(SelList: array of string; ACaption: string; Selection: integer=0): integer;
//function MultiSelect(SelList: array of string; ACaption: string; var Selection: string): Boolean;
procedure raiseWarn(const ErrMsg: string);
procedure raiseError(const ErrMsg: string; ErrClass: ExceptClass=nil);
procedure raiseErrorFmt(const ErrMsg: string; Params: array of const; ErrClass: ExceptClass=nil);

{*** VARIOUS ***}
function RoundFloat(Amount: Extended; Decs: Byte=2): Extended;
function IIF(expr: Boolean; caseTrue, caseFalse: variant): variant;
procedure SwapVar(var sw1, sw2: variant);
function Max(X, Y: Integer): Integer;
function AssignVar(Value, DefaultValue: variant): variant;
function VarEquals(const V1, V2: Variant; CheckAsString: Boolean=False): Boolean;
function EncryptStream(Stream: TStream; decrypt: boolean; c: integer): boolean;
function EncryptString(S: string; decrypt: boolean; c: integer): string;
function AFMCheck(AFM: string): Boolean;

{*** COMPONENT ***}

type
{ TPropInfoList }
  TPropInfoList = class(TObject)
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    function Get(Index: Integer): PPropInfo;
  public
    constructor Create(AObject: TObject; Filter: TTypeKinds);
    destructor Destroy; override;
    function Contains(P: PPropInfo): Boolean;
    function Find(const AName: string): PPropInfo;
    procedure Delete(Index: Integer);
    procedure Intersect(List: TPropInfoList);
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;

function SetComponentProperty(c: TComponent; PropName: string; Value: variant): Boolean;
procedure CloneComponent(C1: TComponent; C2: TComponent);
function ReplicateComponent(C: TComponent; aOwner: TComponent): TComponent;
function CopyComponent(C: TComponent; IncList: array of string): TComponent;
procedure ComponentToFile(Component: TComponent; FileName: string);
function FileToComponent(FileName: string; Component: TComponent=nil): TComponent;
function ComponentToString(Component: TComponent): string;
function StringToComponent(Value: string; Component: TComponent=nil): TComponent;
function LoadFileToString(FileName: string): string;
function SaveStringToFile(s: string; FileName: string): Boolean;

const
{@}  //GV 12/11/2011 remout in favor of gettext, use "sLineBreak" instead ... CRLF = #13#10;
{@}  TAB = #9;
{@}  PAGEBREAK = #12;
{@}  ASTERISK = '*';
{@}  PIPE = '|';
{@}  FORWARDSLASH = '/';
{@}  BACKWARDSLASH = '\';

  //@CopyComponent constants
{@}  CPY_ALL       = '*ALL';
{@}  CPY_CLASS     = '*CLASS';
{@}  CPY_METHOD    = '*METHOD';
{@}  CPY_INTERFACE = '*INTERFACE';
{@}  CPY_RECORD    = '*RECORD';
{@}  CPY_ARRAY     = '*ARRAY';
{@}  CPY_DYNARRAY  = '*DYNARRAY';
{@}  CPY_PROPERTY  = '*PROPERTY';

var
  DrawBitmap: TBitmap;

implementation

uses Windows;

{==============================================================================}
{@ STRING FUNCTIONS                                                            }
{==============================================================================}

{@Extract a string form 'Fields' starting from Pos and ending on 'Delim'
'Pos' at exit points right after delim or >length(fields)}
{@}function ExtractString(const Fields: string; var Pos: Integer; const Delim: Char): string;
var
  I,P: Integer;
begin
  I := Pos;
  if I < 1 then I:=1; {starting position of a string is always 1 not 0}
  P:=I;
  while (I <= Length(Fields)) and (Fields[I] <> Delim) do Inc(I);
  Result := Copy(Fields, P, I - P);
  if (I <= Length(Fields)) and (Fields[I] = Delim) then Inc(I);
  Pos := I;
end;

{@Unstring a 'sStr' into 'Items' based on 'DelimOut' and 'DelimIn'
 delimiter strings, where DelimOut's will be cut out from result
 and DelimIn's will become separate items in result}
{@}procedure Unstring( sStr: string;
                    Items: TStrings;
                    const DelimOut, DelimIn: array of string );
var d,p,minp: integer;
    str, strout, strin: string;
begin
  str:=sStr;
while str <> '' do
  begin
  minp:=0;
  strout:='';
  strin:='';
  for d:=0 to High(DelimOut) do
   begin
   p:=Pos(DelimOut[d],str);
   if (p <> 0) and ((minp = 0) or (p < minp)) then
      begin
      strout := DelimOut[d];
      minp := p;
      end;
   end;
  for d:=0 to High(DelimIn) do
   begin
   p:=Pos(DelimIn[d],str);
   if (p <> 0) and ((minp = 0) or (p < minp)) then
      begin
      strin := DelimIn[d];
      minp := p;
      end;
   end;
  if minp = 0 then
     begin
     Items.Add(str);
     str:='';
     end
  else
  if strin <> '' then
     begin
     if minp > 1 then Items.Add(Copy(str,1,minp-1));
     Items.Add(strin);
     Delete(str,1,minp+Length(strin)-1);
     end
  else
     begin
     if minp > 1 then Items.Add(Copy(str,1,minp-1));
     Delete(str,1,minp+Length(strout)-1);
     end;

  end;{while}
end;
{@ Overloaded version}
{@}function Unstring(sStr: string;
                   const DelimOut, DelimIn: array of string): TStringList; overload;
begin
  Result := TStringList.Create;
  UnString(sStr,Result,DelimOut,DelimIn);
end;

{@remove leading blanks (spaces)}
{@}function LTrim(s: string): string;
var i: integer;
begin
  i := 1;
  while s[i] = ' ' do inc(i);
  result := copy(s, i, length(s) - i + 1);
end;
{@Supresses trailing blanks}
{@}function RTrim(s: string): string;
begin
  while s[length(s)] = ' ' do delete(s,length(s),1);
  result := s;
end;
{@Supresses leading & trailing blanks}
{@}function AllTrim(s: string): string;
begin
  result := rTrim(lTrim(s));
end;

{@BreakApart function for string}
{@}function sBreakApart(BaseString, BreakString: string; StringList: TStringList): TStringList;
var
  EndOfCurrentString: byte;
begin
  repeat
    EndOfCurrentString := Pos(BreakString, BaseString);
    if EndOfCurrentString = 0 then
      StringList.add(BaseString)
    else
      StringList.add(Copy(BaseString, 1, EndOfCurrentString - 1));
    BaseString := Copy(BaseString, EndOfCurrentString + length(BreakString), length(BaseString) - EndOfCurrentString);
  until EndOfCurrentString = 0;
  result := StringList;
end;
{@BreakApart for PChar}
{@}function pBreakApart(BaseString, BreakString: PChar; StringList: TStringList): TStringList;
var
  BreakStringLength: word;
  pEndOfCurrentString, pEndOfBaseString: PChar;
{Automatically gets memory allocated for it.}
  temp: array[0..255] of char;
begin
{Initialize the pointers.}
  BreakStringLength := StrLen(BreakString);
  pEndOfBaseString := BaseString;
  inc(pEndOfBaseString, StrLen(BaseString));
  repeat
    pEndOfCurrentString := StrPos(BaseString, BreakString);

    StringList.add(StrPas(StrLCopy(temp, BaseString, pEndOfCurrentString - BaseString)));
    inc(BaseString, pEndOfCurrentString - BaseString + BreakStringLength);
  until BaseString >= pEndOfBaseString - BreakStringLength;
  result := StringList;
end;

{@ search and replace string}
{@}function ReplaceStr(BaseString, ReplaceThis, WithThis: string): string;
var
  t: TStringList;
  i,ito: integer;
begin
  t := TStringList.create;
  try
  sBreakApart(BaseString, ReplaceThis, t);
  if t.count > 1 then
  begin
    result := '';
    ito := t.count - 1;
    for i := 0 to ito do
      result := result + t[i] + IIF(i=ito,'',WithThis);
    {result := result + t[i + 1];}
  end
  else result := BaseString;
  finally t.free;
  end;
end;

{@ Extract a string between two other strings}
{@}function StringBetween(SearchStr, FromThis, ToThis: string; CaseIns: Boolean; FromPos: integer): string;
var i: integer;
begin
 i:=FromPos;
 Result := StringBetweenPos(SearchStr,FromThis,ToThis,CaseIns,i);
 {REM 24/2/2007
 if FromPos > 1 then SearchStr := Copy(SearchStr,FromPos,Length(SearchStr));
 i1 := Pos(FromThis,SearchStr); if i1 = 0 then Exit;
 Inc(i1,Length(FromThis));
 i2 := Pos(ToThis,Copy(SearchStr,i1,Length(SearchStr))); if i2 = 0 then Exit;
 i2 := i2 + i1 -1;
 if i2 > i1 then Result := Copy(SearchStr,i1,i2-i1);}
end;

{@ Extract a string between two other strings}
{@}function StringBetweenPos(SearchStr, FromThis, ToThis: string; CaseIns: Boolean; var StrPos: integer): string;
var i1,i2: integer;
begin
 Result := '';
 if StrPos > 1 then SearchStr := Copy(SearchStr,StrPos,Length(SearchStr));
 if CaseIns then i1 := Pos(UpperCase(FromThis),UpperCase(SearchStr))
 else i1 := Pos(FromThis,SearchStr);
 if i1 = 0 then Exit;
 Inc(i1,Length(FromThis));
 if CaseIns then i2 := Pos(UpperCase(ToThis),UpperCase(Copy(SearchStr,i1,Length(SearchStr))))
 else i2 := Pos(ToThis,Copy(SearchStr,i1,Length(SearchStr)));
 if i2 = 0 then Exit;
 i2 := i2 + i1 -1;
 StrPos := StrPos + i2 + Length(ToThis) -1;
 if i2 > i1 then Result := Copy(SearchStr,i1,i2-i1);
end;

{@}function IsNumericString(str: string): Boolean;
var i: integer;
begin
  if Length(str) = 0 then begin Result := False; Exit; end;
  Result := True;
  for i:=1 to Length(str) do
   if not (str[i] in ['0'..'9', SysUtils.DecimalSeparator]) then begin
    Result := False; Break; end;
end;
{@}function IsHEXString(str: string): Boolean;
var i: integer;
begin
  if Length(str) = 0 then begin Result := False; Exit; end;
  Result := True;
  for i:=1 to Length(str) do
   if not (str[i] in ['0'..'9', 'A'..'F', 'a'..'f']) then begin
    Result := False; Break; end;
end;
{@ Load Font properties from string in form (Fontname,size,bold,italic,...)}
{@}procedure FontFmStr(var Font: TFont; sFont: string);
var p: integer;
    s: string;
begin
 p:= 1;
 if (Length(sFont) > p) then
    begin
    Font.Name := ExtractString(sFont, p, ','); { font name }
    Font.Size := 8;
    s := ExtractString(sFont, p, ',');
    if IsNumericString(s) then
     try { font size }
      Font.Size := StrToInt(s);
      except
      Font.Size := 8;
      end;
    //Font.Color := clWindowText;
    s := ExtractString(sFont, p, ',');
    //if IsNumericString(s) then
    try { font color }
     Font.Color := StringtoColor(s);
     except
     Font.Color := clWindowText;
     end;
    s := ExtractString(sFont, p, ',');
    if IsNumericString(s) then
     try
      Font.Charset := StrToInt(s);
      except
      end;
    while p <= Length(sFont) do { font pitch & style }
      begin
      s := ExtractString(sFont, p, ',');
      if Uppercase(s) = 'D' then Font.pitch := fpdefault;
      if Uppercase(s) = 'V' then Font.pitch := fpvariable;
      if Uppercase(s) = 'F' then Font.pitch := fpfixed;
      if Uppercase(s) = 'BOLD' then Font.Style := Font.Style + [fsBold];
      if Uppercase(s) = 'ITALIC' then Font.Style := Font.Style + [fsItalic];
      if Uppercase(s) = 'UNDERLINE' then Font.Style := Font.Style + [fsUnderline];
      if Uppercase(s) = 'STRIKEOUT' then Font.Style := Font.Style + [fsStrikeOut];
      end;
    end; { if }
end;
{@ Save font properties to string delimited by ','}
{@}function FontToStr(const Font: TFont): string;
var v: char;
begin
 Result := Font.Name + ',' + IntToStr(Font.Size) + ',' +
           ColorToString(Font.Color);
 Result := Result + ',' + IntToStr(Ord(Font.CharSet));
 v := 'D';
 Case Font.Pitch of
  fpdefault  : v :='D';
  fpvariable : v :='V';
  fpfixed    : v :='F';
 end;
 Result := Result + ',' + v;
 if fsBold      in Font.Style then Result := Result + ',' + 'BOLD';
 if fsItalic    in Font.Style then Result := Result + ',' + 'ITALIC';
 if fsUnderline in Font.Style then Result := Result + ',' + 'UNDERLINE';
 if fsStrikeOut in Font.Style then Result := Result + ',' + 'STRIKEOUT';
end;

{@ Create a string list with items taken from string array }
{@}function QuickStrings(const Ss: array of string): TStringList;
var i: integer;
begin
 Result := TStringList.Create;
 with Result do
  for i:= Low(Ss) to High(Ss) do
    Add(Ss[i]);
end;

{@ Create a string list with items taken from string array & objects from arry of pointer}
{@}function QuickStrings(const Ss: array of string; const Data: array of Pointer): TStringList;
var i: integer;
begin
 Result := TStringList.Create;
 with Result do
  for i:= Low(Ss) to High(Ss) do
    AddObject(Ss[i],Data[i]);
end;

function ArrayFromStrings(Value: TStrings; ValOrName: integer): TStringArray;
var i: integer;
begin
  SetLength(Result,Value.Count);
  for i:=0 to Value.Count-1 do begin
   case ValOrName of
    1: Result[i] := Value.Values[Value.Names[i]];
    2: Result[i] := Value.Names[i];
    else Result[i] := Value[i];
    end;
   if Result[i] = '' then Result[i] := IntToStr(i);  
   end;
end;

function ArrayFromFields(Value: string): TStringArray;
begin
  Result := ArrayFromStrings(Unstring(Value,[';'],[]));
end;

{@ Return the string value of an array of strings separated each
  string value in resulting string by separator}
{@}function StringFromStrings(const Value: array of string; Separator: AnsiString = ''): string;
var H,i: integer;
begin
 Result := '';
 H := High(Value);
 for i:= Low(Value) to H do
   Result := Result + Value[i]+Separator;
 if (Length(Result) > 0) and (Separator <> '') then
    Delete(Result,Length(Result)-Length(Separator)+1,Length(Separator));
end;

{@ Return the string value of a variant and if it is array separate each
  string value in resulting string by separator}
{@}function StringFromVariant(Value: Variant; Separator: AnsiString; WhenEmpty: AnsiString): string;
var H,L,i: integer;
    sep: string;
begin
 Result := '';
 if VarIsArray(Value)then
    begin
    L := VarArrayLowBound(Value,1);
    H := VarArrayHighBound(Value,1);
    sep := '';
    for i:=L to H do
     begin
     if (VarIsEmpty(Value[i]) or VarIsNull(Value[i])) then
      Result := Result + Sep + WhenEmpty
     else
      try Result := Result + Sep + VarAsType(Value[i],varString);
      except
       on Exception do Result := Result + Sep + '*** Variant Convertion Error! ***'
      end;
     sep := Separator;
     end;
    end
 else
    if not (VarIsEmpty(Value) or VarIsNull(Value)) then
    try Result := VarAsType(Value,varString)
     except
      on Exception do Result := '*** Variant Convertion Error! ***'
     end
    else
     Result := WhenEmpty;
end;

{@}function LeftStr(S : String; ToPos : Integer) : String;
begin
	Result := Copy(S, 1, ToPos);
end;

{@}function RightStr(S : String; ToPos : Integer) : String;
begin
	Result := Copy(S, ToPos, Length(S) - ToPos + 1);
end;

{@}procedure StrSplit(SrcStr : String; BreakDownPos : Integer;
	var S1, S2 : String);
begin
	S1 := LeftStr(SrcStr, BreakDownPos - 1);
	S2 := RightStr(SrcStr, BreakDownPos + 1);
end;

{@ Converts Greek characters in a string to Latin representation }
{@}function Greeklish(s: string;grPfx:string;grSfx:string): string;
var L,i,j: integer;
    hasgr: Boolean;
const
  Greeks = 'ÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþ';
  Latins = 'ABGDEZHUIKLMNJOPRSTYFXCWIYaehiyabgdezhuiklmnxoprsstyfxcwiyoyw';
begin
  Result := s;
  hasgr := False;
  L := Length(Result);
  for i:=1 to L do begin
   j := Pos(Result[i],Greeks);
   if j <> 0 then begin
      Result[i] := Latins[j];
      hasgr := True;
   end;
  end;
  if hasgr then Result := grPfx+Result+grSfx;
end;

{@}function StrInArray(Str: string; const sArray: array of string; CaseIns: Boolean): Boolean;
var i,j: integer;
begin
 Result := False;
 if Length(sArray) = 0 then Exit;
 j := High(sArray);
 for i:=Low(sArray) to j do begin
  if CaseIns then Result := AnsiSameText(Str,sArray[i])
  else Result := AnsiSameStr(Str,sArray[i]);
  if Result then Break;
  end;
end;

{@}function StrPosInArray(Str: string; const sArray: array of string; iLow: integer; CaseIns: Boolean): Integer;
var i,j: integer;
begin
// Open arrays in function have always Low(Array) = 0 !!! ??? so compute by iLow
 Result := -1;
 j := High(sArray);
 for i:=Low(sArray) to j do begin
  if CaseIns then begin
     if AnsiSameText(Str,sArray[i]) then Result := i + iLow
     end
  else begin
     if AnsiSameStr(Str,sArray[i]) then Result := i + iLow
     end;
  if Result <> -1 then Break;
  end;
end;

{@ return a valid identifier string replacing/deletig invalid chars}
{@}function ValidIdentStr(const Ident: string; Replstr: string): String;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := '';
  if (Length(Ident) = 0) then Exit;
  if not (Ident[1] in Alpha) then Result := '_' else Result := Ident[1];
  for I := 2 to Length(Ident) do
    if not (Ident[I] in AlphaNumeric) then
      Result := Result + ReplStr
    else
      Result := Result + Ident[I];
end;

{@ Scan string & format values between <#xxx>...</#> tags xxx is based on format strings}
{@}function CheckAndFormat(Text: string): string;

 procedure CheckFormat(sf: string; out W,P: integer; out T, A: Char);
 var i: integer;
     bP: Boolean;
     sW, sP: string;
 begin
  bP := False;
  for i:=1 to Length(sf) do
   case sf[i] of
    '-': A := sf[i];
    '0'..'9': if bP then sP:=sP+sf[i] else sW:=sW+sf[i];
    '.': bP:=True;
    else
     T := sf[i];
   end;
   if sW<>'' then W := StrToInt(sW) else W := -1;
   if sP<>'' then P := StrToInt(sP) else P := -1;
 end;

const
    tgS = '<#';
    tgC = '>';
    tgE = '</#>';
var iStr, i1, i2, W, P: integer;
    sf, ss, s1, s2: string;
    T,A: Char;
begin
 ss := Text;

 iStr := AnsiPos(tgS,ss);
 while iStr<>0 do
  begin
  if (iStr<>0) then
    begin
    sf := StringBetween(ss,tgS,tgC); //get formatdef
    s1 := tgS+sf+tgC;
    i1 := iStr+Length(s1); //start of text to format
    i2 := AnsiPos(tgE,ss);
    if (i2 <> 0) then
       begin
       s2 := StringBetween(ss,s1,tgE);
       CheckFormat(sf,W,P,T,A);
       try
        case T of
        'd','u','D','U': s2 := Format('%'+sf,[Round(StrToFloat(s2))]);
        'e','f','g','n','m','E','F','G','N','M': s2 := Format('%'+sf,[StrToFloat(s2)]);
        else
         s2 := Format('%'+sf,[s2]);
        end;
       except
        ;
       end;
       if (W<>-1) and (Length(s2)>W)then
        begin
        if A = '-' then
           s2 := Copy(s2,1,W)
        else
           s2 := Copy(s2,Length(s2)-W+1,W);
        end;
       //s2 := Format('%'+sf,[s2]); //"%" [index ":"] ["-"] [width] ["." prec] type
       Result := Result+ Copy(ss,1,iStr-1)+s2;
       Delete(ss,1,i2+Length(tgE)-1);
       end
    else
       begin
       Result := Result+ Copy(ss,1,i1-1);
       Delete(ss,1,i1-1);
       end;
    end;
  iStr := AnsiPos(tgS,ss);
  end;
  Result := Result + ss;
end;

function ElfHash(const Value: string): Integer;
var
  i: integer;
  x: Integer;
begin
  Result := 0;
  for i := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := Result xor (x shr 24);
    Result := Result and (not x);
  end;
end;
function GetHashCode(Str: PChar): Integer;
var
  Off, Len, Skip, I: Integer;
begin
  Result := 0;
  Off := 1;
  Len := StrLen(Str);
  if Len < 16 then
    for I := (Len - 1) downto 0 do
    begin
      Result := (Result * 37) + Ord(Str[Off]);
      Inc(Off);
    end
  else
  begin
    { Only sample some characters }
    Skip := Len div 8;
    I := Len - 1;
    while I >= 0 do
    begin
      Result := (Result * 39) + Ord(Str[Off]);
      Dec(I, Skip);
      Inc(Off, Skip);
    end;
  end;
end;


{==============================================================================}
{@ DATE FUNCTIONS                                                              }
{==============================================================================}

{@}function IncreaseMonth(Value: TDateTime; Incr: Integer): TDateTime;
begin
  Result := IncMonth(Value,Incr);
end;

{@}function  EndOfMonth(D: TDateTime): TDateTime;
var
  Year,Month,Day : Word;
begin
  DecodeDate(D,Year,Month,Day);
  if Month=12 then
  begin
    Inc(Year);
    Month:=1;
  end else
    Inc(Month);
  Result:=EncodeDate(Year,Month,1)-1;
end;

{@  DecodeDate(Date,y,m,d);
    NewDate:=DateSer(y-4,m+254,d+1234);}
{@}function DateSer(y,m,d: Integer): TDateTime;
const
   mj: array[1..12] of Integer=(31,28,31,30,31,30,31,31,30,31,30,31);
var
 add: Integer;
begin
 while(true) do begin
  y:=y+(m-1) div 12;
  m:= (m-1) mod 12 +1;
  if m<=0 then begin
     Inc(m,12);
	 Dec(y);
  end;
  if  ((y mod 4 = 0) and
      ((y mod 100<>0) or (y mod 400=0)))
	  and (m=2) then
     add:=1 //add one day in February
  else
    add:=0;
  if (d>0) and (d<=(mj[m]+add)) then break;
  if d>0 then begin Dec(d,mj[m]+add); Inc(m); end
  else begin Inc(d,mj[m]+add); Dec(m); end;
 end;
 Result:=EncodeDate(y,m,d);
end;
{==============================================================================}
{@ SYSTEM FUNCTIONS                                                            }
{==============================================================================}

{@ Click and move components at runtime.
  This code goes on the OnMouseDown event of the component}
{@}procedure DragMoveControl(Ctl: TControl);
const
  SC_DragMove = $F012;   {a magic number}
begin
  ReleaseCapture;
  Ctl.perform(WM_SysCommand, SC_DragMove, 0);
end;

{@ Get Window name at pos(x,Y)}
{@}function GetParentWindowNameAt(
  X, Y : integer ) : string;
var
  P : TPoint;
  W : TWinControl;
begin
  P.X := X;
  P.Y := Y;
  W   := FindVCLWindow( P );
  if( nil <> W )then
  begin
    Result := W.Name;
  end else
  begin
    Result := '';
  end;
end;

{@ Open document/object by it's association (Explorer dblclick like)}
{@}procedure OpenObject( sObjectPath : string );
begin
  ShellExecute( 0, Nil, PChar( sObjectPath ), Nil, Nil, SW_NORMAL );
end;

{@ Print document/object by it's association (Explorer dblclick like)}
{@}procedure PrintObject( sObjectPath : string );
begin
  ShellExecute( 0, PChar( 'print' ), PChar( sObjectPath ), Nil, Nil, SW_NORMAL );
end;

{@ Open a web page }
{@}procedure ExploreWeb(handle:HWND ; page:PChar);
var
  Returnvalue   : integer;
begin
  ReturnValue := ShellExecute(handle, 'open', page, nil, nil, SW_SHOWNORMAL);
  if ReturnValue <= 32 then
    case Returnvalue of
      0                   : MessageBox(handle,'Error: Out of Memory','Error ExploreWeb',0);
      ERROR_FILE_NOT_FOUND: MessageBox(handle,'Error: File not found','Error ExploreWeb',0);
      ERROR_PATH_NOT_FOUND: MessageBox(handle,'Error: Directory not','Error ExploreWeb',0);
      ERROR_BAD_FORMAT    : MessageBox(handle,'Fehler: Wrong format in EXE','Error ExploreWeb',0);
      // All other errors . See help for more ReturnValues of ShellExecute
    else                    MessageBox(handle,PChar('Error Nr: '+IntToStr(Returnvalue)+' in ShellExecute'),'Error ExploreWeb',0)
    end //case
end;

{@ Delete file and send to RecycleBin}
{@}function DeleteFileWithUndo(sFileName: string) : boolean;
var
  fos : TSHFileOpStruct;
  err: integer;
  fName: String;
begin
  FillChar( fos, SizeOf( fos ), 0 );
  fName := sFileName+Chr(0)+Chr(0);
  //Move(sFileName,fName,SizeOf(sFileName));
  with fos do
  begin
    wFunc  := FO_DELETE;
    pFrom  := PChar(fName);
    fFlags := FOF_ALLOWUNDO
              or FOF_NOCONFIRMATION
              or FOF_SILENT;
  end;
  err := ShFileOperation( fos );
  Result := ( 0 = err );
end;

{@ Register program to run on windows startup }
{@}procedure RunOnStartup(sProgTitle, sCmdLine: string; bRunOnce: boolean);
var
  sKey : string;
  reg  : TRegIniFile;
begin
  if( bRunOnce )then
    sKey := 'Once'
  else
    sKey := '';

  reg := TRegIniFile.Create( '' );
  try
  reg.RootKey := HKEY_LOCAL_MACHINE;
  reg.WriteString(
    'Software\Microsoft'
    + '\Windows\CurrentVersion\Run'
    + sKey + #0,
    sProgTitle,
    sCmdLine );
  finally reg.Free;
  end;
end;

{@ Check if a module is running (EXE, DLL and so on) }
{@}function IsModuleRunning(ModuleName: string): Boolean;
{$IFDEF VER80}
var
   S: array [0..127] of Char;
{$ENDIF}
begin
{$IFDEF VER80}
   StrPCopy(S, ModuleName);
   IsModuleRunning := GetModuleHandle(S) <> 0;
{$ELSE}
   IsModuleRunning := GetModuleHandle(PChar(ModuleName)) <> 0;
{$ENDIF}
end;

{@ Delete the requested message from the queue, but throw back
   any WM_QUIT msgs that PeekMessage may also return}
{@}procedure KillMessage(Wnd: HWnd; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

{@ this function checks if VFunctionname exists in VLibraryname}
{@}function FuncAvail(VLibraryname, VFunctionname: string; var VPointer: pointer): boolean;
var
Vlib: tHandle;
begin
Result := false;
if LoadLibrary(PChar(VLibraryname)) = 0 then
    exit;
Vlib := GetModuleHandle(PChar(VLibraryname));
if Vlib <> 0 then
begin
   VPointer := GetProcAddress(Vlib, PChar(VFunctionname));
   if VPointer <> NIL then
      Result := true;
end;
end;

{@ checks if computer is conected to internet}
{@}function IsInternetConnected: Boolean;
var
InetIsOffline : function(dwFlags: DWORD):BOOL; stdcall;
begin
Result := False;
// Call shell32.dll for highter Win98
//       else call url.dll
if FuncAvail('url.dll', 'InetIsOffline', @InetIsOffline) then
   Result := not InetIsOffLine(0)
else
if FuncAvail('shell32.dll', 'InetIsOffline', @InetIsOffline) then
   Result := not InetIsOffLine(0);
end;


{==============================================================================}
{@ GRAPHICS FUNCTIONS                                                          }
{==============================================================================}

{@ Gradient fill procedure}
{@}procedure GradientFill(DC: HDC; FBeginColor, FEndColor: TColor; R: TRect; VertFill: Boolean);
var
  { Set up working variables }
  BeginRGBValue  : array[0..2] of Byte;    { Begin RGB values }
  RGBDifference  : array[0..2] of integer; { Difference between begin and end }
                                           { RGB values                       }
  ColorBand : TRect;    { Color band rectangular coordinates }
  I         : Integer;  { Color band index }
  Red       : Byte;     { Color band Red value }
  Green     : Byte;     { Color band Green value }
  Blue      : Byte;     { Color band Blue value }
  Brush, OldBrush     : HBrush;
  FNumColors : integer;
begin
  FNumColors := 256;
  { Extract the begin RGB values }
  { Set the Red, Green and Blue colors }
  BeginRGBValue[0] := GetRValue (ColorToRGB (FBeginColor));
  BeginRGBValue[1] := GetGValue (ColorToRGB (FBeginColor));
  BeginRGBValue[2] := GetBValue (ColorToRGB (FBeginColor));
  { Calculate the difference between begin and end RGB values }
  RGBDifference[0] := GetRValue (ColorToRGB (FEndColor)) - BeginRGBValue[0];
  RGBDifference[1] := GetGValue (ColorToRGB (FEndColor)) - BeginRGBValue[1];
  RGBDifference[2] := GetBValue (ColorToRGB (FEndColor)) - BeginRGBValue[2];

  { Calculate the color band's top and bottom coordinates }
  { for Left To Right fills }
  if not VertFill then
    begin
    ColorBand.Top := R.Top;
    ColorBand.Bottom := R.Bottom;
    if (ColorBand.Bottom - ColorBand.Top) < FNumColors then
       FNumColors := (ColorBand.Bottom - ColorBand.Top);
    end
  else
    begin
    ColorBand.Left := R.Left;
    ColorBand.Right := R.Right;
    if (ColorBand.Right - ColorBand.Left) < FNumColors then
       FNumColors := (ColorBand.Right - ColorBand.Left);
    end;

  { Perform the fill }
  for I := 0 to FNumColors-1 do
  begin  { iterate through the color bands }
    { Calculate the color band's left and right coordinates }
    if not VertFill then
       begin
       ColorBand.Left  := R.Left+ MulDiv (I    , R.Right-R.Left, FNumColors);
       ColorBand.Right := R.Left+ MulDiv (I + 1, R.Right-R.Left, FNumColors);
       end
    else
       begin
       ColorBand.Top   := R.Top + MulDiv (I    , R.Bottom-R.Top, FNumColors);
       ColorBand.Bottom:= R.Top + MulDiv (I + 1, R.Bottom-R.Top, FNumColors);
       end;
    { Calculate the color band's color }
    if FNumColors > 1 then
    begin
      Red   := BeginRGBValue[0] + MulDiv (I, RGBDifference[0], FNumColors - 1);
      Green := BeginRGBValue[1] + MulDiv (I, RGBDifference[1], FNumColors - 1);
      Blue  := BeginRGBValue[2] + MulDiv (I, RGBDifference[2], FNumColors - 1);
    end
    else
    { Set to the Begin Color if set to only one color }
    begin
      Red   := BeginRGBValue[0];
      Green := BeginRGBValue[1];
      Blue  := BeginRGBValue[2];
    end;

    { Create a brush with the appropriate color for this band }
    Brush := CreateSolidBrush(RGB(Red,Green,Blue));
    { Select that brush into the temporary DC. }
    OldBrush := SelectObject(DC, Brush);
    try
      { Fill the rectangle using the selected brush -- PatBlt is faster than FillRect }
      PatBlt(DC, ColorBand.Left, ColorBand.Top, ColorBand.Right-ColorBand.Left, ColorBand.Bottom-ColorBand.Top, PATCOPY);
    finally
      { Clean up the brush }
      SelectObject(DC, OldBrush);
      DeleteObject(Brush);
    end;
  end;  { iterate through the color bands }
end;  { GradientFill }

{@ WriteText original copy from DBGrids.pas}
procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment);
const
  AlignFlags : array [TAlignment] of Integer =
    ( DT_LEFT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_WORDBREAK or DT_EXPANDTABS or DT_NOPREFIX );
var
  B, R: TRect;
  I, Left: Integer;
  DrawBitmap: Graphics.TBitmap;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if GetNearestColor(ACanvas.Handle, I) = I then
  begin                       { Use ExtTextOut for solid colors}
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter}
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;
    ExtTextOut(ACanvas.Handle, Left, ARect.Top + DY, ETO_OPAQUE or
      ETO_CLIPPED, @ARect, PChar(Text), Length(Text), nil);
  end
  else begin                  { Use FillRect and Drawtext for dithered colors}
    DrawBitmap := Graphics.TBitmap.Create;
    try
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and}
      begin                     { brush origin tics in painting / scrolling.}
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
        DrawText(Handle, PChar(Text), Length(Text), R, AlignFlags[Alignment]);
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Free;
    end;
  end;
end;

{@ Frame3D original copy from ExtCtrls.pas}
{@}procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

{@ ???}
{@}function DisplayWidthToPixels(Canvas: TCanvas; DisplayWidth: integer; Font: TFont): integer;
var RestoreCanvas: Boolean;
    TM: TTextMetric;
begin
  raise exception.Create('I MUST REVIEW FOR CANVAS FONT SAVE/RESTORE');
  RestoreCanvas := False;
  if Canvas.Handle = 0 then
     begin
     Canvas.Handle := GetDC(0);
     RestoreCanvas := True;
     end;
  try
   if Assigned(Font) then Canvas.Font := Font;
   GetTextMetrics(Canvas.Handle, TM);
   Result := DisplayWidth * (Canvas.TextWidth('0') - TM.tmOverhang)
          + TM.tmOverhang + 4;
  finally
   if RestoreCanvas then
      begin
      ReleaseDC(0,Canvas.Handle);
      Canvas.Handle := 0;
      end;
  end;
end;

{@ Create RectWH from Sizes -Left/Top/Width/Height}
{@}function RectWH(ALeft,ATop,AWidth,AHeight: integer): TRectWH;
begin
  Result.Left   := ALeft;
  Result.Top    := ATop;
  Result.Width  := AWidth;
  Result.Height := AHeight;
end;

{@ Create RectWH centered in an other from their sizes}
{@}function CenterInWH(InWidth, InHeight, AWidth, AHeight: integer): TRectWH;
var W, H, WR, HR: integer;
begin
W := AWidth;
H := AHeight;
if (W > InWidth) or (H > InHeight) then
   begin
   WR := MulDiv(100,InWidth,W);
   HR := MulDiv(100,InHeight,H);
   if WR < HR then
      begin
      W := MulDiv(W,WR,100);
      H := MulDiv(H,WR,100);
      end
   else
      begin
      W := MulDiv(W,HR,100);
      H := MulDiv(H,HR,100);
      end;
   end;
Result := RectWH(((InWidth-W) div 2), ((InHeight-H) div 2), W, H);
end;

{==============================================================================}
{@ DIALOGS  FUNCTIONS                                                          }
{==============================================================================}

{@}function ConfirmYesNo(const Msg: string): Word;
begin
 Result := MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0);
end;

{@}function ConfirmYesNoCancel(const Msg: string): Word;
begin
 Result := MessageDlg(Msg, mtConfirmation, mbYesNoCancel, 0);
end;

{function ListSelect(SelList: array of string; ACaption: string; Selection: integer): integer;
var i: integer;
    frm: TForm;
    rd: TRadioGroup;
begin
  i := Length(SelList);
  if i = 0 then Exit;
  Result := -1;
  Frm := TForm.Create(Application);
  with Frm do
   try Caption := '';
    //Color := clBtnShadow;
    //Font.Color := clYellow;
    BorderStyle := bsToolWindow;
    Position := poMainFormCenter;
    rd := TRadioGroup.Create(Frm);
    with rd do
     begin
     Parent := Frm;
     Caption := ACaption;
     Items.Assign(QuickStrings(SelList));
     Align := alTop;
     Height := i*20+15;
     rd.ItemIndex := Selection;
     end;
    Height := i*20+15+50;
    Width := 300;
    with TButton.Create(Frm) do
     begin
     Parent := Frm;
     Default := True;
     ModalResult := mrOK;
     Caption := 'OK';
     Left := 2;
     Top := i*20+18;
     end;
    with TButton.Create(Frm) do
     begin
     Parent := Frm;
     Cancel := True;
     ModalResult := mrCancel;
     Caption := 'Cancel';
     //Font.Color := clWindowText;
     Left := 80;
     Top := i*20+18;
     end;
    if ShowModal = mrOK then
      Result := rd.ItemIndex;
   finally Free;
   end;
end;}

{function MultiSelect(SelList: array of string; ACaption: string; var Selection: string): Boolean;
var i,p: integer;
    frm: TForm;
    rd: TCheckListBox;
    si: string;
begin
  Result := False;
  i := Length(SelList);
  if i = 0 then Exit;
  Frm := TForm.Create(Application);
  with Frm do
   try Caption := '';
    //Color := clBtnShadow;
    //Font.Color := clYellow;
    BorderStyle := bsToolWindow;
    Position := poMainFormCenter;
    rd := TCheckListBox.Create(Frm);
    with rd do
     begin
     Parent := Frm;
     Caption := ACaption;
     Flat := True;
     Items.Assign(QuickStrings(SelList));
     Align := alTop;
     Height := i*20+15;
     rd.ItemIndex := 0;
     end;
    Height := i*20+15+50;
    Width := 300;
    with TButton.Create(Frm) do
     begin
     Parent := Frm;
     Default := True;
     ModalResult := mrOK;
     Caption := 'OK';
     Left := 2;
     Top := i*20+18;
     end;
    with TButton.Create(Frm) do
     begin
     Parent := Frm;
     Cancel := True;
     ModalResult := mrCancel;
     Caption := 'Cancel';
     //Font.Color := clWindowText;
     Left := 80;
     Top := i*20+18;
     end;
    p:=1;
    while p<=Length(Selection) do begin
      si := ExtractString(Selection,p,',');
      try i := StrToInt(si) except on Exception do i:=-1 ; end;
      if (i>=0) and (i <= rd.Items.Count-1) then
         rd.Checked[i] := True;
      end;
    if ShowModal = mrOK then
      begin
      Result := True;
      Selection := '';
      for i:=0 to rd.Items.Count-1 do
       if rd.Checked[i] then
          Selection := Selection + IIF(Selection='','',',') + IntToStr(i);
      end;
   finally Free;
   end;
end;}

{@}procedure raiseWarn(const ErrMsg: string);
begin
  MessageDlg(ErrMsg,mtWarning,[mbOK],0); {WARNING!!! DO NOT USE IN REMOTEDATAMODULE}
  SysUtils.Abort;
end;

{@}procedure raiseError(const ErrMsg: string; ErrClass: ExceptClass);
begin
  if Assigned(ErrClass) then
     raise ErrClass.Create(ErrMsg)
  else
     begin
     MessageDlg(ErrMsg,mtError,[mbOK],0); {WARNING!!! DO NOT USE IN REMOTEDATAMODULE}
     SysUtils.Abort;
     end;
end;

{@}procedure raiseErrorFmt(const ErrMsg: string; Params: array of const; ErrClass: ExceptClass);
begin
  if Assigned(ErrClass) then
     raise ErrClass.CreateFmt(ErrMsg,Params)
  else
     begin
     MessageDlg(ErrMsg,mtError,[mbOK],0); {WARNING!!! DO NOT USE IN REMOTEDATAMODULE}
     SysUtils.Abort;
     end;
end;

{==============================================================================}
{@ VARIOUS  FUNCTIONS                                                          }
{==============================================================================}

{@}function RoundFloat(Amount: Extended; Decs: Byte): Extended;
var s: string;
begin
  s := FloatToStrF(Amount,ffFixed,18,Decs);
  Result := StrToFloat(s);
end;

{@}function IIF(expr: Boolean; caseTrue, caseFalse: variant): variant;
begin
  if expr then Result := caseTrue
  else Result := caseFalse;
end;

{@}procedure SwapVar(var sw1, sw2: variant);
var msw: variant;
begin
  msw := sw1; sw1 := sw2; sw2 := msw;
end;

{@}function Max(X, Y: Integer): Integer;
begin
  Result := Y;
  if X > Y then Result := X;
end;

{@}function AssignVar(Value, DefaultValue: variant): variant;
begin
 if varIsNull(Value) or varIsEmpty(Value) then Result := DefaultValue
 else Result := Value;
end;

{@}function VarEquals(const V1, V2: Variant; CheckAsString: Boolean): Boolean;
var k,i1,i2,j1,j2: integer;
begin
  Result := False;
  if VarIsArray(V1) or VarIsArray(V2) then
     begin
     if not (VarIsArray(V1) and VarIsArray(V2)) then Exit;
     i1:=VarArrayLowBound(V1,1); j1:=VarArrayHighBound(V1,1);
     i2:=VarArrayLowBound(V2,1); j2:=VarArrayHighBound(V2,1);
     if (j1-i1) <> (j2-i2) then Exit;
     k := j1-i1;
     while k >=0 do begin
      if not VarEquals(V1[i1+k], V2[i2+k]) then Exit;
      Dec(k);
      end;
     Result := True;
     end
  else
  if VarIsNull(v1) then Result := VarIsNull(v2)
  else
  if VarIsNull(v2) then Result := VarIsNull(v1)
  else
  if VarIsEmpty(v1) then Result := VarIsEmpty(v2)
  else
  if VarIsEmpty(v2) then Result := VarIsEmpty(v1)
  else
  try
    if CheckAsString then
       Result := ( VarToStr(V1) = VarToStr(V2) )
    else
    Result := ( V1 = V2 );
  except
  end;
end;

{@}function EncryptStream(Stream: TStream; decrypt: boolean; c: integer): boolean;
var i: integer;
    stm: TMemoryStream;
    p: ^byte;
    b: byte;
begin
  stm := TMemoryStream.Create;
  try
   stm.LoadFromStream(Stream);
   p := stm.Memory;
   for i:=1 to stm.Size do
    begin
    b := p^;
    if decrypt then
       begin
       b := p^;
       b := Byte(b - i - c);
       b := Byte((b shl 4) or (b shr 4));
       end
    else
       begin
       b := Byte((b shl 4) or (b shr 4));
       b := Byte(b + i + c);
       end;
    p^ := b;
    inc(p);
    end;
   stm.Seek(0,soFromBeginning);
   Stream.Seek(0,soFromBeginning);
   stm.SaveToStream(Stream);
   Stream.Seek(0,soFromBeginning);
   Result := True;
  finally stm.Free;
  end;
end;
{@}function EncryptString(S: string; decrypt: boolean; c: integer): string;
var ss: TStringStream;
begin
  ss := TStringStream.Create(s);
  try
   if EncryptStream(ss,decrypt,c) then
      Result := ss.DataString;
   finally ss.Free;
   end;
end;

function AFMCheck(AFM: string): Boolean;
var tot, i, k: integer;
begin
  Result := False;
  if Length(AFM) <> 9 then Exit;
  if not IsNumericString(AFM) then Exit;
  AFM := ReverseString(AFM);
  tot:=0;k:=2;
  for i:=2 to 9 do begin
   tot := tot + StrToInt(AFM[i]) * k;
   k := k*2;
   end;
  tot := tot mod 11;
  if (StrToInt(AFM[1]) = tot) or ((tot = 10) and (StrToInt(AFM[1]) = 0))
     then Result := True;
end;

{==============================================================================}
{@ COMPONENT FUNCTIONS                                                         }
{==============================================================================}

{ TPropInfoList }

constructor TPropInfoList.Create(AObject: TObject; Filter: TTypeKinds);
begin
  if AObject <> nil then begin
    FCount := GetPropList(AObject.ClassInfo, Filter, nil);
    FSize := FCount * SizeOf(Pointer);
    GetMem(FList, FSize);
    GetPropList(AObject.ClassInfo, Filter, FList);
  end
  else begin
    FCount := 0;
    FList := nil;
  end;
end;

destructor TPropInfoList.Destroy;
begin
  if FList <> nil then FreeMem(FList, FSize);
end;

function TPropInfoList.Contains(P: PPropInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if (PropType = P^.PropType) and (CompareText(Name, P^.Name) = 0) then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

function TPropInfoList.Find(const AName: string): PPropInfo;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    with FList^[I]^ do
      if (CompareText(Name, AName) = 0) then
      begin
        Result := FList^[I];
        Exit;
      end;
  Result := nil;
end;

procedure TPropInfoList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then Move(FList^[Index + 1], FList^[Index],
    (FCount - Index) * SizeOf(Pointer));
end;

function TPropInfoList.Get(Index: Integer): PPropInfo;
begin
  Result := FList^[Index];
end;

procedure TPropInfoList.Intersect(List: TPropInfoList);
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if not List.Contains(FList^[I]) then Delete(I);
end;


function SetComponentProperty(c: TComponent; PropName: string; Value: variant): Boolean;
var PropInfo: PPropInfo;
begin
  Result := False;
  PropInfo := GetPropInfo(c, PropName);
  if PropInfo <> nil then
     try
      TypInfo.SetPropValue(c,PropName,Value);
      Result := True;
     except
      ;
     end;
end;


{ TODO : cloning a component with the same name results to exception due to same Name property }

{@ "clones" the properties of C1 and writes them to C2.
    C1 and C2 must be of the same type.  Use it for components that do not have an Assign method. }
{@}procedure CloneComponent(C1: TComponent; C2: TComponent);
var S: TMemoryStream;
begin
 if C1.ClassType <> C2.ClassType then
    raise EComponentError.Create('CLONECOMPONENT:Object types are incompatible'+sLineBreak+
          C1.Name+' / '+C2.Name);
 if C1 is TWinControl then
    TWinControl(C2).Parent := TWinControl(C1).Parent;
 S := TMemoryStream.Create;   { create a memory stream }
 try
 with S do begin
  WriteComponent(C1);        { write C1's properties to stream }
  Seek(0, 0);                { get back to the beginning of the stream }
  ReadComponent(C2);         { read properties from stream into C2 }
  end;
 finally S.Free;
 end;
end;

{@ "replicates" component C and returns a new component
   whose type and properties match those of C. }
{@}function ReplicateComponent(C: TComponent; aOwner: TComponent): TComponent;
begin
 Result := TComponentClass(C.ClassType).Create(aOwner);  { create component }
 CloneComponent(C, Result); { clone it }
end;

{@ Creates a new component based on parameter's class and copies
   it's properties based on a give list of names}
{@}function CopyComponent(C: TComponent; IncList: array of string): TComponent;

 function FoundInList(PName: string; Kind: TTypeKind): Boolean;
 var j,k: integer;
 begin
   Result := False;
   k:=High(IncList);
   for j:=0 to k do
    begin
    if AnsiSameText(PName,IncList[j]) then begin Result := True; Break; end;
    if Uppercase(IncList[j]) = CPY_ALL then begin Result := True; Break; end;
    case Kind of
      tkUnknown: ;
      tkClass:      Result := (Uppercase(IncList[j]) = CPY_CLASS);
      tkMethod:     Result := (Uppercase(IncList[j]) = CPY_METHOD);
      tkInterface:  Result := (Uppercase(IncList[j]) = CPY_INTERFACE);
      tkRecord:     Result := (Uppercase(IncList[j]) = CPY_RECORD);
      tkArray:      Result := (Uppercase(IncList[j]) = CPY_ARRAY);
      tkDynArray:   Result := (Uppercase(IncList[j]) = CPY_DYNARRAY);
      else
        Result := (Uppercase(IncList[j]) = CPY_PROPERTY);
      end;
    if Result then Break;
    end;
 end;

var ps: TPropInfoList;
    i: integer;
begin
 Result := TComponentClass(C.ClassType).Create(C.Owner);  { create component }
 if C is TWinControl then
    TWinControl(Result).Parent := TWinControl(C).Parent;
 if High(IncList) < 0 then Exit;
 ps := TPropInfoList.Create(C,tkAny);
 try
  for i:=0 to ps.Count-1 do
   begin
   if FoundInList(ps[i].Name, ps[i].propType^.Kind) then
   case ps[i].propType^.Kind of
    tkUnknown: ;
{ TODO : implement special typeKind copy }
    tkClass: ;
    tkMethod: ;
    tkInterface: ;
    tkRecord: ;
    tkArray: ;
    tkDynArray: ;
    else
     SetPropValue(Result, ps[i].Name, GetPropValue(C, ps[i].Name) );
    end;
   end;
  finally
   ps.Free;
  end;
end;

{@ Store component to text file like DFM }
{@}procedure ComponentToFile(Component: TComponent; FileName: string);
var
  BinStream:TMemoryStream;
  FStream: TFileStream;
begin
  BinStream := TMemoryStream.Create;
  try
    FStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, FStream);
    finally
      FStream.Free;
    end;
  finally
    BinStream.Free;
  end;
end;

{@ Load component from text file like DFM }
{@}function FileToComponent(FileName: string; Component: TComponent): TComponent;
var
  FStream: TFileStream;
  BinStream: TMemoryStream;
begin
  FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone	);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(FStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result := BinStream.ReadComponent(Component);
    finally
      BinStream.Free;
    end;
  finally
    FStream.Free;
  end;
end;

{@ Store component to string like DFM }
{@}function ComponentToString(Component: TComponent): string;
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

{@ Load component from string like DFM }
{@}function StringToComponent(Value: string; Component: TComponent): TComponent;
var
  StrStream:TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result := BinStream.ReadComponent(Component);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

{@ Load a disk file into a string}
{@}function LoadFileToString(FileName: string): string;
var sF: TFileStream;
    sS: TStringStream;
begin
 if not FileExists(FileName) then
    raise exception.Create(FileName+':File not exists!');
 sF := TFileStream.Create(FileName,fmOpenRead);
 try
  sS := TStringStream.Create('');
  try sS.CopyFrom(sF,0);
    Result := sS.DataString;
   finally sS.Free;
   end;
  finally sF.Free;
  end;
end;

{@ Save string to a disk file}
{@}function SaveStringToFile(s: string; FileName: string): Boolean;
var sF: TFileStream;
    sS: TStringStream;
begin
 sS := TStringStream.Create(s);
 try
  sF := TFileStream.Create(FileName,fmCreate, fmShareDenyWrite);
  try sF.CopyFrom(sS,0);
    Result := True;
   finally sF.Free;
   end;
  finally sS.Free;
  end;
end;


end.

{Const
a2e : array [0..255] of byte =
    (000,001,002,003,055,045,046,047,022,005,037,011,012,013,014,159,
     016,017,018,019,182,181,050,038,024,025,063,039,028,029,030,031,
     064,090,127,123,091,108,080,125,077,093,092,078,107,096,075,097,
     240,241,242,243,244,245,246,247,248,249,122,094,076,126,110,111,
     124,193,194,195,196,197,198,199,200,201,209,210,211,212,213,214,
     215,216,217,226,227,228,229,230,231,232,233,173,224,189,095,109,
     121,129,130,131,132,133,134,135,136,137,145,146,147,148,149,150,
     151,152,153,162,163,164,165,166,167,168,169,192,106,208,161,007,
     104,220,081,066,067,068,071,072,082,083,084,087,086,088,099,103,
     113,156,158,203,204,205,219,221,224,236,252,176,177,178,062,180,
     069,085,206,222,073,105,154,155,171,015,186,184,183,170,138,139,
     060,061,098,079,100,101,102,032,033,034,112,035,114,115,116,190,
     118,119,120,128,036,021,140,141,142,065,006,023,040,041,157,042,
     043,044,009,010,172,074,174,175,027,048,049,250,026,051,052,053,
     054,089,008,056,188,057,160,191,202,058,254,059,004,207,218,020,
     225,143,070,117,253,235,238,237,144,239,179,251,185,234,187,255);

     ASCII TO EBCDIC CONVERTER
  Procedure StringA2E(var StringToConvert:String);
  Var     Loop:Integer;
  begin
          For Loop := 1 to length(StringToConvert) do
                  StringToConvert[Loop] := a2e[ord(StringToConvert[Loop])];
  end;

This will convert any string... If you want this same routine can be modified
to also convert any data...If however you want to do a lot more
data conversion/manipluation, then it is probably worth buying a product to do this properly...
}




