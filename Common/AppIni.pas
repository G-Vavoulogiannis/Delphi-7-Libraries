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
 Description.....: Functions to manage application settings in INI file
 Developer/Author: George J. Vavoulogiannis, georgev@hol.gr
                   http://g-vavoulogiannis.blogspot.com

===============================================================================}


unit AppIni;

interface

uses WinTypes, WinProcs, Classes, Graphics, IniFiles, SysUtils, Forms, Quickrpt, Controls;

{ TODO 1 : TXMLINI class to use xml file instead of ini file }

{@ AppIni unit based on OptionsDialog functionality }
{@}function AppIniFile: TIniFile;
{@}function AppValue(Key: string; Default: string=''; IniF: string=''; IniS: string=''): string;
{@}procedure WriteAppValue(Key: string; Value: string; IniF: string=''; IniS: string='');
{@}procedure LoadAppSection(Section: string; IniF: string; InList: TStrings);
{@}procedure LoadAppValues(InList: TStrings);
{@}procedure SetFormFonts(Form: TForm; C: TComponent=nil; Fonts: string='Fonts');
{@}procedure SetPrintFonts(Form: TForm; QRep: TQuickRep; Fonts: string='Fonts');
function BaseFont: TFont;
function PrintFont: TFont;
function Font1: TFont;
function Font2: TFont;
function Font3: TFont;
function Font4: TFont;
function Font5: TFont;
function Font6: TFont;
function PrintFontX(X: integer): TFont;
function BaseColor: TColor;
function ColorX(X: Integer): TColor;

var
   FontSet: string;
   APPLICATION_INI: string = ''; //set it in initialization section to change default

const
   DEFFONTSET = 'Fonts';

implementation

uses TypInfo, Utilfun, StrUtils,
     //DBGrids, DBVGrids4, TabNotBk, Tabs, dxDBGrid,Controls,
     QrCtrls, QrAngLbl;

function AppIniFile: TIniFile;
var sf: string;
begin
  if (APPLICATION_INI<>'') then
     begin
     sf := APPLICATION_INI;
     if AnsiStartsText('<APPPATH>\',APPLICATION_INI) then
        sf := StringReplace(APPLICATION_INI,'<APPPATH>\',ExtractFilePath(Application.ExeName),[rfIgnoreCase]);
     Result := TIniFile.Create(sf);
     end
  else
     Result := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
end;

function AppValue(Key: string; Default: string; IniF: string; IniS: string): string;
var ini: TIniFile;
begin
 if IniF <> '' then
    ini := TIniFile.Create(IniF)
 else
    ini := AppIniFile;
 try
 Result := Ini.ReadString(IIF(IniS<>'',IniS,'Values'),Key,Default);
 finally ini.free;
 end;
end;

{@}procedure WriteAppValue(Key: string; Value: string; IniF: string; IniS: string);
var ini: TIniFile;
begin
 if IniF <> '' then
    ini := TIniFile.Create(IniF)
 else
 ini := AppIniFile;
 try
 Ini.WriteString(IIF(IniS<>'',IniS,'Values'),Key,Value);
 finally ini.free;
 end;
end;

procedure LoadAppSection(Section: string; IniF: string; InList: TStrings);
var ini: TIniFile;
begin
 { TODO -cReviewCode : try except block, if inif not exists }
 if IniF <> '' then
    ini := TIniFile.Create(IniF)
 else
 ini := AppIniFile;
 try
  ini.ReadSectionValues(Section,InList);
 finally ini.free;
 end;
end;

procedure LoadAppValues(InList: TStrings);
var ini: TIniFile;
begin
 ini := AppIniFile;
 try
  ini.ReadSectionValues('Values',InList);
 finally ini.free;
 end;
end;

function BaseFont: TFont;
var s: string;
   ini: TIniFile;
begin
 Result := TFont.Create;
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'BaseFont','');
 if s <> '' then
    FontFmStr(Result,s)
 else
    Result.Name := 'Arial Greek';
 finally ini.free;
 end;
end;
function PrintFont: TFont;
var s: string;
   ini: TIniFile;
begin
 Result := TFont.Create;
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'PrintFont','');
 if s <> '' then
    FontFmStr(Result,s)
 else
    Result.Name := 'Arial Greek';
 finally ini.free;
 end;
end;
function Font1: TFont;
var s: string;
 ini: TIniFile;
begin
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'Font1','');
 if s <> '' then begin
    Result := TFont.Create;
    FontFmStr(Result,s)
    end
 else Result := BaseFont;
 finally ini.free;
 end;
end;
function Font2: TFont;
var s: string;
 ini: TIniFile;
begin
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'Font2','');
 if s <> '' then begin
    Result := TFont.Create;
    FontFmStr(Result,s)
    end
 else Result := BaseFont;
 finally ini.free;
 end;
end;
function Font3: TFont;
var s: string;
 ini: TIniFile;
begin
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'Font3','');
 if s <> '' then begin
    Result := TFont.Create;
    FontFmStr(Result,s)
    end
 else Result := BaseFont;
 finally ini.free;
 end;
end;
function Font4: TFont;
var s: string;
 ini: TIniFile;
begin
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'Font4','');
 if s <> '' then begin
    Result := TFont.Create;
    FontFmStr(Result,s)
    end
 else Result := PrintFont;
 finally ini.free;
 end;
end;
function Font5: TFont;
var s: string;
 ini: TIniFile;
begin
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'Font5','');
 if s <> '' then begin
    Result := TFont.Create;
    FontFmStr(Result,s)
    end
 else Result := PrintFont;
 finally ini.free;
 end;
end;
function Font6: TFont;
var s: string;
 ini: TIniFile;
begin
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'Font6','');
 if s <> '' then begin
    Result := TFont.Create;
    FontFmStr(Result,s)
    end
 else Result := PrintFont;
 finally ini.free;
 end;
end;

function PrintFontX(X: integer): TFont;
var s: string;
 ini: TIniFile;
begin
 ini := AppIniFile;
 try
 s := ini.ReadString(FontSet,'Font'+IntToStr(X),'');
 if s <> '' then begin
    Result := TFont.Create;
    FontFmStr(Result,s)
    end
 else Result := PrintFont;
 finally ini.free;
 end;
end;

function BaseColor: TColor;
var ini: TIniFile;
begin
 ini := AppIniFile;
 try
 Result := StringToColor(ini.ReadString(FontSet,'BaseColor','clBtnFace'));
 finally ini.free;
 end;
end;
function ColorX(X: Integer): TColor;
var ini: TIniFile;
begin
 ini := AppIniFile;
 try
 Result := StringToColor(ini.ReadString(FontSet,'Color'+IntToStr(X),'clBtnFace'));
 finally ini.free;
 end;
end;

type
  TWinControlCracker = class( TWinControl ) end;
// SetFormFonts from Application Ini file
procedure SetFormFonts(Form: TForm; C: TComponent; Fonts: string);
var i,j: integer;
    F: TFontName;
    FC: TColor;
    cl, cb, c1, c2, c3: TColor;
    FB, F1,F2,F3: TFont;
    PColor, PFont: Boolean;
    OnC, Component: TComponent;
    Control: TWinControl;

 procedure doSetFont(sFont: string; tF1, tF2: TFont; tf3: TFont=nil);
 var  Font: TFont;
 begin
  if IsPublishedProp(Component,sFont) then begin
   if (sFont ='Font') and (Component is TWinControl) then
      Font := TWinControlCracker(Component).Font
   else
      Font := GetObjectProp(Component,sFont) as TFont;
   if Assigned(Font) then
    if PFont or ((Font.Name = F) and (Font.Color = FC)) then
       Font.Assign(tF1)
    else
       begin
       if (Font.Name = F) then
          Font.Assign(tF2);
       end
   else
    if Assigned(tf3) then
       Font.Assign(tf3);
   end;
 end;
 procedure doSetColor(sColor: string; tc1, tc2: TColor; cc: TColor=clWindow);
 var  Color: TColor;
 begin
  if IsPublishedProp(Component,sColor) then
  try
   Color := GetOrdProp(Component,sColor);
   //ParentColor or = Form.Color & ParentFont
   if (PColor or (Color = cl)) and PFont then
      SetOrdProp(Component,sColor,tc1)
   else
   //not ParentColor or ParentFont
   if not (PColor or PFont) then
      SetOrdProp(Component,sColor,tc2)
   else
   if (cc <> clWindow) then
      SetOrdProp(Component,sColor,cc);
  except ; end;
 end;
 procedure CheckC;
 //var gc: TComponent;
 begin
     PFont := True;
     PColor := True;
     try
      if IsPublishedProp(Component,'ParentFont') then
       PFont := Boolean(GetOrdProp(Component,'ParentFont'));
      if IsPublishedProp(Component,'ParentColor') then
       PColor := Boolean(GetOrdProp(Component,'ParentColor'));
      if AnsiSameText(Component.ClassName,'TdxDBGrid') or
         AnsiSameText(Component.ClassName,'TTdxDBGrid') or
         AnsiSameText(Component.ClassName,'TdxDBTreeList') or
         AnsiSameText(Component.ClassName,'TTdxDBTreeList') or
         AnsiSameText(Component.ClassName,'TDBGrid') or
         AnsiSameText(Component.ClassName,'TTDBGrid') then
         begin
         doSetFont('Font',F2,F3);
         doSetColor('Color',c2,c3,c2);
         end
      else
      if ( AnsiContainsText(Component.ClassName,'DBGrid') or
           AnsiContainsText(Component.ClassName,'DBTreeList')
         )
         and
         AnsiContainsText(Component.ClassName,'Column') then
         begin
         {gc := Component.Owner;
         if IsPublishedProp(gc,'ParentFont') then
            PFont := Boolean(GetOrdProp(gc,'ParentFont'));
         if IsPublishedProp(gc,'ParentColor') then
            PColor := Boolean(GetOrdProp(gc,'ParentColor'));}
         doSetFont('Font',F2,F2);
         //doSetColor('Color',c2,c3,c2);
         end
      else
      if AnsiSameText(Component.ClassName,'TDBBoxLabel') or
         AnsiSameText(Component.ClassName,'TBoxLabel') then
         begin
         doSetFont('Font',FB,F3,F3);
         doSetColor('Color',cb,c3,c3);
         end
      else
         begin
         doSetFont('Font',FB,F3);
         //doSetColor('Color',cb,c3);
         end;
      doSetFont('BandFont',F1,F3);
      doSetColor('BandColor',c1,c3,cb);
      doSetFont('HeaderFont',F1,F3);
      doSetColor('HeaderColor',c1,c3,cb);
      doSetFont('TabFont',F1,F3);
      doSetFont('TitleFont',F1,F3);
      doSetColor('TitleColor',c1,c3,cb);
     except ;
     end;
 end;

begin
  if AnsiSameText(Fonts,'*NONE') then Exit;
  FontSet := Fonts;
  if FontSet = '' then FontSet := DEFFONTSET;
  F := Form.Font.Name; {Keep design font for special font checking}
  FC := Form.Font.Color;
  cl := Form.Color;
  FB := BaseFont; F1 := Font1; F2 := Font2; F3 := Font3;
  cb := BaseColor; c1:=ColorX(1);  c2:=ColorX(2); c3:=ColorX(3);
  try
   Form.Font.Assign(FB);
   //if Form.Color <> cb then
   //   Form.Color := cb;
   if Assigned(c) and (c is TWinControl) then
    begin
    j := TWinControl(c).ControlCount -1;
    with c as TWinControl do
    for I := 0 to j do
     begin
     Component := Controls[i];
     //Control := Controls[i];
     CheckC;
     SetFormFonts(Form,Controls[i]);
     end;
    end
   else
    begin
    if Assigned(c) then OnC := c else OnC := Form;
    j := OnC.ComponentCount -1;
    with OnC do
    for I := 0 to j do
     begin
     Component := Components[i];
     //Control := nil;
     CheckC;
     end;
    end;
  finally
    FB.Free; F1.Free; F2.Free; F3.Free;
  end;
end;

type TQRAngledCracker = class(TQRAngledCustom);
     TQRCustomCracker = class(TQRCustomLabel);

procedure SetPrintFonts(Form: TForm; QRep: TQuickRep; Fonts: string);
var i: integer;
    F: TFontName;
    obj: TObject;
    tc: TComponent;
    FP, F4,F5,F6, FX: TFont;
    //ParentFont: Boolean;
    FTag: integer;
begin
  if AnsiSameText(Fonts,'*NONE') then Exit;
  FontSet := Fonts;
  if FontSet = '' then FontSet := DEFFONTSET;
  F := QRep.Font.Name; {Keep design font for special font checking}
  FP := PrintFont; F4 := Font4; F5 := Font5; F6 := Font6;
 try
  if Assigned(Form) then Form.Font.Assign(FP);
  QRep.Font.Assign(FP);
  with Form do
  for I := 0 to ComponentCount -1 do begin
   tc := Components[I];

   //FontTag property support
   FTag := 0;
   if IsPublishedProp(tc,'FontTag') then
    try
      FTag := GetPropValue(tc,'FontTag',False);
      if FTag = 0 then Abort;
      FX := PrintFontX(FTag);
      try
        if IsPublishedProp(tc, 'Font') then
         begin
         obj := GetObjectProp(tc,'Font',TFont);
         if obj is TFont then
           (obj as TFont).Assign(FX);
         end;
      finally FX.Free;
      end;
    except ;
    end;
   if FTag <> 0 then Continue;

   if tc is TQRSysData then
     with tc as TQRSysData do
      case Data of
       qrsReportTitle : if ParentFont then Font.Assign(F6);
      else  if ParentFont then Font.Assign(F5);
      end
   else
   if tc is TQRAngledLabel then //Labels only
     with tc as TQRAngledLabel do
      begin
      if ParentFont then Font.Assign(F5)
      else if Font.Name = F then Font.Assign(F4);
      end
   else
   if tc is TQRAngledCustom then //all other customlabels
    if tc is TQRAngledDBText then
     if not TQRAngledDBText(tc).ShowData then
      with tc as TQRAngledDBText do
        begin
        if ParentFont then Font.Assign(F5)
        else if Font.Name = F then Font.Assign(F4);
        end
     else
        begin
        if (not TQRAngledDBText(tc).ParentFont) and (TQRAngledDBText(tc).Font.Name = F) then
           TQRAngledDBText(tc).Font.Assign(F4);
        end
    else
      begin
      if (not TQRAngledCracker(tc).ParentFont) and (TQRAngledCracker(tc).Font.Name = F) then
         TQRAngledCracker(tc).Font.Assign(F4);
      end
   else
   if tc is TQRLabel then //Labels only
     with tc as TQRLabel do
      begin
      if ParentFont then Font.Assign(F5)
      else if Font.Name = F then Font.Assign(F4);
      end
   else
   if tc is TQRCustomLabel then //all other customlabels
     begin
     if (not TQRCustomCracker(tc).ParentFont) and (TQRCustomCracker(tc).Font.Name = F) then
        TQRCustomCracker(tc).Font.Assign(F4);
     end
   ;
  end; //for loop
 finally
  FP.Free; F4.Free; F5.Free; F6.Free;
 end;
end;

initialization
  FontSet := DEFFONTSET;
end.


