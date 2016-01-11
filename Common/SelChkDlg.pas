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
 Description.....: Multi Select Dialog
 Developer/Author: George J. Vavoulogiannis, georgev@hol.gr
                   http://g-vavoulogiannis.blogspot.com

===============================================================================}

unit SelChkDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, CheckLst;

type
  TMultiSelDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    rd: TCheckListBox;
    procedure LBDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function MultiSelect(SelList: array of string; ACaption: string; var Selection: string; revsels: Boolean=False): Boolean;
function MultiSelectFields(FullList, SelList, Caption: string): string; //select from fields string (fieldnames separated by ';')

var
  MultiSelDlg: TMultiSelDlg;

implementation

uses AppIni, UtilFun;

{$R *.dfm}

function MultiSelect(SelList: array of string; ACaption: string; var Selection: string; revsels: Boolean): Boolean;
var i,j,p: integer;
    si: string;
begin
  Result := False;
  //? if i = Length(SelList) then Exit;
  with TMultiSelDlg.Create(Application) do
   try Caption := ACaption;
    Position := poMainFormCenter;
    rd.Items.Assign(QuickStrings(SelList));
    p:=1;
    while p<=Length(Selection) do begin
      si := ExtractString(Selection,p,',');
      try i := StrToInt(si) except on Exception do i:=-1 ; end;
      if (i>=0) and (i <= rd.Items.Count-1) then
         rd.Checked[i] := True;
      end;
    i := Length(SelList);
    j := Abs(rd.Font.Height);
    j := j + j div 5;
    Height := (i*j)+30+Panel2.Height;
    if Height > 500 then Height := 500;
    j := i div 40;
    if j > 0 then rd.Columns := j + 1;
    Width := 500;
    if ShowModal = mrOK then
      begin
      Result := True;
      Selection := '';
      if revsels then
      { се пеяиптысг поу вяеиафетаи ма диацяажоум апо киста ои INDEXES
        г диацяажг пяепеи ма нейимгсеи апо том лецакутеяо INDEX
        диоти то DELETE епамадиатасеи тоус INDEXES тгс кистас TObjectList/TStrings.Delete(i) }
      begin
      for i:=rd.Items.Count-1 downto 0 do
       if rd.Checked[i] then
          Selection := Selection + IIF(Selection='','',',') + IntToStr(i);
      end
      else
      for i:=0 to rd.Items.Count-1 do
       if rd.Checked[i] then
          Selection := Selection + IIF(Selection='','',',') + IntToStr(i);
      end;
   finally Free;
   end;
end;

function MultiSelectFields(FullList, SelList, Caption: string): string;
var sels, s: string;
   sL: TStringList;
   i,p: integer;
begin
  Result := SelList;
  sL := QuickStrings(ArrayFromFields(FullList));
  try
    //for i:=0 to sFields.Count-1 do
    // sL.Add(sFields.Values[sFields.Names[i]]);
  p:=0;sels := '';
  while p<=Length(SelList) do begin
    s := ExtractString(SelList,p,';');
    i := sL.IndexOf(s);
    if i<>-1 then
       sels := sels+IIF(sels='','',',')+IntToStr(i);
    end;
  if MultiSelect(ArrayFromStrings(sL),Caption,sels) and (sels<>'') then
   begin
   p:=0; Result :='';
   while p<=Length(sels) do begin
     s := ExtractString(sels,p,',');
     i:=StrToInt(s);
     Result := Result+IIF(Result='','',';')+sL[i];
     end;
   end;
  finally sL.Free;
  end;
end;

procedure TMultiSelDlg.LBDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TMultiSelDlg.FormCreate(Sender: TObject);
begin
//  AppIni.SetFormFonts(Self);
end;

end.
