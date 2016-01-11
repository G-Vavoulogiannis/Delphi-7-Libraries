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
 Description.....: Select from List Dialog
 Developer/Author: George J. Vavoulogiannis, georgev@hol.gr
                   http://g-vavoulogiannis.blogspot.com

===============================================================================}

unit SelLstDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TListSelDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    LB: TListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure LBDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ListSelect(SelList: array of string; ACaption: string; Selection: integer=0): integer;

var
  ListSelDlg: TListSelDlg;

implementation

uses AppIni, UtilFun;

{$R *.dfm}

function ListSelect(SelList: array of string; ACaption: string; Selection: integer): integer;
var i,j: integer;
begin
  Result := -1;
  i := Length(SelList);
  if i = 0 then Exit;
  with TListSelDlg.Create(Application) do
   try Caption := ACaption;
    Position := poMainFormCenter;
    LB.Items.Assign(QuickStrings(SelList));
    LB.ItemIndex := Selection;
    j := Abs(LB.Font.Height);
    j := j + j div 5;
    Height := (i*j)+30+Panel2.Height;
    if Height > 500 then Height := 500;
    j := i div 40;
    if j > 0 then LB.Columns := j + 1;
    Width := 500;
    if ShowModal = mrOK then
      Result := LB.ItemIndex;
   finally Free;
   end;
end;

procedure TListSelDlg.LBDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TListSelDlg.FormCreate(Sender: TObject);
begin
//  AppIni.SetFormFonts(Self);
end;

end.
