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
 Description.....: A simple Progress form
 Developer/Author: George J. Vavoulogiannis, georgev@hol.gr
                   http://g-vavoulogiannis.blogspot.com

===============================================================================}

unit ProgressFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TProgressForm = class(TForm)
    MsgLabel: TLabel;
    ProgressBar: TProgressBar;
    StsLabel: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowProgress(Min,Max,Posit: integer; sMsg: string=''; sSts: string='');
procedure CloseProgress;

var
  ProgressForm: TProgressForm;

implementation

{$R *.dfm}

procedure ShowProgress(Min,Max,Posit: integer; sMsg: string; sSts: string);
begin
 if not Assigned(ProgressForm) then
    Application.CreateForm(TProgressForm,ProgressForm);
 with ProgressForm do
  begin
  try
   Show;
   BringToFront;
   if (Min<>0) and (ProgressBar.Min <> Min) then ProgressBar.Min := Min;
   if (Max<>0) and (ProgressBar.Max <> Max) then ProgressBar.Max := Max;
   if (Posit<>0) and (ProgressBar.Position <> Posit) then ProgressBar.Position := Posit;
   if sMsg <> '' then MsgLabel.Caption := sMsg;
   if sSts <> '' then StsLabel.Caption := sSts;
   Update;
  except ; end;
  Application.ProcessMessages;
  end;
end;

procedure CloseProgress;
begin
 if Assigned(ProgressForm) then FreeAndNil(ProgressForm);
end;

procedure TProgressForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  ProgressForm := nil;
end;

Initialization
 ProgressForm := nil;

end.
