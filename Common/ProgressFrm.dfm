object ProgressForm: TProgressForm
  Left = 399
  Top = 190
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Progress'
  ClientHeight = 82
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object MsgLabel: TLabel
    Left = 0
    Top = 30
    Width = 306
    Height = 52
    Align = alClient
    Caption = '...'
    WordWrap = True
  end
  object StsLabel: TLabel
    Left = 0
    Top = 0
    Width = 306
    Height = 13
    Align = alTop
    Caption = '...'
    WordWrap = True
  end
  object ProgressBar: TProgressBar
    Left = 0
    Top = 13
    Width = 306
    Height = 17
    Align = alTop
    Min = 1
    Max = 1
    Position = 1
    TabOrder = 0
  end
end
