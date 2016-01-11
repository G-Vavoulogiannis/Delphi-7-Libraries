object MultiSelDlg: TMultiSelDlg
  Left = 312
  Top = 123
  BorderStyle = bsToolWindow
  Caption = '...'
  ClientHeight = 229
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 313
    Height = 199
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object rd: TCheckListBox
      Left = 0
      Top = 0
      Width = 313
      Height = 199
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 199
    Width = 313
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 4
      Top = 4
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 84
      Top = 4
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
