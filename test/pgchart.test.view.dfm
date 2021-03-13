object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 56
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Test Gantt'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 152
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Test Bar'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 248
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Pie'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 336
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Donut'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 417
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Line'
    TabOrder = 4
    OnClick = Button5Click
  end
end
