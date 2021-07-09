object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 530
  ClientWidth = 744
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 50
    Top = 8
    Width = 640
    Height = 480
  end
  object TrackBar: TTrackBar
    Left = 40
    Top = 494
    Width = 650
    Height = 27
    Max = 20
    Min = -20
    TabOrder = 0
    OnChange = TrackBarChange
  end
end
