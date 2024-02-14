object MainF: TMainF
  Left = 201
  Height = 526
  Top = 156
  Width = 825
  Caption = 'Files Listing'
  ClientHeight = 504
  ClientWidth = 825
  Menu = MainM
  PopupMenu = MainPM
  LCLVersion = '8.3'
  object MainS: TStatusBar
    Left = 0
    Height = 22
    Top = 482
    Width = 825
    Panels = <>
  end
  object MainT: TToolBar
    Left = 0
    Height = 26
    Top = 0
    Width = 825
    Caption = 'MainT'
    Images = MainIL
    TabOrder = 1
    object CloseTB: TToolButton
      Left = 1
      Top = 2
      Caption = 'Close'
      ImageIndex = 0
      OnClick = CloseMIClick
    end
  end
  object ListView1: TListView
    Left = 0
    Height = 456
    Top = 26
    Width = 825
    Align = alClient
    Columns = <>
    TabOrder = 2
  end
  object MainM: TMainMenu
    Left = 40
    Top = 40
    object FilesMI: TMenuItem
      Caption = 'Fichiers'
      object SearchMI: TMenuItem
        Caption = 'Recherche'
        OnClick = SearchMIClick
      end
      object Separator1: TMenuItem
        Caption = '-'
      end
      object CloseMI: TMenuItem
        Caption = 'Quitter'
        OnClick = CloseMIClick
      end
    end
    object HAboutMI: TMenuItem
      Caption = '?'
      object AboutMi: TMenuItem
        Caption = 'A propos...'
        OnClick = AboutMiClick
      end
    end
  end
  object MainPM: TPopupMenu
    Left = 128
    Top = 40
  end
  object MainIL: TImageList
    Left = 208
    Top = 32
    Bitmap = {
      4C7A010000001000000010000000000200000000000078DACD93DD4B93511C80
      FB1782220A0AA22EBA880882200882200884203248B69CA5CC645311B5B19AE5
      D44CD4CA98639BDB7499E6C79B334BDB9A656B86BAB635DD479ABAD45CE44D7F
      C3D3D92B7413EF5D175D9CCBE73CE7F79C73801DFC27ABC29D46EF4C52D69540
      6B8B536C8D516489A0EE98A3E0E10C97DBA6B9D8124489D709D69BDA424A6E31
      BCF093C1F80FFA63597A23593CE1EFB86737C86B7AABC897DAE79104D7115AE3
      41F01BAD53ABDC9F5CA1D1BF4CFDEB251C1FD73957EF57E473E7CD39DBDF6768
      79B7CABDC00A66DF57EE8C2F72EBE5173A3F6438631A57E40B1F87E98B666567
      D39B6DA7E9D522C617696A46523C127B9E328C2AF2B9469EF0260D7EE19C5892
      9D86D134D5CF93944B29DA03CB9CA89614F94BAD215C331BDBCEB134B5C25925
      25D004B2E4A5C034B9CEB1CA0145FE42F314F6E9B53FCECAA104BA817954BE4D
      F243BF308B0E47744F15F9F30D012CC18CEC2C1F5CA0AC3F8EB6F733D77BA214
      BA22DC151D0E95F628F267EB26E4467AE12CED8B53F244BC9FEE286AE7270A1C
      616E8B790E94B814F9D3C631DA4477D9E989A17147507585B9629B23DF3A8B41
      4AB2B7C8AEC89FAC1DA159DC59A3E86716EDEB44879CD328B89BC3096AC44CBB
      AF5AFFE27D3E1F5EAF97E355431CAD7826373A7CC3C3416D37FB8B9DECBBE660
      8FC6C62E75273B5516F47ABDBCFEC59FFB0D6935E8F0
    }
  end
end
