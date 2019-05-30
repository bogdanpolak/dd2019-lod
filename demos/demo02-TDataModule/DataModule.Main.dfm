object MainDataModule: TMainDataModule
  OldCreateOrder = False
  Height = 150
  Width = 215
  object FDConnection1: TFDConnection
    Params.Strings = (
      'ConnectionDef=SQLite_Demo')
    LoginPrompt = False
    Left = 40
    Top = 16
  end
  object fdqEmployees: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM Employees')
    Left = 40
    Top = 80
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Console'
    Left = 136
    Top = 16
  end
end
