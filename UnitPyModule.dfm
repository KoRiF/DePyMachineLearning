object PyModule: TPyModule
  Height = 435
  Width = 759
  PixelsPerInch = 96
  object PythonEngine1: TPythonEngine
    AutoLoad = False
    IO = PythonGUIInputOutput1
    Left = 32
    Top = 16
  end
  object PyEmbeddedEnvironment1: TPyEmbeddedEnvironment
    AutoLoad = False
    PythonEngine = PythonEngine1
    Distributions = <
      item
        DeleteEmbeddable = False
      end>
    Scanner.AutoScan = True
    Scanner.ScanRule = srFolder
    Scanner.EmbeddablesPath = 'embeddables'
    Scanner.EnvironmentPath = 'environments'
    Scanner.DeleteEmbeddable = False
    Left = 72
    Top = 112
  end
  object NumPy1: TNumPy
    PythonEngine = PythonEngine1
    ManagerKind = pip
    Left = 40
    Top = 240
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Left = 656
    Top = 24
  end
  object PyEnvironmentAddOnGetPip1: TPyEnvironmentAddOnGetPip
    Left = 456
    Top = 112
  end
  object Pandas1: TPandas
    ManagerKind = pip
    Left = 120
    Top = 240
  end
  object MatplotLib1: TMatplotLib
    ManagerKind = pip
    Left = 200
    Top = 240
  end
  object Meteostat1: TMeteostat
    ManagerKind = pip
    Left = 120
    Top = 312
  end
end
