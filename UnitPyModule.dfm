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
    AfterSetup = PyEmbeddedEnvironment1AfterSetup
    OnReady = PyEmbeddedEnvironment1Ready
    AutoLoad = False
    PythonVersion = '3.9'
    PythonEngine = PythonEngine1
    Distributions = <>
    Scanner.AutoScan = True
    Scanner.ScanRule = srFileName
    Scanner.EmbeddablesPath = 'py/embeddables'
    Scanner.EnvironmentPath = 'py\environment'
    Scanner.DeleteEmbeddable = False
    Left = 72
    Top = 112
  end
  object NumPy1: TNumPy
    AutoImport = False
    BeforeImport = NumPy1BeforeImport
    AfterImport = NumPy1AfterImport
    PythonEngine = PythonEngine1
    PyEnvironment = PyEmbeddedEnvironment1
    ManagerKind = pip
    AutoInstall = False
    BeforeInstall = NumPy1BeforeInstall
    AfterInstall = NumPy1AfterInstall
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
    Environment = PyEmbeddedEnvironment1
    OnExecute = PyEnvironmentAddOnGetPip1Execute
    Left = 456
    Top = 112
  end
  object Pandas1: TPandas
    PythonEngine = PythonEngine1
    PyEnvironment = PyEmbeddedEnvironment1
    ManagerKind = pip
    Left = 120
    Top = 240
  end
  object MatplotLib1: TMatplotLib
    PythonEngine = PythonEngine1
    PyEnvironment = PyEmbeddedEnvironment1
    ManagerKind = pip
    Left = 248
    Top = 240
  end
  object Meteostat1: TMeteostat
    PythonEngine = PythonEngine1
    PyEnvironment = PyEmbeddedEnvironment1
    ManagerKind = pip
    Left = 120
    Top = 312
  end
  object ScikitLearn1: TScikitLearn
    PythonEngine = PythonEngine1
    PyEnvironment = PyEmbeddedEnvironment1
    ManagerKind = pip
    Left = 40
    Top = 376
  end
  object PyEnvironmentAddOnEnsurePip1: TPyEnvironmentAddOnEnsurePip
    Environment = PyEmbeddedEnvironment1
    Verbose = False
    Upgrade = False
    Left = 456
    Top = 176
  end
  object PythonModule1: TPythonModule
    Engine = PythonEngine1
    OnInitialization = PythonModule1Initialization
    ModuleName = 'delphi_mod'
    Errors = <>
    Left = 456
    Top = 16
  end
end
