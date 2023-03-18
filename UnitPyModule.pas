unit UnitPyModule;

interface

uses
  System.SysUtils, System.Classes, PythonEngine, PyCommon, PyModule, PyPackage,
  NumPy, PyEnvironment, PyEnvironment.Embeddable, PyEnvironment.AddOn,
  PyEnvironment.AddOn.GetPip, FMX.PythonGUIInputOutput, MatplotLib, Pandas,
  Meteostat, ScikitLearn, PyEnvironment.AddOn.EnsurePip;

type

  TPyModule = class(TDataModule)
    PythonEngine1: TPythonEngine;
    PyEmbeddedEnvironment1: TPyEmbeddedEnvironment;
    NumPy1: TNumPy;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PyEnvironmentAddOnGetPip1: TPyEnvironmentAddOnGetPip;
    Pandas1: TPandas;
    MatplotLib1: TMatplotLib;
    Meteostat1: TMeteostat;
    ScikitLearn1: TScikitLearn;
    PyEnvironmentAddOnEnsurePip1: TPyEnvironmentAddOnEnsurePip;
    procedure NumPy1BeforeInstall(Sender: TObject);
    procedure NumPy1AfterInstall(Sender: TObject);
    procedure NumPy1BeforeImport(Sender: TObject);
    procedure NumPy1AfterImport(Sender: TObject);
    procedure PyEnvironmentAddOnGetPip1Execute(const ASender: TObject);
    procedure PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1Ready(Sender: TObject;
      const APythonVersion: string);
  private
    { Private declarations }
    _pybin: Variant;
    _pymain: Variant;
    _pyop: Variant;
  public
    { Public declarations }
    procedure InitImportPyModules();
    procedure FetchMeteodata(const meteoID: string);
  type
    TStatusCallback = reference to procedure (const status, description: string; active: Boolean = True);

    procedure CreatePythonEnvironment(InstallStatusCallback: TStatusCallback);
    procedure InitializePythonModules();
  private
    _StatusCallback: TStatusCallback;
  end;

var
  PyModule: TPyModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}


uses
  VarPyth, PyUtils;

procedure TPyModule.CreatePythonEnvironment(InstallStatusCallback: TStatusCallback);
begin
  Self._StatusCallback := InstallStatusCallback;

  InstallStatusCallback('Python', 'Setup Python.', True);

  with PyEmbeddedEnvironment1 do
    ActivateAsync(PyEmbeddedEnvironment1.SetupAsync());
  //var FCancelable :=
  //InstallStatusCallback('Done!', 'All done.', False);
  Exit;




end;

procedure TPyModule.FetchMeteodata(const meteoID: string);
begin
  var datetime := Import('datetime');
  var start_ts := datetime.datetime(2022,01,01, 0);
  var end_ts := datetime.datetime(2022,12,31, 21);

  var hourly := Meteostat1.meteostat.Hourly(meteoID, start_ts, end_ts).fetch();

  MaskFPUExceptions(true);
  _pybin.print(hourly);
end;

procedure TPyModule.InitializePythonModules;
begin
  _StatusCallback('Importing basic modules', '');
  InitImportPyModules();
  _StatusCallback('Advanced modules', 'Install Meteostat');
  Meteostat1.Install();
  _StatusCallback('Advanced modules', 'Import Meteostat');
end;

procedure TPyModule.InitImportPyModules;
begin
  _pybin := BuiltinModule();
  _pymain := MainModule();
  _pyop := Import('operator');
end;

procedure TPyModule.NumPy1AfterImport(Sender: TObject);
begin
  _StatusCallback('Importing modules', 'NumPy successfully imported.');
end;

procedure TPyModule.NumPy1AfterInstall(Sender: TObject);
begin
  _StatusCallback('NumPy', 'NumPy successfully installed.');
end;

procedure TPyModule.NumPy1BeforeImport(Sender: TObject);
begin
  _StatusCallback('Importing modules', 'Importing NumPy...');
end;

procedure TPyModule.NumPy1BeforeInstall(Sender: TObject);
begin
  _StatusCallback('NumPy',  'Installing NumPy...');
end;

procedure TPyModule.PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  _StatusCallback('Setup Done!', 'After setup.', False);
end;

procedure TPyModule.PyEmbeddedEnvironment1Ready(Sender: TObject;
  const APythonVersion: string);
begin
  InitializePythonModules();
  _StatusCallback('Ready!', 'Ready.', False);
end;

procedure TPyModule.PyEnvironmentAddOnGetPip1Execute(const ASender: TObject);
begin
  TThread.Synchronize(nil, procedure() begin
    _StatusCallback('PIP', 'Getting PIP ready...');
  end);
end;

end.
