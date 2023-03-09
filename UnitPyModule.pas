unit UnitPyModule;

interface

uses
  System.SysUtils, System.Classes, PythonEngine, PyCommon, PyModule, PyPackage,
  NumPy, PyEnvironment, PyEnvironment.Embeddable, PyEnvironment.AddOn,
  PyEnvironment.AddOn.GetPip, FMX.PythonGUIInputOutput, MatplotLib, Pandas,
  Meteostat;

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
  private
    { Private declarations }

  public
    { Public declarations }
  type
    TInstallStatusCallback = reference to procedure (const status, description: string; active: Boolean = True);

    procedure CreatePythonEnvironment(InstallStatusCallback: TInstallStatusCallback);
  end;

var
  PyModule: TPyModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}


uses
  VarPyth;

procedure TPyModule.CreatePythonEnvironment(InstallStatusCallback: TInstallStatusCallback);
begin

  InstallStatusCallback('Python', 'Setup Python.', True);
  TThread.CreateAnonymousThread(procedure()
  begin
    try
      PyEmbeddedEnvironment1.Setup('3.10');
      TThread.Synchronize(nil, procedure() begin
        InstallStatusCallback('Python', 'Activating environment.');
        PyEmbeddedEnvironment1.Activate({'3.10'});
      end);

      NumPy1.Install();
      //ScikitLearn1.Install();

      TThread.Synchronize(nil, procedure() begin
        InstallStatusCallback('Importing modules', '');
        var bm := Import('builtins');
        NumPy1.Import();
        InstallStatusCallback('Importing modules', 'NumPy successfully imported.');
        //ScikitLearn1.Import();
        //UpdateInstallationStatus('Importing modules', 'Scikit-learn successfully imported.');

        bm.print('We are good to go...' + sLineBreak);
        bm.print('Printing NumPy installation info.');
        bm.print(NumPy1.np);
        //bm.print('Printing Scikit-learn installation info.');
        //bm.print(ScikitLearn1.sklearn);
        {
        bm.print('Creating a NumPy array of ones');
        bm.print(NumPy1.np.ones(3));
        }
        InstallStatusCallback('Done!', 'All done.', False);
      end);
    except
      on E: Exception do begin
        InstallStatusCallback('Error', 'Setup has failed.', False);
        ShowException(E, ExceptAddr);
      end;
    end;
  end).Start();
end;

end.
