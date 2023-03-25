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
    procedure ClusterizeMeteodata();
  type
    TStatusCallback = reference to procedure (const status, description: string; active: Boolean = True);

    procedure CreatePythonEnvironment(InstallStatusCallback: TStatusCallback);
    procedure InitializePythonModules();
  private
    _StatusCallback: TStatusCallback;
    _isEnvironmentReady: Boolean;
  public
    property IsPythonEnvironmentReady: Boolean read _isEnvironmentReady;
  end;

var
  PyModule: TPyModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}


uses
  VarPyth, PyUtils;

procedure TPyModule.ClusterizeMeteodata;
const
  FIG0_PNG = 'MeteoData_Origin_plot.png';
  FIG_KMEANS_PNG = 'MeteoData_K-Means-Clustered_plot.png';
begin
  Pandas1.Import();
  var pd := Pandas1.pandas;

  //----- Set observation interval ---
  var datetime := Import('datetime');
  var start_ts := datetime.datetime(2022, 12, 1, 0);
  var end_ts := datetime.datetime(2022, 12, 12, 21);

  //----- Define set of meteostations ---
  var meteoIds := TArray<string>.Create('33038', '83566', '50953', '98430');

  //----- Fetch and accumulate meteodata ---
  var hh := TPyEx.List([]);
  for var k := Low(meteoIds) to High(meteoIds) do
  begin
    _pymain.hourly := Meteostat1.meteostat.Hourly(meteoIDs[k], start_ts, end_ts).fetch();
    PythonEngine1.ExecString('hourly["wmo"] = ' + meteoIds[k]); //_pymain.hourly.Values['wmo'] := meteoIds[k]; //
    hh.append(_pymain.hourly);
  end;
  _pymain.hdata := pd.concat(hh);

  //------- Plot meteodata ----
  Matplotlib1.Import();
  var plt := Import('matplotlib.pyplot'); //var plt := MatplotLib1.plt;

  //MatplotLib1.matplot.use('TkAgg');

  plt.scatter(
    _pymain.hdata.Values['temp'],//GetItem('temp'),
    _pymain.hdata.Values['rhum'],//GetItem('rhum'),
    c:=_pymain.hdata.Values['wmo']//GetItem('wmo')
    );
  plt.xlabel('temp');
  plt.ylabel('rhum');
  plt.title('wmo');

  plt.colorbar();

  plt.show();

  // Save the figure to a file
  var fig := plt.gcf();
  fig.savefig(FIG0_PNG);

  // --------------------------------------------------
  //Do clustering
  ScikitLearn1.Import();
  //var KMeans := ScikitLearn1.sklearn.KMeans;

  var xyzcols := TPyEx.List(['temp', 'rhum', 'wmo']);
  var df := _pymain.hdata.Values[xyzcols];
  var xycols := TPyEx.List(['temp', 'rhum']);
  var N_clusters := Length(meteoIds); //_pybin.len(wmos)


  // Create a KMeans clustering model with N_clusters clusters
  PythonEngine1.ExecString('from sklearn.cluster import KMeans');
  var kmeans := ScikitLearn1.sklearn.cluster.KMeans(n_clusters:=N_clusters, n_init:=42, random_state:=13);
  // Fit the model to the data frame
  kmeans.fit(df.Values[xycols]);
  // Get the cluster labels for each data point
  var labels := kmeans.labels_;

  // Create a new column in the data frame with the cluster labels
  _pymain.df := df;
  _pymain.labels := labels;
  PythonEngine1.ExecString('df["cluster"] = labels'); //df.Values['cluster'] := labels;

  var feature_label_majority_checker := function(df: Variant; const orig_label, cluster_label: String): Variant
    begin
      // Get the majority clustered_label for each original label group
      var df_groupby_orig := df.groupby(orig_label);

//      var lambda_counter := function (d: Variant): Variant
//        begin
//          RESULT := 1;//d.value_counts().index[0];
//        end;
      var lambda_counter := VarPythonEval('lambda d: d.value_counts().index[0]');

      var label_cluster :=df_groupby_orig.Values[cluster_label];
      var counted := label_cluster.agg(func:=lambda_counter);
      var majority_labels := counted.to_dict();

//      var lambda_checker := function (d: Variant): Variant
//        begin
//          var cluster_mark := d.loc.Values[cluster_label];
//          var majority_mark := d.loc.Values[orig_label];
//          RESULT := cluster_mark = majority_mark;
//        end;
      //_pyop.eq(df.Values[cluster_label], df.Values[orig_label])
      _pymain.majority_labels := majority_labels;
      _pymain.cluster_label := cluster_label;
      _pymain.orig_label := orig_label;
      var lambda_checker := VarPythonEval('lambda d: d[cluster_label]==majority_labels[d[orig_label]]');
      RESULT := df.apply(lambda_checker, axis:=1);
    end;

  _pymain.kmeans_matched := feature_label_majority_checker(_pymain.df, 'wmo', 'cluster');

  PythonEngine1.ExecString('df["kmeans_matched"] = kmeans_matched');

  plt.scatter(_pymain.df.Values['temp'], _pymain.df.Values['rhum'], c:=_pymain.df.Values['wmo']);

  // Get the points with incorrect labels (i.e. wmo_No<>labels)
  PythonEngine1.ExecString('incorrect = df[~df["kmeans_matched"]]');
  plt.scatter(_pymain.incorrect.Values['temp'], _pymain.incorrect.Values['rhum'], facecolors:='none', edgecolors:='r', alpha:=0.25, s:=100);

  // Get the points with correct labels (i.e. wmo_No==labels)
  PythonEngine1.ExecString('correct = df[df["kmeans_matched"]]');

  _pybin.print(_pymain.majority_labels);
  _pybin.print(_pymain.df);

  plt.scatter(_pymain.correct.Values['temp'], _pymain.correct.Values['rhum'], facecolors:='none', edgecolors:='g', alpha:=0.125, s:=100);

  // Add a color bar to the plot
  plt.colorbar();

  // Set the axis labels
  plt.xlabel('Temperature (�C)');
  plt.ylabel('Relative Humidity (%)');

  // Show the plot
  plt.show();

  // Save the figure to a file
  fig := plt.gcf();
  fig.savefig(FIG_KMEANS_PNG);
end;

procedure TPyModule.CreatePythonEnvironment(InstallStatusCallback: TStatusCallback);
begin
  _isEnvironmentReady := False;
  Self._StatusCallback := InstallStatusCallback;

  InstallStatusCallback('Python', 'Setup Python.', True);

  with PyEmbeddedEnvironment1 do
    ActivateAsync(PyEmbeddedEnvironment1.SetupAsync());

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
  _isEnvironmentReady := True;
  _StatusCallback('Ready!', 'Ready.', False);
end;

procedure TPyModule.PyEnvironmentAddOnGetPip1Execute(const ASender: TObject);
begin
  TThread.Synchronize(nil, procedure() begin
    _StatusCallback('PIP', 'Getting PIP ready...');
  end);
end;

end.
