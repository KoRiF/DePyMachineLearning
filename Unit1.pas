unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.StdCtrls, FMX.ScrollBox, FMX.Grid, FMX.TabControl,
  FMX.Controls.Presentation, FMX.Memo.Types, FMX.Memo, FMX.Edit;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    TabControl1: TTabControl;
    TabItemData: TTabItem;
    StringGrid1: TStringGrid;
    ButtonLoadData: TButton;
    ButtonClear: TButton;
    TabItemEngineering: TTabItem;
    ButtonCreatePy: TButton;
    MemoPyOutput: TMemo;
    AniIndicator1: TAniIndicator;
    LabelPyStatus: TLabel;
    ButtonFetchData: TButton;
    EditMeteoStationId: TEdit;
    LabelMeteostationId: TLabel;
    TabItemEnvironment: TTabItem;
    PanelPyEnvironment: TPanel;
    PanelData: TPanel;
    Panel2: TPanel;
    RadioButtonUnsupervized: TRadioButton;
    PanelWorkflowDivarication: TPanel;
    RadioButtonSupervized: TRadioButton;
    TabItemUnsupervized: TTabItem;
    TabItemSupervized: TTabItem;
    PanelUnsupervizedLearning: TPanel;
    ButtonClusterization: TButton;
    procedure ButtonCreatePyClick(Sender: TObject);
    procedure ButtonFetchDataClick(Sender: TObject);
  private
    { Private declarations }
    procedure SwitchProgressLabelsVisibility(displayed: Boolean);
    procedure DisplayLabelsText(const status, details :string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses UnitPyModule;

procedure TForm1.ButtonCreatePyClick(Sender: TObject);
var UpdateInstallStatus: TPyModule.TStatusCallback;
begin
  PyModule.PythonGUIInputOutput1.Output := MemoPyOutput; //Link from Python Module to GUI

  UpdateInstallStatus := procedure (const status, description: string; active: Boolean)
    begin
      SwitchProgressLabelsVisibility(active);
      DisplayLabelsText(status, description);

      if PyModule.IsPythonEnvironmentReady then
        TabItemData.IsSelected := True;
    end;
  PyModule.CreatePythonEnvironment(UpdateInstallStatus);
end;

procedure TForm1.ButtonFetchDataClick(Sender: TObject);
begin
  PyModule.FetchMeteodata(EditMeteoStationId.Text);
  if RadioButtonUnsupervized.IsChecked then
    TabItemUnsupervized.IsSelected := True
  else if RadioButtonSupervized.IsChecked then
    TabItemSupervized.IsSelected := True
  else
    TabItemEngineering.IsSelected := True;
end;

procedure TForm1.DisplayLabelsText(const status, details: string);
begin
  LabelPyStatus.Text := status;
  MemoPyOutput.Lines.Add(details);
end;

procedure TForm1.SwitchProgressLabelsVisibility(displayed: Boolean);
begin
  var ready := PyModule.IsPythonEnvironmentReady;

  AniIndicator1.Enabled := displayed;
  AniIndicator1.Visible := displayed or not ready;

  LabelPyStatus.Visible := displayed or not ready;
end;

end.
