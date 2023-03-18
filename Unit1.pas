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
    Memo1: TMemo;
    AniIndicator1: TAniIndicator;
    LabelPyStatus: TLabel;
    ButtonFetchData: TButton;
    EditMeteoStationId: TEdit;
    LabelMeteostationId: TLabel;
    procedure ButtonCreatePyClick(Sender: TObject);
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
  PyModule.PythonGUIInputOutput1.Output := Memo1; //Link from Python Module to GUI

  UpdateInstallStatus := procedure (const status, description: string; active: Boolean)
    begin
      SwitchProgressLabelsVisibility(active);
      DisplayLabelsText(status, description);
    end;
  PyModule.CreatePythonEnvironment(UpdateInstallStatus);
end;


procedure TForm1.DisplayLabelsText(const status, details: string);
begin
      LabelPyStatus.Text := status;
      //LabelDetails.Text := details;
      Memo1.Lines.Add(details);
end;

procedure TForm1.SwitchProgressLabelsVisibility(displayed: Boolean);
begin
    AniIndicator1.Enabled := displayed;
    AniIndicator1.Visible := displayed;

    LabelPyStatus.Visible := displayed;
end;

end.
