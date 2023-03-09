unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.StdCtrls, FMX.ScrollBox, FMX.Grid, FMX.TabControl,
  FMX.Controls.Presentation, FMX.Memo.Types, FMX.Memo;

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
    procedure ButtonCreatePyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses UnitPyModule;

procedure TForm1.ButtonCreatePyClick(Sender: TObject);
var UpdateInstallStatus: TPyModule.TInstallStatusCallback;
begin
  PyModule.PythonGUIInputOutput1.Output := Memo1; //Link from Python Module to GUI

  UpdateInstallStatus := procedure (const status, description: string; active: Boolean)
    begin

    end;
  PyModule.CreatePythonEnvironment(UpdateInstallStatus);
end;

end.
