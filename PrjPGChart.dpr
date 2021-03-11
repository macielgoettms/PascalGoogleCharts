program PrjPGChart;

uses
  Vcl.Forms,
  pgchart.enumerations in 'source\pgchart.enumerations.pas',
  pgchart.interfaces in 'source\pgchart.interfaces.pas',
  pgchart in 'source\pgchart.pas',
  pgchart.view in 'source\pgchart.view.pas' {pgchartView},
  webBrowser.Helper in 'helper\webBrowser.Helper.pas',
  pgchart.test.view in 'test\pgchart.test.view.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
