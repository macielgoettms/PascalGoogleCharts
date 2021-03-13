program PrjPGChart;

uses
  Vcl.Forms,
  pgchart.enumerations in 'source\pgchart.enumerations.pas',
  pgchart.interfaces in 'source\pgchart.interfaces.pas',
  pgchart in 'source\pgchart.pas',
  pgchart.view in 'source\pgchart.view.pas' {pgchartView},
  webBrowser.helper in 'helper\webBrowser.helper.pas',
  pgchart.test.view in 'test\pgchart.test.view.pas' {Form1},
  pgchart.line in 'source\pgchart.line.pas',
  pgchart.functions in 'source\pgchart.functions.pas',
  pgchart.gantt in 'source\pgchart.gantt.pas',
  pgchart.bar in 'source\pgchart.bar.pas',
  pgchart.pie in 'source\pgchart.pie.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
