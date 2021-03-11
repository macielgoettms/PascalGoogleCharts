unit pgchart;

interface

uses
  SHDocVw,
  System.Classes,
  webBrowser.Helper,
  pgchart.interfaces,
  pgchart.enumerations,
  pgchart.view;

type
  TPGChart = class(TInterfacedObject, IPGChart)
  private
  class var
    FInstance: IPGChart;
    FGantt: IPGChartGantt;
    constructor Create;
  public
    class function New: IPGChart;
    function Gantt: IPGChartGantt;
  end;

  PGChartGanttRow = record
    TaskID: string;
    TaskName: string;
    Resource: string;
    StartOf: TDate;
    EndOf: TDate;
    Duration: Double;
    PercentComplete: Double;
    Dependencies: string;
  end;

  TPGChartGantt = class(TInterfacedObject, IPGChartGantt)
  private
    FRows: array of PGChartGanttRow;

    function GenerateChartCode: string;
  public
    function AddRows(
      const taskID: string;
      const taskName: string;
      const resource: string;
      const startOf: TDate;
      const endOf: TDate;
      const duration: Double;
      const percentComplete: Double;
      const dependencies: string): IPGChartGantt;

    procedure Show; overload;
    procedure Show(const webBrowser: TWebBrowser); overload;
  end;

implementation

{ TPGChart }

constructor TPGChart.Create;
begin
  inherited;

end;

function TPGChart.Gantt: IPGChartGantt;
begin
  if not Assigned(FGantt) then
    FGantt := TPGChartGantt.Create;
  Result := FGantt;
end;

class function TPGChart.New: IPGChart;
begin
  if not Assigned(FInstance) then
    FInstance := TPGChart.Create;

  Result := Self.FInstance;
end;

{ TPGChartGantt }

function TPGChartGantt.AddRows(const taskID, taskName, resource: string;
  const startOf, endOf: TDate; const duration, percentComplete: Double;
  const dependencies: string): IPGChartGantt;
begin
  SetLength(FRows, Length(FRows) + 1);
  FRows[High(FRows)].TaskID := taskID;
  FRows[High(FRows)].TaskName := taskName;
  FRows[High(FRows)].Resource := resource;
  FRows[High(FRows)].StartOf := startOf;
  FRows[High(FRows)].EndOf := endOf;
  FRows[High(FRows)].Duration := duration;
  FRows[High(FRows)].PercentComplete := percentComplete;
  FRows[High(FRows)].Dependencies := dependencies;
end;

function TPGChartGantt.GenerateChartCode: string;
begin
  Result :=
    '<html>' + sLineBreak +
    '<head>' + sLineBreak +
    '<script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>' + sLineBreak +
    '<script type="text/javascript">' + sLineBreak +
    'google.charts.load(''current'', {''packages'':[''gantt'']});' + sLineBreak +
    'google.charts.setOnLoadCallback(drawChart);' + sLineBreak +
    '' + sLineBreak +
    'function toMilliseconds(minutes) {' + sLineBreak +
    'return minutes * 60 * 1000;' + sLineBreak +
    '}' + sLineBreak +
    '' + sLineBreak +
    'function drawChart() {' + sLineBreak +
    '' + sLineBreak +
    'var col1 = new Object();' + sLineBreak +
    'col1.type = ''number'';' + sLineBreak +
    'col1.label = ''Percentual Completado'';' + sLineBreak +
    '' + sLineBreak +
    'var otherData = new google.visualization.DataTable();' + sLineBreak +
    'otherData.addColumn(''string'', ''ID'');' + sLineBreak +
    'otherData.addColumn(''string'', ''Tarefa'');' + sLineBreak +
    'otherData.addColumn(''string'', ''Recurso'');' + sLineBreak +
    'otherData.addColumn(''date'', ''Início'');' + sLineBreak +
    'otherData.addColumn(''date'', ''Fim'');' + sLineBreak +
    'otherData.addColumn(''number'', ''Duração'');' + sLineBreak +
    'otherData.addColumn(col1);' + sLineBreak +
    'otherData.addColumn(''string'', ''Dependencias'');' + sLineBreak +
    '' + sLineBreak +
    'otherData.addRows([' + sLineBreak +
    '[''atividade01'', ''Atividade 01'', ''Maquina 1'', null, null, toMilliseconds(35), 100, null],' + sLineBreak +
    '[''atividade02'', ''Atividade 02'', ''Maquina 2'', null, null, toMilliseconds(55), 100, null],' + sLineBreak +
    '[''atividade03'', ''Atividade 03'', ''Maquina 3'', null, null, toMilliseconds(20), 100, ''atividade02''],' + sLineBreak +
    '[''atividade04'', ''Atividade 04'', ''Maquina 4'', null, null, toMilliseconds(35), 75, ''atividade03''],' + sLineBreak +
    '[''atividade05'', ''Atividade 05'', ''Maquina 5'', null, null, toMilliseconds(30), 5, null],' + sLineBreak +
    '[''atividade06'', ''Atividade 06'', ''Maquina 5'', null, null, toMilliseconds(2), 2, ''atividade05''],' + sLineBreak +
    '' + sLineBreak +
    ']);' + sLineBreak +
    '' + sLineBreak +
    'var options = {' + sLineBreak +
    'height: 275,' + sLineBreak +
    'gantt: {' + sLineBreak +
    'defaultStartDateMillis: new Date(2015, 3, 28)' + sLineBreak +
    '}' + sLineBreak +
    '};' + sLineBreak +
    '' + sLineBreak +
    'var chart = new google.visualization.Gantt(document.getElementById(''chart_div''));' + sLineBreak +
    '' + sLineBreak +
    'chart.draw(otherData, options);' + sLineBreak +
    '}' + sLineBreak +
    '</script>' + sLineBreak +
    '</head>' + sLineBreak +
    '<body>' + sLineBreak +
    '<div id="chart_div"></div>' + sLineBreak +
    '</body>' + sLineBreak +
    '</html>';
end;

procedure TPGChartGantt.Show(const webBrowser: TWebBrowser);
begin

end;

procedure TPGChartGantt.Show;
var
  view: TpgchartView;
  stream: TStringStream;
begin
  stream := TStringStream.Create(GenerateChartCode);
  view := TpgchartView.Create(stream);
  try
    view.ShowModal;
  finally
    view.Free;
  end;
end;

end.
