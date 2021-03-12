unit pgchart;

interface

uses
  SHDocVw,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.RTTI,
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
    FBar: IPGChartBar;
    constructor Create;
  public
    class function New: IPGChart;
    function Gantt: IPGChartGantt;
    function Bar: IPGChartBar;
  end;

  TPGChartBar = class(TInterfacedObject, IPGChartBar)
  private
    FLabels: array of string;
    FValues: array of array of string;
    FOptions: array of TPGChartOption;

    function GetChartCode: string;
    function GetLabelsCode: string;
    function GetValuesCode: string;
    function GetOptionsCode: string;
  public
    function AddLabels(
      const labels: array of string): IPGChartBar;

    function AddValues(
      const labels: array of string): IPGChartBar;

    function AddOption(
      const name: string;
      const value: string): IPGChartBar;

    procedure Show; overload;
    procedure Show(
      const webBrowser: TWebBrowser); overload;
  end;

  TPGChartGantt = class(TInterfacedObject, IPGChartGantt)
  private
    FRows: array of TPGChartGanttRow;
    FOptions: array of TPGChartOption;

    function GetChartCode: string;
    function GetRowsCode: string;
    function GetOptionsCode: string;
  public
    function AddRow(
      const taskID: string;
      const taskName: string;
      const resource: string;
      const startOf: TDateTime;
      const endOf: TDateTime;
      const duration: Double;
      const percentComplete: Double;
      const dependencies: string): IPGChartGantt;

    function AddOption(
      const name: string;
      const value: string): IPGChartGantt;

    procedure Show; overload;
    procedure Show(
      const webBrowser: TWebBrowser); overload;
  end;

implementation

{ TPGChart }

function TPGChart.Bar: IPGChartBar;
begin
  if not Assigned(FBar) then
    FBar := TPGChartBar.Create;
  Result := FBar;
end;

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

function TPGChartGantt.AddOption(const name, value: string): IPGChartGantt;
begin
  SetLength(FOptions, Length(FOptions) + 1);
  FOptions[High(FOptions)].Name := name;
  FOptions[High(FOptions)].Value := value;
  Result := Self;
end;

function TPGChartGantt.AddRow(const taskID, taskName, resource: string;
  const startOf, endOf: TDateTime; const duration, percentComplete: Double;
  const dependencies: string): IPGChartGantt;

  function FormatedString(const value: String): string;
  begin
    if value.IsEmpty then
      Result := 'null'
    else
      Result := QuotedStr(value);
  end;

  function FormatedDateIso(const value: TDateTime): string;
  begin
    if value > 0 then
      Result := 'new Date("' + DateToISO8601(value, false) + '")'
    else
      Result := 'null';
  end;

begin
  SetLength(FRows, Length(FRows) + 1);
  FRows[High(FRows)].TaskID := FormatedString(taskID);
  FRows[High(FRows)].TaskName := FormatedString(taskName);
  FRows[High(FRows)].Resource := FormatedString(resource);
  FRows[High(FRows)].StartOf := FormatedDateIso(startOf);
  FRows[High(FRows)].EndOf := FormatedDateIso(endOf);
  FRows[High(FRows)].Duration := duration;
  FRows[High(FRows)].PercentComplete := percentComplete;
  FRows[High(FRows)].Dependencies := FormatedString(dependencies);
  Result := Self;
end;

function TPGChartGantt.GetChartCode: string;
const
  defaultCode =
    '<html>' + sLineBreak +
    ' <head>' + sLineBreak +
    '   <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>' + sLineBreak +
    '   <script type="text/javascript">' + sLineBreak +
    '     google.charts.load(''current'', {''packages'':[''gantt'']});' + sLineBreak +
    '     google.charts.setOnLoadCallback(drawChart);' + sLineBreak +
    '' + sLineBreak +
    '     function toMilliseconds(minutes) {' + sLineBreak +
    '       return minutes * 60 * 1000;' + sLineBreak +
    '     }' + sLineBreak +
    '' + sLineBreak +
    '     function drawChart() {' + sLineBreak +
    '' + sLineBreak +
    '       var chartData = new google.visualization.DataTable();' + sLineBreak +
    '           chartData.addColumn(''string'', ''Id'');' + sLineBreak +
    '           chartData.addColumn(''string'', ''Tarefa'');' + sLineBreak +
    '           chartData.addColumn(''string'', ''Recurso'');' + sLineBreak +
    '           chartData.addColumn(''date'', ''Início'');' + sLineBreak +
    '           chartData.addColumn(''date'', ''Fim'');' + sLineBreak +
    '           chartData.addColumn(''number'', ''Duração'');' + sLineBreak +
    '           chartData.addColumn(''number'', ''Percentual Completado'');' + sLineBreak +
    '           chartData.addColumn(''string'', ''Dependencias'');' + sLineBreak +
    '' + sLineBreak +
    '%S' + sLineBreak +
    '' + sLineBreak +
    '%S' + sLineBreak +
    '        var chart = new google.visualization.Gantt(document.getElementById(''chart_div''));' + sLineBreak +
    '' + sLineBreak +
    '           chart.draw(chartData, options);' + sLineBreak +
    '     }' + sLineBreak +
    '    </script>' + sLineBreak +
    '  </head>' + sLineBreak +
    ' <body>' + sLineBreak +
    '   <div id="chart_div"></div>' + sLineBreak +
    ' </body>' + sLineBreak +
    '</html>';
begin
  Result := Format(defaultCode, [GetRowsCode, GetOptionsCode]);
end;

function TPGChartGantt.GetOptionsCode: string;
var
  option: TPGChartOption;
begin
  for option in FOptions do
  begin
    if Result.IsEmpty then
      Result := '        var options = {' + sLineBreak
    else
      Result := Result + ',' + sLineBreak;

    Result := Result + Format('            %S: %S', [option.Name, option.Value]);
  end;
  if not Result.IsEmpty then
    Result := Result + '        };' + sLineBreak;
end;

function TPGChartGantt.GetRowsCode: string;
var
  row: TPGChartGanttRow;
begin
  FormatSettings.DecimalSeparator := '.';
  try
    for row in FRows do
    begin
      if Result.IsEmpty then
        Result := '       chartData.addRows([' + sLineBreak
      else
        Result := Result + ',' + sLineBreak;

      Result := Result + Format('         [%S, %S, %S, %S, %S, %F, %F, %S]',
        [row.TaskID, row.TaskName, row.Resource, row.StartOf, row.EndOf, row.Duration, row.PercentComplete, row.Dependencies],
        FormatSettings);
    end;
    if not Result.IsEmpty then
      Result := Result + '       ]);' + sLineBreak;
  finally
    FormatSettings.DecimalSeparator := ',';
  end;
end;

procedure TPGChartGantt.Show(const webBrowser: TWebBrowser);
begin

end;

procedure TPGChartGantt.Show;
var
  view: TpgchartView;
  stream: TStringStream;
begin
  stream := TStringStream.Create(GetChartCode);
  view := TpgchartView.Create(stream);
  try
    view.ShowModal;
  finally
    view.Free;
  end;
end;

{ TPGChartBar }

function TPGChartBar.AddLabels(const labels: array of string): IPGChartBar;
var
  _label: string;
begin
  for _label in labels do
  begin
    SetLength(FLabels, Length(FLabels) + 1);
    FLabels[High(FLabels)] := _label;
  end;
  Result := Self;
end;

function TPGChartBar.AddOption(const name, value: string): IPGChartBar;
begin
  SetLength(FOptions, Length(FOptions) + 1);
  FOptions[High(FOptions)].Name := name;
  FOptions[High(FOptions)].Value := value;
  Result := Self;
end;

function TPGChartBar.AddValues(const labels: array of string): IPGChartBar;
var
  _label: string;
begin
  SetLength(FValues, Length(FValues) + 1);

  for _label in labels do
  begin
    SetLength(FValues[High(FValues)], Length(FValues[High(FValues)]) + 1);
    FValues[High(FValues), High(FValues[High(FValues)])] := _label;
  end;
  Result := Self;
end;

function TPGChartBar.GetChartCode: string;
const
  defaultCode =
    '<html>' + sLineBreak +
    '  <head>' + sLineBreak +
    '    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>' + sLineBreak +
    '    <script type="text/javascript">' + sLineBreak +
    'google.charts.load(''current'', {''packages'':[''bar'']});' + sLineBreak +
    'google.charts.setOnLoadCallback(drawChart);' + sLineBreak +
    '' + sLineBreak +
    'function drawChart() {' + sLineBreak +
    'var data = google.visualization.arrayToDataTable([' + sLineBreak +
    '[''Ano'', ''Vendas'', ''Despesas'', ''Lucro''],' + sLineBreak +
    '[''2014'', 1000, 400, 200],' + sLineBreak +
    '[''2015'', 1170, 460, 250],' + sLineBreak +
    '[''2016'', 660, 1120, 300],' + sLineBreak +
    '[''2017'', 1030, 540, 350]' + sLineBreak +
    ']);' + sLineBreak +
    '' + sLineBreak +
    'var options = {' + sLineBreak +
    'chart: {' + sLineBreak +
    'title: ''Performance da Empresa'',' + sLineBreak +
    'subtitle: ''Vendas, Despesas e Lucro: 2014-2017'',' + sLineBreak +
    '},' + sLineBreak +
    'bars: ''vertical'', // Required for Material Bar Charts.' + sLineBreak +
    'hAxis: {format: ''short''},' + sLineBreak +
    'height: 400,' + sLineBreak +
    'colors: [''#1b9e77'', ''#d95f02'', ''#7570b3'']' + sLineBreak +
    '};' + sLineBreak +
    '' + sLineBreak +
    'var chart = new google.charts.Bar(document.getElementById(''chart_div''));' + sLineBreak +
    '' + sLineBreak +
    'chart.draw(data, google.charts.Bar.convertOptions(options));' + sLineBreak +
    '' + sLineBreak +
    '' + sLineBreak +
    '}' + sLineBreak +
    '    </script>' + sLineBreak +
    '  </head>' + sLineBreak +
    ' <body>' + sLineBreak +
    '   <div id="chart_div"></div>' + sLineBreak +
    ' </body>' + sLineBreak +
    '</html>';
begin
  Result := defaultCode;
end;

function TPGChartBar.GetLabelsCode: string;
begin

end;

function TPGChartBar.GetOptionsCode: string;
begin

end;

function TPGChartBar.GetValuesCode: string;
begin

end;

procedure TPGChartBar.Show(const webBrowser: TWebBrowser);
begin

end;

procedure TPGChartBar.Show;
var
  view: TpgchartView;
  stream: TStringStream;
begin
  stream := TStringStream.Create(GetChartCode);
  view := TpgchartView.Create(stream);
  try
    view.ShowModal;
  finally
    view.Free;
  end;
end;

end.
