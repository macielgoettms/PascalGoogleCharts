unit pgchart.gantt;

interface

uses
  SHDocVw,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  pgchart.interfaces,
  pgchart.enumerations,
  pgchart.functions;

type
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
  ShowChart(webBrowser, GetChartCode);
end;

procedure TPGChartGantt.Show;
begin
  ShowChart(GetChartCode);
end;

end.
