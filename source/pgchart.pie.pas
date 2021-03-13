unit pgchart.pie;

interface

uses
  SHDocVw,
  System.SysUtils,
  pgchart.enumerations,
  pgchart.functions,
  pgchart.interfaces;

type
  TPGChartPie = class(TInterfacedObject, IPGChartPie)
  private
    FOptions: array of TPGChartOption;
    FValuesCode: string;

    function GetChartCode: string;
    function GetOptionsCode: string;
  public
    function AddValue(
      const description: string;
      const value: Double): IPGChartPie;

    function AddOption(
      const name: string;
      const value: string): IPGChartPie;

    procedure Show; overload;
    procedure Show(
      const webBrowser: TWebBrowser); overload;
  end;

implementation

{ TPGChartPie }

function TPGChartPie.AddOption(const name, value: string): IPGChartPie;
begin
  SetLength(FOptions, Length(FOptions) + 1);
  FOptions[High(FOptions)].Name := name;
  FOptions[High(FOptions)].Value := value;
  Result := Self;
end;

function TPGChartPie.AddValue(const description: string; const value: Double): IPGChartPie;
begin
  if not FValuesCode.IsEmpty then
    FValuesCode := FValuesCode + ',';
  FValuesCode := FValuesCode + '[' + QuotedStr(description) + ',' + value.ToString + ']';
  Result := Self;
end;

function TPGChartPie.GetChartCode: string;
const
  defaultCode =
    '<html>' + sLineBreak +
    '  <head>' + sLineBreak +
    '    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>' + sLineBreak +
    '    <script type="text/javascript">' + sLineBreak +
    '      google.charts.load(''current'', {''packages'':[''corechart'']});' + sLineBreak +
    '      google.charts.setOnLoadCallback(drawChart);' + sLineBreak +
    '' + sLineBreak +
    '      function drawChart() {' + sLineBreak +
    '' + sLineBreak +
    '        var data = google.visualization.arrayToDataTable([' + sLineBreak +
    '          [''Label'', ''Value''],' + sLineBreak +
    '          %S' + sLineBreak +
    '        ]);' + sLineBreak +
    '' + sLineBreak +
    '          %S' + sLineBreak +
    '' + sLineBreak +
    '        var chart = new google.visualization.PieChart(document.getElementById(''piechart''));' + sLineBreak +
    '' + sLineBreak +
    '        chart.draw(data, options);' + sLineBreak +
    '      }' + sLineBreak +
    '</script>' + sLineBreak +
    '</head>' + sLineBreak +
    '<body>' + sLineBreak +
    '<div id="piechart" style="width: 900px; height: 500px;"></div>' + sLineBreak +
    '</body>' + sLineBreak +
    '</html>';
begin
  Result := Format(defaultCode, [FValuesCode, GetOptionsCode]);
end;

function TPGChartPie.GetOptionsCode: string;
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

procedure TPGChartPie.Show(const webBrowser: TWebBrowser);
begin
  ShowChart(webBrowser, GetChartCode);
end;

procedure TPGChartPie.Show;
begin
  ShowChart(GetChartCode);
end;

end.
