unit pgchart.bar;

interface

uses
  SHDocVw,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.RTTI,
  pgchart.interfaces,
  pgchart.enumerations,
  pgchart.functions;

type
  TPGChartBar = class(TInterfacedObject, IPGChartBar)
  private
    FLabels: array of string;
    FOptions: array of TPGChartOption;
    FLabelsCode: string;
    FValuesCode: string;

    function GetChartCode: string;
    function GetOptionsCode: string;
  public
    function AddLabels(
      const labels: array of string): IPGChartBar;

    function AddValues(
      const values: array of TValue): IPGChartBar;

    function AddOption(
      const name: string;
      const value: string): IPGChartBar;

    procedure Show; overload;
    procedure Show(
      const webBrowser: TWebBrowser); overload;
  end;

implementation

{ TPGChartBar }

function TPGChartBar.AddLabels(const labels: array of string): IPGChartBar;
var
  _label: string;
begin
  for _label in labels do
  begin
    SetLength(FLabels, Length(FLabels) + 1);
    FLabels[High(FLabels)] := _label;

    if FLabelsCode.IsEmpty then
      FLabelsCode := '['
    else
      FLabelsCode := FLabelsCode + ',';
    FLabelsCode := FLabelsCode + QuotedStr(_label);
  end;
  FLabelsCode := FLabelsCode + ']';
  Result := Self;
end;

function TPGChartBar.AddOption(const name, value: string): IPGChartBar;
begin
  SetLength(FOptions, Length(FOptions) + 1);
  FOptions[High(FOptions)].Name := name;
  FOptions[High(FOptions)].Value := value;
  Result := Self;
end;

function TPGChartBar.AddValues(const values: array of TValue): IPGChartBar;
var
  value: TValue;
  valueCode: string;
begin
  for value in values do
  begin
    if valueCode.IsEmpty then
      valueCode := '['
    else
      valueCode := valueCode + ',';

    if value.Kind in [tkString, tkUString] then
      valueCode := valueCode + QuotedStr(value.ToString)
    else
      valueCode := valueCode + value.ToString;
  end;
  valueCode := valueCode + ']';

  if FValuesCode.IsEmpty then
    FValuesCode := FValuesCode + valueCode
  else
    FValuesCode := FValuesCode + ',' + sLIneBreak + valueCode;
  Result := Self;
end;

function TPGChartBar.GetChartCode: string;
const
  defaultCode =
    '<html>' + sLineBreak +
    '  <head>' + sLineBreak +
    '    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>' + sLineBreak +
    '    <script type="text/javascript">' + sLineBreak +
    '      google.charts.load(''current'', {''packages'':[''bar'']});' + sLineBreak +
    '      google.charts.setOnLoadCallback(drawChart);' + sLineBreak +
    '' + sLineBreak +
    '      function drawChart() {' + sLineBreak +
    '        var data = google.visualization.arrayToDataTable([' + sLineBreak +
    '          %S,' + sLineBreak +
    '          %S' + sLineBreak +
    '        ]);' + sLineBreak +
    '' + sLineBreak +
    '        %S' + sLineBreak +
    '' + sLineBreak +
    '        var chart = new google.charts.Bar(document.getElementById(''chart_div''));' + sLineBreak +
    '' + sLineBreak +
    '        chart.draw(data, google.charts.Bar.convertOptions(options));' + sLineBreak +
    '' + sLineBreak +
    '' + sLineBreak +
    '      }' + sLineBreak +
    '    </script>' + sLineBreak +
    '  </head>' + sLineBreak +
    ' <body>' + sLineBreak +
    '   <div id="chart_div"></div>' + sLineBreak +
    ' </body>' + sLineBreak +
    '</html>';
begin
  Result := Format(defaultCode, [FLabelsCode, FValuesCode, GetOptionsCode]);
end;

function TPGChartBar.GetOptionsCode: string;
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

procedure TPGChartBar.Show(const webBrowser: TWebBrowser);
begin
  ShowChart(webBrowser, GetChartCode);
end;

procedure TPGChartBar.Show;
begin
  ShowChart(GetChartCode);
end;

end.
