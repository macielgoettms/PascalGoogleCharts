unit pgchart.line;

interface

uses
  SHDocVw,
  System.Rtti,
  System.SysUtils,
  pgchart.functions,
  pgchart.enumerations,
  pgchart.interfaces;

type
  TPGChartline = class(TInterfacedObject, IPGChartLine)
  private
    FOptions: array of TPGChartOption;
    FColumnsCode: string;
    FRowsCode: string;

    function GetChartCode: string;
    function GetOptionsCode: string;
  public
    function addColumn(
      const typeValue: TPGTypeColumn;
      const value: TValue): IPGChartLine;

    function AddRow(
      const values: array of TValue): IPGChartLine;

    function AddOption(
      const name: string;
      const value: string): IPGChartLine;

    procedure Show; overload;
    procedure Show(
      const webBrowser: TWebBrowser); overload;
  end;

implementation

{ TPGChartline }

function TPGChartline.addColumn(const typeValue: TPGTypeColumn;
  const value: TValue): IPGChartLine;
begin
  if not FColumnsCode.IsEmpty then
    FColumnsCode := FColumnsCode + sLineBreak;

  FColumnsCode := FColumnsCode + '      data.addColumn(' +
    QuotedStr(TYPE_COLUMN[typeValue]) + ',' + QuotedStr(value.ToString) + ');';

  Result := Self;
end;

function TPGChartline.AddOption(const name, value: string): IPGChartLine;
begin
  SetLength(FOptions, Length(FOptions) + 1);
  FOptions[High(FOptions)].Name := name;
  FOptions[High(FOptions)].Value := value;
  Result := Self;
end;

function TPGChartline.AddRow(const values: array of TValue): IPGChartLine;
var
  value: TValue;
  valueCode: string;
begin
  FormatSettings.DecimalSeparator := '.';
  try
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

    if FRowsCode.IsEmpty then
      FRowsCode := FRowsCode + valueCode
    else
      FRowsCode := FRowsCode + ',' + sLIneBreak + valueCode;
  finally
    FormatSettings.DecimalSeparator := ',';
  end;
  Result := Self;
end;

function TPGChartline.GetChartCode: string;
const
  defaultCode =
    '<html>' + sLineBreak +
    '<head>' + sLineBreak +
    '  <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>' + sLineBreak +
    '    <script type="text/javascript">' + sLineBreak +
    '      google.charts.load(''current'', {''packages'':[''line'']});' + sLineBreak +
    '      google.charts.setOnLoadCallback(drawChart);' + sLineBreak +
    '' + sLineBreak +
    '    function drawChart() {' + sLineBreak +
    '' + sLineBreak +
    '      var data = new google.visualization.DataTable();' + sLineBreak +
    '      %S' + sLineBreak +
    '' + sLineBreak +
    '      data.addRows([' + sLineBreak +
    '        %S' + sLineBreak +
    '      ]);' + sLineBreak +
    '' + sLineBreak +
    '        %S' + sLineBreak +
    '' + sLineBreak +
    '      var chart = new google.charts.Line(document.getElementById(''line_top_x''));' + sLineBreak +
    '' + sLineBreak +
    '      chart.draw(data, google.charts.Line.convertOptions(options));' + sLineBreak +
    '    }' + sLineBreak +
    '  </script>' + sLineBreak +
    '</head>' + sLineBreak +
    '<body>' + sLineBreak +
    '  <div id="line_top_x"></div>' + sLineBreak +
    '</body>' + sLineBreak +
    '</html>';
begin
  Result := Format(defaultCode, [FColumnsCode, FRowsCode, GetOptionsCode]);
end;

function TPGChartline.GetOptionsCode: string;
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

procedure TPGChartline.Show(const webBrowser: TWebBrowser);
begin
  ShowChart(webBrowser, GetChartCode);
end;

procedure TPGChartline.Show;
begin
  ShowChart(GetChartCode);
end;

end.
