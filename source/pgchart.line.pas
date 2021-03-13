unit pgchart.line;

interface

uses
  SHDocVw,
  pgchart.functions,
  pgchart.interfaces;

type
  TPGChartline = class(TInterfacedObject, IPGChartLine)
  private
    function GetChartCode: string;
  public

    procedure Show; overload;
    procedure Show(
      const webBrowser: TWebBrowser); overload;
  end;

implementation

{ TPGChartline }

function TPGChartline.GetChartCode: string;
const
 defaultCode =
 '<html>'+sLineBreak+
 '  <head>'+sLineBreak+
 '    <script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>'+sLineBreak+
 '    <script type="text/javascript">'+sLineBreak+
 '      google.charts.load(''current'', {''packages'':[''corechart'']});'+sLineBreak+
 '      google.charts.setOnLoadCallback(drawChart);'+sLineBreak+
 ''+sLineBreak+
 '      function drawChart() {'+sLineBreak+
 ''+sLineBreak+
 '        var data = google.visualization.arrayToDataTable(['+sLineBreak+
 '          [''Task'', ''Hours per Day''],'+sLineBreak+
 '          [''Work'',     11],'+sLineBreak+
 '          [''Eat'',      2],'+sLineBreak+
 '          [''Commute'',  2],'+sLineBreak+
 '          [''Watch TV'', 2],'+sLineBreak+
 '          [''Sleep'',    7]'+sLineBreak+
 '        ]);'+sLineBreak+
 ''+sLineBreak+
 '        var options = {'+sLineBreak+
 '          title: ''My Daily Activities'''+sLineBreak+
 '        };'+sLineBreak+
 ''+sLineBreak+
 '        var chart = new google.visualization.PieChart(document.getElementById(''piechart''));'+sLineBreak+
 ''+sLineBreak+
 '        chart.draw(data, options);'+sLineBreak+
 '      }'+sLineBreak+
 '</script>'+sLineBreak+
 '</head>'+sLineBreak+
 '<body>'+sLineBreak+
 '<div id="piechart" style="width: 900px; height: 500px;"></div>'+sLineBreak+
 '</body>'+sLineBreak+
 '</html>';
begin

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
