unit pgchart;

interface

uses
  pgchart.interfaces,
  pgchart.pie,
  pgchart.gantt,
  pgchart.line,
  pgchart.bar;

type
  TPGChart = class(TInterfacedObject, IPGChart)
  private
  class var
    FInstance: IPGChart;
    FDonut: IPGChartPie;
    FGantt: IPGChartGantt;
    FBar: IPGChartBar;
    FPie: IPGChartPie;
    FLine: IPGChartLine;
    constructor Create;
  public
    class function New: IPGChart;
    function Donut: IPGChartPie;
    function Gantt: IPGChartGantt;
    function Bar: IPGChartBar;
    function Pie: IPGChartPie;
    function Line: IPGChartLine;
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

function TPGChart.Donut: IPGChartPie;
begin
  if not Assigned(FDonut) then
  begin
    FDonut := TPGChartPie.Create;
    FDonut.AddOption('pieHole', '0.4');
  end;
  Result := FDonut;
end;

function TPGChart.Gantt: IPGChartGantt;
begin
  if not Assigned(FGantt) then
    FGantt := TPGChartGantt.Create;
  Result := FGantt;
end;

function TPGChart.Line: IPGChartLine;
begin
  if not Assigned(FLine) then
    FLine := TPGChartLine.Create;
  Result := FLine;
end;

class function TPGChart.New: IPGChart;
begin
  if not Assigned(FInstance) then
    FInstance := TPGChart.Create;

  Result := Self.FInstance;
end;

function TPGChart.Pie: IPGChartPie;
begin
  if not Assigned(FPie) then
    FPie := TPGChartPie.Create;
  Result := FPie;
end;

end.
