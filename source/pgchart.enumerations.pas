unit pgchart.enumerations;

interface

type
  TPGTypeColumn = (tcString, tcNumber, tcboolean, tcdate, tcdatetime, tctimeofday);

  TPGChartOption = record
    Name: string;
    Value: string;
  end;

  TPGChartGanttRow = record
    TaskID: string;
    TaskName: string;
    Resource: string;
    StartOf: string;
    EndOf: string;
    Duration: Double;
    PercentComplete: Double;
    Dependencies: string;
  end;

implementation

end.
