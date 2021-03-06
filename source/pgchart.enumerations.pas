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

const
  TYPE_COLUMN: array [TPGTypeColumn] of string = ('string ', 'number', 'boolean', 'date', 'datetime', 'timeofday');

implementation

end.
