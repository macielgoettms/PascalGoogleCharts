unit pgchart.interfaces;

interface

uses
  SHDocVw,
  System.Rtti;

type
  IPGChartPie = interface
    ['{A013900E-9DD9-411F-A9A7-9624DDE9E165}']
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

  IPGChartBar = interface
    ['{25114782-D182-4A14-94DA-84E5FB1B90F8}']
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

  IPGChartLine = interface
    ['{90548DA9-5A93-40D4-B3CE-F3542C078D6D}']
  end;

  IPGChartGantt = interface
    ['{71330023-57FF-4F1B-B4EB-D98B2A696807}']

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

  IPGChart = interface
    ['{199A7C45-A6C6-4D5F-89FF-AE0E85CA9A85}']
    function Donut: IPGChartPie;
    function Gantt: IPGChartGantt;
    function Bar: IPGChartBar;
    function Pie: IPGChartPie;
  end;

implementation

end.
