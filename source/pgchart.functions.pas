unit pgchart.functions;

interface

uses
  SHDocVw,
  System.Classes,
  pgchart.view,
  webBrowser.helper;

procedure ShowChart(const chartCode: string); overload;
procedure ShowChart(const webBrowser: TWebBrowser; chartCode: string); overload;

implementation

procedure ShowChart(const chartCode: string); overload;
var
  view: TpgchartView;
  stream: TStringStream;
begin
  stream := TStringStream.Create(chartCode);
  view := TpgchartView.Create(stream);
  try
    view.ShowModal;
  finally
    view.Free;
    stream.Free;
  end;
end;

procedure ShowChart(const webBrowser: TWebBrowser; chartCode: string); overload;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create(chartCode);
  try
    webBrowser.ConfigureIE11onRegister;
    webBrowser.NavigateFromStream(stream);
  finally
    stream.Free;
  end;
end;

end.
