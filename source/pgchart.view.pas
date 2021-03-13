unit pgchart.view;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw;

type
  TPGChartView = class(TForm)
    WebBrowser: TWebBrowser;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FStream: TStringStream;
  public
    { Public declarations }
    constructor Create(const stream: TStringStream); reintroduce;
  end;

implementation

uses


  webBrowser.Helper;


{$R *.dfm}

constructor TPGChartView.Create(const stream: TStringStream);
begin
  inherited Create(nil);
  FStream := stream;
end;

procedure TPGChartView.FormShow(Sender: TObject);
begin
  if Assigned(FStream) then
  begin
    WebBrowser.ConfigureIE11onRegister;
    WebBrowser.NavigateFromStream(FStream);
  end;
end;

end.
