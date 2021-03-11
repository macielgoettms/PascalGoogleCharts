unit pgchart.view;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw, System.Win.Registry;

type
  TpgchartView = class(TForm)
    WebBrowser: TWebBrowser;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FStream: TStringStream;
  public
    { Public declarations }
    constructor Create(const stream: TStringStream); reintroduce;
  end;

var
  pgchartView: TpgchartView;

implementation

uses
  pgchart,
  pgchart.interfaces,
  webBrowser.Helper;

procedure configureIE11onRegister;
const
  homePath = 'SOFTWARE';
  featureBrowserEmulation =
    'Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION\';
  IE11 = 11001;
var
  registerFile: TRegIniFile;
  key: string;
begin
  key := ExtractFileName(ParamStr(0));
  registerFile := TRegIniFile.Create(homePath);
  try
    if (registerFile.OpenKey(featureBrowserEmulation, True)) and
      not((TRegistry(registerFile).KeyExists(key)) and
      (TRegistry(registerFile).ReadInteger(key) = IE11)) then
      TRegistry(registerFile).WriteInteger(key, IE11);
  finally
    registerFile.Free;
  end;
end;

{$R *.dfm}

constructor TpgchartView.Create(const stream: TStringStream);
begin
  inherited Create(nil);
  FStream := stream;
end;

procedure TpgchartView.FormShow(Sender: TObject);
begin
  if Assigned(FStream) then
    WebBrowser.NavigateFromStream(FStream);
end;

initialization

configureIE11onRegister;

end.
