unit webBrowser.helper;

interface

uses
  SHDocVw,
  Vcl.Forms,
  System.SysUtils,
  System.Classes,
  System.Win.Registry,
  Winapi.ActiveX;

type

  TWebBrowserHelper = class helper for TWebBrowser
    procedure NavigateFromStream(const stream: TStream);
    procedure ConfigureIE11onRegister;
  end;

implementation

{ TWebBrowserHelper }

procedure TWebBrowserHelper.NavigateFromStream(const stream: TStream);
var
  persistStreamInit: IPersistStreamInit;
  streamAdapter: IStream;
  memoryStream: TMemoryStream;
begin
  Self.Navigate('about:blank');
  repeat
    Vcl.Forms.Application.ProcessMessages;
    Sleep(0);
  until Self.ReadyState = READYSTATE_COMPLETE;

  if Self.Document.QueryInterface(IPersistStreamInit, persistStreamInit) = S_OK then
  begin
    if persistStreamInit.InitNew = S_OK then
    begin
      memoryStream := TMemoryStream.Create;
      try
        memoryStream.CopyFrom(Stream, 0);
        memoryStream.Position := 0;
      except
        memoryStream.Free;
        raise;
      end;
      streamAdapter := TStreamAdapter.Create(memoryStream, soOwned);
      persistStreamInit.Load(streamAdapter);
    end;
  end;
end;

procedure TWebBrowserHelper.ConfigureIE11onRegister;
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

end.
