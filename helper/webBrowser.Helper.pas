unit webBrowser.Helper;

interface

uses
  SHDocVw,
  Vcl.Forms,
  System.SysUtils,
  System.Classes,
  Winapi.ActiveX;

type

  TWebBrowserHelper = class helper for TWebBrowser
    procedure NavigateFromStream(const stream: TStream);
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

end.
