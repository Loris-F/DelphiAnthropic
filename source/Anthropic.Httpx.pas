unit Anthropic.Httpx;

interface

uses
  System.SysUtils, System.Classes, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, System.NetEncoding;

type
  THttpx = class
    class function StreamToBytes(AStream: TStream): TBytes;
    class function LoadDataToBase64(const Url: string; var MimeType: string): string;
    class function GetMimeType(const Url: string): string;
  end;

implementation

{ THttpx }

class function THttpx.GetMimeType(const Url: string): string;
begin
  var HttpClient := THTTPClient.Create;
  try
    Result := (HttpClient.Head(Url) as IHTTPResponse).MimeType;
  finally
    HttpClient.Free;
  end;
end;

class function THttpx.LoadDataToBase64(const Url: string; var MimeType: string): string;
begin
  var HttpClient := THTTPClient.Create;
  try
    var Response: IHTTPResponse := HttpClient.Get(Url);
    var ImageBytes := StreamToBytes(Response.ContentStream);
    {$IF RTLVersion >= 35.0}
    Result := TNetEncoding.Base64String.EncodeBytesToString(ImageBytes);
    {$ELSE}
    Result := TNetEncoding.Base64.EncodeBytesToString(ImageBytes);
    {$ENDIF}
    MimeType := GetMimeType(Url);
  finally
    HttpClient.Free;
  end;
end;

class function THttpx.StreamToBytes(AStream: TStream): TBytes;
var
  LBytesStream: TBytesStream;
begin
  if not Assigned(AStream) then
    Exit(nil);
  LBytesStream := TBytesStream.Create;
  try
    AStream.Position := 0;
    LBytesStream.CopyFrom(AStream, AStream.Size);
    Result := LBytesStream.Bytes;
    SetLength(Result, LBytesStream.Size);
  finally
    LBytesStream.Free;
  end;
end;

end.
