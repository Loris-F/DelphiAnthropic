unit Anthropic.API;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Net.URLClient,
  System.Net.Mime, System.JSON, Anthropic.API.Params, Anthropic.Errors;

type
  AnthropicException = class(Exception)
  private
    FCode: Int64;
    FMsg: string;
    FType: string;
  public
    constructor Create(const ACode: Int64; const AError: TErrorCore); reintroduce; overload;
    constructor Create(const ACode: Int64; const Value: string); reintroduce; overload;
    property Code: Int64 read FCode write FCode;
    property &Type: string read FType write FType;
    property Msg: string read FMsg write FMsg;
  end;

  /// <summary>
  /// The `AnthropicExceptionAPI` class represents a generic API-related exception.
  /// It is thrown when there is an issue with the API configuration or request process,
  /// such as a missing API token, invalid base URL, or other configuration errors.
  /// This class serves as a base for more specific API exceptions.
  /// </summary>
  AnthropicExceptionAPI = class(Exception);

  /// <summary>
  /// An InvalidRequestError indicates that your request was malformed or
  /// missing some required parameters, such as a token or an input.
  /// This could be due to a typo, a formatting error, or a logic error in your code.
  /// </summary>
  AnthropicExceptionInvalidRequestError = class(AnthropicException);

  /// <summary>
  /// A `RateLimitError` indicates that you have hit your assigned rate limit.
  /// This means that you have sent too many tokens or requests in a given period of time,
  /// and our services have temporarily blocked you from sending more.
  /// </summary>
  AnthropicExceptionRateLimitError = class(AnthropicException);

  /// <summary>
  /// An `AuthenticationError` indicates that your API key or token was invalid,
  /// expired, or revoked. This could be due to a typo, a formatting error, or a security breach.
  /// </summary>
  AnthropicExceptionAuthenticationError = class(AnthropicException);

  /// <summary>
  /// This error message indicates that your account is not part of an organization
  /// </summary>
  AnthropicExceptionPermissionError = class(AnthropicException);

  /// <summary>
  /// An `InvalidResponse` error occurs when the API response is either empty or not in the expected format.
  /// This error indicates that the API did not return a valid response that can be processed, possibly due to a server-side issue,
  /// a malformed request, or unexpected input data.
  /// </summary>
  AnthropicExceptionInvalidResponse = class(AnthropicException);

  /// <summary>
  /// An `InvalidResponse` error occurs when the API response is either empty or not in the expected format.
  /// This error indicates that the API did not return a valid response that can be processed, possibly due to a server-side issue,
  /// a malformed request, or unexpected input data.
  /// </summary>
  AnthropicExceptionNotFoundError = class(AnthropicException);

  /// <summary>
  /// Une erreur `not_found_error` se produit lorsque la ressource demandée n'a pas été trouvée.
  /// Cette erreur indique que l'API n'a pas renvoyé de réponse valide pouvant être traitée, probablement en raison d'un problème côté serveur,
  /// d'une demande mal formulée ou de données d'entrée inattendues.
  /// </summary>
  AnthropicExceptionRequestTooLarge = class(AnthropicException);

  /// <summary>
  /// An `api_error` error occurs when an unexpected error has occurred internal to Anthropic’s systems
  /// </summary>
  AnthropicExceptionAPIError = class(AnthropicException);

  /// <summary>
  /// An `overloaded_error` error occurs when Anthropic’s API is temporarily overloaded.
  /// </summary>
  AnthropicExceptionOverloadedError = class(AnthropicException);

  /// <summary>
  /// Options available to define the value added to the HTTP header.
  /// </summary>
  THeaderOption = (
    /// <summary>
    /// No value is added to the HTTP header
    /// </summary>
    none,
    /// <summary>
    /// Add the value "anthropic-beta: prompt-caching-2024-07-31" to the HTTP header
    /// </summary>
    caching,
    /// <summary>
    /// Add the value "anthropic-beta: message-batches-2024-09-24" to the HTTP header
    /// </summary>
    batches);

  TAnthropicAPI = class
  public
    const
      URL_BASE = 'https://api.anthropic.com/v1';
  private
    FHTTPClient: THTTPClient;
    FToken: string;
    FBaseUrl: string;
    FOrganization: string;
    FCustomHeaders: TNetHeaders;
    FHeaderOption: THeaderOption;
    procedure SetToken(const Value: string);
    procedure SetBaseUrl(const Value: string);
    procedure SetOrganization(const Value: string);
    procedure RaiseError(Code: Int64; Error: TErrorCore);
    procedure ParseError(const Code: Int64; const ResponseText: string);
    procedure SetCustomHeaders(const Value: TNetHeaders);
    function ObjectToStringFix(const Value: string): string;

  protected
    function GetHeaders: TNetHeaders;
    function GetRequestURL(const Path: string): string;
    function Get(const Path: string; Response: TStringStream): Integer; overload;
    function Delete(const Path: string; Response: TStringStream): Integer; overload;
    function Post(const Path: string; Response: TStringStream): Integer; overload;
    function Post(const Path: string; Body: TJSONObject; Response: TStringStream; OnReceiveData: TReceiveDataCallback = nil): Integer; overload;
    function Post(const Path: string; Body: TMultipartFormData; Response: TStringStream): Integer; overload;
    function ParseResponse<T: class, constructor>(const Code: Int64; const ResponseText: string): T;
    procedure CheckAPI;

  public
    function Get<TResult: class, constructor>(const Path: string): TResult; overload;
    function Get(const Path: string): string; overload;
    procedure GetFile(const Path: string; Response: TStream); overload;
    function Delete<TResult: class, constructor>(const Path: string): TResult; overload;
    function Post<TParams: TJSONParam>(const Path: string; ParamProc: TProc<TParams>; Response: TStringStream; Event: TReceiveDataCallback): Boolean; overload;
    function Post<TResult: class, constructor; TParams: TJSONParam>(const Path: string; ParamProc: TProc<TParams>): TResult; overload;
    function Post<TResult: class, constructor>(const Path: string; ParamJSON: TJSONObject): TResult; overload;
    function Post<TResult: class, constructor>(const Path: string): TResult; overload;
    function PostForm<TResult: class, constructor; TParams: TMultipartFormData, constructor>(const Path: string; ParamProc: TProc<TParams>): TResult; overload;

  public
    constructor Create(const Option: THeaderOption = none); overload;
    constructor Create(const AToken: string; const Option: THeaderOption = none); overload;
    destructor Destroy; override;
    property Token: string read FToken write SetToken;
    property BaseUrl: string read FBaseUrl write SetBaseUrl;
    property Organization: string read FOrganization write SetOrganization;
    property Client: THTTPClient read FHTTPClient;
    property CustomHeaders: TNetHeaders read FCustomHeaders write SetCustomHeaders;
  end;

  TAnthropicAPIRoute = class
  private
    FAPI: TAnthropicAPI;
    procedure SetAPI(const Value: TAnthropicAPI);
  public
    property API: TAnthropicAPI read FAPI write SetAPI;
    constructor CreateRoute(AAPI: TAnthropicAPI); reintroduce;
  end;

implementation

uses
  REST.Json;

constructor TAnthropicAPI.Create(const Option: THeaderOption);
begin
  inherited Create;
  FHTTPClient := THTTPClient.Create;
  FToken := EmptyStr;
  FBaseUrl := URL_BASE;
  FHeaderOption := Option;
end;

constructor TAnthropicAPI.Create(const AToken: string; const Option: THeaderOption);
begin
  Create(Option);
  Token := AToken;
end;

destructor TAnthropicAPI.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TAnthropicAPI.Post(const Path: string; Body: TJSONObject; Response: TStringStream; OnReceiveData: TReceiveDataCallback): Integer;
var
  Headers: TNetHeaders;
  Stream: TStringStream;
begin
  CheckAPI;
  Headers := GetHeaders;
  Stream := TStringStream.Create;
  FHTTPClient.ReceiveDataCallBack := OnReceiveData;
  try
    Stream.WriteString(Body.ToJSON);
    Stream.Position := 0;
    Result := FHTTPClient.Post(GetRequestURL(Path), Stream, Response, Headers).StatusCode;
  finally
    FHTTPClient.OnReceiveData := nil;
    Stream.Free;
  end;
end;

function TAnthropicAPI.Get(const Path: string; Response: TStringStream): Integer;
var
  Headers: TNetHeaders;
begin
  CheckAPI;
  Headers := GetHeaders;
  Result := FHTTPClient.Get(GetRequestURL(Path), Response, Headers).StatusCode;
end;

function TAnthropicAPI.Post(const Path: string; Body: TMultipartFormData; Response: TStringStream): Integer;
var
  Headers: TNetHeaders;
begin
  CheckAPI;
  Headers := GetHeaders;
  Result := FHTTPClient.Post(GetRequestURL(Path), Body, Response, Headers).StatusCode;
end;

function TAnthropicAPI.Post(const Path: string; Response: TStringStream): Integer;
var
  Headers: TNetHeaders;
  Stream: TStringStream;
begin
  CheckAPI;
  Headers := GetHeaders;
  Stream := nil;
  try
    Result := FHTTPClient.Post(GetRequestURL(Path), Stream, Response, Headers).StatusCode;
  finally
  end;
end;

function TAnthropicAPI.Post<TResult, TParams>(const Path: string; ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params.JSON, Response);
    Result := ParseResponse<TResult>(Code, ObjectToStringFix(Response.DataString));
  finally
    Params.Free;
    Response.Free;
  end;
end;

function TAnthropicAPI.Post<TResult>(const Path: string;
  ParamJSON: TJSONObject): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Post(Path, ParamJSON, Response);
    Result := ParseResponse<TResult>(Code, ObjectToStringFix(Response.DataString));
  finally
    Response.Free;
  end;
end;

function TAnthropicAPI.Post<TParams>(const Path: string; ParamProc: TProc<TParams>; Response: TStringStream; Event: TReceiveDataCallback): Boolean;
var
  Params: TParams;
  Code: Integer;
begin
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params.JSON, Response, Event);
    case Code of
      200..299:
        Result := True;
    else
      begin
        Result := False;
        var Recieved := TStringStream.Create;
        try
          Response.Position := 0;
          Recieved.LoadFromStream(Response);
          ParseError(Code, Recieved.DataString);
        finally
          Recieved.Free;
        end;
      end;
    end;
  finally
    Params.Free;
  end;
end;

function TAnthropicAPI.Post<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Post(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

function TAnthropicAPI.Delete(const Path: string; Response: TStringStream): Integer;
var
  Headers: TNetHeaders;
begin
  CheckAPI;
  Headers := GetHeaders;
  Result := FHTTPClient.Delete(GetRequestURL(Path), Response, Headers).StatusCode;
end;

function TAnthropicAPI.Delete<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Delete(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

function TAnthropicAPI.PostForm<TResult, TParams>(const Path: string; ParamProc: TProc<TParams>): TResult;
var
  Response: TStringStream;
  Params: TParams;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  Params := TParams.Create;
  try
    if Assigned(ParamProc) then
      ParamProc(Params);
    Code := Post(Path, Params, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Params.Free;
    Response.Free;
  end;
end;

procedure TAnthropicAPI.RaiseError(Code: Int64; Error: TErrorCore);
begin
  case Code of
    400:
      raise AnthropicExceptionInvalidRequestError.Create(Code, Error);
    401:
      raise AnthropicExceptionAuthenticationError.Create(Code, Error);
    403:
      raise AnthropicExceptionPermissionError.Create(Code, Error);
    404:
      raise AnthropicExceptionNotFoundError.Create(Code, Error);
    413:
      raise AnthropicExceptionRequestTooLarge.Create(Code, Error);
    429:
      raise AnthropicExceptionRateLimitError.Create(Code, Error);
    500:
      raise AnthropicExceptionAPIError.Create(Code, Error);
    529:
      raise AnthropicExceptionOverloadedError.Create(Code, Error);
  else
    raise AnthropicException.Create(Code, Error);
  end;
end;

function TAnthropicAPI.Get(const Path: string): string;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Get(Path, Response);
    case Code of
      200..299: ; //Success
    end;
    Result := Response.DataString;
  finally
    Response.Free;
  end;
end;

function TAnthropicAPI.Get<TResult>(const Path: string): TResult;
var
  Response: TStringStream;
  Code: Integer;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    Code := Get(Path, Response);
    Result := ParseResponse<TResult>(Code, Response.DataString);
  finally
    Response.Free;
  end;
end;

procedure TAnthropicAPI.GetFile(const Path: string; Response: TStream);
var
  Headers: TNetHeaders;
  Code: Integer;
begin
  CheckAPI;
  Headers := GetHeaders;
  Code := FHTTPClient.Get(GetRequestURL(Path), Response, Headers).StatusCode;
  case Code of
    200..299:
      ; {success}
  else
    var Recieved := TStringStream.Create;
    try
      Response.Position := 0;
      Recieved.LoadFromStream(Response);
      ParseError(Code, Recieved.DataString);
    finally
      Recieved.Free;
    end;
  end;
end;

function TAnthropicAPI.GetHeaders: TNetHeaders;
begin
  Result := [TNetHeader.Create('x-api-key', FToken)] + FCustomHeaders;
  Result := Result + [TNetHeader.Create('anthropic-version', '2023-06-01')];
  Result := Result + [TNetHeader.Create('Content-Type', 'application/json')];
  case FHeaderOption of
    caching:
      Result := Result + [TNetHeader.Create('anthropic-beta', 'prompt-caching-2024-07-31')];
    batches:
      Result := Result + [TNetHeader.Create('anthropic-beta', 'message-batches-2024-09-24')];
  end;
end;

function TAnthropicAPI.GetRequestURL(const Path: string): string;
begin
  Result := FBaseURL + '/';
  Result := Result + Path;
end;

function TAnthropicAPI.ObjectToStringFix(const Value: string): string;
begin
  Result := Value;
  var i := Pos('"input":{', Result);
  while (i > 0) and (i < Result.Length) do
    begin
      i := i + 8;
      Result[i] := '"';
      Inc(i);
      var j := 0;
      while (j > 0) or ((j = 0) and not (Result[i] = '}')) do
        begin
          case Result[i] of
            '{':
              Inc(j);
            '}':
              j := j - 1;
            '"':
              Result[i] := '`';
          end;
          Inc(i);
          if i > Result.Length then
            raise Exception.Create('Invalid JSON string');
        end;
      Result[i] := '"';
      i := Pos('"input":{', Result);
    end;
end;

procedure TAnthropicAPI.CheckAPI;
begin
  if FToken.IsEmpty then
    raise AnthropicExceptionAPI.Create('Token is empty!');
  if FBaseUrl.IsEmpty then
    raise AnthropicExceptionAPI.Create('Base url is empty!');
end;

procedure TAnthropicAPI.ParseError(const Code: Int64; const ResponseText: string);
var
  Error: TErrorCore;
begin
  Error := nil;
  try
    try
      Error := TJson.JsonToObject<TError>(ResponseText);
    except
      Error := nil;
    end;
    if Assigned(Error) then
      RaiseError(Code, Error);
  finally
    if Assigned(Error) then
      Error.Free;
  end;
end;

function TAnthropicAPI.ParseResponse<T>(const Code: Int64; const ResponseText: string): T;
begin
  Result := nil;
  case Code of
    200..299:
      try
        Result := TJson.JsonToObject<T>(ResponseText);
      except
        Result := nil;
      end;
    else
      ParseError(Code, ResponseText);
  end;
  if not Assigned(Result) then
    raise AnthropicExceptionInvalidResponse.Create(Code, 'Empty or invalid response');
end;

procedure TAnthropicAPI.SetBaseUrl(const Value: string);
begin
  FBaseUrl := Value;
end;

procedure TAnthropicAPI.SetCustomHeaders(const Value: TNetHeaders);
begin
  FCustomHeaders := Value;
end;

procedure TAnthropicAPI.SetOrganization(const Value: string);
begin
  FOrganization := Value;
end;

procedure TAnthropicAPI.SetToken(const Value: string);
begin
  FToken := Value;
end;

{ AnthropicException }

constructor AnthropicException.Create(const ACode: Int64; const AError: TErrorCore);
begin
  Code := ACode;
  Msg := (AError as TError).Error.Message;
  &Type := (AError as TError).Error.&Type;

  inherited Create(Format('error (%d) %s: '+ sLineBreak + '     %s', [ACode, &Type, Msg]));
end;

constructor AnthropicException.Create(const ACode: Int64; const Value: string);
begin
  Code := ACode;
  Msg := Value;
  inherited Create(Format('error %d: %s', [ACode, Msg]));
end;

{ TAnthropicAPIRoute }

constructor TAnthropicAPIRoute.CreateRoute(AAPI: TAnthropicAPI);
begin
  inherited Create;
  FAPI := AAPI;
end;

procedure TAnthropicAPIRoute.SetAPI(const Value: TAnthropicAPI);
begin
  FAPI := Value;
end;

end.

