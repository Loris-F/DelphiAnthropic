unit Anthropic;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, Anthropic.API, System.Net.URLClient,
  Anthropic.Chat, Anthropic.Batches;

type
  /// <summary>
  /// The <c>IAnthropic</c> interface provides access to the various features and routes of the Anthropic AI API.
  /// This interface allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This interface should be implemented by any class that wants to provide a structured way of accessing
  /// the Anthropic AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  ///
  /// To use this interface, instantiate a class that implements it, set the required properties such as
  /// <see cref="Token"/> and <see cref="BaseURL"/>, and call the relevant methods for the desired operations.
  /// <code>
  ///   var Anthropic: IAnthropic := TAnthropic.Create(API_TOKEN);
  /// </code>
  /// <seealso cref="TAnthropic"/>
  /// </remarks>
  IAnthropic = interface
    ['{7E69221E-3C24-4B38-9AE9-894714CA9A47}']
    function GetAPI: TAnthropicAPI;
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetChatRoute: TChatRoute;
    function GetBatcheRoute: TBatcheRoute;

    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TAnthropicAPI for making API calls.
    /// </returns>
    property API: TAnthropicAPI read GetAPI;
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.anthropic.com/v1
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;
    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the batches API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Batche: TBatcheRoute read GetBatcheRoute;
  end;

  /// <summary>
  /// The <c>TAnthropicFactory</c> class is responsible for creating instances of
  /// the <see cref="IAnthropic"/> interface. It provides a factory method to instantiate
  /// the interface with a provided API token and optional header configuration.
  /// </summary>
  /// <remarks>
  /// This class provides a convenient way to initialize the <see cref="IAnthropic"/> interface
  /// by encapsulating the necessary configuration details, such as the API token and header options.
  /// By using the factory method, users can quickly create instances of <see cref="IAnthropic"/> without
  /// manually setting up the implementation details.
  /// </remarks>
  TAnthropicFactory = class
    /// <summary>
    /// Creates an instance of the <see cref="IAnthropic"/> interface with the specified API token
    /// and optional header configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with Anthropic API services.
    /// </param>
    /// <param name="Option">
    /// An optional header configuration of type <see cref="THeaderOption"/> to customize the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <returns>
    /// An instance of <see cref="IAnthropic"/> initialized with the provided API token and header option.
    /// </returns>
    class function CreateInstance(const AToken: string;
      const Option: THeaderOption = THeaderOption.none): IAnthropic;
  end;

  /// <summary>
  /// The TAnthropic class provides access to the various features and routes of the Anthropic AI API.
  /// This class allows interaction with different services such as agents, chat, code completion,
  /// embeddings, file management, fine-tuning, and model information.
  /// </summary>
  /// <remarks>
  /// This class should be implemented by any class that wants to provide a structured way of accessing
  /// the Anthropic AI services. It includes methods and properties for authenticating with an API key,
  /// configuring the base URL, and accessing different API routes.
  /// <seealso cref="TAnthropic"/>
  /// </remarks>
  TAnthropic = class(TInterfacedObject, IAnthropic)
  strict private

  private
    FAPI: TAnthropicAPI;

    FChatRoute: TChatRoute;
    FBatcheRoute: TBatcheRoute;

    function GetAPI: TAnthropicAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);

    function GetChatRoute: TChatRoute;
    function GetBatcheRoute: TBatcheRoute;

  public
    /// <summary>
    /// the main API object used for making requests.
    /// </summary>
    /// <returns>
    /// An instance of TAnthropicAPI for making API calls.
    /// </returns>
    property API: TAnthropicAPI read GetAPI;
    /// <summary>
    /// Sets or retrieves the API token for authentication.
    /// </summary>
    /// <param name="Value">
    /// The API token as a string.
    /// </param>
    /// <returns>
    /// The current API token.
    /// </returns>
    property Token: string read GetToken write SetToken;
    /// <summary>
    /// Sets or retrieves the base URL for API requests.
    /// Default is https://api.anthropic.com/v1.
    /// </summary>
    /// <param name="Value">
    /// The base URL as a string.
    /// </param>
    /// <returns>
    /// The current base URL.
    /// </returns>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;

  public

    /// <summary>
    /// Provides access to the chat completion API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the batches API.
    /// Allows for interaction with models fine-tuned for instruction-based dialogue.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Batche: TBatcheRoute read GetBatcheRoute;

  public
    /// <summary>
    /// Initializes a new instance of the <see cref="TAnthropic"/> class with optional header configuration.
    /// </summary>
    /// <param name="Option">
    /// An optional parameter of type <see cref="THeaderOption"/> to configure the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <remarks>
    /// This constructor is typically used when no API token is provided initially.
    /// The token can be set later via the <see cref="Token"/> property.
    /// </remarks>
    constructor Create(const Option: THeaderOption = THeaderOption.none); overload;
    /// <summary>
    /// Initializes a new instance of the <see cref="TAnthropic"/> class with the provided API token and optional header configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with the Anthropic AI API.
    /// </param>
    /// <param name="Option">
    /// An optional parameter of type <see cref="THeaderOption"/> to configure the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <remarks>
    /// This constructor allows the user to specify an API token at the time of initialization.
    /// </remarks>
    constructor Create(const AToken: string; const Option: THeaderOption = THeaderOption.none); overload;
    /// <summary>
    /// Releases all resources used by the current instance of the <see cref="TAnthropic"/> class.
    /// </summary>
    /// <remarks>
    /// This method is called to clean up any resources before the object is destroyed.
    /// It overrides the base <see cref="TInterfacedObject.Destroy"/> method.
    /// </remarks>
    destructor Destroy; override;
  end;

implementation

{ TAnthropic }

constructor TAnthropic.Create(const Option: THeaderOption);
begin
  inherited Create;
  FAPI := TAnthropicAPI.Create(Option);
end;

constructor TAnthropic.Create(const AToken: string; const Option: THeaderOption);
begin
  Create(Option);
  Token := AToken;
end;

destructor TAnthropic.Destroy;
begin
  FChatRoute.Free;
  FBatcheRoute.Free;
  FAPI.Free;
  inherited;
end;

function TAnthropic.GetAPI: TAnthropicAPI;
begin
  Result := FAPI;
end;

function TAnthropic.GetBaseUrl: string;
begin
  Result := FAPI.BaseURL;
end;

function TAnthropic.GetBatcheRoute: TBatcheRoute;
begin
  if not Assigned(FBatcheRoute) then
    FBatcheRoute := TBatcheRoute.CreateRoute(API);
  Result := FBatcheRoute;
end;

function TAnthropic.GetChatRoute: TChatRoute;
begin
  if not Assigned(FChatRoute) then
    FChatRoute := TChatRoute.CreateRoute(API);
  Result := FChatRoute;
end;

function TAnthropic.GetToken: string;
begin
  Result := FAPI.Token;
end;

procedure TAnthropic.SetBaseUrl(const Value: string);
begin
  FAPI.BaseURL := Value;
end;

procedure TAnthropic.SetToken(const Value: string);
begin
  FAPI.Token := Value;
end;

{ TAnthropicFactory }

class function TAnthropicFactory.CreateInstance(const AToken: string;
  const Option: THeaderOption): IAnthropic;
begin
  Result := TAnthropic.Create(AToken, Option);
end;

end.
