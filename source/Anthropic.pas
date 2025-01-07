unit Anthropic;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Net.URLClient,
  Anthropic.API, Anthropic.Chat, Anthropic.Batches, Anthropic.Models,
  Anthropic.Batches.Support, Anthropic.Functions.Core, Anthropic.Schema;

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
    function GetModelsRoute : TModelsRoute;

    /// <summary>
    /// Provides access to the chat completion API.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the batches API.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for batches-related operations.
    /// </returns>
    property Batche: TBatcheRoute read GetBatcheRoute;
    /// <summary>
    /// Provides access to the models API.
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for models-related operations.
    /// </returns>
    property Models: TModelsRoute read GetModelsRoute;
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
      const Option: Integer = 0): IAnthropic;
    /// <summary>
    /// Creates an instance of the <see cref="IAnthropic"/> interface with the specified API token
    /// with header managing caching configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with Anthropic API services.
    /// </param>
    /// <returns>
    /// An instance of <see cref="IAnthropic"/> initialized with the provided API token and header option.
    /// </returns>
    class function CreateCachingInstance(const AToken: string): IAnthropic;
    /// <summary>
    /// Creates an instance of the <see cref="IAnthropic"/> interface with the specified API token
    /// with header managing batch configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with Anthropic API services.
    /// </param>
    /// <returns>
    /// An instance of <see cref="IAnthropic"/> initialized with the provided API token and header option.
    /// </returns>
    class function CreateBatchingInstance(const AToken: string): IAnthropic;
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
    FModelsRoute: TModelsRoute;

    function GetAPI: TAnthropicAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);

    function GetChatRoute: TChatRoute;
    function GetBatcheRoute: TBatcheRoute;
    function GetModelsRoute : TModelsRoute;

  public
    /// <summary>
    /// Provides access to the chat completion API.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for chat-related operations.
    /// </returns>
    property Chat: TChatRoute read GetChatRoute;
    /// <summary>
    /// Provides access to the batches API.
    /// </summary>
    /// <returns>
    /// An instance of TChatRoute for batches-related operations.
    /// </returns>
    property Batche: TBatcheRoute read GetBatcheRoute;
    /// <summary>
    /// Provides access to the models API.
    /// </summary>
    /// <returns>
    /// An instance of TModelsRoute for models-related operations.
    /// </returns>
    property Models: TModelsRoute read GetModelsRoute;

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
    constructor Create(const Option: Integer = 0); overload;
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
    constructor Create(const AToken: string; const Option: Integer = 0); overload;
    /// <summary>
    /// Releases all resources used by the current instance of the <see cref="TAnthropic"/> class.
    /// </summary>
    /// <remarks>
    /// This method is called to clean up any resources before the object is destroyed.
    /// It overrides the base <see cref="TInterfacedObject.Destroy"/> method.
    /// </remarks>
    destructor Destroy; override;
  end;

{$REGION 'Anthropic.Chat'}

  /// <summary>
  /// Represents an image source in the content payload.
  /// </summary>
  /// <remarks>
  /// This class is used to construct image-related content in a chat message, including
  /// its type, media type, and data. The data can be encoded in Base64 format for secure
  /// transmission or direct use, depending on the context.
  /// </remarks>
  TContentImageSource = Anthropic.Chat.TContentImageSource;

  /// <summary>
  /// Represents the content of a chat message.
  /// </summary>
  /// <remarks>
  /// This class is used to construct the content of a chat message, which can include
  /// text or image elements. It allows defining the type of content, the actual text or image
  /// data, and optional caching control for optimized performance.
  /// </remarks>
  TChatMessageContent = Anthropic.Chat.TChatMessageContent;

  /// <summary>
  /// Represents the payload for a chat message in a conversational context.
  /// </summary>
  /// <remarks>
  /// The <c>TChatMessagePayload</c> class provides the structure for defining and managing chat messages.
  /// It allows specifying the role of the sender (e.g., user, assistant) and the content of the message.
  /// The class supports multiple formats for message content, including text, images, and documents, and provides
  /// convenient methods for creating instances with predefined roles and content types.
  /// </remarks>
  TChatMessagePayload = Anthropic.Chat.TChatMessagePayload;

  /// <summary>
  /// Represents the payload for a chat message in a conversational context.
  /// </summary>
  /// <remarks>
  /// The <c>Payload</c> class provides the structure for defining and managing chat messages.
  /// It allows specifying the role of the sender (e.g., user, assistant) and the content of the message.
  /// The class supports multiple formats for message content, including text, images, and documents, and provides
  /// convenient methods for creating instances with predefined roles and content types.
  /// </remarks>
  Payload = Anthropic.Chat.Payload;

  /// <summary>
  /// The <c>TSystemPayload</c> record represents the system message payload
  /// This type is used to indicate behavior rules, response format patterns, or general information about the current context.
  /// </summary>
  /// <remarks>
  /// <para>
  /// - The <c>TSystemPayload</c> record is essential for managing conversations in a chat application, allowing to customize the response that will be built by the LLM.
  /// </para>
  /// <para>
  /// - This record provides several helper methods to create messages with predefined roles for easier management of system prompts.
  /// </para>
  /// <para>
  /// - It notably manages server-side caching.
  /// </para>
  /// <para>
  /// - In a system prompt there can be only two <c>TSystemPayload</c> records at most. The second <c>TSystemPayload</c> will be automatically marked to use caching.
  /// </para>
  /// </remarks>
  TSystemPayload = Anthropic.Chat.TSystemPayload;

  /// <summary>
  /// The <c>TChatParams</c> class represents the set of parameters used to configure a chat interaction with an AI model.
  /// </summary>
  /// <remarks>
  /// This class allows you to define various settings that control how the model behaves, including which model to use, how many tokens to generate,
  /// what kind of messages to send, and how the model should handle its output. By using this class, you can fine-tune the AI's behavior and response format
  /// based on your application's specific needs.
  /// <para>
  /// It inherits from <c>TJSONParam</c>, which provides methods for handling and serializing the parameters as JSON, allowing seamless integration
  /// with JSON-based APIs.
  /// </para>
  /// <code>
  /// var
  ///   Params: TChatParams;
  /// begin
  ///   Params := TChatParams.Create
  ///     .Model('my_model')
  ///     .MaxTokens(100)
  ///     .Messages([TChatMessagePayload1.User('Hello!')])
  ///     .Temperature(0.7)
  ///     .TopP(1)
  /// end;
  /// </code>
  /// This example shows how to instantiate and configure a <c>TChatParams</c> object for interacting with an AI model.
  /// </remarks>
  TChatParams = Anthropic.Chat.TChatParams;

  /// <summary>
  /// Represents the token usage statistics for a chat interaction, including the number of tokens
  /// used in the prompt, the completion, and the total number of tokens consumed.
  /// </summary>
  /// <remarks>
  /// The <c>TChatUsage</c> class provides insight into the number of tokens used during a chat interaction.
  /// This information is critical for understanding the cost of a request when using token-based billing systems
  /// or for monitoring the model's behavior in terms of input (prompt) and output (completion) size.
  /// </remarks>
  TChatUsage = Anthropic.Chat.TChatUsage;

  /// <summary>
  /// The response content made by a tool.
  /// </summary>
  TToolUse = Anthropic.Chat.TToolUse;

  /// <summary>
  /// The response content made by the LLM.
  /// </summary>
  /// <remarks>
  /// This class inherits from the <c>TToolUse</c> class because if a tool has been called, then the arguments obtained must be provided, otherwise it gives access to the textual content of a simple response.
  /// </remarks>
  TChatContent = Anthropic.Chat.TChatContent;

  /// <summary>
  /// Represents a data structure used for managing streaming chunks in a conversational system.
  /// </summary>
  /// <remarks>
  /// This class is primarily designed to handle the incremental data (chunks) returned by an LLM (Large Language Model) during streaming responses.
  /// It stores the type of chunk, textual content, and relevant metadata such as an identifier and tool name if applicable.
  /// The 'Input' property can contain partial JSON data, which should be parsed to construct the final response.
  /// </remarks>
  TChatDelta = Anthropic.Chat.TChatDelta;

  /// <summary>
  /// Represents a chat completion response generated by an AI model, containing the necessary metadata,
  /// the generated choices, and usage statistics.
  /// </summary>
  /// <remarks>
  /// The <c>TChat</c> class encapsulates the results of a chat request made to an AI model.
  /// It contains details such as a unique identifier, the model used, when the completion was created,
  /// the choices generated by the model, and token usage statistics.
  /// This class is crucial for managing the results of AI-driven conversations and understanding the
  /// underlying usage and response characteristics of the AI.
  /// </remarks>
  TChat = Anthropic.Chat.TChat;

  /// <summary>
  /// Count the number of tokens in a Message.
  /// The Token Count API can be used to count the number of tokens in a Message, including tools,
  /// images, and documents, without creating it.
  /// </summary>
  TTokenCount = Anthropic.Chat.TTokenCount;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TTokenCount</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynTokenCount</c> type extends the <c>TAsynParams&lt;TTokenCount&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynTokenCount = Anthropic.Chat.TAsynTokenCount;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChat</c> type extends the <c>TAsynParams&lt;TChat&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynChat = Anthropic.Chat.TAsynChat;

  /// <summary>
  /// Manages asynchronous streaming chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChatStream</c> type extends the <c>TAsynStreamParams&lt;TChat&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynChatStream = Anthropic.Chat.TAsynChatStream;

  /// <summary>
  /// Represents a callback procedure used during the reception of responses from a chat request in streaming mode.
  /// </summary>
  /// <param name="Chat">
  /// The <c>TChat</c> object containing the current information about the response generated by the model.
  /// If this value is <c>nil</c>, it indicates that the data stream is complete.
  /// </param>
  /// <param name="IsDone">
  /// A boolean flag indicating whether the streaming process is complete.
  /// If <c>True</c>, it means the model has finished sending all response data.
  /// </param>
  /// <param name="Cancel">
  /// A boolean flag that can be set to <c>True</c> within the callback to cancel the streaming process.
  /// If set to <c>True</c>, the streaming will be terminated immediately.
  /// </param>
  /// <remarks>
  /// This callback is invoked multiple times during the reception of the response data from the model.
  /// It allows for real-time processing of received messages and interaction with the user interface or other systems
  /// based on the state of the data stream.
  /// When the <c>IsDone</c> parameter is <c>True</c>, it indicates that the model has finished responding,
  /// and the <c>Chat</c> parameter will be <c>nil</c>.
  /// </remarks>
  TChatEvent = Anthropic.Chat.TChatEvent;

{$ENDREGION}

{$REGION 'Anthropic.Batches'}

  /// <summary>
  /// The <c>TBatcheParams</c> class is used to manage and define parameters for a batch of messages.
  /// It provides methods to customize the batch with specific identifiers and additional parameters.
  /// </summary>
  TBatcheParams = Anthropic.Batches.TBatcheParams;

  /// <summary>
  /// The <c>TRequestParams</c> class is used to manage and define request parameters for sending message batches.
  /// It allows you to specify multiple batch requests as part of a single request operation.
  /// </summary>
  TRequestParams = Anthropic.Batches.TRequestParams;

  /// <summary>
  /// The <c>TRequestCounts</c> class represents the counts of different statuses related to batch processing.
  /// It tracks the number of batches that are currently being processed, successfully completed, errored, canceled, and expired.
  /// </summary>
  /// <remarks>
  /// This class provides an overview of the state of batch processing by categorizing the results into several status types,
  /// helping to monitor the success and failure rates of batch operations.
  /// </remarks>
  TRequestCounts = Anthropic.Batches.TRequestCounts;

  /// <summary>
  /// The <c>TBatche</c> class represents a batch of messages in the system.
  /// It contains detailed information about the batch, including its processing status, request counts, timestamps, and related URLs.
  /// </summary>
  /// <remarks>
  /// This class provides key details for managing and tracking a batch of messages, such as the batch's unique identifier,
  /// its current state (in progress, canceled, or ended), and related metadata. It is essential for operations that involve handling
  /// message batches in a structured and organized manner.
  /// </remarks>
  TBatche = Anthropic.Batches.TBatche;

  /// <summary>
  /// The <c>TBatcheList</c> class represents a collection of batch objects, along with metadata about the batch list.
  /// It includes information about whether there are more batches to be fetched and provides identifiers for pagination purposes.
  /// </summary>
  /// <remarks>
  /// This class is used to handle lists of batches returned from the API, enabling pagination through the first and last batch identifiers
  /// and indicating whether additional batches are available beyond the current list.
  /// </remarks>
  TBatcheList = Anthropic.Batches.TBatcheList;

  /// <summary>
  /// The <c>TBatchDelete</c> class represents a batch deletion operation.
  /// It provides information about the identifier and type of the batch being deleted.
  /// </summary>
  /// <remarks>
  /// This class is used to manage the deletion of batches, enabling the application
  /// to track which batch is being removed from the system. It encapsulates the batch
  /// identifier and type information necessary for deletion requests.
  /// </remarks>
  TBatchDelete = Anthropic.Batches.TBatchDelete;

  /// <summary>
  /// The <c>TListParams</c> class is used to define parameters for retrieving lists of batches.
  /// It allows for pagination by setting limits, specifying batch IDs to start after, or ending before.
  /// </summary>
  /// <remarks>
  /// This class helps in controlling the number of results returned in list queries and enables efficient data navigation
  /// through the use of pagination parameters such as <c>Limit</c>, <c>AfterId</c>, and <c>BeforeId</c>.
  /// <para>
  /// <b>--- Warning:</b> The parameters <c>AfterId</c> and <c>BeforeId</c> are mutually exclusive, meaning that both cannot be used simultaneously
  /// in a single query. Ensure that only one of these parameters is set at a time to avoid conflicts.
  /// </para>
  /// </remarks>
  TListParams = Anthropic.Batches.TListParams;

  /// <summary>
  /// The <c>TAsynBatche</c> class is a type alias used to handle asynchronous callbacks for batch processing.
  /// It provides support for executing batch operations asynchronously and processing the results upon completion.
  /// </summary>
  /// <remarks>
  /// This class is part of the asynchronous framework that allows non-blocking batch operations.
  /// It uses a callback mechanism to return the result of a batch process once it is completed.
  /// </remarks>
  TAsynBatche = Anthropic.Batches.TAsynBatche;

  /// <summary>
  /// The <c>TAsynBatcheList</c> class represents an asynchronous callback for handling operations that return a list of batch objects (<c>TBatcheList</c>).
  /// It is used to manage asynchronous processes where a list of batches is retrieved, processed, or manipulated.
  /// </summary>
  /// <remarks>
  /// This class is typically employed in scenarios where batch lists need to be fetched or processed asynchronously, allowing for
  /// non-blocking execution and handling of potentially large sets of batch data.
  /// </remarks>
  TAsynBatcheList = Anthropic.Batches.TAsynBatcheList;

  /// <summary>
  /// The <c>TAsynStringList</c> class is a callback handler for asynchronous operations that return a <c>TStringList</c> result.
  /// It is used to process string list data asynchronously, such as retrieving batch results from the API.
  /// </summary>
  /// <remarks>
  /// This class allows for non-blocking operations where a <c>TStringList</c> is returned, enabling efficient handling of large datasets or long-running tasks.
  /// The callback mechanism helps in managing success, error handling, and overall execution flow.
  /// </remarks>
  TAsynStringList = Anthropic.Batches.TAsynStringList;

  /// <summary>
  /// The <c>TAsynBatchDelete</c> class is a callback handler for asynchronous operations that return a <c>TStringList</c> result.
  /// It is used to process string list data asynchronously, such as retrieving batch results from the API.
  /// </summary>
  /// <remarks>
  /// This class allows for non-blocking operations where a <c>TBatchDelete</c> is returned, enabling efficient handling of large datasets or long-running tasks.
  /// The callback mechanism helps in managing success, error handling, and overall execution flow.
  /// </remarks>
  TAsynBatchDelete = Anthropic.Batches.TAsynBatchDelete;

{$ENDREGION}

{$REGION 'Anthropic.Batches.Support'}

  /// <summary>
  /// Provides an interface for managing batch results and associated file information.
  /// </summary>
  /// <remarks>
  /// The IBatcheResults interface allows for the retrieval of batch result items
  /// and management of the file from which these results are loaded. It ensures
  /// that the specified file exists before loading and provides access to individual
  /// batch results through the Batches property.
  /// </remarks>
  IBatcheResults = Anthropic.Batches.Support.IBatcheResults;

  /// <summary>
  /// Factory class responsible for creating instances of <see cref="IBatcheResults"/>.
  /// </summary>
  /// <remarks>
  /// The <c>TBatcheResultsFactory</c> provides a centralized way to instantiate <c>IBatcheResults</c>
  /// objects, optionally initializing them with a specified file. This ensures that all
  /// instances are created consistently throughout the application.
  /// </remarks>
  TBatcheResultsFactory = Anthropic.Batches.Support.TBatcheResultsFactory;

  /// <summary>
  /// Represents an individual item within a batch result.
  /// </summary>
  /// <remarks>
  /// The <c>TBatcheResultItem</c> class encapsulates a single result item from a batch operation,
  /// including a custom identifier and the corresponding batch result.
  /// </remarks>
  TBatcheResultItem = Anthropic.Batches.Support.TBatcheResultItem;

  /// <summary>
  /// Represents the result of a single batch operation.
  /// </summary>
  /// <remarks>
  /// The <c>TBatcheResult</c> class contains detailed information about the outcome of a batch request,
  /// including the type of result and the associated chat message.
  /// </remarks>
  TBatcheResult = Anthropic.Batches.Support.TBatcheResult;

{$ENDREGION}

{$REGION 'Anthropic.Models'}

  /// <summary>
  /// The <c>TListModelsParams</c> class is used to define parameters for retrieving lists of models.
  /// It allows for pagination by setting limits, specifying model IDs to start after, or ending before.
  /// </summary>
  /// <remarks>
  /// This class helps in controlling the number of results returned in list queries and enables efficient data navigation
  /// through the use of pagination parameters such as <c>Limit</c>, <c>AfterId</c>, and <c>BeforeId</c>.
  /// <para>
  /// <b>--- Warning:</b> The parameters <c>AfterId</c> and <c>BeforeId</c> are mutually exclusive, meaning that both cannot be used simultaneously
  /// in a single query. Ensure that only one of these parameters is set at a time to avoid conflicts.
  /// </para>
  /// </remarks>
  TListModelsParams = Anthropic.Models.TListModelsParams;

  /// <summary>
  /// The <c>TModel</c> class represents a single model entity within the Anthropic API.
  /// It provides access to details about the model, including its unique identifier,
  /// type, human-readable name, and creation timestamp.
  /// </summary>
  /// <remarks>
  /// This class is used to encapsulate the metadata associated with a model.
  /// It is designed to be serialized and deserialized from JSON format
  /// when interacting with the Anthropic API endpoints.
  /// <para>
  /// The <c>Type</c> property will always return the value "model" for instances
  /// of this class. The <c>Id</c> property uniquely identifies the model, while
  /// the <c>DisplayName</c> property provides a human-readable description of the model.
  /// The <c>CreatedAt</c> property indicates when the model was released or first made available.
  /// </para>
  /// </remarks>
  TModel = Anthropic.Models.TModel;

  /// <summary>
  /// The <c>TModels</c> class represents a collection of model entities retrieved from the Anthropic API.
  /// It encapsulates the list of models and additional metadata for pagination purposes.
  /// </summary>
  /// <remarks>
  /// This class is designed to manage and store a list of <c>TModel</c> objects, along with
  /// information to facilitate navigation through paginated results.
  /// <para>
  /// The <c>Data</c> property contains the array of models retrieved in the current query.
  /// The <c>HasMore</c> property indicates whether there are additional results beyond the current page.
  /// The <c>FirstId</c> and <c>LastId</c> properties provide cursors for navigating to the previous
  /// and next pages of results, respectively.
  /// </para>
  /// <para>
  /// When an instance of this class is destroyed, it ensures proper memory cleanup
  /// by freeing the individual <c>TModel</c> objects in the <c>Data</c> array.
  /// </para>
  /// </remarks>
  TModels = Anthropic.Models.TModels;

  /// <summary>
  /// The <c>TAsynModel</c> class is a type alias used to handle asynchronous callbacks for batch processing.
  /// It provides support for executing batch operations asynchronously and processing the results upon completion.
  /// </summary>
  /// <remarks>
  /// This class is part of the asynchronous framework that allows non-blocking batch operations.
  /// It uses a callback mechanism to return the result of a batch process once it is completed.
  /// </remarks>
  TAsynModel = Anthropic.Models.TAsynModel;

  /// <summary>
  /// The <c>TAsynModels</c> class is a type alias used to handle asynchronous callbacks for batch processing.
  /// It provides support for executing batch operations asynchronously and processing the results upon completion.
  /// </summary>
  /// <remarks>
  /// This class is part of the asynchronous framework that allows non-blocking batch operations.
  /// It uses a callback mechanism to return the result of a batch process once it is completed.
  /// </remarks>
  TAsynModels = Anthropic.Models.TAsynModels;

{$ENDREGION}

{$REGION 'Anthropic.Functions.Core'}

  /// <summary>
  /// Interface defining the core structure and functionality of a function in the system.
  /// </summary>
  /// <remarks>
  /// This interface outlines the basic properties and methods that any function implementation must include.
  /// </remarks>
  IFunctionCore = Anthropic.Functions.Core.IFunctionCore;

  /// <summary>
  /// Abstract base class for implementing core function behavior.
  /// </summary>
  /// <remarks>
  /// This class provides basic implementations for some methods and defines the structure that derived classes must follow.
  /// </remarks>
  TFunctionCore = Anthropic.Functions.Core.TFunctionCore;

{$ENDREGION}

{$REGION 'Anthropic.Schema'}

  /// <summary>
  /// Provides helper methods for creating property items in OpenAPI schema definitions.
  /// </summary>
  /// <remarks>
  /// This record simplifies the creation of property entries when building schema objects,
  /// particularly for object properties in OpenAPI specifications.
  /// </remarks>
  TPropertyItem = Anthropic.Schema.TPropertyItem;

  /// <summary>
  /// Represents the Schema Object in OpenAPI, enabling the definition of input and output data types.
  /// These types can be objects, primitives, or arrays. This class provides methods to build and
  /// configure schema definitions as per the OpenAPI 3.0 Specification.
  /// </summary>
  /// <remarks>
  /// The Schema Object allows the definition of input and output data types in the OpenAPI Specification.
  /// This class provides a fluent interface to construct schema definitions programmatically.
  /// </remarks>
  TSchemaParams = Anthropic.Schema.TSchemaParams;

{$ENDREGION}

function fromAssistant(const Value: string): Payload;
function FromPdf(const Value: string; const Documents: TArray<string>;
  CacheControl: Boolean = False): Payload; overload;
function FromPdf(const Value: string; const Documents: string;
  CacheControl: Boolean = False): Payload; overload;
function fromUser(const Value: string; CacheControl: Boolean = False): Payload; overload;
function fromUser(const Value: string; const Images: TArray<string>;
  CacheControl: Boolean = False): Payload; overload;
function fromUser(const Value: string; const Image: string;
  CacheControl: Boolean = False): Payload; overload;

implementation

const
  batches = 1;
  caching = 2;

function fromAssistant(const Value: string): Payload;
begin
  Result := TChatMessagePayload.Assistant(Value);
end;

function FromPdf(const Value: string; const Documents: TArray<string>;
  CacheControl: Boolean = False): Payload;
begin
  Result := TChatMessagePayload.Pdf(Value, Documents, CacheControl);
end;

function FromPdf(const Value: string; const Documents: string;
  CacheControl: Boolean = False): Payload; overload;
begin
  Result := TChatMessagePayload.Pdf(Value, [Documents], CacheControl);
end;

function fromUser(const Value: string; CacheControl: Boolean = False): Payload;
begin
  Result := TChatMessagePayload.User(Value, CacheControl);
end;

function fromUser(const Value: string; const Images: TArray<string>;
  CacheControl: Boolean = False): Payload;
begin
  Result := TChatMessagePayload.User(Value, Images, CacheControl);
end;

function fromUser(const Value: string; const Image: string;
  CacheControl: Boolean = False): Payload; overload;
begin
  Result := TChatMessagePayload.User(Value, [Image], CacheControl);
end;

{ TAnthropic }

constructor TAnthropic.Create(const Option: Integer);
begin
  inherited Create;
  FAPI := TAnthropicAPI.Create(Option);
end;

constructor TAnthropic.Create(const AToken: string; const Option: Integer);
begin
  Create(Option);
  Token := AToken;
end;

destructor TAnthropic.Destroy;
begin
  FChatRoute.Free;
  FBatcheRoute.Free;
  FModelsRoute.Free;
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

function TAnthropic.GetModelsRoute: TModelsRoute;
begin
  if not Assigned(FModelsRoute) then
    FModelsRoute := TModelsRoute.CreateRoute(API);
  Result := FModelsRoute;
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

class function TAnthropicFactory.CreateBatchingInstance(
  const AToken: string): IAnthropic;
begin
  Result := CreateInstance(AToken, batches);
end;

class function TAnthropicFactory.CreateCachingInstance(
  const AToken: string): IAnthropic;
begin
  Result := CreateInstance(AToken, caching);
end;

class function TAnthropicFactory.CreateInstance(const AToken: string;
  const Option: Integer): IAnthropic;
begin
  Result := TAnthropic.Create(AToken, Option);
end;

end.
