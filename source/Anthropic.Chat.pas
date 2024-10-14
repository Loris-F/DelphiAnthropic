unit Anthropic.Chat;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Anthropic.API.Params, Anthropic.API, Anthropic.Functions.Core,
  Anthropic.Vision.Params, Anthropic.Async.Support, Anthropic.Async.Params;

type
  /// <summary>
  /// The different states of the cache control for Vision
  /// </summary>
  TSysCachingType = (
    /// <summary>
    /// no caching
    /// </summary>
    none,
    /// <summary>
    /// Caching only for content
    /// </summary>
    contentCached,
    /// <summary>
    /// Caching only for images
    /// </summary>
    imagesCached,
    /// <summary>
    /// Caching for images and contents
    /// </summary>
    both
  );

  /// <summary>
  /// Type of message role
  /// </summary>
  TMessageRole = (
    /// <summary>
    /// User message
    /// </summary>
    user,
    /// <summary>
    /// Assistant message
    /// </summary>
    assistant);

  /// <summary>
  /// Helper record for the <c>TMessageRole</c> enumeration, providing utility methods for converting
  /// between <c>TMessageRole</c> values and their string representations.
  /// </summary>
  TMessageRoleHelper = record helper for TMessageRole
    /// <summary>
    /// Converts the current <c>TMessageRole</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TMessageRole</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TMessageRole</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TMessageRole</c>.
    /// </param>
    /// <returns>
    /// The <c>TMessageRole</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function FromString(const Value: string): TMessageRole; static;
  end;

  /// <summary>
  /// Represents the different reasons why the processing of a request can terminate.
  /// </summary>
  TStopReason = (
    /// <summary>
    /// The model reached a natural stopping point
    /// </summary>
    end_turn,
    /// <summary>
    /// We exceeded the requested max_tokens or the model's maximum
    /// </summary>
    max_tokens,
    /// <summary>
    /// One of your provided custom stop_sequences was generated
    /// </summary>
    stop_sequence,
    /// <summary>
    /// The model invoked one or more tools
    /// </summary>
    tool_use);

  /// <summary>
  /// Helper record for the <c>TFinishReason</c> enumeration, providing utility methods for conversion between string representations and <c>TFinishReason</c> values.
  /// </summary>
  TStopReasonHelper = record helper for TStopReason
    /// <summary>
    /// Converts the current <c>TStopReasonHelper</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TStopReasonHelper</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TStopReasonHelper</c> value from its corresponding string representation.
    /// </summary>
    /// <param name="Value">
    /// The string value representing a <c>TStopReasonHelper</c>.
    /// </param>
    /// <returns>
    /// The corresponding <c>TStopReasonHelper</c> enumeration value for the provided string.
    /// </returns>
    /// <remarks>
    /// This method throws an exception if the input string does not match any valid <c>TStopReasonHelper</c> values.
    /// </remarks>
    class function Create(const Value: string): TStopReason; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TStopReason</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TStopReason</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TStopReasonInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// Converts the <c>TStopReason</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TStopReason</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TStopReason</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TStopReason</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TStopReason</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TStopReason</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TStopReason</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Indicator to specify how to use tools.
  /// </summary>
  TToolChoiceType = (
    /// <summary>
    /// Allows Claude to decide whether to call any provided tools or not. This is the default value.
    /// </summary>
    auto,
    /// <summary>
    /// Tells Claude that it must use one of the provided tools, but doesn’t force a particular tool.
    /// </summary>
    any,
    /// <summary>
    ///  Allows us to force Claude to always use a particular tool.
    /// </summary>
    tool
  );

  /// <summary>
  /// Helper record for the <c>TToolChoiceType</c> enumeration, providing utility methods for converting
  /// between <c>TToolChoiceType</c> values and their string representations.
  /// </summary>
  TToolChoiceTypeHelper = record helper for TToolChoiceType
    /// <summary>
    /// Converts the current <c>TToolChoiceType</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TToolChoiceType</c> value.
    /// </returns>
    function ToString: string;
  end;

  /// <summary>
  /// The <c>TChatMessagePayload</c> record represents a chat message payload, which includes both the role of the message sender and the content of the message.
  /// This type is used to distinguish between different participants in the conversation (e.g., user or assistant) and manage the flow of messages accordingly.
  /// </summary>
  /// <remarks>
  /// The <c>TChatMessagePayload</c> record is essential for managing conversations in a chat application, allowing the differentiation between user inputs, assistant responses.
  /// Each message has a role (defining the participant type) and a content field (the actual message being conveyed).
  /// This record provides several helper methods to create messages with predefined roles for easier message handling.
  /// <para>
  /// - It notably manages server-side caching.
  /// </para>
  /// </remarks>
  TChatMessagePayload = record
  private
    FRole: TMessageRole;
    FContent: string;
    FImages: TArray<TVisionSource>;
    FCacheControl: Boolean;
    FVisionCaching: TSysCachingType;
  public
    /// <summary>
    /// Gets or sets the role of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Role</c> property determines who is sending the message. It can be a "user" (representing the end user), an "assistant" (representing an AI or bot).
    /// This property is essential for contextualizing the content of the message within the chat.
    /// </remarks>
    property Role: TMessageRole read FRole write FRole;
    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant.
    /// </remarks>
    property Content: string read FContent write FContent;
    /// <summary>
    /// Enable/disable the cache contol flag.
    /// </summary>
    /// <remarks>
    /// If enable, it allow continuing a multi-turn conversation.
    /// </remarks>
    property CacheControl: Boolean read FCacheControl write FCacheControl;
    /// <summary>
    /// Enable/disable the cache contol flag only for vision (content & images).
    /// </summary>
    property VisionCaching: TSysCachingType read FVisionCaching write FVisionCaching;
    /// <summary>
    /// Creates a new chat message payload with the role of the assistant.
    /// </summary>
    /// <param name="Content">
    /// The content of the message that the assistant is sending.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "assistant" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is a convenience for creating assistant messages. Use this method when the assistant needs to respond to the user or system.
    /// </remarks>
    class function Assistant(const Content: string): TChatMessagePayload; static;
    /// <summary>
    /// Creates a new chat message payload with the role of the user.
    /// </summary>
    /// <param name="Content">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="CacheControl">
    /// Enable/disable the prompt Catching
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user" and the provided content.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective, typically representing inputs or queries in the conversation.
    /// </remarks>
    class function User(const Content: string; CacheControl: Boolean = False): TChatMessagePayload; overload; static;
    /// <summary>
    /// Creates a new chat message payload with the role of the user and includes associated vision sources.
    /// </summary>
    /// <param name="Content">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="FileNames">
    /// An array of strings representing vision sources.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user", the provided content, and the specified vision sources.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective that include both text content and optional vision sources.
    /// The vision sources can be onlyr Base64-encoded images, and they are used to enhance the message with visual information.
    /// </remarks>
    class function User(const Content: string; FileNames: TArray<string>;
      Caching: TSysCachingType = none): TChatMessagePayload; overload; static;
  end;

  /// <summary>
  /// The <c>TSystemPayload</c> record represents the system message payload
  ///This type is used to indicate behavior rules, response format patterns, or general information about the current context.
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
  TSystemPayload = record
  private
    FType: string;
    FText: string;
    FCacheControl: Boolean;
  public
    /// <summary>
    /// Allways "text" in this context
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// Content of the system prompt
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// Enable/disable the cache contol flag.
    /// </summary>
    property CacheControl: Boolean read FCacheControl write FCacheControl;
    /// <summary>
    /// Create a new <c>TSystemPayload</c> record
    /// </summary>
    /// <param name="Text">
    /// The text of the system prompt.
    /// </param>
    /// <param name="CacheControl">
    /// If True, then then cache control is actived server-side.
    /// </param>
    /// <returns>
    /// The updated <c>TSystemPayload</c> record.
    /// </returns>
    /// <remarks>
    /// System messages: Content blocks in the system array.
    /// <para>
    /// - The cache has a 5 minute time to live (TTL). Currently, “ephemeral” is the only supported cache type, which corresponds to this 5-minute lifetime.
    /// </para>
    /// </remarks>
    class function Create(const Text: string; const CacheControl: Boolean = False): TSystemPayload; overload; static;
    /// <summary>
    /// Create a new <c>TSystemPayload</c> record with the content of a <c>text/plain</c> file content.
    /// </summary>
    /// <param name="Text">
    /// An explanatory text to introduce the contents of the file that will be attached.
    /// </param>
    /// <param name="CacheControl">
    /// If True, then then cache control is actived server-side.
    /// </param>
    /// <returns>
    /// The updated <c>TSystemPayload</c> record.
    /// </returns>
    /// <remarks>
    /// System messages: Content blocks in the system array.
    /// <para>
    /// - The cache has a 5 minute time to live (TTL). Currently, “ephemeral” is the only supported cache type, which corresponds to this 5-minute lifetime.
    /// </para>
    /// </remarks>
    class function Create(const Text: string; const FileNames: TArray<string>;
      const CacheControl: Boolean = False): TSystemPayload; overload; static;
  end;

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
  ///     .Messages([TChatMessagePayload.User('Hello!')])
  ///     .Temperature(0.7)
  ///     .TopP(1)
  /// end;
  /// </code>
  /// This example shows how to instantiate and configure a <c>TChatParams</c> object for interacting with an AI model.
  /// </remarks>
  TChatParams = class(TJSONParam)
    /// <summary>
    /// Specifies the identifier of the model to use.
    /// Currently compatible with "claude-3-haiku-20240307" , "claude-3-5-sonnet-20240620"...
    /// </summary>
    /// <param name="Value">
    /// The model ID to be used for the completion.
    /// Ensure that the specified model is supported and correctly spelled.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// This parameter is required and determines which model will process the request.
    /// </remarks>
    function Model(const Value: string): TChatParams;
    /// <summary>
    /// Sets the maximum number of tokens to generate in the completion.
    /// The total token count of your prompt plus <c>max_tokens</c> cannot exceed the model's context length.
    /// </summary>
    /// <param name="Value">
    /// The maximum number of tokens to generate.
    /// Choose an appropriate value based on your prompt length to avoid exceeding the model's limit.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function MaxTokens(const Value: Integer): TChatParams;
    /// <summary>
    /// Provides the prompt(s) for the model to generate completions from, structured as a list of messages with roles (user, assistant, system) and content.
    /// </summary>
    /// <param name="Value">An array of <c>TChatMessagePayload</c> representing the messages in the conversation.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// The first message should have a "user" role to initiate the conversation properly.
    /// </remarks>
    function Messages(const Value: TArray<TChatMessagePayload>): TChatParams;
    /// <summary>
    /// An object describing metadata about the request.
    /// </summary>
    /// <param name="Value">
    /// An external identifier for the user who is associated with the request.
    /// This should be a uuid, hash value, or other opaque identifier. Anthropic may use this id to help detect abuse.
    /// Do not include any identifying information such as name, email address, or phone number.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function Metadata(const Value: string): TChatParams;
    /// <summary>
    /// Custom text sequences that will cause the model to stop generating.
    /// </summary>
    /// <param name="paramname">
    /// List of text sequences
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// The models will normally stop when they have naturally completed their turn, which will result in a response stop_reason of "end_turn".
    /// <para>
    /// If you want the model to stop generating when it encounters custom strings of text, you can use the stop_sequences parameter. If the model encounters one of the custom sequences, the response stop_reason value will be "stop_sequence" and the response stop_sequence value will contain the matched stop sequence.
    /// </para>
    /// </remarks>
    function StopSequences(const Value: TArray<string>): TChatParams;
    /// <summary>
    /// Specifies whether to stream back partial progress as server-sent events (SSE).
    /// If <c>true</c>, tokens are sent as they become available.
    /// If <c>false</c>, the server will hold the request open until timeout or completion.
    /// </summary>
    /// <param name="Value">
    /// A boolean value indicating whether to enable streaming. Default is <c>true</c>, meaning streaming is enabled by default.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    function Stream(const Value: Boolean = True): TChatParams;
    /// <summary>
    /// Set the system prompt.
    /// </summary>
    /// <param name="Value">
    /// Gives context and instructions as text.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// A system prompt is a way of providing context and instructions to Claude, such as specifying a particular goal or role. See the guide to system prompts.
    /// </remarks>
    function System(const Value: string): TChatParams; overload;
    /// <summary>
    /// Set the system prompt from an array of <c>TSystemPayload</c>.
    /// </summary>
    /// <param name="Value">
    /// Build context and instructions with "Create" methods defined in <c>TSystemPayload</c>.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// The array of <c>TSystemPayload</c> can exceded two items. And teh second item will be automatically marked to use caching serveur-side.
    /// <para>
    /// - A system prompt is a way of providing context and instructions to Claude, such as specifying a particular goal or role. See the guide to system prompts.
    /// </para>
    /// </remarks>
    function System(const Value: TArray<TSystemPayload>): TChatParams; overload;
    /// <summary>
    /// Amount of randomness injected into the response.
    /// Sets the sampling temperature to use for the model's output.
    /// Higher values like 0.8 make the output more random, while lower values like 0.2 make it more focused and deterministic.
    /// </summary>
    /// <param name="Value">
    /// The temperature value between 0.0 and 1.0. Default is 1.0.
    /// A temperature of 0 makes the model deterministic, while a temperature of 1 allows for maximum creativity.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Defaults to 1.0. Ranges from 0.0 to 1.0. Use temperature closer to 0.0 for analytical / multiple choice, and closer to 1.0 for creative and generative tasks.
    /// <para>
    /// Note that even with temperature of 0.0, the results will not be fully deterministic.
    /// </para>
    /// </remarks>
    function Temperature(const Value: Single = 1.0): TChatParams;
    /// <summary>
    /// Only sample from the top K options for each subsequent token.
    /// Top k sampling means sorting by probability and zero-ing out the probabilities for anything below the k’th token.
    /// </summary>
    /// <param name="Value">
    /// TProbability between 0 and 1
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// Used to remove "long tail" low probability responses.
    /// <para>
    /// Recommended for advanced use cases only. You usually only need to use temperature.
    /// It appears to improve quality by removing the tail and making it less likely to go off topic.
    /// </para>
    /// </remarks>
    function TopK(const Value: Single): TChatParams;
    /// <summary>
    /// Sets the nucleus sampling probability mass for the model (Top-p).
    /// For example, 0.1 means only the tokens comprising the top 10% probability mass are considered.
    /// </summary>
    /// <param name="Value">
    /// The <c>top_p</c> value between 0.0 and 1.0. Default is 1.
    /// Lower values limit the model to consider only the most probable options.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// In nucleus sampling, it compute the cumulative distribution over all the options for each subsequent token in decreasing probability order and cut it off once it reaches a particular probability specified by top_p. You should either alter temperature or top_p, but not both.
    /// <para>
    /// Recommended for advanced use cases only. You usually only need to use temperature.
    /// </para>
    /// </remarks>
    function TopP(const Value: Single): TChatParams;
    /// <summary>
    /// How the model should use the provided tools. The model can use a specific tool, any available tool, or decide by itself.
    /// </summary>
    /// <param name="Value">
    /// Expects a value from "auto" or "any" enum
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// tool_choice.disable_parallel_tool_use boolean
    /// Whether to disable parallel tool use.
    /// Defaults to false. If set to true, the model will output at most one tool use.
    /// </remarks>
    function ToolChoice(const Value: TToolChoiceType): TChatParams; overload;
    /// <summary>
    /// The model should use the provided tools.
    /// Toolchoice is tool in this case.
    /// </summary>
    /// <param name="Name">
    /// Name of the tool
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// tool_choice.disable_parallel_tool_use boolean
    /// Whether to disable parallel tool use.
    /// Defaults to false. If set to true, the model will output at most one tool use.
    /// </remarks>
    function ToolChoice(const Name: string): TChatParams; overload;
    /// <summary>
    /// Definitions of tools that the model may use.
    /// </summary>
    /// <param name="Value">
    /// The list of object interface with the JSON input schemas.
    /// </param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// If you include tools in your API request, the model may return tool_use content blocks that represent the model's use of those tools. You can then run those tools using the tool input generated by the model and then optionally return results back to the model using tool_result content blocks.
    /// Example: JSON input schema
    /// <code>
    /// [
    ///  {
    ///   "name": "get_stock_price",
    ///   "description": "Get the current stock price for a given ticker symbol.",
    ///   "input_schema": {
    ///     "type": "object",
    ///     "properties": {
    ///       "ticker": {
    ///          "type": "string",
    ///          "description": "The stock ticker symbol, e.g. AAPL for Apple Inc."
    ///        }
    ///     },
    ///     "required": ["ticker"]
    ///   }
    ///   }
    ///]
    /// </code>
    /// </remarks>
    function Tools(const Value: TArray<IFunctionCore>): TChatParams;
    /// <summary>
    /// Constructor to initialize the <c>TChatParams</c> object with default values.
    /// </summary>
    /// <remarks>
    /// The default model is <c>claude-3-haiku-20240307</c>
    /// </remarks>
    constructor Create; override;
  end;

  /// <summary>
  /// Represents the token usage statistics for a chat interaction, including the number of tokens
  /// used in the prompt, the completion, and the total number of tokens consumed.
  /// </summary>
  /// <remarks>
  /// The <c>TChatUsage</c> class provides insight into the number of tokens used during a chat interaction.
  /// This information is critical for understanding the cost of a request when using token-based billing systems
  /// or for monitoring the model's behavior in terms of input (prompt) and output (completion) size.
  /// </remarks>
  TChatUsage = class
  private
    [JsonNameAttribute('input_tokens')]
    FInputTokens: Int64;
    [JsonNameAttribute('cache_creation_input_tokens')]
    FCacheCreationInputTokens: Int64;
    [JsonNameAttribute('cache_read_input_tokens')]
    FCacheReadInputTokens: Int64;
    [JsonNameAttribute('output_tokens')]
    FOutputTokens: Int64;
  public
    /// <summary>
    /// The number of input tokens which were used.
    /// </summary>
    property InputTokens: Int64 read FInputTokens write FInputTokens;
    /// <summary>
    /// (prompt caching beta) The number of input tokens used to create the cache entry.
    /// </summary>
    property CacheCreationInputTokens: Int64 read FCacheCreationInputTokens write FCacheCreationInputTokens;
    /// <summary>
    /// (prompt caching beta) The number of input tokens read from the cache.
    /// </summary>
    property CacheReadInputTokens: Int64 read FCacheReadInputTokens write FCacheReadInputTokens;
    /// <summary>
    /// The number of output tokens which were used.
    /// </summary>
    property OutputTokens: Int64 read FOutputTokens write FOutputTokens;
  end;

  /// <summary>
  /// Interceptor class for converting <c>input</c> value into JSON string format in JSON deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>input</c> and its string equivalent during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TInputFixInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// When JSON deserialization, converts <c>input</c> value into JSON string to retrieve arguments made by the tool.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>input</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>input</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>input</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// Interceptor class for converting <c>Text</c> values to formatting for display in JSON deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>Text</c> and its string equivalent during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TContentInterceptor = class(TJSONInterceptorStringToString)
  public
    /// <summary>
    /// When JSON deserialization, converts a string back to a <c>Text</c> value to formatting for displaymanaging the returns carriet.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>Text</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>Text</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>Text</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  /// <summary>
  /// The response content made by a tool.
  /// </summary>
  TToolUse = class
  private
    FType: string;
    FId: string;
    FName: string;
    [JsonReflectAttribute(ctString, rtString, TInputFixInterceptor)]
    FInput: string;
  public
    /// <summary>
    /// Available options: tool_use
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// Id provided by the LLM
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Name of the tool identified by the LLM
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Arguments returned by the LLM, these arguments will have to be used to construct the final answer.
    /// </summary>
    property Input: string read FInput write FInput;
  end;

  /// <summary>
  /// The response content made by the LLM.
  /// </summary>
  /// <remarks>
  /// This class inherits from the <c>TToolUse</c> class because if a tool has been called, then the arguments obtained must be provided, otherwise it gives access to the textual content of a simple response.
  /// </remarks>
  TChatContent = class(TToolUse)
  private
    [JsonReflectAttribute(ctString, rtString, TContentInterceptor)]
    FText: string;
  public
    /// <summary>
    /// Available options: text
    /// </summary>
    property &Type;
    /// <summary>
    /// The textual content of a response.
    /// </summary>
    /// <remarks>
    /// The string is correctly formatted with carriage returns and can be directly displayed without prior processing of its formatting.
    /// </remarks>
    property Text: string read FText write FText;
  end;

  /// <summary>
  /// Represents a data structure used for managing streaming chunks in a conversational system.
  /// </summary>
  /// <remarks>
  /// This class is primarily designed to handle the incremental data (chunks) returned by an LLM (Large Language Model) during streaming responses.
  /// It stores the type of chunk, textual content, and relevant metadata such as an identifier and tool name if applicable.
  /// The 'Input' property can contain partial JSON data, which should be parsed to construct the final response.
  /// </remarks>
  TChatDelta = class
  private
    FType: string;
    FText: string;
    FId: string;
    FName: string;
    [JsonNameAttribute('partial_json')]
    FInput: string;
  public
    /// <summary>
    /// Available options: tool_use
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// The textual content of a response.
    /// </summary>
    /// <remarks>
    /// The string is correctly formatted with carriage returns and can be directly displayed without prior processing of its formatting.
    /// </remarks>
    property Text: string read FText write FText;
    /// <summary>
    /// Id provided by the LLM
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Name of the tool identified by the LLM
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    /// Arguments returned by the LLM, these arguments will have to be used to construct the final answer.
    /// </summary>
    property Input: string read FInput write FInput;
  end;

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
  TChat = class
  private
    FId: string;
    FType: string;
    FRole: string;
    FContent: TArray<TChatContent>;
    FModel: string;
    [JsonReflectAttribute(ctString, rtString, TStopReasonInterceptor)]
    [JsonNameAttribute('stop_reason')]
    FStopReason: TStopReason;
    [JsonNameAttribute('stop_sequence')]
    FStopSequence: string;
    FDelta: TChatDelta;
    FUsage: TChatUsage;
  public
    /// <summary>
    /// Unique object identifier.
    /// </summary>
    /// <remarks>
    /// The format and length of IDs may change over time.
    /// </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// Object type.
    /// </summary>
    /// <remarks>
    /// For Messages, this is always "message".
    /// Available options: message
    /// </remarks>
    property &Type: string read FType write FType;
    /// <summary>
    /// Conversational role of the generated message.
    /// </summary>
    /// <remarks>
    /// This will always be "assistant".
    /// Available options: assistant
    /// </remarks>
    property Role: string read FRole write FRole;
    /// <summary>
    /// Content generated by the model.
    /// </summary>
    /// <remarks>
    /// This is an array of content blocks, each of which has a type that determines its shape.
    /// Exemples:
    /// <code>
    /// [{"type": "text", "text": "Hi, I'm Claude."}]
    /// </code>
    /// If the request input messages ended with an assistant turn, then the response content will continue directly from that last turn. You can use this to constrain the model's output.
    /// For example, if the input messages were:
    /// <code>
    /// [
    ///   {"role": "user", "content": "What's the Greek name for Sun? (A) Sol (B) Helios (C) Sun"},
    ///   {"role": "assistant", "content": "The best answer is ("}
    /// ]
    /// </code>
    /// Then the response content might be:
    /// <code>
    /// [{"type": "text", "text": "B)"}]
    /// </code>
    /// </remarks>
    property Content: TArray<TChatContent> read FContent write FContent;
    /// <summary>
    /// The model that handled the request.
    /// </summary>
    property Model: string read FModel write FModel;
    /// <summary>
    /// The reason that we stopped.
    /// </summary>
    /// <remarks>
    /// This may be one the following values:
    /// <para>
    ///  - "end_turn": the model reached a natural stopping point
    /// </para>
    /// <para>
    ///  - "max_tokens": we exceeded the requested max_tokens or the model's maximum
    /// </para>
    /// <para>
    ///  - "stop_sequence": one of your provided custom stop_sequences was generated
    /// </para>
    /// <para>
    ///  - "tool_use": the model invoked one or more tools
    /// </para>
    /// In non-streaming mode this value is always non-null. In streaming mode, it is null in the message_start event and non-null otherwise.
    /// Available options: end_turn, max_tokens, stop_sequence, tool_use
    /// </remarks>
    property StopReason: TStopReason read FStopReason write FStopReason;
    /// <summary>
    /// Which custom stop sequence was generated, if any.
    /// </summary>
    /// <remarks>
    /// This value will be a non-null string if one of your custom stop sequences was generated.
    /// </remarks>
    property StopSequence: string read FStopSequence write FStopSequence;
    /// <summary>
    /// Billing and rate-limit usage.
    /// </summary>
    /// <remarks>
    /// Anthropic's API bills and rate-limits by token counts, as tokens represent the underlying cost to our systems.
    /// <para>
    ///  - Under the hood, the API transforms requests into a format suitable for the model. The model's output then goes through a parsing stage before becoming an API response. As a result, the token counts in usage will not match one-to-one with the exact visible content of an API request or response.
    /// </para>
    /// <para>
    ///  - For example, output_tokens will be non-zero, even for an empty string response from Claude.
    /// </para>
    /// </remarks>
    property Usage: TChatUsage read FUsage write FUsage;
    /// <summary>
    /// Represents the incremental updates (delta) for streaming responses during a conversation.
    /// </summary>
    /// <remarks>
    /// In the context of streaming, this property holds the partial data returned by the model in real-time.
    /// It allows the system to manage ongoing or incomplete responses by updating the chat content progressively as the model generates it.
    /// This is especially useful for handling large responses that are broken into chunks.
    /// </remarks>
    property Delta: TChatDelta read FDelta write FDelta;
    /// <summary>
    /// Destructor to clean up resources used by this <c>TChat</c> instance.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that any allocated resources, such as the memory for the array of choices or usage statistics, are
    /// properly released when the object is no longer needed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChat</c> type extends the <c>TAsynParams&lt;TChat&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynChat = TAsynCallBack<TChat>;

  /// <summary>
  /// Manages asynchronous streaming chat callBacks for a chat request using <c>TChat</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynChatStream</c> type extends the <c>TAsynStreamParams&lt;TChat&gt;</c> record to support the lifecycle of an asynchronous streaming chat operation.
  /// It provides callbacks for different stages, including when the operation starts, progresses with new data chunks, completes successfully, or encounters an error.
  /// This structure is ideal for handling scenarios where the chat response is streamed incrementally, providing real-time updates to the user interface.
  /// </remarks>
  TAsynChatStream = TAsynStreamCallBack<TChat>;

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
  TChatEvent = reference to procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean);

  /// <summary>
  /// The <c>TChatRoute</c> class inherits from <c>TAnthropicAPIRoute</c> and provides an interface for managing various interactions with the chat API.
  /// It supports creating chat completion requests in synchronous, asynchronous, and streaming modes, offering mechanisms to handle responses generated by the model.
  /// </summary>
  /// <remarks>
  /// This class facilitates sending messages to a chat model, receiving responses, and managing them, whether synchronously or asynchronously.
  /// The primary methods in the class are:
  /// <para>
  /// - <c>Create</c> : Sends a chat request and waits for a full response.
  /// </para>
  /// <para>
  /// - <c>AsynCreate</c> : Performs an asynchronous chat completion request with event handling.
  /// </para>
  /// <para>
  /// - <c>CreateStream</c> : Initiates a chat completion request in streaming mode, receiving tokens progressively.
  /// </para>
  /// <para>
  /// - <c>ASynCreateStream</c> : Performs an asynchronous request in streaming mode with event handling.
  /// </para>
  /// Each method allows configuring model parameters, setting input messages, managing token limits, and including callbacks for processing responses or errors.
  /// </remarks>
  TChatRoute = class(TAnthropicAPIRoute)
  public
    /// <summary>
    /// Create an asynchronous completion for chat message
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the chat request, such as model selection, messages, and other parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a record containing event handlers for the asynchronous chat completion, such as on success and on error.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous request to generate a chat completion based on the provided parameters. The response or error is handled by the provided callBacks.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// Anthropic.Chat.AsynCreate(
    ///   procedure (Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///   end,
    ///   function: TAsynChat
    ///   begin
    ///     Result.Sender := Memo1;  // Instance passed to callback parameter
    ///
    ///     Result.OnStart := nil;   // If nil then; Can be omitted
    ///
    ///     Result.OnSuccess := procedure (Sender: TObject; Chat: TChat)
    ///     begin
    ///       var M := Sender as TMemo; // Because Result.Sender = Memo1
    ///       // Handle success operation
    ///     end;
    ///
    ///     Result.OnError := procedure (Sender: TObject; Value: string)
    ///     begin
    ///       // Handle error message
    ///     end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreate(ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynChat>);
    /// <summary>
    /// Creates an asynchronous streaming chat completion request.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, including the model, messages, and additional options such as max tokens and streaming mode.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynChatStream</c> record which contains event handlers for managing different stages of the streaming process: progress updates, success, errors, and cancellation.
    /// </param>
    /// <remarks>
    /// This procedure initiates an asynchronous chat operation in streaming mode, where tokens are progressively received and processed.
    /// The provided event handlers allow for handling progress (i.e., receiving tokens in real time), detecting success, managing errors, and enabling cancellation logic.
    /// <code>
    /// CheckBox1.Checked := False;  //Click to stop the streaming
    /// // WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// Anthropic.Chat.AsynCreateStream(
    ///   procedure(Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///     Params.Stream(True);
    ///   end,
    ///
    ///   function: TAsynChatStream
    ///   begin
    ///     Result.Sender := Memo1; // Instance passed to callback parameter
    ///     Result.OnProgress :=
    ///         procedure (Sender: TObject; Chat: TChat)
    ///         begin
    ///           // Handle progressive updates to the chat response
    ///         end;
    ///     Result.OnSuccess :=
    ///         procedure (Sender: TObject; Chat: TChat)
    ///         begin
    ///           // Handle success when the operation completes
    ///         end;
    ///     Result.OnError :=
    ///         procedure (Sender: TObject; Value: string)
    ///         begin
    ///           // Handle error message
    ///         end;
    ///     Result.OnDoCancel :=
    ///         function: Boolean
    ///         begin
    ///           Result := CheckBox1.Checked; // Click on checkbox to cancel
    ///         end;
    ///     Result.OnCancellation :=
    ///         procedure (Sender: TObject)
    ///         begin
    ///           // Processing when process has been canceled
    ///         end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreateStream(ParamProc: TProc<TChatParams>;
      CallBacks: TFunc<TAsynChatStream>);
    /// <summary>
    /// Creates a completion for the chat message using the provided parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, such as selecting the model, providing messages, setting token limits, etc.
    /// </param>
    /// <returns>
    /// Returns a <c>TChat</c> object that contains the chat response, including the choices generated by the model.
    /// </returns>
    /// <exception cref="AnthropicExceptionAPI">
    /// Thrown when there is an error in the communication with the API or other underlying issues in the API call.
    /// </exception>
    /// <exception cref="AnthropicExceptionInvalidRequestError">
    /// Thrown when the request is invalid, such as when required parameters are missing or values exceed allowed limits.
    /// </exception>
    /// <remarks>
    /// The <c>Create</c> method sends a chat completion request and waits for the full response. The returned <c>TChat</c> object contains the model's generated response, including multiple choices if available.
    ///
    /// Example usage:
    /// <code>
    ///   var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    ///   var Chat := Anthropic.Chat.Create(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///     end);
    ///   try
    ///     for var Item in Chat.Content do
    ///       WriteLn(Item.Text);
    ///   finally
    ///     Chat.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function Create(ParamProc: TProc<TChatParams>): TChat;
    /// <summary>
    /// Creates a chat message completion with a streamed response.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters for the chat request, such as selecting the model, providing messages, and adjusting other settings like token limits or temperature.
    /// </param>
    /// <param name="Event">
    /// A callback of type <c>TChatEvent</c> that is triggered with each chunk of data received during the streaming process. It includes the current state of the <c>TChat</c> object, a flag indicating if the stream is done, and a boolean to handle cancellation.
    /// </param>
    /// <returns>
    /// Returns <c>True</c> if the streaming process started successfully, <c>False</c> otherwise.
    /// </returns>
    /// <remarks>
    /// This method initiates a chat request in streaming mode, where the response is delivered incrementally in real-time.
    /// The <c>Event</c> callback will be invoked multiple times as tokens are received.
    /// When the response is complete, the <c>IsDone</c> flag will be set to <c>True</c>, and the <c>Chat</c> object will be <c>nil</c>.
    /// The streaming process can be interrupted by setting the <c>Cancel</c> flag to <c>True</c> within the event.
    ///
    /// Example usage:
    /// <code>
    ///   var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    ///   Anthropic.Chat.CreateStream(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///       Params.Stream(True);
    ///     end,
    ///
    ///     procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    ///     begin
    ///       // Handle displaying
    ///     end
    ///   );
    /// </code>
    /// </remarks>
    function CreateStream(ParamProc: TProc<TChatParams>; Event: TChatEvent): Boolean;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json, Anthropic.NetEncoding.Base64,
  Anthropic.Stream.API;

{ TMessageRoleHelper }

class function TMessageRoleHelper.FromString(const Value: string): TMessageRole;
begin
  case IndexStr(AnsiLowerCase(Value), ['user', 'assistant']) of
    0 :
      Exit(user);
    1 :
      Exit(assistant);
  end;
  Result := user;
end;

function TMessageRoleHelper.ToString: string;
begin
  case Self of
    user:
      Exit('user');
    assistant:
      Exit('assistant');
  end;
end;

{ TStopReasonHelper }

class function TStopReasonHelper.Create(const Value: string): TStopReason;
begin
  case IndexStr(AnsiLowerCase(Value), ['end_turn', 'max_tokens', 'stop_sequence', 'tool_use']) of
    0 :
      Exit(end_turn);
    1 :
      Exit(max_tokens);
    2 :
      Exit(stop_sequence);
    3 :
      Exit(tool_use);
  end;
  Result := end_turn;
end;

function TStopReasonHelper.ToString: string;
begin
  case Self of
    end_turn:
      Exit('end_turn');
    max_tokens:
      Exit('max_tokens');
    stop_sequence:
      Exit('stop_sequence');
    tool_use:
      Exit('tool_use');
  end;
end;

{ TStopReasonInterceptor }

function TStopReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TStopReason>.ToString;
end;

procedure TStopReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TStopReason.Create(Arg)));
end;

{ TChatMessagePayload }

class function TChatMessagePayload.Assistant(
  const Content: string): TChatMessagePayload;
begin
  Result.Role := TMessageRole.assistant;
  Result.Content := Content;
end;

class function TChatMessagePayload.User(
  const Content: string; CacheControl: Boolean): TChatMessagePayload;
begin
  Result.Role := TMessageRole.user;
  Result.Content := Content;
  Result.CacheControl := CacheControl;
end;

class function TChatMessagePayload.User(const Content: string;
  FileNames: TArray<string>; Caching: TSysCachingType): TChatMessagePayload;
begin
  Result.Role := TMessageRole.user;
  Result.Content := Content;
  Result.VisionCaching := Caching;
  Result.CacheControl := Caching in [contentCached, both];
  for var FileName in FileNames do
    Result.FImages := Result.FImages + [TVisionSource.Create(FileName)];
end;

{ TChatParams }

constructor TChatParams.Create;
begin
  inherited;
  Model('claude-3-haiku-20240307');
end;

function TChatParams.MaxTokens(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('max_tokens', Value));
end;

function TChatParams.Messages(
  const Value: TArray<TChatMessagePayload>): TChatParams;

  function HandleForVision(const PayLoad: TChatMessagePayload): TJSONArray;
  begin
    var index := 0;
    Result := TJSONArray.Create;
    for var Image in PayLoad.FImages do
      begin
        var JSONImage := TJSONObject.Create;
        {--- Add type }
        JSONImage.AddPair('type', 'image');

        {--- Add source }
        var JSONSource := TJSONObject.Create;

          {--- Add the encoding type  }
          JSONSource.AddPair('type', 'base64');

          {--- Add the image format type  }
          JSONSource.AddPair('media_type', Image.MimeType);

        {--- Add the base64-encoded data  }
        JSONSource.AddPair('data', Image.Data);

        {--- Add the source JSONImage }
        JSONImage.AddPair('source', JSONSource);

        {--- Mark the cache control flag on the last image record }
        if (PayLoad.VisionCaching in [imagesCached, both]) and
           (index >= Pred(Length(PayLoad.FImages)) ) then
        begin
          {--- create cache_control object }
          var JSONCacheValue := TJSONObject.Create(TJSONPair.Create('type', 'ephemeral'));

          {--- Add cache control flag }
          JSONImage.AddPair('cache_control', JSONCacheValue);
        end;

        {--- Add new entry to result array }
        Result.Add(JSONImage);

        Inc(index);
      end;

    var JSONContent := TJSONObject.Create;
    {--- Add text type }
    JSONContent.AddPair('type', 'text');

    {--- Add text content }
    JSONContent.AddPair('text', PayLoad.Content);

    if PayLoad.CacheControl then
      begin
        {--- create cache_control object }
        var JSONCacheValue := TJSONObject.Create(TJSONPair.Create('type', 'ephemeral'));

        {--- Add cache control flag }
        JSONContent.AddPair('cache_control', JSONCacheValue);
      end;

    {--- Add new entry to result array }
    Result.Add(JSONContent);
  end;

  function HandleCacheControl(const PayLoad: TChatMessagePayload): TJSONArray;
  begin
    Result := TJSONArray.Create;

    var JSONContent := TJSONObject.Create;
    {--- Add type }
    JSONContent.AddPair('type', 'text');

    {--- Add text }
    JSONContent.AddPair('text', PayLoad.Content);

    {--- create cache_control object }
    var JSONCacheValue := TJSONObject.Create(TJSONPair.Create('type', 'ephemeral'));

    {--- Add cache control flag }
    JSONContent.AddPair('cache_control', JSONCacheValue);

    {--- Add new entry to result array }
    Result.Add(JSONContent);
  end;

var
  JSON: TJSONObject;
begin
  var Items := TJSONArray.Create;
  try
    var JSONVision: TJSONArray := nil;
    for var Item in Value do
      begin
        {--- Check vision option }
        if Length(Item.FImages) > 0 then
          begin
            JSONVision := HandleForVision(Item);
          end;

        JSON := TJSONObject.Create;
        {--- Add role }
        JSON.AddPair('role', Item.Role.ToString);

        {--- Add content }
        if Length(Item.FImages) > 0 then
          JSON.AddPair('content', JSONVision)
        else
          begin
            {--- Check cache_control option }
            if Item.CacheControl then
              JSON.AddPair('content', HandleCacheControl(Item)) else
              JSON.AddPair('content', Item.Content);
          end;

        {--- Add new entry to final array }
        Items.Add(JSON);
      end;
  except
    Items.Free;
    raise;
  end;
  Result := TChatParams(Add('messages', Items));
end; {Messages}

function TChatParams.Metadata(const Value: string): TChatParams;
begin
  var JSON := TJSONObject.Create;
  JSON.AddPair('user_id', Value);
  Result := TChatParams(Add('metadata', JSON));
end;

function TChatParams.Model(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('model', Value));
end;

function TChatParams.StopSequences(const Value: TArray<string>): TChatParams;
begin
  Result := TChatParams(Add('stop_sequences', Value));
end;

function TChatParams.Stream(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('stream', Value));
end;

function TChatParams.System(
  const Value: TArray<TSystemPayload>): TChatParams;
var
  JSON: TJSONObject;
begin
  if Length(Value) > 2 then
    raise Exception.Create('"TSystemPayload" with more than two items is not supported');

  var Items := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        JSON := TJSONObject.Create;
        {--- Add type }
        JSON.AddPair('type', Item.&Type);

        {--- Add text }
        JSON.AddPair('text', Item.Text);

        {--- Check cache_control option }
        if Item.CacheControl then
          begin
            {--- Create cache control option ephemeral }
            var JSONCacheValue := TJSONObject.Create(TJSONPair.Create('type', 'ephemeral'));

            {--- Set Cache control option into JSON onject }
            JSON.AddPair('cache_control', JSONCacheValue);
          end;

        {--- Add new entry to final array }
        Items.Add(JSON);
      end;
  except
    Items.Free;
    raise;
  end;
  Result := TChatParams(Add('system', Items));
end;

function TChatParams.System(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('system', Value));
end;

function TChatParams.Temperature(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('temperature', Value));
end;

function TChatParams.ToolChoice(const Value: TToolChoiceType): TChatParams;
begin
  var JSON := TJSONObject.Create;
  JSON.AddPair('type', Value.ToString);
  Result := TChatParams(Add('tool_choice', JSON));
end;

function TChatParams.ToolChoice(const Name: string): TChatParams;
begin
  var JSON := TJSONObject.Create;
  JSON.AddPair('type', tool.ToString);
  JSON.AddPair('name', Name);
  Result := TChatParams(Add('tool_choice', JSON));
end;

function TChatParams.Tools(const Value: TArray<IFunctionCore>): TChatParams;
begin
  var Items := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        Items.Add(Item.ToJson);
      end;
  except
    Items.Free;
    raise;
  end;
  Result := TChatParams(Add('tools', Items));
end;

function TChatParams.TopK(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('top_k', Value));
end;

function TChatParams.TopP(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('top_p', Value));
end;

{ TChat }

destructor TChat.Destroy;
begin
  if Assigned(FDelta) then
    FDelta.Free;
  if Assigned(FUsage) then
    FUsage.Free;
  for var Item in FContent do
    Item.Free;
  inherited;
end;

{ TChatRoute }

procedure TChatRoute.AsynCreate(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynChat>);
begin
  with TAsynCallBackExec<TAsynChat, TChat>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TChat
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TChatRoute.AsynCreateStream(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynChatStream>);
begin
  var CallBackParams := TUseParamsFactory<TAsynChatStream>.CreateInstance(CallBacks);

  var Sender := CallBackParams.Param.Sender;
  var OnStart := CallBackParams.Param.OnStart;
  var OnSuccess := CallBackParams.Param.OnSuccess;
  var OnProgress := CallBackParams.Param.OnProgress;
  var OnError := CallBackParams.Param.OnError;
  var OnCancellation := CallBackParams.Param.OnCancellation;
  var OnDoCancel := CallBackParams.Param.OnDoCancel;

  var Task: ITask := TTask.Create(
          procedure()
          begin
            {--- Pass the instance of the current class in case no value was specified. }
            if not Assigned(Sender) then
              Sender := Self;

            {--- Trigger OnStart callback }
            if Assigned(OnStart) then
              TThread.Queue(nil,
                procedure
                begin
                  OnStart(Sender);
                end);
            try
              var Stop := False;

              {--- Processing }
              CreateStream(ParamProc,
                procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
                begin
                  {--- Check that the process has not been canceled }
                  if Assigned(OnDoCancel) then
                    TThread.Queue(nil,
                        procedure
                        begin
                          Stop := OnDoCancel();
                        end);
                  if Stop then
                    begin
                      {--- Trigger when processus was stopped }
                      if Assigned(OnCancellation) then
                        TThread.Queue(nil,
                        procedure
                        begin
                          OnCancellation(Sender)
                        end);
                      Cancel := True;
                      Exit;
                    end;
                  var LocalChat := Chat;
                  Chat := nil;
                  if not IsDone then
                    begin
                      {--- Triggered when processus is progressing }
                      if Assigned(OnProgress) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnProgress(Sender, LocalChat);
                          finally
                            {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalChat.Free;
                          end;
                        end);
                    end
                  else
                    begin
                      {--- Trigger OnEnd callback when the process is done }
                      if Assigned(OnSuccess) then
                        TThread.Synchronize(TThread.Current,
                        procedure
                        begin
                          try
                            OnSuccess(Sender, LocalChat);
                          finally
                             {--- Makes sure to release the instance containing the data obtained
                                 following processing}
                            LocalChat.Free;
                          end;
                        end);
                    end;
                end);
            except
              on E: Exception do
                begin
                  var Error := AcquireExceptionObject;
                  try
                    var ErrorMsg := (Error as Exception).Message;

                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(OnError) then
                      TThread.Queue(nil,
                      procedure
                      begin
                        OnError(Sender, ErrorMsg);
                      end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
          end);
  Task.Start;
end;

function TChatRoute.Create(ParamProc: TProc<TChatParams>): TChat;
begin
  Result := API.Post<TChat, TChatParams>('messages', ParamProc);
end;

function TChatRoute.CreateStream(ParamProc: TProc<TChatParams>;
  Event: TChatEvent): Boolean;
var
  Response: TStringStream;
  LFPos: Integer;
  Delta: TDelta;
  Prev: string;
begin
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    LFPos := 0;
    Result := API.Post<TChatParams>('messages', ParamProc, Response,
      procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
      var
        IsDone: Boolean;
        Data: string;
        Chat: TChat;
        TextBuffer: string;
        Line: string;
        LF: Integer;
        BlockType: TBlockType;
      begin
        try
          TextBuffer := Response.DataString;
        except
          on E: EEncodingError do
            Exit;
        end;

        repeat
          LF := TextBuffer.IndexOf(#10, LFPos);
          if LF < 0 then
            Continue;
          Line := TextBuffer.Substring(LFPos, LF - LFPos);
          LFPos := LF + 1;

          if Line.IsEmpty or Line.StartsWith(#10) or Line.StartsWith('event') then
            Continue;

          BlockType := TDelta.BlockType(Line);
          if BlockType = btNone then
            Continue;

          IsDone := BlockType = btdone;

          Data := Line.Replace('data: ', '').Trim([' ', #13, #10]);
          Chat := nil;
          try
            if not IsDone then
              begin
                Prev := Data;
                case BlockType of
                  btBlockDelta:
                    Chat := TJson.JsonToObject<TChat>(Delta.BlockDelta(Data).ToString);
                  btMessageStart:
                    Chat := TJson.JsonToObject<TChat>(Delta.MessageStart(Data).ToString);
                  btMessageDelta:
                    Chat := TJson.JsonToObject<TChat>(Delta.MessageDelta(Data).ToString(False));
                  btBlockStart:
                    begin
                      Delta.BlockStart(Data);
                      Continue;
                    end;
                end;
              end
            else
              begin
                Chat := TJson.JsonToObject<TChat>(Delta.MessageDelta(Prev).ToString(False));
              end;
          except
            Chat := nil;
          end;

          try
            Event(Chat, IsDone, AAbort);
          finally
            Chat.Free;
          end;
        until LF < 0;

      end);
  finally
    Response.Free;
  end;
end;

{ TToolChoiceTypeHelper }

function TToolChoiceTypeHelper.ToString: string;
begin
  case Self of
    auto:
      Exit('auto');
    any:
      Exit('any');
    tool:
      Exit('tool');
  end;
end;

{ TInputFixInterceptor }

procedure TInputFixInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var FixValue := Format('{%s}', [ReplaceStr(Arg, '`', '"')]);
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, FixValue);
end;

{ TContentInterceptor }

procedure TContentInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  with TStringList.Create do
  try
    Text := Arg;
    RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, Text);
  finally
    Free;
  end;
end;

{ TSystemPayload }

class function TSystemPayload.Create(const Text: string;
  const CacheControl: Boolean): TSystemPayload;
begin
  Result.FType := 'text';
  Result.FText := Text;
  Result.FCacheControl := CacheControl;
end;

class function TSystemPayload.Create(const Text: string;
  const FileNames: TArray<string>; const CacheControl: Boolean): TSystemPayload;

  function GetTextFileContent(const S: string): string;
  begin
    with TStringList.Create do
    try
      LoadFromFile(S, TEncoding.UTF8);
      Result := Text + sLineBreak;
    finally
      Free;
    end;
  end;

  procedure CheckFile(const S: string);
  begin
    if not FileExists(S) then
      raise Exception.CreateFmt('File not found : %s', [S]);
    var MimeType := ResolveMimeType(S);
    if IndexStr(MimeType, ['text/plain']) = -1 then
      raise Exception.CreateFmt(
         'File : %s' + sLineBreak +
         'Only UTF8 encoded "plain text" text files are supported.', [S]);
  end;

begin
  var AttachedText := EmptyStr;
  for var Item in FileNames do
    begin
      CheckFile(Item);
      if AttachedText.IsEmpty then
         AttachedText := GetTextFileContent(Item) else
         AttachedText := AttachedText + GetTextFileContent(Item);
    end;
  Result.FType := 'text';
  if Trim(AttachedText) <> EmptyStr then
    Result.Text := Format('%s <book> %s </book>', [Text, AttachedText]) else
    Result.Text := AttachedText;

  Result.FCacheControl := CacheControl;
end; {Create}

end.
