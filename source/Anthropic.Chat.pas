unit Anthropic.Chat;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Anthropic.API.Params, Anthropic.API, Anthropic.Functions.Core,
  Anthropic.Async.Support, Anthropic.Async.Params, Anthropic.Types;

type
  /// <summary>
  /// Represents an image source in the content payload.
  /// </summary>
  /// <remarks>
  /// This class is used to construct image-related content in a chat message, including
  /// its type, media type, and data. The data can be encoded in Base64 format for secure
  /// transmission or direct use, depending on the context.
  /// </remarks>
  TContentImageSource = class(TJSONParam)
  public
    /// <summary>
    /// Sets the type of the content.
    /// </summary>
    /// <param name="Value">
    /// The type of the content, typically "image".
    /// </param>
    /// <returns>
    /// The updated <c>TContentImageSource</c> instance.
    /// </returns>
    /// <remarks>
    /// This property is used to specify the general type of the content being added.
    /// </remarks>
    function &Type(const Value: string): TContentImageSource;
    /// <summary>
    /// Sets the media type of the image content.
    /// </summary>
    /// <param name="Value">
    /// The MIME type of the image, such as "image/png" or "image/jpeg".
    /// </param>
    /// <returns>
    /// The updated <c>TContentImageSource</c> instance.
    /// </returns>
    /// <remarks>
    /// The media type defines the format of the image, enabling proper handling during processing.
    /// </remarks>
    function MediaType(const Value: string): TContentImageSource;
    /// <summary>
    /// Sets the image data.
    /// </summary>
    /// <param name="Value">
    /// The Base64-encoded string representing the image data.
    /// </param>
    /// <returns>
    /// The updated <c>TContentImageSource</c> instance.
    /// </returns>
    /// <remarks>
    /// The data property holds the image content in a format suitable for embedding in
    /// JSON-based payloads. Ensure that the input is Base64-encoded if required by the context.
    /// </remarks>
    function Data(const Value: string): TContentImageSource;
    /// <summary>
    /// Creates a new instance of <c>TContentImageSource</c> with the specified image value.
    /// </summary>
    /// <param name="Value">
    /// The string representing the image data or file.
    /// </param>
    /// <param name="Kind">
    /// Enum: 'image' or 'base64'
    /// </param>
    /// <returns>
    /// A new instance of <c>TContentImageSource</c> with its fields initialized.
    /// </returns>
    /// <remarks>
    /// This method validates the MIME type and initializes the instance with the appropriate type,
    /// media type, and data. The input can be a file path or a Base64-encoded string.
    /// </remarks>
    class function New(const Value: string): TContentImageSource;
  end;

  /// <summary>
  /// Represents the content of a chat message.
  /// </summary>
  /// <remarks>
  /// This class is used to construct the content of a chat message, which can include
  /// text or image elements. It allows defining the type of content, the actual text or image
  /// data, and optional caching control for optimized performance.
  /// </remarks>
  TChatMessageContent = class(TJSONParam)
  public
    /// <summary>
    /// Sets the type of the content.
    /// </summary>
    /// <param name="Value">
    /// The type of the content, such as "text", "image" or document.
    /// </param>
    /// <returns>
    /// The updated <c>TChatMessageContent</c> instance.
    /// </returns>
    /// <remarks>
    /// The content type determines how the content will be processed and displayed.
    /// Supported values include "text" for textual content and "image" for image data.
    /// </remarks>
    function &Type(const Value: string): TChatMessageContent;
    /// <summary>
    /// Sets the text content of the message.
    /// </summary>
    /// <param name="Value">
    /// The text to be included in the chat message.
    /// </param>
    /// <returns>
    /// The updated <c>TChatMessageContent</c> instance.
    /// </returns>
    /// <remarks>
    /// This method is used to define the main textual content of the message.
    /// </remarks>
    function Text(const Value: string): TChatMessageContent;
    /// <summary>
    /// Sets the source of the image content.
    /// </summary>
    /// <param name="Value">
    /// The Base64-encoded string or file path of the image.
    /// </param>
    /// <param name="Kind">
    /// Enum: 'image' or 'base64'
    /// </param>
    /// <param name="Caching">
    /// Optional caching type to control the use of cached image data.
    /// </param>
    /// <returns>
    /// The updated <c>TChatMessageContent</c> instance.
    /// </returns>
    /// <remarks>
    /// This method is used to add an image to the message content. The optional caching
    /// parameter allows specifying how the image data should be handled.
    /// </remarks>
    function Source(const Value: string; const Caching: TCachingType = nocaching): TChatMessageContent;
    /// <summary>
    /// Sets the caching control for the content.
    /// </summary>
    /// <param name="Value">
    /// The caching type to be applied, such as "nocaching" or "ephemeral".
    /// </param>
    /// <returns>
    /// The updated <c>TChatMessageContent</c> instance.
    /// </returns>
    /// <remarks>
    /// Caching control is used to optimize content retrieval and reduce latency by defining
    /// how the content is cached on the server.
    /// </remarks>
    function CacheControl(const Value: TCachingType): TChatMessageContent;
    /// <summary>
    /// Creates a new text content instance with optional caching.
    /// </summary>
    /// <param name="Value">
    /// The text to be included in the message.
    /// </param>
    /// <param name="Caching">
    /// Optional caching type to control the use of cached text data.
    /// </param>
    /// <returns>
    /// A new instance of <c>TChatMessageContent</c> configured as text content.
    /// </returns>
    /// <remarks>
    /// This method simplifies the creation of text content with caching options.
    /// </remarks>
    class function AddText(const Value: string; const Caching: TCachingType = nocaching): TChatMessageContent;
    /// <summary>
    /// Creates a new image content instance with optional caching.
    /// </summary>
    /// <param name="Value">
    /// The Base64-encoded string or file path of the image.
    /// </param>
    /// <param name="Caching">
    /// Optional caching type to control the use of cached image data.
    /// </param>
    /// <returns>
    /// A new instance of <c>TChatMessageContent</c> configured as image content.
    /// </returns>
    /// <remarks>
    /// This method simplifies the creation of image content with caching options.
    /// </remarks>
    class function AddImage(const Value: string; const Caching: TCachingType = nocaching): TChatMessageContent;
    /// <summary>
    /// Creates a new PDF content instance with optional caching.
    /// </summary>
    /// <param name="Value">
    /// The Base64-encoded string or file path of the PDF.
    /// </param>
    /// <param name="Caching">
    /// Optional caching type to control the use of cached PDF data.
    /// </param>
    /// <returns>
    /// A new instance of <c>TChatMessageContent</c> configured as PDF content.
    /// </returns>
    /// <remarks>
    /// This method simplifies the creation of PDF content with caching options.
    /// </remarks>
    class function AddPDF(const Value: string; const Caching: TCachingType = nocaching): TChatMessageContent;
  end;

  /// <summary>
  /// Represents the payload for a chat message in a conversational context.
  /// </summary>
  /// <remarks>
  /// The <c>TChatMessagePayload</c> class provides the structure for defining and managing chat messages.
  /// It allows specifying the role of the sender (e.g., user, assistant) and the content of the message.
  /// The class supports multiple formats for message content, including text, images, and documents, and provides
  /// convenient methods for creating instances with predefined roles and content types.
  /// </remarks>
  TChatMessagePayload = class(TJSONParam)
  public
    /// <summary>
    /// Gets or sets the role of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Role</c> property determines who is sending the message. It can be a "user" (representing the end user), an "assistant" (representing an AI or bot).
    /// This property is essential for contextualizing the content of the message within the chat.
    /// </remarks>
    function Role(const Value: TMessageRole): TChatMessagePayload;
    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant.
    /// </remarks>
    function Content(const Value: string): TChatMessagePayload; overload;
    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant.
    /// </remarks>
    function Content(const Value: TJSONArray): TChatMessagePayload; overload;
    /// <summary>
    /// Gets or sets the content of the message.
    /// </summary>
    /// <remarks>
    /// The <c>Content</c> property contains the actual message text. This is a required field and cannot be empty, as it represents the core information being exchanged
    /// in the chat, whether it's from the user, the assistant.
    /// </remarks>
    function Content(const Value: TChatMessageContent): TChatMessagePayload; overload;
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
    class function Assistant(const Value: string): TChatMessagePayload;
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
    class function User(const Value: string; CacheControl: Boolean = False): TChatMessagePayload; overload;
    /// <summary>
    /// Creates a new chat message payload with the role of the user and includes associated vision sources.
    /// </summary>
    /// <param name="Content">
    /// The content of the message that the user is sending.
    /// </param>
    /// <param name="Images">
    /// An array of strings representing image file sources.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance with the role set to "user", the provided content, and the specified vision sources.
    /// </returns>
    /// <remarks>
    /// This method is used to create messages from the user's perspective that include both text content and optional vision sources.
    /// The vision sources can be only Base64-encoded images, and they are used to enhance the message with visual information.
    /// </remarks>
    class function User(const Value: string; const Images: TArray<string>;
      CacheControl: Boolean = False): TChatMessagePayload; overload;
    /// <summary>
    /// Creates a new chat message payload with the role of the user, including PDF documents.
    /// </summary>
    /// <param name="Value">
    /// The text content of the message being sent by the user.
    /// </param>
    /// <param name="Documents">
    /// An array of strings representing the PDF documents to be included in the message.
    /// Each entry in the array is expected to be a Base64-encoded representation of a PDF file.
    /// </param>
    /// <param name="CacheControl">
    /// A boolean flag indicating whether to enable cache control for the provided content.
    /// If set to <c>True</c>, the PDF documents and the message text will be cached.
    /// The default value is <c>False</c>, meaning no caching is applied.
    /// </param>
    /// <returns>
    /// A <c>TChatMessagePayload</c> instance containing the user role, the provided text content,
    /// and the associated PDF documents in the message payload.
    /// </returns>
    /// <remarks>
    /// This method is particularly useful for scenarios where a user needs to include PDF documents as part of their chat interaction.
    /// The PDFs are embedded in the message payload and can be accompanied by a textual message for additional context.
    /// Example:
    /// <code>
    /// var Payload := TChatMessagePayload.Pdf('Here are the requested documents:', [Base64PDF1, Base64PDF2]);
    /// </code>
    /// This generates a chat message with the specified text and two attached PDF documents.
    /// </remarks>
    class function Pdf(const Value: string; const Documents: TArray<string>;
      CacheControl: Boolean = False): TChatMessagePayload;
  end;

  /// <summary>
  /// Represents the payload for a chat message in a conversational context.
  /// </summary>
  /// <remarks>
  /// The <c>Payload</c> class provides the structure for defining and managing chat messages.
  /// It allows specifying the role of the sender (e.g., user, assistant) and the content of the message.
  /// The class supports multiple formats for message content, including text, images, and documents, and provides
  /// convenient methods for creating instances with predefined roles and content types.
  /// </remarks>
  Payload = TChatMessagePayload;

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
  TSystemPayload = class(TJSONParam)
  public
    /// <summary>
    /// Allways "text" in this context
    /// </summary>
    function &Type(const Value: string): TSystemPayload;
    /// <summary>
    /// Content of the system prompt
    /// </summary>
    function Text(const Value: string): TSystemPayload;
    /// <summary>
    /// Enable/disable the cache contol flag.
    /// </summary>
    function CacheControl(const Value: TCachingType): TSystemPayload;
    /// <summary>
    /// Creates a new system payload containing a textual message with optional caching behavior.
    /// </summary>
    /// <param name="Value">
    /// The text content to include in the system payload. This text typically serves as instructions, rules, or contextual information for the AI model.
    /// </param>
    /// <param name="Caching">
    /// Specifies the caching behavior for the system payload. Options include:
    /// <para>
    /// <c>nocaching</c> (default): Disables caching for this payload.
    /// </para>
    /// <para>
    /// <c>ephemeral</c>: Enables server-side caching to optimize performance for repeated requests.
    /// </para>
    /// </param>
    /// <returns>
    /// A <c>TSystemPayload</c> instance initialized with the provided text and caching behavior.
    /// </returns>
    /// <remarks>
    /// This method simplifies the creation of system payloads by allowing the inclusion of textual instructions or guidelines along with optional caching settings.
    /// Use the <c>ephemeral</c> caching option to reduce latency when the same system payload is used multiple times.
    /// </remarks>
    class function AddText(const Value: string; const Caching: TCachingType = nocaching): TSystemPayload;
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
  ///     .Messages([TChatMessagePayload1.User('Hello!')])
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
    /// <param name="Value">An array of <c>TChatMessagePayload1</c> representing the messages in the conversation.</param>
    /// <returns>
    /// The updated <c>TChatParams</c> instance.
    /// </returns>
    /// <remarks>
    /// The first message should have a "user" role to initiate the conversation properly.
    /// </remarks>
    function Messages(const Value: TArray<TChatMessagePayload>): TChatParams; overload;
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
    /// Set the system prompt.
    /// </summary>
    /// <param name="Value">
    /// Array of string: maximum 2 items
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
    function System(const Value: TArray<string>): TChatParams; overload;
    /// <summary>
    /// Set the system prompt.
    /// </summary>
    /// <param name="Value">
    /// the system prompt first value
    /// </param>
    /// <param name="TextFilePath">
    /// The name of a text file for caching.
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
    function System(const Value: string; TextFilePath: string): TChatParams; overload;
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
  /// Count the number of tokens in a Message.
  /// The Token Count API can be used to count the number of tokens in a Message, including tools,
  /// images, and documents, without creating it.
  /// </summary>
  TTokenCount = class
  private
    [JsonNameAttribute('input_tokens')]
    FInputTokens: Int64;
  public
    /// <summary>
    /// The total number of tokens across the provided list of messages, system prompt, and tools.
    /// </summary>
    property InputTokens: Int64 read FInputTokens write FInputTokens;
  end;

  /// <summary>
  /// Manages asynchronous chat callBacks for a chat request using <c>TTokenCount</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsynTokenCount</c> type extends the <c>TAsynParams&lt;TTokenCount&gt;</c> record to handle the lifecycle of an asynchronous chat operation.
  /// It provides event handlers that trigger at various stages, such as when the operation starts, completes successfully, or encounters an error.
  /// This structure facilitates non-blocking chat operations and is specifically tailored for scenarios where multiple choices from a chat model are required.
  /// </remarks>
  TAsynTokenCount = TAsynCallBack<TTokenCount>;

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
    ///     Result.Sender := my_display_component;
    ///
    ///     Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///     Result.OnSuccess := procedure (Sender: TObject; Value: TChat)
    ///     begin
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
    ///     Result.Sender := my_display_component;
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
    /// Count the number of tokens in a Message.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters to evaluate.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a record containing event handlers for the asynchronous chat completion, such as on success and on error.
    /// </param>
    /// <remarks>
    /// The Token Count API can be used to count the number of tokens in a Message, including tools, images, and documents, without creating it.
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// Anthropic.Chat.AsynTokenCount(
    ///   procedure (Params: TChatParams)
    ///   begin
    ///     // Define chat parameters
    ///   end,
    ///   function: TAsynTokenCount
    ///   begin
    ///     Result.Sender := my_display_component;
    ///
    ///     Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///     Result.OnSuccess := procedure (Sender: TObject; Value: TTokenCount)
    ///     begin
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
    procedure AsynTokenCount(ParamProc: TProc<TChatParams>; CallBacks: TFunc<TAsynTokenCount>);
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
    /// <code>
    ///   var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    ///   var Value := Anthropic.Chat.Create(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///     end);
    ///   try
    ///     // Handle the value
    ///   finally
    ///     Value.Free;
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
    ///       // Handle the "Chat" value
    ///     end);
    /// </code>
    /// </remarks>
    function CreateStream(ParamProc: TProc<TChatParams>; Event: TChatEvent): Boolean;
    /// <summary>
    /// Count the number of tokens in a Message.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure used to configure the parameters to evaluate.
    /// </param>
    /// <returns>
    /// Returns a <c>TTokenCount</c> object that contains the tokencount response.
    /// </returns>
    /// <exception cref="AnthropicExceptionAPI">
    /// Thrown when there is an error in the communication with the API or other underlying issues in the API call.
    /// </exception>
    /// <exception cref="AnthropicExceptionInvalidRequestError">
    /// Thrown when the request is invalid, such as when required parameters are missing or values exceed allowed limits.
    /// </exception>
    /// <remarks>
    /// The Token Count API can be used to count the number of tokens in a Message, including tools, images, and documents, without creating it.
    /// <code>
    ///   var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    ///   var Value := Anthropic.Chat.TokenCount(
    ///     procedure (Params: TChatParams)
    ///     begin
    ///       // Define chat parameters
    ///     end);
    ///   try
    ///     // Handle the value
    ///   finally
    ///     Value.Free;
    ///   end;
    /// </code>
    /// </remarks>
    function TokenCount(ParamProc: TProc<TChatParams>): TTokenCount;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json, Anthropic.NetEncoding.Base64,
  Anthropic.Stream.API, System.IOUtils, Anthropic.Httpx;

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
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TChatParams(Add('messages', JSONArray));
end;

{Messages}

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

function TChatParams.System(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('system', Value));
end;

function TChatParams.System(
  const Value: TArray<string>): TChatParams;
begin
  if Length(Value) > 2 then
    raise Exception.Create('"TSystemPayload" with more than two items is not supported');

  if Length(Value) = 1 then
    Exit(System(Value[0]));

  var JSONArray := TJSONArray.Create
    .Add(TSystemPayload.AddText(Value[0]).Detach)
    .Add(TSystemPayload.AddText(Value[1], ephemeral).Detach);

  Result := TChatParams(Add('system', JSONArray));
end;

function TChatParams.System(const Value: string; TextFilePath: string): TChatParams;
begin
  if not FileExists(TextFilePath) or not ExtractFileExt(TextFilePath).ToLower.StartsWith('.txt') then
    raise Exception.Create('"System" parameter error: File not found or file is not of type .txt');

  var JSONArray := TJSONArray.Create
    .Add(TSystemPayload.AddText(Value).Detach)
    .Add(TSystemPayload.AddText(TFile.ReadAllText(TextFilePath, TEncoding.UTF8), ephemeral).Detach);

  Result := TChatParams(Add('system', JSONArray));
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
                        end)
                      else
                        LocalChat.Free;
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
                        end)
                      else
                        LocalChat.Free;
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

procedure TChatRoute.AsynTokenCount(ParamProc: TProc<TChatParams>;
  CallBacks: TFunc<TAsynTokenCount>);
begin
  with TAsynCallBackExec<TAsynTokenCount, TTokenCount>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TTokenCount
      begin
        Result := Self.TokenCount(ParamProc);
      end);
  finally
    Free;
  end;
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

function TChatRoute.TokenCount(ParamProc: TProc<TChatParams>): TTokenCount;
begin
  Result := API.Post<TTokenCount, TChatParams>('messages/count_tokens', ParamProc);
end;

{ TInputFixInterceptor }

procedure TInputFixInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  Arg := Format('{%s}', [Trim(Arg.Replace('`', '"').Replace(#10, ''))]);
  while Arg.Contains(', ') do Arg := Arg.Replace(', ', ',');
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, Arg.Replace(',', ', '));
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

{ TChatMessagePayload }

function TChatMessagePayload.Content(const Value: string): TChatMessagePayload;
begin
  Result := TChatMessagePayload(Add('content', Value));
end;

class function TChatMessagePayload.Assistant(
  const Value: string): TChatMessagePayload;
begin
  Result := TChatMessagePayload.Create.Role(TMessageRole.assistant).Content(Value);
end;

function TChatMessagePayload.Content(
  const Value: TChatMessageContent): TChatMessagePayload;
begin
  Result := TChatMessagePayload(Add('content', TJSONArray.Create.Add(Value.Detach)));
end;

function TChatMessagePayload.Content(
  const Value: TJSONArray): TChatMessagePayload;
begin
  Result := TChatMessagePayload(Add('content', Value));
end;

function TChatMessagePayload.Role(
  const Value: TMessageRole): TChatMessagePayload;
begin
  Result := TChatMessagePayload(Add('role', Value.ToString));
end;

class function TChatMessagePayload.Pdf(const Value: string;
  const Documents: TArray<string>; CacheControl: Boolean): TChatMessagePayload;
begin
  var JSONArray := TJSONArray.Create;
  var index := 1;
  for var Item in Documents do
    begin
      if CacheControl and (index = Length(Documents)) then
        JSONArray.Add(TChatMessageContent.AddPDF(Item, ephemeral).Detach) else
        JSONArray.Add(TChatMessageContent.AddPDF(Item).Detach);
      Inc(index);
    end;
  if CacheControl then
    JSONArray.Add(TChatMessageContent.AddText(Value, ephemeral).Detach) else
    JSONArray.Add(TChatMessageContent.AddText(Value).Detach);
  Result := TChatMessagePayload.Create.Role(TMessageRole.user).Content(JSONArray);
end;

class function TChatMessagePayload.User(const Value: string;
  const Images: TArray<string>; CacheControl: Boolean): TChatMessagePayload;
begin
  var JSONArray := TJSONArray.Create;
  var index := 1;
  for var Item in Images do
    begin
      if CacheControl and (index = Length(Images)) then
        JSONArray.Add(TChatMessageContent.AddImage(Item, ephemeral).Detach) else
        JSONArray.Add(TChatMessageContent.AddImage(Item).Detach);
      Inc(index);
    end;
  if CacheControl then
    JSONArray.Add(TChatMessageContent.AddText(Value, ephemeral).Detach) else
    JSONArray.Add(TChatMessageContent.AddText(Value).Detach);
  Result := TChatMessagePayload.Create.Role(TMessageRole.user).Content(JSONArray);
end;

class function TChatMessagePayload.User(const Value: string;
  CacheControl: Boolean): TChatMessagePayload;
begin
  case CacheControl of
    True :
      Result := TChatMessagePayload.Create.Role(TMessageRole.user).Content(TChatMessageContent.AddText(Value, ephemeral));
    else
      Result := TChatMessagePayload.Create.Role(TMessageRole.user).Content(Value);
  end;
end;

{ TChatMessageContent }

class function TChatMessageContent.AddImage(const Value: string;
  const Caching: TCachingType): TChatMessageContent;
begin
  Result := TChatMessageContent.Create.&Type('image').Source(Value);
  case Caching of
    ephemeral:
      Result := Result.CacheControl(Caching);
  end;
end;

class function TChatMessageContent.AddPDF(const Value: string;
  const Caching: TCachingType): TChatMessageContent;
begin
  Result := TChatMessageContent.Create.&Type('document').Source(Value);
  case Caching of
    ephemeral:
      Result := Result.CacheControl(Caching);
  end;
end;

class function TChatMessageContent.AddText(const Value: string;
  const Caching: TCachingType): TChatMessageContent;
begin
  Result := TChatMessageContent.Create.&Type('text').Text(Value);
  case Caching of
    ephemeral:
      Result := Result.CacheControl(Caching);
  end;
end;

function TChatMessageContent.CacheControl(const Value: TCachingType): TChatMessageContent;
begin
  Result := TChatMessageContent(Add('cache_control', TJSONObject.Create.AddPair('type', Value.ToString)));
end;

function TChatMessageContent.Source(const Value: string;
  const Caching: TCachingType): TChatMessageContent;
begin
  Result := TChatMessageContent(Add('source', TContentImageSource.New(Value).Detach));
end;

function TChatMessageContent.Text(const Value: string): TChatMessageContent;
begin
  Result := TChatMessageContent(Add('text', Value));
end;

function TChatMessageContent.&Type(const Value: string): TChatMessageContent;
begin
  Result := TChatMessageContent(Add('type', Value));
end;

{ TContentImageSource }

function TContentImageSource.Data(const Value: string): TContentImageSource;
begin
  Result := TContentImageSource(Add('data', Value));
end;

function TContentImageSource.MediaType(
  const Value: string): TContentImageSource;
begin
  Result := TContentImageSource(Add('media_type', Value));
end;

class function TContentImageSource.New(
  const Value: string): TContentImageSource;
var
  MimeType: string;
  Base64: string;
begin
  if Value.ToLower.StartsWith('http') then
    Base64 := THttpx.LoadDataToBase64(Value, MimeType) else
    base64 := FileToBase64(Value, MimeType);
  CheckMimeType(MimeType);
  Result := TContentImageSource.Create.&Type('base64').MediaType(MimeType).Data(Base64);
end;

function TContentImageSource.&Type(const Value: string): TContentImageSource;
begin
  Result := TContentImageSource(Add('type', Value));
end;

{ TSystemPayload }

class function TSystemPayload.AddText(const Value: string;
  const Caching: TCachingType): TSystemPayload;
begin
  Result := TSystemPayload.Create.&Type('text').Text(Value);
  if Caching <> nocaching then
  case Caching of
    ephemeral:
      Result := Result.CacheControl(Caching);
  end;
end;

function TSystemPayload.CacheControl(const Value: TCachingType): TSystemPayload;
begin
  Result := TSystemPayload(Add('cache_control', TJSONObject.Create.AddPair('type', Value.ToString)));
end;

function TSystemPayload.Text(const Value: string): TSystemPayload;
begin
  Result := TSystemPayload(Add('text', Value));
end;

function TSystemPayload.&Type(const Value: string): TSystemPayload;
begin
  Result := TSystemPayload(Add('type', Value));
end;

end.
