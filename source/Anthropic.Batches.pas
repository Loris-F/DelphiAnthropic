unit Anthropic.Batches;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Anthropic.API.Params, Anthropic.API, Anthropic.Async.Support,
  Anthropic.Chat, Anthropic.Types;

type
  /// <summary>
  /// The <c>TBatcheParams</c> class is used to manage and define parameters for a batch of messages.
  /// It provides methods to customize the batch with specific identifiers and additional parameters.
  /// </summary>
  TBatcheParams = class(TJSONParam)
    /// <summary>
    /// Adds a custom identifier to the batch.
    /// </summary>
    /// <param name="Value">
    /// A string representing the custom ID to be added to the batch.
    /// </param>
    /// <returns>
    /// The updated <c>TBatcheParams</c> instance with the custom ID included.
    /// </returns>
    function CustomId(const Value: string): TBatcheParams;
    /// <summary>
    /// Adds chat parameters to the batch using a provided procedure.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure that defines and customizes the chat parameters for the batch.
    /// </param>
    /// <returns>
    /// The updated <c>TBatcheParams</c> instance with the added parameters.
    /// </returns>
    function Params(const ParamProc: TProc<TChatParams>): TBatcheParams;
    /// <summary>
    /// Creates a new <c>TBatcheParams</c> instance, adds a custom identifier, and defines chat parameters.
    /// </summary>
    /// <param name="Value">
    /// A string representing the custom ID for the batch.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to define the chat parameters for the batch.
    /// </param>
    /// <returns>
    /// A new <c>TBatcheParams</c> instance with the specified custom ID and parameters.
    /// </returns>
    class function Add(const Value: string; ParamProc: TProc<TChatParams>): TBatcheParams; overload;
  end;

  /// <summary>
  /// The <c>TRequestParams</c> class is used to manage and define request parameters for sending message batches.
  /// It allows you to specify multiple batch requests as part of a single request operation.
  /// </summary>
  TRequestParams = class(TJSONParam)
  public
    /// <summary>
    /// Specifies a set of batch requests to be included in the request operation.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TBatcheParams</c> instances, where each represents the parameters for an individual batch request.
    /// </param>
    /// <returns>
    /// The updated <c>TRequestParams</c> instance containing the specified batch requests.
    /// </returns>
    function Requests(Value: TArray<TBatcheParams>): TRequestParams; overload;
    /// <summary>
    /// Specifies JSONL file to be included in the request operation.
    /// </summary>
    /// <param name="FilePath">
    /// The JSONL filename.
    /// </param>
    /// <returns>
    /// The updated <c>TRequestParams</c> instance containing the specified batch requests.
    /// </returns>
    function Requests(FilePath: string): TRequestParams; overload;
  end;

  /// <summary>
  /// The <c>TRequestCounts</c> class represents the counts of different statuses related to batch processing.
  /// It tracks the number of batches that are currently being processed, successfully completed, errored, canceled, and expired.
  /// </summary>
  /// <remarks>
  /// This class provides an overview of the state of batch processing by categorizing the results into several status types,
  /// helping to monitor the success and failure rates of batch operations.
  /// </remarks>
  TRequestCounts = class
  private
    FProcessing: Int64;
    FSucceeded: Int64;
    FErrored: Int64;
    FCanceled: Int64;
    FExpired: Int64;
  public
    /// <summary>
    /// Number of requests in the Message Batch that are processing.
    /// </summary>
    property Processing: Int64 read FProcessing write FProcessing;
    /// <summary>
    /// Number of requests in the Message Batch that have completed successfully.
    /// <para>
    /// This is zero until processing of the entire Message Batch has ended.
    /// </para>
    /// </summary>
    property Succeeded: Int64 read FSucceeded write FSucceeded;
    /// <summary>
    /// Number of requests in the Message Batch that encountered an error.
    /// <para>
    /// This is zero until processing of the entire Message Batch has ended.
    /// </para>
    /// </summary>
    property Errored: Int64 read FErrored write FErrored;
    /// <summary>
    /// Number of requests in the Message Batch that have been canceled.
    /// <para>
    /// This is zero until processing of the entire Message Batch has ended.
    /// </para>
    /// </summary>
    property Canceled: Int64 read FCanceled write FCanceled;
    /// <summary>
    /// Number of requests in the Message Batch that have expired.
    /// <para>
    /// This is zero until processing of the entire Message Batch has ended.
    /// </para>
    /// </summary>
    property Expired: Int64 read FExpired write FExpired;
  end;

  /// <summary>
  /// The <c>TBatche</c> class represents a batch of messages in the system.
  /// It contains detailed information about the batch, including its processing status, request counts, timestamps, and related URLs.
  /// </summary>
  /// <remarks>
  /// This class provides key details for managing and tracking a batch of messages, such as the batch's unique identifier,
  /// its current state (in progress, canceled, or ended), and related metadata. It is essential for operations that involve handling
  /// message batches in a structured and organized manner.
  /// </remarks>
  TBatche = class
  private
    FId: string;
    FType: string;
    [JsonNameAttribute('processing_status')]
    FProcessingStatus: TProcessingStatusType;
    [JsonNameAttribute('request_counts')]
    FRequestCounts: TRequestCounts;
    [JsonNameAttribute('ended_at')]
    FEndedAt: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('expires_at')]
    FExpiresAt: string;
    [JsonNameAttribute('cancel_initiated_at')]
    FCancelInitiatedAt: string;
    [JsonNameAttribute('results_url')]
    FResultsUrl: string;
  public
    /// <summary>
    /// Unique object identifier.
    /// </summary>
    /// <remarks>
    /// The format and length of IDs may change over time.
    /// </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// For Message Batches, this is always "message_batch".
    /// </summary>
    /// <remarks>
    /// Available options: message_batch
    /// </remarks>
    property &Type: string read FType write FType;
    /// <summary>
    /// Processing status of the Message Batch.
    /// </summary>
    /// <remarks>
    /// Available options: in_progress, canceling, ended
    /// </remarks>
    property ProcessingStatus: TProcessingStatusType read FProcessingStatus write FProcessingStatus;
    /// <summary>
    /// Tallies requests within the Message Batch, categorized by their status.
    /// </summary>
    /// <remarks>
    /// Requests start as processing and move to one of the other statuses only once processing of the entire batch ends. The sum of all values always matches the total number of requests in the batch.
    /// </remarks>
    property RequestCounts: TRequestCounts read FRequestCounts write FRequestCounts;
    /// <summary>
    /// RFC 3339 datetime string representing the time at which processing for the Message Batch ended. Specified only once processing ends.
    /// </summary>
    /// <remarks>
    /// Processing ends when every request in a Message Batch has either succeeded, errored, canceled, or expired.
    /// </remarks>
    property EndedAt: string read FEndedAt write FEndedAt;
    /// <summary>
    /// RFC 3339 datetime string representing the time at which the Message Batch was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    /// <summary>
    /// RFC 3339 datetime string representing the time at which the Message Batch will expire and end processing, which is 24 hours after creation.
    /// </summary>
    property ExpiresAt: string read FExpiresAt write FExpiresAt;
    /// <summary>
    /// RFC 3339 datetime string representing the time at which cancellation was initiated for the Message Batch. Specified only if cancellation was initiated.
    /// </summary>
    property CancelInitiatedAt: string read FCancelInitiatedAt write FCancelInitiatedAt;
    /// <summary>
    /// URL to a .jsonl file containing the results of the Message Batch requests. Specified only once processing ends.
    /// </summary>
    /// <remarks>
    /// Results in the file are not guaranteed to be in the same order as requests. Use the <b>custom_id</b> field to match results to requests.
    /// </remarks>
    property ResultsUrl: string read FResultsUrl write FResultsUrl;
    /// <summary>
    /// Destructor to clean up resources used by this <c>TBatche</c> instance.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that any allocated resources, such as <b>RequestCounts</>, is properly released when the object is no longer needed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// The <c>TBatcheList</c> class represents a collection of batch objects, along with metadata about the batch list.
  /// It includes information about whether there are more batches to be fetched and provides identifiers for pagination purposes.
  /// </summary>
  /// <remarks>
  /// This class is used to handle lists of batches returned from the API, enabling pagination through the first and last batch identifiers
  /// and indicating whether additional batches are available beyond the current list.
  /// </remarks>
  TBatcheList = class
  private
    FData: TArray<TBatche>;
    [JsonNameAttribute('has_more')]
    FHasMore: Boolean;
    [JsonNameAttribute('first_id')]
    FFirstId: string;
    [JsonNameAttribute('last_id')]
    FLastId: string;
  public
    /// <summary>
    /// Array of batches of messages
    /// </summary>
    property Data: TArray<TBatche> read FData write FData;
    /// <summary>
    /// Indicates if there are more results in the requested page direction.
    /// </summary>
    property HasMore: Boolean read FHasMore write FHasMore;
    /// <summary>
    /// First ID in the data list. Can be used as the before_id for the previous page.
    /// </summary>
    property FirstId: string read FFirstId write FFirstId;
    /// <summary>
    /// Last ID in the data list. Can be used as the after_id for the next page.
    /// </summary>
    property LastId: string read FLastId write FLastId;
    /// <summary>
    /// Destructor to clean up resources used by this <c>TBatcheList</c> instance.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that any allocated resources, such as the array of <b>TBatche</>, is properly released when the object is no longer needed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// The <c>TBatchDelete</c> class represents a batch deletion operation.
  /// It provides information about the identifier and type of the batch being deleted.
  /// </summary>
  /// <remarks>
  /// This class is used to manage the deletion of batches, enabling the application
  /// to track which batch is being removed from the system. It encapsulates the batch
  /// identifier and type information necessary for deletion requests.
  /// </remarks>
  TBatchDelete = class
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// Gets or sets the unique identifier of the batch to be deleted.
    /// </summary>
    /// <remarks>
    /// This property specifies the unique batch ID that is used to identify
    /// the batch to be removed from the system.
    /// </remarks>
    property Id: string read FId write FId;
    /// <summary>
    /// Gets or sets the type of the batch being deleted.
    /// </summary>
    /// <remarks>
    /// For most cases, this will be set to "message_batch" to indicate
    /// that the batch being deleted is of type message batch.
    /// </remarks>
    property &Type: string read FType write FType;
  end;

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
  TListParams = class(TUrlParam)
  public
    /// <summary>
    /// Sets the limit for the number of batches to be retrieved.
    /// </summary>
    /// <param name="Value">
    /// An integer representing the limit. The valid range is 1 to 100.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListParams</c> with the specified limit.
    /// </returns>
    /// <exception cref="Exception">
    /// Thrown if the value is less than 1 or greater than 100.
    /// </exception>
    /// <remarks>
    /// The default value of limit set to 20.
    /// </remarks>
    function Limite(const Value: Integer): TListParams;
    /// <summary>
    /// Sets the batch ID that will be used as a reference to fetch batches created after it.
    /// </summary>
    /// <param name="Value">
    /// A string representing the batch ID.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListParams</c> with the specified <c>after_id</c> value.
    /// </returns>
    function AfterId(const Value: string): TListParams;
    /// <summary>
    /// Sets the batch ID that will be used as a reference to fetch batches created before it.
    /// </summary>
    /// <param name="Value">
    /// A string representing the batch ID.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListParams</c> with the specified <c>before_id</c> value.
    /// </returns>
    function BeforeId(const Value: string): TListParams;
  end;

  /// <summary>
  /// The <c>TAsynBatche</c> class is a type alias used to handle asynchronous callbacks for batch processing.
  /// It provides support for executing batch operations asynchronously and processing the results upon completion.
  /// </summary>
  /// <remarks>
  /// This class is part of the asynchronous framework that allows non-blocking batch operations.
  /// It uses a callback mechanism to return the result of a batch process once it is completed.
  /// </remarks>
  TAsynBatche = TAsynCallBack<TBatche>;

  /// <summary>
  /// The <c>TAsynBatcheList</c> class represents an asynchronous callback for handling operations that return a list of batch objects (<c>TBatcheList</c>).
  /// It is used to manage asynchronous processes where a list of batches is retrieved, processed, or manipulated.
  /// </summary>
  /// <remarks>
  /// This class is typically employed in scenarios where batch lists need to be fetched or processed asynchronously, allowing for
  /// non-blocking execution and handling of potentially large sets of batch data.
  /// </remarks>
  TAsynBatcheList = TAsynCallBack<TBatcheList>;

  /// <summary>
  /// The <c>TAsynStringList</c> class is a callback handler for asynchronous operations that return a <c>TStringList</c> result.
  /// It is used to process string list data asynchronously, such as retrieving batch results from the API.
  /// </summary>
  /// <remarks>
  /// This class allows for non-blocking operations where a <c>TStringList</c> is returned, enabling efficient handling of large datasets or long-running tasks.
  /// The callback mechanism helps in managing success, error handling, and overall execution flow.
  /// </remarks>
  TAsynStringList = TAsynCallBack<TStringList>;

  /// <summary>
  /// The <c>TAsynBatchDelete</c> class is a callback handler for asynchronous operations that return a <c>TStringList</c> result.
  /// It is used to process string list data asynchronously, such as retrieving batch results from the API.
  /// </summary>
  /// <remarks>
  /// This class allows for non-blocking operations where a <c>TBatchDelete</c> is returned, enabling efficient handling of large datasets or long-running tasks.
  /// The callback mechanism helps in managing success, error handling, and overall execution flow.
  /// </remarks>
  TAsynBatchDelete = TAsynCallBack<TBatchDelete>;

  /// <summary>
  /// The <c>TBatcheRoute</c> class provides methods to interact with and manage message batches via the API.
  /// It allows for creating, retrieving, canceling, and listing batches asynchronously and synchronously.
  /// </summary>
  /// <remarks>
  /// This class serves as the main route for performing batch operations within the API. It supports both asynchronous and synchronous operations,
  /// enabling batch creation, retrieval of batch results, cancellation of batch processing, and fetching lists of batches.
  /// </remarks>
  TBatcheRoute = class(TAnthropicAPIRoute)
  public
    /// <summary>
    /// Creates a batch request asynchronously.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to define the parameters for the batch request, including necessary configurations such as model selection, messages, and additional options.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatche</c> record containing event handlers for managing the asynchronous request, including success and error handling.
    /// </param>
    /// <remarks>
    /// This method initiates an asynchronous batch creation request based on the provided parameters. The result or any errors will be handled by the specified callbacks.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.AsynCreate(
    ///   procedure (Params: TRequestParams)
    ///   begin
    ///     // Define parameters
    ///   end,
    ///
    ///   function : TAsynBatche
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TBatche)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreate(ParamProc: TProc<TRequestParams>; CallBacks: TFunc<TAsynBatche>); overload;
    /// <summary>
    /// Creates a batch request asynchronously using a <c>TJSONObject</c>.
    /// </summary>
    /// <param name="FilePath"> The JSON file containing the parameters for the batch request.</param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatche</c> record for handling asynchronous events, such as on success or on error.
    /// </param>
    /// <remarks>
    /// This method allows for creating a batch request asynchronously using a <c>TJSONObject</c> to specify the parameters.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.AsynCreate(FilePath,
    ///   function : TAsynBatche
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TBatche)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreate(const FilePath: string; CallBacks: TFunc<TAsynBatche>); overload;
    /// <summary>
    /// Creates a batch request asynchronously using a <c>TJSONObject</c>.
    /// </summary>
    /// <param name="Value">The JSON object containing the parameters for the batch request.</param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatche</c> record for handling asynchronous events, such as on success or on error.
    /// </param>
    /// <remarks>
    /// This method allows for creating a batch request asynchronously using a <c>TJSONObject</c> to specify the parameters.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.AsynCreate(Value,
    ///   function : TAsynBatche
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TBatche)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCreate(Value: TJSONObject; CallBacks: TFunc<TAsynBatche>); overload;
    /// <summary>
    /// Retrieves a batch result asynchronously by its identifier.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to retrieve.</param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatche</c> record, handling success or error during the asynchronous retrieval.
    /// </param>
    /// <remarks>
    /// This method retrieves the result of a batch process asynchronously using its unique ID. Callbacks handle the process results or errors.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.ASynRetrieve(Id,
    ///   function : TAsynBatche
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TBatche)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynRetrieve(const Id: string; CallBacks: TFunc<TAsynBatche>); overload;
    /// <summary>
    /// Retrieves a batch result asynchronously and saves it to a file.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to retrieve.</param>
    /// <param name="FileName">The name of the file where the batch result will be saved.</param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynStringList</c> record for managing success or error events during the asynchronous file saving process.
    /// </param>
    /// <remarks>
    /// This method retrieves the result of a batch process asynchronously by its ID and saves the result to a file. Callbacks manage the process events.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.ASynRetrieve(Id, FileName,
    ///   function : TAsynStringList
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TStringList)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure ASynRetrieve(const Id: string; FileName: string; CallBacks: TFunc<TAsynStringList>); overload;
    /// <summary>
    /// Lists all batches asynchronously.
    /// </summary>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatcheList</c> record, managing the event handling for asynchronous listing, including progress, success, and error callbacks.
    /// </param>
    /// <remarks>
    /// This method fetches a list of all available batches asynchronously. Callbacks handle events such as receiving data or errors during the listing process.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.AsynList(
    ///   function : TAsynBatcheList
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TBatcheList)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynList(CallBacks: TFunc<TAsynBatcheList>); overload;
    /// <summary>
    /// Lists batches asynchronously using specific parameters.
    /// </summary>
    /// <param name="Params">
    /// A <c>TListParams1</c> object containing filtering and paging options for batch listing.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatcheList</c> record for handling asynchronous batch listing, including success and error events.
    /// </param>
    /// <remarks>
    /// This method allows for retrieving a list of batches asynchronously, with the ability to apply specific filters and paging parameters.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.AsynList(
    ///   procedure (Params: TListParams)
    ///   begin
    ///     // Define parameters
    ///   end,
    ///
    ///   function : TAsynBatcheList
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TBatcheList)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynList(ParamProc: TProc<TListParams>; CallBacks: TFunc<TAsynBatcheList>); overload;
    /// <summary>
    /// Cancels a batch operation asynchronously by its ID.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to be canceled.</param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatche</c> record to handle success or error during the asynchronous cancellation.
    /// </param>
    /// <remarks>
    /// This method cancels a batch process asynchronously using its unique ID. The result or any error is managed by the callbacks provided.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.AsynCancel(Id,
    ///   function : TAsynBatche
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TBatche)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynCancel(const Id: string; CallBacks: TFunc<TAsynBatche>);
    /// <summary>
    /// Delete a batch operation asynchronously by its ID.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to be deleted.</param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatchDelete</c> record to handle success or error during the asynchronous cancellation.
    /// </param>
    /// <remarks>
    /// This method delete a batch process asynchronously using its unique ID. The result or any error is managed by the callbacks provided.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// AnthropicBatche.Batche.AsynDelete(Id,
    ///   function : TAsynBatchDelete
    ///   begin
    ///      Result.Sender := my_display_component;
    ///
    ///      Result.OnStart :=
    ///        procedure (Sender: TObject);
    ///        begin
    ///          // Handle the start
    ///        end;
    ///
    ///      Result.OnSuccess :=
    ///        procedure (Sender: TObject; Value: TBatchDelete)
    ///        begin
    ///          // Handle the value
    ///        end;
    ///
    ///      Result.OnError :=
    ///        procedure (Sender: TObject; Error: string)
    ///        begin
    ///          // Handle the error message
    ///        end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsynDelete(const Id: string; CallBacks: TFunc<TAsynBatchDelete>);
    /// <summary>
    /// Creates a batch request synchronously.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the batch request, such as model, messages, token limits, etc.
    /// </param>
    /// <returns>
    /// A <c>TBatche</c> object containing the result of the created batch request.
    /// </returns>
    /// <remarks>
    /// This method sends a batch creation request synchronously and returns the result as a <c>TBatche</c> object.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var Value := AnthropicBatche.Batche.Create(
    ///   procedure (Params: TRequestParams)
    ///   begin
    ///     // Define parameters
    ///   end);
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Create(ParamProc: TProc<TRequestParams>): TBatche; overload;
    /// <summary>
    /// Creates a batch request synchronously using a <c>TJSONObject</c>.
    /// </summary>
    /// <param name="FilePath"> The JSONL file name containing the parameters for the batch request.</param>
    /// <returns>
    /// A <c>TBatche</c> object containing the result of the created batch request.
    /// </returns>
    /// <remarks>
    /// This method sends a batch creation request synchronously using a <c>TJSONObject</c> to specify parameters and returns the result.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var Value := AnthropicBatche.Batche.Create(FilePath);
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Create(const FilePath: string): TBatche; overload;
    /// <summary>
    /// Creates a batch request synchronously using a <c>TJSONObject</c>.
    /// </summary>
    /// <param name="Value">The JSON object containing the parameters for the batch request.</param>
    /// <returns>
    /// A <c>TBatche</c> object containing the result of the created batch request.
    /// </returns>
    /// <remarks>
    /// This method sends a batch creation request synchronously using a <c>TJSONObject</c> to specify parameters and returns the result.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var Value := AnthropicBatche.Batche.Create(Value);
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Create(Value: TJSONObject): TBatche; overload;
    /// <summary>
    /// Retrieves a batch result synchronously by its identifier.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to retrieve.</param>
    /// <returns>
    /// A <c>TBatche</c> object containing the result of the batch request.
    /// </returns>
    /// <remarks>
    /// This method retrieves a batch result synchronously using its unique ID.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var Value := AnthropicBatche.Batche.Retrieve(BatchId);
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Retrieve(const Id: string): TBatche; overload;
    /// <summary>
    /// Retrieves a batch result synchronously by its identifier and saves it to a file.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to retrieve.</param>
    /// <param name="FileName">The name of the file where the batch result will be saved.</param>
    /// <returns>
    /// A <c>TStringList</c> object containing the retrieved batch result saved to the file.
    /// </returns>
    /// <remarks>
    /// This method retrieves a batch result synchronously and saves it to a file.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var JSONL := AnthropicBatche.Batche.Retrieve(BatchId, 'Result.jsonl');
    ///
    ///  with JSONL.GetEnumerator do
    ///    try
    ///      while MoveNext do
    ///        // Handle the "current" data
    ///    finally
    ///      Free;
    ///      JSONL.Free;
    ///    end;
    /// </code>
    /// </remarks>
    function Retrieve(const Id: string; FileName: string): TStringList; overload;
    /// <summary>
    /// Lists all batches synchronously.
    /// </summary>
    /// <returns>
    /// A <c>TBatcheList</c> object containing a list of all batches.
    /// </returns>
    /// <remarks>
    /// This method retrieves a list of all available batches synchronously.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var Value := AnthropicBatche.Batche.List;
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List: TBatcheList; overload;
    /// <summary>
    /// Lists batches synchronously using specific parameters.
    /// </summary>
    /// <param name="Params">
    /// A <c>TListParams1</c> object containing filtering and paging options for batch listing.
    /// </param>
    /// <returns>
    /// A <c>TBatcheList</c> object containing the filtered list of batches.
    /// </returns>
    /// <remarks>
    /// This method retrieves a list of batches synchronously, with the option to apply specific filters and paging parameters.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var Value := AnthropicBatche.Batche.Delete(
    ///   procedure (Params: TListParams)
    ///   begin
    ///     // Define parameters
    ///   end);
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List(ParamProc: TProc<TListParams>): TBatcheList; overload;
    /// <summary>
    /// Cancels a batch operation synchronously by its ID.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to be canceled.</param>
    /// <returns>
    /// A <c>TBatche</c> object containing the result of the cancellation request.
    /// </returns>
    /// <remarks>
    /// This method cancels a batch process synchronously by its unique ID.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var Value := AnthropicBatche.Batche.Cancel(Id);
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Cancel(const Id: string): TBatche;
    /// <summary>
    /// Delete a batch operation synchronously by its ID.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to be deleted.</param>
    /// <returns>
    /// A <c>TBatche</c> object containing the result of the deletion request.
    /// </returns>
    /// <remarks>
    /// This method deletes a batch process synchronously by its unique ID.
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
    /// var Value := AnthropicBatche.Batche.Delete(Id);
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Delete(const Id: string): TBatchDelete;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json;

{ TBatcheParamsParams }

function TBatcheParams.CustomId(const Value: string): TBatcheParams;
begin
  Result := TBatcheParams(Add('custom_id', Value));
end;

class function TBatcheParams.Add(const Value: string;
  ParamProc: TProc<TChatParams>): TBatcheParams;
begin
  Result := TBatcheParams.Create.CustomId(Value).Params(ParamProc);
end;

function TBatcheParams.Params(const ParamProc: TProc<TChatParams>): TBatcheParams;
begin
  var Data := TChatParams.Create;
  try
    if Assigned(ParamProc) then
      begin
        ParamProc(Data);
        Result := TBatcheParams(Add('params', Data as TJSONParam));
      end
    else
      Result := Self;
  finally
    Data.Free;
  end;
end;

{ TRequestParams }

function TRequestParams.Requests(Value: TArray<TBatcheParams>): TRequestParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TRequestParams(Add('requests', JSONArray));
end;

function TRequestParams.Requests(FilePath: string): TRequestParams;
begin
  Result := TRequestParams(AddJSONL('requests', FilePath));
end;

{ TBatcheRoute }

procedure TBatcheRoute.AsynCreate(ParamProc: TProc<TRequestParams>;
  CallBacks: TFunc<TAsynBatche>);
begin
  with TAsynCallBackExec<TAsynBatche, TBatche>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatche
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TBatcheRoute.AsynCancel(const Id: string;
  CallBacks: TFunc<TAsynBatche>);
begin
  with TAsynCallBackExec<TAsynBatche, TBatche>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatche
      begin
        Result := Self.Cancel(Id);
      end);
  finally
    Free;
  end;
end;

procedure TBatcheRoute.AsynCreate(Value: TJSONObject;
  CallBacks: TFunc<TAsynBatche>);
begin
  with TAsynCallBackExec<TAsynBatche, TBatche>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatche
      begin
        Result := Self.Create(Value);
      end);
  finally
    Free;
  end;
end;

procedure TBatcheRoute.AsynCreate(const FilePath: string;
  CallBacks: TFunc<TAsynBatche>);
begin
  with TAsynCallBackExec<TAsynBatche, TBatche>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatche
      begin
        Result := Self.Create(FilePath);
      end);
  finally
    Free;
  end;
end;

procedure TBatcheRoute.AsynDelete(const Id: string;
  CallBacks: TFunc<TAsynBatchDelete>);
begin
  with TAsynCallBackExec<TAsynBatchDelete, TBatchDelete>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchDelete
      begin
        Result := Self.Delete(Id);
      end);
  finally
    Free;
  end;
end;

procedure TBatcheRoute.AsynList(ParamProc: TProc<TListParams>;
  CallBacks: TFunc<TAsynBatcheList>);
begin
  with TAsynCallBackExec<TAsynBatcheList, TBatcheList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatcheList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TBatcheRoute.AsynRetrieve(const Id: string; FileName: string;
  CallBacks: TFunc<TAsynStringList>);
begin
  with TAsynCallBackExec<TAsynStringList, TStringList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TStringList
      begin
        Result := Self.Retrieve(Id, FileName);
      end);
  finally
    Free;
  end;
end;

procedure TBatcheRoute.ASynRetrieve(const Id: string;
  CallBacks: TFunc<TAsynBatche>);
begin
  with TAsynCallBackExec<TAsynBatche, TBatche>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatche
      begin
        Result := Self.Retrieve(Id);
      end);
  finally
    Free;
  end;
end;

procedure TBatcheRoute.AsynList(CallBacks: TFunc<TAsynBatcheList>);
begin
  with TAsynCallBackExec<TAsynBatcheList, TBatcheList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatcheList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

function TBatcheRoute.Cancel(const Id: string): TBatche;
begin
  Result := API.Post<TBatche>(Format('messages/batches/%s/cancel', [Id]));
end;

function TBatcheRoute.Create(const FilePath: string): TBatche;
begin
  Result := Create(
    procedure (Params: TRequestParams)
    begin
      Params.Requests(FilePath);
    end);
end;

function TBatcheRoute.Create(ParamProc: TProc<TRequestParams>): TBatche;
begin
  Result := API.Post<TBatche, TRequestParams>('messages/batches', ParamProc);
end;

function TBatcheRoute.Create(Value: TJSONObject): TBatche;
begin
  Result := API.Post<TBatche>('messages/batches', Value);
end;

function TBatcheRoute.Delete(const Id: string): TBatchDelete;
begin
  Result := API.Delete<TBatchDelete>('messages/batches/' + Id);
end;

function TBatcheRoute.List: TBatcheList;
begin
  Result := API.Get<TBatcheList>('messages/batches');
end;

function TBatcheRoute.List(ParamProc: TProc<TListParams>): TBatcheList;
begin
  Result := API.Get<TBatcheList, TListParams>('messages/batches', ParamProc);
end;

function TBatcheRoute.Retrieve(const Id: string): TBatche;
begin
  Result := API.Get<TBatche>('messages/batches/' + Id);
end;

function TBatcheRoute.Retrieve(const Id: string; FileName: string): TStringList;
begin
  Result := TStringList.Create;
  with Result do
  begin
    var Response := API.Get(Format('messages/batches/%s/results', [Id]));
    Text := Response;
    SaveToFile(FileName, TEncoding.UTF8);
  end;
end;

{ TBatcheList }

destructor TBatcheList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TBatche }

destructor TBatche.Destroy;
begin
  if Assigned(FRequestCounts) then
    FRequestCounts.Free;
  inherited;
end;

{ TListParams }

function TListParams.AfterId(const Value: string): TListParams;
begin
  Result := TListParams(Add('after_id', Value));
end;

function TListParams.BeforeId(const Value: string): TListParams;
begin
  Result := TListParams(Add('before_id', Value));
end;

function TListParams.Limite(const Value: Integer): TListParams;
begin
  Result := TListParams(Add('limit', Value));
end;

end.

