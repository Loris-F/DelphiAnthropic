unit Anthropic.Batches;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Anthropic.API.Params, Anthropic.API, Anthropic.Async.Support,
  Anthropic.Async.Params, Anthropic.Chat;

type
  /// <summary>
  /// Processing status of the Message Batch.
  /// </summary>
  TProcessingStatusType = (
    /// <summary>
    /// Batch of messages pending or being processed
    /// </summary>
    in_progress,
    /// <summary>
    /// Message batch processing canceled
    /// </summary>
    canceling,
    /// <summary>
    /// Batch processing is complete
    /// </summary>
    ended
  );

  /// <summary>
  /// Helper record for the <c>TProcessingStatusType</c> enumeration, providing utility methods for converting
  /// between <c>TProcessingStatusType</c> values and their string representations.
  /// </summary>
  TProcessingStatusTypeHelper = record helper for TProcessingStatusType
    /// <summary>
    /// Converts the current <c>TProcessingStatusType</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TProcessingStatusType</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Converts a string representation of a <c>TProcessingStatusType</c> into its corresponding enumeration value.
    /// </summary>
    /// <param name="Value">
    /// The string representing a <c>TTProcessingStatusType</c>.
    /// </param>
    /// <returns>
    /// The <c>TProcessingStatusType</c> enumeration value that corresponds to the provided string.
    /// </returns>
    class function Create(const Value: string): TProcessingStatusType; static;
  end;

  /// <summary>
  /// Interceptor class for converting <c>TTProcessingStatusType</c> values to and from their string representations in JSON serialization and deserialization.
  /// </summary>
  /// <remarks>
  /// This class is used to facilitate the conversion between the <c>TTProcessingStatusType</c> enum and its string equivalents during JSON processing.
  /// It extends the <c>TJSONInterceptorStringToString</c> class to override the necessary methods for custom conversion logic.
  /// </remarks>
  TProcessingStatusInterceptor = class(TJSONInterceptorStringToString)
    /// <summary>
    /// Converts the <c>TJSONInterceptorStringToString</c> value of the specified field to a string during JSON serialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be converted.
    /// </param>
    /// <param name="Field">
    /// The field name representing the <c>TJSONInterceptorStringToString</c> value.
    /// </param>
    /// <returns>
    /// The string representation of the <c>TJSONInterceptorStringToString</c> value.
    /// </returns>
    function StringConverter(Data: TObject; Field: string): string; override;
    /// <summary>
    /// Converts a string back to a <c>TJSONInterceptorStringToString</c> value for the specified field during JSON deserialization.
    /// </summary>
    /// <param name="Data">
    /// The object containing the field to be set.
    /// </param>
    /// <param name="Field">
    /// The field name where the <c>TJSONInterceptorStringToString</c> value will be set.
    /// </param>
    /// <param name="Arg">
    /// The string representation of the <c>TJSONInterceptorStringToString</c> to be converted back.
    /// </param>
    /// <remarks>
    /// This method converts the string argument back to the corresponding <c>TJSONInterceptorStringToString</c> value and assigns it to the specified field in the object.
    /// </remarks>
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

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
    function Requests(Value: TArray<TBatcheParams>): TRequestParams;
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
    Fsucceeded: Int64;
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
    property succeeded: Int64 read Fsucceeded write Fsucceeded;
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
  TListParams = record
  private
    FLimite: Integer;
    FAfterId: string;
    FBeforeId: string;
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
    /// <summary>
    /// Converts the current instance of <c>TListParams</c> to its string representation
    /// for use in building request URLs.
    /// </summary>
    /// <returns>
    /// A string representing the parameters formatted as query parameters for a URL.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a new instance of the <c>TListParams</c> structure with default values.
    /// </summary>
    /// <returns>
    /// A new instance of <c>TListParams</c> with default settings.
    /// </returns>
    class function Create: TListParams; static;
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
    /// </remarks>
    procedure AsynCreate(ParamProc: TProc<TRequestParams>; CallBacks: TFunc<TAsynBatche>); overload;
    /// <summary>
    /// Creates a batch request asynchronously using a <c>TJSONObject</c>.
    /// </summary>
    /// <param name="Value">The JSON object containing the parameters for the batch request.</param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatche</c> record for handling asynchronous events, such as on success or on error.
    /// </param>
    /// <remarks>
    /// This method allows for creating a batch request asynchronously using a <c>TJSONObject</c> to specify the parameters.
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
    /// </remarks>
    procedure AsynList(CallBacks: TFunc<TAsynBatcheList>); overload;
    /// <summary>
    /// Lists batches asynchronously using specific parameters.
    /// </summary>
    /// <param name="Params">
    /// A <c>TListParams</c> object containing filtering and paging options for batch listing.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatcheList</c> record for handling asynchronous batch listing, including success and error events.
    /// </param>
    /// <remarks>
    /// This method allows for retrieving a list of batches asynchronously, with the ability to apply specific filters and paging parameters.
    /// </remarks>
    procedure AsynList(const Params: TListParams; CallBacks: TFunc<TAsynBatcheList>); overload;
    /// <summary>
    /// Cancels a batch operation asynchronously by its ID.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to be canceled.</param>
    /// <param name="CallBacks">
    /// A function that returns a <c>TAsynBatche</c> record to handle success or error during the asynchronous cancellation.
    /// </param>
    /// <remarks>
    /// This method cancels a batch process asynchronously using its unique ID. The result or any error is managed by the callbacks provided.
    /// </remarks>
    procedure AsynCancel(const Id: string; CallBacks: TFunc<TAsynBatche>);
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
    /// </remarks>
    function Create(ParamProc: TProc<TRequestParams>): TBatche; overload;
    /// <summary>
    /// Creates a batch request synchronously using a <c>TJSONObject</c>.
    /// </summary>
    /// <param name="Value">The JSON object containing the parameters for the batch request.</param>
    /// <returns>
    /// A <c>TBatche</c> object containing the result of the created batch request.
    /// </returns>
    /// <remarks>
    /// This method sends a batch creation request synchronously using a <c>TJSONObject</c> to specify parameters and returns the result.
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
    /// </remarks>
    function List: TBatcheList; overload;
    /// <summary>
    /// Lists batches synchronously using specific parameters.
    /// </summary>
    /// <param name="Params">
    /// A <c>TListParams</c> object containing filtering and paging options for batch listing.
    /// </param>
    /// <returns>
    /// A <c>TBatcheList</c> object containing the filtered list of batches.
    /// </returns>
    /// <remarks>
    /// This method retrieves a list of batches synchronously, with the option to apply specific filters and paging parameters.
    /// </remarks>
    function List(const Params: TListParams): TBatcheList; overload;
    /// <summary>
    /// Cancels a batch operation synchronously by its ID.
    /// </summary>
    /// <param name="Id">The unique identifier of the batch to be canceled.</param>
    /// <returns>
    /// A <c>TBatche</c> object containing the result of the cancellation request.
    /// </returns>
    /// <remarks>
    /// This method cancels a batch process synchronously by its unique ID.
    /// </remarks>
    function Cancel(const Id: string): TBatche;
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
      ParamProc(Data);
    Result := TBatcheParams(Add('params', Data as TJSONParam));
  finally
    Data.Free;
  end;
end;

{ TRequestParams }

function TRequestParams.Requests(Value: TArray<TBatcheParams>): TRequestParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Batche in Value do
    if Assigned(Batche) then
      begin
        var JSON := TJSONObject.ParseJSONValue(Batche.ToJsonString(False)) as TJSONObject;
        JSONArray.Add(JSON);
        Batche.Free;
      end;
  Result := TRequestParams(Add('requests', JSONArray));
end;

{ TProcessingStatusTypeHelper }

class function TProcessingStatusTypeHelper.Create(
  const Value: string): TProcessingStatusType;
begin
  var index := IndexStr(AnsiLowerCase(Value), ['in_progress', 'canceling', 'ended']);
  if index = -1 then
    raise Exception.CreateFmt('TProcessingStatusType(%s): Type conversion failed', [Value]);
  Result := TProcessingStatusType(index);
end;

function TProcessingStatusTypeHelper.ToString: string;
begin
  case Self of
    in_progress:
      Exit('in_progress');
    canceling:
      Exit('canceling');
    ended:
      Exit('ended');
  end;
end;

{ TProcessingStatusInterceptor }

function TProcessingStatusInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  Result := RTTI.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TProcessingStatusType>.ToString;
end;

procedure TProcessingStatusInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  RTTI.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From(TProcessingStatusType.Create(Arg)));
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

procedure TBatcheRoute.AsynList(const Params: TListParams;
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
        Result := Self.List(Params);
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

function TBatcheRoute.Create(ParamProc: TProc<TRequestParams>): TBatche;
begin
  Result := API.Post<TBatche, TRequestParams>('messages/batches', ParamProc);
end;

function TBatcheRoute.Create(Value: TJSONObject): TBatche;
begin
  Result := API.Post<TBatche>('messages/batches', Value);
end;

function TBatcheRoute.List: TBatcheList;
begin
  Result := API.Get<TBatcheList>('messages/batches');
end;

function TBatcheRoute.List(const Params: TListParams): TBatcheList;
begin
  Result := API.Get<TBatcheList>(Format('messages/batches%s', [Params.ToString]));
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

{ TListParams }

function TListParams.AfterId(const Value: string): TListParams;
begin
  FAfterId := Value;
  Result := Self;
end;

function TListParams.BeforeId(const Value: string): TListParams;
begin
  FBeforeId := Value;
  Result := Self;
end;

class function TListParams.Create: TListParams;
begin
  Result.FLimite := 20;
  Result.FAfterId := '';
  Result.FBeforeId := '';
end;

function TListParams.Limite(const Value: Integer): TListParams;
begin
  if (Value <= 0) or (Value > 100) then
    raise Exception.Create('Limit out of bounds [1..100]');
  FLimite := Value;
  Result := Self;
end;

function TListParams.ToString: string;
var
  S1, S2, S3: string;

  function Delimiter(const Value: Boolean): string;
  begin
    if Value then
      Result := '?' else
      Result := '&&';
  end;

begin
  if (FLimite <> 20) and (FLimite > 0) and (FLimite < 100) then
    S1 := Format('?limit=%d', [FLimite]);

  if not FBeforeId.IsEmpty then
    S2 := Format(Delimiter(S1.IsEmpty) + 'before_id=%s', [FBeforeId]);

  if not FAfterId.IsEmpty then
    S3 := Format(Delimiter(S1.IsEmpty and S2.IsEmpty) + 'after_id=%s', [FAfterId]);

  Result := Format('%s%s%s', [S1, S2, S3]);
end; {ToString}

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

end.
