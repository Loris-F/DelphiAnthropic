unit Anthropic.Models;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Anthropic.API.Params, Anthropic.API, Anthropic.Async.Support;

type
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
  TListModelsParams = class(TUrlParam)
  public
    /// <summary>
    /// Number of items to return per page.
    /// </summary>
    /// <param name="Value">
    /// Defaults to 20. Ranges from 1 to 1000.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListModelsParams</c> with the specified limit.
    /// </returns>
    /// <exception cref="Exception">
    /// Thrown if the value is less than 1 or greater than 100.
    /// </exception>
    /// <remarks>
    /// The default value of limit set to 20.
    /// </remarks>
    function Limite(const Value: Integer): TListModelsParams;
    /// <summary>
    /// ID of the object to use as a cursor for pagination. When provided, returns the page of results immediately after this object.
    /// </summary>
    /// <param name="Value">
    /// A string representing the model ID.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListModelsParams</c> with the specified <c>after_id</c> value.
    /// </returns>
    function AfterId(const Value: string): TListModelsParams;
    /// <summary>
    /// ID of the object to use as a cursor for pagination. When provided, returns the page of results immediately before this object.
    /// </summary>
    /// <param name="Value">
    /// A string representing the model ID.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListModelsParams</c> with the specified <c>before_id</c> value.
    /// </returns>
    function BeforeId(const Value: string): TListModelsParams;
  end;

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
  TModel = class
  private
    FType: string;
    FId: string;
    [JsonNameAttribute('display_name')]
    FDisplayName: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
  public
    /// <summary>
    /// For Models, this is always "model".
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// Unique model identifier.
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// A human-readable name for the model.
    /// </summary>
    property DisplayName: string read FDisplayName write FDisplayName;
    /// <summary>
    /// RFC 3339 datetime string representing the time at which the model was released.
    /// May be set to an epoch value if the release date is unknown.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;
  end;

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
  TModels = class
  private
    FData: TArray<TModel>;
    [JsonNameAttribute('has_more')]
    FHasMore: Boolean;
    [JsonNameAttribute('first_id')]
    FFirstId: string;
    [JsonNameAttribute('last_id')]
    FLastId: string;
  public
    /// <summary>
    /// The array of models retrieved in the current query.
    /// </summary>
    property Data: TArray<TModel> read FData write FData;
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
    destructor Destroy; override;
  end;

  /// <summary>
  /// The <c>TAsynModel</c> class is a type alias used to handle asynchronous callbacks for batch processing.
  /// It provides support for executing batch operations asynchronously and processing the results upon completion.
  /// </summary>
  /// <remarks>
  /// This class is part of the asynchronous framework that allows non-blocking batch operations.
  /// It uses a callback mechanism to return the result of a batch process once it is completed.
  /// </remarks>
  TAsynModel = TAsynCallBack<TModel>;

  /// <summary>
  /// The <c>TAsynModels</c> class is a type alias used to handle asynchronous callbacks for batch processing.
  /// It provides support for executing batch operations asynchronously and processing the results upon completion.
  /// </summary>
  /// <remarks>
  /// This class is part of the asynchronous framework that allows non-blocking batch operations.
  /// It uses a callback mechanism to return the result of a batch process once it is completed.
  /// </remarks>
  TAsynModels = TAsynCallBack<TModels>;

  /// <summary>
  /// The <c>TModelsRoute</c> class provides methods for interacting with the Anthropic API
  /// to retrieve and manage models. It offers both synchronous and asynchronous operations
  /// for listing and retrieving individual models.
  /// </summary>
  /// <remarks>
  /// This class serves as a high-level interface for accessing model-related API endpoints.
  /// It extends the <c>TAnthropicAPIRoute</c> class, inheriting base functionalities
  /// while adding methods specific to model operations.
  /// <para>
  /// Key functionalities include:
  /// <list type="bullet">
  /// <item><description>Listing all available models using <c>List</c> and <c>AsynList</c> methods.</description></item>
  /// <item><description>Retrieving details of a specific model by its ID using <c>Retrieve</c> and <c>AsynRetrieve</c> methods.</description></item>
  /// </list>
  /// </para>
  /// <para>
  /// Asynchronous methods (<c>AsynList</c> and <c>AsynRetrieve</c>) allow the application
  /// to handle API responses in a non-blocking manner, suitable for UI-intensive scenarios.
  /// Synchronous methods (<c>List</c> and <c>Retrieve</c>) block execution until the API response is received.
  /// </para>
  /// </remarks>
  TModelsRoute = class(TAnthropicAPIRoute)
    /// <summary>
    /// List available models.
    /// The Models API response can be used to determine which models are available for use in the API.
    /// More recently released models are listed first.
    /// </summary>
    /// <param name="CallBacks">
    /// A function that returns <c>TAsynModels</c> to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// <para>
    /// The <c>CallBacks</c> function is invoked when the operation completes, either successfully or
    /// with an error.
    /// </para>
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// Anthropic.Models.AsynList(
    ///   function : TAsynModels
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
    ///        procedure (Sender: TObject; Value: TModels)
    ///        begin
    ///          // Handle the display
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
    procedure AsynList(CallBacks: TFunc<TAsynModels>); overload;
    /// <summary>
    /// List available models.
    /// The Models API response can be used to determine which models are available for use in the API.
    /// More recently released models are listed first.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TListModelsParams</c> parameters.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns <c>TAsynModels</c> to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// <para>
    /// The <c>CallBacks</c> function is invoked when the operation completes, either successfully or
    /// with an error.
    /// </para>
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// Anthropic.Models.AsynList(
    ///   procedure (Params: TListModelsParams)
    ///   begin
    ///     // Define parameters
    ///   end,
    ///
    ///   function : TAsynModels
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
    ///        procedure (Sender: TObject; Value: TModels)
    ///        begin
    ///          // Handle the display
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
    procedure AsynList(ParamProc: TProc<TListModelsParams>; CallBacks: TFunc<TAsynModels>); overload;
    /// <summary>
    /// Get a specific model.
    /// The Models API response can be used to determine information about a specific model or
    /// resolve a model alias to a model ID.
    /// </summary>
    /// <param name="ModelId">
    /// Model identifier or alias.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns <c>TAsynModel</c> to handle the asynchronous result.
    /// </param>
    /// <remarks>
    /// <para>
    /// The <c>CallBacks</c> function is invoked when the operation completes, either successfully or
    /// with an error.
    /// </para>
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// Anthropic.Models.AsynRetrieve(ModelId,
    ///   function : TAsynModels
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
    ///        procedure (Sender: TObject; Value: TModel)
    ///        begin
    ///          // Handle the display
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
    procedure AsynRetrieve(const ModelId: string; CallBacks: TFunc<TAsynModel>);
    /// <summary>
    /// List available models.
    /// The Models API response can be used to determine which models are available for use in the API.
    /// More recently released models are listed first.
    /// </summary>
    /// <returns>
    /// A <c>TModels</c> object containing the list of models.
    /// </returns>
    /// <remarks>
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// var Value := Anthropic.Models.List;
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List: TModels; overload;
    /// <summary>
    /// List available models.
    /// The Models API response can be used to determine which models are available for use in the API.
    /// More recently released models are listed first.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <c>TListModelsParams</c> parameters.
    /// </param>
    /// <returns>
    /// A <c>TModels</c> object containing the list of models.
    /// </returns>
    /// <remarks>
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// var Value := Anthropic.Models.List(
    ///     procedure (Params: TListModelsParams)
    ///     begin
    ///       // Define parameters
    ///     end;
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List(ParamProc: TProc<TListModelsParams>): TModels; overload;
    /// <summary>
    /// Get a specific model.
    /// The Models API response can be used to determine information about a specific model or
    /// resolve a model alias to a model ID.
    /// </summary>
    /// <param name="ModelId">
    /// Model identifier or alias.
    /// </param>
    /// <returns>
    /// A <c>TModel</c> object containing the model data.
    /// </returns>
    /// <remarks>
    /// <code>
    /// //WARNING - Move the following line into the main OnCreate
    /// //var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);
    /// var Value := Anthropic.Models.Retrieve(ModelId);
    /// try
    ///   // Handle the Value
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Retrieve(const ModelId: string): TModel;
  end;

implementation

{ TModels }

destructor TModels.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TListModelsParams }

function TListModelsParams.AfterId(const Value: string): TListModelsParams;
begin
  Result := TListModelsParams(Add('after_id', Value));
end;

function TListModelsParams.BeforeId(const Value: string): TListModelsParams;
begin
  Result := TListModelsParams(Add('before_id', Value));
end;

function TListModelsParams.Limite(const Value: Integer): TListModelsParams;
begin
  Result := TListModelsParams(Add('limit', Value));
end;

{ TModelsRoute }

function TModelsRoute.List: TModels;
begin
  Result := API.Get<TModels>('models');
end;

procedure TModelsRoute.AsynList(CallBacks: TFunc<TAsynModels>);
begin
  with TAsynCallBackExec<TAsynModels, TModels>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModels
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsynList(ParamProc: TProc<TListModelsParams>;
  CallBacks: TFunc<TAsynModels>);
begin
  with TAsynCallBackExec<TAsynModels, TModels>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModels
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TModelsRoute.AsynRetrieve(const ModelId: string;
  CallBacks: TFunc<TAsynModel>);
begin
  with TAsynCallBackExec<TAsynModel, TModel>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModel
      begin
        Result := Self.Retrieve(ModelId);
      end);
  finally
    Free;
  end;
end;

function TModelsRoute.List(ParamProc: TProc<TListModelsParams>): TModels;
begin
  Result := API.Get<TModels, TListModelsParams>('models', ParamProc);
end;

function TModelsRoute.Retrieve(const ModelId: string): TModel;
begin
  Result := API.Get<TModel>('models/' + ModelId);
end;

end.
