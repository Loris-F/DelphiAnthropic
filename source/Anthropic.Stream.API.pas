unit Anthropic.Stream.API;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, Rest.Json, REST.JsonReflect, System.JSON, REST.Json.Types;

type
  /// <summary>
  /// Enumerates the possible categories for received messages in a chunked process.
  /// </summary>
  /// <remarks>
  /// This type categorizes incoming messages that are part of a chunked process, where messages and content blocks are sent in multiple parts (chunks).
  /// It helps identify the different stages or types of chunks, such as the start of a message, a delta update, or the end of a message.
  /// </remarks>
  TBlockType = (
    /// <summary>
    /// Indicates that the chunk does not match any expected patterns or models in the chunked process.
    /// </summary>
    btNone,
    /// <summary>
    /// Represents that the chunked message or event has finished, signaling the end of the chunked process.
    /// </summary>
    btDone,
    /// <summary>
    /// Represents a chunk that updates or modifies an existing content block, typically a partial or incremental update.
    /// </summary>
    btBlockDelta,
    /// <summary>
    /// Marks the beginning of a new chunked message in the process.
    /// </summary>
    btMessageStart,
    /// <summary>
    /// Represents an incremental update (delta) to a chunked message in progress.
    /// </summary>
    btMessageDelta,
    /// <summary>
    /// Marks the beginning of a new content block in the chunked process, where content is split into smaller chunks.
    /// </summary>
    btBlockStart);

  /// <summary>
  /// The response content made by a tool.
  /// </summary>
  TTool = record
  private
    FId: string;
    FName: string;
    FInput: string;
  public
    /// <summary>
    /// Id provided by the LLM
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// Arguments returned by the LLM, these arguments will have to be used to construct the final answer.
    /// </summary>
    property Input: string read FInput write FInput;
    /// <summary>
    /// Name of the tool identified by the LLM
    /// </summary>
    property Name: string read FName write FName;
  end;

  /// <summary>
  /// Represents the token usage statistics for a chat interaction, including the number of tokens
  /// used in the prompt, the completion, and the total number of tokens consumed.
  /// </summary>
  /// <remarks>
  /// The <c>TMessageUsage</c> class provides insight into the number of tokens used during a chat interaction.
  /// This information is critical for understanding the cost of a request when using token-based billing systems
  /// or for monitoring the model's behavior in terms of input (prompt) and output (completion) size.
  /// </remarks>
  TUsage = record
  private
    FInputTokens: Int64;
    FCacheCreationInputTokens: Int64;
    FCacheReadInputTokens: Int64;
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
  /// Record that provides tools for processing received message blocks in a chunked process.
  /// </summary>
  /// <remarks>
  /// The TDelta record encapsulates methods and properties to handle different types of blocks (chunks) such as message starts, deltas, and block starts.
  /// It also includes utility functions for determining the type of block and converting the block data to a string.
  /// </remarks>
  TDelta = record
  private
    /// <summary>
    /// Stores the template or structure of the message being processed.
    /// </summary>
    /// <remarks>
    /// The value is retrieved during a "messageStart", then this value is kept throughout the chunked process
    /// </remarks>
    FMessagePattern: string;
    /// <summary>
    /// Obtained value that can be formatted with FMessagePattern if necessary.
    /// </summary>
    /// <remarks>
    /// This value is used by the ToString function to return a result.
    /// </remarks>
    FValue: string;
    /// <summary>
    /// Record including tool data if tool_use was detected.
    /// </summary>
    FTool: TTool;
    /// <summary>
    /// Contains usage statistics or metadata related to the message block.
    /// </summary>
    FUsage: TUsage;
  public
    /// <summary>
    /// Processes a block delta, which is an update or modification to an existing block of data.
    /// </summary>
    /// <param name="Data">The raw data representing the block delta.</param>
    /// <returns>A new instance of TDelta with the processed block delta.</returns>
    function BlockDelta(const Data: string): TDelta;
    /// <summary>
    /// Processes the start of a new message block in the chunked process.
    /// </summary>
    /// <param name="Data">The raw data representing the start of a message.</param>
    /// <returns>A new instance of TDelta for the message start.</returns>
    function MessageStart(Data: string): TDelta;
    /// <summary>
    /// Processes an incremental update (delta) to an ongoing message.
    /// </summary>
    /// <param name="Data">The raw data representing the message delta.</param>
    /// <returns>A new instance of TDelta with the processed message delta.</returns>
    function MessageDelta(const Data: string): TDelta;
    /// <summary>
    /// Processes the start of a new content block in the chunked process.
    /// </summary>
    /// <param name="Data">The raw data representing the start of the content block.</param>
    /// <returns>A new instance of TDelta with the processed block start.</returns>
    function BlockStart(const Data: string): TDelta;
    /// <summary>
    /// Determines the type of block based on the provided line of data.
    /// </summary>
    /// <param name="Line">The raw line of data to analyze.</param>
    /// <returns>A TBlockType value representing the type of block.</returns>
    class function BlockType(const Line: string): TBlockType; static;
    /// <summary>
    /// Converts the message block to a formatted or unformatted string representation.
    /// </summary>
    /// <param name="IsFormated">Specifies whether the string should be formatted (True) or unformatted (False).</param>
    /// <returns>A string representation of the message block.</returns>
    function ToString(IsFormated: Boolean = True): string;
  end;

  /// <summary>
  /// Represents the token usage statistics for a chat interaction, including the number of tokens
  /// used in the prompt, the completion, and the total number of tokens consumed.
  /// </summary>
  /// <remarks>
  /// The <c>TMessageUsage</c> class provides insight into the number of tokens used during a chat interaction.
  /// This information is critical for understanding the cost of a request when using token-based billing systems
  /// or for monitoring the model's behavior in terms of input (prompt) and output (completion) size.
  /// </remarks>
  TMessageUsage = class
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

  TMessageContent = class
  private
    FId: string;
    FType: string;
    FRole: string;
    FModel: string;
    [JsonNameAttribute('stop_sequence')]
    FStopSequence: string;
    [JsonNameAttribute('stop_reason')]
    FStopReason: string;
    FContent: TArray<string>;
    FUsage: TMessageUsage;
  public
    property Id: string read FId write FId;
    property &Type: string read FType write FType;
    property Role: string read FRole write FRole;
    property Model: string read FModel write FModel;
    property StopSequence: string read FStopSequence write FStopSequence;
    property StopReason: string read FStopReason write FStopReason;
    property Content: TArray<string> read FContent write FContent;
    property Usage: TMessageUsage read FUsage write FUsage;
    destructor Destroy; override;
  end;

  TMessageStart = class
  private
    FType: string;
    FMessage: TMessageContent;
  public
    property &Type: string read FType write FType;
    property Message: TMessageContent read FMessage write FMessage;
    destructor Destroy; override;
  end;

  TDeltaContent = class
  private
    FType: string;
    FText: string;
    [JsonNameAttribute('partial_json')]
    FArgument: string;
  public
    property &Type: string read FType write FType;
    property Text: string read FText write FText;
    property Argument: string read FArgument write FArgument;
  end;

  TDeltaMessage = class
  private
    FType: string;
    FIndex: int64;
    FDelta: TDeltaContent;
  public
    property &Type: string read FType write FType;
    property &Index: int64 read FIndex write FIndex;
    property Delta: TDeltaContent read FDelta write FDelta;
    destructor Destroy; override;
  end;

  TContentBlock = class
  private
    FType: string;
    FId: string;
    FName: string;
    FInput: string;
  public
    property &Type: string read FType write FType;
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Input: string read FInput write FInput;
  end;

  TBlockStart = class
  private
    FType: string;
    FIndex: int64;
    [JsonNameAttribute('content_block')]
    FContentBlock: TContentBlock;
  public
    property &Type: string read FType write FType;
    property &Index: int64 read FIndex write FIndex;
    property ContentBlock: TContentBlock read FContentBlock write FContentBlock;
    destructor Destroy; override;
  end;

  function ExtractJSONPair(Value: string; Key: string; ToObject: Boolean): string;

implementation

const
  EMPTY_DELTA =
    '"delta":{"type":"text_delta","text":""}';
  DELTA_PARTIAL_FMT =
    '"delta":{"type":"%s","partial_json":"%s","id":"%s","name":"%s"}';
  DELTA_FMT =
    '"delta":{"type":"%s","text":"%s"}';
  USAGE_FMT =
    '"usage":{"input_tokens":%d,"cache_creation_input_tokens":%d,"cache_read_input_tokens":%d,"output_tokens":%d}';

type
  TDataHelper = class
    class function KeyToStr(const Key: string): string;
    class function ToolUsed: string;
  end;

resourcestring
  StringStartException = 'The string must start with the character ''{'' without exception.';
  InvalidJSONException = 'The JSON string is not valid.';

function ToEnd(const Value: string): Integer;
begin
  if Value[1] <> '{' then
    raise Exception.Create(StringStartException);
  Result := 2;
  var K := 1;
  while K > 0 do
    begin
      case Value[Result] of
        '{':
          Inc(k);
        '}':
          begin
            Dec(K);
            if k = 0 then
              Exit(Result);
          end;
      end;
      Inc(Result);
      if (Result > Value.Length) and (k > 0) then
        raise Exception.Create(InvalidJSONException);
    end;
end;

function ExtractJSONPair(Value: string; Key: string; ToObject: Boolean): string;
begin
  Key := Format('"%s":', [Key]);
  var Start := Value.IndexOf(Key) + Key.Length;
  Result :=  Value.Substring(Start).Trim;
  Result := Result.Trim.Substring(0, ToEnd(Result.Trim));
  if not ToObject then
    Result := Result.Substring(1, Result.Length - 2);
end;

function ToKeyStop(const Value: string): Integer;
begin
  if Value[1] <> '{' then
    begin
      var ComaPos :=  Value.IndexOf(',');
      if ComaPos > 0 then
        Exit( ComaPos + 1 ) else
        Exit( ComaPos );
    end;
  Result := ToEnd(Value);
  if Value[Result + 1] = ',' then
    Result := Result + 1;
end;

function DeleteJSONPair(Value: string; Key: string): string;
begin
  Key := Format('"%s":', [Key]);
  var StartPos := Value.IndexOf(Key);
  var Buffer := Value.Substring(StartPos + Key.Length);
  var L := StartPos + Key.Length + ToKeyStop(Buffer);
  Result := Value.Substring(0, StartPos) + Value.Substring(L);
end;

{ TDeltaMessage }

destructor TDeltaMessage.Destroy;
begin
  if Assigned(FDelta) then
    FDelta.Free;
  inherited;
end;

{ TDelta }

function TDelta.BlockDelta(const Data: string): TDelta;
begin
  {--- Retrieving the values of DeltaMessage from 'data' through the reflection process }
  var Block := TJson.JsonToObject<TDeltaMessage>(Data);
  try
    if Block.Delta.&Type.Equals('input_json_delta') then
      begin
        FValue := Format(DELTA_PARTIAL_FMT,
          [Block.&Type,
           Block.Delta.Argument.Replace('"', '\"'),
           FTool.Id,
           FTool.Name]);
        FTool.Input := FTool.Input + Block.Delta.Argument;
      end
    else
      FValue :=
        Format(DELTA_FMT, [Block.&Type, Block.Delta.Text.Replace('"', '\"')]);
  finally
    Result := Self;
    Block.Free;
  end;
end;

function TDelta.BlockStart(const Data: string): TDelta;
begin
  {--- Retrieving the values of BlockStart from 'data' through the reflection process }
  var Block := TJson.JsonToObject<TBlockStart>(Data);
  try
    FTool.Id := Block.ContentBlock.Id;
    FTool.Name := Block.ContentBlock.Name;
    FTool.Input := EmptyStr;
  finally
    Result := Self;
    Block.Free;
  end;
end;

class function TDelta.BlockType(const Line: string): TBlockType;
begin
  if Line.Contains(TDataHelper.KeyToStr('message_stop')) then
    Exit(btDone);
  if Line.StartsWith(TDataHelper.KeyToStr('content_block_delta')) then
    Exit(btBlockDelta);
  if Line.StartsWith(TDataHelper.KeyToStr('message_start')) then
    Exit(btMessageStart);
  if Line.StartsWith(TDataHelper.KeyToStr('message_delta')) then
    Exit(btMessageDelta);
  if Line.StartsWith(TDataHelper.KeyToStr('content_block_start')) and
     Line.Contains(TDataHelper.ToolUsed) then
    Exit(btBlockStart);
  Result := btNone;
end;

function TDelta.MessageDelta(const Data: string): TDelta;
begin
  {--- Retrieving from the 'delta' and 'usage' fields and their values from Data }
  var DeltaStr := ExtractJSONPair(Data, 'delta', False);
  var UsageStr := ExtractJSONPair(Data, 'usage', True);

  {--- Prepare a new pattern from FMessagePattern string }
  FValue := DeleteJSONPair(FMessagePattern, 'usage');
  FValue := DeleteJSONPair(FValue, 'stop_reason');
  FValue := DeleteJSONPair(FValue, 'stop_sequence');

  {--- Retrieving 'usage' values from UsageStr through the reflection process }
  var BlockUsage := TJson.JsonToObject<TMessageUsage>(UsageStr);
  try
    var Usage := Format(USAGE_FMT,
          [FUsage.FInputTokens,
           BlockUsage.FCacheCreationInputTokens,
           FUsage.CacheReadInputTokens,
           BlockUsage.OutputTokens]);

    {--- Build the parameter of the pattern from collected datas }
    var SubStr := EmptyStr;
    if not FTool.Input.IsEmpty then
      begin
        SubStr := Format(DELTA_PARTIAL_FMT,
          ['tool_use', FTool.Input.Replace('"', '\"'), FTool.Id, FTool.Name]);

        SubStr := Format('%s,%s, %s',
          [DeltaStr, Usage, SubStr]);
      end
    else
      SubStr := Format('%s,%s, %s', [DeltaStr, Usage, EMPTY_DELTA]);

    {--- Finalizing the result value in the FValue field using the pattern and the parameter }
    FValue := FValue.Format(FValue, [SubStr]);
    Result := Self;
  finally
    BlockUsage.Free;
  end;
end;

function TDelta.MessageStart(Data: string): TDelta;
begin
  {--- Retrieving 'message' values from 'data' through the reflection process and saving 'usage' values }
  var Start := TJson.JsonToObject<TMessageStart>(Data);
  try
    Self.FUsage.InputTokens := Start.Message.Usage.InputTokens;
    Self.FUsage.CacheCreationInputTokens := Start.Message.Usage.CacheCreationInputTokens;
    Self.FUsage.CacheReadInputTokens := Start.Message.Usage.CacheReadInputTokens;
    Self.FUsage.OutputTokens := Start.Message.Usage.OutputTokens;
  finally
    Start.Free;
  end;

  {--- Creating the pattern and saving it into FMessagePattern }
  Data := ExtractJSONPair(Data, 'message', True);
  Data := Data.Substring(0, Data.Trim.Length - 1);
  Data.Insert(Data.Length, ',%s}');
  Self.FMessagePattern := Data;

  {--- Finalizing the result value in the FValue field }
  Self.FValue := EMPTY_DELTA;
  Result := Self;
end;

function TDelta.ToString(IsFormated: Boolean): string;
begin
  if IsFormated then
    Result := Format(FMessagePattern, [FValue]) else
    Result := FValue;
end;

{ TBlockStart }

destructor TBlockStart.Destroy;
begin
  if Assigned(FContentBlock) then
    FContentBlock.Free;
  inherited;
end;

{ TMessageContent }

destructor TMessageContent.Destroy;
begin
  if Assigned(FUsage) then
    FUsage.Free;
  inherited;
end;

{ TMessageStart }

destructor TMessageStart.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  inherited;
end;

{ TDataHelper }

class function TDataHelper.KeyToStr(const Key: string): string;
begin
  Result := Format('data: {"type":"%s"', [Key]);
end;

class function TDataHelper.ToolUsed: string;
begin
  Result := '"content_block":{"type":"tool_use"';
end;

end.
