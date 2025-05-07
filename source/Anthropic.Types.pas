unit Anthropic.Types;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, Anthropic.API.Params;

type

{$REGION 'Anthropic.Chat'}

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
  /// Helper record for the <c>TStopReason</c> enumeration, providing utility methods for conversion between string representations and <c>TStopReason</c> values.
  /// </summary>
  TStopReasonHelper = record helper for TStopReason
    /// <summary>
    /// Converts the current <c>TStopReason</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TStopReason</c> value.
    /// </returns>
    function ToString: string;
    /// <summary>
    /// Creates a <c>TStopReason</c> value from its corresponding string representation.
    /// </summary>
    /// <param name="Value">
    /// The string value representing a <c>TStopReason</c>.
    /// </param>
    /// <returns>
    /// The corresponding <c>TStopReason</c> enumeration value for the provided string.
    /// </returns>
    /// <remarks>
    /// This method throws an exception if the input string does not match any valid <c>TStopReason</c> values.
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
  /// Indicator to specify how to use caching.
  /// </summary>
  TCachingType = (
    /// <summary>
    /// Cache is not used.
    /// </summary>
    nocaching,
    /// <summary>
    /// This is the only type currently defined by Anthropic.
    /// </summary>
    ephemeral
  );

  /// <summary>
  /// Helper record for the <c>TCachingType</c> enumeration, providing utility methods for converting
  /// between <c>TCachingType</c> values and their string representations.
  /// </summary>
  TCachingTypeHelper = record Helper for TCachingType
    /// <summary>
    /// Converts the current <c>TCachingType</c> value to its string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TCachingType</c> value.
    /// </returns>
    function ToString: string;
  end;

{$ENDREGION}

{$REGION 'Anthropic.Batches'}

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

{$ENDREGION}

{$REGION 'Anthropic.Schema'}

  /// <summary>
  /// Type contains the list of OpenAPI data types as defined by https://spec.openapis.org/oas/v3.0.3#data-types
  /// </summary>
  TSchemaType = (
    /// <summary>
    /// Not specified, should not be used.
    /// </summary>
    TYPE_UNSPECIFIED,
    /// <summary>
    /// String type.
    /// </summary>
    stSTRING,
    /// <summary>
    /// Number type.
    /// </summary>
    stNUMBER,
    /// <summary>
    /// Integer type.
    /// </summary>
    stINTEGER,
    /// <summary>
    /// Boolean type.
    /// </summary>
    stBOOLEAN,
    /// <summary>
    /// Array type.
    /// </summary>
    stARRAY,
    /// <summary>
    /// Object type.
    /// </summary>
    stOBJECT
  );

  /// <summary>
  /// Helper record for the <c>TSchemaType</c> enumeration, providing utility methods for converting
  /// between <c>TSchemaType</c> values and their string representations.
  /// </summary>
  TSchemaTypeHelper = record helper for TSchemaType
    /// <summary>
    /// Converts the current <c>TSchemaType</c> value to its corresponding string representation.
    /// </summary>
    /// <returns>
    /// A string representing the current <c>TSchemaType</c> value.
    /// </returns>
    function ToString: string;
  end;

{$ENDREGION}

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json;

{ TMessageRoleHelper }

class function TMessageRoleHelper.FromString(const Value: string): TMessageRole;
begin
  case IndexStr(Value.ToLower, ['user', 'assistant']) of
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

{ TCachingTypeHelper }

function TCachingTypeHelper.ToString: string;
begin
  case Self of
    nocaching:
      Exit('nocaching');
    ephemeral:
      Exit('ephemeral');
  end;
end;

{ TProcessingStatusTypeHelper }

class function TProcessingStatusTypeHelper.Create(
  const Value: string): TProcessingStatusType;
begin
  var index := IndexStr(AnsiLowerCase(Value), ['in_progress', 'canceling', 'ended']);
  if index = -1 then
    raise Exception.Create('Invalid processing status value.');
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

{ TSchemaTypeHelper }

function TSchemaTypeHelper.ToString: string;
begin
  case Self of
    TYPE_UNSPECIFIED:
      Exit('type_unspecified');
    stSTRING:
      Exit('string');
    stNUMBER:
      Exit('number');
    stINTEGER:
      Exit('integer');
    stBOOLEAN:
      Exit('boolean');
    stARRAY:
      Exit('array');
    stOBJECT:
      Exit('object');
  end;
end;

end.
