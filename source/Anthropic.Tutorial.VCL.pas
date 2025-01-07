unit Anthropic.Tutorial.VCL;

{ Tutorial Support Unit

   WARNING:
     This module is intended solely to illustrate the examples provided in the
     README.md file of the repository :
          https://github.com/MaxiDonkey/DelphiAnthropic
     Under no circumstances should the methods described below be used outside
     of the examples presented on the repository's page.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.UITypes,
  Anthropic, Anthropic.Types, Anthropic.API.Params;

type
  TToolProc = procedure (const Value: string) of object;

  /// <summary>
  /// Represents a tutorial hub for handling visual components in a Delphi application,
  /// including text display, button interactions, and navigation through pages.
  /// </summary>
  TVCLTutorialHub = class
  private
    FMemo1: TMemo;
    FButton: TButton;
    FBatchId: string;
    FFileName: string;
    FTool: IFunctionCore;
    FToolCall: TToolProc;
    FCancel: Boolean;
    FJSONParam: TJSONParam;
    procedure OnButtonClick(Sender: TObject);
    procedure SetButton(const Value: TButton);
    procedure SetMemo1(const Value: TMemo);
  public
    /// <summary>
    /// Gets or sets the first memo component for displaying messages or data.
    /// </summary>
    property Memo1: TMemo read FMemo1 write SetMemo1;
    /// <summary>
    /// Gets or sets the button component used to trigger actions or handle cancellation.
    /// </summary>
    property Button: TButton read FButton write SetButton;
    /// <summary>
    /// Gets or sets a value indicating whether the operation has been canceled.
    /// </summary>
    property Cancel: Boolean read FCancel write FCancel;
    /// <summary>
    /// Gets or sets the TJSONParam object associated with the tutorial hub.
    /// </summary>
    property JSONParam: TJSONParam read FJSONParam write FJSONParam;
    /// <summary>
    /// Gets or sets the batch identifier associated with the tutorial hub.
    /// </summary>
    property BatchId: string read FBatchId write FBatchId;
    /// <summary>
    /// Gets or sets the name of the file associated with the tutorial hub.
    /// </summary>
    property FileName: string read FFileName write FFileName;
    /// <summary>
    /// Gets or sets the core function tool used for processing.
    /// </summary>
    property Tool: IFunctionCore read FTool write FTool;
    /// <summary>
    /// Gets or sets the procedure for handling tool-specific calls.
    /// </summary>
    property ToolCall: TToolProc read FToolCall write FToolCall;
    constructor Create(const AMemo1: TMemo; const AButton: TButton);
  end;

  procedure Cancellation(Sender: TObject);
  function DoCancellation: Boolean;
  procedure Start(Sender: TObject);

  procedure Display(Sender: TObject); overload;
  procedure Display(Sender: TObject; Value: string); overload;
  procedure Display(Sender: TObject; Value: TArray<string>); overload;
  procedure Display(Sender: TObject; Value: TChat); overload;
  procedure Display(Sender: TObject; Value: TModel); overload;
  procedure Display(Sender: TObject; Value: TModels); overload;
  procedure Display(Sender: TObject; Value: TChatUsage); overload;
  procedure Display(Sender: TObject; Value: TBatcheList); overload;
  procedure Display(Sender: TObject; Value: TBatche); overload;
  procedure Display(Sender: TObject; Value: TBatchDelete); overload;
  procedure Display(Sender: TObject; Value: TStringList); overload;
  procedure Display(Sender: TObject; Value: IBatcheResults); overload;
  procedure Display(Sender: TObject; Value: TTokenCount); overload;

  procedure DisplayStream(Sender: TObject; Value: string); overload;
  procedure DisplayStream(Sender: TObject; Value: TChat); overload;

  procedure DisplayUsage(Sender: TObject; Value: TChat);

  function F(const Name, Value: string): string; overload;
  function F(const Name: string; const Value: TArray<string>): string; overload;
  function F(const Name: string; const Value: boolean): string; overload;
  function F(const Name: string; const State: Boolean; const Value: Double): string; overload;

var
  /// <summary>
  /// A global instance of the <see cref="TVCLTutorialHub"/> class used as the main tutorial hub.
  /// </summary>
  /// <remarks>
  /// This variable serves as the central hub for managing tutorial components, such as memos, buttons, and pages.
  /// It is initialized dynamically during the application's runtime, and its memory is automatically released during
  /// the application's finalization phase.
  /// </remarks>
  TutorialHub: TVCLTutorialHub = nil;

implementation

uses
  System.DateUtils;

function UnixIntToDateTime(const Value: Int64): TDateTime;
begin
  Result := TTimeZone.Local.ToLocalTime(UnixToDateTime(Value));
end;

function UnixDateTimeToString(const Value: Int64): string;
begin
  Result := DateTimeToStr(UnixIntToDateTime(Value))
end;

procedure Cancellation(Sender: TObject);
begin
  Display(Sender, 'The operation was cancelled');
  Display(Sender);
  TutorialHub.Cancel := False;
end;

function DoCancellation: Boolean;
begin
  Result := TutorialHub.Cancel;
end;

procedure Start(Sender: TObject);
begin
  Display(Sender, 'Please wait...');
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: string);
var
  M: TMemo;
begin
  if Sender is TMemo then
    M := TMemo(Sender) else
    M := (Sender as TVCLTutorialHub).Memo1;

  var S := Value.Split([#10]);
  if Length(S) = 0 then
    begin
      M.Lines.Add(Value)
    end
  else
    begin
      for var Item in S do
        M.Lines.Add(Item);
    end;

  M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure Display(Sender: TObject; Value: TArray<string>);
begin
  var index := 0;
  for var Item in Value do
    begin
      if not Item.IsEmpty then
        begin
          if index = 0 then
            Display(Sender, Item) else
            Display(Sender, '    ' + Item);
        end;
      Inc(index);
    end;
end;

procedure Display(Sender: TObject);
begin
  Display(Sender, sLineBreak);
end;

procedure Display(Sender: TObject; Value: TChat);
begin
  for var Item in Value.Content do
    begin
      if Item.&Type = 'text' then
          begin
            Display(Sender, Item.Text);
            DisplayUsage(Sender, Value);
          end
        else
        if Item.&Type = 'tool_use' then
          begin
            if Assigned(TutorialHub.ToolCall) then
              TutorialHub.ToolCall(TutorialHub.Tool.Execute(Item.Input));
          end;
    end;
end;

procedure Display(Sender: TObject; Value: TModel);
begin
  Display(Sender, [
    Value.Id,
    F('Type', Value.&Type),
    F('DisplayName', Value.DisplayName),
    F('CreatedAt', Value.CreatedAt)
  ]);
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TModels);
begin
  if Length(Value.Data) = 0 then
    begin
      Display(Sender, 'No model found');
      Exit;
    end;
  for var Item in Value.Data do
    begin
      Display(Sender, Item);
      Application.ProcessMessages;
    end;
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TChatUsage);
begin
  Display(Sender, [F('input_tokens', [Value.InputTokens.ToString,
      F('output_tokens', Value.OutputTokens.ToString),
      F('cache_creation_input_tokens', Value.CacheCreationInputTokens.ToString),
      F('cache_read_input_tokens', Value.CacheReadInputTokens.ToString)
   ])]);
  Display(Sender)
end;

procedure Display(Sender: TObject; Value: TBatche);
begin
  Display(Sender, [EmptyStr,
    Value.Id,
    F('Type', Value.&Type),
    F('Processing_status', Value.ProcessingStatus.ToString),
    F('CreatedAt', Value.CreatedAt),
    F('ExpiresAt', Value.ExpiresAt),
    F('CancelInitiatedAt', Value.CancelInitiatedAt),
    F('ResultsUrl', Value.ResultsUrl),
    F('Processing', Value.RequestCounts.Processing.ToString),
    F('Succeeded', Value.RequestCounts.Succeeded.ToString),
    F('Errored', Value.RequestCounts.Errored.ToString),
    F('Canceled', Value.RequestCounts.Canceled.ToString),
    F('Expired', Value.RequestCounts.Expired.ToString)
  ]);
  Display(Sender, EmptyStr);
  if Assigned(TutorialHub.JSONParam) then
    FreeAndNil(TutorialHub.JSONParam);
end;

procedure Display(Sender: TObject; Value: TBatchDelete);
begin
  Display(Sender, F('Id', [
     Value.Id,
     F('Type', Value.&Type)
  ]));
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TBatcheList);
begin
  Display(Sender, F('HasMore', BoolToStr(Value.HasMore, True)));
  Display(Sender, F('FirstId', Value.FirstId));
  Display(Sender, F('LastId', Value.LastId));
  Display(Sender, EmptyStr);

  for var Item in Value.Data do
    begin
      Display(Sender, [EmptyStr,
        F('Id', [
          Item.Id,
          Item.ProcessingStatus.ToString
        ])
      ]);
    end;
  Display(Sender);
end;

procedure Display(Sender: TObject; Value: TStringList);
begin
  with Value.GetEnumerator do
  try
    while MoveNext do
      Display(Sender, Current);
  finally
    Free;
    Display(Sender);
  end;
end;

procedure Display(Sender: TObject; Value: IBatcheResults);
begin
  for var Item in Value.Batches do
    Display(Sender, F(Item.CustomId, Item.Result.Message.Content[0].Text));
  Display(Sender, EmptyStr);
end;

procedure Display(Sender: TObject; Value: TTokenCount);
begin
  Display(Sender, F('Input_tokens', Value.InputTokens.ToString));
end;

procedure DisplayStream(Sender: TObject; Value: string);
var
  M: TMemo;
  CurrentLine: string;
  Lines: TArray<string>;
begin
  if Sender is TMemo then
    M := TMemo(Sender) else
    M := (Sender as TVCLTutorialHub).Memo1;
  var OldSelStart := M.SelStart;
  var ShouldScroll := (OldSelStart = M.GetTextLen);
  M.Lines.BeginUpdate;
  try
    Lines := Value.Split([#10]);
    if Length(Lines) > 0 then
    begin
      if M.Lines.Count > 0 then
        CurrentLine := M.Lines[M.Lines.Count - 1]
      else
        CurrentLine := '';
      CurrentLine := CurrentLine + Lines[0];
      if M.Lines.Count > 0 then
        M.Lines[M.Lines.Count - 1] := CurrentLine
      else
        M.Lines.Add(CurrentLine);
      for var i := 1 to High(Lines) do
        M.Lines.Add(Lines[i]);
    end;
  finally
    M.Lines.EndUpdate;
  end;
  if ShouldScroll then
  begin
    M.SelStart := M.GetTextLen;
    M.SelLength := 0;
    M.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure DisplayStream(Sender: TObject; Value: TChat);
begin
  if Assigned(Value) then
    begin
      if Value.Delta.&Type = 'tool_use' then
        begin
          if Assigned(TutorialHub.ToolCall) then
            TutorialHub.ToolCall(TutorialHub.Tool.Execute(Value.Delta.Input));
        end
      else
        begin
          DisplayStream(Sender, Value.Delta.Text);
        end;
    end;
end;

procedure DisplayUsage(Sender: TObject; Value: TChat);
begin
  Display(Sender, Value.Usage);
end;

function F(const Name, Value: string): string;
begin
  if not Value.IsEmpty then
    Result := Format('%s: %s', [Name, Value])
end;

function F(const Name: string; const Value: TArray<string>): string;
begin
  var index := 0;
  for var Item in Value do
    begin
      if index = 0 then
        Result := Format('%s: %s', [Name, Item]) else
        Result := Result + '    ' + Item;
      Inc(index);
    end;
end;

function F(const Name: string; const Value: boolean): string;
begin
  Result := Format('%s: %s', [Name, BoolToStr(Value, True)])
end;

function F(const Name: string; const State: Boolean; const Value: Double): string;
begin
  Result := Format('%s (%s): %s%%', [Name, BoolToStr(State, True), (Value * 100).ToString(ffNumber, 3, 2)])
end;

{ TVCLTutorialHub }

constructor TVCLTutorialHub.Create(const AMemo1: TMemo; const AButton: TButton);
begin
  inherited Create;
  Memo1 := AMemo1;
  Button := AButton;
  JSONParam := nil;
end;

procedure TVCLTutorialHub.OnButtonClick(Sender: TObject);
begin
  Cancel := True;
end;

procedure TVCLTutorialHub.SetButton(const Value: TButton);
begin
  FButton := Value;
  FButton.OnClick := OnButtonClick;
  FButton.Caption := 'Cancel';
end;

procedure TVCLTutorialHub.SetMemo1(const Value: TMemo);
begin
  FMemo1 := Value;
  FMemo1.ScrollBars := TScrollStyle.ssVertical;
end;

initialization
finalization
  if Assigned(TutorialHub) then
    TutorialHub.Free;
end.
