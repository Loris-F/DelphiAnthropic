# Delphi Anthropics API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20the%2010/11/2024-blue)

<br/>
<br/>

- [Introduction](#Introduction)
- [Remarks](#remarks)
- [Usage](#usage)
    - [Initialization](#initialization)
    - [Claude Models Overview](#Claude-Models-Overview)
    - [Embeddings](#embeddings)
    - [Chats](#chats)
        - [Create a message](#Create-a-message)
        - [Streaming messages](#Streaming-messages)
    - [Callbacks (asynchronous mode)](#Callbacks-asynchronous-mode)
    - [Asynchronous chat](#Asynchronous-chat)
        - [Create a message ASYNC](#Create-a-message-ASYNC)
        - [Streaming messages ASYNC](#Streaming-messages-ASYNC)
    - [Vision](#vision)
        - [Passing a Base64 Encoded Image](#Passing-a-base64-encoded-image)
    - [Function calling](#function-calling)
        - [Overview of Tool Use in Claude](#Overview-of-Tool-Use-in-Claude)
        - [Examples](#Examples)
- [Beta versions](#Beta-versions)
    - [Prompt Caching](#Prompt-Caching) 
        - [Caching initialization](#Caching-initialization)
        - [System Caching](#System-Caching)
        - [Tools Caching](#Tools-Caching)
        - [Images Caching](#Images-Caching)
    - [Message Batches](#Message-Batches) 
        - [Message Batches initialization](#Message-Batches-initialization)
        - [How it works](#How-it-works)
        - [Batch create](#Batch-create)
        - [Batch list](#Batch-list)
        - [Batch cancel](#Batch-cancel)
        - [Batch retrieve message](#Batch-retrieve-message)
        - [Batch retrieve results](#Batch-retrieve-results)  
        - [Batch asynchronous](#Batch-asynchronous)  
        - [Console](#Console) 
- [Contributing](#contributing)
- [License](#license)

<br/>
<br/>


## Introduction

Welcome to the unofficial Delphi **Anthropic** API library. This project aims to provide a `Delphi` interface for interacting with the **Anthropic** public API, making it easier to integrate advanced natural language processing features into your `Delphi` applications. Whether you want to generate text, create embeddings, use chat models, or generate code, this library offers a simple and effective solution.

**Anthropic** is a powerful natural language processing API that enables developers to incorporate advanced AI functionalities into their applications. For more details, visit the [official Anthropic documentation](https://docs.anthropic.com/en/docs/welcome/).

<br/>

## Remarks 

> [!IMPORTANT]
>
> This is an unofficial library. **Anthropic** does not provide any official library for `Delphi`.
> This repository contains `Delphi` implementation over [Anthropic](https://docs.anthropic.com/en/api/getting-started/) public API.

<br/>

## Usage

### Initialization

To initialize the API instance, you need to [obtain an API key from Anthropic](https://console.anthropic.com/settings/keys/).

Once you have a token, you can initialize `IAnthropic` interface, which is an entry point to the API.

Due to the fact that there can be many parameters and not all of them are required, they are configured using an anonymous function.

> [!NOTE]
>```Pascal
>uses Anthropic;
>
>var Anthropic := TAnthropicFactory.CreateInstance(API_KEY);
>```

<br/>

### Claude Models Overview

`Claude` is a family of advanced language models developed by Anthropic, designed for a range of tasks from fast, cost-efficient responses to highly complex operations. 

>[!WARNING]
>Note: There is no API available for retrieving a list of models automatically; model names and versions must be referenced directly from the documentation.
>

Key Models: <br/>
- `Claude 3.5 Sonnet`: The most intelligent model, offering top-tier performance for high-complexity tasks.<br/>
- `Claude 3 Opus`: Excellent for complex tasks requiring deep intelligence and fluency.<br/>
- `Claude 3 Sonnet`: A balanced model that offers a good mix of intelligence and speed.<br/>
- `Claude 3 Haiku`: The fastest and most responsive model, ideal for quick, targeted performance.<br/><br/>
All models support text and image input, have a **200k** context window, and multilingual capabilities. Output token limits vary, with `Claude 3.5 Sonnet` allowing up to **8192 tokens**, while other models can handle up to **4096 tokens**. Pricing and latency also vary, with `Claude 3 Haiku` being the most cost-effective.

For more details on getting started with Claude, [visit Anthropic’s documentation](https://docs.anthropic.com/en/docs/about-claude/models) to manually select models and obtain API keys.

<br/>

To simplify working with the models in the upcoming examples, we’ll first declare the following constants. This will make it easier to reference and manipulate the models throughout the code :

```Pascal
const
  ClaudeSonnet3_5 = 0;
  ClaudeOpus3 = 1;
  ClaudeSonnet3 = 2;
  ClaudeHaiku3 = 3;
  /// <summary>
  /// Array of Available Models for v3 and Beyond
  /// </summary>
  Models : TArray<string> = [
             'claude-3-5-sonnet-20240620', 'claude-3-opus-20240229', 
             'claude-3-sonnet-20240229', 'claude-3-haiku-20240307'
  ];
```

<br/>

### Embeddings

`Anthropic` does not offer its own models for **text embeddings**. While the documentation mentions `Voyage AI` as an embeddings provider, we do not include access to their APIs in our **GitHub repository**. This is because providing tools for Voyage models falls outside the scope of our focus on `Anthropic` APIs exclusively. Users seeking embedding solutions are encouraged to explore various vendors to find the best fit for their needs, but our resources concentrate solely on supporting `Anthropic's` offerings.

<br/>

### Chats

`Claude` is capable of performing a wide range of text-based tasks. Trained on code, prose, and various natural language inputs, `Claude` excels in generating text outputs in response to detailed prompts. For optimal results, prompts should be written as detailed natural language instructions, and further improvements can be achieved through prompt engineering.

- **Text Summarization**: Condense lengthy content into key insights.
- **Content Generation:** Create engaging content like blog posts, emails, and product descriptions.
- **Data and Entity Extraction**: Extract structured information from unstructured text.
- **Question Answering**: Develop intelligent systems such as chatbots and educational tutors.
- **Text Translation**: Facilitate communication across different languages.
- **Text Analysis and Recommendations**: Analyze sentiment and patterns to personalize experiences.
- **Dialogue and Conversation**: Generate context-aware interactions for games and storytelling.
- **Code Explanation and Generation**: Assist in code reviews and generate boilerplate code.

<br/>

#### Create a message

You can send a structured list of input messages containing text and/or image content, and the model will generate the next message in the conversation.

The Messages API can be used for both single-turn requests and multi-turn, stateless conversations.

Example :
```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat;

  var Chat := Anthropic.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeSonnet3_5]);
       Params.MaxTokens(1024);
       Params.System('You are an expert in art history');
       Params.Messages([TChatMessagePayload.User('Can you enlighten me on the technique of chiaroscuro and also on the Flemish school of painting in the 18th century ?')]);
     end);

  //Set a TMemo on a form to display the response
  Memo1.Lines.BeginUpdate;  
  try
    for var Item in Chat.Content do
      begin
        Memo1.Text := Memo1.Text + sLineBreak + Item.Text + sLineBreak;
        Memo1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
      end;
  finally
    Memo1.Lines.EndUpdate;
    Chat.Free;
  end;
```

<br/>

#### Streaming messages

When generating a Message, you can enable "stream": true to progressively receive the response through server-sent events (SSE).

Example :
```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat;
  Anthropic.Chat.CreateStream(
    procedure (Params: TChatParams)
    begin
      Params.Model(Models[ClaudeHaiku3]);
      Params.MaxTokens(1024);
      Params.System([ TSystemPayload.Create('You are an expert in art history') ]);
      Params.Messages([TChatMessagePayload.User('Can you enlighten me on the technique of chiaroscuro and also on the Flemish school of painting in the 18th century ?')]);
      Params.Stream(True);
    end,
    procedure(var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
    begin
      if not IsDone then
        begin
          Memo1.Lines.BeginUpdate;
          try
            var S := Chat.Delta.Text;
            for var i := 1 to S.Length do
              if (S[i] <> #10) and (S[i] <> #13) then
                Memo1.Text := Memo1.Text + S[i] else
                Memo1.Text := Memo1.Text + sLineBreak;
            Memo1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
          finally
            Memo1.Lines.EndUpdate;
          end;
        end
      else
        //is done
        begin
          Memo1.Text := Memo1.Text + sLineBreak + '--- Done';
          Memo1.Text := Memo1.Text + sLineBreak + Chat.Usage.InputTokens.ToString;
          Memo1.Text := Memo1.Text + sLineBreak + Chat.Usage.OutputTokens.ToString;
        end;
    end);
  end;
```

<br/>

### Callbacks (asynchronous mode)

In the context of asynchronous methods, for a method that does not involve streaming, callbacks use the following generic record: `TAsynCallBack<T> = record` defined in the `Anthropic.Async.Support.pas` unit. This record exposes the following properties:

```Pascal
   TAsynCallBack<T> = record
   ... 
       Sender: TObject;
       OnStart: TProc<TObject>;
       OnSuccess: TProc<TObject, T>;
       OnError: TProc<TObject, string>; 
```
<br/>

For methods requiring streaming, callbacks use the generic record `TAsynStreamCallBack<T> = record`, also defined in the `Anthropic.Async.Support.pas` unit. This record exposes the following properties:

```Pascal
   TAsynCallBack<T> = record
   ... 
       Sender: TObject;
       OnStart: TProc<TObject>;
       OnSuccess: TProc<TObject, T>;
       OnProgress: TProc<TObject, T>;
       OnError: TProc<TObject, string>;
       OnCancellation: TProc<TObject>;
       OnDoCancel: TFunc<Boolean>;
```

The name of each property is self-explanatory; if needed, refer to the internal documentation for more details.

<br>

### Asynchronous chat

A mechanism for handling both streamed and non-streamed messages has been implemented to enable asynchronous use of the API.

This approach allows for flexibility in how responses are received:

 - Create a Message ASYNC:

- Streaming Messages ASYNC

This setup ensures efficient asynchronous workflows, offering seamless handling of responses for various messaging scenarios.

<br/>

#### Create a message ASYNC

You can send a structured list of input messages containing text and/or image content, and the model will asynchronously generate the next message in the conversation.

The Messages API can be used for both single-turn requests and multi-turn, stateless conversations, with responses being processed asynchronously.

>[!WARNING]
>It is necessary to extend the scope of the interface :
>
>    - Anthropic := TAnthropicFactory.CreateInstance(BaererKey)
>
>by declaring it in the application's onCreate method.

Example :
```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat;
  
  // WARNING - Move the following line into the main OnCreate
  var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);

  Anthropic.Chat.AsynCreate(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeOpus3]);
       Params.MaxTokens(1024);
       Params.System([ TSystemPayload.Create('You are an expert in art history') ]);
       Params.Messages([TChatMessagePayload.User('Can you enlighten me on the technique of chiaroscuro and also on the Flemish school of painting in the 18th century ?')]);
     end,

     function : TAsynChat
     begin
       Result.Sender := Memo1; //Using TMemo for displaying

       Result.OnStart := nil;

       Result.OnSuccess :=
         procedure (Sender: TObject; Chat: TChat)
         begin
           var M := Sender as TMemo;
           M.Lines.BeginUpdate;
           try
             for var Item in Chat.Content do
               begin
                 M.Text := M.Text + sLineBreak + Item.Text + sLineBreak;
                 M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
               end;
           finally
             M.Lines.EndUpdate;
           end;
         end;

       Result.OnError :=
         procedure (Sender: TObject; value: string)
         begin
           (Sender as TMemo).Text := Value;
         end;
     end);
```
<br/>

#### Streaming messages ASYNC

When generating a Message, you can set 'async': true to asynchronously receive the response once it is fully processed, without the need for server-sent events (SSE).

>[!WARNING]
>It is necessary to extend the scope of the interface :
>
>    - Anthropic := TAnthropicFactory.CreateInstance(BaererKey)
>
>by declaring it in the application's onCreate method.

Example :
```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat;

  // WARNING - Move the following line into the main OnCreate
  var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);  

  //Set a TCheckBox on the form  
  CheckBox1.Checked := False;
  
  Anthropic.Chat.AsynCreateStream(
      procedure(Params: TChatParams)
      begin
        Params.Model(Models[ClaudeSonnet3_5]);
        Params.MaxTokens(1024);
        Params.System([ TSystemPayload.Create('You are an expert in art history') ]);
        Params.Messages([TChatMessagePayload.User('Can you enlighten me on the technique of chiaroscuro and also on the Flemish school of painting in the 18th century ?')]);
        Params.Stream(True);
      end,

      function: TAsynChatStream
      begin
        Result.Sender := Memo1;  //Using TMemo for displaying

        Result.OnProgress :=
          procedure (Sender: TObject; Chat: TChat)
          begin
            var M := Sender as TMemo;
            M.Lines.BeginUpdate;
            try
              var S := Chat.Delta.Text;
              for var i := 1 to S.Length do
                if (S[i] <> #10) and (S[i] <> #13) then
                  M.Text := M.Text + S[i] else
                  M.Text := M.Text + sLineBreak;
              M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
            finally
              M.Lines.EndUpdate;
            end;
          end;

        Result.OnSuccess :=
          procedure (Sender: TObject; Chat: TChat)
          begin
            var M := Sender as TMemo;
            M.Lines.BeginUpdate;
            try
              M.Text := M.Text + sLineBreak + '--- Done';
              M.Text := M.Text + sLineBreak + Chat.Usage.InputTokens.ToString;
              M.Text := M.Text + sLineBreak + Chat.Usage.OutputTokens.ToString;
              M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
            finally
              M.Lines.EndUpdate;
            end;
          end;

        Result.OnError :=
          procedure (Sender: TObject; Value: string)
          begin
            (Sender as TMemo).Text := Value;
          end;

        Result.OnDoCancel :=
          function: Boolean
          begin
            Result := CheckBox1.Checked; // Click on checkbox to cancel
          end;

        Result.OnCancellation :=
          procedure (Sender: TObject)
          begin
            // Processing when process has been canceled
          end;
      end);
```

<br/>

### Vision

All `Claude version 3` models add vision capabilities, allowing them to analyze both images and text, expanding their potential for applications requiring multimodal understanding. See also the [official documentation](https://docs.anthropic.com/en/docs/build-with-claude/vision/).

To support both synchronous and asynchronous completion methods, we focused on generating the appropriate payload for message parameters. An overloaded version of the `TChatMessagePayload.User` class function was added, allowing users to include a dynamic array of text elements—file paths—alongside the user's text content. 
Internally, this data is processed to ensure the correct operation of the vision system in both synchronous and asynchronous contexts.

<br/>

#### Passing a Base64 Encoded Image

Example :
```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat;
  
  var Ref := 'T:\My_Folder\Images\Picture.png';
  var Chat := Anthropic.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeHaiku3]);
       Params.MaxTokens(1024);
       Params.Messages([TChatMessagePayload.User(Memo2.Text, [Ref])]);
     end);

  Memo1.Lines.BeginUpdate;
  try
    for var Item in Chat.Content do
      begin
        if Item.&Type = 'text' then
          begin
            Memo1.Text := Memo1.Text + Item.Text + sLineBreak;
            Memo1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
          end
      end;
  finally
    Memo1.Lines.EndUpdate;
    Chat.Free;
  end;
```

<br/>

Example - Asynchronous vision :

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat;

  // WARNING - Move the following line into the main OnCreate
  var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);

  var Ref := 'T:\My_Folder\Images\Picture.png';
  Anthropic.Chat.AsynCreate(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeOpus3]);
       Params.MaxTokens(1024);
       Params.Messages([TChatMessagePayload.User(Memo2.Text, [Ref])]);
     end,

     function : TAsynChat
     begin
       Result.Sender := Memo1; //Using TMemo for displaying

       Result.OnSuccess :=
         procedure (Sender: TObject; Chat: TChat)
         begin
           var M := Sender as TMemo;
           M.Lines.BeginUpdate;
           try
             for var Item in Chat.Content do
               begin
                 M.Text := M.Text + sLineBreak + Item.Text + sLineBreak;
                 M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
               end;
           finally
             M.Lines.EndUpdate;
           end;
         end;

       Result.OnError :=
         procedure (Sender: TObject; value: string)
         begin
           (Sender as TMemo).Text := Value;
         end;
     end);
```

<br/>

### Function calling

Claude can connect with external client-side tools provided by users to perform various tasks more efficiently. 

>[!WARNING]
>Warning: Ensure user confirmation for actions like sending emails or making purchases to avoid unintended consequences.
>

For more details, refer to the Anthropic [website documentation.](https://docs.anthropic.com/en/docs/build-with-claude/tool-use) 

<br/>

#### Overview of Tool Use in Claude

`Here's a quick guide on how to implement tool use:` <br/>
- **Provide Tools & User Prompt**: Define tools in your API request with names, descriptions, and input schemas. Add a user prompt, e.g., “What’s the weather in San Francisco?” <br/>
- **Claude Decides to Use a Tool**: If a tool is helpful, Claude sends a tool use request with a tool_use stop_reason. <br/>
- **Run Tool and Return Results**: On your side, extract the tool input, run it, and return the results to Claude via a tool_result content block. <br/>
- **Claude’s Final Response**: Claude analyzes the tool results and crafts its final answer. <br/>

`Forcing Tool Use` : <br/>
- **auto (default)**: Claude decides whether to use a tool. <br/>
- **any**: Claude must use one of the provided tools. <br/>
- **tool**: Forces Claude to use a specific tool. <br/>

`Flexibility and Control`: <br/>
- All tools are user-provided, giving you complete control. You can guide or force tool use for specific tasks or let Claude decide when tools are necessary.

<br/>

#### Examples

1 : What’s the weather in Paris?

In the `Anthropic.Functions.Example` unit, there is a class that defines a function which `Claude` can choose to use or not, depending on the options provided. This class inherits from a parent class defined in the `Anthropic.Functions.Core` unit. To create new functions, you can derive from the `TFunctionCore class` and define a new plugin.

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Functions.Core, Anthropic.Functions.Example;
  
  var WeatherFunc := TWeatherReportFunction.CreateInstance;
  
  var Chat := Anthropic.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeHaiku3]);
       Params.MaxTokens(1024);
       Params.Messages([TChatMessagePayload.User(Memo2.Text)]);
       Params.ToolChoice(auto);
       Params.Tools([WeatherFunc]);
     end);

  Memo1.Lines.BeginUpdate;
  try
    for var Item in Chat.Content do
      begin
        if Item.&Type = 'text' then
          begin
            Memo1.Text := Memo1.Text + Item.Text + sLineBreak;
            Memo1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
          end
        else
        if Item.&Type = 'tool_use' then
          begin
            var Arguments := Format('%s' + sLineBreak + '%s', [Memo2.Text, WeatherFunc.Execute(Item.Input)]);
            WeatherExecute(Arguments);
          end;
      end;
  finally
    Memo1.Lines.EndUpdate;
    Chat.Free;
  end;

...

procedure WeatherExecute(const Value: string);
begin
  var Chat := Anthropic.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeHaiku3]);
       Params.MaxTokens(1024);
       Params.Messages([TChatMessagePayload.User(Value)]);
     end);
  try
    for var Item in Chat.Content do
      begin
        if Item.&Type = 'text' then
          begin
            Memo1.Text := Memo1.Text + Item.Text + sLineBreak;
            Memo1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
          end
      end;
  finally
    Chat.Free;
  end;
end;
```

<br/>

2 : What’s the weather in Paris? (asynchronous streaming mode)

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Functions.Core, Anthropic.Functions.Example;

  // WARNING - Move the following line into the main OnCreate
  var Anthropic := TAnthropicFactory.CreateInstance(BaererKey);

  CheckBox1.Checked := False;

  var WeatherFunc := TWeatherReportFunction.CreateInstance;

  Anthropic.Chat.AsynCreateStream(
      procedure(Params: TChatParams)
      begin
        Params.Model(Models[ClaudeSonnet3_5]);
        Params.MaxTokens(1024);
        Params.Messages([TChatMessagePayload.User(Memo2.Text)]);
        Params.ToolChoice(auto);
        Params.Tools([WeatherFunc]);
        Params.Stream(True);
      end,

      function: TAsynChatStream
      begin
        Result.Sender := Memo1; 

        Result.OnProgress :=
          procedure (Sender: TObject; Chat: TChat)
          begin
            var M := Sender as TMemo;
            M.Lines.BeginUpdate;
            try
              var S := Chat.Delta.Text;
              for var i := 1 to S.Length do
                if (S[i] <> #10) and (S[i] <> #13) then
                  M.Text := M.Text + S[i] else
                  M.Text := M.Text + sLineBreak;
              M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
            finally
              M.Lines.EndUpdate;
            end;
          end;

        Result.OnSuccess :=
          procedure (Sender: TObject; Chat: TChat)
          begin
            var M := Sender as TMemo;
            if Chat.StopReason = tool_use then
              begin
                var Arguments := Format('%s' + sLineBreak + '%s', [Memo1.Text, WeatherFunc.Execute(Chat.Delta.Input)]);
                WeatherExecuteStream(Arguments);
                Exit;
              end;

            M.Lines.BeginUpdate;
            try
              M.Text := M.Text + sLineBreak + '--- Done';
              M.Text := M.Text + sLineBreak + Chat.Usage.InputTokens.ToString;
              M.Text := M.Text + sLineBreak + Chat.Usage.OutputTokens.ToString;
              M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
            finally
              M.Lines.EndUpdate;
            end;
          end;

        Result.OnError :=
          procedure (Sender: TObject; Value: string)
          begin
            ShowMessage(Value); // Display error message
          end;

        Result.OnDoCancel :=
          function: Boolean
          begin
            Result := CheckBox1.Checked; // Click on checkbox to cancel
          end;

        Result.OnCancellation :=
          procedure (Sender: TObject)
          begin
            // Processing when process has been canceled
          end;
      end);

...

procedure WeatherExecuteStream(const Value: string);
begin
  Anthropic.Chat.AsynCreateStream(
      procedure(Params: TChatParams)
      begin
        Params.Model(Models[ClaudeHaiku3]);
        Params.MaxTokens(1024);
        Params.Messages([TChatMessagePayload.User(Value)]);
        Params.Stream(True);
      end,

      function: TAsynChatStream
      begin
        Result.Sender := Memo1;  

        Result.OnStart :=
          procedure (Sender: TObject)
          begin
            var M := Sender as TMemo;
            if not (M.Text = EmptyStr) then
              begin
                M.Text := M.Text + sLineBreak + sLineBreak;
              end;
          end;

        Result.OnProgress :=
          procedure (Sender: TObject; Chat: TChat)
          begin
            var M := Sender as TMemo;
            M.Lines.BeginUpdate;
            try
              var S := Chat.Delta.Text;
              for var i := 1 to S.Length do
                if (S[i] <> #10) and (S[i] <> #13) then
                  M.Text := M.Text + S[i] else
                  M.Text := M.Text + sLineBreak;
              M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
            finally
              M.Lines.EndUpdate;
            end;
          end;

        Result.OnSuccess :=
          procedure (Sender: TObject; Chat: TChat)
          begin
            var M := Sender as TMemo;
            M.Lines.BeginUpdate;
            try
              M.Text := M.Text + sLineBreak + '--- Done';
              M.Text := M.Text + sLineBreak + Chat.Usage.InputTokens.ToString;
              M.Text := M.Text + sLineBreak + Chat.Usage.OutputTokens.ToString;
              M.Perform(WM_VSCROLL, SB_BOTTOM, 0);
            finally
              M.Lines.EndUpdate;
            end;
          end;

        Result.OnError :=
          procedure (Sender: TObject; Value: string)
          begin
            ShowMessage(Value); // Display error message
          end;

        Result.OnDoCancel :=
          function: Boolean
          begin
            Result := CheckBox1.Checked; // Click on checkbox to cancel
          end;

        Result.OnCancellation :=
          procedure (Sender: TObject)
          begin
            // Processing when process has been canceled
          end;
      end);
end;
```

<br/>

## Beta versions

Two new features now available in public beta:

`Prompt Caching (Beta)`: To access this feature, include the `anthropic-beta: prompt-caching-2024-07-31` header in your API requests. 

`Message Batches API (Beta)`: To use this feature, include the `anthropic-beta: message-batches-2024-09-24` header in your API requests, or call client.beta.messages.batches in your SDK. 

<br/>

### Prompt Caching

`Prompt Caching` optimizes API usage by caching prompt prefixes, reducing processing time and costs for repetitive tasks. If a prompt prefix is cached from a recent query, it's reused; otherwise, the full prompt is processed and cached for future use. The cache lasts **5 minutes** and **is refreshed with each use**, making it ideal for prompts with many examples, background information, or consistent instructions.

For more details, refer to the Anthropic [website documentation.](https://docs.anthropic.com/en/docs/build-with-claude/prompt-caching/) 

<br/>

#### Caching initialization

To include the `anthropic-beta: prompt-caching-2024-07-31` header, you must declare :

> [!NOTE]
>```Pascal
>uses Anthropic;
>
>var Anthropic := TAnthropicFactory.CreateInstance(API_KEY, caching);
>```

Prompt Caching is supported on models like `Claude 3.5 Sonnet`, `Claude 3 Haiku`, and `Claude 3 Opus`. Any part of the request can be flagged for caching using cache_control. 

This includes:

- `Tools`: Definitions in the tools array.
- `System Messages`: Content blocks within the system array.
- `Messages`: Content blocks in the messages.content array, for both user and assistant turns.
- `Images`: Content blocks in the messages.content array during user turns.
- `Tool Usage and Results`: Content blocks in the messages.content array, for both user and assistant turns.

Each of these components can be designated for caching by applying cache_control to that specific portion of the request.

>[!WARNING]
>Minimum Cacheable Prompt Length:
>
>- **1024 tokens** for `Claude 3.5 Sonnet` and `Claude 3 Opus`
>- **2048 tokens** for `Claude 3 Haiku`
>Prompts shorter than these lengths cannot be cached, even if they include cache_control. Any request to cache a prompt with fewer tokens than the minimum required will be processed without caching. To check if a prompt was cached, refer to the response usage fields.
>
>The cache has a 5-minute time-to-live (TTL). Currently, the only supported cache type is "ephemeral," which corresponds to this 5-minute lifespan.
>

<br/>

#### System Caching

In the following example, we have a plain text file `text/plain` whose size exceeds the minimum threshold for caching. We will include this file in the ***system section*** of the prompt. This can be beneficial in a ***multi-turn conversation***.

```Pascal
// uses Anthropic.API, Anthropic, Anthropic.Chat;

  //Create an interface with caching
  var AnthropicCaching := TAnthropicFactory.CreateInstance(BaererKey, caching);

  var LongText1 := 'T:\my_folder\documents\legal.txt';
  var LongText2 := 'T:\my_folder\documents\legal-add-on.txt';

  var Chat := AnthropicCaching.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeSonnet3_5]);
       Params.MaxTokens(1024);
       Params.System([
         TSystemPayload.Create('You are an AI assistant tasked with analyzing legal documents.'),
         TSystemPayload.Create('Here is the full text of a complex legal agreement:', 
            [LongText1, LongText2], True)
       ]);
       Params.Messages([TChatMessagePayload.User(Memo2.Text)]);
      // You can also add question and answer caching
      // Params.Messages([TChatMessagePayload.User(Memo2.Text), True]); 
     end);
  try
    for var Item in Chat.Content do
      begin
        Memo1.Text := Memo1.Text + Item.Text + sLineBreak;
        Memo1.Text := Memo1.Text + '  Cache Creation Input Tokens : ' + Chat.Usage.CacheCreationInputTokens.ToString + sLineBreak;
        Memo1.Text := Memo1.Text + '  Cache Read Input Tokens : ' + Chat.Usage.CacheReadInputTokens.ToString + sLineBreak;
        Memo1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
      end;
  finally
    Chat.Free;
  end;
```

<br/>

#### Tools Caching

The `cache_control` parameter is applied to the final tool (get_time), allowing all previous tool definitions, like get_weather, to be cached as a single block. This is useful for reusing a consistent set of tools across multiple requests without reprocessing them each time.

Let's assume we have several tools, each defined in a plugin, as we did with the get_time tool, which we decided to call last. When instantiating each plugin, we'll call the associated factory method to create an instance, for example:

```Pascal
var tool_n := TMy_tool_nFunction.CreateInstance;
```

For the get_time tool, the instantiation will be done like this:

```Pascal
var WeatherFunc := TWeatherReportFunction.CreateInstance(True);

  // True indicates that WeatherFunc is marked for cache control, 
  // along with all the tools preceding it in the list provided to Claude.
```

When making all these tools available, we will simply write:

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Functions.Core, Anthropic.Functions.Example;

  var AnthropicCaching := TAnthropicFactory.CreateInstance(BaererKey, caching);

  var Chat := AnthropicCaching.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeHaiku3]);
       Params.MaxTokens(1024);
       Params.Messages([TChatMessagePayload.User('my request')]);
       Params.ToolChoice(auto);
       Params.Tools([tool_1, ... , tool_n, WeatherFunc]);
         // List of tools provided to Claude
     end);
   ...
```

And so the whole list of tools will be cached.

<br/>

#### Images Caching

Image caching for the vision part has been implemented by overloading the `TChatMessagePayload.User` method, which now includes a new parameter `Caching: TSysCachingType`.

```pascal
TSysCachingType = (none, contentCached, imagesCached, both);
```

- **none**: Caching is disabled.
- **contentCached**: Only the question and answer part is cached.
- **imagesCached**: Only images are cached.
- **both**: Both images and the question and answer part are cached.

As with tools, if we provide a collection of images to Vision, only the last image in the list will be marked with `cache_control`. This process is handled internally when the JSON request is constructed before being submitted to Claude.

```pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat;  

  //Create an interface with caching
  var AnthropicCaching := TAnthropicFactory.CreateInstance(BaererKey, caching);

  var Ref1 := 'T:\my_folder\Images\My_Image1.png';
  var Ref2 := 'T:\my_folder\Images\My_Image2.png';

  var Chat := AnthropicCaching.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeHaiku3]);
       Params.MaxTokens(1024);
       Params.Messages([TChatMessagePayload.User('My_request', [Ref1, Ref2], both)]);
     end);
  ...
```

<br/>

### Message Batches

The `Message Batches` API enables efficient, asynchronous processing of large volumes of message requests. This method is ideal for tasks that don’t require immediate responses, cutting costs by 50% and boosting throughput.

For more details, refer to the [Anthropic website documentation](https://docs.anthropic.com/en/docs/build-with-claude/message-batches/).

<br/>

#### Message Batches initialization

To include the `anthropic-beta: message-batches-2024-09-24` header, you must declare :

> [!NOTE]
>```Pascal
>uses Anthropic;
>
>var AnthropicBatches := TAnthropicFactory.CreateInstance(API_KEY, batches);
>```

The `Message Batches` API supports `Claude 3.5 Sonnet`, `Claude 3 Haiku`, and `Claude 3 Opus`. Any request that can be made through the Messages API can be batched, including : 
 - **Vision** 
 - **Tool use**
 - **System messages** 
 - **Multi-turn conversations**
 - **Beta features** 

Different types of requests can be mixed within a single batch, as each request is processed independently.

>[!WARNING]
>**Batch limitations**
> - A `Message Batch` is limited to **10,000 requests** or **32 MB**, with up to **24 hours** for processing. 
> - Results are available only after the entire batch is processed and can be accessed for **29 days**. 
> - `Batches` are scoped to a Workspace, and rate limits apply to **HTTP requests**, not batch size. 
> - Processing may slow down based on demand, and the Workspace's spend limit may be slightly exceeded.
>

<br/>

#### How it works

The `Message Batches` API creates a batch of requests, processed asynchronously with each request handled independently. You can track the batch status and retrieve results once processing is complete. This is ideal for large-scale tasks like evaluations, content moderation, data analysis, or bulk content generation.

<br/>

#### Batch create

A Message Batch consists of a collection of requests to generate individual Messages. Each request is structured as follows:

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches;

  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);

  var Batche := AnthropicBatche.Batche.Create(
        procedure (Params: TRequestParams)
        begin
          Params.Requests([
            TBatcheParams.Add('my-first-request',
            procedure (Params: TChatParams)
            begin
              Params.Model('claude-3-5-sonnet-20240620');
              Params.MaxTokens(1024);
              Params.Messages([
                TChatMessagePayload.User('Hello, world') ]);
            end),

            TBatcheParams.Add('my-second-request',
            procedure (Params: TChatParams)
            begin
              Params.Model('claude-3-5-sonnet-20240620');
              Params.MaxTokens(1024);
              Params.Messages([
              TChatMessagePayload.User('Hi again, friend') ]);
            end)
          ])
        end);

  // Display batche informations
  try
    Memo1.Text := Memo1.Text + sLineBreak + Batche.Id;
    Memo1.Text := Memo1.Text + sLineBreak + Batche.ProcessingStatus.ToString;
  finally
    Batche.Free;
  end;
```

**alternative approach**
 - Pre-creating the requests, followed by sending the generated batches in a separate phase. This approach allows for a focused effort on batch preparation first, optimizing the processing workflow and minimizing the risk of errors.

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches;

  // Create the content of the batche
  var Request := TRequestParams.Create;
  Request.Requests([
    TBatcheParams.Add('my-first-request',
        procedure (Params: TChatParams)
        begin
          Params.Model('claude-3-5-sonnet-20240620');
          Params.MaxTokens(1024);
          Params.Messages([
            TChatMessagePayload.User('Hello, world') ]);
        end),
      TBatcheParams.Add('my-second-request',
        procedure (Params: TChatParams)
        begin
          Params.Model('claude-3-5-sonnet-20240620');
          Params.MaxTokens(1024);
          Params.Messages([
            TChatMessagePayload.User('Hi again, friend') ]);
        end)
    ]);
  Memo1.Text := Request.ToFormat();

  // Send the batche to Claude
  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
  var Batche := AnthropicBatche.Batche.Create(Request.JSON);
  try
    Memo1.Text := Memo1.Text + sLineBreak + Batche.Id;
    Memo1.Text := Memo1.Text + sLineBreak + Batche.ProcessingStatus.ToString;
  finally
    Batche.Free;
    Request.Free;
  end;
```

<br/>

#### Batch list

Retrieve all message batches within a workspace, with the most recently created batches appearing first.

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches;

  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
  var Batche := AnthropicBatche.Batche.List;
  try
    for var Item in Batche.Data do
      Memo1.Text := Memo1.Text + sLineBreak + Item.Id + '  ' + Item.ProcessingStatus.ToString;
  finally
    Batche.Free;
  end;
```

You can use the "list" API with the following query parameters:

- **before_id (string)**: Use this parameter as a cursor for pagination. When specified, it returns the page of results immediately preceding the object identified by this ID.

- **after_id (string)**: Similar to the above, but this cursor returns the page of results immediately following the specified object ID.

- **limit (integer)**: Specifies how many items to return per page. The default is set to 20, with valid values ranging from 1 to 100.

Make sure to use the `TListParams` record as demonstrated in the example that follow :

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches;

  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
  var Batche := AnthropicBatche.Batche.List(TListParams.Create.Limite(5).AfterId('msgbatch_XXXX'));
  try
    for var Item in Batche.Data do
      Memo1.Text := Memo1.Text + sLineBreak + Item.Id + '  ' + Item.ProcessingStatus.ToString;
  finally
    Batche.Free;
  end;
```

<br/>

#### Batch cancel

Batches can be canceled at any point before the processing is fully completed. Once a cancellation is triggered, the batch moves into a canceling state, during which the system may still finish any ongoing, non-interruptible requests before the cancellation is finalized.

The count of canceled requests is listed in the `request_counts`. To identify which specific requests were canceled, review the individual results within the batch. Keep in mind that no requests may actually be canceled if all active requests were non-interruptible.

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches;

  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
  var Batche := AnthropicBatche.Batche.Cancel('msgbatch_XXXX');
  try
    Memo1.Text := Memo1.Text + sLineBreak + Batche.ProcessingStatus.ToString;
  finally
    Batche.Free;
  end;
```

<br/>

#### Batch retrieve message

This endpoint is repeatable and can be used to check the status of a Message Batch completion. To retrieve the results of a Message Batch, make a request to the `results_url` field provided in the response.

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches;

  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
  var Batche := AnthropicBatche.Batche.Retrieve('msgbatch_XXXX');
  try
    Memo1.Text := Memo1.Text + sLineBreak + Batche.ResultsUrl;
  finally
    Batche.Free;
  end;
```

<br/>

#### Batch retrieve results

Streams the results of a Message Batch in a **JSONL** file format.

Each line in the file represents a **JSON** object containing the outcome of an individual request from the Message Batch. The order of results may not correspond to the original request order, so use the `custom_id` field to align results with their respective requests.

>[!WARNING]
>The path to retrieve Message Batch results should be obtained from the `results_url` of the batch. This path should not be assumed, as it may vary.
>

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches;

  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
  var JSONL := AnthropicBatche.Batche.Retrieve('msgbatch_XXXX', 'Result.jsonl');

  with JSONL.GetEnumerator do
    try
      while MoveNext do
        Memo1.Text := Memo1.Text + sLineBreak + Current + sLineBreak;
    finally
      Free;
      JSONL.Free;
    end;
```

In the `Anthropic.Batches.Support.pas` unit, the object interface `IBatcheResults` allows access to the data returned by Claude by providing the name of the **JSONL** file containing the batch data. All the information can be accessed through the Batches array, as demonstrated in the example below.

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches, Anthropic.Batches.Support;
  
  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);
  var S := AnthropicBatche.Batche.Retrieve('msgbatch_XXXX', 'Result.jsonl');

  var BatchResults := TBatcheResultsFactory.CreateInstance('Result.jsonl');

  for var Item in BatchResults.Batches do
    begin
      Memo1.Text := Memo1.Text + sLineBreak + Item.CustomId + sLineBreak;
      Memo1.Text := Memo1.Text + sLineBreak + Item.Result.Message.Content[0].Text + sLineBreak;
    end;
```

<br/>

#### Batch asynchronous

There are also asynchronous methods available for managing batches, with each method having its asynchronous counterpart. However, to utilize them, it is necessary to extend the scope of the interface : <br/><br/>
          - `IAnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches)`  <br/><br/>
by declaring it in the application's `onCreate` method. Otherwise, a limited scope will lead to failure when using asynchronous methods. 

In the following example, we will use the AsynRetrieve method to fetch a remote batch result.

```Pascal
//uses Anthropic.API, Anthropic, Anthropic.Chat, Anthropic.Batches, Anthropic.Batches.Support;
  
  // WARNING - Move the following line into the main OnCreate
  var AnthropicBatche := TAnthropicFactory.CreateInstance(BaererKey, batches);

  var FileName := 'Result.jsonl';
  AnthropicBatche.Batche.ASynRetrieve('msgbatch_XXXX', FileName,
        function : TAsynStringList
        begin
          Result.Sender := Memo1;

          Result.OnSuccess :=
            procedure (Sender: TObject; Params: TStringList)
            begin
              var M := Sender as TMemo;
              var BatchResults := TBatcheResultsFactory.CreateInstance(FileName);
              for var Item in BatchResults.Batches do
                begin
                  M.Text := M.Text + sLineBreak + Item.CustomId + sLineBreak;
                  M.Text := M.Text + sLineBreak + Item.Result.Message.Content[0].Text + sLineBreak;
                end;
            end;
        end);
```

<br/>

#### Console

> [!NOTE]
>You can access all batches through the [Anthropic console](https://console.anthropic.com/settings/workspaces/default/batches). A complete history is maintained, allowing you to view and download the computed results.
>

<br/>

## Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

## License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.

