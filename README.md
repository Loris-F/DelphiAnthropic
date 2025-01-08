# Delphi Anthropics API

___
![GitHub](https://img.shields.io/badge/IDE%20Version-Delphi%2010.3/11/12-yellow)
![GitHub](https://img.shields.io/badge/platform-all%20platforms-green)
![GitHub](https://img.shields.io/badge/Updated%20on%20january%207,%202025-blue)

<br/>
<br/>

- [Introduction](#Introduction)
- [Changelog](#Changelog)
- [Remarks](#remarks)
- [Wrapper Tools Info](#Wrapper-Tools-Info)
    - [Tools for simplifying this tutorial](#Tools-for-simplifying-this-tutorial)
    - [Asynchronous callback mode management](#Asynchronous-callback-mode-management)
    - [Simplified Unit Declaration](#Simplified-Unit-Declaration) 
- [Usage](#usage)
    - [Initialization](#initialization)
    - [Claude Models Overview](#Claude-Models-Overview)
        - [List of models](#List-of-models)
        - [Retrieve a model](#Retrieve-a-model)
    - [Embeddings](#embeddings)
    - [Chats](#chats)
        - [Create a message](#Create-a-message)
        - [Streaming messages](#Streaming-messages)
        - [Multi-turn conversation](#Multi-turn-conversation)
        - [Token counting](#Token-counting)
    - [Document processing](#Document-processing)
    - [Vision](#vision)
        - [Passing a Base64 Encoded Image](#Passing-a-base64-encoded-image)
        - [Passing an Image URL](#Passing-an-Image-URL)
    - [Function calling](#function-calling)
        - [Overview of Tool Use in Claude](#Overview-of-Tool-Use-in-Claude)
        - [Examples](#Examples)
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
        - [Batch delete](#Batch-delete)
        - [Console](#Console) 
- [Contributing](#contributing)
- [License](#license)

<br/>
<br/>


# Introduction

Welcome to the unofficial Delphi **Anthropic** API library. This project aims to provide a `Delphi` interface for interacting with the **Anthropic** public API, making it easier to integrate advanced natural language processing features into your `Delphi` applications. Whether you want to generate text, create embeddings, use chat models, or generate code, this library offers a simple and effective solution.

**Anthropic** is a powerful natural language processing API that enables developers to incorporate advanced AI functionalities into their applications. For more details, visit the [official Anthropic documentation](https://docs.anthropic.com/en/docs/welcome/).

<br/>

# Changelog

[See the changes](https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/Changelog.md) made in this version.

<br/>

# Remarks 

> [!IMPORTANT]
>
> This is an unofficial library. **Anthropic** does not provide any official library for `Delphi`.
> This repository contains `Delphi` implementation over [Anthropic](https://docs.anthropic.com/en/api/getting-started/) public API.

<br/>

# Wrapper Tools Info

This section provides brief notifications and explanations about the tools available to simplify the presentation and understanding of the wrapper's functions in the tutorial.

<br>

## Tools for simplifying this tutorial

To streamline the code examples provided in this tutorial and facilitate quick implementation, two units have been included in the source code: `Anthropic.Tutorial.VCL` and `Anthropic.Tutorial.FMX`. Depending on the platform you choose to test the provided source code, you will need to instantiate either the `TVCLTutorialHub` or `TFMXTutorialHub` class in the application's OnCreate event, as demonstrated below:

>[!TIP]
>```Pascal
> //uses Anthropic.Tutorial.VCL;
> TutorialHub := TVCLTutorialHub.Create(Memo1, Button1);
>```

or

>[!TIP]
>```Pascal
> //uses Anthropic.Tutorial.FMX;
> TutorialHub := TFMXTutorialHub.Create(Memo1, Button1);
>```

Make sure to add a `TMemo` and a `TButton` component to your form beforehand.

The `TButton` will allow the interruption of any streamed reception.

<br/>

## Asynchronous callback mode management

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
       OnProgress: TProc<TObject, T>;
       OnSuccess: TProc<TObject, T>;
       OnError: TProc<TObject, string>;
       OnCancellation: TProc<TObject>;
       OnDoCancel: TFunc<Boolean>;
```

The name of each property is self-explanatory; if needed, refer to the internal documentation for more details.

<br>

>[!NOTE]
> All methods managed by the wrapper are designed to support both synchronous and asynchronous execution modes. This dual-mode functionality ensures greater flexibility for users, allowing them to choose the approach that best suits their application's requirements and workflow.

<br/>

## Simplified Unit Declaration

To streamline the use of the API wrapper, the process for declaring units has been simplified. Regardless of the methods being utilized, you only need to reference the following two core units:

```Pascal
  uses
    Anthropic, Anthropic.Types;
```

If required, you may also include any plugin units developed for specific function calls (e.g., `Anthropic.Functions.Example`). This simplification ensures a more intuitive and efficient integration process for developers.

<br/>

# Usage

## Initialization

To initialize the API instance, you need to [obtain an API key from Anthropic](https://console.anthropic.com/settings/keys/).

Once you have a token, you can initialize `IAnthropic` interface, which is an entry point to the API.


> [!NOTE]
>```Pascal
>uses Anthropic;
>
>var Anthropic := TAnthropicFactory.CreateInstance(API_KEY);
>```

<br/>

To implement batch processing or enable caching, it is necessary to specify the corresponding elements in the request header :
- `Prompt Caching (Beta)`: To access this feature, include the `anthropic-beta: prompt-caching-2024-07-31` header in your API requests. 
- `Message Batches API (Beta)`: To use this feature, include the `anthropic-beta: message-batches-2024-09-24` header in your API requests, or call client.beta.messages.batches in your SDK. 

To automate the process, the `TAnthropicFactory` class provides two class methods. These methods simplify the code by removing the need to manually handle request headers :
- **CreateBatchingInstance**
- **CreateCachingInstance**

<br/>

>[!WARNING]
>To fully leverage the examples featured in this tutorial—especially when working with asynchronous methods—I suggest configuring the HuggingFace interface with the broadest possible scope. To simplify the tutorial and provide practical, ready-to-use code, we will set up the following instances:
>

```Pascal
  Anthropic: IAnthropic;
  AnthropicBatch: IAnthropic;
  AnthropicCaching: IAnthropic;

.....
  // Configuration in the OnCreate event
  Anthropic := TAnthropicFactory.CreateInstance(API_KEY);
  AnthropicBatch := TAnthropicFactory.CreateBatchingInstance(API_KEY);
  AnthropicCaching := TAnthropicFactory.CreateCachingInstance(API_KEY); 
```

<br/>

## Claude Models Overview

Claude models include a snapshot date in their name, ensuring a stable and identical version across platforms. The `-latest` alias points to the most recent version for testing convenience, but using a specific version is recommended in production to ensure stability. The -latest alias is updated with new releases while maintaining the same usage conditions and pricing.

Refer to the [official documentation](https://docs.anthropic.com/en/docs/about-claude/models)

### List of models

The list of available models can be retrieved from the Models API response. The models are ordered by release date, with the most recently published appearing first.

>[!TIP]
> For the purposes of this tutorial, we have chosen to use the `Anthropic.Tutorial.FMX` unit in the examples. However, you are free to substitute it with its VCL equivalent, `Anthropic.Tutorial.VCL`.
>

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;
  
  //Asynchronous example
  Anthropic.Models.AsynList(
    procedure (Params: TListModelsParams)
    begin
      Params.Limite(10);
    end,
    function : TAsynModels
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  //Synchronous example
//  var Value := Anthropic.Models.List(
//    procedure (Params: TListModelsParams)
//    begin
//      Params.Limite(10);
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br/>

### Retrieve a model

The Models API allows you to retrieve information about a specific model or map a model alias to its unique model ID.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  //Asynchronous example
  Anthropic.Models.AsynRetrieve(Model_ID,
    function : TAsynModel
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  //Synchronous example
//  var Value := Anthropic.Models.Retrieve(Model_ID);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

## Embeddings

`Anthropic` does not offer its own models for **text embeddings**. While the documentation mentions `Voyage AI` as an embeddings provider, we do not include access to their APIs in our **GitHub repository**. This is because providing tools for Voyage models falls outside the scope of our focus on `Anthropic` APIs exclusively. Users seeking embedding solutions are encouraged to explore various vendors to find the best fit for their needs, but our resources concentrate solely on supporting `Anthropic's` offerings.

<br/>

## Chats

`Claude` is capable of performing a wide range of text-based tasks. Trained on code, prose, and various natural language inputs, `Claude` excels in generating text outputs in response to detailed prompts. For optimal results, prompts should be written as detailed natural language instructions, and further improvements can be achieved through prompt engineering.

- **Text Summarization**: Condense lengthy content into key insights.
- **Content Generation:** Create engaging content like blog posts, emails, and product descriptions.
- **Data and Entity Extraction**: Extract structured information from unstructured text.
- **Question Answering**: Develop intelligent systems such as chatbots and educational tutors.
- **Text Translation**: Facilitate communication across different languages.
- **Text Analysis and Recommendations**: Analyze sentiment and patterns to personalize experiences.
- **Dialogue and Conversation**: Generate context-aware interactions for games and storytelling.
- **Code Explanation and Generation**: Assist in code reviews and generate boilerplate code.

Refer to [the prompt engineering overview](https://docs.anthropic.com/en/docs/build-with-claude/prompt-engineering/overview)

<br/>

### Create a message

You can send a structured list of input messages containing text and/or image content, and the model will generate the next message in the conversation.

The Messages API can be used for both single-turn requests and multi-turn, stateless conversations.

Example :
```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  //Asynchronous example
  Anthropic.Chat.AsynCreate(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.System('You are an expert in art history');
       Params.Messages([
         FromUser('Can you enlighten me on the technique of chiaroscuro and also on the Flemish school of painting in the 18th century ?')
       ]);
     end,
     function : TAsynChat
     begin
       Result.Sender := TutorialHub;
       Result.OnStart := Start;
       Result.OnSuccess := Display;
       Result.OnError := Display;
     end);

    //Synchronous example
//  var Chat := Anthropic.Chat.Create(
//     procedure (Params: TChatParams)
//     begin
//       Params.Model('claude-3-5-sonnet-20241022');
//       Params.MaxTokens(1024);
//       Params.System('You are an expert in art history');
//       Params.Messages([
//         FromUser('Can you enlighten me on the technique of chiaroscuro and also on the Flemish school of painting in the 18th century ?')
//       ]);
//     end);
//  try
//    Display(TutorialHub, Chat);
//    DisplayUsage(TutorialHub, Chat);
//  finally
//    Chat.Free;
//  end;
```

<br/>

### Streaming messages

When generating a Message, you can enable "stream": true to progressively receive the response through server-sent events (SSE).

Example :
```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  //Asynchronous example
  Anthropic.Chat.AsynCreateStream(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.System('You are an expert in art history');
       Params.Messages([
         FromUser('Can you enlighten me on the technique of chiaroscuro and also on the Flemish school of painting in the 18th century ?')
       ]);
       Params.Stream;
     end,
     function : TAsynChatStream
     begin
       Result.Sender := TutorialHub;
       Result.OnStart := Start;
       Result.OnProgress := DisplayStream;
       Result.OnSuccess := DisplayUsage;
       Result.OnError := Display;
       Result.OnDoCancel := DoCancellation;
       Result.OnCancellation := Cancellation;
     end);

    //Synchronous example
//  Anthropic.Chat.CreateStream(
//     procedure (Params: TChatParams)
//     begin
//       Params.Model('claude-3-5-sonnet-20241022');
//       Params.MaxTokens(1024);
//       Params.System('You are an expert in art history');
//       Params.Messages([
//         FromUser('Can you enlighten me on the technique of chiaroscuro and also on the Flemish school of painting in the 18th century ?')
//       ]);
//       Params.Stream;
//     end,
//     procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
//     begin
//       if not IsDone then
//         DisplayStream(TutorialHub, Chat) else
//         DisplayUsage(TutorialHub, Chat);
//       Application.ProcessMessages;
//     end);
```

<br/>

### Multi-turn conversation

The `Anthropic API` enables the creation of interactive chat experiences tailored to your users' needs. Its chat functionality supports multiple rounds of questions and answers, allowing users to gradually work toward solutions or receive help with complex, multi-step issues. This capability is especially useful for applications requiring ongoing interaction, such as:
- **Chatbots**
- **Educational tools**
- **Customer support assistants.**

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  //Streamed Asynchronous example
  Anthropic.Chat.AsynCreateStream(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.System('You are a funny domestic assistant.');
       Params.Messages([
         FromUser('Hello'),
         FromAssistant('Great to meet you. What would you like to know?'),
         FromUser('I have two dogs in my house. How many paws are in my house?')
       ]);
       Params.Stream;
     end,
     function : TAsynChatStream
     begin
       Result.Sender := TutorialHub;
       Result.OnStart := Start;
       Result.OnProgress := DisplayStream;
       Result.OnSuccess := DisplayUsage;
       Result.OnError := Display;
       Result.OnDoCancel := DoCancellation;
       Result.OnCancellation := Cancellation;
     end);
```

>[!TIP]
> The `FromUser` and `FromAssistant` methods streamline role management while enhancing code readability. They eliminate the need to use the `Payload` alias (e.g., `Payload.User('Hello')`) or the `TChatMessagePayload` type (e.g., `TChatMessagePayload.User('Hello')`).
>

<br/>

### Token counting

Token counting estimates the number of tokens in a message before sending it, helping manage costs and optimize message structure. The tool provides an estimate based on structured inputs (text, tools, PDFs) and supports Claude 3 and 3.5 models.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  //Asynchronous example
  Anthropic.Chat.AsynTokenCount(
    procedure (Params: TChatParams)
    begin
      Params.Model('claude-3-5-sonnet-20241022');
       Params.System(
         'You are an expert in art history'
       );
       Params.Messages([
         FromUser('In which artistic movement could we classify the Eiffel Tower?')
       ]);
    end,
    function : TAsynTokenCount
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  //Synchronous example
//  var Value := Anthropic.Chat.TokenCount(
//    procedure (Params: TChatParams)
//    begin
//      Params.Model('claude-3-5-sonnet-20241022');
//       Params.System(
//         'You are an expert in art history'
//       );
//       Params.Messages([
//         FromUser('In which artistic movement could we classify the Eiffel Tower?')
//       ]);
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br/>

## Document processing

`Claude 3.5 Sonnet` can extract text, analyze charts, and interpret content from `PDF documents`. Example use cases include financial report analysis, legal document extraction, translation, and data structuring.

PDF requirements:
- **Size:** up to 32 MB.
- **Pages:** up to 100.
- **Format:** standard, unprotected PDF.

Refer to the [official documentation](https://docs.anthropic.com/en/docs/build-with-claude/pdf-support).

This feature is available via API on `Claude 3.5 Sonnet` models and will soon be supported on additional platforms.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  var Pdf := 'https://assets.anthropic.com/m/1cd9d098ac3e6467/original/Claude-3-Model-Card-October-Addendum.pdf';

  //Streamed  asynchronous example
  Anthropic.Chat.AsynCreateStream(
      procedure(Params: TChatParams)
      begin
        Params.Model('claude-3-5-sonnet-20241022');
        Params.MaxTokens(1024);
        Params.Messages([
          FromPdf('Which model has the highest human preference win rates across each use-case?', Pdf)
        ]);
        Params.Stream(True);
      end,
      function: TAsynChatStream
      begin
        Result.Sender := TutorialHub;
        Result.OnStart := Start;
        Result.OnProgress := DisplayStream;
        Result.OnSuccess := DisplayUsage;
        Result.OnError := Display;
        Result.OnDoCancel := DoCancellation;
        Result.OnCancellation := Cancellation;
      end);

  //Streamed synchronous example
//  Anthropic.Chat.CreateStream(
//      procedure(Params: TChatParams)
//      begin
//        Params.Model('claude-3-5-sonnet-20241022');
//        Params.MaxTokens(1024);
//        Params.Messages([
//          FromPdf('Which model has the highest human preference win rates across each use-case?', Pdf)
//        ]);
//        Params.Stream(True);
//      end,
//      procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
//      begin
//        if not IsDone then
//          DisplayStream(TutorialHub, Chat) else
//          DisplayUsage(TutorialHub, Chat);
//        Application.ProcessMessages;
//      end);
```

>[!TIP]
> The `FromPdf` method can process PDF files from either a URL or local disk storage.
>

<br/>

## Vision

All `Claude version 3` models add vision capabilities, allowing them to analyze both images and text, expanding their potential for applications requiring multimodal understanding. See also the [official documentation](https://docs.anthropic.com/en/docs/build-with-claude/vision/).

To support both synchronous and asynchronous completion methods, we focused on generating the appropriate payload for message parameters. An overloaded version of the `TChatMessagePayload.User` class function was added, allowing users to include a dynamic array of text elements—file paths—alongside the user's text content. 
Internally, this data is processed to ensure the correct operation of the vision system in both synchronous and asynchronous contexts.

<br/>

### Passing a Base64 Encoded Image

Example :
```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;
  
  var Ref := 'T:\My_Folder\Images\Picture.png';

  //Streamed asynchronous example
  Anthropic.Chat.AsynCreateStream(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.Messages([
         FromUser('Describe this image.', [Ref])
       ]);
       Params.Stream;
     end,
     function : TAsynChatStream
     begin
       Result.Sender := TutorialHub;
       Result.OnStart := Start;
       Result.OnProgress := DisplayStream;
       Result.OnSuccess := DisplayUsage;
       Result.OnError := Display;
       Result.OnDoCancel := DoCancellation;
       Result.OnCancellation := Cancellation;
     end);

    //Streamed synchronous example
//  Anthropic.Chat.CreateStream(
//     procedure (Params: TChatParams)
//     begin
//       Params.Model('claude-3-5-sonnet-20241022');
//       Params.MaxTokens(1024);
//       Params.Messages([
//         FromUser('Describe this image.', [Ref])
//       ]);
//       Params.Stream;
//     end,
//     procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
//     begin
//       if not IsDone then
//         DisplayStream(TutorialHub, Chat) else
//         DisplayUsage(TutorialHub, Chat);
//       Application.ProcessMessages;
//     end);
```

<br/>

### Passing an Image URL

Example :
```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  var Ref := 'https://tripfixers.com/wp-content/uploads/2019/11/eiffel-tower-with-snow.jpeg';

  //Streamed asynchronous example
  Anthropic.Chat.AsynCreateStream(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.Messages([
         FromUser('Describe this image.', [Ref])
       ]);
       Params.Stream;
     end,
     function : TAsynChatStream
     begin
       Result.Sender := TutorialHub;
       Result.OnStart := Start;
       Result.OnProgress := DisplayStream;
       Result.OnSuccess := DisplayUsage;
       Result.OnError := Display;
       Result.OnDoCancel := DoCancellation;
       Result.OnCancellation := Cancellation;
     end);

    //Streamed synchronous example
//  Anthropic.Chat.CreateStream(
//     procedure (Params: TChatParams)
//     begin
//       Params.Model('claude-3-5-sonnet-20241022');
//       Params.MaxTokens(1024);
//       Params.Messages([
//         FromUser('Describe this image.', [Ref])
//       ]);
//       Params.Stream;
//     end,
//     procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
//     begin
//       if not IsDone then
//         DisplayStream(TutorialHub, Chat) else
//         DisplayUsage(TutorialHub, Chat);
//       Application.ProcessMessages;
//     end);
```

<br/>

## Function calling

Claude can connect with external client-side tools provided by users to perform various tasks more efficiently. 

>[!WARNING]
>Warning: Ensure user confirmation for actions like sending emails or making purchases to avoid unintended consequences.
>

For more details, refer to the Anthropic [website documentation.](https://docs.anthropic.com/en/docs/build-with-claude/tool-use) 

<br/>

### Overview of Tool Use in Claude

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

### Examples

What’s the weather in Paris?

In the `Anthropic.Functions.Example` unit, there is a class that defines a function which `Claude` can choose to use or not, depending on the options provided. This class inherits from a parent class defined in the `Anthropic.Functions.Core` unit. To create new functions, you can derive from the `TFunctionCore class` and define a new plugin.

In this unit, this schema will be used for function calls.

```Json
{
    "type": "object",
    "properties": {
         "location": {
             "type": "string",
             "description": "The city and department, e.g. Marseille, 13"
         },
         "unit": {
             "type": "string",
             "enum": ["celsius", "fahrenheit"]
         }
     },
     "required": ["location"]
  }
```
<br/>

1. We will use the TWeatherReportFunction plugin defined in the [`Anthropic.Functions.Example`](https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/source/Anthropic.Functions.Example.pas) unit.

```Pascal
  var WeatherFunc := TWeatherReportFunction.CreateInstance;  
  //See step 3
```

<br/>

2. We then define a method to display the **result** of the query using the **Weather tool**.

```Pascal
procedure TMy_Form.WeatherExecuteStream(const Value: string);
begin
  //Asynchronous example
  Anthropic.Chat.AsynCreateStream(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.Messages([
         FromUser(Value)
       ]);
       Params.System('You are a star weather presenter on a national TV channel.');
       Params.Stream;

     end,
     function : TAsynChatStream
     begin
       Result.Sender := TutorialHub;
       Result.OnProgress := DisplayStream;
       Result.OnSuccess := DisplayUsage;
       Result.OnError := Display;
     end);

    //Synchronous example
//  Anthropic.Chat.CreateStream(
//     procedure (Params: TChatParams)
//     begin
//       Params.Model('claude-3-5-sonnet-20241022');
//       Params.MaxTokens(1024);
//       Params.Messages([
//         FromUser(Value)
//       ]);
//       Params.System('You are a star weather presenter on a national TV channel.');
//       Params.Stream;
//     end,
//     procedure (var Chat: TChat; IsDone: Boolean; var Cancel: Boolean)
//     begin
//       if not IsDone then
//         DisplayStream(TutorialHub, Chat) else
//         DisplayUsage(TutorialHub, Chat);
//     end);
end;

```

<br/>

3. Building the query using the Weather tool

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX, Anthropic.Functions.Example;
  
  var WeatherFunc := TWeatherReportFunction.CreateInstance;
  TutorialHub.ToolCall := WeatherExecute;
  TutorialHub.Tool := WeatherFunc;

  //Asynchronous example
  Anthropic.Chat.AsynCreate(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.Messages([
         FromUser('What is the weather in Paris ?')
       ]);
       Params.ToolChoice(auto);
       Params.Tools([WeatherFunc]);
     end,
     function : TAsynChat
     begin
       Result.Sender := TutorialHub;
       Result.OnStart := Start;
       Result.OnSuccess := Display;
       Result.OnError := Display;
     end);

    //Synchronous example
//  var Chat := Anthropic.Chat.Create(
//     procedure (Params: TChatParams)
//     begin
//       Params.Model('claude-3-5-sonnet-20241022');
//       Params.MaxTokens(1024);
//       Params.Messages([
//         FromUser('What is the weather in Paris ?')
//       ]);
//       Params.ToolChoice(auto);
//       Params.Tools([WeatherFunc]);
//     end);
//  try
//    Display(TutorialHub, Chat);
//  finally
//    Chat.Free;
//  end;
```

<br/>


## Prompt Caching

`Prompt Caching` optimizes API usage by caching prompt prefixes, reducing processing time and costs for repetitive tasks. If a prompt prefix is cached from a recent query, it's reused; otherwise, the full prompt is processed and cached for future use. The cache lasts **5 minutes** and **is refreshed with each use**, making it ideal for prompts with many examples, background information, or consistent instructions.

For more details, refer to the Anthropic [website documentation.](https://docs.anthropic.com/en/docs/build-with-claude/prompt-caching/) 

<br/>

### Caching initialization

To include the `anthropic-beta: prompt-caching-2024-07-31` header, you must declare :

> [!NOTE]
>```Pascal
>uses Anthropic;
>
>AnthropicCaching := TAnthropicFactory.CreateCachingInstance(API_KEY); 
>```

Refert to [initialization](#initialization)

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
>- **2048 tokens** for `Claude 3.5 Haiku` and  `Claude 3 Haiku` <br/>
>Prompts shorter than these lengths cannot be cached, even if they include cache_control. Any request to cache a prompt with fewer tokens than the minimum required will be processed without caching. To check if a prompt was cached, refer to the response usage fields.
>
>The cache has a 5-minute time-to-live (TTL). Currently, the only supported cache type is "ephemeral," which corresponds to this 5-minute lifespan.
>

<br/>

### System Caching

In the following example, we have a plain text file `text/plain` whose size exceeds the minimum threshold for caching. We will include this file in the ***system section*** of the prompt. This can be beneficial in a ***multi-turn conversation***.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  var LongText := 'T:\my_folder\documents\legal.txt';

  //Asynchronous example
  AnthropicCaching.Chat.AsynCreate(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.System(
         'You are an AI assistant tasked with analyzing legal documents.' + sLineBreak +
         'Here is the full text of a complex legal agreement:',
         LongText
       );
       Params.Messages([
         FromUser('What are the important key points?', True)
       ]);
     end,
     function : TAsynChat
     begin
       Result.Sender := TutorialHub;
       Result.OnStart := Start;
       Result.OnSuccess := Display;
       Result.OnError := Display;
     end);

    //Synchronous example
//  var Chat := AnthropicCaching.Chat.Create(
//     procedure (Params: TChatParams)
//     begin
//       Params.Model('claude-3-5-sonnet-20241022');
//       Params.MaxTokens(1024);
//       Params.System(
//         'You are an AI assistant tasked with analyzing legal documents.' + sLineBreak +
//         'Here is the full text of a complex legal agreement:',
//         LongText
//       );
//       Params.Messages([
//         FromUser('What are the important key points?', True)
//       ]);
//     end);
//  try
//    Display(TutorialHub, Chat);
//    DisplayUsage(TutorialHub, Chat);
//  finally
//    Chat.Free;
//  end;
```

Not only is the message flagged for caching (`FromUser('What are the important key points?', True)`), but the system parameters are also seamlessly configured, as two elements are defined: a text and a file.

<br/>

### Tools Caching

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
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX, Anthropic.Functions.Example;

  var Chat := AnthropicCaching.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model('claude-3-5-sonnet-20241022');
       Params.MaxTokens(1024);
       Params.Messages([
            FromUser('my request')
       ]);
       Params.ToolChoice(auto);
       Params.Tools([tool_1, ... , tool_n, WeatherFunc]);
         // List of tools provided to Claude
     end);
   ...
```

And so the whole list of tools will be cached.

<br/>

### Images Caching

```pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  var Ref1 := 'T:\my_folder\Images\My_Image1.png';
  var Ref2 := 'T:\my_folder\Images\My_Image2.png';

  var Chat := AnthropicCaching.Chat.Create(
     procedure (Params: TChatParams)
     begin
       Params.Model(Models[ClaudeHaiku3]);
       Params.MaxTokens(1024);
       Params.Messages([
            FromUser('Describe these images.', [Ref1, Ref2], True)  //True for the caching
       ]);
     end);
  ...
```

<br/>

## Message Batches

The `Message Batches` API enables efficient, asynchronous processing of large volumes of message requests. This method is ideal for tasks that don’t require immediate responses, cutting costs by 50% and boosting throughput.

For more details, refer to the [Anthropic website documentation](https://docs.anthropic.com/en/docs/build-with-claude/message-batches/).

<br/>

### Message Batches initialization

To include the `anthropic-beta: message-batches-2024-09-24` header, you must declare :

> [!NOTE]
>```Pascal
>uses Anthropic;
>
>AnthropicBatche := TAnthropicFactory.CreateBatchingInstance(BaererKey);
>```

Refert to [initialization](#initialization)

The `Message Batches` API supports `Claude 3.5 Sonnet`, `Claude 3.5 Haiku`, `Claude 3 Haiku`, and `Claude 3 Opus`. Any request that can be made through the Messages API can be batched, including : 
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

### How it works

The `Message Batches` API creates a batch of requests, processed asynchronously with each request handled independently. You can track the batch status and retrieve results once processing is complete. This is ideal for large-scale tasks like evaluations, content moderation, data analysis, or bulk content generation.

<br/>

### Batch create

A Message Batch consists of a collection of requests to generate individual Messages. Each request is structured as follows:

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  // Create the content of the batche
  var Request := TRequestParams.Create;
  Request.Requests([
    TBatcheParams.Add('my-first-request',
        procedure (Params: TChatParams)
        begin
          Params.Model('claude-3-5-sonnet-20241022');
          Params.MaxTokens(1024);
          Params.Messages([
            FromUser('Hello, world') ]);
        end),
    TBatcheParams.Add('my-second-request',
        procedure (Params: TChatParams)
        begin
          Params.Model('claude-3-5-sonnet-20241022');
          Params.MaxTokens(1024);
          Params.Messages([
            FromUser('Hi again, friend') ]);
        end)
    ]);
  Display(TutorialHub, Request.ToFormat());

  TutorialHub.JSONParam := Request;

  //ASynchronous example
  AnthropicBatche.Batche.AsynCreate(Request.JSON,
    function : TAsynBatche
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  // Synchronous example
//  var Batche := AnthropicBatche.Batche.Create(Request.JSON);
//  try
//    Display(TutorialHub, Batche);
//  finally
//    Batche.Free;
//  end;
```

<br/>

**alternative approach**
 - In this approach, each item in the batch is defined in a JSONL file (`BatchExample.jsonl`). For instance, using the previous example, the JSONL file could be structured as follows:

```Json
{ "custom_id": "my-first-request","params":{"model":"claude-3-5-sonnet-20241022","max_tokens": 1024,"messages":[{"role":"user","content":"Hello, world"}]}}
{"custom_id":"my-second-request",  "params":{"model":"claude-3-5-sonnet-20241022","max_tokens": 1024,"messages":[{"role":"user","content":"Hi again, friend"}]}}
```

<br/>

Therefore, the batch creation would be carried out using the following code:

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  TutorialHub.FileName := 'BatchExample.jsonl';

  //Asynchronous example
  AnthropicBatche.Batche.AsynCreate(TutorialHub.FileName,
    function : TAsynBatche
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

    //Synchronous example
//  var Batche := AnthropicBatche.Batche.Create(TutorialHub.FileName);
//  try
//    Display(TutorialHub, Batche);
//  finally
//    Batche.Free;
//  end;
``` 

<br/>

### Batch list

Retrieve all message batches within a workspace, with the most recently created batches appearing first.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  //Asynchronous example
  AnthropicBatche.Batche.AsynList(
    procedure (Params: TListParams)
    begin
      Params.Limite(20);
    end,
    function : TAsynBatcheList
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  //Synchronous example
//  var Batche := AnthropicBatche.Batche.List(
//    procedure (Params: TListParams)
//    begin
//      Params.Limite(20);
//    end);
//  try
//    Display(TutorialHub, Batche);
//  finally
//    Batche.Free;
//  end;
```

You can use the "list" API with the following query parameters:

- **before_id (string)**: Use this parameter as a cursor for pagination. When specified, it returns the page of results immediately preceding the object identified by this ID.

- **after_id (string)**: Similar to the above, but this cursor returns the page of results immediately following the specified object ID.

- **limit (integer)**: Specifies how many items to return per page. The default is set to 20, with valid values ranging from 1 to 100.

<br/>

### Batch cancel

Batches can be canceled at any point before the processing is fully completed. Once a cancellation is triggered, the batch moves into a canceling state, during which the system may still finish any ongoing, non-interruptible requests before the cancellation is finalized.

The count of canceled requests is listed in the `request_counts`. To identify which specific requests were canceled, review the individual results within the batch. Keep in mind that no requests may actually be canceled if all active requests were non-interruptible.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  TutorialHub.BatchId := ID;

  //ASynchonous example
  AnthropicBatche.Batche.AsynCancel(TutorialHub.BatchId,
    function : TAsynBatche
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end)

  //Synchronous example
//  var Batche := AnthropicBatche.Batche.Cancel(TutorialHub.BatchId);
//  try
//    Display(TutorialHub, Batche);
//  finally
//    Batche.Free;
//  end;
```

<br/>

### Batch retrieve message

This endpoint is repeatable and can be used to check the status of a Message Batch completion. To retrieve the results of a Message Batch, make a request to the `results_url` field provided in the response.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  TutorialHub.BatchId := ID;

  //Asynchronous example
  AnthropicBatche.Batche.AsynRetrieve(TutorialHub.BatchId,
    function : TAsynBatche
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  //Synchronous example
//  var Batche := AnthropicBatche.Batche.Retrieve(TutorialHub.BatchId);
//  try
//    Display(TutorialHub, Batche);
//  finally
//    Batche.Free;
//  end;
```

<br/>

### Batch retrieve results

Streams the results of a Message Batch in a **JSONL** file format.

Each line in the file represents a **JSON** object containing the outcome of an individual request from the Message Batch. The order of results may not correspond to the original request order, so use the `custom_id` field to align results with their respective requests.

>[!WARNING]
>The path to retrieve Message Batch results should be obtained from the `results_url` of the batch. This path should not be assumed, as it may vary.
>

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  TutorialHub.BatchId := ID;
  TutorialHub.FileName := 'Result.jsonl';

  //Asynchronous example
  AnthropicBatche.Batche.AsynRetrieve(TutorialHub.BatchId, TutorialHub.FileName,
    function : TAsynStringList
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  //Synchronous example
//  var JSONL := AnthropicBatche.Batche.Retrieve(TutorialHub.BatchId, TutorialHub.FileName);
//  try
//    Display(TutorialHub, JSONL);
//  finally
//    JSONL.Free;
//  end;  
```

In the `Anthropic.Batches.Support.pas` unit, the object interface `IBatcheResults` allows access to the data returned by Claude by providing the name of the **JSONL** file containing the batch data. All the information can be accessed through the Batches array, as demonstrated in the example below.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;
  
  if not FileExists('Result.jsonl') then
    begin
      Display(TutorialHub, 'Result.jsonl not found');
      Exit;
    end;

  var BatchResults := TBatcheResultsFactory.CreateInstance('Result.jsonl');
  Display(TutorialHub, BatchResults);
```

<br/>

### Batch delete

Message Batches can only be deleted after they have completed processing. To delete a batch that is still in progress, you must cancel it first.

```Pascal
// uses Anthropic, Anthropic.Types, Anthropic.Tutorial.FMX;

  TutorialHub.BatchId := ID;

  //Asynchronous example
  AnthropicBatche.Batche.AsynDelete(TutorialHub.BatchId,
    function : TAsynBatchDelete
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);

  //Synchronous example
//  var Value := AnthropicBatche.Batche.Delete(TutorialHub.BatchId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

>[!CAUTION]
> A 500 error may occur when attempting to delete messages in bulk, particularly for batches created before the release of the dedicated deletion API. This issue is especially concerning as it could result in a permanent inability to delete these batches, particularly if a significant number of them were generated prior to the availability of the deletion API.
>

<br/>

### Console

> [!NOTE]
>You can access all batches through the [Anthropic console](https://console.anthropic.com/settings/workspaces/default/batches). A complete history is maintained, allowing you to view and download the computed results.
>

<br/>

# Contributing

Pull requests are welcome. If you're planning to make a major change, please open an issue first to discuss your proposed changes.

# License

This project is licensed under the [MIT](https://choosealicense.com/licenses/mit/) License.

