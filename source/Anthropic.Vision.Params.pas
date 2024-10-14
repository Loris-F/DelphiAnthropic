unit Anthropic.Vision.Params;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.StrUtils;

type
  /// <summary>
  /// The <c>TVisionSource</c> record represents an images source used in the vision system.
  /// The source can be a Base64-encoded image.
  /// </summary>
  /// <remarks>
  /// The <c>TVisionSource</c> record is responsible for holding the image data and validating its format.
  /// It support Base64-encoded image.
  /// The record includes methods to create and validate these source.
  /// Example usage:
  /// <code>
  /// var Source: TVisionSource;
  /// Source := TVisionSource.Create(['c:\my_folder\img1.webp', 'c:\my_folder\img2.png']);
  /// </code>
  /// </remarks>
  TVisionSource = record
  private
    FValue: WideString;
    FMimeType: string;
    procedure FileCheck(const Value: string);
  public
    /// <summary>
    /// "Data" field is the Base64 image format, included with the request.
    /// </summary>
    property Data: WideString read FValue write FValue;

    property MimeType: string read FMimeType write FMimeType;
    /// <summary>
    /// Creates an instance of <c>TVisionSource</c> based on the provided value.
    /// </summary>
    /// <param name="Value">
    /// The value representing the filename to Base64-encoded encode.
    /// </param>
    /// <returns>
    /// A <c>TVisionSource</c> object contains the image Base64-encoded.
    /// </returns>
    /// <exception cref="Exception">
    /// Thrown if the provided file does not exists
    /// </exception>
    /// <remarks>
    /// The method attempts to handle the value as a Base64-encoded image.
    /// </remarks>
    class function Create(const FileName: string): TVisionSource; overload; static;
  end;

implementation

uses
  Anthropic.NetEncoding.Base64;

{ TVisionSource }

class function TVisionSource.Create(const FileName: string): TVisionSource;
begin
  {--- Handle the value as a Base64-encoded image }
  Result.FileCheck(FileName);
end;

procedure TVisionSource.FileCheck(const Value: string);
begin
  FMimeType := ResolveMimeType(Value);

  if IndexStr(FMimeType, ['image/png', 'image/jpeg', 'image/gif', 'image/webp']) = -1 then
    raise Exception.Create('Unsupported image format');

  Data := EncodeBase64(Value);
end;

end.
