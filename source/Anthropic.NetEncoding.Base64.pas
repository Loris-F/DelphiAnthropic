unit Anthropic.NetEncoding.Base64;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding, System.Net.Mime;

  /// <summary>
  /// Encodes the content of a file into a Base64-encoded string.
  /// </summary>
  /// <param name="FileLocation">The full path to the file that will be encoded.</param>
  /// <returns>A Base64-encoded string representing the content of the file.</returns>
  /// <exception cref="Exception">Thrown if the specified file does not exist at the provided location.</exception>
  /// <remarks>
  /// This method reads the file from the specified location and converts it to a Base64 string.
  /// It uses different encoding methods depending on the version of the RTL.
  /// For RTL version 35.0 and later, it uses <c>TNetEncoding.Base64String.Encode</c>,
  /// and for earlier versions, it uses <c>TNetEncoding.Base64.Encode</c>.
  /// </remarks>
  function EncodeBase64(FileLocation : string) : string;

  /// <summary>
  /// Retrieves the MIME type of the specified file based on its location.
  /// </summary>
  /// <param name="FileLocation">The full path to the file whose MIME type is to be resolved.</param>
  /// <returns>
  /// A string representing the MIME type of the file.
  /// If the file does not exist, an exception will be raised.
  /// </returns>
  /// <exception cref="Exception">
  /// Thrown if the specified file cannot be found at the provided location.
  /// </exception>
  /// <remarks>
  /// This method checks if the specified file exists and retrieves its MIME type
  /// using the <c>TMimeTypes.Default.GetFileInfo</c> method.
  /// Ensure the provided path is valid before calling this function.
  /// </remarks>
  function ResolveMimeType(const FileLocation: string): string;

  /// <summary>
  /// Validates the MIME type of the specified file to ensure it is supported.
  /// </summary>
  /// <param name="FileLocation">
  /// The full path to the file whose MIME type is to be checked.
  /// </param>
  /// <exception cref="Exception">
  /// Thrown if the specified file cannot be found at the provided location, or if the MIME type
  /// of the file is not supported.
  /// </exception>
  /// <remarks>
  /// This procedure verifies if the file exists at the given path and checks its MIME type
  /// using the <see cref="ResolveMimeType"/> function.
  /// <para>
  /// Supported MIME types include: image/png, image/jpeg, image/gif, image/webp, and application/pdf.
  /// </para>
  /// <para>
  /// If the file's MIME type is not in this list, an exception is raised indicating an unsupported format.
  /// </para>
  /// </remarks>
  procedure CheckMimeType(const MimeType: string);

  /// <summary>
  /// Retrieves the MIME type of the specified file and returns its content as a Base64-encoded string.
  /// </summary>
  /// <param name="FileLocation">
  /// The full path to the file that will be encoded.
  /// </param>
  /// <param name="MimeType">
  /// On successful return, this parameter contains the MIME type of the file.
  /// </param>
  /// <returns>
  /// A Base64-encoded string representing the content of the file.
  /// </returns>
  /// <exception cref="Exception">
  /// Thrown if the file does not exist or if its MIME type cannot be determined.
  /// </exception>
  /// <remarks>
  function FileToBase64(FileLocation : string; var MimeType: string): string;

implementation

uses
  System.StrUtils;

function EncodeBase64(FileLocation : string): string;
begin
  if not FileExists(FileLocation) then
    raise Exception.CreateFmt('File not found : %s', [FileLocation]);

  var Stream := TMemoryStream.Create;
  var StreamOutput := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.LoadFromFile(FileLocation);
    Stream.Position := 0;
    {$IF RTLVersion >= 35.0}
    TNetEncoding.Base64String.Encode(Stream, StreamOutput);
    {$ELSE}
    TNetEncoding.Base64.Encode(Stream, StreamOutput);
    {$ENDIF}
    Result := StreamOutput.DataString;
  finally
    Stream.Free;
    StreamOutput.Free;
  end;
end;

function ResolveMimeType(const FileLocation: string): string;
begin
  if not FileExists(FileLocation) then
    raise Exception.CreateFmt('File not found: %s', [FileLocation]);

  var LKind: TMimeTypes.TKind;
  TMimeTypes.Default.GetFileInfo(FileLocation, Result, LKind);
end;

procedure CheckMimeType(const MimeType: string);
begin
  if IndexStr(MimeType.ToLower, ['image/png', 'image/jpeg', 'image/gif', 'image/webp', 'application/pdf']) = -1 then
    raise Exception.Create('Unsupported document format');
end;

function FileToBase64(FileLocation : string; var MimeType: string): string;
begin
  MimeType := ResolveMimeType(FileLocation);
  Result := EncodeBase64(FileLocation);
end;

end.
