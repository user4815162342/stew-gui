unit sys_dynval_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_dynval, jsonscanner, sys_json;

type

  { TJSONWriterState }

  TJSONWriterState = record
    Indent: UTF8String;
    StepBack: UTF8String;
    IsMap: Boolean;
    ItemsWritten: Boolean;
  end;

  TJSONWriterStateArray = array of TJSONWriterState;

  { TJSONWriter }


  TJSONWriter = class(TDynamicValueWriter)
  strict private
    fStream: TStream;
    fGap: LongInt;
    procedure Write(const aValue: UTF8String);
    procedure WriteQuote(const aValue: UTF8String);
  strict protected
    procedure WriteBooleanToken(const aValue: Boolean); override;
    procedure WriteGap(aIndentCount: Longint); override;
    procedure WriteNullToken; override;
    procedure WriteNumberToken(const aValue: Double); override;
    procedure WriteStringToken(const aValue: UTF8String); override;
    procedure WriteMapStartToken; override;
    procedure WriteMapEndToken; override;
    procedure WriteListEndToken; override;
    procedure WriteListStartToken; override;
    procedure WriteKeyIndicatorToken; override;
    procedure WriteListSeparatorToken; override;
  public
    constructor Create(aStream: TStream; aIndent: Word);
    destructor Destroy; override;
  end;

  TJSONTokens = set of TJSONToken;

  { TJSONReader }



  TJSONReader = class(TDynamicValueReader)
  strict private
    // FUTURE: Make sure this is the best scanner available
    fScanner: TJSONScanner;
    procedure Expect(const aTokens: TJSONTokens; const aValue: UTF8String);
    procedure SkipWhitespace;
  strict protected
    function ReadBooleanToken: Boolean; override;
    procedure ReadListEndToken; override;
    procedure ReadListSeparatorToken; override;
    procedure ReadListStartToken; override;
    procedure ReadMapEndToken; override;
    procedure ReadMapStartToken; override;
    procedure ReadNullToken; override;
    function ReadNumberToken: Double; override;
    function ReadStringToken: UTF8String; override;
    procedure ReadKeyIndicatorToken; override;
  public
    function IsBoolean: Boolean; override;
    function IsListEnd: Boolean; override;
    function IsListStart: Boolean; override;
    function IsMapEnd: Boolean; override;
    function IsMapStart: Boolean; override;
    function IsNull: Boolean; override;
    function IsNumber: Boolean; override;
    function IsString: Boolean; override;
    constructor Create(aStream: TStream);
    destructor Destroy; override;
  end;

procedure ToJSON(const aObject: IDynamicValue; const aStream: TStream; aIndent: Word = 0);
function ToJSON(const aObject: IDynamicValue; const aIndent: Word = 0): UTF8String;
function FromJSON(const aStream: TStream): IDynamicValue;
function FromJSON(const aString: UTF8String): IDynamicValue;

function ToOldJSONValue(const aValue: IDynamicValue): TJSValue;
function FromOldJSONValue(const aValue: TJSValue): IDynamicValue;

const
  NullText: UTF8String = 'null';
  TrueText: UTF8String = 'true';
  FalseText: UTF8String = 'false';

implementation

uses
  Math;

procedure ToJSON(const aObject: IDynamicValue; const aStream: TStream; aIndent: Word);
var
  lWriter: TJSONWriter;
begin
  lWriter := TJSONWriter.Create(aStream,aIndent);
  try
    lWriter.WriteValue(aObject);
  finally
    lWriter.Free;
  end;

end;

function ToJSON(const aObject: IDynamicValue; const aIndent: Word): UTF8String;
var
  lOutput: TStringStream;
begin
  lOutput := TStringStream.Create('');
  try
    ToJSON(aObject,lOutput,aIndent);
    Result := lOutput.DataString;
  finally
    lOutput.Free;
  end;

end;

function FromJSON(const aStream: TStream): IDynamicValue;
var
  lReader: TJSONReader;
begin
  lReader := TJSONReader.Create(aStream);
  try
    Result := lReader.ReadValue;
  finally
    lReader.Free;
  end;

end;

function FromJSON(const aString: UTF8String): IDynamicValue;
var
  lStream: TStringStream;
begin
  lStream := TStringStream.Create(aString);
  try
    result := FromJSON(lStream);
  finally
    lStream.Free;
  end;

end;

function ToOldJSONValue(const aValue: IDynamicValue): TJSValue;
begin
  if aValue <> nil then
     result := sys_json.FromJSON(ToJSON(aValue,0))
  else
     result := nil;
end;

function FromOldJSONValue(const aValue: TJSValue): IDynamicValue;
begin
  if aValue <> nil then
     result := FromJSON(sys_json.ToJSON(aValue))
  else
     result := nil;
end;

{ TJSONReader }

procedure TJSONReader.Expect(const aTokens: TJSONTokens; const aValue: UTF8String);
begin
  if not (fScanner.CurToken in aTokens) then
     raise Exception.Create('Expected ' + aValue + ' in JSON data.');
end;

procedure TJSONReader.SkipWhitespace;
begin
  while fScanner.CurToken = tkWhitespace do
    fScanner.FetchToken;
end;

constructor TJSONReader.Create(aStream: TStream);
begin
  inherited Create;
  fScanner := TJSONScanner.Create(aStream,true);
  fScanner.Strict := true;
  fScanner.FetchToken;
  SkipWhitespace;
end;

destructor TJSONReader.Destroy;
begin
  FreeAndNil(fScanner);
  inherited Destroy;
end;

function TJSONReader.IsBoolean: Boolean;
begin
  result := fScanner.CurToken in [tkTrue,tkFalse];
end;

function TJSONReader.IsListEnd: Boolean;
begin
  result := fScanner.CurToken = tkSquaredBraceClose;
end;

function TJSONReader.IsListStart: Boolean;
begin
  result := fScanner.CurToken = tkSquaredBraceOpen;

end;

function TJSONReader.IsMapEnd: Boolean;
begin
  result := fScanner.CurToken = tkCurlyBraceClose;

end;

function TJSONReader.IsMapStart: Boolean;
begin
  result := fScanner.CurToken = tkCurlyBraceOpen;

end;

function TJSONReader.IsNull: Boolean;
begin
  result := fScanner.CurToken = tkNull;

end;

function TJSONReader.IsNumber: Boolean;
begin
  result := fScanner.CurToken = tkNumber;

end;

function TJSONReader.IsString: Boolean;
begin
  result := fScanner.CurToken = tkString;

end;

function TJSONReader.ReadBooleanToken: Boolean;
begin
  Expect([tkTrue,tkFalse],'Boolean');
  result := fScanner.CurToken = tkTrue;
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadListEndToken;
begin
  Expect([tkSquaredBraceClose],'List end');
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadListSeparatorToken;
begin
  Expect([tkComma],'List separator');
  fScanner.FetchToken;
  SkipWhitespace;
end;

procedure TJSONReader.ReadListStartToken;
begin
  Expect([tkSquaredBraceOpen],'List start');
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadMapEndToken;
begin
  Expect([tkCurlyBraceClose],'Map end');
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadMapStartToken;
begin
  Expect([tkCurlyBraceOpen],'Map start');
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadNullToken;
begin
  Expect([tkNull],'Null');
  fScanner.FetchToken;
  SkipWhitespace;

end;

function TJSONReader.ReadNumberToken: Double;
var
  lResult: UTF8String;
begin
  Expect([tkNumber],'Number');
  lResult := fScanner.CurTokenString;
  if not TryStrToFloat(lResult,Result) then
     result := Math.NaN;
  fScanner.FetchToken;
  SkipWhitespace;


end;

function TJSONReader.ReadStringToken: UTF8String;
begin
  Expect([tkString],'String');
  result := fScanner.CurTokenString;
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadKeyIndicatorToken;
begin
 Expect([tkColon],'Key indicator');
 fScanner.FetchToken;
 SkipWhitespace;

end;

{ TJSONWriter }

procedure TJSONWriter.Write(const aValue: UTF8String);
begin
 if aValue <> '' then
    fStream.Write(aValue[1],Length(aValue));
end;

procedure TJSONWriter.WriteQuote(const aValue: UTF8String);
var
  lText: UTF8String;
  i: Integer;
  c: Char;
begin
  lText := '"';
  for i := 1 to Length(aValue) do
  begin
    c := aValue[i];
    case c of
      '"','\':
        lText := lText + '\' + c;
      #$08,#$0C,#$0A,#$0D,#$09:
      begin
        lText := lText + '\';
        case c of
          #$08:
            lText := lText + 'b';
          #$0C:
            lText := lText + 'f';
          #$0A:
            lText := lText + 'n';
          #$0D:
            lText := lText + 'r';
          #$09:
            lText := lText + 't';
        end;
      end;
      else if c < ' ' then
      begin
        lText := lText +  '\' + 'u' + hexStr(ord(c),4);
      end
      else
        lText := lText + c;

    end;
  end;
  lText := lText + '"';
  Write(lText);
end;

constructor TJSONWriter.Create(aStream: TStream; aIndent: Word);
begin
  inherited Create;
  fStream := aStream;

  // In EcmaScript specs for JSON.stringify, the longest indent is 10 characters.
  // I'm doing a sanity filter by also making sure the string is only
  // spaces, and not something else.
  fGap := math.Min(aIndent,10);

end;

destructor TJSONWriter.Destroy;
begin
  inherited Destroy;
end;

procedure TJSONWriter.WriteBooleanToken(const aValue: Boolean);
begin
 Write(BoolToStr(aValue,TrueText,FalseText));
end;

procedure TJSONWriter.WriteGap(aIndentCount: Longint);
begin
 if (fGap > 0) then
 begin
   Write(#10 + StringOfChar(' ',aIndentCount * fGap))
 end;
end;


procedure TJSONWriter.WriteListStartToken;
begin
 Write('[');
end;

procedure TJSONWriter.WriteKeyIndicatorToken;
begin
 if fGap > 0 then
    Write(': ')
 else
    Write(':');
end;

procedure TJSONWriter.WriteListEndToken;
begin
 Write(']');

end;

procedure TJSONWriter.WriteListSeparatorToken;
begin
  Write(',');
end;

procedure TJSONWriter.WriteMapStartToken;
begin
 Write('{');
end;

procedure TJSONWriter.WriteMapEndToken;
begin
 Write('}');

end;

procedure TJSONWriter.WriteNullToken;
begin
  Write(NullText);
end;

procedure TJSONWriter.WriteNumberToken(const aValue: Double);
begin
  Write(FloatToStr(aValue));

end;

procedure TJSONWriter.WriteStringToken(const aValue: UTF8String);
begin
  WriteQuote(aValue);

end;

end.

