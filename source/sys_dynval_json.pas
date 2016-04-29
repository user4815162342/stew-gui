unit sys_dynval_json;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sys_dynval, jsonscanner;

type

  // TODO: Test JSON Reading and writing...
  // TODO: Then, work on the dynobject_store stuff.

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
  private
    procedure SetItemsWritten(AValue: Boolean);
  private
    fStack: TJSONWriterStateArray;
    fStream: TStream;
    fGap: LongInt;
    procedure Write(aValue: UTF8String);
    procedure WriteQuote(aValue: UTF8String);
    procedure WriteValueHeader;
    procedure Pop;
    function Peek: TJSONWriterState;
    procedure Push(aIsMap: Boolean);
  public
    constructor Create(aStream: TStream; aIndent: Word);
    destructor Destroy; override;
    procedure WriteBoolean(aValue: Boolean); override;
    procedure WriteListEnd; override;
    procedure WriteListSeparator; override;
    procedure WriteListStart; override;
    procedure WriteMapEnd; override;
    procedure WriteMapKey(aValue: UTF8String); override;
    procedure WriteMapStart; override;
    procedure WriteNull; override;
    procedure WriteNumber(aValue: Double); override;
    procedure WriteString(aValue: UTF8String); override;
  end;

  TJSONTokens = set of TJSONToken;

  { TJSONReader }



  TJSONReader = class(TDynamicValueReader)
  private
    // FUTURE: Make sure this is the best scanner available
    fScanner: TJSONScanner;
    procedure Expect(aTokens: TJSONTokens; const aValue: UTF8String);
    procedure SkipWhitespace;
  public
    constructor Create(aStream: TStream);
    destructor Destroy; override;
    function IsBoolean: Boolean; override;
    function IsListEnd: Boolean; override;
    function IsListStart: Boolean; override;
    function IsMapEnd: Boolean; override;
    function IsMapStart: Boolean; override;
    function IsNull: Boolean; override;
    function IsNumber: Boolean; override;
    function IsString: Boolean; override;
    function ReadBoolean: Boolean; override;
    procedure ReadListEnd; override;
    procedure ReadListSeparator; override;
    procedure ReadListStart; override;
    procedure ReadMapEnd; override;
    function ReadMapKey: UTF8String; override;
    procedure ReadMapStart; override;
    procedure ReadNull; override;
    function ReadNumber: Double; override;
    function ReadString: UTF8String; override;

  end;

procedure ToJSON(aObject: IDynamicValue; aStream: TStream; aIndent: Word = 0);
function ToJSON(aObject: IDynamicValue; aIndent: Word = 0): UTF8String;
function FromJSON(aStream: TStream): IDynamicValue;
function FromJSON(aString: UTF8String): IDynamicValue;

const
  NullText: UTF8String = 'null';
  TrueText: UTF8String = 'true';
  FalseText: UTF8String = 'false';

implementation

uses
  Math;

procedure ToJSON(aObject: IDynamicValue; aStream: TStream; aIndent: Word);
var
  lWriter: TJSONWriter;
begin
  lWriter := TJSONWriter.Create(aStream,aIndent);
  try
    WriteDynamicValue(aObject,lWriter);
  finally
    lWriter.Free;
  end;

end;

function ToJSON(aObject: IDynamicValue; aIndent: Word): UTF8String;
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

function FromJSON(aStream: TStream): IDynamicValue;
var
  lReader: TJSONReader;
begin
  lReader := TJSONReader.Create(aStream);
  try
    Result := ReadDynamicValue(lReader);
  finally
    lReader.Free;
  end;

end;

function FromJSON(aString: UTF8String): IDynamicValue;
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

{ TJSONReader }

procedure TJSONReader.Expect(aTokens: TJSONTokens; const aValue: UTF8String);
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

function TJSONReader.ReadBoolean: Boolean;
begin
  Expect([tkTrue,tkFalse],'Boolean');
  result := fScanner.CurToken = tkTrue;
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadListEnd;
begin
  Expect([tkSquaredBraceClose],'List end');
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadListSeparator;
begin
  Expect([tkComma],'List separator');
  fScanner.FetchToken;
  SkipWhitespace;
end;

procedure TJSONReader.ReadListStart;
begin
  Expect([tkSquaredBraceOpen],'List start');
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadMapEnd;
begin
  Expect([tkCurlyBraceClose],'Map end');
  fScanner.FetchToken;
  SkipWhitespace;

end;

function TJSONReader.ReadMapKey: UTF8String;
begin
  Expect([tkString],'Map key');
  result := fScanner.CurTokenString;
  fScanner.FetchToken;
  SkipWhitespace;
  Expect([tkColon],'Map key indicator');
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadMapStart;
begin
  Expect([tkCurlyBraceOpen],'Map start');
  fScanner.FetchToken;
  SkipWhitespace;

end;

procedure TJSONReader.ReadNull;
begin
  Expect([tkNull],'Null');
  fScanner.FetchToken;
  SkipWhitespace;

end;

function TJSONReader.ReadNumber: Double;
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

function TJSONReader.ReadString: UTF8String;
begin
  Expect([tkString],'String');
  result := fScanner.CurTokenString;
  fScanner.FetchToken;
  SkipWhitespace;

end;

{ TJSONWriter }

procedure TJSONWriter.SetItemsWritten(AValue: Boolean);
var
  l: Longint;
begin
 l := Length(fStack);
 if l > 0 then
 begin
   fStack[l - 1].ItemsWritten := AValue;
 end
 else
   raise Exception.Create('TJSONWriter has gone past the top of the stack');
end;

procedure TJSONWriter.Write(aValue: UTF8String);
begin
 if aValue <> '' then
    fStream.Write(aValue[1],Length(aValue));
end;

procedure TJSONWriter.WriteQuote(aValue: UTF8String);
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

procedure TJSONWriter.WriteValueHeader;
begin
 if (fGap > 0) and (Length(fStack) > 0) and (not Peek.IsMap) then
 begin
   Write(#10 + Peek.Indent)
 end;
end;

procedure TJSONWriter.Pop;
var
  l: Longint;
begin
 l := Length(fStack);
 if l > 0 then
 begin
   SetLength(fStack,l - 1);
 end
 else
   raise Exception.Create('TJSONWriter has gone past the top of the stack');
end;

function TJSONWriter.Peek: TJSONWriterState;
var
  l: Longint;
begin
 l := Length(fStack);
 if l > 0 then
 begin
   result := fStack[l - 1];
 end
 else
   raise Exception.Create('TJSONWriter has gone past the top of the stack');
end;

procedure TJSONWriter.Push(aIsMap: Boolean);
var
  l: Longint;
  lIndent: UTF8String;
begin
 l := Length(fStack);
 if l > 0 then
   lIndent := fStack[l - 1].Indent
 else
   lIndent := '';
 SetLength(fStack,l + 1);
 fStack[l].Indent := lIndent + StringOfChar(' ',fGap);
 fStack[l].StepBack := lIndent;
 fStack[l].ItemsWritten := false;
 // TODO: Not sure if I need this...
 fStack[l].IsMap := aIsMap;
end;

constructor TJSONWriter.Create(aStream: TStream; aIndent: Word);
begin
  inherited Create;
  fStream := aStream;
  SetLength(fStack,0);

  // In EcmaScript specs for JSON.stringify, the longest indent is 10 characters.
  // I'm doing a sanity filter by also making sure the string is only
  // spaces, and not something else.
  fGap := math.Min(aIndent,10);

end;

destructor TJSONWriter.Destroy;
begin
  fStack := nil;
  inherited Destroy;
end;

procedure TJSONWriter.WriteBoolean(aValue: Boolean);
begin
 Write(BoolToStr(aValue,TrueText,FalseText));
end;


procedure TJSONWriter.WriteListStart;
begin
 WriteValueHeader;
 Push(false);
 Write('[');
end;

procedure TJSONWriter.WriteListEnd;
begin
 if (fGap > 0) and Peek.ItemsWritten then
    Write(#10 + Peek.StepBack);
 Write(']');
 Pop;

end;

procedure TJSONWriter.WriteListSeparator;
begin
  Write(',');
end;

procedure TJSONWriter.WriteMapStart;
begin
 WriteValueHeader;
 Push(true);

 Write('{');
end;

procedure TJSONWriter.WriteMapEnd;
begin
 if (fGap > 0) and Peek.ItemsWritten then
   Write(#10 + Peek.StepBack);
 Write('}');
 Pop;

end;

procedure TJSONWriter.WriteMapKey(aValue: UTF8String);
begin
   if fGap > 0 then
   begin
     Write(#10 + Peek.Indent)
   end;
   WriteQuote(aValue);
   if fGap > 0 then
      Write(': ')
   else
      Write(':');
end;

procedure TJSONWriter.WriteNull;
begin
  WriteValueHeader;
  Write(NullText);
end;

procedure TJSONWriter.WriteNumber(aValue: Double);
begin
  WriteValueHeader;
  Write(FloatToStr(aValue));

end;

procedure TJSONWriter.WriteString(aValue: UTF8String);
begin
  WriteValueHeader;
  WriteQuote(aValue);

end;

end.

