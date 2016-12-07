unit brothmark_sys_parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{
Other SynEdit highlighters keep this all in the highlighter class. But,
I want to make sure the parser is useful for other tools outside of SynEdit
as well. For example, if I ever need to write a converter.
}

{
TODO: We have to think of this as a tokenizer, looking for specific tokens,
rather than a parser that creates structure. Thus, we'll see tokens like
left-flanking emphasis run, not a string of bold text. We will need to keep some
state in the range however, because the tokens can have different meanings
in different states.

TODO: I still don't know if I can get the nesting levels from the folded ranges.
One way to easily know the nesting levels. Keep track of the range as
a dynamic array of block types, showing the current scope. These are stored
in a sorted list themselves, so we can re-use ones that are the same, and the
index into this table is what is passed around as the range value.

}

type
  {
  TODO: This needs to be finished in order for it to work. However, the brick
  wall I hit with styles in the highlighter seems to make this useless.
  }

  TBrothMarkTokenKind = (
    bmtkUnparsed,
    bmtkEndOfLine,
  // these are actual tokens, or blocks which consist entirely of one token.
    bmtkThematicBreak, // can combine with formatted annotation
    bmtkHeading1OpenMarker, // can combine with heading, formatted annotation
    bmtkHeading2OpenMarker, // can combine with heading, formatted annotation
    bmtkHeading3OpenMarker, // can combine with heading, formatted annotation
    bmtkHeading4OpenMarker, // can combine with heading, formatted annotation
    bmtkHeading5OpenMarker, // can combine with heading, formatted annotation
    bmtkHeading6OpenMarker, // can combine with heading, formatted annotation
    bmtkHeadingCloseMarker, // can combine with heading, formatted annotation
    bmtkCodeBlockMarker, // can combine with code block, formatted annotation
    bmtkCodeBlockEscapedBacktickMarker, // can combine with code block, formatted annotation
    bmtkCodeBlockInfoString, // can combine with code block, formatted annotation
    bmtkLinkReferenceDelimiter, // can combine with link reference definition, formatted annotation
    bmtkBlankLine, // can combine with formatted annotation
    bmtkBlockQuoteMarker, // can combine with formatted annotation
    bmtkBulletListMarker, // can combine with formatted annotation
    bmtkOrderedListMarker, // can combine with formatted annotation
    bmtkFormattedAnnotationOpenMarker, // can combine with formatted annotation
    bmtkMetadataBlockOpenMarker,
    bmtkAnnotationBlockOpenMarker,
    bmtkAnnotationInlineOpenMarker,
    bmtkAnnotationCloseMarker, // can combine with formatted annotation
    bmtkEscapedCharacter, // can combine with heading, formatted annotation, inline styles
    bmtkCharacterReference, // can combine with heading, formatted annotation, inline styles
    bmtkCodeSpanMarker, // can combine with heading, formatted annotation, inline styles
    bmtkCodeSpanEscapedBacktickMarker, // can combine with heading, formatted annotation, inline styles
    bmtkLeftFlankingBoldSpanMarker, // can combine with heading, formatted annotation, inline styles
    bmtkRightFlankingBoldSpanMarker, // can combine with heading, formatted annotation, inline styles
    bmtkLeftFlankingItalicSpanMarker, // can combine with heading, formatted annotation, inline styles
    bmtkRightFlankingItalicSpanMarker, // can combine with heading, formatted annotation, inline styles
    bmtkLeftFlankingUnderlinedSpanMarker, // can combine with heading, formatted annotation, inline styles
    bmtkRightFlankingUnderlinedSpanMarker, // can combine with heading, formatted annotation, inline styles
    bmtkLinkOpenMarker, // can combine with heading, formatted annotation, inline styles
    bmtkLinkCloseMarker, // can combine with heading, formatted annotation, inline styles
    bmtkLinkLabel, // can combine with heading, formatted annotation, link reference defintion, inline styles
    bmtkCollapsedLinkLabel, // can combine with heading, formatted annotation, inline styles
    bmtkLinkDestination, // can combine with heading, formatted annotation, link reference defition, inline styles
    bmtkInternalLinkAnchor, // can combine with heading, formatted annotation, inline styles
    bmtkImageDescriptionOpenMarker, // can combine with heading, formatted annotation, inline styles
    bmtkShortcutImageLabel, // can combine with heading, formatted annotation, inline styles
    bmtkHardLineBreakMarker, // can combine with heading, formatted annotation, inline styles
    bmtkTextualContent, // can combine with heading, formatted annotation, inline styles
    bmtkBMNFKey,
    bmtkBMNFUnkeyedItemMarker,
    bmtkBMNFListContinuationIndent,
    bmtkBMNFCharacterReference,
    bmtkBMNFEscapedCharacter,
    bmtkBMNFHardLineBreakMarker,
    bmtkBMNFEscapedWhitespace,
    bmtkBMNFTextualContent,

    bmtkAnnotation // never mixes, this is always the same
  );

  TBrothMarkContext = (
    bmrsAnnotation,
    bmrsThematicBreak,
    bmrsHeading1,
    bmrsHeading2,
    bmrsHeading3,
    bmrsHeading4,
    bmrsHeading5,
    bmrsHeading6,
    bmrsCodeBlock,
    bmrsFormattedAnotation,
    bmrsMetadataBlock, // TODO: Don't forget the children of this one.
    bmrsLinkReferenceDefinition,
    bmrsParagraph,
    bmrsBlockQuote,
    bmrsHyphenList,
    bmrsHyphenListItem,
    bmrsPlusList,
    bmrsPlusListItem,
    bmrsAsteriskList,
    bmrsAsteriskListItem,
    bmrsDotOrderedList,
    bmrsDotOrderedList3Item,
    bmrsDotOrderedList4Item,
    bmrsDotOrderedList5Item,
    bmrsDotOrderedList6Item,
    bmrsDotOrderedList7Item,
    bmrsDotOrderedList8Item,
    bmrsDotOrderedList9Item,
    bmrsDotOrderedList10Item,
    bmrsDotOrderedList11Item,
    bmrsDotOrderedList12Item,
    bmrsDotOrderedList13Item,
    bmrsDotOrderedList14Item,
    bmrsParensOrderedList,
    bmrsParensOrderedList3Item,
    bmrsParensOrderedList4Item,
    bmrsParensOrderedList5Item,
    bmrsParensOrderedList6Item,
    bmrsParensOrderedList7Item,
    bmrsParensOrderedList8Item,
    bmrsParensOrderedList9Item,
    bmrsParensOrderedList10Item,
    bmrsParensOrderedList11Item,
    bmrsParensOrderedList12Item,
    bmrsParensOrderedList13Item,
    bmrsParensOrderedList14Item,
    bmrsInlineText, // represents general paragraph text that hasn't been styled yet.
    bmrsCodeSpan,
    bmrsBoldSpan,
    bmrsItalicSpan,
    bmrsUnderlinedSpan,
    bmrsLinkText,
    bmrsImageDescription,
    bmrsBMNFStructureLiteral,
    bmrsBMNFStringLiteral,
    bmrsBMNFUnkeyedItemLiteral,
    bmrsBMNFKeyedItemLiteral
  );

  TBrothMarkContexts = set of TBrothMarkContext;

  TBrothMarkContextStack = array of TBrothMarkContext;

  TContextChangeMethod = procedure(Sender: TObject; const aContext: TBrothMarkContext) of object;

  { TBrothMarkContextTable }

  TBrothMarkContextTable = class
  private
    class var fTable: array of TBrothMarkContextStack;
  protected
    class constructor CreateClass;
    class destructor DestroyClass;
  public
    class function GetIndex(const aContext: TBrothMarkContextStack): Longint;
    class function GetContext(const aIndex: Longint): TBrothMarkContextStack;
  end;

  { TBrothMarkScanner }

  TBrothMarkScanner = class
  private
    fContext: TBrothMarkContextStack;
    fContextPos: Longint;
    fLine: PChar;
    fPos: LongInt;
    fTokenPos: Integer;
    FTokenID: TBrothMarkTokenKind;
  private

    // context management
    procedure EndContext;
    procedure StartContext(const aContext: TBrothMarkContext);


    // special tokens that might be anywhere.
    function ScanBlankLineToken: Boolean;

    procedure ScanParagraphContinuation;

    procedure ScanContinuationToken;

    procedure ScanInlineTextToken;
    procedure ScanParagraphToken;
    procedure ScanDocumentToken;

    procedure ScanContextToken;

  private
    FOnContextEnded: TContextChangeMethod;
    FOnContextStarted: TContextChangeMethod;
    procedure SetContext(AValue: TBrothMarkContextStack);
  public
    constructor Create;
    property Context: TBrothMarkContextStack read fContext write SetContext;
    procedure ClearContext;
    procedure ParseLine(const NewValue: String);
    procedure Next;
    function InContext(const aContext: TBrothMarkContext): Boolean;
    function GetTokenString: String;
    procedure GetTokenPChar(out Token: PChar; out Length: integer);
    property TokenPos: Longint read fTokenPos;
    property TokenID: TBrothMarkTokenKind read FTokenID;
    property OnContextStarted: TContextChangeMethod read FOnContextStarted write FOnContextStarted;
    property OnContextEnded: TContextChangeMethod read FOnContextEnded write FOnContextEnded;
  end;


implementation

uses
  Math, LazUTF8;

{ TBrothMarkContextTable }

class constructor TBrothMarkContextTable.CreateClass;
begin
  SetLength(fTable,0);
  // TODO: I should fill the table up with some basic
  // and simple contexts, that way we don't run the risk
  // of throwing a bunch of complicated contexts in that
  // will be harder to see.
end;

class destructor TBrothMarkContextTable.DestroyClass;
begin
  SetLength(fTable,0);

end;

class function TBrothMarkContextTable.GetIndex(
  const aContext: TBrothMarkContextStack): Longint;
var
  lTableLen: Longint;
  lContextLen: Longint;
  i: Longint;
  j: Longint;
begin
  result := -1;
  lTableLen := Length(fTable);
  lContextLen:= Length(aContext);
  if lContextLen > 0 then
  begin
    for i := 0 to lTableLen - 1 do
    begin
      if Length(fTable[i]) = lContextLen then
      begin
        // set result now so we can use it as a 'found' marker.
        result := i;

        for j := 0 to lContextLen - 1 do
        begin
          if fTable[i][j] <> aContext[j] then
          begin
            // unset result to indicate that it was definitely not found.
            result := -1;
            break;
          end;
        end;

        if result <> -1 then
           break;

      end;
    end;

    if Result = -1 then
    begin
      // add a new one.
      SetLength(fTable,lTableLen + 1);
      fTable[lTableLen] := aContext;
      result := lTableLen;
    end;

  end;
  // else the context is empty, and will be returned with a -1 anyway.

end;

class function TBrothMarkContextTable.GetContext(const aIndex: Longint
  ): TBrothMarkContextStack;
begin
  if (aIndex >= 0) and (aIndex < Length(fTable)) then
  begin
    result := fTable[aIndex];
  end
  else
    SetLength(result,0);
end;

procedure TBrothMarkScanner.SetContext(AValue: TBrothMarkContextStack);
begin
  fContext:=AValue;
  fContextPos := 0;
end;


procedure TBrothMarkScanner.EndContext;
var
  i: Longint;
begin
  if Assigned(FOnContextEnded) then
  begin
    for i := Length(fContext) - 1 downto fContextPos do
    begin
      FOnContextEnded(Self,fContext[i]);
    end;
  end;
  // TODO: For each of the events at fContextPos and above, call an event
  // indicating that it's closing.
  SetLength(fContext,fContextPos);
end;

procedure TBrothMarkScanner.StartContext(const aContext: TBrothMarkContext
  );
var
  lContextLen: LongInt;
begin
  lContextLen := Length(fContext);
  SetLength(fContext,lContextLen + 1);
  fContext[lContextLen] := aContext;
  fContextPos := lContextLen + 1;
  if Assigned(FOnContextStarted) then
    FOnContextStarted(Self,aContext);

end;

function TBrothMarkScanner.ScanBlankLineToken: Boolean;
var
  lPos: Longint;
begin
  lPos := fPos;
  while fLine[lPos] in [#9,#32] do
  begin
      inc(lPos);
  end;
  result := fLine[lPos] = #0;
  if result then
  begin
     FTokenID := bmtkBlankLine;
     fPos := lPos;
  end;
end;

procedure TBrothMarkScanner.ScanParagraphContinuation;
begin
  // TODO: Paragraph ends when:
//* a [heading], [thematic break], [code block], [blank line], [block quote],
//  [bullet list], [ordered list], [formatted annotation], or
//  [metadata block] appears on a potential continuation line.
//* a [annotation close marker] appears (ensures that the annotation close
//  marker is not treated as textual content in a formatted annotation, since
//  it's the only structure that's not inline and can end after the line begins).
//* a containing block ends.
//* there are no more lines in the document.

  // whitespace is acceptable for the text of a paragraph...
  case fLine[fPos] of
    #0,#9,#32:
      // could be a blank line, but we don't know until we get to the end.
    begin
      if ScanBlankLineToken then
      begin
        // end the paragraph context. Blank line doesn't have a context.
        EndContext;
        Exit;
      end;
      // else we might have skipped some text, but it's not
      // a blank line, so the paragraph will continue...
    end
  else
    // anything else is definitely a paragraph. Don't scan it, though,
    // because we might have child contexts.
  end;
  inc(fContextPos);



end;

procedure TBrothMarkScanner.ScanContinuationToken;
begin
  // TODO: Depending on the current context
  case fContext[fContextPos] of
    bmrsParagraph:
      ScanParagraphContinuation;
    // TODO: Work out all of these...
  else
    // no procedure, so assume that it just continues (such as for
    // inline contexts).
    inc(fContextPos);
  end;
end;

procedure TBrothMarkScanner.ScanInlineTextToken;
begin
  while fLine[fPos] <> #0 do
    inc(fPos);
  FTokenID := bmtkTextualContent;
end;

procedure TBrothMarkScanner.ScanParagraphToken;
begin
  // means we're on a new paragraph if we get here...
  StartContext(bmrsInlineText);
  ScanInlineTextToken;
end;

procedure TBrothMarkScanner.ScanDocumentToken;
begin
  // TODO: Right now, anything can be a paragraph token
  StartContext(bmrsParagraph);
  ScanParagraphToken;
end;

procedure TBrothMarkScanner.ScanContextToken;
var
  lContextLen: Longint;
  lContext: TBrothMarkContext;
begin
  lContextLen := Length(fContext);
  if lContextLen = 0 then
  begin
    ScanDocumentToken;
  end
  else
  begin
    lContext := fContext[lContextLen - 1];
    case lContext of
      bmrsParagraph:
        ScanParagraphToken;
      bmrsInlineText:
        ScanInlineTextToken;
    end;
  end;

end;

constructor TBrothMarkScanner.Create;
begin
  inherited Create;
  ClearContext;
end;

procedure TBrothMarkScanner.ClearContext;
begin
  SetLength(fContext,0);
  fContextPos := 0;
end;

procedure TBrothMarkScanner.ParseLine(const NewValue: String);
begin
  fLine := PChar(NewValue);
  // the highlighter only seems to call SetRange if it doesn't
  // think the highlighter already has the right range, so we
  // have to make sure the context position is set here as well.
  fContextPos := 0;
  fPos := 0;
  Next;
end;

procedure TBrothMarkScanner.Next;
begin
  fTokenPos := fPos;
  // TODO: To catch the ones that are unparsed.
  FTokenID := bmtkUnparsed;


  // look for continuation tokens (that could change the context as well)
  // (We have to keep checking the length of the context directly, instead
  // of storing it in a local variable, because the scan could change it).
  while (fContextPos < Length(fContext)) and
        (fPos = fTokenPos) do
  begin
    ScanContinuationToken;
  end;

  // we've been through the continuation tokens, so now
  // let's look for tokens that are valid for the current context.
  // This includes tokens that initiate new contexts at this level.

  if fPos = fTokenPos then
  begin
    if fLine[fPos] = #0 then
    begin
      FTokenID := bmtkEndOfLine;
      Exit;
    end;
    // continuation tokens could not be found, or we're past that, so
    // scan the current context.
    ScanContextToken;
  end;

  // TODO: While we're debugging, anything that doesn't lead to a
  // token gets collected here.
  if fPos = fTokenPos then
  begin
    // increment by 1, so we don't keep going.
    inc(fPos);
    FTokenID := bmtkUnparsed;
  end;

end;

function TBrothMarkScanner.InContext(const aContext: TBrothMarkContext
  ): Boolean;
var
  i: Longint;
  lContextHi: Longint;
begin
  lContextHi := Min(fContextPos,Length(fContext) - 1);
  for i := 0 to lContextHi do
     if fContext[i] = aContext then
     begin
       result := true;
       exit;
     end;
  result := false;

end;

function TBrothMarkScanner.GetTokenString: String;
var
  Len: LongInt;
begin
  Result := '';
  Len := fPos - fTokenPos;
  SetString(Result, (fLine + fTokenPos), Len);
end;

procedure TBrothMarkScanner.GetTokenPChar(out Token: PChar; out Length: integer
  );
begin
  Length:= fPos - fTokenPos;
  Token:= fLine + fTokenPos;

end;



end.

