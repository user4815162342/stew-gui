unit brothmark_gui_highlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics,
  SynEditHighlighter, SynEditHighlighterFoldBase, brothmark_sys_parser;

type

  // TODO: I've hit a brick wall. As can seen in getting the attributes,
  // in order to define the attributes, and have bold text also be italic
  // in a heading and a formatted annotation, etc. I'm going to need to
  // create hundreds, possibly thousands of 'attribute' objects for each
  // one. I see no way of getting around this (short of overriding
  // some sort of 'onpaint' even in the editor object itself). I would have
  // to talk to the people who designed the component to see if they have
  // any ideas, but at this point, I think broth might not be worth it,
  // if I can get an actual code IDE to work instead.

  { TBrothMarkHighlighter }

  TBrothMarkHighlighter = class(TSynCustomFoldHighlighter)
    procedure ParserContextEnded(Sender: TObject;
      const {%H-}aContext: TBrothMarkContext);
    procedure ParserContextStarted(Sender: TObject;
      const aContext: TBrothMarkContext);
  private
    fLineNumber: Longint;
    fParser: TBrothMarkScanner;
  private
    // TODO: Need to develop these out...
    fBrokenParserAttri: TSynHighlighterAttributes;
    FUnknownAttri: TSynHighlighterAttributes;
  protected
    function GetSampleSource : String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function  GetTokenKind: integer; override;
    function  GetTokenPos: Integer; override;
    function  GetTokenID: TBrothMarkTokenKind;

    procedure Next; override;
    function GetEol: Boolean; override;

    procedure SetLine(const NewValue: String;
      LineNumber: Integer); override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;

    function GetDefaultAttribute({%H-}Index: integer): TSynHighlighterAttributes; override;
    class function GetLanguageName: string; override;
  protected
    // folding

  end;

implementation

uses
  SysUtils, SynEditStrConst, SynEditTypes;


{ TBrothMarkHighlighter }

procedure TBrothMarkHighlighter.ParserContextEnded(Sender: TObject;
  const aContext: TBrothMarkContext);
begin
  if not (aContext in [bmrsInlineText,
                       bmrsCodeSpan,
                       bmrsBoldSpan,
                       bmrsItalicSpan,
                       bmrsUnderlinedSpan,
                       bmrsLinkText,
                       bmrsImageDescription]) then
    EndCodeFoldBlock(true);
end;

procedure TBrothMarkHighlighter.ParserContextStarted(Sender: TObject;
  const aContext: TBrothMarkContext);
begin
  if not (aContext in [bmrsInlineText,
                       bmrsCodeSpan,
                       bmrsBoldSpan,
                       bmrsItalicSpan,
                       bmrsUnderlinedSpan,
                       bmrsLinkText,
                       bmrsImageDescription]) then
    StartCodeFoldBlock({%H-}Pointer(PtrInt(aContext)), true);
end;

function TBrothMarkHighlighter.GetSampleSource: string;
begin
  Result := ''#13#10 +
            '';
end;

constructor TBrothMarkHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fBrokenParserAttri := TSynHighlighterAttributes.Create;
  fBrokenParserAttri.Background:= clRed;
  AddAttribute(fBrokenParserAttri);

  FUnknownAttri := TSynHighlighterAttributes.Create(@SYNS_AttrUnknownWord, SYNS_XML_AttrUnknownWord);
  FUnknownAttri.Style := [fsItalic];
  // TODO: The following is done for our debugging purposes, until
  // we can have different styles for each token.
  FUnknownAttri.FrameColor:=clBlack;
  FUnknownAttri.FrameStyle := slsSolid;
  FUnknownAttri.FrameEdges := sfeAround;
  AddAttribute(FUnknownAttri);

  SetAttributesOnChange(@DefHighlightChange);
  fDefaultFilter := '';

  fParser := TBrothMarkScanner.Create;
  fParser.OnContextStarted:=@ParserContextStarted;
  fParser.OnContextEnded:=@ParserContextEnded;
end;

destructor TBrothMarkHighlighter.Destroy;
begin
  FreeAndNil(fParser);
  inherited Destroy;
end;

function TBrothMarkHighlighter.GetToken: String;
begin
  result := fParser.GetTokenString;
end;

procedure TBrothMarkHighlighter.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  fParser.GetTokenPChar(TokenStart,TokenLength);
end;

function TBrothMarkHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := fBrokenParserAttri;
  // TODO: Styles change based on whether certain ranges are in effect.
  // I either need to define a heck of a lot of styles (I'm talking
  // hundreds of styles) or figure out how to specify combinations
  // of attributes.
  case fParser.TokenID of
    // cmtkUnparsed: this is just an error in parsing.
    //cmtkEndOfLine: end of line isn't "painted";

    // generic block markers, only combine with formatted annotation
    bmtkThematicBreak,
    bmtkCodeBlockMarker,
    bmtkBlockQuoteMarker,
    bmtkBulletListMarker,
    bmtkOrderedListMarker:
      // TODO: Can combine with formatted annotations
    ;

    // these block markers combine with formatted annotation,
    // but they might be styled differently because they are headings.
    bmtkHeading1OpenMarker,
    bmtkHeading2OpenMarker,
    bmtkHeading3OpenMarker,
    bmtkHeading4OpenMarker,
    bmtkHeading5OpenMarker,
    bmtkHeading6OpenMarker,
    bmtkHeadingCloseMarker:;
      // TODO: Can combine with formatted annotations

    // code block info (can be styled differently from operator)
    bmtkCodeBlockInfoString:
      // TODO: Can combine with formatted annotations
    ;

    // might need to be styled differently than above.
    bmtkLinkReferenceDelimiter:
      // TODO: Can combine with formatted annotations
    ;

    // span markers, these can combine with all sorts of inline
    // stuff
    bmtkCodeSpanMarker,
    bmtkCodeSpanEscapedBacktickMarker:
       // TODO: combines with
       // - formatted annotation
       // - heading
       // - bold, italic, underlined, link text, image description text

       ;
    bmtkLeftFlankingBoldSpanMarker,
    bmtkRightFlankingBoldSpanMarker:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - code, italic, underlined, link text, image description text
       ;
    bmtkLeftFlankingItalicSpanMarker,
    bmtkRightFlankingItalicSpanMarker:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - code, bold, underlined, link text, image description text
       ;
    bmtkLeftFlankingUnderlinedSpanMarker,
    bmtkRightFlankingUnderlinedSpanMarker:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - code, bold, italic, link text, image description text
       ;
    bmtkLinkOpenMarker:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - code, bold, italic, underlined
       ;
    bmtkLinkCloseMarker,
    bmtkCollapsedLinkLabel,
    bmtkInternalLinkAnchor:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - code, bold, italic, underlined

    // also can look different depending on whether it's in a link or
    // an image description.
    ;
    bmtkImageDescriptionOpenMarker,
    bmtkShortcutImageLabel:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - code, bold, italic, underlined, link text
    ;

    // text content
    bmtkTextualContent:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - code, bold, italic, underlined, link text, image description text
    ;

    // blank line
    bmtkBlankLine:
    // TODO: combines with
    // - formatted annotation
    ;

    // escaped text
    bmtkHardLineBreakMarker,
    bmtkEscapedCharacter,
    bmtkCharacterReference,
    bmtkCodeBlockEscapedBacktickMarker:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - code, bold, italic, underlined, link text, image description text
    ;

    // Link labels and destinations
    // (these may have different attributes depending on whether they
    // are inline or in a link reference description)
    bmtkLinkLabel:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - link reference definition
    // - code, bold, italic, underlined
    ;


    bmtkLinkDestination:
    // TODO: combines with
    // - formatted annotation
    // - heading
    // - link reference definition
    // - code, bold, italic, underlined
    ;

    // BMNF Operators
    bmtkBMNFKey,
    bmtkBMNFUnkeyedItemMarker,
    bmtkBMNFListContinuationIndent:
    // TODO: can combine with formatted annotation
    ;

    // BMNF Text content
    bmtkBMNFTextualContent:
    // TODO: can combine with formatted annotation
    ;

    // BMNF Escaped Text
    bmtkBMNFCharacterReference,
    bmtkBMNFEscapedCharacter,
    bmtkBMNFHardLineBreakMarker,
    bmtkBMNFEscapedWhitespace:
    // TODO: can combine with formatted annotation
    ;

    // Formatted annotation stuff...
    bmtkFormattedAnnotationOpenMarker:
    ;
    // Metadata block
    bmtkMetadataBlockOpenMarker:
    ;

    // Annotation markers
    bmtkAnnotationBlockOpenMarker,
    bmtkAnnotationInlineOpenMarker:
    ;

    // Annotation close (can look different depending on what
    // is being closed.
    bmtkAnnotationCloseMarker:
    // TODO: can be different depending on annotation, formatted annotation and
    // metadata block
    ;

    // general annotation text
    bmtkAnnotation:
    ;

    else Result := FUnknownAttri;
  end;
end;

function TBrothMarkHighlighter.GetTokenKind: integer;
begin
  Result := Ord(fParser.TokenID);
end;

function TBrothMarkHighlighter.GetTokenPos: Integer;
begin
  Result := fParser.TokenPos;
end;

function TBrothMarkHighlighter.GetTokenID: TBrothMarkTokenKind;
begin
  Result := fParser.TokenID;
end;

procedure TBrothMarkHighlighter.Next;
begin
  fParser.Next;
  // TODO: The starting and ending of fold blocks needs to be
  // done based on context change events from the parser which haven't been
  // created yet.

{  if not fParser.IsEOL then
  begin
    if ((fParser.Range in [rsCtxFileOrig, rsCtxFileNew, rsUniFileOrig..rsUniFileNew]) and
        not (OldRange in [rsCtxFileOrig, rsCtxFileNew, rsUniFileOrig..rsUniFileNew]))
    then begin
      while (TopDiffCodeFoldBlockType in [cfbtDiffChunkSect, cfbtDiffChunk, cfbtDiffFile]) do
        EndDiffCodeFoldBlock;
      StartDiffCodeFoldBlock(cfbtDiffFile);
    end;

    if (fParser.Range in [rsCtxChunkHeader, rsUniChunkHeader])
    then begin
      while (TopDiffCodeFoldBlockType in [cfbtDiffChunkSect, cfbtDiffChunk]) do
        EndDiffCodeFoldBlock;
      StartDiffCodeFoldBlock(cfbtDiffChunk);
    end;

    if (fParser.Range in [rsCtxChunkOrig, rsCtxChunkNew])
    then begin
      if (TopDiffCodeFoldBlockType = cfbtDiffChunkSect) then
        EndDiffCodeFoldBlock;
      StartDiffCodeFoldBlock(cfbtDiffChunkSect);
    end;

  end;}

end;

function TBrothMarkHighlighter.GetEol: Boolean;
begin
  Result := fParser.TokenID = bmtkEndOfLine;
end;

procedure TBrothMarkHighlighter.SetLine(const NewValue: String;
  LineNumber: Integer);
begin
  inherited;
  fLineNumber := LineNumber;
  fParser.ParseLine(NewValue);
end;

function TBrothMarkHighlighter.GetRange: Pointer;
begin
  CodeFoldRange.RangeType:={%H-}Pointer(PtrUInt(TBrothMarkContextTable.GetIndex(fParser.Context)));
  Result := inherited;
end;

procedure TBrothMarkHighlighter.SetRange(Value: Pointer);
begin
  inherited;
  fParser.Context := TBrothMarkContextTable.GetContext(Integer({%H-}PtrUInt(CodeFoldRange.RangeType)));
end;

procedure TBrothMarkHighlighter.ResetRange;
begin
  inherited;
  fParser.ClearContext;
end;

function TBrothMarkHighlighter.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  Result := nil;
end;

class function TBrothMarkHighlighter.GetLanguageName: string;
begin
  Result := SYNS_LangDiff;
end;

initialization
  RegisterPlaceableHighlighter(TBrothMarkHighlighter);
end.

