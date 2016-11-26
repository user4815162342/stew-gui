unit cm_gui_highlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SynEditHighlighter, SynEditHighlighterFoldBase;

type

  { TCommonMarkHighlighter }

  TCommonMarkHighlighter = class(TSynCustomFoldHighlighter)

  protected
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
  public
    function GetEol: Boolean; override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
      override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  end;

implementation

uses
  SynHighlighterDiff; // diff highlighter is a simple, folded highlighter, which I can use for assistance in creating this one.

{ TCommonMarkHighlighter }

function TCommonMarkHighlighter.GetDefaultAttribute(Index: integer
  ): TSynHighlighterAttributes;
begin
    hereiam;
end;

function TCommonMarkHighlighter.GetEol: Boolean;
begin
    hereiam;

end;

function TCommonMarkHighlighter.GetToken: String;
begin
    hereiam;

end;

function TCommonMarkHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
begin
    hereiam;

end;

procedure TCommonMarkHighlighter.GetTokenEx(out TokenStart: PChar; out
  TokenLength: integer);
begin
    hereiam;

end;

function TCommonMarkHighlighter.GetTokenKind: integer;
begin
    hereiam;

end;

function TCommonMarkHighlighter.GetTokenPos: Integer;
begin
    hereiam;

end;

procedure TCommonMarkHighlighter.Next;
begin
    hereiam;

end;

end.

