<!--:
title: BrothMark Specification
author: Neil M. Sheldon
version: 0.1
date: '2016-11-18'
license: '[CC-BY-SA 4.0](http://creativecommons.org/licenses/by-sa/4.0/)'
-->

# Introduction

BrothMark is a format for writing structured content in plain text. It is
designed to make use of popular conventions which have developed over the
decades that plain text has been used to store such content on computers. It is
intended to be simple and intuitive to use.

Most of the ideas for BrothMark come from dialects of Markdown, especially the
CommonMark specification. However, the goals of those languages, as plain text
formats intended to produce HTML with a human-readable structure, differ
slightly from the goals of BrothMark.

The goals of the BrothMark format:

* Be written in plain text, so no special programs are required to use it.
* Have little clutter which would make it look like a programming language, so
  a user wouldn't get too confused when reading it.
* Indicate the structure and format of a manuscript or similar content-based
  document.
* Be compatible with a version control system.
* Be easy to parse.
* Be able to parse in one pass (except for link reference definitions).
* Be able to parse a random line from a document given only minimal context
  information (see [Parser] below)
* Make use of conventions which have developed over the decades that people have
  been trying to write using plain text.
* Make use of the simpler and more intuitive such conventions where possible.
* Be consistent in the results of syntax.
* Avoid multiple ways to achieve the same concept, so a reader won't be confused
  by a context written in the same way.
* Not be able to create a full HTML document: those should be created straight
  in HTML.

## About this document

This document is written in the BrothMark format.

This document is written in a way that describes a parser which takes in text
from a BrothMark document and creates styled and structured content. The style
and structure are often described in terms of HTML, and in fact, the examples
show the HTML produced by such a parser. However, HTML is not necessarily the
expected output of the markup language.

Some organization of the document, some of the text, and most of the examples
(and the structure of the examples) are taken directly from a version of the
CommonMark specification version 0.27. The "Creative License" assigned to the
CommonMark specification document allows me to "remix, transform and build upon
the material".

## Where's the Grammar?

I do not believe that it is possible to create a context-free grammar for
any dialect of Markdown, without transforming the dialect to a position where
it no longer looks like Markdown. Others have suggested that this is due to
the nature of how emphasis is marked, although I believe I have solved that
in this dialect. The real culprit is the way blocks can be nested inside list
items, since embedding another block inside a list item involves injecting
text into the middle of the block (i.e. the indentation at the beginning of
each line).

For that reason, BrothMark is not defined in BNF notation, or anything like
that, but rather, as a set of algorithms for a parser that can parse BrothMark
and produce formatted text.

# Parser

The BrothMark [parser]<@> is designed to parse single lines of a BrothMark
document at a time. These lines may not be received to the parser in order,
and only a minimal amount of context is provided at the beginning of each line.
This is done because BrothMark is designed to work with syntax highlighting
in an ever-changing editor, and such a limitation is often necessary in such
an environment.

When the parser is asked to parse a line, it is provided with a [context]<@>.
The context is a pointer to a FILO stack of [context types][context type]. A
[context type]<@> is an enumeration which indicates what type of block, span
or other nested state is in scope at a given point in the document. No other
state information is available to the parser, including such things as
position where the context type started, length of the context type, or metadata
such as list item numbers. Most importantly, the parser has no information at
all about the document after the current line it is working on.

The only information the parser has to work with is the line and this context,
and any state it is able to store during the parsing of the line, and which
must be thrown away when the line is completely parsed. When the parser is done
with the line, the most it can do is return the new current context to the
application, which can then store it for when it is ready to parse the next
line.

The parser divides the line it is provided into [tokens][token] based on the
characters present and the current context. A [token]<@> is a series of
contiguous characters that can only have one meaning or purpose. These tokens
are then used to produce text, new formatting elements, or change the current
context in some way.

More information on how parsing is done can be seen in the reference
implementation.

# Thematic sections

A BrothMark document can be divided into logical, [thematic sections][@] by
certain block elements. These sections can form a hierarchical structure, with
subsections nested inside other sections. This structure can be used to create
an outline of the document, or a tool for quickly navigating between sections.
It can also be used to indicate metadata for parts of the document.

Thematic sections are delimited by the bounds of the document, heading blocks,
and thematic breaks. Each thematic section has a 'level', which indicates where
it is nested in the hierarchy.

Thematic sections are defined and maintained outside of the parser, so they can
depend on context that is not available to the parser, such as the most
recent heading. However, the functionality is still important to how BrothMark
works, so it is part of the specification.

There are four ways these are defined.

1. The beginning of the document defines the start of a section that contains
   all content within the document, and ends with the end of the document. All
   this content is considered to be a 0-level section.

2. An ATX heading *that is not nested inside a container block* defines the
   beginning of a new section that is the same level as the heading that defines
   it. This section ends when an ATX heading of the same level appears or the
   end of the section containing this heading is reached. This section is
   contained within any sections defined by higher level headings, or by the
   document itself.

   Note that, if heading levels are skipped, for example if a level 2 heading
   appears before a level 1 heading in the document, the section is implied to
   exist anyway, it is simply not marked by any headings. However, in an
   outline, these implied sections will not be visible. In this case, the lower
   level heading will appear at the same level as the upper level, although
   possibly being displayed in the style of that lower level.

3. All contiguous content that is not a heading or thematic break, defines a
   section that is contained within the section above. This section is contained
   within the section defined by the heading that defines it, and can not
   contain any further nested sections.

4. A thematic break *that is not nested inside a container block* defines the
   end of a section and the beginning of a new section. The level of that
   section is the same level as the section that preceded it. Thus, the
   contiguous content before the break is at the same level as the content
   after. This section ends with the next thematic break, or when the n
   section that contains it ends.

Note that headings and thematic breaks that are inside a container block like
a list or blockquote, do not delimit thematic sections.

Content is considered [sectioning]<@> if the content can affect the thematic
sections.

# Characters

The encoding of a BrothMark document is is not specified. Instead, it is treated
as a series of Unicode characters. Only encodings that can be translated to
unicode can be used for BrothMark.

Any sequence of characters is a valid BrothMark document.

A [character](@) is a Unicode code point.

A [line](@) is a sequence of zero or more [characters] other than newline
(`U+000A`) or carriage return (`U+000D`), followed by a [line ending] or by the
end of file.

A [line ending](@) is a newline (`U+000A`), a carriage return (`U+000D`) not
followed by a newline, or a carriage return and a following newline. Note
that the parser will never see these, the parser may assume that they exist
at the end of the line.

A [whitespace character]<@> is a space (`U+0020`) or tab (`U+0009`). Note that
I do not count such characters as line tabulation (`U+000B`) or form feed
(`U+000C`) as whitespace, as their use in a plain-text format is discouraged.
They may be considered unicode whitespace, however. Also note that the newline
and carriage return should never be seen by the parser, since either of them
alone can break up a line.

[Whitespace]<@> is a sequence of one or more [whitespace characters].

A [Unicode whitespace character]<@> is any code point in the Unicode `Zs` class,
or a tab (`U+0009`), carriage return (`U+000D`), newline (`U+000A`), or form
feed (`U+000C`).

[Unicode whitespace]<@> is a sequence of one or more [Unicode whitespace
characters].

A [space]<@> is `U+0020`.

A [digit]<@> is `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, or `0`.

A [non-whitespace character]<@> is any character that is not a [whitespace
character] and also not line tabulation (`U+000B`), form feed
(`U+000C`), newline (`U+000A`), or carriage return (`U+000D`).

An [ASCII punctuation character]<@> is `!`, `"`, `#`, `$`, `%`, `&`, `'`, `(`,
`)`, `*`, `+`, `,`, `-`, `.`, `/`, `:`, `;`, `<`, `=`, `>`, `?`, `@`,
`[`, `\`, `]`, `^`, `_`, `` ` ``, `{`, `|`, `}`, or `~`.

A [punctuation character]<@> is an [ASCII punctuation character] or anything in
the Unicode classes `Pc`, `Pd`, `Pe`, `Pf`, `Pi`, `Po`, or `Ps`.

## Insecure characters

For safety, the null unicode character (`U+0000`) is always treated as the
Unicode Replacement Character (`U+FFFD`).

# Tokens

This section contains a list of [tokens][token] that may be found during the
parsing. Tokens are based on the series of characters they match as well as
the current context. Just because a series of characters matches a token doesn't
mean that those characters mean that token. They will not match if that is not
a valid token for the current context.

These tokens are described in a dialect of BNF, where text itself is enclosed
in double quotes. Most of the operators are probably understood. `!` means that
the following expression must not be found at that position in order for the
entire expression to match. An expression in angle-brackets
indicates a character set. A number range in curly brackets indicates a minimum
to maximum count.

When a [whitespace character] is referenced in the descriptions, it expects
no [line ending] characters are included. [Line ending] characters will be
explicitly stated, only if the expression must extend to the end of the line.

* `"---" "-"* <[whitespace character]>* ! <[non-whitespace character]>
  <[line ending]>`:
  [thematic break marker]<@>
* `"#"{1-6} (" " | <[line ending]>)`: [heading open marker]<@>
* `" " "#"* " "* <[line ending]>`: [heading close marker]<@>
* ` "````" "``"* `: [code block marker]<@>
* ` "\````" "``"* `: [code block escaped backtick marker]<@>
* ` <[whitespace character]>* (<[non-whitespace character]> ! `` ` ``)+
  <[whitespace character]>* <[line ending]>`:
  [code block info string]<@>
* `":"`: [link reference delimiter]<@>
* `<[whitespace character]>* ! <[non-whitespace character]> <[line ending]>`:
  [blank line token]<@>
* `">" (" " | [line ending])`: [block quote marker]<@>
* `("-" | "+" | "*") (" " | [line ending])`: [bullet list marker]<@>
* ` "  " `: [bullet list indent]<@>
* `<[digit]>{1-9} ("." | ")") (" "{1-4}` | [line ending]):
  [ordered list marker]<@>
* ` " "{3-14} `: [ordered list indent]<@>
* `"<!--~" " "* <[line ending]>`: [formatted annotation open marker]<@>
* `"<!--:" " "* <[line ending]>`: [metadata block open marker]<@>
* `"<!--" ! (">" | "->" | "~" | ":")`: [annotation block open marker]<@>
* `"<!--" ! (">" | "->")`: [inline annotation open marker]<@>
* `"-->"`: [annotation close marker]<@>
* `"\" <[ASCII punctuation character]>`: [escaped character token]<@>
* `"&" <[non-whitespace character]>+ ";"`: [character reference token]<@>
* ` "``" ! "``" `: [code span marker]<@>
* ` "````" `: [code span escaped backtick sequence]<@>
* ` "*" ! <[unicode whitespace character]>`:
  [left-flanking bold span marker]<@>
* ` "/" ! <[unicode whitespace character]>`:
  [left-flanking italic span marker]<@>
* ` "_" ! <[unicode whitespace character]>`:
  [left-flanking underlined span marker]<@>
* ` ! <[unicode whitespace character]> "*"`:
  [right-flanking bold span marker]<@>
* ` ! <[unicode whitespace character]> "/"`:
  [right-flanking italic span marker]<@>
* ` ! <[unicode whitespace character]> "_"`:
  [right-flanking underlined span marker]<@>
* ` "[" `: [link open marker]<@>
* ` "]" `: [link close marker]<@>
* ` "[" <[whitespace character]>* <[non-whitespace character]>+
  (<[whitespace character]> | <[non-whitespace character]>) "]" `:
  [link label]<@>
* ` "[]" `: [collapsed link label]<@>
* ` "<" !"@" ( (<[non-whitespace character]> - "<" - ">") | "\<" | "\>") ">" `:
    [link destination]<@>
* ` "<@>" `: [internal link anchor]<@>
* ` "![" `: [image description open marker]<@>
* ` "![" <[whitespace character]>* <[non-whitespace character]>+
  (<[whitespace character]> | <[non-whitespace character]>) "]" `:
  [shortcut image label]<@>
* ` "\" " "* <[line ending]> `: [hard line break marker]<@>
* Anything else is considered [textual content]<@>.

# [Annotation]<@>

An annotation is content intended only for certain privileged eyes, especially
the eyes of the documents' author. This is useful for writing in comments,
suggestions, and notes for future changes.

There are also a couple of special cases for annotations, described below,
called the [formatted annotation block] and the [metadata block].

When processing BrothMark into other formats, the annotation and its contents
should be ignored. Although the converter could choose to output them into
an appropriate structure for the output format.

Apart from the special cases mentioned above, annotations are special constructs
outside of the concept of blocks and inlines. When parsing, the context of being
in an annotation actually supercedes all other contexts. This does not mean they
can just exist anywhere. It just means that they may be parsed in block as well
as inline contexts, outside of delimiters and markers.

If parsing is at the beginning of a line where block contexts can appear, an
annotation starts with an [annotation block open marker]. Otherwise, an
annotation can begin with an [inline annotation open marker]. The only
difference is that the first distinguishes it from a [formatted annotation
block] and a [metadata block], which can also appear in the same context.

Within the annotation context, all content is ignored, except other annotations
nested inside it. These are parsed to ensure a match for close markers.

The parsing context does not change within an annotation, so that when the
annotation ends, it can continue where it left off. If text appears on a line
after the [annotation close marker] it is treated as if it were a continuation
of the line that it started with (the information should be available in the
context).

An annotation ends when:
* an [annotation close marker] appears.
* there are no more lines in the document.

An annotation does not effect thematic sectioning.

# [Block Content]<@>

A [block]<@> is a structural element, like paragraphs, block quotations, lists
and headings. Some blocks (like block quotes and list items) can contain other
blocks, others contain [inline content].

If block content is allowed, then the following sections describe the possible
content that could be parsed.

## [Thematic break]<@>

A thematic break occurs when a [thematic break marker] appears as the first
and last token on a line (after outer block continuation).

In HTML, this is rendered as a horizontal rule (`<hr/>`).

A thematic break ends when:
* a [metadata block] appears on the next line, although this
  metadata will be applied to the thematic section which this thematic break
  starts.
* Anything else appears on the next line.

If the thematic break occurs outside of any block, and it is not the first
non-blank content after a heading or the beginning of the document, then it
marks the beginning of a new thematic section at the same level as the previous.

If the thematic break occurs outside of any block, and it *is* the first
non-blank content after a heading or the beginning of the document, then it
marks the beginning of a new thematic section at one level above the current.

## [Heading]<@>

A heading is used to indicate the beginning of a major section of the document,
and it's title.

A heading starts when a [heading open marker] appears as the first token on
a line (after outer block continuation).

There are six levels of headings, and the level of the heading is identified
by the number of `#` in the heading open marker that started it.

After the heading open marker, inline content will be parsed into the content
of the heading. This content can span multiple lines, and can optionally begin
with a repated heading open marker of the same level at the beginning of the
remaining lines.

A heading ends when:
* a [heading close marker] appears at the end of a line (a [heading open
  marker] can not be a [heading close marker], although they look alike).
* a [heading open marker] appears at the beginning of a line without any
  further content. This applies to the first line as well.
  (Headings can not contain multiple paragraphs)
* a [heading] of a different level appears on a potential continuation line.
* a [thematic break], [code block], [blank line], [block quote], [bullet list],
  [ordered list], or [formatted annotation] appears on a potential
  continuation line without a [heading open marker] of the correct type.
* a [metadata block] appears on a potential continuation line, although this
  metadata will be applied to the thematic section which this heading
  starts.
* a containing block ends.
* there are no more lines in the document.

If the heading start occurs outside of all block context types except document
context, then it marks the beginning of a new thematic section at the same
level as the heading.

## [Code block]<@>

A code block is used to indicate preformatted, monospace text, for such
purposes as showing programming examples.

A code block starts when a [code block marker] is found at the beginning of the
line (after outer block continuations). The code block marker consists of three
or more backticks. The marker can optionally continue with a [code block info
string], which must be the final item on the line (if the line does not match
this structure, then a code block is not started).

The info string may be used in the output to identify the styling of the content
of the code block. It is otherwise meaningless to the parser.

Within a code block, all text, including whitespace (starting with the first
line after the code block begins, and ending with the last line before the code
block ends), is kept as-is as the content of the code block, except certain
sequences of backticks.

If a [code block escaped backtick marker] appears at the beginning of a line
anywhere within a code block, the backslash will be trimmed from the beginning
and the remaining backticks will be passed as-is. This token will not work the
same if it is not at the beginning of the line. It is unnecessary if there is
content after the backtick, as that does not match what is required to end the
block.

A code block ends when:
* a [code block marker] appears at the beginning of the line, and is the only
  remaining token on the line.
* a containing block ends.
* there are no more lines in the document.

If the code block occurs outside of any other blocks, and it is the first
sectioning content after a heading or the beginning of the document, then it
marks the beginning of a thematic section one level higher than the previous.

## [Formatted annotation]

A formatted annotation is just an annotation, except that the application has
the option of continuing regular parsing within the annotation. This is useful
for cases where the annotation contains formattings which you still need to see.
This is used in Broth to allow you to still see the syntax highlighting of
partial content that isn't considered ready for publishing.

Suppose there are scenes in a manuscript that is not yet ready for beta-readers
to see, but you still want to make sure they get the latest edits from the rest
of the document while you are finishing up the scene. You can turn these scenes
into formatted annotations, continue to edit them with syntax highlighting, and
hit that publish button to produce a copy for your beta-readers without having
to go back and uncomment the scenes later.

This could also be useful if you have two editions of your manuscript available,
one with author annotations, and the other without. In that case, publishing
will probably require transforming these annotations into some other form that
would make them visible to those with the privilege of seeing those annotations.

A formatted annotation starts with a [formatted annotation open marker] at the
beginning of a line. It can not start within another block. This is because a
formatted annotation contains its own block markup and structure, and any
text appearing after it would not be able to continue at the same point.

Within the formatted annotation context, content is parsed as normal, as if it
as at the document context (including the development of new thematic sections).
It is even possible to nest other annotations and formatted annotations inside
(these should be dealt with appropriately when converting to HTML).
If text appears on a line after the [annotation close marker] it is treated as
if it were the start of a new line.

A formatted annotation ends when:
* an [annotation block close marker] appears.
* there are no more lines in the document.

A formatted annotation itself does not effect thematic sectioning. However,
content within the annotation can effect thematic sectioning at the discretion
of the parser.

## [Metadata Block]<@>

A metadata block is just an annotation, except that the contents of it can be
parsed as metadata, to be applied to the current section. This allows you to
mark up your manuscript with information about different sections, keeping
track of characters, setting, timing, and many other options.

The metadata, as an annotation, is generally not processed when creating HTML.
However, it can be used by other processes to create filters and reports.

A metadata block starts with an [metadata block open marker] at the beginning
of a line. It can not start within another block. This is simply because the
metadata needs to be applied to a section, and if it were to start in another
block, it would look like it's applying to that block.

Within the metadata block, content is parsed in [metadata notation format]. No
textual data is contained in the metadata block, even if it looks like textual
data.

If the metadata block is found within a heading context (i.e. the line
immediately following a heading) it is applied to the section that heading
belongs to.

If it is found immediately after a thematic break, it applies to the section
that break starts.

If it is found outside of a heading context, but before any sectioning content,
it is applied to the section that would begin with that sectioning content.

If it is found anywhere else, the metadata is ignored. However, this is not the
job of the standard parser, so this context requirement does not need to be
stored.

If text appears on a line after the [annotation close marker] it is treated as
if it were a continuation of the line that it started with (the information
should be available in the context).

A metadata block ends when:
* an [annotation close marker] appears.
* there are no more lines in the document.

If the metadata block occurs outside of any other blocks, and it is the first
sectioning content after a heading (but not immediately after the heading)
or the beginning of the document, then it marks the beginning of a thematic
section one level higher than the previous.

## [Link reference definition]<@>

A [link reference definition] defines a label which can be used in [reference
links] and [images] elsewhere in the document.

There is another way to define a label, described under [link], but this one
allows you to define it in a less crowded part of the document, outside of
text.

A link reference definition begins with a [link label] at the beginning of a
line, followed immediately by a [link reference delimiter]. It can not be
found inside any block.

After the [link reference delimiter], any amount of whitespace, including up
to one line ending can appear. This should be followed by a [link destination].
Any content appearing after the link destination will be ignored.

The link reference definition ends when:
* the [link destination] is found, and any extraneous content on that line is
  skipped.
* any content appears at the beginning of the next line that isn't whitespace
  or a [link destination].
* there are no more lines in the document.

The [link reference definition] does not add to the structure of the document,
nor does it affect any thematic sectioning.

## [Paragraph]<@>

A paragraph is the simplest block of content itself. It contains only text
and other inline content.

A paragraph begins with any non-blank line that can not be interpreted as
beginning any other block, after outer block continuation.

Once the paragraph starts, inline content will be parsed into the content
of the paragraph. This content can span multiple lines.

A paragraph ends when:
* a [heading], [thematic break], [code block], [blank line], [block quote],
  [bullet list], [ordered list], [formatted annotation], or
  [metadata block] appears on a potential continuation line.
* a [annotation close marker] appears (ensures that the annotation close
  marker is not treated as textual content in a formatted annotation, since
  it's the only structure that's not inline and can end after the line begins).
* a containing block ends.
* there are no more lines in the document.

If the paragraph occurs outside of any other blocks, and it is the first
sectioning content after a heading or the beginning of the document, then it
marks the beginning of a thematic section one level higher than the previous.

## [Blank line]<@>

A blank line is used only to aid in separating other block-level elements from
each other. It is not part of the structure, nor does it provide any other
useful information.

A blank line consists of the [blank line token] existing alone on a given line.
It does not change the context in itself, but previous context can change itself
because of its existence.

A blank line does not affect thematic sectioning.

## [Container block]<@>

A container block is capable of containing most other blocks. They do not
immediately contain text, but they may consist solely of a single paragraph
which contains the text.

The container blocks generally have a starting token, and a continuation
mechanism which is barsed on inserting characters prior to other characters
at the beginning of the line.

### [Block quote]<@>

A block quote is used to show longer quotes or snippets from other sources. It
could also be used to just indicate content that needs to be indented or
separated out from the rest of the content.

Block quotes contain other block content, and can even include other block
quotes.

A block quote starts when a [block quote marker] appears at the beginning of
a line, after outer block continuation.

A block quote continues on the next line as long as 1) the block quote marker
appears at the beginning of that line, after outer block continuation, or 2)
paragraph continuation content appears at the beginning of that line, after
outer block continuation. This means that the block quote marker is optional
when the paragraph is continuing. Other types of blocks, including blank lines,
have to be denoted by the block quote continuation.

The content after the block quote marker and block quote continuations on
each line define content that will be contained inside the block quote.

A block quote ends when:
* a [heading], [thematic break], [code block], [blank line], [block quote],
  [bullet list], [ordered list], [formatted annotation], or
  [metadata block] appears on a potential continuation line without a block
  quote marker to indicate continuation of the block quote.
* a containing block ends.
* there are no more lines in the document.

If the block quote occurs outside of any other blocks, and it is the first
sectioning content after a heading or the beginning of the document, then it
marks the beginning of a thematic section one level higher than the previous.

### [List item]<@>

A list item is used to put numbers or bullets on a series of blocks in the
content.

List items are actually only contained inside a [list], not any other block.
However, the list container is defined by the existence of the list item,
there are no special delimiters for them.

There are two basic types of list items, further divided into 27 individual types
of list items, but they follow a similar structure.

A [bullet list item]<@> starts with a [bullet list marker]. There are three
types of bullet list markers, identified by the non-whitespace character used in
the marker. All bullet list types use the same [bullet list indent] as a
continuation marker.

An [ordered list item]<@> starts with an [ordered list marker]. There are 24
types of ordered list numbers, identified by the punctuation used in and the
width of the list that begins the list. Each ordered list type uses an [ordered
list indent] of the same width as the marker for a continuation marker. The
ordered list marker also specifies an integer which the list item should start
with. This is not included in the context, but is state that is only available
during the parsing of the start of the list.

To start a list item, the appropriate list marker must appear at the beginning
of the line, after any outer block continuation.

A list item continues on the next line as long as 1) the correct indent appears
at the beginng of that line, after outer block continuation, or 2) paragraph
continuation content appears at the beginning of that line, after outer block
continuation. This means that the indent is optional when a paragraph is
continuing. Other types of blocks, except blank lines, have to be denoted by
the indent.

A list item ends when:
* a [heading], [thematic break], [code block], [block quote], [bullet list],
  [ordered list], [formatted annotation], or [metadata
  block] appears on a potential continuation line without an indent matching the
  list continuation marker for the type of list. [Blank lines] do not end
  a list item, although they will end blocks contained in it.
* a [paragraph] is found while the list item has no inner blocks (for example,
  after a blank line has ended the last paragraph) without an indent matching
  the list continuation for the list item.
* a containing block ends.
* there are no more lines in the document.

There is a separate structure categorization of list items, which is related
to its contents, but this is not important to parsing, only rendering.

A [tight list item]<@> is a list item which contains only one paragraph, no
blank lines, and no other types of blocks. When rendering to HTML, this list
item consists of a simple `<li>` element directly containing the text of the
paragraph.

A [loost list item]<@> is a list item which contains more than one paragraph,
a block of a different type than paragraph, or at least one blank line. This
list item will be rendered in HTML as containing those blocks separately. Thus,
you can force a list item to include its text in a separate paragraph element
simply by adding a blank line somewhere in it.

If the list item occurs outside of any other blocks, and it is the first
sectioning content after a heading or the beginning of the document, then it
marks the beginning of a thematic section one level higher than the previous.

### [List]<@>

A list is a logical container for list items. Depending on rendering, it can
have affect the appearance of the final output for all items in the list.

There are two basic types of lists, divided into five individual types of lists.
These individual types are not exactly the same as the individual types of list
items.

In a [bullet list]<@> all list items are started with the same [bullet list
marker]. The three different punctuations used as markers lead to three
different types of bullet lists.

In an [ordered list]<@> all list items are started with an [ordered list marker]
that contains the same punctuation. The two different punctuations used in these
markers lead to two different types of ordered lists (not the 24 types of list
items, as list items can have different indents and still be in the same list).

A list starts with a list item.

A list ends when a contained [list item] ends without continuing on to another
[list item] using the same punctuation. If a list item does appear, but uses a
different punctuation mark in its marker, then a new list starts.

This should allow us to end a whole mess of lists simply by not indenting the
next item to the appropriate level. There shouldn't be a need for two blank
lines.


# [Inlines]<@>

An inline is a text formatting element, used to apply a small set of standard
styles to the text in a paragraph. Inlines are all parsed sequentially from
left to right (or right to left in right-to-left languages).

Most inlines involve an opening delimiter and a closing delimiter. If the
closing delimiter does not appear before the end of the containing paragraph,
the inline is automatically closed.

## [Escaped character]<@>

In normal text content, any [ASCII punctuation character] can be escaped by
placing a backslash before the character, except within code blocks, code spans,
annotations and link destinations. This is essentially the [escaped character
token].

This backslash prevents the character from having it's usual effect as markup
creating other structures, and instead, the character should be rendered as
plain text in the rendering process, without the backslash.

A backslash before any other character that is not punctuation will result in
that backslash being treated as a literal.

Escaped characters can not contain line endings. Although, there is another
option called a hard line break which makes use of the same notation.

## [Hard line break]<@>

A backslash at the end of a line, a [hard line break marker], optionally with
whitespace following, is a hard line break (rendered in HTML as a `<br />` tag).

Note that in other markdown dialects, it is possible to do the same by placing
two spaces at the end of the line. However, this is bad design, since the two
spaces are essentially invisible to the user, plus many editors may trim
whitespace from the end of lines. Invisible whitespace should *never* be
meaningful syntax, except perhaps in joke programming languages.

A backslash before the [line ending] is parsed as a [hard line break]<@>

## Soft line breaks

Every other [line ending] that isn't a [hard line break] can be rendered as
whitespace or a line ending when processing.

## [Character reference]<@>

A character reference is way of inserting symbols and unusual unicode characters
into a document more easily than finding a way to type them directly on the
keyboard. In general, inserting the actual character will work fine, but may
cause formatting issues when pulled into certain monitors.

A character reference consists of a [character reference token]. The parser
makes no validation of whether these are valid HTML character references. This
is the job of the renderer.

Character references can not contain line endings. Inserting a line ending using
a character reference will not be treated as a line ending.

## [Code span]<@>

A code span serves the same purpose as a code block, except it allows you to
insert the content inline into a paragraph.

A code span starts with a [code span marker]. You'll notice that this is
differentiated from a [code block marker] by having only one backtick, so the
parser can't mistake the two for each other when the code span starts on the
beginning of a line.

Within a code span, all text and whitespace is kept as-is as the content of the
span, except sequences of backticks.

If a [code span escaped backtick sequence] appears in a code span, it will be
converted from two backticks into one. This allows you to indicate a backtick
within a code span, without closing the sequence, by simply doubling the
backtick: (``````). Each pair of backticks found within a code span will
collapse to just one. The odd backtick found will be the end of the code span.

A code span ends when:
* a [code span marker] appears outside of a [code span escaped backtick
  sequence].
* the containing paragraph ends.

Code spans can not contain any other inlines.

## [Styled spans]<@>

Styled spans is a name for a group of three different inlines which
behave in a similar manner. Each one is marked by a single symbol, which
we will call the style marker here.

Note that the mechanism discussed below differs from markdown. Markdown uses
double symbols for strong emphasis and single for regular. In my experience,
however, the convention (up until markdown took over) has usually been an
asterisk for strong emphasis and  slashes for regular emphasis. This is also
much easier to parse than dealing with long chains of delimiters, and opens up
to extensions with other styles using other marks. Thus, I'm going with the
more accepted convention.

A [bold span]<@> is a span of text intended on being rendered with strong
emphasis, which is usually rendered with a bold font. The style marker for
a bold span is `*`.

An [italic span]<@> is a span of text intended on being rendered with regular
emphasis, which is usually rendered with an italic font. The style marker for
an italic span is `/`.

An [underlined span]<@> is a span of text intended on being rendered with an
alternative highlight that isn't emphasis, which might be rendered with an
underlined font. The style marker for an underlined span is `_`.

The markers for these spans are defined as left-flanking and right-flanking.
A left-flanking marker is immeditately followed by a non-whitespace character.
A right-flanking marker immediately follows a non-whitespace character. A marker
can be both left-flanking and right-flanking, it can also be neither.

A styled span will begin with a left-flanking marker of the appropriate type.
A styled span will end with a right-flanking marker, or if the
paragraph which contains it ends.

If a marker can be both left-flanking and right-flanking, then what happens
depends on the state. If the specified style is turned off, then the symbol
will turn it on. If the specified style is turned on, then the symbol will turn
it off.

Although it is possible to nest styles of the same type, although what effect
this should cause in the rendering is undetermined, this can only be done
if the first nested delimiter is left-flanking and not right-flanking.

If a marker can be neither left-flanking or right-flanking, then it will be
rendered as plain text. If a right-flanking marker appears when the style is
already off, then it will be rendered as plain-text.

Only the most recent style in the text can be turned off by a marker, which
means you will probably have unwanted results if you attempt to overlap them.
For example, if bold is turned on, then italic, then a right-flanking delimiter
intended to turn the bold off will not do so, and be rendered as plain text.
Instead, the italic will turn off as expected, but the bold will continue on
to the end of the paragraph.

This problem with nesting also occurs when mixing styled spans with link text
and link labels.

## [Link]<@>

A link is a connection to another document, or another location in the same
document.

Note some variation between my syntax and that of Markdown. It all started
with choosing not to support titles for links. The title attribute on an a
tag in HTML is used primarily for displaying a tooltip to the reader on
mouse-hover. This is UI, not content, and therefore out of scope for BrothMark.
The title attribute is not used for presenting alternate content for
accessibility, that's the alt attribute on images (which are supported).

With the title, it made sense to enclose the URI and the title in a parenthesis.
Without it, there's still a need to separate the URI from the text, but the
twenty year old convention for doing this is to use angle-brackets, not
parenthesis (a convention illustrated by their use in 'autolinks' in Markdown).
But, without the title, there really isn't a difference.

There are several variations that can result in a link, but before we get
into them, we need to describe a few things.

[Link text]<@> is a span of text which is styled to indicate that it is a link,
and works as a link when clicked.

Link text begins with a [link open marker]. Link text will end with a [link
close marker], or if the paragraph which contains it ends. Note that link
text looks a lot like a [link label], but the link label is slightly more
limited in what it can contain. For example, link text can pass across multiple
lines, but link labels can not. Link text can contain formatted inline text,
link labels can not.

Links can not contain other links. Within a link, a [link open marker] will
be treated as a literal, and the first [link close marker] to be found will
close it. A [link close marker] will be treated literally if it is found outside
of link text or an image description. Link text can, however, contain images,
which look similar.

The same care must be taken with link text as styled text in regards to
overlapping. Link text can not be closed unless it is the innermost 'style' in
the context. For example, starting a bold span in the middle of a link text
without closing it within will cause that link to 'leak' to the end of the
paragraph.

A [link destination] is just a URI inside angle-brackets. It can be an e-mail
address, in which case it will be converted into a `mailto:` URI when rendering
to HTML.

An [internal link anchor] is a token that can be used mostly anywhere a [link
destination] can be used, to define an anchor point which other links in the
document can link to.

The following structures can be used to define a link:

* A [standard link]<@> consists of a [link text] immediately followed by a [link
  destination]. In this case, the link text will be the text of the link and
  the link destination will be the URI.
  ```
  [link text]<uri>
  ```
* A [reference link]<@> consists of [link text] immediately followed by a [link
  label]. In this case, an attempt will be made to [match][link matching] the
  link label to a destination defined elsewhere.
  ```
  [link text][link label]
  ```
* An [inline link reference definition]<@> consists of [link text], immediately
  followed by a [link label], immediately followed by a [link destination]. This
  triple threat allows you to specify a link in the text as well as define a
  label for it to use with other links, at the same time. This construct comes
  out of a similar construction used for anchors.
  ```
  [link text][link label]<uri>
  ```
* An [link anchor definition]<@> consists of [link text], immediately
  followed by a [link label], immediately followed by an [internal link anchor].
  This allows you to create an internal anchor, identified by the specified
  label, to the link text.
  ```
  [link text][link label]<@>
  ```
* A [shortcut reference link]<@> consists of a [link label], optionally but
  immediately followed by a [collapsed link label]. In this case, an attempt
  will be made to [match][link matching] the link label to a destination defined
  elsewhere.
  ```
  [link label][]
  [link label]
  ```
* A [shortcut inline link reference definition]<@> consists of a [link label],
  immediately followed by a [collapsed link label], immediately followed by
  a [link destination]. This works like an [inline link reference definition],
  except that the link label will also be used as the link text.
  ```
  [link label][]<uri>
  ```
* A [shortcut link anchor definition]<@> consists of a [link label], optionally
  but immediately followed by a [collapsed link label], immediately
  followed by an [internal link anchor]. This is the same as a [link anchor
  definition], except that the specified link label will also work as a link
  text.
  ```
  [link label][]<@>
  [link label]<@>
  ```
* An [incomplete link]<@> consists of a [link text] that can not be mistaken for
  a [link label] (for example, it contains a [line ending]) and is not part
  of the other structures above. An incomplete link will not have a link
  destination.
  ```
  [link text]
  ```

### [Matching link destinations][link matching]<@>

Connecting a link with a link destination using link labels must be done outside
of the parser, as link labels can be defined after being referenced, and
therefore context not available to the parser. However, the functionality is
still important to how BrothMark works, so it is part of the specification.

When using a [link label] to define the destination, the parser will attempt
to find a destination defined for that link label elsewhere in the document,
either by a [link reference definition], [inline link reference definition],
[link anchor definition], [shortcut inline link reference definition],
[shortcut link anchor definition], [inline image reference definition], or
[shortcut inline image reference definition]. This is done by matching the
label against a defined list of such links.

Before matching, non-whitespace characters in labels are case folded, whitespace
on the edges is trimmed off, and whitespace is collapsed to a single space. If
there are multiple matching labels, the one that comes first in the document,
and contains a link destination, is used.

If a link does not have a destination, either because it could not be found by
matching link labels, or because none was defined, the link can be treated as a
[wiki link]<@> if the application desires. In this case, the link label, or the
link text if that is not available, is matched against filenames in a
pre-specified directory (such as the directory the document exists in), with
a pre-specified extension (such as the extension of the document), and if a
match is found, the link will become a relative link to that document. If
such a file does not exist, the application can have the link cause the user
to be prompted to create one. Keep in mind that path delimiters are valid
characters in a link label, so the application may need to put controls in to
limit the link to specific directories.

## [Image]<@>

An image is a reference to a graphic file located outside the document that
could be embedded into the document.

Note that the variations in syntax with Markdown follow from the same variations
in [links][link]. However, the use of the alt attribute for image description
text is done correctly in this case, so this remains.

There are several variations that can result in an image, but before we get
into them, we need to describe a few things.

[Image description text]<@> is a span of text which will be used to describe
the image. This text can be styled for some rendering, so it can contain
any inline content.

Image description text begins with a [image description open marker].
The text will end with a [link close marker], or if the paragraph which contains
it ends.

Image descriptions can not contain links, or other images. Within a the
description, a [link open marker] or [image description open marker] will
be treated as a literal, and the first [link close marker] to be found will
close it. A [link close marker] will be treated literally if it is found outside
of link text or an image description.

The same care must be taken with image description text as styled text in
regards to overlapping. Image description text can not be closed unless it is
the innermost 'style' in the context. For example, starting a bold span in the
middle of an image description without closing it within will cause that
description to 'leak' to the end of the paragraph.

The following structures can be used to define an image:

* A [standard image]<@> consists of an [image description text] immediately
  followed by a [link destination]. In this case, the link text will be the text
  of the link and the link destination will be the URI.
  ```
  ![image description]<uri>
  ```
* A [reference image]<@> consists of [image description text] immediately
  followed by a [link label]. In this case, an attempt will be made to
  [match][link matching] the link label to a destination defined elsewhere.
  ```
  ![image description][link label]
  ```
* An [inline image reference definition]<@> consists of [image description
  text], immediately followed by a [link label], immediately followed by a [link
  destination]. This allows you to specify an image as well as define a label
  for it to use with other links, at the same time.
  ```
  ![image description][link label]<uri>
  ```
* A [shortcut reference image]<@> consists of a [shortcut image label],
  optionally but immediately followed by a [collapsed link label]. In this case,
  an attempt will be made to [match][link matching] the image label to a
  destination defined elsewhere.
  ```
  ![image label][]
  ![image label]
  ```
* A [shortcut inline image reference definition]<@> consists of a [shortcut
  image label], immediately followed by a [collapsed link label], immediately
  followed by a [link destination]. This works like an [inline link reference
  definition], except that the image label will also be used as the image
  description.
  ```
  ![image label][]<uri>
  ```
* An [incomplete image]<@> consists of an [image description text] that can not
  be mistaken for a [shortcut image label] (for example, it contains a
  [line ending]) and is not part of the other structures above. An incomplete
  image will not have an image source.
  ```
  ![image label]
  ```

Images are matched their sources using the same process as
[matching][link matching] a link to its destination.

## Textual content

Any characters not given an interpretation by the above rules will
be parsed as plain textual content.

# Metadata notation format

<!--~
NOTE: I originally intended to use a subset of YAML here, having only heard
of it but never really explored it (since JSON is so simple and expressive for
the purpose). Then, I went to their home page at <yaml.org>. Oh god, that hurt
my eyes. Okay, the page is simple, but it is *not* easily readable. The
punctuation marks all over the place distracted from the useful information,
and when I looked for something that would actually tell me what it was, the
best I could find was the complex spec. YAML might be simple to figure out,
but I didn't want to figure it out in order to read that page, I wanted to
know what it was.

I went to google to get more information. I looked up "yaml", and it put their
site at the top. The second (or third?) link said "Get Started - YAML". That
link led to their site, to a page that *wasn't linked to on their home page*,
at least not easily visible. However, even that page sucked, it still required
me to know YAML before I could read it. There was a link to a FAQ, with just
two questions that had nothing to do with the syntax itself. The best I could
find was a 'reference card'. It was still written in YAML itself, but at least
it gave me the basic ideas.

At that point, I decided I'm not going to bother implementing even just a
subset of YAML. I'm going to take the intuitive stuff off of that reference
card and define my own, simpler syntax.

Seriously, look at how [Markdown is described by Gruber]<http://daringfireball.net/>.
Look at how [JSON is described]<http://json.org>. Both of these came from the
same tradition as YAML, of simplifying conventions that were already in use.
Their descriptions are simple, quick, to the point and formatted for human
reading. They knew that their format wasn't as good at this as a nicely laid
out web page, even if they could have used it.

In any case, after a review of YAML, and the realization that there are dozens
of ways to reach the same goal for almost everything, I decided that YAML ain't
right, so I'm choosing something a little simpler.
-->

The Metadata notation format is defined separately from the rest of Brothmark.
Tokens and delimiters are not shared between the two. The only thing about it
is that it is designed not to conflict with the closing delimiter used by the
metadata block.

[Metadata notation format]<@>, also known as
[BrothMark Metadata Notation Format]<@> or [BMNF]<@> is designed to store simple
structured data in a way that is easy to parse and easy to read. It follows many
of the same goals as BrothMark itself.

BMNF can be used to define a three different data value types.
* A [string][BMNF-string]<@>, which is a collection of Unicode characters.
* A [list][BMNF-list]<@>, containing a list of values, indexed by number.
* A [map][BMNF-map]<@> containing a list of values, indexed with a
  string key.

This simple data structure should be capable of storing almost any kind of data,
assuming that non-string data can be encoded into a string form. However, BMNF
is generally designed to store data that is human-readable, it is not intended
as a data transmission format.

Apart from a suggestion regarding three basic data types, and algorithms for
processing and parsing the data, this specification does not specify exactly
how the data should be stored or manipulated within a program, it only defines
one way to store and edit data in plain text format, and specifies the three
basic data types required for storage.

## Characters

BMNF uses the same character definitions as defined for BrothMark, above.

## Tokens

The tokens are defined below in the same syntax as tokens are defined for
BrothMark.

* ` (<[non-whitespace character]> - "\" - "&" |
     ("\" <[whitespace character]>) | ("\\") |
     ("&" <[non-whitespace character]>+ ";"))+ ":" `: [BMNF key]<@>
* ` "* " `: [BMNF unkeyed item marker]<@>
* ` "  " `: [BMNF list continuation indent]<@>
* ` "&" <[non-whitespace character]>+ ";" `: [BMNF character reference]<@>
* ` "\" <[non-whitespace character]> `: [BMNF escaped character]<@>
* ` "\" " "* <[line ending]> `: [BMNF hard line break marker]<@> (NOTE: This
  should be higher priority than [BMNF escaped whitespace].
* ` "\" <[whitespace character]> `: [BMNF escaped whitespace]<@>
* `"-->"`: [annotation close marker]
* Anything else, including blank lines, is considered [BMNF textual content]<@>

## [BMNF document]<@>

A BMNF document consists of a BMNF structure literal. It begins when that
structure literal begins, and ends with an [annotation close marker], which
should not be a valid value in any of the contexts.

## [BMNF structure literal]<@>

A structure literal describes a set of values, and optionally their keys,
that will be contained inside a list or map value.

A structure literal begins with the first contained item, and ends when
any outer container ends.

A structure can contain a [BMNF string], a [BMNF unkeyed item], or a
[BMNF keyed item].

The value of a structure is the result of processing all items into a list, map,
or a string, according to the following algorithm.
* Create a map /resultMap/
* Let a variable /unkeyedItemFound/ be /false/.
* Let a variable /keyItemFound/ be /false/.
* Let a variable /counter/ be /0/.
* for each item:
  * If the item is a string, add the value of that string to the map, with
    a key equal to the string value the number /counter/.
  * If the item is an unkeyed item:
    * let /value/ be the value of that item.
    * add /value/ to the map with a key equal to the string
      value of the number /counter/.
    * set /unkeyedItemFound/ to /true/.
  * If the item is a keyed item:
    * let /value/ be the value of that item.
    * Let /key/ be the string value of the key of the keyed item.
    * If the map does not contain a value at /key/:
      * add /value/ to the map with a key equal to /key/
      * set /keyedItemFound/ to /true/.
    * otherwise:
      * add the value of the item to the map with a key equal to the string
        value of the number /counter/.
    * /These last steps have the result of ignoring duplicate keys, but not
      losing their data/.
  * increase /counter/ by /1/.
* if /counter/ equals exactly /1/, /unkeyedItemFound/ equals exactly /false/,
  and /keyedItemFound/ equals exactly /false/, then:
  * return the value found at the key with the string value of "0" as the
    result of this process.
  * /This has the result of returning a value defined as a single string literal
    as a string instead of a structure/.
* otherwise, if /keyedItemFound/ equals exactly /false/, then:
  * create a list /resultList/
  * let /i/ be /0/
  * while /i/ is less than /counter/:
    * let /item/ be the value found at the key with the string value of /i/.
    * add /item/ to the end of /resultList/
    * increase /i/ by /1/.
  * return /resultList/
  * /This has the result of turning a list without any keyed items into a list,
    whereas a list with some keyed items returns a map, with some of the items
    keyed only by number/
* otherwise, return /resultMap/

## [BMNF string literal]<@>

A string literal begins at the beginning of a line, after outer container
continuation, with a [[BMNF character reference], [BMNF escaped character],
[BMNF hard line break marker], [BMNF escaped whitespace], [Line Ending] or any
[BMNF textual content].

The string will end when:
* the outer container ends.
* a line begins with a [BMNF key].
* a line beins with a [BMNF unkeyed item marker].

That's it. A string literal is multiline, and looks basically just like text
content. If you wish to include content in the string that looks like a BMNF
keyed value, you can simply escape the colon in the [BMNF key]. If you wish to
include content that looks like a BMNF unkeyed value, you can simply escape
the [BMNF unkeyed item marker] (actually, it should be possible to escape
the space and get this result). To include text that looks like an
[annotation close marker], you should be able to escape any character
in the string.

The value of a string is the result of processing that string literal. This
means:
* BMNF hard line break markers are converted into `&#000A;` (or optionally,
  `&#000D;` or `&#000D;&#000A;`. This insures that they are not converted,
  trimmed or collapsed with later steps.
* BMNF escaped whitespace is converted into `&#0020;` or `&#0009;` depending
  on which character is escaped. This insures that they are not trimmed or
  collapsed with later steps.
* All whitespace, including line endings, are trimmed off of the beginning and
  end of the string.
* All whitespace, including line endings, are collapsed to a single space inside
  the string.
* All escaped non-whitespace characters are collapsed to the character without
  the backslash.
* All character references are converted to appropriate unicode characters
  according to HTML character reference specifications if possible. If they
  are not valid HTML character references, they will remain untranslated.
  This should convert the hard line breaks and escaped whitespace, that were
  converted to references, back into regular line endings and whitespace

## [BMNF unkeyed item literal]<@>

An unkeyed item literal is used to add an unkeyed item to the parent structure.
An unkeyed item is not unindexed, it is merely not accessible by a string key.
The primary purpose of the unkeyed item literal is to differentiate multiple
string literals from each other, and to insure that a structure is returned
instead of a string when only one item is in the structure.

An unkeyed item begins at the beginning of a line, after outer container
continuation, with a [BMNF unkeyed item marker]. This is followed by a structure
literal, whose following lines must be prefixed by outer container continuation
plus one [BMNF list continuation indent].

The unkeyed item will end when:
* the outer container ends.
* a line does not begin with a [BMNF list continuation indent].

Unlike BrothMark lists, the indents are required. String continuation text
that does not start with the appropriate indent becomes a different value
for the literal.

The value of the unkeyed item will be an unkeyed item containing the value
of the structure literal it contains.

## [BMNF keyed item literal]<@>

A keyed item literal is used to add a keyed item to a structure literal.

A keyed item begins at the beginning of a line, after outer container
continuation, with a [BMNF key]. This is followed by a structure literal, whose
following lines must be prefixed by outer container continuation plus one
[BMNF list continuation indent].

The keyed item will end when:
* the outer container ends.
* a line does not begin with a [BMNF list continuation indent].

The value of the key in the literal is retrieved by processing it with the
following steps:
* BMNF escaped whitespace is converted into `&#0020;` or `&#0009;` depending
  on which character is escaped. This insures that they are not trimmed or
  collapsed with later steps.
* escaped slashes are collapsed to a single slash.
* All character references are converted to appropriate unicode characters
  according to HTML character reference specifications if possible. If they
  are not valid HTML character references, they will remain untranslated.
  This should convert the hard line breaks and escaped whitespace, that were
  converted to references, back into regular line endings and whitespace

The value of the keyed item will be an keyed item containing the value of
the key and the value of the structure literal it contains.

TODO: HEREIAM Next, work on parsing. The section on parsing starts to describe
a parser, but I'd rather just make a reference implementation instead of
putting this into the spec. But first, if we get Broth to where we
can edit and save a document, we can then use it to "test" the document
as we go with the parser.


TODO: This stuff should actually just be done in the code itself.

# Parsing

The specification describes the actions of the parser based on specific
contexts.

A few definitions:
* The [context] stack is arranged from [innermost]<@> context type to
[outermost]<@> context type. The [innermost] context is the most recent
context type placed on the stack.
* A position in a document can be said to be [within]<@> a [context type] if,
and only if, said context type is somewhere on the [context] stack at that time.
* A position in a document can be said to be [immediately within]<@> a [context
type] if that [context type] is the [innermost] context type on the stack.
* A position in a document can be said to be [outside]<@> of a [context type]
  if that context type is nowhere in the [context] stack at that time.


The parsing of a given line works by passing the work off to a procedure
in charge of parsing the [outermost] context. This procedure will look at the
line to decide what to do. This procedure then does the work of processing
the line, or passing the remainder off to different procedures.

If the parser starts a new line with an empty context stack, then the context
is said to be in [document context].

# [Document Context]<@>

In document context, the following delimiters might be found at the beginning
of the line:

* [thematic break marker]:
  call [Thematic Break].
* [heading open marker]:
  call [Heading Start].
* [code block marker]:
  call [Code Block Start]
* [link label]:
  if the link label is followed immediately by a [link reference delimiter]
  then call [Link Reference Definition].
* [blank line token]:
  Do nothing and return, blank lines do nothing in the document context.
* [block quote marker]:
  call [Block Quote Start]
* [bullet list marker]:
  call [Bullet List Start]
* [ordered list marker]:
  call [Ordered List Start]
* [annotation open marker]:
  call [Annotation Start]
* [formatted annotation open marker]:
  call [Formatted Annotation Start]

If after the above, the context stack remains empty, call [Paragraph Start].

# [Block Content]<@>

A [block]<@> is a structural element, like paragraphs, block quotations, lists
and headings. Some blocks (like block quotes and list items) can contain other
blocks, others contain [inline content].

If block content is allowed, then the following sections describe the possible
content that could be parsed.

## [Thematic break]<@>

The Thematic Break procedure expects a [thematic break marker] in the remainder
of the line. In HTML, this is rendered as a horizontal rule (`<hr/>`).

A thematic break does not add any context type to the context, nor does it
remove any context types. The context does not change with the end of the
line.

If the thematic break occurs outside of all block context types except document
context, then it marks the beginning of a new thematic section at one level
below the most recent heading.

## [Heading Start]<@>

* Expect a [heading open marker] at the current position in the line.
* Determine the level of the heading ( a value of 1-6) based on the length
  of the heading open marker that started this.
* Add a new heading context to the context stack, identified by the calculated
  heading level.
* If the next token is not a [heading close marker], nor a [blank line], then
  call [Inline Content], which will become the content of the heading.
* If the next token is a [heading close marker], then remove the heading context
  from the stack.
* Parsing is done.

If the heading start occurs outside of all block context types except document
context, then it marks the beginning of a new thematic section at the same
level as the heading.

## [Heading Context]<@>

The following tokens are allowed at the beginning of the heading context in
the line:

* [thematic break marker]:
  remove the heading context, and call [Thematic Break].
* [heading open marker]:
  if the marker is a different length than the current heading context level,
  then remove the heading context, and call [Heading Start].
* [code block marker]:
  remove the heading context, and call [Code Block Start]
* [link label]:
  if the link label is followed immediately by a [link reference delimiter]
  then remove the heading context, and call [Link Reference Definition].
* [blank line token]:
  remove the heading context, and return.
* [block quote marker]:
  remove the heading context, and call [Block Quote Start]
* [bullet list marker]:
  remove the heading context, and call [Bullet List Start]
* [ordered list marker]:
  remove the heading context, and call [Ordered List Start]
* [annotation open marker]:
  call [Metadata Block Start]
* [formatted annotation open marker]:
  remove the heading context, and call [Formatted Annotation Start]

If after the above, the heading context has not been removed, then call [Inline
Content], which will be added to the content of the heading.

etc.



TODO: Examples below
TODO: These all need to be fixed, as many are still defined for CommonMark,
or something in between this and CommonMark. I also want to turn this into
a sort of 'test suite', so that I can compare the output given in the example
with actual output from a parser, and make sure they are correct.

# Examples

The following examples help illustrate and test the parser defined above.

## Thematic Break Examples
```````````````````````````````` example
---
.
<hr />
````````````````````````````````

---


Wrong characters:

```````````````````````````````` example
+++
.
<p>+++</p>
````````````````````````````````

+++


```````````````````````````````` example
===
.
<p>===</p>
````````````````````````````````

===


Not enough characters:

```````````````````````````````` example
--
.
<p>--</p>
````````````````````````````````

--



More than three characters may be used:

```````````````````````````````` example
-----------------------------------
.
<hr />
````````````````````````````````

-----------------------------------


Spaces are allowed at the end:

```````````````````````````````` example
----
.
<hr />
````````````````````````````````

----


However, no other characters may occur in the line:

```````````````````````````````` example
---- a

a------

---a---
.
<p>---- a</p>
<p>a------</p>
<p>---a---</p>
````````````````````````````````

---- a

a------

---a---


Thematic breaks do not need blank lines before or after:

```````````````````````````````` example
- foo
---
- bar
.
<ul>
<li>foo</li>
</ul>
<hr />
<ul>
<li>bar</li>
</ul>
````````````````````````````````


Thematic breaks can interrupt a paragraph:

```````````````````````````````` example
Foo
---
bar
.
<p>Foo</p>
<hr />
<p>bar</p>
````````````````````````````````

# Headings

Simple headings:

```````````````````````````````` example
# foo
## foo
### foo
#### foo
##### foo
###### foo
.
<h1>foo</h1>
<h2>foo</h2>
<h3>foo</h3>
<h4>foo</h4>
<h5>foo</h5>
<h6>foo</h6>
````````````````````````````````

# foo
## foo
### foo
#### foo
##### foo
###### foo


More than six `#` characters is not a heading:

```````````````````````````````` example
####### foo
.
<p>####### foo</p>
````````````````````````````````

####### foo


At least one space is required between the `#` characters and the
heading's contents, unless the heading is empty. Note that many
implementations currently do not require the space. However, the
space was required by the
[original ATX implementation](http://www.aaronsw.com/2002/atx/atx.py),
and it helps prevent things like the following from being parsed as
headings:

```````````````````````````````` example
#5 bolt

#hashtag
.
<p>#5 bolt</p>
<p>#hashtag</p>
````````````````````````````````

#5 bolt

#hashtag


This is not a heading, because the first `#` is escaped:

```````````````````````````````` example
\## foo
.
<p>## foo</p>
````````````````````````````````

\## foo


Contents are parsed as inlines:

```````````````````````````````` example
# foo *bar* \*baz\*
.
<h1>foo <em>bar</em> *baz*</h1>
````````````````````````````````

# foo *bar* \*baz\*


Leading and trailing blanks are ignored in parsing inline content:

```````````````````````````````` example
#                  foo
.
<h1>foo</h1>
````````````````````````````````

#                  foo


A closing sequence of `#` characters is optional:

```````````````````````````````` example
## foo ##
###   bar    ###
.
<h2>foo</h2>
<h3>bar</h3>
````````````````````````````````

## foo ##
###   bar    ###


It need not be the same length as the opening sequence:

```````````````````````````````` example
# foo ##################################
##### foo ##
.
<h1>foo</h1>
<h5>foo</h5>
````````````````````````````````

# foo ##################################
##### foo ##


Spaces are allowed after the closing sequence:

```````````````````````````````` example
### foo ###
.
<h3>foo</h3>
````````````````````````````````

### foo ###


A sequence of `#` characters with anything but [spaces] following it
is not a closing sequence, but counts as part of the contents of the
heading:

```````````````````````````````` example
### foo ### b
.
<h3>foo ### b</h3>
````````````````````````````````

### foo ### b


The closing sequence must be preceded by a space:

```````````````````````````````` example
# foo#
.
<h1>foo#</h1>
````````````````````````````````

# foo#


Backslash-escaped `#` characters do not count as part
of the closing sequence:

```````````````````````````````` example
### foo \###
## foo #\##
# foo \#
.
<h3>foo ###</h3>
<h2>foo ###</h2>
<h1>foo #</h1>
````````````````````````````````

### foo \###
## foo #\##
# foo \#


ATX headings need not be separated from surrounding content by blank
lines, and they can interrupt paragraphs:

```````````````````````````````` example
****
## foo
****
.
<hr />
<h2>foo</h2>
<hr />
````````````````````````````````

****
## foo
****


```````````````````````````````` example
Foo bar
# baz
Bar foo
.
<p>Foo bar</p>
<h1>baz</h1>
<p>Bar foo</p>
````````````````````````````````

Foo bar
# baz
Bar foo


ATX headings can be empty:

```````````````````````````````` example
##
#
### ###
.
<h2></h2>
<h1></h1>
<h3></h3>
````````````````````````````````

##
#
### ###

# Code Block

TODO: These need to be fixed.


Here is a simple example with backticks:

```````````````````````````````` example
```
<
 >
```
.
<pre><code>&lt;
 &gt;
</code></pre>
````````````````````````````````

```
<
 >
```


With tildes:

```````````````````````````````` example
~~~
<
 >
~~~
.
<pre><code>&lt;
 &gt;
</code></pre>
````````````````````````````````

~~~
<
 >
~~~


The closing code fence must use the same character as the opening
fence:

```````````````````````````````` example
```
aaa
~~~
```
.
<pre><code>aaa
~~~
</code></pre>
````````````````````````````````

```
aaa
~~~
```


```````````````````````````````` example
~~~
aaa
```
~~~
.
<pre><code>aaa
```
</code></pre>
````````````````````````````````

~~~
aaa
```
~~~


Unclosed code blocks are closed by the end of the document
(or the enclosing [block quote][block quotes] or [list item][list items]):

```````````````````````````````` example
```
.
<pre><code></code></pre>
````````````````````````````````

(The example is not shown as it would lead to a corrupted document)

```````````````````````````````` example
`````

```
aaa
.
<pre><code>
```
aaa
</code></pre>
````````````````````````````````

(The example is not shown as it would lead to a corrupted document)



```````````````````````````````` example
> ```
> aaa

bbb
.
<blockquote>
<pre><code>aaa
</code></pre>
</blockquote>
<p>bbb</p>
````````````````````````````````

> ```
> aaa

bbb


A code block can have all empty lines as its content:

```````````````````````````````` example
```


```
.
<pre><code>

</code></pre>
````````````````````````````````

```


```


A code block can be empty:

```````````````````````````````` example
```
```
.
<pre><code></code></pre>
````````````````````````````````

```
```


Code fences (opening and closing) cannot contain internal spaces:

```````````````````````````````` example
``` ```
aaa
.
<p><code></code>
aaa</p>
````````````````````````````````

``` ```
aaa


```````````````````````````````` example
~~~~~~
aaa
~~~ ~~
.
<pre><code>aaa
~~~ ~~
</code></pre>
````````````````````````````````

(The example is not shown as it would lead to a corrupted document)


Fenced code blocks can interrupt paragraphs, and can be followed
directly by paragraphs, without a blank line between:

```````````````````````````````` example
foo
```
bar
```
baz
.
<p>foo</p>
<pre><code>bar
</code></pre>
<p>baz</p>
````````````````````````````````

foo
```
bar
```
baz


Other blocks can also occur before and after fenced code blocks
without an intervening blank line:

```````````````````````````````` example
foo
---
~~~
bar
~~~
# baz
.
<h2>foo</h2>
<pre><code>bar
</code></pre>
<h1>baz</h1>
````````````````````````````````

foo
---
~~~
bar
~~~
# baz


An [info string] can be provided after the opening code fence.
Opening and closing spaces will be stripped, and the first word, prefixed
with `language-`, is used as the value for the `class` attribute of the
`code` element within the enclosing `pre` element.

```````````````````````````````` example
```ruby
def foo(x)
  return 3
end
```
.
<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>
````````````````````````````````

```ruby
def foo(x)
  return 3
end
```


```````````````````````````````` example
~~~~    ruby startline=3 $%@#$
def foo(x)
  return 3
end
~~~~~~~
.
<pre><code class="language-ruby">def foo(x)
  return 3
end
</code></pre>
````````````````````````````````

~~~~    ruby startline=3 $%@#$
def foo(x)
  return 3
end
~~~~~~~


```````````````````````````````` example
````;
````
.
<pre><code class="language-;"></code></pre>
````````````````````````````````

````;
````


[Info strings] for backtick code blocks cannot contain backticks:

```````````````````````````````` example
``` aa ```
foo
.
<p><code>aa</code>
foo</p>
````````````````````````````````

``` aa ```
foo


Closing code fences cannot have [info strings]:

```````````````````````````````` example
```
``` aaa
```
.
<pre><code>``` aaa
</code></pre>
````````````````````````````````

```
``` aaa
```

## Link reference definitions
TODO: These need to be fixed for the current requirements.

```````````````````````````````` example
[foo]: /url

[foo]
.
<p><a href="/url" >foo</a></p>
````````````````````````````````

[foo]: /url

[foo]


```````````````````````````````` example
[foo]:
      /url
           'the title'

[foo]
.
<p><a href="/url" title="the title">foo</a></p>
````````````````````````````````

[foo]:
      /url
           'the title'

[foo]


```````````````````````````````` example
[Foo*bar\]]:my_(url) 'title (with parens)'

[Foo*bar\]]
.
<p><a href="my_(url)" title="title (with parens)">Foo*bar]</a></p>
````````````````````````````````

[Foo*bar\]]:my_(url) 'title (with parens)'

[Foo*bar\]]


```````````````````````````````` example
[Foo bar]:
<my%20url>
'title'

[Foo bar]
.
<p><a href="my%20url" title="title">Foo bar</a></p>
````````````````````````````````

[Foo bar]:
<my%20url>
'title'

[Foo bar]


The title may extend over multiple lines:

```````````````````````````````` example
[foo]: /url '
title
line1
line2
'

[foo]
.
<p><a href="/url" title="
title
line1
line2
">foo</a></p>
````````````````````````````````

[foo]: /url '
title
line1
line2
'

[foo]


However, it may not contain a [blank line]:

```````````````````````````````` example
[foo]: /url 'title

with blank line'

[foo]
.
<p>[foo]: /url 'title</p>
<p>with blank line'</p>
<p>[foo]</p>
````````````````````````````````

[foo]: /url 'title

with blank line'

[foo]


The title may be omitted:

```````````````````````````````` example
[foo]:
/url

[foo]
.
<p><a href="/url">foo</a></p>
````````````````````````````````

[foo]:
/url

[foo]


The link destination may not be omitted:
TODO: If this is going to happen, then I can't allow the destination to appear
on the next line.

```````````````````````````````` example
[foo]:

[foo]
.
<p>[foo]:</p>
<p>[foo]</p>
````````````````````````````````

[foo]:

[foo]


Both title and destination can contain backslash escapes
and literal backslashes:

```````````````````````````````` example
[foo]: /url\bar\*baz "foo\"bar\baz"

[foo]
.
<p><a href="/url%5Cbar*baz" title="foo&quot;bar\baz">foo</a></p>
````````````````````````````````

[foo]: /url\bar\*baz "foo\"bar\baz"

[foo]


A link can come before its corresponding definition:

```````````````````````````````` example
[foo]

[foo]: url
.
<p><a href="url">foo</a></p>
````````````````````````````````

[foo]

[foo]: url


If there are several matching definitions, the first one takes
precedence:

```````````````````````````````` example
[foo]

[foo]: first
[foo]: second
.
<p><a href="first">foo</a></p>
````````````````````````````````

[foo]

[foo]: first
[foo]: second


As noted in the section on [Links], matching of labels is
case-insensitive (see [matches]).

```````````````````````````````` example
[FOO]: /url

[Foo]
.
<p><a href="/url">Foo</a></p>
````````````````````````````````

[FOO]: /url

[Foo]


```````````````````````````````` example
[ΑΓΩ]: /φου

[αγω]
.
<p><a href="/%CF%86%CE%BF%CF%85">αγω</a></p>
````````````````````````````````

[ΑΓΩ]: /φου

[αγω]


Here is a link reference definition with no corresponding link.
It contributes nothing to the document.

```````````````````````````````` example
[foo]: /url
.
````````````````````````````````

[foo]: /url


Here is another one:

```````````````````````````````` example
[
foo
]: /url
bar
.
<p>bar</p>
````````````````````````````````

[
foo
]: /url
bar


This is not a link reference definition, because there are
[non-whitespace characters] after the title:
TODO: This is going to be a problem if I have to parse quotes for the title.

```````````````````````````````` example
[foo]: /url "title" ok
.
<p>[foo]: /url &quot;title&quot; ok</p>
````````````````````````````````

[foo]: /url "title" ok


This is a link reference definition, but it has no title:

```````````````````````````````` example
[foo]: /url
"title" ok
.
<p>&quot;title&quot; ok</p>
````````````````````````````````

[foo]: /url
"title" ok


This is not a link reference definition, because it is indented
four spaces:

```````````````````````````````` example
    [foo]: /url "title"

[foo]
.
<p>[foo]: /url &quot;title&quot;
</p>
<p>[foo]</p>
````````````````````````````````

    [foo]: /url "title"

[foo]


This is not a link reference definition, because it occurs inside
a code block:

```````````````````````````````` example
```
[foo]: /url
```

[foo]
.
<pre><code>[foo]: /url
</code></pre>
<p>[foo]</p>
````````````````````````````````

```
[foo]: /url
```

[foo]


A [link reference definition] cannot interrupt a paragraph.

```````````````````````````````` example
Foo
[bar]: /baz

[bar]
.
<p>Foo
[bar]: /baz</p>
<p>[bar]</p>
````````````````````````````````

Foo
[bar]: /baz

[bar]


However, it can directly follow other block elements, such as headings
and thematic breaks, and it need not be followed by a blank line.

```````````````````````````````` example
# [Foo]
[foo]: /url
> bar
.
<h1><a href="/url">Foo</a></h1>
<blockquote>
<p>bar</p>
</blockquote>
````````````````````````````````

# [Foo]
[foo]: /url
> bar


Several [link reference definitions]
can occur one after another, without intervening blank lines.

```````````````````````````````` example
[foo]: /foo-url "foo"
[bar]: /bar-url
  "bar"
[baz]: /baz-url

[foo],
[bar],
[baz]
.
<p><a href="/foo-url" title="foo">foo</a>,
<a href="/bar-url" title="bar">bar</a>,
<a href="/baz-url">baz</a></p>
````````````````````````````````

[foo]: /foo-url "foo"
[bar]: /bar-url
  "bar"
[baz]: /baz-url

[foo],
[bar],
[baz]


[Link reference definitions] can occur
inside block containers, like lists and block quotations.  They
affect the entire document, not just the container in which they
are defined:

```````````````````````````````` example
[foo]

> [foo]: /url
.
<p><a href="/url">foo</a></p>
<blockquote>
</blockquote>
````````````````````````````````

[foo]

> [foo]: /url

## Paragraph

```````````````````````````````` example
aaa

bbb
.
<p>aaa</p>
<p>bbb</p>
````````````````````````````````

aaa

bbb


Paragraphs can contain multiple lines, but no blank lines:

```````````````````````````````` example
aaa
bbb

ccc
ddd
.
<p>aaa
bbb</p>
<p>ccc
ddd</p>
````````````````````````````````

aaa
bbb

ccc
ddd


Multiple blank lines between paragraph have no effect:

```````````````````````````````` example
aaa


bbb
.
<p>aaa</p>
<p>bbb</p>
````````````````````````````````

aaa


bbb


Leading spaces are skipped:

```````````````````````````````` example
  aaa
 bbb
.
<p>aaa
bbb</p>
````````````````````````````````

  aaa
 bbb


Lines after the first may be indented any amount.

```````````````````````````````` example
aaa
             bbb
                                       ccc
.
<p>aaa
bbb
ccc</p>
````````````````````````````````

aaa
             bbb
                                       ccc


Final spaces are stripped before inline parsing, so a paragraph
that ends with two or more spaces will not end with a [hard line
break]:

```````````````````````````````` example
aaa
bbb
.
<p>aaa<br />
bbb</p>
````````````````````````````````

aaa
bbb

## Blank line

```````````````````````````````` example


aaa


# aaa


.
<p>aaa</p>
<h1>aaa</h1>
````````````````````````````````



aaa


# aaa



## Block quote

Here is a simple example:

```````````````````````````````` example
> # Foo
> bar
> baz
.
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>
````````````````````````````````

> # Foo
> bar
> baz


The Laziness clause allows us to omit the `>` before
[paragraph continuation text]:

```````````````````````````````` example
> # Foo
> bar
baz
.
<blockquote>
<h1>Foo</h1>
<p>bar
baz</p>
</blockquote>
````````````````````````````````

> # Foo
> bar
baz


A block quote can contain some lazy and some non-lazy
continuation lines:

```````````````````````````````` example
> bar
baz
> foo
.
<blockquote>
<p>bar
baz
foo</p>
</blockquote>
````````````````````````````````

> bar
baz
> foo


Laziness only applies to lines that would have been continuations of
paragraphs had they been prepended with [block quote markers].
For example, the `> ` cannot be omitted in the second line of

``` markdown
> foo
> ---
```

> foo
> ---

without changing the meaning:

```````````````````````````````` example
> foo
---
.
<blockquote>
<p>foo</p>
</blockquote>
<hr />
````````````````````````````````

> foo
---


Similarly, if we omit the `> ` in the second line of

``` markdown
> - foo
> - bar
```

> - foo
> - bar

then the block quote ends after the first line:

```````````````````````````````` example
> - foo
- bar
.
<blockquote>
<ul>
<li>foo</li>
</ul>
</blockquote>
<ul>
<li>bar</li>
</ul>
````````````````````````````````

> - foo
- bar


For the same reason, we can't omit the `> ` in front of
subsequent lines of a fenced code block:

```````````````````````````````` example
> ```
foo
```
.
<blockquote>
<pre><code></code></pre>
</blockquote>
<p>foo</p>
<pre><code></code></pre>
````````````````````````````````

> ```
foo
```


Note that in the following case, we have a [lazy
continuation line]:

```````````````````````````````` example
> foo
    - bar
.
<blockquote>
<p>foo
- bar</p>
</blockquote>
````````````````````````````````

> foo
    - bar


To see why, note that in

```markdown
> foo
>     - bar
```

the `- bar` is indented too far to start a list, and can't
be an indented code block because indented code blocks cannot
interrupt paragraphs, so it is [paragraph continuation text].

A block quote can be empty:

```````````````````````````````` example
>
.
<blockquote>
</blockquote>
````````````````````````````````

>


```````````````````````````````` example
>
>
>
.
<blockquote>
</blockquote>
````````````````````````````````

>
>
>


A block quote can have initial or final blank lines:

```````````````````````````````` example
>
> foo
>
.
<blockquote>
<p>foo</p>
</blockquote>
````````````````````````````````

>
> foo
>


A blank line always separates block quotes:

```````````````````````````````` example
> foo

> bar
.
<blockquote>
<p>foo</p>
</blockquote>
<blockquote>
<p>bar</p>
</blockquote>
````````````````````````````````

> foo

> bar


Consecutiveness means that if we put these block quotes together,
we get a single block quote:

```````````````````````````````` example
> foo
> bar
.
<blockquote>
<p>foo
bar</p>
</blockquote>
````````````````````````````````

> foo
> bar


To get a block quote with two paragraphs, use:

```````````````````````````````` example
> foo
>
> bar
.
<blockquote>
<p>foo</p>
<p>bar</p>
</blockquote>
````````````````````````````````

> foo
>
> bar


Block quotes can interrupt paragraphs:

```````````````````````````````` example
foo
> bar
.
<p>foo</p>
<blockquote>
<p>bar</p>
</blockquote>
````````````````````````````````

foo
> bar


In general, blank lines are not needed before or after block
quotes:

```````````````````````````````` example
> aaa
***
> bbb
.
<blockquote>
<p>aaa</p>
</blockquote>
<hr />
<blockquote>
<p>bbb</p>
</blockquote>
````````````````````````````````

> aaa
***
> bbb


However, because of laziness, a blank line is needed between
a block quote and a following paragraph:

```````````````````````````````` example
> bar
baz
.
<blockquote>
<p>bar
baz</p>
</blockquote>
````````````````````````````````

> bar
baz


```````````````````````````````` example
> bar

baz
.
<blockquote>
<p>bar</p>
</blockquote>
<p>baz</p>
````````````````````````````````

> bar

baz


```````````````````````````````` example
> bar
>
baz
.
<blockquote>
<p>bar</p>
</blockquote>
<p>baz</p>
````````````````````````````````

> bar
>
baz


It is a consequence of the Laziness rule that any number
of initial `>`s may be omitted on a continuation line of a
nested block quote:

```````````````````````````````` example
> > > foo
bar
.
<blockquote>
<blockquote>
<blockquote>
<p>foo
bar</p>
</blockquote>
</blockquote>
</blockquote>
````````````````````````````````

> > > foo
bar


```````````````````````````````` example
>>> foo
> bar
>>baz
.
<blockquote>
<blockquote>
<blockquote>
<p>foo
bar
baz</p>
</blockquote>
</blockquote>
</blockquote>
````````````````````````````````

>>> foo
> bar
>>baz

## List items

For example, let *Ls* be the lines

```````````````````````````````` example
A paragraph
with two lines.

    indented code

> A block quote.
.
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
````````````````````````````````

A paragraph
with two lines.

    indented code

> A block quote.


And let *M* be the marker `1.`, and *N* = 2.  Then rule #1 says
that the following is an ordered list item with start number 1,
and the same contents as *Ls*:

```````````````````````````````` example
1.  A paragraph
    with two lines.

        indented code

    > A block quote.
.
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>
````````````````````````````````

1.  A paragraph
    with two lines.

        indented code

    > A block quote.


The most important thing to notice is that the position of
the text after the list marker determines how much indentation
is needed in subsequent blocks in the list item.  If the list
marker takes up two spaces, and there are three spaces between
the list marker and the next [non-whitespace character], then blocks
must be indented five spaces in order to fall under the list
item.

Here are some examples showing how far content must be indented to be
put under the list item:

```````````````````````````````` example
- one

 two
.
<ul>
<li>one</li>
</ul>
<p>two</p>
````````````````````````````````

- one

 two


```````````````````````````````` example
- one

  two
.
<ul>
<li>
<p>one</p>
<p>two</p>
</li>
</ul>
````````````````````````````````

- one

  two


```````````````````````````````` example
 -    one

     two
.
<ul>
<li>one</li>
</ul>
<pre><code> two
</code></pre>
````````````````````````````````

 -    one

     two


```````````````````````````````` example
 -    one

      two
.
<ul>
<li>
<p>one</p>
<p>two</p>
</li>
</ul>
````````````````````````````````

 -    one

      two


It is tempting to think of this in terms of columns:  the continuation
blocks must be indented at least to the column of the first
[non-whitespace character] after the list marker. However, that is not quite right.
The spaces after the list marker determine how much relative indentation
is needed.  Which column this indentation reaches will depend on
how the list item is embedded in other constructions, as shown by
this example:

```````````````````````````````` example
   > > 1.  one
>>
>>     two
.
<blockquote>
<blockquote>
<ol>
<li>
<p>one</p>
<p>two</p>
</li>
</ol>
</blockquote>
</blockquote>
````````````````````````````````

   > > 1.  one
>>
>>     two


Here `two` occurs in the same column as the list marker `1.`,
but is actually contained in the list item, because there is
sufficient indentation after the last containing blockquote marker.

The converse is also possible.  In the following example, the word `two`
occurs far to the right of the initial text of the list item, `one`, but
it is not considered part of the list item, because it is not indented
far enough past the blockquote marker:

```````````````````````````````` example
>>- one
>>
  >  > two
.
<blockquote>
<blockquote>
<ul>
<li>one</li>
</ul>
<p>two</p>
</blockquote>
</blockquote>
````````````````````````````````

>>- one
>>
  >  > two


Note that at least one space is needed between the list marker and
any following content, so these are not list items:

```````````````````````````````` example
-one

2.two
.
<p>-one</p>
<p>2.two</p>
````````````````````````````````

-one

2.two


A list item may contain blocks that are separated by more than
one blank line.

```````````````````````````````` example
- foo


  bar
.
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>
````````````````````````````````

- foo


  bar


A list item may contain any kind of block:

```````````````````````````````` example
1.  foo

    ```
    bar
    ```

    baz

    > bam
.
<ol>
<li>
<p>foo</p>
<pre><code>bar
</code></pre>
<p>baz</p>
<blockquote>
<p>bam</p>
</blockquote>
</li>
</ol>
````````````````````````````````

1.  foo

    ```
    bar
    ```

    baz

    > bam


Note that ordered list start numbers must be nine digits or less:

```````````````````````````````` example
123456789. ok
.
<ol start="123456789">
<li>ok</li>
</ol>
````````````````````````````````

123456789. ok


```````````````````````````````` example
1234567890. not ok
.
<p>1234567890. not ok</p>
````````````````````````````````

1234567890. not ok


A start number may begin with 0s:

```````````````````````````````` example
0. ok
.
<ol start="0">
<li>ok</li>
</ol>
````````````````````````````````

0. ok


```````````````````````````````` example
003. ok
.
<ol start="3">
<li>ok</li>
</ol>
````````````````````````````````

003. ok


A start number may not be negative:

```````````````````````````````` example
-1. not ok
.
<p>-1. not ok</p>
````````````````````````````````

-1. not ok



Note that rules #1 only applies to cases in which the lines to be included in a
list item begin with a [non-whitespace character].  In a case like the
following, where the first block begins with a three-space indent, the rules do
not allow us to form a list item by indenting the whole thing and prepending a
list marker:

```````````````````````````````` example
   foo

bar
.
<p>foo</p>
<p>bar</p>
````````````````````````````````

   foo

bar


```````````````````````````````` example
-    foo

  bar
.
<ul>
<li>foo</li>
</ul>
<p>bar</p>
````````````````````````````````

-    foo

  bar


This is not a significant restriction, because when a block begins
with 1-3 spaces indent, the indentation can always be removed without
a change in interpretation, allowing rule #1 to be applied.  So, in
the above case:

```````````````````````````````` example
-  foo

   bar
.
<ul>
<li>
<p>foo</p>
<p>bar</p>
</li>
</ul>
````````````````````````````````

-  foo

   bar

Here are some list items that start with a blank line but are not empty:

```````````````````````````````` example
-
  foo
-
  ```
  bar
  ```
-
      baz
.
<ul>
<li>foo</li>
<li>
<pre><code>bar
</code></pre>
</li>
<li>
<pre><code>baz
</code></pre>
</li>
</ul>
````````````````````````````````

-
  foo
-
  ```
  bar
  ```
-
      baz

When the list item starts with a blank line, the number of spaces
following the list marker doesn't change the required indentation:

```````````````````````````````` example
-
  foo
.
<ul>
<li>foo</li>
</ul>
````````````````````````````````

-
  foo


A list item can begin with at most one blank line.
In the following example, `foo` is not part of the list
item:

```````````````````````````````` example
-

  foo
.
<ul>
<li></li>
</ul>
<p>foo</p>
````````````````````````````````

-

  foo


Here is an empty bullet list item:

```````````````````````````````` example
- foo
-
- bar
.
<ul>
<li>foo</li>
<li></li>
<li>bar</li>
</ul>
````````````````````````````````

- foo
-
- bar


It does not matter whether there are spaces following the [list marker]:

```````````````````````````````` example
- foo
-
- bar
.
<ul>
<li>foo</li>
<li></li>
<li>bar</li>
</ul>
````````````````````````````````

- foo
-
- bar


Here is an empty ordered list item:

```````````````````````````````` example
1. foo
2.
3. bar
.
<ol>
<li>foo</li>
<li></li>
<li>bar</li>
</ol>
````````````````````````````````

1. foo
2.
3. bar


A list may start or end with an empty list item:

```````````````````````````````` example
*
.
<ul>
<li></li>
</ul>
````````````````````````````````

*

However, an empty list item cannot interrupt a paragraph:

```````````````````````````````` example
foo
*

foo
1.
.
<p>foo
*</p>
<p>foo
1.</p>
````````````````````````````````

foo
*

foo
1.


Here is an example with [lazy continuation lines]:

```````````````````````````````` example
  1.  A paragraph
with two lines.

      ```
      indented code
      ```

      > A block quote.
.
<ol>
<li>
<p>A paragraph
with two lines.</p>
<pre><code>indented code
</code></pre>
<blockquote>
<p>A block quote.</p>
</blockquote>
</li>
</ol>
````````````````````````````````

  1.  A paragraph
with two lines.

      ```
      indented code
      ```

      > A block quote.


Indentation can be partially deleted:

```````````````````````````````` example
  1.  A paragraph
    with two lines.
.
<ol>
<li>A paragraph
with two lines.</li>
</ol>
````````````````````````````````

  1.  A paragraph
    with two lines.


These examples show how laziness can work in nested structures:

```````````````````````````````` example
> 1. > Blockquote
continued here.
.
<blockquote>
<ol>
<li>
<blockquote>
<p>Blockquote
continued here.</p>
</blockquote>
</li>
</ol>
</blockquote>
````````````````````````````````

> 1. > Blockquote
continued here.


```````````````````````````````` example
> 1. > Blockquote
> continued here.
.
<blockquote>
<ol>
<li>
<blockquote>
<p>Blockquote
continued here.</p>
</blockquote>
</li>
</ol>
</blockquote>
````````````````````````````````

> 1. > Blockquote
> continued here.



So, in this case we need two spaces indent:

```````````````````````````````` example
- foo
  - bar
    - baz
      - boo
.
<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>baz
<ul>
<li>boo</li>
</ul>
</li>
</ul>
</li>
</ul>
</li>
</ul>
````````````````````````````````

- foo
  - bar
    - baz
      - boo


One is not enough:

```````````````````````````````` example
- foo
 - bar
  - baz
   - boo
.
<ul>
<li>foo</li>
<li>bar</li>
<li>baz</li>
<li>boo</li>
</ul>
````````````````````````````````

- foo
 - bar
  - baz
   - boo


Here we need four, because the list marker is wider:

```````````````````````````````` example
10) foo
    - bar
.
<ol start="10">
<li>foo
<ul>
<li>bar</li>
</ul>
</li>
</ol>
````````````````````````````````

10) foo
    - bar


Three is not enough:

```````````````````````````````` example
10) foo
   - bar
.
<ol start="10">
<li>foo</li>
</ol>
<ul>
<li>bar</li>
</ul>
````````````````````````````````

10) foo
   - bar


A list may be the first block in a list item:

```````````````````````````````` example
- - foo
.
<ul>
<li>
<ul>
<li>foo</li>
</ul>
</li>
</ul>
````````````````````````````````

- - foo


```````````````````````````````` example
1. - 2. foo
.
<ol>
<li>
<ul>
<li>
<ol start="2">
<li>foo</li>
</ol>
</li>
</ul>
</li>
</ol>
````````````````````````````````

1. - 2. foo


A list item can contain a heading:

```````````````````````````````` example
- # Foo
- Bar
  ---
  baz
.
<ul>
<li>
<h1>Foo</h1>
</li>
<li>
<h2>Bar</h2>
baz</li>
</ul>
````````````````````````````````

- # Foo
- Bar
  ---
  baz


## Lists

Changing the bullet or ordered list delimiter starts a new list:

```````````````````````````````` example
- foo
- bar
+ baz
.
<ul>
<li>foo</li>
<li>bar</li>
</ul>
<ul>
<li>baz</li>
</ul>
````````````````````````````````

- foo
- bar
+ baz


```````````````````````````````` example
1. foo
2. bar
3) baz
.
<ol>
<li>foo</li>
<li>bar</li>
</ol>
<ol start="3">
<li>baz</li>
</ol>
````````````````````````````````

1. foo
2. bar
3) baz


A list can interrupt a paragraph. That is,
no blank line is needed to separate a paragraph from a following
list:

```````````````````````````````` example
Foo
- bar
- baz
.
<p>Foo</p>
<ul>
<li>bar</li>
<li>baz</li>
</ul>
````````````````````````````````

Foo
- bar
- baz

`Markdown.pl` does not allow this, through fear of triggering a list
via a numeral in a hard-wrapped line:

``` markdown
The number of windows in my house is
14.  The number of doors is 6.
```

Oddly, though, `Markdown.pl` *does* allow a blockquote to
interrupt a paragraph, even though the same considerations might
apply.

In CommonMark, we do allow lists to interrupt paragraphs, for
two reasons.  First, it is natural and not uncommon for people
to start lists without blank lines:

``` markdown
I need to buy
- new shoes
- a coat
- a plane ticket
```

Second, we are attracted to a

> [principle of uniformity]<@>:
> if a chunk of text has a certain
> meaning, it will continue to have the same meaning when put into a
> container block (such as a list item or blockquote).

(Indeed, the spec for [list items] and [block quotes] presupposes
this principle.) This principle implies that if

``` markdown
  * I need to buy
    - new shoes
    - a coat
    - a plane ticket
```

is a list item containing a paragraph followed by a nested sublist,
as all Markdown implementations agree it is (though the paragraph
may be rendered without `<p>` tags, since the list is "tight"),
then

``` markdown
I need to buy
- new shoes
- a coat
- a plane ticket
```

by itself should be a paragraph followed by a nested sublist.

Since it is well established Markdown practice to allow lists to
interrupt paragraphs inside list items, the [principle of
uniformity] requires us to allow this outside list items as
well.  ([reStructuredText](http://docutils.sourceforge.net/rst.html)
takes a different approach, requiring blank lines before lists
even inside other list items.)

In order to solve of unwanted lists in paragraphs with
hard-wrapped numerals, we allow only lists starting with `1` to
interrupt paragraphs.  Thus,

```````````````````````````````` example
The number of windows in my house is
14.  The number of doors is 6.
.
<p>The number of windows in my house is
14.  The number of doors is 6.</p>
````````````````````````````````

The number of windows in my house is
14.  The number of doors is 6.

We may still get an unintended result in cases like

```````````````````````````````` example
The number of windows in my house is
1.  The number of doors is 6.
.
<p>The number of windows in my house is</p>
<ol>
<li>The number of doors is 6.</li>
</ol>
````````````````````````````````

The number of windows in my house is
1.  The number of doors is 6.

but this rule should prevent most spurious list captures.

There can be any number of blank lines between items:

```````````````````````````````` example
- foo

- bar


- baz
.
<ul>
<li>
<p>foo</p>
</li>
<li>
<p>bar</p>
</li>
<li>
<p>baz</p>
</li>
</ul>
````````````````````````````````

- foo

- bar


- baz

```````````````````````````````` example
- foo
  - bar
    - baz


      bim
.
<ul>
<li>foo
<ul>
<li>bar
<ul>
<li>
<p>baz</p>
<p>bim</p>
</li>
</ul>
</li>
</ul>
</li>
</ul>
````````````````````````````````

- foo
  - bar
    - baz


      bim


To separate consecutive lists of the same type, or to separate a
list from an indented code block that would otherwise be parsed
as a subparagraph of the final list item, you can insert a blank HTML
comment:

```````````````````````````````` example
- foo
- bar

<!-- -->

- baz
- bim
.
<ul>
<li>foo</li>
<li>bar</li>
</ul>
<!-- -->
<ul>
<li>baz</li>
<li>bim</li>
</ul>
````````````````````````````````

- foo
- bar

<!-- -->

- baz
- bim


This is a loose list, because there is a blank line between
two of the list items:

```````````````````````````````` example
- a
- b

- c
.
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>c</p>
</li>
</ul>
````````````````````````````````

- a
- b

- c


So is this, with a empty second item:

```````````````````````````````` example
* a
*

* c
.
<ul>
<li>
<p>a</p>
</li>
<li></li>
<li>
<p>c</p>
</li>
</ul>
````````````````````````````````

* a
*

* c


These are loose lists, even though there is no space between the items,
because one of the items directly contains two block-level elements
with a blank line between them:

```````````````````````````````` example
- a
- b

  c
- d
.
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
<p>c</p>
</li>
<li>
<p>d</p>
</li>
</ul>
````````````````````````````````

- a
- b

  c
- d


```````````````````````````````` example
- a
- b

  [ref]: /url
- d
.
<ul>
<li>
<p>a</p>
</li>
<li>
<p>b</p>
</li>
<li>
<p>d</p>
</li>
</ul>
````````````````````````````````

- a
- b

  [ref]: /url
- d


This is a tight list, because the blank lines are in a code block:

```````````````````````````````` example
- a
- ```
  b


  ```
- c
.
<ul>
<li>a</li>
<li>
<pre><code>b


</code></pre>
</li>
<li>c</li>
</ul>
````````````````````````````````

- a
- ```
  b


  ```
- c


This is a tight list, because the blank line is between two
paragraphs of a sublist.  So the sublist is loose while
the outer list is tight:

```````````````````````````````` example
- a
  - b

    c
- d
.
<ul>
<li>a
<ul>
<li>
<p>b</p>
<p>c</p>
</li>
</ul>
</li>
<li>d</li>
</ul>
````````````````````````````````

- a
  - b

    c
- d


This is a tight list, because the blank line is inside the
block quote:

```````````````````````````````` example
* a
  > b
  >
* c
.
<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
</li>
<li>c</li>
</ul>
````````````````````````````````

* a
  > b
  >
* c


This list is tight, because the consecutive block elements
are not separated by blank lines:

```````````````````````````````` example
- a
  > b
  ```
  c
  ```
- d
.
<ul>
<li>a
<blockquote>
<p>b</p>
</blockquote>
<pre><code>c
</code></pre>
</li>
<li>d</li>
</ul>
````````````````````````````````

- a
  > b
  ```
  c
  ```
- d


A single-paragraph list is tight:

```````````````````````````````` example
- a
.
<ul>
<li>a</li>
</ul>
````````````````````````````````

- a


```````````````````````````````` example
- a
  - b
.
<ul>
<li>a
<ul>
<li>b</li>
</ul>
</li>
</ul>
````````````````````````````````

- a
  - b


This list is loose, because of the blank line between the
two block elements in the list item:

```````````````````````````````` example
1. ```
   foo
   ```

   bar
.
<ol>
<li>
<pre><code>foo
</code></pre>
<p>bar</p>
</li>
</ol>
````````````````````````````````

1. ```
   foo
   ```

   bar


Here the outer list is loose, the inner list tight:

```````````````````````````````` example
* foo
  * bar

  baz
.
<ul>
<li>
<p>foo</p>
<ul>
<li>bar</li>
</ul>
<p>baz</p>
</li>
</ul>
````````````````````````````````

* foo
  * bar

  baz


```````````````````````````````` example
- a
  - b
  - c

- d
  - e
  - f
.
<ul>
<li>
<p>a</p>
<ul>
<li>b</li>
<li>c</li>
</ul>
</li>
<li>
<p>d</p>
<ul>
<li>e</li>
<li>f</li>
</ul>
</li>
</ul>
````````````````````````````````

- a
  - b
  - c

- d
  - e
  - f

## Inline

```````````````````````````````` example
`hi`lo`
.
<p><code>hi</code>lo`</p>
````````````````````````````````

`hi`lo`


`hi` is parsed as code, leaving the backtick at the end as a literal
backtick.

## Backslash escapes

Any ASCII punctuation character may be backslash-escaped:

```````````````````````````````` example
\!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~
.
<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>
````````````````````````````````

\!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~


Backslashes before other characters are treated as literal
backslashes:

```````````````````````````````` example
\→\A\a\ \3\φ\«
.
<p>\→\A\a\ \3\φ\«</p>
````````````````````````````````

\→\A\a\ \3\φ\«


Escaped characters are treated as regular characters and do
not have their usual Markdown meanings:

```````````````````````````````` example
\*not emphasized*
\<br/> not a tag
\[not a link](/foo)
\`not code`
1\. not a list
\* not a list
\# not a heading
\[foo]: /url "not a reference"
.
<p>*not emphasized*
&lt;br/&gt; not a tag
[not a link](/foo)
`not code`
1. not a list
* not a list
# not a heading
[foo]: /url &quot;not a reference&quot;</p>
````````````````````````````````

\*not emphasized*
\<br/> not a tag
\[not a link](/foo)
\`not code`
1\. not a list
\* not a list
\# not a heading
\[foo]: /url "not a reference"


If a backslash is itself escaped, the following character is not:

```````````````````````````````` example
\\*emphasis*
.
<p>\<em>emphasis</em></p>
````````````````````````````````

\\*emphasis*


A backslash at the end of the line is a [hard line break]:

```````````````````````````````` example
foo\
bar
.
<p>foo<br />
bar</p>
````````````````````````````````

foo\
bar


Backslash escapes do not work in code blocks, code spans, autolinks, or
raw HTML:

```````````````````````````````` example
`` \[\` ``
.
<p><code>\[\`</code></p>
````````````````````````````````

`` \[\` ``


```````````````````````````````` example
~~~
\[\]
~~~
.
<pre><code>\[\]
</code></pre>
````````````````````````````````

~~~
\[\]
~~~


```````````````````````````````` example
<http://example.com?find=\*>
.
<p><a href="http://example.com?find=%5C*">http://example.com?find=\*</a></p>
````````````````````````````````

<http://example.com?find=\*>


```````````````````````````````` example
<a href="/bar\/)">
.
<a href="/bar\/)">
````````````````````````````````

<a href="/bar\/)">


But they work in all other contexts, including URLs and link titles,
link references, and [info strings] in [fenced code blocks]:

```````````````````````````````` example
[foo](/bar\* "ti\*tle")
.
<p><a href="/bar*" title="ti*tle">foo</a></p>
````````````````````````````````

[foo](/bar\* "ti\*tle")


```````````````````````````````` example
[foo]

[foo]: /bar\* "ti\*tle"
.
<p><a href="/bar*" title="ti*tle">foo</a></p>
````````````````````````````````

[foo]

[foo]: /bar\* "ti\*tle"


```````````````````````````````` example
``` foo\+bar
foo
```
.
<pre><code class="language-foo+bar">foo
</code></pre>
````````````````````````````````

``` foo\+bar

## Character references

```````````````````````````````` example
&nbsp; &amp; &copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;
.
<p>  &amp; © Æ Ď
¾ ℋ ⅆ
∲ ≧̸</p>
````````````````````````````````

&nbsp; &amp; &copy; &AElig; &Dcaron;
&frac34; &HilbertSpace; &DifferentialD;
&ClockwiseContourIntegral; &ngE;


[Decimal numeric character
references]<@>
consist of `&#` + a string of 1--8 arabic digits + `;`. A
numeric character reference is parsed as the corresponding
Unicode character. Invalid Unicode code points will be replaced by
the REPLACEMENT CHARACTER (`U+FFFD`).  For security reasons,
the code point `U+0000` will also be replaced by `U+FFFD`.

```````````````````````````````` example
&#35; &#1234; &#992; &#98765432; &#0;
.
<p># Ӓ Ϡ � �</p>
````````````````````````````````

&#35; &#1234; &#992; &#98765432; &#0;


[Hexadecimal numeric character
references]<@> consist of `&#` +
either `X` or `x` + a string of 1-8 hexadecimal digits + `;`.
They too are parsed as the corresponding Unicode character (this
time specified with a hexadecimal numeral instead of decimal).

```````````````````````````````` example
&#X22; &#XD06; &#xcab;
.
<p>&quot; ആ ಫ</p>
````````````````````````````````

&#X22; &#XD06; &#xcab;


Here are some nonentities:

```````````````````````````````` example
&nbsp &x; &#; &#x;
&ThisIsNotDefined; &hi?;
.
<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;
&amp;ThisIsNotDefined; &amp;hi?;</p>
````````````````````````````````

&nbsp &x; &#; &#x;
&ThisIsNotDefined; &hi?;


Although HTML5 does accept some entity references
without a trailing semicolon (such as `&copy`), these are not
recognized here, because it makes the grammar too ambiguous:

```````````````````````````````` example
&copy
.
<p>&amp;copy</p>
````````````````````````````````

&copy


Strings that are not on the list of HTML5 named entities are not
recognized as entity references either:

```````````````````````````````` example
&MadeUpEntity;
.
<p>&amp;MadeUpEntity;</p>
````````````````````````````````

&MadeUpEntity;


Entity and numeric character references are recognized in any
context besides code spans or code blocks, including
URLs, [link titles], and [fenced code block][] [info strings]:

```````````````````````````````` example
<a href="&ouml;&ouml;.html">
.
<a href="&ouml;&ouml;.html">
````````````````````````````````

<a href="&ouml;&ouml;.html">


```````````````````````````````` example
[foo](/f&ouml;&ouml; "f&ouml;&ouml;")
.
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>
````````````````````````````````

[foo](/f&ouml;&ouml; "f&ouml;&ouml;")


```````````````````````````````` example
[foo]

[foo]: /f&ouml;&ouml; "f&ouml;&ouml;"
.
<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>
````````````````````````````````

[foo]

[foo]: /f&ouml;&ouml; "f&ouml;&ouml;"


```````````````````````````````` example
``` f&ouml;&ouml;
foo
```
.
<pre><code class="language-föö">foo
</code></pre>
````````````````````````````````

``` f&ouml;&ouml;
foo
```


Entity and numeric character references are treated as literal
text in code spans and code blocks:

```````````````````````````````` example
`f&ouml;&ouml;`
.
<p><code>f&amp;ouml;&amp;ouml;</code></p>
````````````````````````````````

`f&ouml;&ouml;`

## Code spans

TODO: Need to fix these examples
This is a simple code span:

```````````````````````````````` example
`foo`
.
<p><code>foo</code></p>
````````````````````````````````

`foo`


Here two backticks are used, because the code contains a backtick.
This example also illustrates stripping of leading and trailing spaces:

```````````````````````````````` example
`` foo ` bar  ``
.
<p><code>foo ` bar</code></p>
````````````````````````````````

`` foo ` bar  ``


This example shows the motivation for stripping leading and trailing
spaces:

```````````````````````````````` example
` `` `
.
<p><code>``</code></p>
````````````````````````````````

` `` `


[Line endings] are treated like spaces:

```````````````````````````````` example
``
foo
``
.
<p><code>foo</code></p>
````````````````````````````````

``
foo
``


Interior spaces and [line endings] are collapsed into
single spaces, just as they would be by a browser:

```````````````````````````````` example
`foo   bar
  baz`
.
<p><code>foo bar baz</code></p>
````````````````````````````````

`foo   bar
  baz`


Not all [Unicode whitespace] (for instance, non-breaking space) is
collapsed, however:

```````````````````````````````` example
`a  b`
.
<p><code>a  b</code></p>
````````````````````````````````

`a  b`


Q: Why not just leave the spaces, since browsers will collapse them
anyway?  A:  Because we might be targeting a non-HTML format, and we
shouldn't rely on HTML-specific rendering assumptions.

(Existing implementations differ in their treatment of internal
spaces and [line endings].  Some, including `Markdown.pl` and
`showdown`, convert an internal [line ending] into a
`<br />` tag.  But this makes things difficult for those who like to
hard-wrap their paragraphs, since a line break in the midst of a code
span will cause an unintended line break in the output.  Others just
leave internal spaces as they are, which is fine if only HTML is being
targeted.)

```````````````````````````````` example
`foo `` bar`
.
<p><code>foo `` bar</code></p>
````````````````````````````````

`foo `` bar`


Note that backslash escapes do not work in code spans. All backslashes
are treated literally:

```````````````````````````````` example
`foo\`bar`
.
<p><code>foo\</code>bar`</p>
````````````````````````````````

`foo\`bar`


Backslash escapes are never needed, because one can always choose a
string of *n* backtick characters as delimiters, where the code does
not contain any strings of exactly *n* backtick characters.

Code span backticks have higher precedence than any other inline
constructs except HTML tags and autolinks.  Thus, for example, this is
not parsed as emphasized text, since the second `*` is part of a code
span:

```````````````````````````````` example
*foo`*`
.
<p>*foo<code>*</code></p>
````````````````````````````````

*foo`*`


And this is not parsed as a link:

```````````````````````````````` example
[not a `link](/foo`)
.
<p>[not a <code>link](/foo</code>)</p>
````````````````````````````````

[not a `link](/foo`)


Code spans, HTML tags, and autolinks have the same precedence.
Thus, this is code:

```````````````````````````````` example
`<a href="`">`
.
<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>
````````````````````````````````

`<a href="`">`


But this is an HTML tag:

```````````````````````````````` example
<a href="`">`
.
<p><a href="`">`</p>
````````````````````````````````

<a href="`">`


And this is code:

```````````````````````````````` example
`<http://foo.bar.`baz>`
.
<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>
````````````````````````````````

`<http://foo.bar.`baz>`


But this is an autolink:

```````````````````````````````` example
<http://foo.bar.`baz>`
.
<p><a href="http://foo.bar.%60baz">http://foo.bar.`baz</a>`</p>
````````````````````````````````

<http://foo.bar.`baz>`


(Note that this is different than CommonMark)
When a backtick string is not closed by a matching backtick string,
then the code section ends at the end of the current block, and the wrong-number
backtick string that follows is converted as-is.

```````````````````````````````` example
```foo``
.
<p><code>foo``</code></p>
````````````````````````````````

```foo``


```````````````````````````````` example
`foo
.
<p><code>foo</code></p>
````````````````````````````````
`foo

## Styled Spans

TODO: Need to fix these examples, as they don't show the correct operations.

Here are some examples of delimiter runs.

  - left-flanking but not right-flanking:

    ```
    ***abc
      _abc
    **"abc"
     _"abc"
    ```

  - right-flanking but not left-flanking:

    ```
     abc***
     abc_
    "abc"**
    "abc"_
    ```

  - Both left and right-flanking:

    ```
     abc***def
    "abc"_"def"
    ```

  - Neither left nor right-flanking:

    ```
    abc *** def
    a _ b
    ```

The following rules define emphasis and strong emphasis:

1.  A [left-flanking delimiter run] of `*` or `_` will open emphasis
    if emphasis is not already opened by the same character.

2.  A [right-flanking delimiter run] of `*` or `_` will close emphasis
    if emphasis was opened by the same character.

3.  A [left-flanking delimiter run] of `**` or `__` will open strong emphasis
    if strong emphasis is not already opened by a delimiter run of the
    same characters.

4.  A [right-flanking delimiter run] of `**` or `__` will close strong emphasis
    if strong emphasis was opened by delimiter run of the
    same characters.

5.  A [left-flanking delimiter run] of `***` or `___` will open emphasis if
    emphasis is not already opened by the same character *and* open strong
    emphasis if strong emphasis is not already opened by the same character.
    If strong emphasis can not be opened, the first two characters will be
    parsed as plain text. If emphasis can not be opened, the last character will
    be parsed as plain text.

6.  A [right-flanking delimiter run] of `***` or `___` will close emphasis
    if emphasis was opened by the same character *and* close strong emphasis
    if strong emphasis was opened by the same character. If strong emphasis can
    not be closed, the last two characters will be parsed as plain text. If
    emphasis can not be closed, the first character will be parsed as plain
    text. The first one to be closed is going to be the last one opened.

7.  A [delimiter run] longer than 3 characters will behave as a delimiter
    run of three characters. If the delimiter run is left-flanking, the
    characters after the third will be parsed as plain text. If the delimiter
    run is right-flanking, all characters except the last three will be parsed
    as plain text.

8.  If emphasis or strong emphasis remains open at the end of a block, it will
    be closed at the end of the block.

9.  (TODO: This might be a general rule for all inline content) When an emphasis
    or strong emphasis span overlaps with another inline span, even if it is
    another of the same kind, the first can not be closed until
    the second is closed. Thus, for example, `*foo _bar* baz_` is parsed as
    `<em>foo <em>bar* baz</em>` rather than `<em>foo <em>bar</em> baz</em>`.
    And `*[foo*](bar)` is parsed as `<em><a href="bar">foo*</a>` rather than as
    `<em><a href="bar">foo</em></a>`.

10. When emphasis and strong emphasis was initiated by a three-character
    delimiter run, and each was closed by separate delimiter runs, the regular
    emphasis must be closed before the strong emphasis.

These rules can be illustrated through a series of examples.


```````````````````````````````` example
*foo bar*
.
<p><em>foo bar</em></p>
````````````````````````````````

*foo bar*


This is not emphasis, because the opening `*` is followed by
whitespace, and hence not part of a [left-flanking delimiter run]:

```````````````````````````````` example
a * foo bar*
.
<p>a * foo bar*</p>
````````````````````````````````

a * foo bar*


Unicode nonbreaking spaces count as whitespace, too:

```````````````````````````````` example
* a *
.
<p>* a *</p>
````````````````````````````````

* a *


Intraword emphasis with `*` is permitted:

```````````````````````````````` example
foo*bar*
.
<p>foo<em>bar</em></p>
````````````````````````````````

foo*bar*


```````````````````````````````` example
5*6*78
.
<p>5<em>6</em>78</p>
````````````````````````````````

5*6*78


```````````````````````````````` example
_foo bar_
.
<p><em>foo bar</em></p>
````````````````````````````````

_foo bar_


This is not emphasis, because the opening `_` is followed by
whitespace:

```````````````````````````````` example
_ foo bar_
.
<p>_ foo bar_</p>
````````````````````````````````

_ foo bar_


This is not emphasis, because the closing delimiter does
not match the opening delimiter:

```````````````````````````````` example
_foo*
.
<p>_foo*</p>
````````````````````````````````

_foo*


This is not emphasis, because the closing `*` is preceded by
whitespace:

```````````````````````````````` example
*foo bar *
.
<p>*foo bar *</p>
````````````````````````````````

*foo bar *


A newline also counts as whitespace:

```````````````````````````````` example
*foo bar
*
.
<p>*foo bar
*</p>
````````````````````````````````

*foo bar
*


Here is some nested emphasis:

```````````````````````````````` example
*(_foo_)*
.
<p><em>(<em>foo</em>)</em></p>
````````````````````````````````

*(_foo_)*


Intraword emphasis with `*` is allowed:

```````````````````````````````` example
*foo*bar
.
<p><em>foo</em>bar</p>
````````````````````````````````

*foo*bar


This is not emphasis, because the closing `_` is preceded by
whitespace:

```````````````````````````````` example
_foo bar _
.
<p>_foo bar _</p>
````````````````````````````````

_foo bar _


This is emphasis within emphasis:

```````````````````````````````` example
_(*foo*)_
.
<p><em>(<em>foo</em>)</em></p>
````````````````````````````````

_(*foo*)_


```````````````````````````````` example
**foo bar**
.
<p><strong>foo bar</strong></p>
````````````````````````````````

**foo bar**


This is not strong emphasis, because the opening delimiter is
followed by whitespace:

```````````````````````````````` example
** foo bar**
.
<p>** foo bar**</p>
````````````````````````````````

** foo bar**


Intraword strong emphasis with `**` is permitted:

```````````````````````````````` example
foo**bar**
.
<p>foo<strong>bar</strong></p>
````````````````````````````````

foo**bar**



```````````````````````````````` example
__foo bar__
.
<p><strong>foo bar</strong></p>
````````````````````````````````

__foo bar__


This is not strong emphasis, because the opening delimiter is
followed by whitespace:

```````````````````````````````` example
__ foo bar__
.
<p>__ foo bar__</p>
````````````````````````````````

__ foo bar__


A newline counts as whitespace:
```````````````````````````````` example
__
foo bar__
.
<p>__
foo bar__</p>
````````````````````````````````

__
foo bar__


Delimiters must be different to nest:

```````````````````````````````` example
__foo, __bar__, baz__
.
<p><strong>foo, __bar__, baz</strong></p>
````````````````````````````````

__foo, __bar__, baz__


This is strong emphasis, even though the opening delimiter is
both left- and right-flanking, because it is preceded by
punctuation:

```````````````````````````````` example
foo-__(bar)__
.
<p>foo-<strong>(bar)</strong></p>
````````````````````````````````

foo-__(bar)__



This is not strong emphasis, because the closing delimiter is preceded
by whitespace:

```````````````````````````````` example
**foo bar **
.
<p>**foo bar **</p>
````````````````````````````````

**foo bar **


Nesting strong inside regular emphasis:

```````````````````````````````` example
*(**foo**)*
.
<p><em>(<strong>foo</strong>)</em></p>
````````````````````````````````

*(**foo**)*


```````````````````````````````` example
**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**
.
<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.
<em>Asclepias physocarpa</em>)</strong></p>
````````````````````````````````

**Gomphocarpus (*Gomphocarpus physocarpus*, syn.
*Asclepias physocarpa*)**


```````````````````````````````` example
**foo "*bar*" foo**
.
<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>
````````````````````````````````

**foo "*bar*" foo**


Intraword emphasis:

```````````````````````````````` example
**foo**bar
.
<p><strong>foo</strong>bar</p>
````````````````````````````````

**foo**bar


This is not strong emphasis, because the closing delimiter is
preceded by whitespace:

```````````````````````````````` example
__foo bar __
.
<p>__foo bar __</p>
````````````````````````````````

__foo bar __


Nesting strong and regular emphasis:

```````````````````````````````` example
_(__foo__)_
.
<p><em>(<strong>foo</strong>)</em></p>
````````````````````````````````

_(__foo__)_


Any nonempty sequence of inline elements can be the contents of an
emphasized span.

```````````````````````````````` example
*foo [bar](/url)*
.
<p><em>foo <a href="/url">bar</a></em></p>
````````````````````````````````

*foo [bar](/url)*


```````````````````````````````` example
*foo
bar*
.
<p><em>foo
bar</em></p>
````````````````````````````````

*foo
bar*


```````````````````````````````` example
*foo**bar**baz*
.
<p><em>foo<strong>bar</strong>baz</em></p>
````````````````````````````````

*foo**bar**baz*

Note that in the preceding case, the interpretation

``` markdown
<p><em>foo</em><em>bar<em></em>baz</em></p>
```

is precluded because a two-character delimiter can only
start or close strong emphasis.

The same condition ensures that the following
cases are all strong emphasis nested inside
emphasis, even when the interior spaces are
omitted:


```````````````````````````````` example
***foo** bar*
.
<p><em><strong>foo</strong> bar</em></p>
````````````````````````````````

***foo** bar*


```````````````````````````````` example
*foo **bar***
.
<p><em>foo <strong>bar</strong></em></p>
````````````````````````````````

*foo **bar***


```````````````````````````````` example
*foo**bar***
.
<p><em>foo<strong>bar</strong></em></p>
````````````````````````````````

*foo**bar***


There can be no empty emphasis or strong emphasis:

```````````````````````````````` example
** is not an empty emphasis
.
<p>** is not an empty emphasis</p>
````````````````````````````````

** is not an empty emphasis


```````````````````````````````` example
**** is not an empty strong emphasis
.
<p>**** is not an empty strong emphasis</p>
````````````````````````````````

**** is not an empty strong emphasis


Any nonempty sequence of inline elements can be the contents of an
strongly emphasized span.

```````````````````````````````` example
**foo [bar](/url)**
.
<p><strong>foo <a href="/url">bar</a></strong></p>
````````````````````````````````

**foo [bar](/url)**


```````````````````````````````` example
**foo
bar**
.
<p><strong>foo
bar</strong></p>
````````````````````````````````

**foo
bar**


Emphasis and strong emphasis can be nested
inside strong emphasis:

```````````````````````````````` example
__foo _bar_ baz__
.
<p><strong>foo <em>bar</em> baz</strong></p>
````````````````````````````````

__foo _bar_ baz__


```````````````````````````````` example
__foo **bar** baz__
.
<p><strong>foo <strong>bar</strong> baz</strong></p>
````````````````````````````````

__foo **bar** baz__


```````````````````````````````` example
__**foo** bar__
.
<p><strong><strong>foo</strong> bar</strong></p>
````````````````````````````````

__**foo** bar__


```````````````````````````````` example
**foo __bar__**
.
<p><strong>foo <strong>bar</strong></strong></p>
````````````````````````````````

**foo __bar__**


```````````````````````````````` example
**foo *bar* baz**
.
<p><strong>foo <em>bar</em> baz</strong></p>
````````````````````````````````

**foo *bar* baz**


```````````````````````````````` example
**foo*bar*baz**
.
<p><strong>foo<em>bar</em>baz</strong></p>
````````````````````````````````

**foo*bar*baz**


```````````````````````````````` example
***foo* bar**
.
<p><strong><em>foo</em> bar</strong></p>
````````````````````````````````

***foo* bar**


```````````````````````````````` example
**foo *bar***
.
<p><strong>foo <em>bar</em></strong></p>
````````````````````````````````

**foo *bar***


There can be no empty emphasis or strong emphasis:

```````````````````````````````` example
__ is not an empty emphasis
.
<p>__ is not an empty emphasis</p>
````````````````````````````````

__ is not an empty emphasis


```````````````````````````````` example
____ is not an empty strong emphasis
.
<p>____ is not an empty strong emphasis</p>
````````````````````````````````

____ is not an empty strong emphasis



```````````````````````````````` example
foo ***
.
<p>foo ***</p>
````````````````````````````````

foo ***


```````````````````````````````` example
foo *\**
.
<p>foo <em>*</em></p>
````````````````````````````````

foo *\**


This one is weird, but it should start two emphasises, but the second
asterisk can't close the first emphasis because the second hasn't been closed
yet. However, the end of the paragraph will close it.

```````````````````````````````` example
foo *_*
.
<p>foo <em><em>*</em></em></p>
````````````````````````````````

foo *_*


But this one won't do anything because it's not left-flanking or right-flanking.

```````````````````````````````` example
foo *****
.
<p>foo *****</p>
````````````````````````````````

foo *****


```````````````````````````````` example
foo **\***
.
<p>foo <strong>*</strong></p>
````````````````````````````````

foo **\***


```````````````````````````````` example
foo **_**
.
<p>foo <strong><em>**</em></strong></p>
````````````````````````````````

foo **_**


Note that when delimiters do not match evenly, things are closed automatically
at the end of the block, but some spare asterisks are left lying around:

```````````````````````````````` example
**foo*
.
<p><strong>foo*</strong></p>
````````````````````````````````

**foo*


```````````````````````````````` example
*foo**
.
<p><em>foo**</em></p>
````````````````````````````````

*foo**


```````````````````````````````` example
***foo**
.
<p><strong><em>foo**</em></strong></p>
````````````````````````````````

***foo**


```````````````````````````````` example
****foo*
.
<p><strong><em>*foo</em></strong></p>
````````````````````````````````

****foo*


```````````````````````````````` example
**foo***
.
<p><strong>foo*</strong></p>
````````````````````````````````

**foo***


```````````````````````````````` example
*foo****
.
<p><em>foo</em>***</p>
````````````````````````````````

*foo****



```````````````````````````````` example
foo ___
.
<p>foo ___</p>
````````````````````````````````

foo ___


```````````````````````````````` example
foo _\__
.
<p>foo <em>_</em></p>
````````````````````````````````

foo _\__


And some weird garbage, like above:

```````````````````````````````` example
foo _*_
.
<p>foo <em><em>_</em></em></p>
````````````````````````````````

foo _*_


```````````````````````````````` example
foo _____
.
<p>foo _____</p>
````````````````````````````````

foo _____


```````````````````````````````` example
foo __\___
.
<p>foo <strong>_</strong></p>
````````````````````````````````

foo __\___


```````````````````````````````` example
foo __*__
.
<p>foo <strong><em>__</em></strong></p>
````````````````````````````````

foo __*__


```````````````````````````````` example
__foo_
.
<p><strong>foo_</strong></p>
````````````````````````````````

__foo_


```````````````````````````````` example
_foo__
.
<p><em>foo__</em></p>
````````````````````````````````

_foo__


```````````````````````````````` example
___foo__
.
<p><strong></em>foo__</strong></p>
````````````````````````````````

___foo__


```````````````````````````````` example
____foo_
.
<p><strong><em>_foo</em></strong></p>
````````````````````````````````

____foo_


```````````````````````````````` example
__foo___
.
<p><strong>foo_</strong></p>
````````````````````````````````

__foo___


```````````````````````````````` example
_foo____
.
<p><em>foo</em>___</p>
````````````````````````````````

_foo____


If you want emphasis nested directly inside
emphasis, you must use different delimiters:

```````````````````````````````` example
**foo**
.
<p><strong>foo</strong></p>
````````````````````````````````

**foo**


```````````````````````````````` example
*_foo_*
.
<p><em><em>foo</em></em></p>
````````````````````````````````

*_foo_*


```````````````````````````````` example
__foo__
.
<p><strong>foo</strong></p>
````````````````````````````````

__foo__


```````````````````````````````` example
_*foo*_
.
<p><em><em>foo</em></em></p>
````````````````````````````````

_*foo*_


Same with strong emphasis:

```````````````````````````````` example
****foo****
.
<p><strong><em>*foo*</em></strong></p>
````````````````````````````````

****foo****


```````````````````````````````` example
____foo____
.
<p><strong><em>_foo_</em></strong></p>
````````````````````````````````

____foo____


```````````````````````````````` example
**__foo__**
.
<p><strong><strong>foo</strong></strong></p>
````````````````````````````````



Some long sequences of delimiters:

```````````````````````````````` example
******foo******
.
<p><strong><em>***foo***</em></strong></p>
````````````````````````````````

******foo******


```````````````````````````````` example
***foo***
.
<p><strong><em>foo</em></strong></p>
````````````````````````````````

***foo***


```````````````````````````````` example
_____foo_____
.
<p><strong><em>__foo__</em></strong></p>
````````````````````````````````

_____foo_____


```````````````````````````````` example
*foo _bar* baz_
.
<p><em>foo <em>bar* baz</em></p>
````````````````````````````````

*foo _bar* baz_


```````````````````````````````` example
*foo __bar *baz bim__ bam*
.
<p><em>foo <strong>bar *baz bim</strong> bam</em></p>
````````````````````````````````

*foo __bar *baz bim__ bam*


```````````````````````````````` example
**foo **bar baz**
.
<p><strong>foo **bar baz</strong></p>
````````````````````````````````

**foo **bar baz**


```````````````````````````````` example
*foo *bar baz*
.
<p><em>foo *bar baz</em></p>
````````````````````````````````

*foo *bar baz*


```````````````````````````````` example
*[bar*](/url)
.
<p><em><a href="/url">bar*</a></em></p>
````````````````````````````````

*[bar*](/url)


```````````````````````````````` example
_foo [bar_](/url)
.
<p><em>foo <a href="/url">bar_</a></em></p>
````````````````````````````````

_foo [bar_](/url)


```````````````````````````````` example
*a `*`*
.
<p><em>a <code>*</code></em></p>
````````````````````````````````

*a `*`*


```````````````````````````````` example
_a `_`_
.
<p><em>a <code>_</code></em></p>
````````````````````````````````

_a `_`_

## Link

Here is a simple inline link:

```````````````````````````````` example
[link]</uri>
.
<p><a href="/uri">link</a></p>
````````````````````````````````

[link]</uri>


The destination may be omitted:

```````````````````````````````` example
[link]<>
.
<p><a href="">link</a></p>
````````````````````````````````

[link]<>


The destination cannot contain spaces or line breaks,
even if enclosed in pointy brackets (NOTE: This
is treated as a shortcut reference link):

```````````````````````````````` example
[link]</my uri>
.
<p><a href="...">link</a>(/my uri)</p>
````````````````````````````````

[link]</my uri>


```````````````````````````````` example
[link]<foo
bar>
.
<p><a href="...">link</a>(foo
bar)</p>
````````````````````````````````

[link](foo
bar)

Brackets and other symbols can also be escaped, as usual
in Markdown:

```````````````````````````````` example
[link]<foo\>\:>
.
<p><a href="foo>:">link</a></p>
````````````````````````````````

[link]<foo\>\:>


A link can contain fragment identifiers and queries:

```````````````````````````````` example
[link]<#fragment>

[link]<http://example.com#fragment>

[link]<http://example.com?foo=3#frag>
.
<p><a href="#fragment">link</a></p>
<p><a href="http://example.com#fragment">link</a></p>
<p><a href="http://example.com?foo=3#frag">link</a></p>
````````````````````````````````

[link]<#fragment>

[link]<http://example.com#fragment>

[link]<http://example.com?foo=3#frag>


Note that a backslash before a non-escapable character is
just a backslash:

```````````````````````````````` example
[link]<foo\bar>
.
<p><a href="foo%5Cbar">link</a></p>
````````````````````````````````

[link]<foo\bar>


URL-escaping should be left alone inside the destination, as all
URL-escaped characters are also valid URL characters. Entity and
numerical character references in the destination will be parsed
into the corresponding Unicode code points, as usual.  These may
be optionally URL-escaped when written as HTML, but this spec
does not enforce any particular policy for rendering URLs in
HTML or other formats.  Renderers may make different decisions
about how to escape or normalize URLs in the output.

```````````````````````````````` example
[link]<foo%20b&auml;>
.
<p><a href="foo%20b%C3%A4">link</a></p>
````````````````````````````````

[link]<foo%20b&auml;>


Whitespace is not allowed between the link text and the
following parenthesis. Note that this creates an auto-link.

```````````````````````````````` example
[link] </uri>
.
<p><a href="...">link</a> <a href="/uri">/uri</a></p>
````````````````````````````````

[link] </uri>


The link text may not contain closing brackets,
unless they are escaped:

```````````````````````````````` example
[link [foo [bar]]]</uri>
.
<p><a href="...">link [foo [bar</a>]]<a href="/uri">/uri</a></p>
````````````````````````````````

[link [foo [bar]]]</uri>


```````````````````````````````` example
[link] bar]</uri>
.
<p><a href="...">link</a> bar]<a href="/uri">/uri</a></p>
````````````````````````````````

[link] bar]</uri>


```````````````````````````````` example
[link [bar]</uri>
.
<p><a href="/uri">link [bar</a></p>
````````````````````````````````

[link [bar]</uri>


```````````````````````````````` example
[link \[bar\]</uri>
.
<p><a href="/uri">link [bar]</a></p>
````````````````````````````````

[link \[bar\]</uri>


The link text may contain inline content:

```````````````````````````````` example
[link *foo **bar** `#`*]</uri>
.
<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>
````````````````````````````````

[link *foo **bar** `#`*]</uri>


```````````````````````````````` example
[![moon]<moon.jpg>]</uri>
.
<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>
````````````````````````````````

[![moon]<moon.jpg>]</uri>


However, links may not contain other links, at any level of nesting.

```````````````````````````````` example
[foo [bar]</uri>]</uri>
.
<p><a href="/uri">foo [bar</a>]<a href="/uri">/uri</a></p>
````````````````````````````````

[foo [bar]</uri>]</uri>


```````````````````````````````` example
[foo *[bar [baz]</uri>]</uri>*]</uri>
.
<p><a href="/uri">[foo *[bar [baz</a>]<a href="/uri">/uri</a>*]<a href="/uri">/uri</a></p>
````````````````````````````````

[foo *[bar [baz]</uri>]</uri>*]</uri>


This example probably belongs in images:

```````````````````````````````` example
![[[foo]<uri1>]<uri2>]<uri3>
.
<p><img src="uri1" alt="[[foo"/>]<a href="uri2">uri2</a>]<a href="uri3">uri3</a></p>
````````````````````````````````

![[[foo]<uri1>]<uri2>]<uri3>


These cases illustrate what happens when mixing link text and emphasis:

```````````````````````````````` example
*[foo*]</uri>
.
<p><em><a href="/uri">foo*</a></em></p>
````````````````````````````````

*[foo*]</uri>


```````````````````````````````` example
[foo *bar]<baz*>
.
<p><a href="...">foo <em>bar]&lt;baz*&gt;</a></p>
````````````````````````````````

[foo *bar]<baz*>


```````````````````````````````` example
*foo [bar* baz]
.
<p><em>foo <a href="...">bar* baz</a></em></p>
````````````````````````````````


These cases illustrate the combination with code spans, and autolinks with
link grouping:

```````````````````````````````` example
[foo <bar attr="]<baz>">
.
<p><a href="<baz>">foo &lt;bar attr="</a>"&gt;</p>
````````````````````````````````

[foo <bar attr="]<baz>">


```````````````````````````````` example
[foo`]</uri>`
.
<p><a href="...">foo<code>]&lt;/uri&gt;</code></a></p>
````````````````````````````````

[foo`]</uri>`


```````````````````````````````` example
[foo<http://example.com/?search=]<uri>>
.
<p><a href="uri">foo<http://example.com/?search=</a>&gt;</p>
````````````````````````````````

Here is a simple example:

```````````````````````````````` example
[foo][bar]

[bar]: /url
.
<p><a href="/url">foo</a></p>
````````````````````````````````

[foo][bar]



The rules for the [link text] are the same as with
[inline links].  Thus:

The link text may contain opening brackets, but not closing ones,
unless they are escaped:

```````````````````````````````` example
[link [foo [bar]]][ref]

[ref]: /uri
.
<p><a href="...">link [foo [bar</a>]]<a href="ref">ref</a></p>
````````````````````````````````

[link [foo [bar]]][ref]

[ref]: /uri


```````````````````````````````` example
[link \[bar][ref]

[ref]: /uri
.
<p><a href="...">link \[bar</a><a href="/uri">ref</a></p>
````````````````````````````````


The link text may contain inline content:

```````````````````````````````` example
[link *foo **bar** `#`*][ref]

[ref]: /uri
.
<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>
````````````````````````````````

[link *foo **bar** `#`*][ref]

[ref]: /uri


```````````````````````````````` example
[![moon]<moon.jpg>][ref]

[ref]: /uri
.
<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>
````````````````````````````````

[![moon]<moon.jpg>][ref]


However, links may not contain other links, at any level of nesting.

```````````````````````````````` example
[foo [bar]</uri>][ref]

[ref]: /uri
.
<p><a href="/uri">foo [bar</a>]<a href="/uri">ref</a></p>
````````````````````````````````

[foo [bar]</uri>][ref]

[ref]: /uri


```````````````````````````````` example
[foo *bar [baz][ref]*][ref]

[ref]: /uri
.
<p><a href="/uri">foo <em>bar [baz][ref]</em></a></p>
````````````````````````````````

[foo *bar [baz][ref]*][ref]

[ref]: /uri


(In the examples above, we have [shortcut reference links]
instead of one [full reference link].)

The following cases illustrate the precedence of link text grouping over
emphasis grouping:

```````````````````````````````` example
*[foo*][ref]

[ref]: /uri
.
<p><em><a href="/uri">foo*</a></p>
````````````````````````````````

*[foo*][ref]

[ref]: /uri


```````````````````````````````` example
[foo *bar][ref]

[ref]: /uri
.
<p><a href="...">foo <em>bar][ref]</a></p>
````````````````````````````````

[foo *bar][ref]

[ref]: /uri


These cases illustrate the combination of code spans,
and autolinks with link grouping:

```````````````````````````````` example
[foo`][ref]`

[ref]: /uri
.
<p><a href="...">foo<code>][ref]</code></a></p>
````````````````````````````````

[foo`][ref]`

[ref]: /uri


```````````````````````````````` example
[foo<http://example.com/?search=][ref]>

[ref]: /uri
.
<p><a href="/uri">foo&lt;http://example.com/?search=</a></p>
````````````````````````````````

[foo<http://example.com/?search=][ref]>

[ref]: /uri


Matching is case-insensitive:

```````````````````````````````` example
[foo][BaR]

[bar]: /url
.
<p><a href="/url">foo</a></p>
````````````````````````````````

[foo][BaR]

[bar]: /url


Unicode case fold is used:

```````````````````````````````` example
[Толпой][Толпой] is a Russian word.

[ТОЛПОЙ]: /url
.
<p><a href="/url">Толпой</a> is a Russian word.</p>
````````````````````````````````

[Толпой][Толпой] is a Russian word.

[ТОЛПОЙ]: /url


Consecutive internal [whitespace] is treated as one space for
purposes of determining matching:

```````````````````````````````` example
[Foo   bar]: /url

[Baz][Foo bar]
.
<p><a href="/url">Baz</a></p>
````````````````````````````````

[Foo   bar]: /url

[Baz][Foo bar]

TODO: The above example originally had a linefeed. I don't want references to
have line feeds, as they would be impossible to distinguish from a link label
when only given one line.

No [whitespace] is allowed between the [link text] and the
[link label]:

```````````````````````````````` example
[foo] [bar]

[bar]: /url
.
<p><a href="...">foo</a> <a href="/url">bar</a></p>
````````````````````````````````

[foo] [bar]

[bar]: /url


```````````````````````````````` example
[foo]
[bar]

[bar]: /url
.
<p>[foo]
<p><a href="...">foo</a> <a href="/url">bar</a></p>
````````````````````````````````


When there are multiple matching [link reference definitions],
the first is used:

```````````````````````````````` example
[foo]: /url1

[foo]: /url2

[bar][foo]
.
<p><a href="/url1">bar</a></p>
````````````````````````````````

[foo]: /url1

[foo]: /url2

[bar][foo]


Note that matching is performed on normalized strings, not parsed
inline content.  So the following does not match, even though the
labels define equivalent inline content:

```````````````````````````````` example
[bar][foo\!]

[foo!]: /url
.
<p>[bar][foo!]</p>
````````````````````````````````

[bar][foo\!]

[foo!]: /url


A [collapsed reference link]<@>
consists of a [link label] that [matches] a
[link reference definition] elsewhere in the
document, followed by the string `[]`.
The contents of the first link label are parsed as inlines,
which are used as the link's text.  The link's URI and title are
provided by the matching reference link definition.  Thus,
`[foo][]` is equivalent to `[foo][foo]`.

```````````````````````````````` example
[foo][]

[foo]: /url
.
<p><a href="/url">foo</a></p>
````````````````````````````````

[foo][]

[foo]: /url


```````````````````````````````` example
[*foo* bar][]

[*foo* bar]: /url
.
<p><a href="/url"><em>foo</em> bar</a></p>
````````````````````````````````

[*foo* bar][]

[*foo* bar]: /url


The link labels are case-insensitive:

```````````````````````````````` example
[Foo][]

[foo]: /url
.
<p><a href="/url">Foo</a></p>
````````````````````````````````

[Foo][]

[foo]: /url



As with full reference links, [whitespace] is not
allowed between the two sets of brackets:

```````````````````````````````` example
[foo]
[]

[foo]: /url
.
<p><a href="/url">foo</a>
[]</p>
````````````````````````````````

[foo]
[]

[foo]: /url


A [shortcut reference link]<@>
consists of a [link label] that [matches] a
[link reference definition] elsewhere in the
document and is not followed by `[]` or a link label.
The contents of the first link label are parsed as inlines,
which are used as the link's text.  The link's URI and title
are provided by the matching link reference definition.
Thus, `[foo]` is equivalent to `[foo][]`.

```````````````````````````````` example
[foo]

[foo]: /url
.
<p><a href="/url">foo</a></p>
````````````````````````````````

[foo]

[foo]: /url


```````````````````````````````` example
[*foo* bar]

[*foo* bar]: /url
.
<p><a href="/url"><em>foo</em> bar</a></p>
````````````````````````````````

[*foo* bar]

[*foo* bar]: /url


```````````````````````````````` example
[[*foo* bar]]

[*foo* bar]: /url
.
<p><a href="...">[<em>foo</em> bar</a>]</p>
````````````````````````````````

[[*foo* bar]]

[*foo* bar]: /url


```````````````````````````````` example
[[bar [foo]

[foo]: /url
.
<p><a href="..">[bar [foo</a></p>
````````````````````````````````

[[bar [foo]

[foo]: /url


The link labels are case-insensitive:

```````````````````````````````` example
[Foo]

[foo]: /url
.
<p><a href="/url">Foo</a></p>
````````````````````````````````

[Foo]

[foo]: /url


A space after the link text should be preserved:

```````````````````````````````` example
[foo] bar

[foo]: /url
.
<p><a href="/url">foo</a> bar</p>
````````````````````````````````

[foo] bar

[foo]: /url


If you just want bracketed text, you can backslash-escape the
opening bracket to avoid links:

```````````````````````````````` example
\[foo]

[foo]: /url "title"
.
<p>[foo]</p>
````````````````````````````````

\[foo]

[foo]: /url "title"


Note that this is a link, because a link label ends with the first
following closing bracket:

```````````````````````````````` example
[foo*]: /url

*[foo*]
.
<p><em><a href="/url">foo*</a></em></p>
````````````````````````````````

[foo*]: /url

*[foo*]


Full and compact references take precedence over shortcut
references:

```````````````````````````````` example
[foo][bar]

[foo]: /url1
[bar]: /url2
.
<p><a href="/url2">foo</a></p>
````````````````````````````````

[foo][bar]

[foo]: /url1
[bar]: /url2

```````````````````````````````` example
[foo][]

[foo]: /url1
.
<p><a href="/url1">foo</a></p>
````````````````````````````````

[foo][]

[foo]: /url1

Inline links also take precedence:

```````````````````````````````` example
[foo]<>

[foo]: /url1
.
<p><a href="">foo</a></p>
````````````````````````````````

[foo]<>

[foo]: /url1

```````````````````````````````` example
[foo]<not a link>

[foo]: /url1
.
<p><a href="/url1">foo</a><not a link></p>
````````````````````````````````

[foo]<not a link>

[foo]: /url1

In the following case `[foo][bar]` is parsed as a reference first, then
`[baz]`. It does not matter that bar is not defined:

```````````````````````````````` example
[foo][bar][baz]

[baz]: /url
.
<p><a href="...">foo</a><a href="/url">baz</a></p>
````````````````````````````````

## Images

```````````````````````````````` example
![foo]</url>
.
<p><img src="/url" alt="foo"/></p>
````````````````````````````````

![foo]</url>


```````````````````````````````` example
![foo *bar*]

[foo *bar*]: train.jpg
.
<p><img src="train.jpg" alt="foo bar"/></p>
````````````````````````````````

![foo *bar*]

[foo *bar*]: train.jpg


```````````````````````````````` example
![foo ![bar]</url>]</url2>
.
<p><img src="/url2" alt="foo ![bar]&lt;/url&gt;" /></p>
````````````````````````````````

![foo ![bar]</url>]</url2>


```````````````````````````````` example
![foo [bar]</url>]</url2>
.
<p><img src="/url2" alt="foo [bar]&lt;/url&gt;" /></p>
````````````````````````````````

![foo [bar]</url>]</url2>


```````````````````````````````` example
My ![foo bar]</path/to/train.jpg>
.
<p>My <img src="/path/to/train.jpg" alt="foo bar"/></p>
````````````````````````````````

My ![foo bar]</path/to/train.jpg>


```````````````````````````````` example
![foo]<url>
.
<p><img src="url" alt="foo" /></p>
````````````````````````````````

![foo]<url>


```````````````````````````````` example
![]</url>
.
<p><img src="/url" alt="" /></p>
````````````````````````````````

![]</url>


Reference-style:

```````````````````````````````` example
![foo][bar]

[bar]: /url
.
<p><img src="/url" alt="foo" /></p>
````````````````````````````````

![foo][bar]

[bar]: /url


```````````````````````````````` example
![foo][bar]

[BAR]: /url
.
<p><img src="/url" alt="foo" /></p>
````````````````````````````````

![foo][bar]

[BAR]: /url


Collapsed:

```````````````````````````````` example
![foo][]

[foo]: /url
.
<p><img src="/url" alt="foo"/></p>
````````````````````````````````

![foo][]

[foo]: /url


```````````````````````````````` example
![*foo* bar][]

[*foo* bar]: /url
.
<p><img src="/url" alt="foo bar"/></p>
````````````````````````````````

![*foo* bar][]

[*foo* bar]: /url


The labels are case-insensitive:

```````````````````````````````` example
![Foo][]

[foo]: /url
.
<p><img src="/url" alt="Foo"/></p>
````````````````````````````````

![Foo][]

[foo]: /url


As with reference links, [whitespace] is not allowed
between the two sets of brackets:

```````````````````````````````` example
![foo]
[]

[foo]: /url
.
<p><img src="/url" alt="foo"/>
[]</p>
````````````````````````````````

![foo]
[]

[foo]: /url


Shortcut:

```````````````````````````````` example
![foo]

[foo]: /url
.
<p><img src="/url" alt="foo"/></p>
````````````````````````````````

![foo]

[foo]: /url


```````````````````````````````` example
![*foo* bar]

[*foo* bar]: /url
.
<p><img src="/url" alt="*foo* bar"/></p>
````````````````````````````````

![*foo* bar]

[*foo* bar]: /url


If you want a link after a literal `!`, backslash-escape the
`!`:

```````````````````````````````` example
\![foo]

[foo]: /url
.
<p>!<a href="/url">foo</a></p>
````````````````````````````````

\![foo]

[foo]: /url


```````````````````````````````` example
<http://foo.bar.baz>
.
<p><a href="http://foo.bar.baz">http://foo.bar.baz</a></p>
````````````````````````````````

<http://foo.bar.baz>


```````````````````````````````` example
<http://foo.bar.baz/test?q=hello&id=22&boolean>
.
<p><a href="http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>
````````````````````````````````

<http://foo.bar.baz/test?q=hello&id=22&boolean>


```````````````````````````````` example
<irc://foo.bar:2233/baz>
.
<p><a href="irc://foo.bar:2233/baz">irc://foo.bar:2233/baz</a></p>
````````````````````````````````

<irc://foo.bar:2233/baz>


Uppercase is also fine:

```````````````````````````````` example
<MAILTO:FOO@BAR.BAZ>
.
<p><a href="MAILTO:FOO@BAR.BAZ">MAILTO:FOO@BAR.BAZ</a></p>
````````````````````````````````

<MAILTO:FOO@BAR.BAZ>


Note that many strings that count as [URIs] for
purposes of this spec are not valid URIs, because their
schemes are not registered or because of other problems
with their syntax:

```````````````````````````````` example
<a+b+c:d>
.
<p><a href="a+b+c:d">a+b+c:d</a></p>
````````````````````````````````

<a+b+c:d>


```````````````````````````````` example
<made-up-scheme://foo,bar>
.
<p><a href="made-up-scheme://foo,bar">made-up-scheme://foo,bar</a></p>
````````````````````````````````

<made-up-scheme://foo,bar>


```````````````````````````````` example
<http://../>
.
<p><a href="http://../">http://../</a></p>
````````````````````````````````

<http://../>


```````````````````````````````` example
<localhost:5001/foo>
.
<p><a href="localhost:5001/foo">localhost:5001/foo</a></p>
````````````````````````````````

<localhost:5001/foo>


Spaces are not allowed in autolinks:

```````````````````````````````` example
<http://foo.bar/baz bim>
.
<p>&lt;http://foo.bar/baz bim&gt;</p>
````````````````````````````````

<http://foo.bar/baz bim>


Backslash-escapes do not work inside autolinks:

```````````````````````````````` example
<http://example.com/\[\>
.
<p><a href="http://example.com/%5C%5B%5C">http://example.com/\[\</a></p>
````````````````````````````````

<http://example.com/\[\>


An [email autolink]<@>
consists of `<`, followed by an [email address],
followed by `>`.  The link's label is the email address,
and the URL is `mailto:` followed by the email address.

An [email address]<@>,
for these purposes, is anything that matches
the [non-normative regex from the HTML5
spec](https://html.spec.whatwg.org/multipage/forms.html#e-mail-state-(type=email)):

    /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?
    (?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/

Examples of email autolinks:

```````````````````````````````` example
<foo@bar.example.com>
.
<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>
````````````````````````````````

<foo@bar.example.com>


```````````````````````````````` example
<foo+special@Bar.baz-bar0.com>
.
<p><a href="mailto:foo+special@Bar.baz-bar0.com">foo+special@Bar.baz-bar0.com</a></p>
````````````````````````````````

<foo+special@Bar.baz-bar0.com>


Backslash-escapes do not work inside email autolinks:

```````````````````````````````` example
<foo\+@bar.example.com>
.
<p>&lt;foo+@bar.example.com&gt;</p>
````````````````````````````````

<foo\+@bar.example.com>


These are not autolinks:

```````````````````````````````` example
<>
.
<p>&lt;&gt;</p>
````````````````````````````````

<>


```````````````````````````````` example
< http://foo.bar >
.
<p>&lt; http://foo.bar &gt;</p>
````````````````````````````````

< http://foo.bar >


```````````````````````````````` example
http://example.com
.
<p>http://example.com</p>
````````````````````````````````

http://example.com


```````````````````````````````` example
foo@bar.example.com
.
<p>foo@bar.example.com</p>
````````````````````````````````

foo@bar.example.com

## Annotations

```````````````````````````````` example
foo <!-- this is a
comment - with hyphen -->
.
<p>foo <!-- this is a
comment - with hyphen --></p>
````````````````````````````````

foo <!-- this is a
comment - with hyphen -->


```````````````````````````````` example
foo <!-- is a comment -- two hyphens will be processed out -->
.
<p>foo <!-- is a comment - - two hyphens will be processed out --></p>
````````````````````````````````

foo <!-- is a comment -- two hyphens will be processed out -->


Not comment:

```````````````````````````````` example
foo <!--> foo -->
.
<p>foo &lt;!--&gt; foo --&gt;</p>
````````````````````````````````

foo <!--> foo -->


Comment with adjacent hyphens fixed:

```````````````````````````````` example
foo <!-- foo--->
.
<p>foo <!-- foo- -->;</p>
````````````````````````````````

foo <!-- foo--->



Backslash escapes do not work in annotations:

```````````````````````````````` example
foo <!-- foo \-->
.
<p>foo <!-- foo \--></p>
````````````````````````````````

(This example is not shown as it would corrupt the document


```````````````````````````````` example
<!--
 This is an annotation
-->
okay.
.
<!--
 This is an annotation
-->
<p>okay.</p>
````````````````````````````````

<!--
 This is an annotation
-->
okay.

The start need not be on a line by
itself:

```````````````````````````````` example
<!-- *foo*
-->
.
<!-- *foo*
-->
````````````````````````````````

<!-- *foo*
-->


If there is no matching end tag, the block will end at the
end of the document (or the enclosing [block quote][block quotes]
or [list item][list items]):

```````````````````````````````` example
<!--
foo
.
<!--
foo
````````````````````````````````

(The example is not shown as it would lead to a corrupted document)

The end tag can occur on the same line as the start tag:

```````````````````````````````` example
<!-- foo -->
.
<!-- foo -->
````````````````````````````````

<!-- foo -->


```````````````````````````````` example
<!-- foo -->*bar*
*baz*
.
<p><!-- foo --><em>bar</em></p>
<p><em>baz</em></p>
````````````````````````````````

<!-- foo -->*bar*
*baz*


```````````````````````````````` example
<!-- Foo

bar
   baz -->
okay
.
<!-- Foo

bar
   baz -->
<p>okay</p>
````````````````````````````````

<!-- Foo

bar
   baz -->
okay

## Hard line breaks

```````````````````````````````` example
foo\
baz
.
<p>foo<br />
baz</p>
````````````````````````````````


```````````````````````````````` example
foo\
     bar
.
<p>foo<br />
bar</p>
````````````````````````````````

foo\
     bar


Line breaks can occur inside emphasis, links, and other constructs
that allow inline content:

```````````````````````````````` example
*foo\
bar*
.
<p><em>foo<br />
bar</em></p>
````````````````````````````````

*foo\
bar*


Line breaks do not occur inside code spans

```````````````````````````````` example
`code\
span`
.
<p><code>code\ span</code></p>
````````````````````````````````

`code\
span`


Hard line breaks are for separating inline content within a block. However,
you can put one at the end of a paragraph, watch out for this.

```````````````````````````````` example
foo\
.
<p>foo<br/></p>
````````````````````````````````

foo\


```````````````````````````````` example
### foo\
.
<h3>foo<br/></h3>
````````````````````````````````

### foo\

## Soft line breaks

```````````````````````````````` example
foo
baz
.
<p>foo
baz</p>
````````````````````````````````

foo
baz


Spaces at the end of the line and beginning of the next line are
removed:

```````````````````````````````` example
foo
 baz
.
<p>foo
baz</p>
````````````````````````````````

foo
 baz


A conforming parser may render a soft line break in HTML either as a
line break or as a space.

## Textual content

```````````````````````````````` example
hello $.;'there
.
<p>hello $.;'there</p>
````````````````````````````````

hello $.;'there


```````````````````````````````` example
Foo χρῆν
.
<p>Foo χρῆν</p>
````````````````````````````````

Foo χρῆν


Internal spaces are preserved verbatim:

```````````````````````````````` example
Multiple     spaces
.
<p>Multiple     spaces</p>
````````````````````````````````

Multiple     spaces


<!-- END TESTS -->


