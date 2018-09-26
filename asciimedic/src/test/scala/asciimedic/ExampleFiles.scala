package asciimedic

case class ParsingTest[T](snippet: String, res: T)

object ExampleFiles {

  val link = """We're parsing link:http://asciidoc.org[AsciiDoc] markup"""

  val attributedParagraph= """
. With a title
[someAttribute, someOtherAttribute="test, nochnTest"]
A paragraph
"""

  val blockWithSingleAttribute = """
some initial paragraph

[role=portrait]
image::images/2018-09-26_Lächeln-in-der-Bahn.jpg[Lächeln in der Bahn]"""

  val sample ="""Document Title
==============
Doc Writer <thedoc@asciidoctor.org>
:idprefix: id_

Preamble paragraph.

NOTE: This is test, only a test.

== Section A

*Section A* paragraph.

=== Section A Subsection

*Section A* 'subsection' paragraph.

== Section B

*Section B* paragraph.

|===
|a |b |c
|1 |2 |3
|===

.Section B list
* Item 1
* Item 2
* Item 3
"""

  val manySections = ParsingTest("""= Document Title (Level 0)

== Level 1 Section Title

=== Level 2 Section Title

==== Level 3 Section Title

===== Level 4 Section Title

====== Level 5 Section Title

== Another Level 1 Section Title
""",
                                 Document(
                                   Some(Header("Document Title (Level 0)", Seq(), Seq())),
                                   Seq(
                                     SectionTitle(1, " Level 1 Section Title"),
                                     SectionTitle(2, " Level 2 Section Title"),
                                     SectionTitle(3, " Level 3 Section Title"),
                                     SectionTitle(4, " Level 4 Section Title"),
                                     SectionTitle(5, " Level 5 Section Title"),
                                     SectionTitle(1, " Another Level 1 Section Title")
                                   )
                                 ))



  val multipleAuthors = """= The Dangerous and Thrilling Documentation Chronicles
Kismet Rainbow Chameleon <kismet@asciidoctor.org>; Lazarus het_Draeke <lazarus@asciidoctor.org>
"""

  val simpleLists = ParsingTest("""
.Unordered, basic
* Edgar Allen Poe
* Sheri S. Tepper
* Bill Bryson

.Unordered, max nesting
* level 1
** level 2
*** level 3
**** level 4
***** level 5
* level 1

""",
                                Document(
                                  None,
                                  Seq(
                                    BlockWithAttributes(
                                      ListBlock(
                                        Seq(
                                          ListItem("* ", "Edgar Allen Poe"),
                                          ListItem("* ", "Sheri S. Tepper"),
                                          ListItem("* ", "Bill Bryson")
                                        )
                                      ),
                                      Seq(),
                                      Some("Unordered, basic")
                                    ),
                                    BlockWithAttributes(
                                      ListBlock(
                                        Seq(
                                          ListItem("* ", "level 1"),
                                          ListItem("** ", "level 2"),
                                          ListItem("*** ", "level 3"),
                                          ListItem("**** ", "level 4"),
                                          ListItem("***** ", "level 5"),
                                          ListItem("* ", "level 1")
                                        )
                                      ),
                                      Seq(),
                                      Some("Unordered, max nesting")
                                    )
                                  )
                                ))

  val lists =
    """= Document Title
Doc Writer <thedoc@asciidoctor.org>

Preamble paragraph.

NOTE: This is test, only a test.

== Lists

.Unordered, basic
* Edgar Allen Poe
* Sheri S. Tepper
* Bill Bryson

.Unordered, max nesting
* level 1
** level 2
*** level 3
**** level 4
***** level 5
* level 1

.Checklist
- [*] checked
- [x] also checked
- [ ] not checked
-     normal list item

.Ordered, basic
. Step 1
. Step 2
. Step 3

.Ordered, nested
. Step 1
. Step 2
.. Step 2a
.. Step 2b
. Step 3

.Ordered, max nesting
. level 1
.. level 2
... level 3
.... level 4
..... level 5
. level 1

.Labeled, single-line
first term:: definition of first term
section term:: definition of second term

.Labeled, multi-line
first term::
definition of first term
second term::
definition of second term

.Q&A
[qanda]
What is Asciidoctor?::
  An implementation of the AsciiDoc processor in Ruby.
What is the answer to the Ultimate Question?:: 42

.Mixed
Operating Systems::
  Linux:::
    . Fedora
      * Desktop
    . Ubuntu
      * Desktop
      * Server
  BSD:::
    . FreeBSD
    . NetBSD

Cloud Providers::
  PaaS:::
    . OpenShift
    . CloudBees
  IaaS:::
    . Amazon EC2
    . Rackspace

.Unordered, complex
* level 1
** level 2
*** level 3
This is a new line inside an unordered list using {plus} symbol.
We can even force content to start on a separate line... +
Amazing, isn't it?
**** level 4
+
The {plus} symbol is on a new line.

***** level 5
"""

  val nestedExample = """.Sample document
====
Here's a sample AsciiDoc document:

[listing]
....
= Title of Document
Doc Writer
:toc:

This guide provides...
....

The document header is useful, but not required.
====
"""

  val nestedExampleParsed = Document(
    None,
    Seq(
      BlockWithAttributes(
        DelimitedBlock(
          "====",
          """Here's a sample AsciiDoc document:

[listing]
....
= Title of Document
Doc Writer
:toc:

This guide provides...
....

The document header is useful, but not required."""
        ),
        Nil,
        Some("Sample document")
      ),
      DelimitedBlock("====", "")
    )
  )

}
