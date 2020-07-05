# What can you do with Scitzen?

With Scitzen you can write documents in a lightweight markup language (think markdown) and generate blog-like websites (think Jekyll) or long-form PDF articles (think LaTeX).

Scitzen has its own markup language, which is a mixture of Markdown, Asciidoc(tor), and LaTeX. It takse the idea of flexible blocks from Asciidoc but reduces complexity by removing variety. The concrete syntax is still changing regularly.

A collection of Scitzen documents can be compiled into a very minimal HTML based blog. This essentially makes Scitzen a static site generator. The only thing remarkable about this part of Scitzen is a lack of features and flexibility compared to the hundreds of existing static site generators.

A single Scitzen document can be separated into multiple files and then compiled into a single HTML or PDF document. The PDF is generated using XeLaTeX, thus you can produce good looking PDFs. However, there is – again – no flexibility of how the output looks like.

# Whats the point then?

The original motivation behind Scitzen were twofold. First, configuring static site generators and their plugins to produce the result you want is much harder than writing your own. Second, Markdown was to limited as a language, and Asciidoc was way too complicated and I could never remember what punctuation did what.

The current version of Scitzen uses very little punctuation ( basically these symbols: `=:{};` ) and most complicated things just have names (like macros in LaTeX) to make them easier to remember. A lot of current thinking goes into how the structure of documents looks like, and how to express them in a way that they look “right” no matter if published as a research paper, or as a blog post. 

# What have you learned?

It is fascinating, how much better you understand the general structure of documents, if you try to unify multiple kinds of documents (blog post, research papers) into a single framework. It sounds like they are so similar, and they are, but also not really.

There is also a very interesting tradeoff between having a notation to write such documents having a regular and semantic structure to make them easy to understand (for both parsing, but also make it clear to people how the result will look like), and having them look like “just normal text with some funny symbols“ (what Markdown is trying to do). I came to the conclusion that Markdown – for me – is a really bad tradeoff between being just plain text and being machine readable. That is, I feel when using Markdown I have to often deviate from what I would just normally write in plain text, such that the text looks good and structured, but it is also way to limited to have precise control over how the output looks especially when compared to LaTeX. Do not let that stop you liking Markdown. This is just me.

# So how do I use it?

Well … `sbt run` works. Or `sbt nativeImage` to generate a binary. Probably needs linux. Has a lot of optional dependencies on tools to convert other markup (Mermaid, Katex, Graphviz, Inkscape, Cairo, … ).

Also, you would probably have to figure out the syntax, because I am currently to lazy to write that down 😛.

# So, why is the readme still written in Markdown?

Because .txt documents do not look good in Github.
