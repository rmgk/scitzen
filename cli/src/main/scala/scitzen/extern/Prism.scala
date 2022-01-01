package scitzen.extern

import better.files.*
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import org.graalvm.polyglot.*
import org.graalvm.polyglot.proxy.ProxyObject

import java.io.{ByteArrayOutputStream, File}
import java.lang.ProcessBuilder.Redirect
import scala.jdk.CollectionConverters.*

object Prism:

  val prismcontext =
    val engine: Engine = Engine.newBuilder().build()
    val start          = System.nanoTime()
    Context.newBuilder("js").engine(engine).build()

  lazy val highlightVal = {
    prismcontext.eval("js", "Prism.highlight")
  }


  val resolvePath: String = "META-INF/resources/webjars/prism/components"

  var loadedLanguages = Set.empty[String]

  def ensureLoaded(lang: String): Unit =
    if !loadedLanguages.contains(lang) then
      if (lang != "core") ensureLoaded("core")
      dependencies.getOrElse(lang, Nil).foreach(ensureLoaded)
      val start = System.nanoTime()
      prismcontext.eval("js", Resource.getAsString(s"$resolvePath/prism-$lang.min.js"))
      loadedLanguages = loadedLanguages + lang
      println(s"loading prism $lang took ${(System.nanoTime() - start) / 1000000}ms")

  def highlight(code: String, lang: String): String =
    val actualLang = aliases.getOrElse(lang, lang)
    ensureLoaded(actualLang)

    val start = System.nanoTime()
    val res   = highlightVal.execute(code, prismcontext.eval("js", s"Prism.languages.$actualLang"), actualLang).asString()
    println(s"highlighting took ${(System.nanoTime() - start) / 1000000}ms")
    res

  // see https://github.com/PrismJS/prism/blob/master/plugins/autoloader/prism-autoloader.js
  // for the generated list
  val dependencies: Map[String, List[String]] = Map(
    "javascript"               -> List("clike"),
    "actionscript"             -> List("javascript"),
    "apex"                     -> List("clike", "sql"),
    "arduino"                  -> List("cpp"),
    "aspnet"                   -> List("markup", "csharp"),
    "birb"                     -> List("clike"),
    "bison"                    -> List("c"),
    "c"                        -> List("clike"),
    "csharp"                   -> List("clike"),
    "cpp"                      -> List("c"),
    "cfscript"                 -> List("clike"),
    "chaiscript"               -> List("clike", "cpp"),
    "coffeescript"             -> List("javascript"),
    "crystal"                  -> List("ruby"),
    "css-extras"               -> List("css"),
    "d"                        -> List("clike"),
    "dart"                     -> List("clike"),
    "django"                   -> List("markup-templating"),
    "ejs"                      -> List("javascript", "markup-templating"),
    "etlua"                    -> List("lua", "markup-templating"),
    "erb"                      -> List("ruby", "markup-templating"),
    "fsharp"                   -> List("clike"),
    "firestore-security-rules" -> List("clike"),
    "flow"                     -> List("javascript"),
    "ftl"                      -> List("markup-templating"),
    "gml"                      -> List("clike"),
    "glsl"                     -> List("c"),
    "go"                       -> List("clike"),
    "groovy"                   -> List("clike"),
    "haml"                     -> List("ruby"),
    "handlebars"               -> List("markup-templating"),
    "haxe"                     -> List("clike"),
    "hlsl"                     -> List("c"),
    "idris"                    -> List("haskell"),
    "java"                     -> List("clike"),
    "javadoc"                  -> List("markup", "java", "javadoclike"),
    "jolie"                    -> List("clike"),
    "jsdoc"                    -> List("javascript", "javadoclike", "typescript"),
    "js-extras"                -> List("javascript"),
    "json5"                    -> List("json"),
    "jsonp"                    -> List("json"),
    "js-templates"             -> List("javascript"),
    "kotlin"                   -> List("clike"),
    "latte"                    -> List("clike", "markup-templating", "php"),
    "less"                     -> List("css"),
    "lilypond"                 -> List("scheme"),
    "liquid"                   -> List("markup-templating"),
    "markdown"                 -> List("markup"),
    "markup-templating"        -> List("markup"),
    "mongodb"                  -> List("javascript"),
    "n4js"                     -> List("javascript"),
    "objectivec"               -> List("c"),
    "opencl"                   -> List("c"),
    "parser"                   -> List("markup"),
    "php"                      -> List("markup-templating"),
    "phpdoc"                   -> List("php", "javadoclike"),
    "php-extras"               -> List("php"),
    "plsql"                    -> List("sql"),
    "processing"               -> List("clike"),
    "protobuf"                 -> List("clike"),
    "pug"                      -> List("markup", "javascript"),
    "purebasic"                -> List("clike"),
    "purescript"               -> List("haskell"),
    "qsharp"                   -> List("clike"),
    "qml"                      -> List("javascript"),
    "qore"                     -> List("clike"),
    "racket"                   -> List("scheme"),
    "cshtml"                   -> List("markup", "csharp"),
    "jsx"                      -> List("markup", "javascript"),
    "tsx"                      -> List("jsx", "typescript"),
    "reason"                   -> List("clike"),
    "ruby"                     -> List("clike"),
    "sass"                     -> List("css"),
    "scss"                     -> List("css"),
    "scala"                    -> List("java"),
    "shell-session"            -> List("bash"),
    "smarty"                   -> List("markup-templating"),
    "solidity"                 -> List("clike"),
    "soy"                      -> List("markup-templating"),
    "sparql"                   -> List("turtle"),
    "sqf"                      -> List("clike"),
    "squirrel"                 -> List("clike"),
    "t4-cs"                    -> List("t4-templating", "csharp"),
    "t4-vb"                    -> List("t4-templating", "vbnet"),
    "tap"                      -> List("yaml"),
    "tt2"                      -> List("clike", "markup-templating"),
    "textile"                  -> List("markup"),
    "twig"                     -> List("markup-templating"),
    "typescript"               -> List("javascript"),
    "v"                        -> List("clike"),
    "vala"                     -> List("clike"),
    "vbnet"                    -> List("basic"),
    "velocity"                 -> List("markup"),
    "wiki"                     -> List("markup"),
    "xeora"                    -> List("markup"),
    "xml-doc"                  -> List("markup"),
    "xquery"                   -> List("markup")
  )

  val aliases = Map(
    "html"              -> "markup",
    "xml"               -> "markup",
    "svg"               -> "markup",
    "mathml"            -> "markup",
    "ssml"              -> "markup",
    "atom"              -> "markup",
    "rss"               -> "markup",
    "js"                -> "javascript",
    "g4"                -> "antlr4",
    "ino"               -> "arduino",
    "adoc"              -> "asciidoc",
    "avs"               -> "avisynth",
    "avdl"              -> "avro-idl",
    "shell"             -> "bash",
    "shortcode"         -> "bbcode",
    "rbnf"              -> "bnf",
    "oscript"           -> "bsl",
    "cs"                -> "csharp",
    "dotnet"            -> "csharp",
    "cfc"               -> "cfscript",
    "coffee"            -> "coffeescript",
    "conc"              -> "concurnas",
    "jinja2"            -> "django",
    "dns-zone"          -> "dns-zone-file",
    "dockerfile"        -> "docker",
    "gv"                -> "dot",
    "eta"               -> "ejs",
    "xlsx"              -> "excel-formula",
    "xls"               -> "excel-formula",
    "gamemakerlanguage" -> "gml",
    "gni"               -> "gn",
    "go-mod"            -> "go-module",
    "hbs"               -> "handlebars",
    "hs"                -> "haskell",
    "idr"               -> "idris",
    "gitignore"         -> "ignore",
    "hgignore"          -> "ignore",
    "npmignore"         -> "ignore",
    "webmanifest"       -> "json",
    "kt"                -> "kotlin",
    "kts"               -> "kotlin",
    "kum"               -> "kumir",
    "tex"               -> "latex",
    "context"           -> "latex",
    "ly"                -> "lilypond",
    "emacs"             -> "lisp",
    "elisp"             -> "lisp",
    "emacs-lisp"        -> "lisp",
    "md"                -> "markdown",
    "moon"              -> "moonscript",
    "n4jsd"             -> "n4js",
    "nani"              -> "naniscript",
    "objc"              -> "objectivec",
    "qasm"              -> "openqasm",
    "objectpascal"      -> "pascal",
    "px"                -> "pcaxis",
    "pcode"             -> "peoplecode",
    "pq"                -> "powerquery",
    "mscript"           -> "powerquery",
    "pbfasm"            -> "purebasic",
    "purs"              -> "purescript",
    "py"                -> "python",
    "qs"                -> "qsharp",
    "rkt"               -> "racket",
    "razor"             -> "cshtml",
    "rpy"               -> "renpy",
    "robot"             -> "robotframework",
    "rb"                -> "ruby",
    "sh-session"        -> "shell-session",
    "shellsession"      -> "shell-session",
    "smlnj"             -> "sml",
    "sol"               -> "solidity",
    "sln"               -> "solution-file",
    "rq"                -> "sparql",
    "t4"                -> "t4-cs",
    "trickle"           -> "tremor",
    "troy"              -> "tremor",
    "trig"              -> "turtle",
    "ts"                -> "typescript",
    "tsconfig"          -> "typoscript",
    "uscript"           -> "unrealscript",
    "uc"                -> "unrealscript",
    "url"               -> "uri",
    "vb"                -> "visual-basic",
    "vba"               -> "visual-basic",
    "webidl"            -> "web-idl",
    "mathematica"       -> "wolfram",
    "nb"                -> "wolfram",
    "wl"                -> "wolfram",
    "xeoracube"         -> "xeora",
    "yml"               -> "yaml"
  )
