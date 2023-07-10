package scitzen.html

val validHtml5Tags = List(
  "!DOCTYPE html", // Defines the document type
  "a",             // Defines a hyperlink
  "abbr",          // Defines an abbreviation or an acronym
  "address",       // Defines contact information for the author/owner of a document
  "area",          // Defines an area inside an image map
  "article",       // Defines an article
  "aside",         // Defines content aside from the page content
  "audio",         // Defines embedded sound content
  "b",             // Defines bold text
  "base",          // Specifies the base URL/target for all relative URLs in a document
  "bdi",        // Isolates a part of text that might be formatted in a different direction from other text outside it
  "bdo",        // Overrides the current text direction
  "blockquote", // Defines a section that is quoted from another source
  "body",       // Defines the document's body
  "br",         // Defines a single line break
  "button",     // Defines a clickable button
  "canvas",     // Used to draw graphics, on the fly, via scripting (usually JavaScript)
  "caption",    // Defines a table caption
  "cite",       // Defines the title of a work
  "code",       // Defines a piece of computer code
  "col",        // Specifies column properties for each column within a <colgroup> element
  "colgroup",   // Specifies a group of one or more columns in a table for formatting
  "data",       // Adds a machine-readable translation of a given content
  "datalist",   // Specifies a list of pre-defined options for input controls
  "dd",         // Defines a description/value of a term in a description list
  "del",        // Defines text that has been deleted from a document
  "details",    // Defines additional details that the user can view or hide
  "dfn",        // Specifies a term that is going to be defined within the content
  "dialog",     // Defines a dialog box or window
  "div",        // Defines a section in a document
  "dl",         // Defines a description list
  "dt",         // Defines a term/name in a description list
  "em",         // Defines emphasized text
  "embed",      // Defines a container for an external application
  "fieldset",   // Groups related elements in a form
  "figcaption", // Defines a caption for a <figure> element
  "figure",     // Specifies self-contained content
  "footer",     // Defines a footer for a document or section
  "form",       // Defines an HTML form for user input
  "h1",
  "h2",
  "h3",
  "h4",
  "h5",
  "h6",     // Defines HTML headings
  "head",   // Contains metadata/information for the document
  "header", // Defines a header for a document or section
  "hr",     // Defines a thematic change in the content
  "html",   // Defines the root of an HTML document
  "i",      // Defines a part of text in an alternate voice or mood
  "iframe", // Defines an inline frame
  "img",    // Defines an image
  "input",  // Defines an input control
  "ins",    // Defines a text that has been inserted into a document
  "kbd",    // Defines keyboard input
  "label",  // Defines a label for an <input> element
  "legend", // Defines a caption for a <fieldset> element
  "li",     // Defines a list item
  "link",   // Defines the relationship between a document and an external resource (most used to link to style sheets)
  "main",   // Specifies the main content of a document
  "math",   // technically not html5, but allowed
  "map",    // Defines an image map
  "mark",   // Defines marked/highlighted text
  "meta",   // Defines metadata about an HTML document
  "meter",  // Defines a scalar measurement within a known range (a gauge)
  "nav",    // Defines navigation links
  "noscript", // Defines an alternate content for users that do not support client-side scripts
  "object",   // Defines a container for an external application
  "ol",       // Defines an ordered list
  "optgroup", // Defines a group of related options in a drop-down list
  "option",   // Defines an option in a drop-down list
  "output",   // Defines the result of a calculation
  "p",        // Defines a paragraph
  "param",    // Defines a parameter for an object
  "picture",  // Defines a container for multiple image resources
  "pre",      // Defines preformatted text
  "progress", // Represents the progress of a task
  "q",        // Defines a short quotation
  "rp",       // Defines what to show in browsers that do not support ruby annotations
  "rt",       // Defines an explanation/pronunciation of characters (for East Asian typography)
  "ruby",     // Defines a ruby annotation (for East Asian typography)
  "s",        // Defines text that is no longer correct
  "samp",     // Defines sample output from a computer program
  "script",   // Defines a client-side script
  "section",  // Defines a section in a document
  "select",   // Defines a drop-down list
  "small",    // Defines smaller text
  "source",   // Defines multiple media resources for media elements (<video> and <audio>)
  "span",     // Defines a section in a document
  "strong",   // Defines important text
  "style",    // Defines style information for a document
  "sub",      // Defines subscripted text
  "summary",  // Defines a visible heading for a <details> element
  "sup",      // Defines superscripted text
  "svg",      // Defines a container for SVG graphics
  "table",    // Defines a table
  "tbody",    // Groups the body content in a table
  "td",       // Defines a cell in a table
  "template", // Defines a container for content that should be hidden when the page loads
  "textarea", // Defines a multiline input control (text area)
  "tfoot",    // Groups the footer content in a table
  "th",       // Defines a header cell in a table
  "thead",    // Groups the header content in a table
  "time",     // Defines a specific time (or datetime)
  "title",    // Defines a title for the document
  "tr",       // Defines a row in a table
  "track",    // Defines text tracks for media elements (<video> and <audio>)
  "u",        // Defines some text that is unarticulated and styled differently from normal text
  "ul",       // Defines an unordered list
  "var",      // Defines a variable
  "video",    // Defines embedded video content
  "wbr",      // Defines a possible line-break
)

case class AttributeScope(attr: String, scope: List[String])

val attributeScopes = List(
  AttributeScope(
    "accept",
    List("input")
  ), // Specifies the types of files that the server accepts (only for type="file")
  AttributeScope(
    "accept-charset",
    List("form")
  ),                                   // Specifies the character encodings that are to be used for the form submission
  AttributeScope("accesskey", List()), // Specifies a shortcut key to activate/focus an element
  AttributeScope("action", List("form")), // Specifies where to send the form-data when a form is submitted
  AttributeScope(
    "alt",
    List("area", "img", "input")
  ), // Specifies an alternate text when the original element fails to display
  AttributeScope(
    "async",
    List("script")
  ), // Specifies that the script is executed asynchronously (only for external scripts)
  AttributeScope(
    "autocomplete",
    List("form", "input")
  ), // Specifies whether the "form" or the "input" element should have autocomplete enabled
  AttributeScope(
    "autofocus",
    List("button", "input", "select", "textarea")
  ), // Specifies that the element should automatically get focus when the page loads
  AttributeScope(
    "autoplay",
    List("audio", "video")
  ), // Specifies that the audio/video will start playing as soon as it is ready
  AttributeScope("charset", List("meta", "script")), // Specifies the character encoding
  AttributeScope(
    "checked",
    List("input")
  ), // Specifies that an "input" element should be pre-selected when the page loads (for type="checkbox" or type="radio")
  AttributeScope(
    "cite",
    List("blockquote", "del", "ins", "q")
  ), // Specifies a URL which explains the quote/deleted/inserted text
  AttributeScope(
    "class",
    List()
  ), // Specifies one or more classnames for an element (refers to a class in a style sheet)
  AttributeScope("cols", List("textarea")),    // Specifies the visible width of a text area
  AttributeScope("colspan", List("td", "th")), // Specifies the number of columns a table cell should span
  AttributeScope("content", List("meta")),     // Gives the value associated with the http-equiv or name attribute
  AttributeScope("contenteditable", List()),   // Specifies whether the content of an element is editable or not
  AttributeScope(
    "controls",
    List("audio", "video")
  ), // Specifies that audio/video controls should be displayed (such as a play/pause button etc)
  AttributeScope("coords", List("area")), // Specifies the coordinates of the area
  AttributeScope("data", List("object")), // Specifies the URL of the resource to be used by the object
  AttributeScope("data-*", List()),       // Used to store custom data private to the page or application
  AttributeScope("datetime", List("del", "ins", "time")), // Specifies the date and time
  AttributeScope(
    "default",
    List("track")
  ), // Specifies that the track is to be enabled if the user's preferences do not indicate that another track would be more appropriate
  AttributeScope(
    "defer",
    List("script")
  ), // Specifies that the script is executed when the page has finished parsing (only for external scripts)
  AttributeScope("dir", List()),                        // Specifies the text direction for the content in an element
  AttributeScope("dirname", List("input", "textarea")), // Specifies that the text direction will be submitted
  AttributeScope(
    "disabled",
    List("button", "fieldset", "input", "optgroup", "option", "select", "textarea")
  ), // Specifies that the specified element/group of elements should be disabled
  AttributeScope(
    "download",
    List("a", "area")
  ), // Specifies that the target will be downloaded when a user clicks on the hyperlink
  AttributeScope("draggable", List()), // Specifies whether an element is draggable or not
  AttributeScope(
    "enctype",
    List("form")
  ), // Specifies how the form-data should be encoded when submitting it to the server (only for method="post")
  AttributeScope("for", List("label", "output")), // Specifies which form element(s) a label/calculation is bound to
  AttributeScope(
    "form",
    List("button", "fieldset", "input", "label", "meter", "object", "output", "select", "textarea")
  ), // Specifies the name of the form the element belongs to
  AttributeScope(
    "formaction",
    List("button", "input")
  ), // Specifies where to send the form-data when a form is submitted. Only for type="submit"
  AttributeScope("headers", List("td", "th")), // Specifies one or more headers cells a cell is related to
  AttributeScope(
    "height",
    List("canvas", "embed", "iframe", "img", "input", "object", "video")
  ),                                     // Specifies the height of the element
  AttributeScope("hidden", List()),      // Specifies that an element is not yet, or is no longer, relevant
  AttributeScope("high", List("meter")), // Specifies the range that is considered to be a high value
  AttributeScope("href", List("a", "area", "base", "link")), // Specifies the URL of the page the link goes to
  AttributeScope("hreflang", List("a", "area", "link")),     // Specifies the language of the linked document
  AttributeScope(
    "http-equiv",
    List("meta")
  ),                                     // Provides an HTTP header for the information/value of the content attribute
  AttributeScope("id", List()),          // Specifies a unique id for an element
  AttributeScope("ismap", List("img")),  // Specifies an image as a server-side image map
  AttributeScope("kind", List("track")), // Specifies the kind of text track
  AttributeScope("label", List("track", "option", "optgroup")), // Specifies the title of the text track
  AttributeScope("lang", List()),                               // Specifies the language of the element's content
  AttributeScope(
    "list",
    List("input")
  ), // Refers to a "datalist" element that contains pre-defined options for an "input" element
  AttributeScope(
    "loop",
    List("audio", "video")
  ), // Specifies that the audio/video will start over again, every time it is finished
  AttributeScope("low", List("meter")),                      // Specifies the range that is considered to be a low value
  AttributeScope("max", List("input", "meter", "progress")), // Specifies the maximum value
  AttributeScope(
    "maxlength",
    List("input", "textarea")
  ), // Specifies the maximum number of characters allowed in an element
  AttributeScope(
    "media",
    List("a", "area", "link", "source", "style")
  ),                                             // Specifies what media/device the linked document is optimized for
  AttributeScope("method", List("form")),        // Specifies the HTTP method to use when sending form-data
  AttributeScope("min", List("input", "meter")), // Specifies a minimum value
  AttributeScope("multiple", List("input", "select")), // Specifies that a user can enter more than one value
  AttributeScope("muted", List("video", "audio")),     // Specifies that the audio output of the video should be muted
  AttributeScope(
    "name",
    List(
      "button",
      "fieldset",
      "form",
      "iframe",
      "input",
      "map",
      "meta",
      "object",
      "output",
      "param",
      "select",
      "textarea"
    )
  ),                                          // Specifies the name of the element
  AttributeScope("novalidate", List("form")), // Specifies that the form should not be validated when submitted
  AttributeScope("onabort", List("audio", "embed", "img", "object", "video")), // Script to be run on abort
  AttributeScope("onafterprint", List("body")),   // Script to be run after the document is printed
  AttributeScope("onbeforeprint", List("body")),  // Script to be run before the document is printed
  AttributeScope("onbeforeunload", List("body")), // Script to be run when the document is about to be unloaded
  AttributeScope("onblur", List()),               // Script to be run when the element loses focus
  AttributeScope(
    "oncanplay",
    List("audio", "embed", "object", "video")
  ), // Script to be run when a file is ready to start playing (when it has buffered enough to begin)
  AttributeScope(
    "oncanplaythrough",
    List("audio", "video")
  ), // Script to be run when a file can be played all the way to the end without pausing for buffering
  AttributeScope("onchange", List()),           // Script to be run when the value of the element is changed
  AttributeScope("onclick", List()),            // Script to be run when the element is being clicked
  AttributeScope("oncontextmenu", List()),      // Script to be run when a context menu is triggered
  AttributeScope("oncopy", List()),             // Script to be run when the content of the element is being copied
  AttributeScope("oncuechange", List("track")), // Script to be run when the cue changes in a "track" element
  AttributeScope("oncut", List()),              // Script to be run when the content of the element is being cut
  AttributeScope("ondblclick", List()),         // Script to be run when the element is being double-clicked
  AttributeScope("ondrag", List()),             // Script to be run when the element is being dragged
  AttributeScope("ondragend", List()),          // Script to be run at the end of a drag operation
  AttributeScope("ondragenter", List()), // Script to be run when an element has been dragged to a valid drop target
  AttributeScope("ondragleave", List()), // Script to be run when an element leaves a valid drop target
  AttributeScope("ondragover", List()),  // Script to be run when an element is being dragged over a valid drop target
  AttributeScope("ondragstart", List()), // Script to be run at the start of a drag operation
  AttributeScope("ondrop", List()),      // Script to be run when dragged element is being dropped
  AttributeScope("ondurationchange", List("audio", "video")), // Script to be run when the length of the media changes
  AttributeScope(
    "onemptied",
    List("audio", "video")
  ), // Script to be run when something bad happens and the file is suddenly unavailable (like unexpectedly disconnects)
  AttributeScope(
    "onended",
    List("audio", "video")
  ), // Script to be run when the media has reach the end (a useful event for messages like "thanks for listening")
  AttributeScope(
    "onerror",
    List("audio", "body", "embed", "img", "object", "script", "style", "video")
  ),                                 // Script to be run when an error occurs
  AttributeScope("onfocus", List()), // Script to be run when the element gets focus
  AttributeScope(
    "onhashchange",
    List("body")
  ),                                    // Script to be run when there has been changes to the anchor part of the a URL
  AttributeScope("oninput", List()),    // Script to be run when the element gets user input
  AttributeScope("oninvalid", List()),  // Script to be run when the element is invalid
  AttributeScope("onkeydown", List()),  // Script to be run when a user is pressing a key
  AttributeScope("onkeypress", List()), // Script to be run when a user presses a key
  AttributeScope("onkeyup", List()),    // Script to be run when a user releases a key
  AttributeScope(
    "onload",
    List("body", "iframe", "img", "input", "link", "script", "style")
  ),                                                      // Script to be run when the element is finished loading
  AttributeScope("onloadeddata", List("audio", "video")), // Script to be run when media data is loaded
  AttributeScope(
    "onloadedmetadata",
    List("audio", "video")
  ), // Script to be run when meta data (like dimensions and duration) are loaded
  AttributeScope(
    "onloadstart",
    List("audio", "video")
  ), // Script to be run just as the file begins to load before anything is actually loaded
  AttributeScope("onmousedown", List()),     // Script to be run when a mouse button is pressed down on an element
  AttributeScope("onmousemove", List()),     // Script to be run as long as the  mouse pointer is moving over an element
  AttributeScope("onmouseout", List()),      // Script to be run when a mouse pointer moves out of an element
  AttributeScope("onmouseover", List()),     // Script to be run when a mouse pointer moves over an element
  AttributeScope("onmouseup", List()),       // Script to be run when a mouse button is released over an element
  AttributeScope("onmousewheel", List()),    // Script to be run when a mouse wheel is being scrolled over an element
  AttributeScope("onoffline", List("body")), // Script to be run when the browser starts to work offline
  AttributeScope("ononline", List("body")),  // Script to be run when the browser starts to work online
  AttributeScope("onpagehide", List("body")), // Script to be run when a user navigates away from a page
  AttributeScope("onpageshow", List("body")), // Script to be run when a user navigates to a page
  AttributeScope("onpaste", List()),          // Script to be run when the user pastes some content in an element
  AttributeScope(
    "onpause",
    List("audio", "video")
  ), // Script to be run when the media is paused either by the user or programmatically
  AttributeScope("onplay", List("audio", "video")),    // Script to be run when the media has started playing
  AttributeScope("onplaying", List("audio", "video")), // Script to be run when the media has started playing
  AttributeScope("onpopstate", List("body")),          // Script to be run when the window's history changes.
  AttributeScope(
    "onprogress",
    List("audio", "video")
  ), // Script to be run when the browser is in the process of getting the media data
  AttributeScope(
    "onratechange",
    List("audio", "video")
  ), // Script to be run each time the playback rate changes (like when a user switches to a slow motion or fast forward mode).
  AttributeScope("onreset", List("form")),  // Script to be run when a reset button in a form is clicked.
  AttributeScope("onresize", List("body")), // Script to be run when the browser window is being resized.
  AttributeScope("onscroll", List()),       // Script to be run when an element's scrollbar is being scrolled
  AttributeScope(
    "onsearch",
    List("input")
  ), // Script to be run when the user writes something in a search field (for "input type="search"")
  AttributeScope(
    "onseeked",
    List("audio", "video")
  ), // Script to be run when the seeking attribute is set to false indicating that seeking has ended
  AttributeScope(
    "onseeking",
    List("audio", "video")
  ), // Script to be run when the seeking attribute is set to true indicating that seeking is active
  AttributeScope("onselect", List()), // Script to be run when the element gets selected
  AttributeScope(
    "onstalled",
    List("audio", "video")
  ), // Script to be run when the browser is unable to fetch the media data for whatever reason
  AttributeScope("onstorage", List("body")), // Script to be run when a Web Storage area is updated
  AttributeScope("onsubmit", List("form")),  // Script to be run when a form is submitted
  AttributeScope(
    "onsuspend",
    List("audio", "video")
  ), // Script to be run when fetching the media data is stopped before it is completely loaded for whatever reason
  AttributeScope(
    "ontimeupdate",
    List("audio", "video")
  ), // Script to be run when the playing position has changed (like when the user fast forwards to a different point in the media)
  AttributeScope("ontoggle", List("details")), // Script to be run when the user opens or closes the "details" element
  AttributeScope(
    "onunload",
    List("body")
  ), // Script to be run when a page has unloaded (or the browser window has been closed)
  AttributeScope(
    "onvolumechange",
    List("audio", "video")
  ), // Script to be run each time the volume of a video/audio has been changed
  AttributeScope(
    "onwaiting",
    List("audio", "video")
  ), // Script to be run when the media has paused but is expected to resume (like when the media pauses to buffer more data)
  AttributeScope("onwheel", List()),        // Script to be run when the mouse wheel rolls up or down over an element
  AttributeScope("open", List("details")),  // Specifies that the details should be visible (open) to the user
  AttributeScope("optimum", List("meter")), // Specifies what value is the optimal value for the gauge
  AttributeScope(
    "pattern",
    List("input")
  ), // Specifies a regular expression that an "input" element's value is checked against
  AttributeScope(
    "placeholder",
    List("input", "textarea")
  ), // Specifies a short hint that describes the expected value of the element
  AttributeScope(
    "poster",
    List("video")
  ), // Specifies an image to be shown while the video is downloading, or until the user hits the play button
  AttributeScope(
    "preload",
    List("audio", "video")
  ), // Specifies if and how the author thinks the audio/video should be loaded when the page loads
  AttributeScope("readonly", List("input", "textarea")), // Specifies that the element is read-only
  AttributeScope(
    "rel",
    List("a", "area", "form", "link")
  ), // Specifies the relationship between the current document and the linked document
  AttributeScope(
    "required",
    List("input", "select", "textarea")
  ),                                        // Specifies that the element must be filled out before submitting the form
  AttributeScope("reversed", List("ol")),   // Specifies that the list order should be descending (9,8,7...)
  AttributeScope("rows", List("textarea")), // Specifies the visible number of lines in a text area
  AttributeScope("rowspan", List("td", "th")), // Specifies the number of rows a table cell should span
  AttributeScope("sandbox", List("iframe")),   // Enables an extra set of restrictions for the content in an "iframe"
  AttributeScope(
    "scope",
    List("th")
  ), // Specifies whether a header cell is a header for a column, row, or group of columns or rows
  AttributeScope("selected", List("option")), // Specifies that an option should be pre-selected when the page loads
  AttributeScope("shape", List("area")),      // Specifies the shape of the area
  AttributeScope(
    "size",
    List("input", "select")
  ), // Specifies the width, in characters (for "input") or specifies the number of visible options (for "select")
  AttributeScope("sizes", List("img", "link", "source")), // Specifies the size of the linked resource
  AttributeScope("span", List("col", "colgroup")),        // Specifies the number of columns to span
  AttributeScope(
    "spellcheck",
    List()
  ), // Specifies whether the element is to have its spelling and grammar checked or not
  AttributeScope(
    "src",
    List("audio", "embed", "iframe", "img", "input", "script", "source", "track", "video")
  ),                                        // Specifies the URL of the media file
  AttributeScope("srcdoc", List("iframe")), // Specifies the HTML content of the page to show in the "iframe"
  AttributeScope(
    "srclang",
    List("track")
  ), // Specifies the language of the track text data (required if kind="subtitles")
  AttributeScope("srcset", List("img", "source")), // Specifies the URL of the image to use in different situations
  AttributeScope("start", List("ol")),             // Specifies the start value of an ordered list
  AttributeScope("step", List("input")),           // Specifies the legal number intervals for an input field
  AttributeScope("style", List()),                 // Specifies an inline CSS style for an element
  AttributeScope("tabindex", List()),              // Specifies the tabbing order of an element
  AttributeScope(
    "target",
    List("a", "area", "base", "form")
  ), // Specifies the target for where to open the linked document or where to submit the form
  AttributeScope("title", List()),     // Specifies extra information about an element
  AttributeScope("translate", List()), // Specifies whether the content of an element should be translated or not
  AttributeScope(
    "type",
    List("a", "button", "embed", "input", "link", "menu", "object", "script", "source", "style")
  ),                                               // Specifies the type of element
  AttributeScope("usemap", List("img", "object")), // Specifies an image as a client-side image map
  AttributeScope(
    "value",
    List("button", "input", "li", "option", "meter", "progress", "param")
  ), // Specifies the value of the element
  AttributeScope(
    "width",
    List("canvas", "embed", "iframe", "img", "input", "object", "video")
  ), // Specifies the width of the element
  AttributeScope(
    "wrap",
    List("textarea")
  ), // Specifies how the text in a text area is to be wrapped when submitted in a form
).map(as => Tuple2(as.attr, as.scope)).toMap
