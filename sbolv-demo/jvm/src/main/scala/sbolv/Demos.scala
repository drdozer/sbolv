package sbolv

import scalatags.Text.all._

/**
 *
 *
 * @author Matthew Pocock
 */
trait Demos {

  def cds = demoTemplate(
    titleText = "CDS",
    headingText = "Complement Determining Sequence Glyph",
    descriptionText = "The SBOLv CDS glyph is a rectangle with a triangular end.",
    parametersText = "The CDS glyph has a variable width, depth, arrow head size.",
    demoClassName = "CdsDemo")

  def primerBindingSite = demoTemplate(
    titleText = "Primer Binding Site",
    headingText = "Primer Binding Site Glyph",
    descriptionText = "The SBOLv primer binding site glyph is a line with a tick.",
    parametersText = "The primer binding site glyph has a variable width, depth and tick position.",
    demoClassName = "PrimerBindingSiteDemo")

  def promoter = demoTemplate(
    titleText = "Promoter",
    headingText = "Promoter Glyph",
    descriptionText = "The SBOLv Promoter glyph is an arrow with a horizontal and vertical segment.",
    parametersText = "The Promoter glyph has a variable vertical size, horizontal size, arrow height and arrow width.",
    demoClassName = "PromoterDemo")

  def proteaseSite = demoTemplate(
    titleText = "Protease Site",
    headingText = "Protease Site Glyph",
    descriptionText = "The SBOLv protease site glyph is a stem topped with an X.",
    parametersText = "The protease site glyph has a variable width, depth and stem length.",
    demoClassName = "ProteaseSiteDemo")

  def proteinStabilityElement = demoTemplate(
    titleText = "Protein Stability Element",
    headingText = "Protein Stability Element Glyph",
    descriptionText = "The SBOLv Protein Stability Element glyph is a stem topped with an ellipse.",
    parametersText = "The protein stability element glyph has a variable width, depth and stem length.",
    demoClassName = "ProteinStabilityDemo")

  def ribonucleaseSite = demoTemplate(
    titleText = "Ribonuclease Site",
    headingText = "Ribonuclease Site Glyph",
    descriptionText = "The SBOLv ribonuclease site glyph is a dashed stem topped with an X.",
    parametersText = "The ribonuclease site glyph has a variable width, depth and stem length.",
    demoClassName = "RibonucleaseSiteDemo")

  def rnaStabilityElement = demoTemplate(
    titleText = "RNA Stability Element",
    headingText = "RNA Stability Element Glyph",
    descriptionText = "The SBOLv RNA Stability Element glyph is a dashed stem topped with an ellipse.",
    parametersText = "The RNA stability element glyph has a variable width, depth and stem length.",
    demoClassName = "RnaStabilityDemo")

  def terminator = demoTemplate(
    titleText = "Terminator",
    headingText = "Terminator Glyph",
    descriptionText = "The SBOLv Terminator glyph is a T.",
    parametersText = "The SBOLv Terminator element glyph has a variable width and depth.",
    demoClassName = "TerminatorDemo")

  def index = mainTemplate("SBOLv Web Widgets")(
    raw("""    <p>
    |        The SBOLv Web Widgets toolkit provides SBOLv symbols directly in your browser. They are rendered as interactive
    |        SVG, and can be fully styled using standard SVG CSS.
    |    </p>
    |
    |    <p>
    |        Two APIs are provided for working with the SBOLv Web Widgets. For people writing web pages who need to embed
    |        static SBOLv symbols, there is a shortcodes hook. For people building powerful, interactive visualisations,
    |        there is full programatic access to the widgets themselves.
    |    </p>
    |
    |    <p>
    |        The library is under active development. At this time, there is support for
    |            <a href="ribosomeEntrySite.html">res</a>,
    |            <a href="cds.html">cds</a>,
    |            <a href="promoter.html">promoter</a> and
    |            <a href="terminator.html">term</a>
    |            symbols, together with
    |            <a href="fixedOrProportional.html">fixed-width</a> layout.
    |    </p>
    |
    |    <p>
    |        Shortcodes are easy to use. The shortcodes are written like HTML tags but with square boxes. So for example, to
    |        render a cds, use [cds][/cds]. To activate shortcode handling, include <a href="./public/javascripts/sbolv_js-opt.js">sbolv.js</a> your page.
    |        Then in each element were you want shortcodes to be processed, add a script that runs
    |        <code>SBOLv().shortcodes()</code> as the last child.
    |    </p>
    |    <p>
    |        Shortcodes can render all supported SBOLv symbols, including [cds]cds[/cds], [promoter]pro[/promoter], [res]res[/res] and [term]term[/term].
    |        It can also lay out a series of symbols [sbolv width="40"]p> r> c> t> c< r< t< p<[/sbolv] inline.
    |
    |        <script>
    |        SBOLv().shortcodes()
    |        </script>
    |    </p>""".stripMargin)
  )
  
  def fixedOrProportional = mainTemplate("Fixed width and proportional width glyphs")(
    raw(
      """
        |    <p>
        |        SBOL Visual symbols can be rendered fixed-width or proportional width.
        |        In Fixed width, all symbols take up the same horizontal space and are drawn one next to the other.
        |        In proportional width, symbols are scaled horizontally relative to the underlying DNA, and are spaced out along the DNA.
        |    </p>
        |
        |    <div id="fixed_width_example">
        |        <p>
        |            This is a fixed-width SBOLv display. You can add new items by clicking on the buttons. Then click a glyph
        |            to edit its label(s).
        |        </p>
        |
        |        <div>
        |            <div style="display: inline-block">
        |                <p>Length: <span class="length"></span></p>
        |                <input type="range" name="length" min="40" max="100" value="40" class="length_slider">
        |            </div>
        |
        |            <div style="display: inline-block">
        |                <div class="rightwards">
        |                    <button value="promoter_rightwards">[promoter dir=">"/]</button>
        |                    <button value="res_rightwards">[res dir=">"/]</button>
        |                    <button value="cds_rightwards">[cds dir=">"/]</button>
        |                    <button value="term_rightwards">[term dir=">"/]</button>
        |                    <button value="pbs_rightwards">[pbs dir=">"/]</button>
        |                </div>
        |                <div class="leftwards">
        |                    <button value="promoter_leftwards">[promoter dir="<"/]</button>
        |                    <button value="res_leftwards">[res dir="<"/]</button>
        |                    <button value="cds_leftwards">[cds dir="<"/]</button>
        |                    <button value="term_leftwards">[term dir="<"/]</button>
        |                    <button value="pbs_leftwards">[pbs dir="<"/]</button>
        |                </div>
        |                <script>SBOLv().shortcodes()</script>
        |            </div>
        |        </div>
        |        <div>
        |            <svg width = 600 height = 300 class="glyphs" style="background-color: whitesmoke">
        |                <g class="track" transform="translate(0, 150)"></g>
        |            </svg>
        |        </div>
        |    </div>
        |
        |    <div id="shortcode_example">
        |        <p>You can embed multiple elements using their shortcodes.</p>
        |        <p>[promoter/][cds/]</p>
        |        <p>If you use individual shortcodes, they are each laid out independently of one-another.
        |            You can render a string of symbols using the [sbolv/] shortcode. The content is a series of pigeoncad
        |        single-letter codes and orientation indicated with >/<, separated by spaces.</p>
        |        <p>[sbolv]p> r> c>"gfp" t> t< r< c<"lacI" p<[/sbolv]</p>
        |
        |        <div style="border-color: black; border-style: solid;">
        |            <p>You can embed multiple elements using their shortcodes.</p>
        |            <p>[promoter/][cds/]</p>
        |            <p>If you use individual shortcodes, they are each laid out independently of one-another.
        |                You can render a string of symbols using the [sbolv/] shortcode. The content is a series of pigeoncad
        |            single-letter codes and orientation indicated with >/<, separated by spaces.</p>
        |            <p>[sbolv]p> r> c>"gfp" t> t< r< c<"lacI" p<[/sbolv]</p>
        |            <script>SBOLv().shortcodes()</script>
        |        </div>
        |    </div>
        |
        |    <script>
        |    FixedOrProportionalDemo().wireFixedWidthExample("fixed_width_example");
        |    </script>
        |
      """.stripMargin)
  )

  def pigeonParser = mainTemplate("Pigeon Parser")(
    raw(
      """
        | <script src='../public/javascript/pigeon-parser.js' type='text/javascript'></script>
        |
        |    <textarea id="pigeon" rows=32 cols=64>
        |o A 1
        |<t t
        |<c c2 2
        |<r r1 1
        |<p p1 6
        |p p2 2
        |r r2 5
        |c c1 6
        |t t
        |o B 1
        |v KanR 1
        |# Arcs
        |c1 rep p1
        |c2 rep p2
        |y rep c1-p1
        |x rep c2-p2
        |    </textarea>
        |
        |    <textarea id="json" rows=32 cols=64 disabled="disabled">
        |    </textarea>
        |
        |    <div id="example">
        |
        |        <div style="display: inline-block">
        |            <p>Length: <span class="length"></span></p>
        |            <input type="range" name="length" min="40" max="100" value="40" class="length_slider">
        |        </div>
        |
        |        <div>
        |            <svg width=600 height=300 class="glyphs" style="background-color: whitesmoke">
        |                <g class="track" transform="translate(0, 150)"></g>
        |            </svg>
        |        </div>
        |    </div>
        |
        |    <script>
        |    PigeonParserDemo().wireExample("example");
        |    </script>
      """.stripMargin)
  )

  private val title = "title".tag(scalatags.generic.Namespace.htmlNamespaceConfig)
  private val media = "media".attr

  def demoTemplate(titleText: String,
                   headingText: String,
                   descriptionText: String,
                   parametersText: String,
                   demoClassName: String) =
    mainTemplate(titleText)(
      h1(headingText),
      p(descriptionText),
      div(id := "scaled_example")(
        h2("Glyph Designer"),
        p(parametersText),
        div(`class` := "sliders"),
        div(`class` := "glyphs")
      ),
      div(id := "shortcode_example")(
        h2("Shortcodes")
      ),
      script(s"""$demoClassName().wireScaledExample('scaled_example', 'shortcode_example');""")
    )

  def mainTemplate(titleTxt: String)(htmlBits: Frag*) =
    html(
      head(
        title(titleTxt),
        link(rel := "stylesheet", media := "screen", href := "../public/stylesheets/main.css"),
        link(rel := "stylesheet", media := "screen", href := "../public/stylesheets/sbolv.css")
      ),
      body(
        script(src := "../public/javascript/sbolv-demo-fastopt.js", `type` := "text/javascript"),
        htmlBits
      )
    )

}
