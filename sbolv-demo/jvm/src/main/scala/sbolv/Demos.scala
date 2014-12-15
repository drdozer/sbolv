package sbolv

import scalatags.Text.all._

/**
 *
 *
 * @author Matthew Pocock
 */
trait Demos {

  def cdsDemo = demoTemplate("CDS")(
    h1("CDS Glyph"),
    p("The SBOL CDS symbol is a rectangle with a triangular end"),
    div(id := "scaled_example")(
      h2("Glyph Designer"),
      p("The CDS symbol has a variable width, depth, arrow head size"),
      div(`class` := "sliders"),
      div(`class` := "glyphs")
    ),
    div(id := "shortcode_example")(
      h2("Shortcodes")
    ),
    script("""CdsDemo().wireScaledExample('scaled_example', 'shortcode_example');""")
  )

  private val title = "title".tag(scalatags.generic.Namespace.htmlNamespaceConfig)
  private val media = "media".attr

  def demoTemplate(titleTxt: String)(htmlBits: Frag*) =
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
