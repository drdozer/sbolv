package talk

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js._
import org.scalajs.dom._
import scala.util.parsing.combinator._

/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object SBOLv extends ShortcodeProvider with Cds.SCProvider
                                       with Promoter.SCProvider
                                       with RibosomeEntrySite.SCProvider
                                       with Terminator.SCProvider
                                       with FixedWidth.SCProvider {

  import Enhancements._

  @JSExport
  def shortcodes(): Unit = {
    val me = Dynamic(document).currentScript.asInstanceOf[HTMLScriptElement]
    val parent = me.parentNode

    applyShortcodes(parent)
  }

  def applyShortcodes(node: Node): Unit = {
    // constants hacks as the Node constants are exposed as def-s.
    val TEXT_NODE = Node.TEXT_NODE
    val ELEMENT_NODE = Node.ELEMENT_NODE
    node.nodeType match {
      case TEXT_NODE =>
        val expanded = expandShortcodes(node.nodeValue)
        if(expanded.length == 1 && expanded.head == node) {
          // no change
        } else {
          val parent = node.parentNode
          val nextNode = node.nextSibling
          parent.removeChild(node)
          for(n <- expanded)
            parent.insertBefore(n, nextNode)
        }
      case ELEMENT_NODE =>
        for(n <- node.childNodes.nodes)
          applyShortcodes(n)
    }
  }

  def expandShortcodes(raw: String): Seq[Node] = {
    val parse = shortcodeParser.parseAll(shortcodeParser.allText, raw)
    parse.get
  }

  object shortcodeParser extends ShortcodeParser {
    override type Attribute = (TagName, String)
    override type Shortcode = Node
    override type TagName = String
    override type Content = Node
    override type Flattened = List[Node]

    override def Content(txt: String) = document.createTextNode(txt)

    override def TagName(name: String) = name

    override def Attribute(tagName: TagName, tagValue: String) = {
      (tagName, tagValue)
    }

    override def Shortcode(tagName: TagName, attributes: Seq[shortcodeParser.Attribute], content: Option[String]) =
    {
      shortcodeHandlers(talk.Shortcode(tagName, attributes, content)).getOrElse {
        import scalatags.JsDom.all._

        span(style := "color: red;")(s"Unknown shortcode: ${tagName}").render
      }
    }

    override def Flatten(scc: List[Shortcode ~ Content]) = for(c <- scc; n <- Flatten(c)) yield n

    def Flatten(scc: Shortcode ~ Content): Flattened = scc match {
      case sc ~ c => List(sc, c)
    }

    override def Flatten(ct: Content, fl: Flattened) = ct :: fl
  }
}

case class Shortcode(tagName: String, attrs: Seq[(String, String)], content: Option[String])

abstract class ShortcodeProvider {
  def shortcodeHandlers(sc: Shortcode): Option[Node] = None

  def asDirection(dir: Option[String]) = dir match {
    case Some(d) => d match {
      case "-" | "<" | "left" | "leftwards" | "reverse" | "bottom" => Leftwards
      case _ => Rightwards
    }
    case _ => Rightwards
  }
}

trait ShortcodeParser extends RegexParsers {
  override val skipWhitespace = false

  lazy val leftBox: Parser[String] = "["
  lazy val rightBox: Parser[String] = "]"
  lazy val allNotLeftBox: Parser[String] = """[^\[]*""".r
  lazy val dblQuote: Parser[String] = "\""
  lazy val allNotDblQuote: Parser[String] = """[^"]*""".r
  lazy val slash: Parser[String] = "/"
  lazy val equ: Parser[String] = "="

  lazy val whitespace: Parser[String] = """\s+""".r


  lazy val tagName = """[a-zA-Z_][\w\d_]+""".r ^^ {
    case tn => TagName(tn)
  } named "tagName"

  lazy val attribute = whitespace.? ~ tagName ~ whitespace.? ~ equ ~ whitespace.? ~ dblQuote ~ allNotDblQuote ~ dblQuote ^^ {
    case ws1 ~ an ~ ws2 ~ eq1 ~ ws3 ~ q1 ~ av ~ q2 => Attribute(an, av)
  }
  lazy val attributeList = attribute.*

  lazy val shortCodeOpeningContent = whitespace.? ~ tagName ~ attributeList ^^ {
    case ws1 ~ tn ~ attrs => (tn, attrs)
  }

  lazy val shortCodeOpening = leftBox ~ shortCodeOpeningContent ~ whitespace.? ~ rightBox ^^ {
    case lb ~ tn_attrs ~ ws1 ~ rb => tn_attrs
  }
  lazy val shortCodeClosing = leftBox ~ whitespace.? ~ slash ~ whitespace.? ~ tagName ~ whitespace.? ~ rightBox ^^ {
    case lb ~ ws1 ~ sl ~ ws2 ~ tn ~ ws3 ~ rb => tn
  }
  lazy val shortCodeOpenClose = shortCodeOpening ~ allNotLeftBox ~ shortCodeClosing >> {
    case (tnOpening, attrs) ~ content ~ tnClosing if tnOpening == tnClosing =>
      success(Shortcode(tnOpening, attrs, Some(content)))
    case (tnOpening, attrs) ~ content ~ tnClosing =>
      err(s"Shortcode opening and closing tags don't match: ${tnOpening} and ${tnClosing}")
  }

  lazy val shortCodeSelfClosing = leftBox ~ shortCodeOpeningContent ~ whitespace.? ~ slash ~ whitespace.? ~ rightBox ^^ {
    case lb ~ tn_attrs ~ ws1 ~ sl1 ~ ws2 ~ rb => Shortcode(tn_attrs._1, tn_attrs._2, None)
  }
  
  lazy val shortCode = shortCodeOpenClose | shortCodeSelfClosing

  lazy val content = allNotLeftBox ^^ {
    case txt => Content(txt)
  }
  
  lazy val allText = content ~ (shortCode ~ content).* ^^ {
    case cl ~ scc => Flatten(cl, Flatten(scc))
  }

  type Content
  type Attribute
  type TagName
  type Shortcode
  type Flattened

  def Content(txt: String): Content
  def TagName(name: String): TagName
  def Attribute(tagName: TagName, tagValue: String): Attribute
  def Shortcode(tagName: TagName, attributes: Seq[Attribute], content: Option[String]): Shortcode
  def Flatten(scc: List[Shortcode ~ Content]): Flattened
  def Flatten(ct: Content, fl: Flattened): Flattened
}
