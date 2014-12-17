package sbolv

import sbolv.GlyphFamily.GlyphSpec
import sbolv.geom._

import org.scalajs.dom._
import org.scalajs.dom.extensions._

import rx._
import rx.ops._

import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom
import scalatags.JsDom.all._
import scalatags.ext._
import Framework._


/**
 *
 *
 * @author Matthew Pocock
 */
@JSExport
object FixedOrProportionalDemo {

  @JSExport
  def wireFixedWidthExample(divId: String): Unit = {
    val exampleDiv = document.getElementById(divId).asInstanceOf[HTMLDivElement]
    val lengthSlider = ReactiveSlider(exampleDiv.getElementsByClassName("length_slider").head.asInstanceOf[HTMLInputElement])
    val lengthSpan = exampleDiv.getElementsByClassName("length")
    val buttons = exampleDiv.getElementsByTagName("button").map(_.asInstanceOf[HTMLButtonElement])
    val fixedWidthSvg = exampleDiv.getElementsByClassName("glyphs").map(_.asInstanceOf[SVGSVGElement]).head
    val track = fixedWidthSvg.getElementsByClassName("track").map(_.asInstanceOf[SVGGElement]).head

    Obs(lengthSlider.valueAsNumber) {
      lengthSpan.foreach(_.textContent = lengthSlider.valueAsNumber().toString)
    }

    val glyphs = Var(IndexedSeq.empty[GlyphSpec])
    val fw = FixedWidth(lengthSlider.valueAsNumber map (_.toDouble), Var(CentredOnBackbone : BackboneAlignment), glyphs)
    track.modifyWith(fw.allGlyphs).render

    val selectedGlyphHolder: Var[Option[FixedWidth.GlyphHolder]] = Var(None)
    val selectedGlyphIndex: Rx[Option[Int]] = Rx {
      for(gh <- selectedGlyphHolder()) yield gh.index()
    }

    val selectedGlyphSpec = Rx {
      for(i <- selectedGlyphIndex()) yield glyphs()(i)
    }

    val differ = {
          val specToEdit = Var(None : Option[GlyphSpec])
          val specObs = Obs(specToEdit) {
            println(s"specObs: Spec to edit has changed to: ${specToEdit()}")
          }
          val updateObs = Obs(specToEdit, skipInitial = true) {
            println(s"updateObs: Checking if we shoukld update glyphs with ${specToEdit()}")
            for(
              indx <- selectedGlyphIndex();
              toEdit <- specToEdit() if glyphs()(indx) != toEdit
            ) {
              println(s"updateObs: Updating glyphs at $indx to $toEdit")
              glyphs() = glyphs().updated(indx, toEdit)
            }

          }

          var editWidget = Var(None : Option[Frag])
          println("****** this should get run once ***")

          (oldSelection: Option[GlyphSpec], newSelection: Option[GlyphSpec]) => {
            (oldSelection, newSelection) match {
              case (None, None) =>
                println("No selected glyph")
                editWidget() = None
                editWidget()
              case (None, Some(currentSpecToEdit)) =>
                println(s"Glyph spec has been selected as $currentSpecToEdit")
                specToEdit() = Some(currentSpecToEdit)
                def deleter = {
                  def deleteHandler(e: Event): Unit = {
                    for(indx <- selectedGlyphIndex()) {
                      val newGlyphs = glyphs().zipWithIndex.filter(_._2 != indx).unzip._1
                      // fixme: find a better way to clear the selection when items are deleted
                      selectedGlyphHolder() = None // clear the selection
                      glyphs() = newGlyphs
                    }
                  }
                  span(`class` := "delete_glyph"){
                    button("x", Events.click := deleteHandler _)
                  }
                }
                def flipper = {
                  def flipHandler(e: Event) = specToEdit() = specToEdit().map(s => s.copy(
                    horizontalOrientation = s.horizontalOrientation.reverse))

                  span(`class` := "direction_editor")(
                    button("flip", Events.click := flipHandler _)
                  )
                }
                def editor = {
                  def changeHandler(e: Event): Unit =
                    specToEdit() = specToEdit().map(s => s.copy(
                      label = Option(e.srcElement.asInstanceOf[HTMLInputElement].value.trim()) filter (_.length > 0)))

                  span(`class` := "label_editor")(
                    span(`class` := "label_description")("label"),
                    input(
                      `class` := "label_textField",
                      `type` := "text",
                      value := (specToEdit() flatMap (_.label) getOrElse ""),
                      Events.keypress := changeHandler _,
                      Events.input := changeHandler _)
                  )
                }
                editWidget() = Some(
                                      div(
                        `class` := "glyph_editor")(
                          deleter,
                          flipper,
                          editor
                      )
                )
                editWidget()
              case (Some(oldSpecToEdit), Some(currentSpecToEdit)) =>
                println(s"Glyph spec to edit has changed from $oldSpecToEdit to $currentSpecToEdit")
                specToEdit() = Some(currentSpecToEdit)
                editWidget()
              case (Some(oldSpecToEdit), None) =>
                println(s"Glyph spec has been deselected from $oldSpecToEdit")
                specToEdit() = None
                editWidget() = None
                editWidget()
              //      for(glyphSpec <- selectedGlyphSpec) {
              //
              //        val doUpdate = Obs(glyphSpec) {
              //          for(
              //            indx <- selectedGlyphIndex();
              //            toEdit <- glyphSpec()
              //          ) {
              //            println(s"Updating glyphs at $indx to $toEdit")
              //            glyphs() = glyphs().updated(indx, toEdit)
              //          }
              //        }
              //
              //
              //        def flipper = {
              //          def flipHandler(e: Event) = specToEdit() = specToEdit().map(s => s.copy(
              //            horizontalOrientation = s.horizontalOrientation.reverse))
              //
              //          span(`class` := "direction_editor")(
              //            button("flip", Events.click := flipHandler _)
              //          )
              //        }
              //
              //
              //        def deleter = {
              //          def deleteHandler(e: Event): Unit = {
              //            for(indx <- selectedGlyphIndex()) {
              //              val newGlyphs = glyphs().zipWithIndex.filter(_._2 != indx).unzip._1
              //              glyphs() = newGlyphs
              //            }
              //          }
              //          span(`class` := "delete_glyph"){
              //            button("x", Events.click := deleteHandler _)
              //          }
              //        }
              //
              //        div(
              //          `class` := "glyph_editor")(
              //            deleter,
              //            flipper,
              //            editor
              //        )
            }
          }
        }

    val selectionWidget = selectedGlyphSpec.diff(differ,_ => None)

    val labelEditorPosition = Var(Point2(0,0))
    val labelEditor = div(
      position.absolute,
      display := selectedGlyphHolder map { g => if(g.isDefined) "block" else "none"},
      JsDom.all.left := labelEditorPosition map (_.x px),
      JsDom.all.top := labelEditorPosition map (_.y px),
      RxT(selectionWidget))
    exampleDiv.modifyWith(labelEditor).render

    case class ClickAdder(gffw: GlyphFamily.GlyphType) extends GlyphFamily.GlyphType {
      def apply(boxWidthHeight: Rx[Double],
                horizontalOrientation: Rx[HorizontalOrientation],
                verticalOrientation: Rx[VerticalOrientation]): GlyphFamily = {

        import scala.scalajs.js.Dynamic

        val g = gffw(boxWidthHeight, horizontalOrientation, verticalOrientation)
        g.glyph.addEventListener("click", (e: Event) => {
          e.stopPropagation()
          selectedGlyphHolder() = Some(Dynamic(g.glyph.parentNode.parentNode).__sbolv_widget.asInstanceOf[FixedWidth.GlyphHolder])
          labelEditorPosition() = Point2(Dynamic(e).pageX.asInstanceOf[Double], Dynamic(e).pageY.asInstanceOf[Double])
        }, true)
        g
      }

      override def fixedWidthId = gffw.fixedWidthId
    }

    fixedWidthSvg.modifyWith(
      Events.click := {(me: Event) => selectedGlyphHolder() = None }
    ).render

    for(b <- buttons) {
      b.onclick = (e: Event) => {
        b.value match {
          case "promoter_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Promoter.GlyphType), Rightwards)
          case "promoter_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Promoter.GlyphType), Leftwards)
          case "cds_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Cds.GlyphType), Rightwards)
          case "cds_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Cds.GlyphType), Leftwards)
          case "res_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(RibosomeEntrySite.GlyphType), Rightwards)
          case "res_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(RibosomeEntrySite.GlyphType), Leftwards)
          case "term_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Terminator.GlyphType), Rightwards)
          case "term_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(Terminator.GlyphType), Leftwards)
          case "pbs_rightwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(PrimerBindingSite.GlyphType), Rightwards)
          case "pbs_leftwards" =>
            glyphs() = glyphs() :+ GlyphSpec(ClickAdder(PrimerBindingSite.GlyphType), Leftwards)
        }
      }
    }
  }


}
