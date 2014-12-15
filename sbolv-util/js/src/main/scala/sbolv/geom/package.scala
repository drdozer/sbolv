package sbolv

import org.scalajs.dom.SVGRect
import sbolv.geom.Box

/**
 *
 *
 * @author Matthew Pocock
 */
package object geom {
  implicit class BoxEnhancer(_box: Box.type) {

    def apply(rect: SVGRect): Box = Box(
      left = rect.x,
      top = rect.y,
      right = rect.x + rect.width,
      bottom = rect.y + rect.height)

  }
}
