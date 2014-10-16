package sbolv.geom

import org.scalajs.dom.SVGRect
import sbolv.vis.Interval


/**
 * A box of screen. Used for simple boundary, padding and layout tasks.
 *
 * @param left
 * @param right
 * @param top
 * @param bottom
 */
case class Box(left: Double, right: Double, top: Double, bottom: Double) {
  self =>

  def width = right - left
  def height = bottom - top

  def horizontal = Interval(left, right)
  def vertical = Interval(top, bottom)

  def withInset(pad: Double): Box = withInset(pad, pad)
  def withInset(padH: Double, padV: Double): Box = withInset(padH, padH, padV, padV)
  def withInset(padL: Double, padR: Double, padT: Double, padB: Double): Box = Box(
    left + padL,
    right - padR,
    top + padT,
    bottom - padB)

  def withOutset(pad: Double): Box = withOutset(pad, pad)
  def withOutset(padH: Double, padV: Double): Box = withOutset(padH, padH, padV, padV)
  def withOutset(padL: Double, padR: Double, padT: Double, padB: Double): Box = Box(
      left - padL,
      right + padR,
      top - padT,
      bottom + padB)

  def moveToOrigin = Box(
    left = 0,
    right = width,
    top = 0,
    bottom = height)

  def translate(p: Point2): Box = Box(
    left = left + p.x,
    right = right + p.x,
    top = top + p.y,
    bottom = bottom + p.y
  )

  def union(box: Box): Box = Box(
    left = Math.min(left, box.left),
    right = Math.max(right, box.right),
    top = Math.min(top, box.top),
    bottom = Math.max(bottom, box.bottom)
  )

  def places(toPosition: Box): Box.Placement = new Box.Placement(self, toPosition)

  def topLeft: Point2 = Point2(x = left, y = top)
  def centre: Point2 = Point2(x = (left + right) / 2.0,
                              y = (top + bottom) / 2.0)

  def contains(p: Point2): Boolean =
    (this.left <= p.x && this.right >= p.x) &&
      (this.top <= p.y && this.bottom >= p.y)

  def overlapsCross(p: Point2): Boolean =
    (this.left <= p.x && this.right >= p.x) ||
      (this.top <= p.y && this.bottom >= p.y)

  def overlaps(b: Box): Boolean = ! this.noOverlap(b)

  def noOverlap(b: Box): Boolean =
    ((this.right < b.left) || (this.left > b.right)) ||
      ((this.bottom < b.top) || (this.top > b.bottom))
}

object Box {
  case class Placement(parent: Box, child: Box, hPos: Positioning = Inline, vPos: Positioning = Inline) {
    def at(alphaH: Double, alphaV: Double): Point2 = Point2(
      x = alphaH * parent.width + parent.left - hPos(alphaH) * child.width - child.left,
      y = alphaV * parent.height + parent.top - vPos(alphaV) * child.height - child.top)

    def hOutline = copy(hPos = Outline)
    def vOutline = copy(vPos = Outline)
  }

  sealed trait Positioning { def apply(d: Double): Double }
  case object Inline extends Positioning { def apply(d: Double) = d }
  case object Outline extends Positioning { def apply(d: Double) = 1.0 - d }

  def apply(width: Double, height: Double): Box = Box(
    left = 0,
    right = width,
    top = 0,
    bottom = height)

  def apply(rect: SVGRect): Box = Box(
    left = rect.x,
    top = rect.y,
    right = rect.x + rect.width,
    bottom = rect.y + rect.height)

  def empty(p: Point2): Box = Box(left = p.x, right = p.x, top = p.y, bottom = p.y)

  def union(ifEmpty: =>Box)(boxes: Seq[Box]): Box = if(boxes.isEmpty) ifEmpty else boxes.reduce(_ union _)

}

object QuadTree {
  val splitSize = 10
  val meanBias = 0.5

  def apply(centre: Point2): QuadTree = QuadLeaf(centre, Nil)
}

sealed trait QuadTree {
  def overlap(toTest: Box, log: Boolean = false): Boolean
  def insert(toInsert: Box): QuadTree
  def paranoidOverlap(toTest: Box): Boolean
}

case class QuadLeaf(centre: Point2, children: List[Box]) extends QuadTree {
  def overlap(toTest: Box, log: Boolean = false): Boolean = {
    if(log) println(s"Testing $toTest on $children: ${children exists (_ overlaps toTest)}")
    children exists (_ overlaps toTest)
  }
  def paranoidOverlap(toTest: Box): Boolean = overlap(toTest)

  def insert(toInsert: Box): QuadTree = if(children.length < QuadTree.splitSize) {
    this.copy(children = toInsert :: children)
  } else {
    val (directChildren, indirectChildren) = children.partition(_ overlapsCross centre)
    val (toLeft, toRight) = indirectChildren.partition(_.right < centre.x)
    val (toLeftTop, toLeftBottom) = toLeft.partition(_.bottom < centre.y)
    val (toRightTop, toRightBottom) = toRight.partition(_.bottom < centre.y)

    def mostLeft(boxes: List[Box]): Double = (boxes map (_.left)).min
    def mostRight(boxes: List[Box]): Double = (boxes map (_.right)).max
    def mostTop(boxes: List[Box]): Double = (boxes map (_.top)).min
    def mostBottom(boxes: List[Box]): Double = (boxes map (_.bottom)).max

    val topLeft = if(toLeftTop.isEmpty) None else Some(
      QuadLeaf(
        Point2(
          x = (mostLeft(toLeftTop) + mostRight(toLeftTop)) * QuadTree.meanBias,
          y = (mostTop(toLeftTop) + mostBottom(toLeftTop)) * QuadTree.meanBias),
        toLeftTop))

    val topRight = if(toRightTop.isEmpty) None else Some(
      QuadLeaf(
        Point2(
          x = (mostLeft(toRightTop) + mostRight(toRightTop)) * (1.0 - QuadTree.meanBias),
          y = (mostTop(toRightTop) + mostBottom(toRightTop)) * QuadTree.meanBias),
        toRightTop))

    val bottomLeft = if(toLeftBottom.isEmpty) None else Some(
      QuadLeaf(
        Point2(
          x = (mostLeft(toLeftBottom) + mostRight(toLeftBottom)) * QuadTree.meanBias,
          y = (mostTop(toLeftBottom) + mostBottom(toLeftBottom)) * (1.0 - QuadTree.meanBias)),
        toLeftBottom
      ))

    val bottomRight = if(toRightBottom.isEmpty) None else Some(
      QuadLeaf(
        Point2(
          x = (mostLeft(toRightBottom) + mostRight(toRightBottom)) * (1.0 - QuadTree.meanBias),
          y = (mostTop(toRightBottom) + mostBottom(toRightBottom)) * (1.0 - QuadTree.meanBias)),
        toRightBottom))

    def countDefined[T](o: Option[T]): Int = if(o.isDefined) 1 else 0

    val nDefined =
      countDefined(topLeft) +
      countDefined(topRight) +
      countDefined(bottomLeft) +
      countDefined(bottomRight)

    def absDefined(o: Option[Point2]): Point2 = o.fold(Point2.zero)(_.abs)
    val absAll =
      absDefined(topLeft.map(_.centre - centre)) +
      absDefined(topRight.map(_.centre - centre)) +
      absDefined(bottomLeft.map(_.centre - centre)) +
      absDefined(bottomRight.map(_.centre - centre))
    val absScaled = absAll * (1.0 / nDefined.toDouble)

    QuadNode(
      centre = centre,
      children = Cruciform(directChildren, centre),
      topLeft = topLeft.getOrElse(QuadTree(centre + absScaled * Point2(-1.0, -1.0))),
      topRight = topRight.getOrElse(QuadTree(centre + absScaled * Point2(+1.0, -1.0))),
      bottomLeft = bottomLeft.getOrElse(QuadTree(centre + absScaled * Point2(-1.0, +1.0))),
      bottomRight = bottomRight.getOrElse(QuadTree(centre + absScaled))
    ) insert toInsert
  }

}

case class QuadNode(centre: Point2,
                    children: Cruciform,
                    topLeft: QuadTree,
                    topRight: QuadTree,
                    bottomLeft: QuadTree,
                    bottomRight: QuadTree) extends QuadTree {
  def overlap(toTest: Box, log: Boolean = false): Boolean = {
    if(log) println(s"Testing $toTest")
    if(children.overlaps(toTest, centre)) {
      if(log) println("hit children")
      true
    } else {
      val isLeftOf = toTest.left < centre.x
      val isTopOf = toTest.top < centre.y
      val isRightOf = toTest.right > centre.x
      val isBottomOf = toTest.bottom > centre.y

      val leftTopHit = isLeftOf && isTopOf && topLeft.overlap(toTest, log)
      val rightTopHit = isRightOf && isTopOf && topRight.overlap(toTest, log)
      val leftBottomHit = isLeftOf && isBottomOf && bottomLeft.overlap(toTest, log)
      val rightBottomHit = isRightOf && isBottomOf && bottomRight.overlap(toTest, log)

      leftTopHit || rightTopHit || leftBottomHit || rightBottomHit
    }
  }

  def paranoidOverlap(toTest: Box): Boolean = {
    children.paranoidOverlaps(toTest, centre) ||
      topLeft.paranoidOverlap(toTest) ||
      topRight.paranoidOverlap(toTest) ||
      bottomLeft.paranoidOverlap(toTest) ||
      bottomRight.paranoidOverlap(toTest)
  }

  override def insert(toInsert: Box) = if(toInsert overlapsCross centre) {
    this.copy(children = children.insert(toInsert, centre))
  } else {
    val isLeftOf = toInsert.right < centre.x
    val isTopOf = toInsert.bottom < centre.y

    (isLeftOf, isTopOf) match {
      case (true, true) =>
        this.copy(topLeft = topLeft.insert(toInsert))
      case (false, true) =>
        this.copy(topRight = topRight.insert(toInsert))
      case (true, false) =>
        this.copy(bottomLeft = bottomLeft.insert(toInsert))
      case (false, false) =>
        this.copy(bottomRight = bottomRight.insert(toInsert))
    }
  }
}

object Cruciform {
  def apply(children: List[Box], centre: Point2): Cruciform = Cruciform(
    horizontals = children.filter(b => isHorizontal(b, centre)),
    verticals = children.filter(b => isVertical(b, centre)),
    centre)

  def apply(horizontals: List[Box], verticals: List[Box], centre: Point2): Cruciform = Cruciform(
    horizontalBox = Box.union(Box.empty(centre))(horizontals),
    horizontals = horizontals,
    verticalBox = Box.union(Box.empty(centre))(verticals),
    verticals = verticals)

  def isHorizontal(toCheck: Box, centre: Point2) = toCheck.top < centre.y && toCheck.bottom > centre.y
  def isVertical(toCheck: Box, centre: Point2) = toCheck.left < centre.x && toCheck.right > centre.x
}

case class Cruciform(horizontalBox: Box, horizontals: List[Box], verticalBox: Box, verticals: List[Box]) {

  def overlaps(toCheck: Box, centre: Point2): Boolean = {
    val horHit = horizontalBox.overlaps(toCheck) && horizontals.exists(_ overlaps toCheck)
    val vertHit = verticalBox.overlaps(toCheck) && verticals.exists(_ overlaps toCheck)

    horHit || vertHit
  }

  def paranoidOverlaps(toCheck: Box, centre: Point2): Boolean = {
    horizontals.exists(_ overlaps toCheck) ||
      verticals.exists(_ overlaps toCheck)
  }

  def insert(toInsert: Box, centre: Point2): Cruciform = {
    val isH = Cruciform.isHorizontal(toInsert, centre)
    val hb = if(isH) horizontalBox.union(toInsert) else horizontalBox
    val hs = if(isH) toInsert::horizontals else horizontals

    val isV = Cruciform.isVertical(toInsert, centre)
    val vb = if(isV) verticalBox.union(toInsert) else verticalBox
    val vs = if(isV) toInsert::verticals else verticals

    Cruciform(hb, hs, vb, vs)
  }
}
