package talk

sealed trait Alignment

case object BackboneCentred extends Alignment
case object BackboneStrandRelative extends Alignment
case object BackboneAbove extends Alignment
case object BackboneBelow extends Alignment