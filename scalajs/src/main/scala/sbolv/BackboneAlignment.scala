package sbolv

sealed trait BackboneAlignment

object BackboneAlignment {
  def parse(s: String): BackboneAlignment = s match {
    case "above_backbone" => AboveBackbone
    case "centred_on_backbone" => CentredOnBackbone
    case "below_backbone" => BelowBackbone
  }
}

case object AboveBackbone extends BackboneAlignment
case object CentredOnBackbone extends BackboneAlignment
case object BelowBackbone extends BackboneAlignment