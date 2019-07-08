package cz.cuni.mff.d3s.enact

trait Component extends WithName with Notifiable {
  override def toString: String =
    s"""Component "$name""""
}
