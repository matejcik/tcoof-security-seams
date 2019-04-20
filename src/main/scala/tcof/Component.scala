package tcof

trait Component extends WithName with Notifiable {
  override def toString: String =
    s"""Component "$name""""
}
