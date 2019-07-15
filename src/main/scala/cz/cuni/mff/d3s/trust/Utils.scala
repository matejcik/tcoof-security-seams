package cz.cuni.mff.d3s.trust

object Utils {

  /** Internal method used in pretty-printing solving results */
  private[trust] def indent(str: String, level: Int) = {
    val indented = str.lines.map("  " * level + _)
    val joined = indented.mkString("\n")
    joined + (if (str.endsWith("\n")) "\n" else "") // handle end newline
  }

  private var randomNameIdx = 0

  private[trust] def randomName = {
    val name = f"<$randomNameIdx%06d>"
    randomNameIdx = randomNameIdx + 1
    name
  }
}
