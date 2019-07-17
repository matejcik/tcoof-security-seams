package cz.cuni.mff.d3s.trust

/** Miscellaneous functions. */
private[trust] object Utils {

  /** Add indentation to multi-line string. */
  private[trust] def indent(str: String, level: Int) = {
    val indented = str.linesIterator.map("  " * level + _)
    val joined = indented.mkString("\n")
    joined + (if (str.endsWith("\n")) "\n" else "") // handle end newline
  }

  /** Counter for generated names. */
  private var randomNameIdx = 0

  /** Generate a new unique name.
    *
    * Stringifies and then increases the name counter.
    *
    * @return a string value of the current name counter.
    */
  private[trust] def randomName = {
    val name = f"<$randomNameIdx%06d>"
    randomNameIdx = randomNameIdx + 1
    name
  }
}
