package pneuma.proto

/** class to generate new names of the format 'base0000'
  * @note not thread-safe, uses mutable state, should probably be removed in later versions
  * @param base the root string of a generated name
  */
class Namer(val base: String) {
    private var index = 0
    def next() = synchronized {
        val name = f"$base$index%04d"
        index += 1
        name
    }
}
