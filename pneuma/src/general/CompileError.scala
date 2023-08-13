package general

trait CompileError {
    def name: String
    def message: String
    def meta: Metadata
    def format(content: Seq[String]): String = s"[$name in ${meta.format(content)}] $message\n${meta.mark(content, 2)}"
    override def toString: String = s"[$name in $meta] $message"
}
