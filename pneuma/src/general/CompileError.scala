package general

trait CompileError {
    def name: String
    def message: String
    def region: Region
    def format(content: Seq[String]): String = s"[$name in ${region.format(content)}] $message\n${region.mark(content, 2)}"
    override def toString: String = s"[$name in $region] $message"
}
