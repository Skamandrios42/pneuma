package general

trait CompileError {
    def name: String
    def message: String
    def region: Region
    def format(content: Seq[String]): String
    override def toString: String = s"[$name in $region] $message"
}
