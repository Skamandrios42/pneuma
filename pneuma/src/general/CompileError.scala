package general

trait CompileError {
    def name: String
    def message: String
    def region: Region
    def format(content: Seq[String]) = s"$this\n ... ${region.extract(content)}"
    override def toString: String = s"[$name in $region] $message"
}