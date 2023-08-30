package general

import java.time.Instant
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import fansi.{Color, EscapeAttr}
import Logger.{Level}

object Logger {
    enum Level {
        case Debug, Info, Warn, Error, Fatal
    }

    def default = new Logger(Level.values.toSet, Map(Level.Debug -> Color.Blue, Level.Info -> Color.White, Level.Warn -> Color.Yellow, Level.Error -> Color.Red, Level.Fatal -> Color.LightRed))

}

class Logger(val levels: Set[Level], val colors: Map[Level, EscapeAttr]) {

    def log(msg: String, lvl: Level)(using name: sourcecode.FullName, line: sourcecode.Line): Unit = 
        if levels.contains(lvl) then
            val time = LocalDateTime.now().format(DateTimeFormatter.ofPattern("uuuu/MM/dd-HH:mm:ss"))
            println(colors(lvl)(s"[${lvl.toString.toUpperCase}:$time-${name.value}:${line.value}] $msg"))
    def log(msg: String)(using name: sourcecode.FullName, line: sourcecode.Line): Unit = log(msg, Level.Debug)

    extension [T](self: T) {
        def log(msg: String, lvl: Level)(using name: sourcecode.FullName, line: sourcecode.Line): T = 
            this.log(msg, lvl)(using name, line)
            self
        def log(msg: String)(using name: sourcecode.FullName, line: sourcecode.Line): T = 
            this.log(msg, Level.Debug)(using name, line)
            self
        def log(msg: T => String, lvl: Level)(using name: sourcecode.FullName, line: sourcecode.Line): T = 
            this.log(msg(self), lvl)(using name, line)
            self
        def log(msg: T => String)(using name: sourcecode.FullName, line: sourcecode.Line): T = 
            this.log(msg(self), Level.Debug)
            self
        def log(using name: sourcecode.FullName, line: sourcecode.Line): T =
            this.log(self.toString, Level.Debug)
            self
        def log(lvl: Level)(using name: sourcecode.FullName, line: sourcecode.Line): T =
            this.log(self.toString, lvl)
            self
    }
}