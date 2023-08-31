package general

import java.time.Instant
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import fansi.{Color, EscapeAttr}
import Logger.{Level}

object Logger {
    enum Level {
        case Trace, Debug, Info, Warn, Error, Fatal
    }

    def default = new Logger(Level.values.toSet - Level.Trace, Map(Level.Trace -> Color.Blue, Level.Debug -> Color.White, Level.Info -> Color.Green, Level.Warn -> Color.Yellow, Level.Error -> Color.Red, Level.Fatal -> Color.LightRed))
    def log(msg: String, lvl: Level)(using name: sourcecode.FullName, line: sourcecode.Line, logger: Logger): Unit = logger.log(msg, lvl)
    def log(msg: String)(using name: sourcecode.FullName, line: sourcecode.Line, logger: Logger): Unit = logger.log(msg)
}

class Logger(val levels: Set[Level], val colors: Map[Level, EscapeAttr]) {

    def set(lvl: Level) = new Logger(Level.values.filter(_.ordinal >= lvl.ordinal).toSet, colors)

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