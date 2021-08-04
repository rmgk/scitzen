package benchmarks.basic

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.BenchmarkParams

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(1)
@Threads(1)
@State(Scope.Benchmark)
class Latexencode {

  val testString           = """Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum."""
  val testStringReplaceOne = """Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod \\
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, \\
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo \\
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse \\
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non \\
proident, sunt in culpa qui officia deserunt mollit anim id est laborum. \\"""

  val testStringReplaceAll = """
          case '~' => "\\textasciitilde{}"
        case '^' => "\\textasciicircum{}"
        case '`' => "\\textasciigrave{}"
        case '&' => "\\&"
        case '%' => "\\%"
        case '$' => "\\$"
        case '#' => "\\#"
        case '_' => "\\_"
        case '{' => "\\{"
        case '}' => "\\}"
        case '\\' => "\\textbackslash{}""""

  @Setup
  def setup(): Unit = {}

  @Benchmark
  def noReplace(): Unit = {
    scitzen.outputs.SastToTexConverter.latexencode(testString)
  }

    @Benchmark
  def replace(): Unit = {
    scitzen.outputs.SastToTexConverter.latexencode(testStringReplaceOne)
  }

        @Benchmark
  def replaceAll(): Unit = {
    scitzen.outputs.SastToTexConverter.latexencode(testStringReplaceAll)
  }
  
  

}
