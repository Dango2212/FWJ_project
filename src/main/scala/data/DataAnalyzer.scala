import scala.io.Source
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.DayOfWeek
import java.time.temporal.TemporalAdjusters

// online add sups
private class StatsAccumulator {
  private var count = 0
  private var sum = 0.0
  private var min = Double.MaxValue
  private var max = Double.MinValue
  private val freqs = scala.collection.mutable.Map[Double, Int]().withDefaultValue(0)
  private val buffer = scala.collection.mutable.ArrayBuffer[Double]()

  def add(x: Double): Unit = {
    count += 1
    sum += x
    if (x < min) min = x
    if (x > max) max = x
    freqs(x) += 1
    buffer += x
  }

  def mean: Double = sum / count

  def median: Double = {
    val s = buffer.sorted
    if (count % 2 == 1) s(count / 2)
    else (s(count / 2 - 1) + s(count / 2)) / 2.0
  }

  def mode: Double = freqs.maxBy(_._2)._1

  def range: Double = max - min

  def midRange: Double = (max + min) / 2.0
}

class DataAnalyzer(fileName: String) {
  private val fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def analyze(period: String, startDate: LocalDate, endDate: LocalDate): Unit = {
    // Period 's StatsAccumulator
    val accs = scala.collection.mutable.Map[LocalDate, StatsAccumulator]()

    val src = Source.fromFile(fileName)
    try {
      for (line <- src.getLines().drop(1)) {
        val parts = line.split(",", -1).map(_.trim)
        if (parts.length >= 2 && parts(0).nonEmpty && parts(1).nonEmpty) {
          val ds = parts(0)
          val vs = parts(1)
          try {
            val date = LocalDate.parse(ds, fmt)
            if (!date.isBefore(startDate) && !date.isAfter(endDate)) {
              val key = period match {
                case "day"   => date
                case "week"  => date.`with`(DayOfWeek.MONDAY)
                case "month" => date.`with`(TemporalAdjusters.firstDayOfMonth())
                case _       => date
              }
              val v = vs.toDouble
              val sta = accs.getOrElseUpdate(key, new StatsAccumulator)
              sta.add(v)
            }
          } catch {
            case _: Exception =>
          }
        }
      }
    } finally {
      src.close()
    }

    if (accs.isEmpty) {
      println(s"No data found from $startDate to $endDate.")
    } else {
      accs.toSeq.sortBy(_._1).foreach { case (pStart, sta) =>
        println(s"[Streaming] Analysis for period starting $pStart:")
        println(f"  Mean     : ${sta.mean}%.2f")
        println(f"  Median   : ${sta.median}%.2f")
        println(f"  Mode     : ${sta.mode}%.2f")
        println(f"  Range    : ${sta.range}%.2f")
        println(f"  Midrange : ${sta.midRange}%.2f")
      }
    }
  }
}
