import java.time.LocalDateTime
import java.util.concurrent.{Executors, TimeUnit}
import scala.util.Random

class WeatherCondition {

  private val rnd = new Random()
  private var lightFactor: Double = 0.0
  private var windSpeed:   Double = 0.0
  private var flowRate:    Double = 0.0

  updateMetrics()

  private val scheduler = Executors.newSingleThreadScheduledExecutor()
  scheduler.scheduleAtFixedRate(
    new Runnable { override def run(): Unit = updateMetrics() },
    1,
    1,             // Every time 1 sec
    TimeUnit.MINUTES
  )

  def manualRefresh(): Unit = updateMetrics()
  def getSunlightFactor: Double = lightFactor
  def getWindVelocity: Double = windSpeed
  def getWaterFlowRate: Double = flowRate


  private def updateMetrics(): Unit = {
    val now = LocalDateTime.now()
    lightFactor = computeLight(now)
    windSpeed   = computeWind(now)
    flowRate    = computeWater(now)
  }

  private def computeLight(time: LocalDateTime): Double = {
    val h = time.getHour
    if (h >= 6 && h <= 18)
      rnd.nextDouble() * (1 - math.abs(h - 12) / 6.0)
    else
      rnd.nextDouble() * 0.1
  }

  private def computeWind(time: LocalDateTime): Double = {
    val m = time.getMonthValue
    if (m <= 2 || m >= 11)
      0.5 + rnd.nextDouble() * 0.5
    else
      0.1 + rnd.nextDouble() * 0.3
  }

  private def computeWater(time: LocalDateTime): Double = {
    val m = time.getMonthValue
    if ((m >= 3 && m <= 5) || (m >= 9 && m <= 11))
      0.3 + rnd.nextDouble() * 0.7
    else
      0.1 + rnd.nextDouble() * 0.2
  }
}
