import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.{Try, Using}

class DataCollector(facilityManager: FacilityManager, dataFile: String, weatherProvider: WeatherCondition) {

  /** 读取文件并注册所有合法的工厂实例 */
  def loadFacilityData(): Unit = {
    Using(Source.fromFile(dataFile)) { src =>
      val plants = src.getLines().drop(1).flatMap(parseLine).toList
      plants.foreach(facilityManager.registerPlant)
    }.recover {
      case _: Exception => // 忽略文件不存在或读取错误
    }
  }

  /** 解析一行CSV，成功时返回 Some(Plant)，否则 None */
  private def parseLine(line: String): Option[EnergyPlant] = {
    val cols = line.split(",", -1).map(_.trim)
    if (cols.length >= 6) {
      Try {
        val id       = cols(0)
        val kind     = cols(1)
        val status   = cols(2)
        val capacity = cols(4).toDouble
        val health   = cols(5)

        val plant = kind match {
          case "SolarPanel"  => new SolarPanel(id, capacity, weatherProvider)
          case "WindTurbine" => new WindTurbine(id, capacity, weatherProvider)
          case "Hydropower"  => new HydropowerPlant(id, capacity, weatherProvider)
          case other         => throw new IllegalArgumentException(s"Unknown type '$other'")
        }

        plant.updateOperationState(status)
        plant.updateHardwareStatus(health match {
          case "Normal"  => DeviceHealth.Normal
          case "Damaged" => DeviceHealth.Damaged
          case bad       => throw new IllegalArgumentException(s"Invalid health '$bad'")
        })

        plant
      }.toOption
    } else None
  }

  /** 刷新所有厂站的即时发电并将状态写回同一文件 */
  def captureCurrentFacilityState(): Unit = {
    // 先刷新状态，再收集所有输出记录
    facilityManager.listAllPlants.values.foreach(_.refreshGeneratedPower())

    val records = facilityManager.listAllPlants.toSeq.map { case (id, plant) =>
      Seq(
        id,
        plant.getClass.getSimpleName.replace("Plant", ""),
        plant.getOperationState,
        plant.getGeneratedPower.toString,
        plant.getMaxCapacity.toString,
        plant.getHardwareStatus.toString
      ).mkString(", ")
    }

    // 写文件
    Using(new PrintWriter(new File(dataFile))) { writer =>
      writer.println("Plant ID, Facility Type, Status, Current Output, Max Output, Device Status")
      records.foreach(writer.println)
    }.recover {
      case _: Exception => // 无需处理写入异常
    }
  }
}
