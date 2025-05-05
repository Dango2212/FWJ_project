import java.io.{BufferedWriter, FileWriter}
import java.time.LocalDate
import scala.io.Source

class SimulationExecutor(startDate: LocalDate, manager: FacilityManager, scheduler: PlantScheduler, historyFile: String = "historydata.csv") {

  private var simulationDate: LocalDate = startDate

  def executeSimulation(totalDays: Int): Unit = {
    val writer = new BufferedWriter(new FileWriter(historyFile, true))
    try {
      val source = Source.fromFile(historyFile)
      val isEmptyFile = source.getLines().isEmpty
      source.close()

      if (isEmptyFile) {
        writer.write("Date, Total Output, Efficiency\n")
      }

      for (_ <- 1 to totalDays) {
        scheduler.performDailyScheduling(simulationDate)

        val activeFacilities = manager.listAllPlants.values.filter { plant =>
          plant.getOperationState == "Running" && plant.getHardwareStatus == DeviceHealth.Normal
        }

        val totalGenerated = activeFacilities.map(_.getGeneratedPower).sum
        val totalCapacity = activeFacilities.map(_.getMaxCapacity).sum
        val operationalEfficiency = if (totalCapacity > 0) totalGenerated / totalCapacity else 0.0

        writer.write(s"$simulationDate, $totalGenerated, $operationalEfficiency\n")

        simulationDate = simulationDate.plusDays(1)
      }
    } finally {
      writer.close()
    }
  }
}
