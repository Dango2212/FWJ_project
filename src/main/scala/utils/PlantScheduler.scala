import java.time.LocalDate
import scala.collection.mutable
import scala.util.Random

class PlantScheduler(facilityManager: FacilityManager, monitoringSystem: MonitoringSystem, weatherCondition: WeatherCondition) {

  private val MaxOperationalDays = 30
  private val operationDaysTracker = mutable.Map[String, Int]()

  def initializeOperation(startRatio: Double): Unit = {
    val allFacilities = facilityManager.listAllPlants.values.toList
    Random.shuffle(allFacilities)
      .take((allFacilities.size * startRatio).toInt)
      .foreach { facility =>
        facility.activate()
        operationDaysTracker(facility.plantId) = 0
      }
  }

  def performDailyScheduling(currentDate: LocalDate): Unit = {
    println(s"[Scheduler] Simulating operations for date: ${currentDate}")

    weatherCondition.manualRefresh()

    facilityManager.listAllPlants.values.foreach { facility =>
      if (!operationDaysTracker.contains(facility.plantId) && facility.getOperationState == "Running") {
        operationDaysTracker(facility.plantId) = 0
      }

      if (facility.getOperationState == "Running") {
        if (operationDaysTracker(facility.plantId) >= MaxOperationalDays) {
          facility.deactivate()
          println(s"[Scheduler] Facility ${facility.plantId} automatically deactivated after $MaxOperationalDays days.")
          operationDaysTracker -= facility.plantId
        } else {
          operationDaysTracker(facility.plantId) += 1
        }
      }

      monitoringSystem.inspectFacilityOutputs()
      monitoringSystem.induceRandomFailures()
    }
  }
}
