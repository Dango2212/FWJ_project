import Main.dataCollector
import scala.util.Random

class MonitoringSystem(facilityManager: FacilityManager) {

  private val rng = new Random()
  private val LowOutputThresholdRatio = 0.1
  private val DeviceFailureProbability = 0.01

  def inspectFacilityOutputs(): Unit = {
    facilityManager.listAllPlants.foreach { case (plantId, facility) =>
      if (facility.getOperationState.equalsIgnoreCase("running") && facility.getGeneratedPower < facility.getMaxCapacity * LowOutputThresholdRatio) {
        println(s"[Alert] Low performance detected: $plantId")
      }
    }
  }

  def induceRandomFailures(): Unit = {
    facilityManager.listAllPlants.foreach { case (plantId, facility) =>
      if (rng.nextDouble() < DeviceFailureProbability) {
        facility.updateHardwareStatus(DeviceHealth.Damaged)
        println(s"[Warning] Facility failure occurred: $plantId")
        dataCollector.captureCurrentFacilityState()
      }
    }
  }
}
