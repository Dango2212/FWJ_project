

sealed trait DeviceHealth

object DeviceHealth {
  case object Normal extends DeviceHealth
  case object Damaged extends DeviceHealth
}

abstract class EnergyPlant(val plantId: String, val maxCapacity: Double) {

  protected var generatedPower: Double = 0.0
  private var hardwareStatus: DeviceHealth = DeviceHealth.Normal
  private var operationState: String = "Shutdown"

  def getMaxCapacity: Double = maxCapacity
  def getGeneratedPower: Double = generatedPower
  def getHardwareStatus: DeviceHealth = hardwareStatus
  def getOperationState: String = operationState

  def updateOperationState(newState: String): Unit = {
    println(s"Updating operation state of plant $plantId to $newState")
    operationState = newState
  }

  def updateHardwareStatus(newStatus: DeviceHealth): Unit = {
    println(s"Updating hardware status of plant $plantId to $newStatus")
    hardwareStatus = newStatus
  }

  def activate(): Unit = {
    try {
      println(s"Activating plant $plantId...")
      operationState = "Running"
      refreshGeneratedPower()
      println(s"Plant $plantId is now active, outputting $generatedPower MW.")
    } catch {
      case ex: Exception =>
        println(s"Activation failed for plant $plantId: ${ex.getMessage}")
        updateHardwareStatus(DeviceHealth.Damaged)
    }
  }

  def deactivate(): Unit = {
    try {
      println(s"Deactivating plant $plantId...")
      operationState = "Shutdown"
      generatedPower = 0.0
      println(s"Plant $plantId has been deactivated.")
    } catch {
      case ex: Exception =>
        println(s"Deactivation failed for plant $plantId: ${ex.getMessage}")
        updateHardwareStatus(DeviceHealth.Damaged)
    }
  }

  def refreshGeneratedPower(): Unit
}
