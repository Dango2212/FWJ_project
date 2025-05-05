


class HydropowerPlant(plantId: String, maxCapacity: Double, envData: WeatherCondition) extends EnergyPlant(plantId, maxCapacity) {

  private val EnergyConversionEfficiency = 0.8

  override def refreshGeneratedPower(): Unit = {
    if (getOperationState == "Running") {
      val flowRate = envData.getWaterFlowRate
      val expectedOutput = flowRate * maxCapacity * EnergyConversionEfficiency
      generatedPower = math.min(expectedOutput, maxCapacity)
    } else {
      generatedPower = 0.0
    }
  }
}
