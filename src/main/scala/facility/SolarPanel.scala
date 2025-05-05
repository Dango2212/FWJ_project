


class SolarPanel(plantId: String, maxCapacity: Double, envData: WeatherCondition) extends EnergyPlant(plantId, maxCapacity) {

  private val EfficiencyFactor = 0.75

  override def refreshGeneratedPower(): Unit = {
    if (getOperationState == "Running") {
      val sunlightEffect = envData.getSunlightFactor
      val estimatedPower = sunlightEffect * maxCapacity * EfficiencyFactor
      generatedPower = math.min(estimatedPower, maxCapacity)
    } else {
      generatedPower = 0.0
    }
  }
}
