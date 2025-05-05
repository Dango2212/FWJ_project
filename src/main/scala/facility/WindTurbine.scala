


class WindTurbine(plantId: String, maxCapacity: Double, envData: WeatherCondition) extends EnergyPlant(plantId, maxCapacity) {

  private val EfficiencyRatio = 0.65
  private val MaxOperationalWindSpeed = 25.0

  override def refreshGeneratedPower(): Unit = {
    if (getOperationState == "Running") {
      val currentWindSpeed = envData.getWindVelocity
      val effectiveWindSpeed = math.min(currentWindSpeed, MaxOperationalWindSpeed)
      val potentialEnergy = effectiveWindSpeed * maxCapacity * EfficiencyRatio
      generatedPower = math.min(potentialEnergy, maxCapacity)
    } else {
      generatedPower = 0.0
    }
  }
}
