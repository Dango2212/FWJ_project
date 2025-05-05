import scala.collection.mutable

class FacilityManager {

  private val facilityMap: mutable.Map[String, EnergyPlant] = mutable.Map()

  def registerPlant(plant: EnergyPlant): Unit = {
    facilityMap.get(plant.plantId) match {
      case Some(_) => throw new IllegalArgumentException("Duplicate plant ID detected.")
      case None    => facilityMap += (plant.plantId -> plant)
    }
  }

  def removePlantById(plantId: String): Unit = {
    facilityMap.get(plantId) match {
      case Some(_) => facilityMap -= plantId
      case None    => throw new IllegalArgumentException("No such plant found.")
    }
  }

  def findPlant(plantId: String): Option[EnergyPlant] = facilityMap.get(plantId)

  def listAllPlants: Map[String, EnergyPlant] = facilityMap.toMap

  def deactivatePlant(plantId: String): Unit = {
    findPlant(plantId).fold(
      throw new IllegalArgumentException("Cannot deactivate a non-existing plant.")
    )(_.deactivate())
  }

  def activatePlant(plantId: String): Unit = {
    findPlant(plantId).fold(
      throw new IllegalArgumentException("Cannot activate a non-existing plant.")
    )(_.activate())
  }

  def calculateTotalGeneratedPower: Double = facilityMap.values.map(_.getGeneratedPower).sum
}
