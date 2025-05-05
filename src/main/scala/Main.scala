//Zherui Zhang
//Xiangwei Zhai

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.io.Source
import scala.io.StdIn.readLine
import scala.sys.exit
import scala.util.Try

object Main extends App {

  val facilityManager   = new FacilityManager()
  val weatherCondition  = new WeatherCondition()
  val monitoringSystem  = new MonitoringSystem(facilityManager)
  val dataAnalyzer      = new DataAnalyzer("historydata.csv")
  val dataCollector     = new DataCollector(facilityManager, "data.csv", weatherCondition)
  val plantScheduler    = new PlantScheduler(facilityManager, monitoringSystem, weatherCondition)

  // Initialize and load
  plantScheduler.initializeOperation(0.5)
  dataCollector.loadFacilityData()

  private def printMenu(): Unit = {
    println(
      """
        |--- Renewable Energy Plant System ---
        |1. Add a new plant
        |2. Remove a plant
        |3. Start a plant
        |4. Shutdown a plant
        |5. Show all plants status
        |6. Simulate running in a given period
        |7. Analyze collected data
        |0. Exit
        |--------------------------------------
      """.stripMargin)
  }

  //read date
  private def readDate(prompt: String,
                       fmt: DateTimeFormatter,
                       example: LocalDate): LocalDate = {
    val exampleStr = example.format(fmt)
    while (true) {
      print(prompt)
      val norm = readLine().trim.replace('\\', '/')
      try return LocalDate.parse(norm, fmt)
      catch {
        case _: DateTimeParseException =>
          println(s"Invalid date format. Please enter date as '${fmt.toString}'.")
          println(s"For example: '$exampleStr' represents ${example.getDayOfMonth}/${example.getMonthValue}/${example.getYear}.")
      }
    }
    LocalDate.now()
  }

  private var running = true
  while (running) {
    printMenu()
    val opt = Try(readLine("Select an option: ").trim.toInt).getOrElse(-1)
    try {
      opt match {
        case 1 =>
          // Add a new plant
          val plantType = readLine("Type (solar, wind, hydro): ").trim.toLowerCase
          if (!Seq("solar","wind","hydro").contains(plantType)) {
            println(s"Invalid type: '$plantType'. Supported: solar, wind, hydro.")
          } else {
            val idInput = FacilityUtils.createFacilityId(plantType, 5)
            val maxIn   = readLine("Max output (MW): ").trim
            Try(maxIn.toDouble) match {
              case scala.util.Success(m) if m > 0 =>
                val plant = plantType match {
                  case "solar" => new SolarPanel(idInput, m, weatherCondition)
                  case "wind"  => new WindTurbine(idInput, m, weatherCondition)
                  case "hydro" => new HydropowerPlant(idInput, m, weatherCondition)
                }
                facilityManager.registerPlant(plant)
                dataCollector.captureCurrentFacilityState()
                println(s"Added $plantType plant ID=$idInput")
              case _ => println("Invalid number. Please enter a positive numeric value.")
            }
          }

        case 2 =>
          // Remove a plant
          val pid = readLine("Plant ID to remove: ").trim
          if (facilityManager.listAllPlants.contains(pid)) {
            facilityManager.removePlantById(pid)
            dataCollector.captureCurrentFacilityState()
            println(s"Removed plant ID=$pid")
          } else println(s"No plant found with ID=$pid")

        case 3 =>
          // Start a plant
          val pid = readLine("Plant ID to start: ").trim
          facilityManager.listAllPlants.get(pid) match {
            case Some(p) if p.getOperationState != "Running" =>
              facilityManager.activatePlant(pid)
              dataCollector.captureCurrentFacilityState()
              println(s"Started plant ID=$pid")
            case Some(_) => println(s"Plant ID=$pid is already running.")
            case None    => println(s"No plant found with ID=$pid")
          }

        case 4 =>
          // Shutdown a plant
          val pid = readLine("Plant ID to shutdown: ").trim
          facilityManager.listAllPlants.get(pid) match {
            case Some(p) if p.getOperationState != "Stopped" =>
              facilityManager.deactivatePlant(pid)
              dataCollector.captureCurrentFacilityState()
              println(s"Shutdown plant ID=$pid")
            case Some(_) => println(s"Plant ID=$pid is already stopped.")
            case None    => println(s"No plant found with ID=$pid")
          }

        case 5 =>
          // Show status
          dataCollector.captureCurrentFacilityState()
          val all = facilityManager.listAllPlants
          if (all.isEmpty) println("No facilities available.")
          else {
            println("All Facilities:")
            all.foreach { case (id, plant) =>
              val t   = plant.getClass.getSimpleName.replaceAll("Plant$", "")
              val st  = plant.getOperationState
              val out = plant.getGeneratedPower
              println(s"ID: $id, Type: $t, State: $st, Output: $out MW")
            }
          }

        case 6 =>
          // Simulate
          dataCollector.captureCurrentFacilityState()
          val daysInput = readLine("Days to simulate: ").trim
          Try(daysInput.toInt) match {
            case scala.util.Success(d) if d > 0 =>
              val historyPath = "historydata.csv"
              val startDate   = Try {
                val lines = Source.fromFile(historyPath).getLines().filter(_.nonEmpty).toList
                if (lines.isEmpty) LocalDate.of(2023, 5, 1)
                else LocalDate.parse(lines.last.split(",")(0).trim)
              }.getOrElse(LocalDate.of(2023, 5, 1))
              new SimulationExecutor(startDate, facilityManager, plantScheduler).executeSimulation(d)
              println(s"Simulation of $d days complete.")
            case _ => println("Invalid input. Please enter an integer > 0.")
          }

        case 7 =>
          // Analyze with concise summaries
          val fileFmt  = DateTimeFormatter.ofPattern("yyyy-MM-dd")
          val inputFmt = DateTimeFormatter.ofPattern("dd/MM/yyyy")
          val today    = LocalDate.now()

          val start = readDate("Enter start date (DD/MM/YYYY): ", inputFmt, today.minusDays(2))
          val end   = readDate("Enter   end date (DD/MM/YYYY): ", inputFmt, today)
          if (start.isAfter(end)) {
            println("Start date must be on or before end date.")
          } else {
            val hasData = Try {
              Source.fromFile("historydata.csv").getLines().drop(1)
                .map(_.split(",")(0).trim)
                .map(d => LocalDate.parse(d, fileFmt))
                .exists(d => !d.isBefore(start) && !d.isAfter(end))
            }.getOrElse(false)

            if (!hasData) {
              println("No available data for the selected date range. Please choose another range.")
            } else {
              println(s"[Streaming] Day summary for $start:")
              dataAnalyzer.analyze("day", start, start)

              if (end != start) {
                println(s"[Streaming] Day summary for $end:")
                dataAnalyzer.analyze("day", end, end)
              }

              println("[Streaming] Weekly summary for period:")
              dataAnalyzer.analyze("week", start, end)

              println("[Streaming] Monthly summary for period:")
              dataAnalyzer.analyze("month", start, end)
            }
          }

        case 0 =>
          println("Exiting...")
          running = false
          exit(0)

        case _ => println("Invalid option. Please enter a number from the menu.")
      }
    } catch {
      case ex: Exception => println(s"Unexpected error: ${ex.getMessage}")
    }
  }
}
