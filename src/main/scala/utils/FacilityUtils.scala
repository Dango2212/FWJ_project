object FacilityUtils {

  private val rng = new scala.util.Random()
  private val AlphaNum = ('0' to '9') ++ ('A' to 'Z')

  def createFacilityId(prefix: String, idLength: Int): String = {
    // Timestamp segment: date + time
    val now = java.time.LocalDateTime.now()
    val ts   = now.format(java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss"))

    // Random suffix
    val suffix = (1 to idLength)
      .map(_ => AlphaNum(rng.nextInt(AlphaNum.length)))
      .mkString

    s"${prefix.toUpperCase}-$ts-$suffix"
  }
}
