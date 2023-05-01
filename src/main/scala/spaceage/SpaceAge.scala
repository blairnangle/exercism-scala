package spaceage

object SpaceAge {
  private def secondsToYears(seconds: Double): Double = {
    seconds / 60 / 60 / 24 / 365.25
  }

  def onEarth(age: Double): Double = secondsToYears(seconds = age)

  def onMercury(age: Double): Double = secondsToYears(seconds = age) / 0.2408467

  def onVenus(age: Double): Double = secondsToYears(seconds = age) / 0.61519726

  def onMars(age: Double): Double = secondsToYears(seconds = age) / 1.8808158

  def onJupiter(age: Double): Double = secondsToYears(seconds = age) / 11.862615

  def onSaturn(age: Double): Double = secondsToYears(seconds = age) / 29.447498

  def onUranus(age: Double): Double = secondsToYears(seconds = age) / 84.016846

  def onNeptune(age: Double): Double = secondsToYears(seconds = age) / 164.79132
}
