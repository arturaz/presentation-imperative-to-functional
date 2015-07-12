package imperative_to_functional.game.mutable

import scala.collection.mutable

object IdCounter {
  type Id = Int
}
class IdCounter {
  private[this] var nextId = 0
  def newId() = {
    val id = nextId
    nextId += 1
    id
  }
}

trait WObject {
  val id: IdCounter.Id
}

class Asteroid(val id: IdCounter.Id, var resources: Int) extends WObject

class Extractor(
  val id: IdCounter.Id, asteroid: Asteroid, extractionRate: Int
) extends WObject {
  def extract(): Int = {
    val resources = extractionRate min asteroid.resources
    asteroid.resources -= resources
    resources
  }
}

class World {
  val objects = mutable.Set.empty[WObject]

  def extracted = _extracted
  private[this] var _extracted = 0

  def turnStart(): Int = {
    val extracted = objects.collect { case e: Extractor => e.extract() }.sum
    _extracted += extracted
    extracted
  }

  def resourcesLeft = objects.collect { case a: Asteroid => a.resources }.sum
}

object MutableReferences {
  def extract(world: World): Unit = {
    println(s"Resources Left: ${world.resourcesLeft}")
    println(
      s"Extracted, before: ${world.extracted}, this turn: ${world.turnStart()
      }, after: ${world.extracted}"
    )
    println(s"Resources Left after extraction: ${world.resourcesLeft}")
    println()
  }

  def main(args: Array[String]): Unit = {
    val ids = new IdCounter
    val asteroid = new Asteroid(ids.newId(), 10)
    val extractor = new Extractor(ids.newId(), asteroid, 4)

    val world = new World
    world.objects.add(asteroid)
    world.objects.add(extractor)

    while (world.resourcesLeft > 4) extract(world)
    println("!!! Removing asteroid !!!")
    world.objects.remove(asteroid)
    extract(world)
  }
}