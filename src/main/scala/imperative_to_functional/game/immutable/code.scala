package imperative_to_functional.game.immutable

import imperative_to_functional.game.immutable.IdCounter.Id

import scala.reflect.ClassTag

case class IdCounter(id: IdCounter.Id=0) {
  def next = IdCounter(id + 1)
}
object IdCounter {
  type Id = Int
}

sealed trait WObject {
  val id: IdCounter.Id
}

case class Asteroid(id: IdCounter.Id, resources: Int) extends WObject {
  def +(res: Int) = copy(resources = resources + res)
  def -(res: Int) = this + -res
}

case class Extractor(
  id: IdCounter.Id, asteroidId: IdCounter.Id, extractionRate: Int
) extends WObject {
  def extract(world: World): (World, Int) = {
    world.find[Asteroid](asteroidId)
      .fold((world, 0)) { asteroid =>
        val resources = extractionRate min asteroid.resources
        (world.updated(asteroid - resources), resources)
      }
  }
}

object World {
  def apply(objects: TraversableOnce[WObject]): World =
    objects.foldLeft(World())(_.updated(_))
}
case class World(
  objectMap: Map[IdCounter.Id, WObject]=Map.empty,
  extracted: Int = 0
) {
  def find[A <: WObject : ClassTag](id: IdCounter.Id) =
    objectMap.get(id).collect { case a: A => a }

  def updated(obj: WObject) = copy(objectMap = objectMap + (obj.id -> obj))
  def remove(id: Id): World = copy(objectMap = objectMap - id)
  def resourcesLeft = objects.collect { case a: Asteroid => a.resources }.sum

  def objects = objectMap.values

  def turnStart: (World, Int) = {
    val (newWorld, totalExtracted) = objects.collect { case e: Extractor => e }
      .foldLeft((this, 0)) { case ((world, totalExtracted), e) =>
        val (newWorld, extracted) = e.extract(world)
        (newWorld, totalExtracted + extracted)
      }
    (
      newWorld.copy(extracted = newWorld.extracted + totalExtracted),
      totalExtracted
    )
  }
}

object FPGame {
  def extract(world: World): World = {
    val (newWorld, extracted) = world.turnStart

    println(s"Resources Left: ${world.resourcesLeft}")
    println(
      s"Extracted, before: ${world.extracted}, this turn: $extracted, after: ${
        newWorld.extracted}"
    )
    println(s"Resources Left after extraction: ${newWorld.resourcesLeft}")
    println()
    newWorld
  }

  def main(args: Array[String]): Unit = {
    val ids = IdCounter()
    val asteroid = Asteroid(ids.id, 10)
    val extractorIds = ids.next
    val extractor = Extractor(extractorIds.id, asteroid.id, 4)

    var world = World(Seq(asteroid, extractor))

    while (world.resourcesLeft > 4) world = extract(world)
    println("!!! Removing asteroid !!!")
    world = world.remove(asteroid.id)
    extract(world)
  }
}
