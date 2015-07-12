package imperative_to_functional.game.scalaz

import imperative_to_functional.game.scalaz.IdCounter.Id

import scala.reflect.ClassTag
import scalaz._, Scalaz._, scalaz.effect._

case class IdCounter(id: IdCounter.Id=0)
object IdCounter {
  type Id = Int

  val next = State { (counter: IdCounter) =>
    val newCounter = counter.copy(id = counter.id + 1)
    (newCounter, newCounter.id)
  }
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
) extends WObject
object Extractor {
  def extract(extractor: Extractor) = State { (world: World) =>
    world.find[Asteroid](extractor.asteroidId)
      .fold((world, 0)) { asteroid =>
        val resources = extractor.extractionRate min asteroid.resources
        (world.updated(asteroid - resources), resources)
      }
  }
}

object World {
  def apply(objects: TraversableOnce[WObject]): World =
    objects.foldLeft(World())(_.updated(_))

  val turnStart = for {
    world <- State.get[World]
    totalExtracted <- (world.objects.collect {
      case e: Extractor => Extractor.extract(e)
    }.toVector.sequenceU: State[World, Vector[Int]]).map(_.sum)
    world <- State.get
    _ <- State.put(world.copy(extracted = world.extracted + totalExtracted))
  } yield totalExtracted
}
case class World(
  objectMap: Map[IdCounter.Id, WObject]=Map.empty,
  extracted: Int = 0
) {
  def find[A <: WObject : ClassTag](id: IdCounter.Id) =
    objectMap.get(id).collect { case a: A => a }

  def updated(obj: WObject) = copy(objectMap = objectMap + (obj.id -> obj))
  def remove(id: Id): World = copy(objectMap = objectMap - id)

  def objects = objectMap.values

  def resourcesLeft = objects.collect { case a: Asteroid => a.resources }.sum
}

object FPGame extends SafeApp {
  def extract(world: World): IO[World] = {
    val (newWorld, extracted) = World.turnStart(world)

    for {
      _ <- IO.putStrLn(s"Resources Left: ${world.resourcesLeft}")
      _ <- IO.putStrLn(
        s"Extracted, before: ${world.extracted}, this turn: ${extracted
        }, after: ${newWorld.extracted}"
      )
      _ <- IO.putStrLn(s"Resources Left after extraction: ${newWorld.resourcesLeft}")
      _ <- IO.putStrLn("")
    } yield newWorld
  }

  override def runc: IO[Unit] = {
    val createWorld = for {
      id <- IdCounter.next
      asteroid = Asteroid(id, 10)
      id <- IdCounter.next
      extractor = Extractor(id, asteroid.id, 4)
    } yield (asteroid.id, World(Seq(asteroid, extractor)))

    val (_, (asteroidId, world)) = createWorld(IdCounter())

    def rec(world: World): IO[Unit] =
      if (world.resourcesLeft > 4) extract(world).flatMap(rec)
      else for {
        _ <- IO.putStrLn("!!! Removing asteroid !!!")
        _ <- extract(world.remove(asteroidId))
      } yield ()

    rec(world)
  }
}