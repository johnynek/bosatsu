package dev.bosatsu.hashing

import cats.data.NonEmptyList

class HashableTest extends munit.FunSuite {
  case class User(name: String, age: Int) derives Hashable
  case class Admin(name: String, age: Int) derives Hashable

  case class Person(
      name: String,
      age: Int,
      nick: Option[String],
      tags: List[String],
      scores: Vector[Int]
  ) derives Hashable

  enum Mode derives Hashable {
    case Fast
    case Slow
    case Custom(level: Int)
  }

  enum MyList[+A] derives Hashable {
    case Empty
    case NonEmpty(a: A, tail: MyList[A])
  }

  sealed trait Animal derives Hashable
  object Animal {
    case class Dog(name: String, age: Int) extends Animal derives Hashable
    case class Cat(name: String) extends Animal derives Hashable
  }

  case class HashTree(
      root: Hashed[Algo.Blake3, String],
      children: List[Hashed[Algo.Blake3, Int]]
  ) derives Hashable

  test("derived case class hash returns a Hashed wrapper") {
    val p = Person(
      name = "alice",
      age = 42,
      nick = Some("ally"),
      tags = List("scala", "bosatsu"),
      scores = Vector(1, 2, 3)
    )

    val hashed = Hashable.hash[Algo.Blake3](p)

    assertEquals(hashed.arg, p)
    assertEquals(hashed.hash.hex.length, 64)
  }

  test("different values have different blake3 hashes") {
    val p1 = Person(
      name = "alice",
      age = 42,
      nick = Some("ally"),
      tags = List("scala", "bosatsu"),
      scores = Vector(1, 2, 3)
    )
    val p2 = Person(
      name = "bob",
      age = 42,
      nick = Some("ally"),
      tags = List("scala", "bosatsu"),
      scores = Vector(1, 2, 3)
    )

    val h1 = Hashable.hash[Algo.Blake3](p1)
    val h2 = Hashable.hash[Algo.Blake3](p2)

    assertNotEquals(h1.hash, h2.hash)
  }

  test("derived enum hash differs by case") {
    val fast = Hashable.hash[Algo.Blake3](Mode.Fast)
    val slow = Hashable.hash[Algo.Blake3](Mode.Slow)

    assertNotEquals(fast.hash, slow.hash)
  }

  test("derived sealed trait hash differs by subtype") {
    val dog: Animal = Animal.Dog(name = "milo", age = 3)
    val cat: Animal = Animal.Cat(name = "milo")

    val dogHash = Hashable.hash[Algo.Blake3](dog)
    val catHash = Hashable.hash[Algo.Blake3](cat)

    assertNotEquals(dogHash.hash, catHash.hash)
  }

  test("type labels are included for derived products") {
    val userHash = Hashable.hash[Algo.Blake3](User(name = "sam", age = 10))
    val adminHash = Hashable.hash[Algo.Blake3](Admin(name = "sam", age = 10))

    assertNotEquals(userHash.hash, adminHash.hash)
  }

  test("recursive enum hash differs by nested value") {
    val derived: Hashable[MyList[Int]] = summon[Hashable[MyList[Int]]]

    val xs =
      MyList.NonEmpty(1, MyList.NonEmpty(2, MyList.Empty))
    val ys =
      MyList.NonEmpty(1, MyList.NonEmpty(3, MyList.Empty))

    val xh = derived.hash(xs)(using Algo.blake3Algo)
    val yh = Hashable.hash[Algo.Blake3](ys)

    assertNotEquals(xh.hash, yh.hash)
  }

  test("tuple hashable is provided and hashes tuple members in order") {
    val tupleHashable: Hashable[(Int, String, Boolean)] =
      summon[Hashable[(Int, String, Boolean)]]

    val t1 = (1, "alpha", true)
    val t2 = (1, "beta", true)

    val h1 = tupleHashable.hash(t1)(using Algo.blake3Algo)
    val h2 = Hashable.hash(Algo.blake3Algo, t2)

    assertNotEquals(h1.hash, h2.hash)
  }

  test("hashable for Hashed uses hash identity and composes through derived fields") {
    val hashedString: Hashable[Hashed[Algo.Blake3, String]] =
      summon[Hashable[Hashed[Algo.Blake3, String]]]

    val rootHash = Hashable.hash[Algo.Blake3]("root").hash
    val oneHash = Hashable.hash[Algo.Blake3](1).hash
    val twoHash = Hashable.hash[Algo.Blake3](2).hash

    val directA = hashedString.hash(Hashed(rootHash, "payload-a"))(using Algo.blake3Algo)
    val directB = hashedString.hash(Hashed(rootHash, "payload-b"))(using Algo.blake3Algo)
    assertEquals(directA.hash, directB.hash)

    val treeA = HashTree(
      root = Hashed(rootHash, "payload-a"),
      children = List(Hashed(oneHash, 10), Hashed(twoHash, 20))
    )
    val treeB = HashTree(
      root = Hashed(rootHash, "payload-b"),
      children = List(Hashed(oneHash, 30), Hashed(twoHash, 40))
    )
    val treeC = HashTree(
      root = Hashed(rootHash, "payload-b"),
      children = List(Hashed(twoHash, 30), Hashed(twoHash, 40))
    )

    val ha = Hashable.hash[Algo.Blake3](treeA)
    val hb = Hashable.hash[Algo.Blake3](treeB)
    val hc = Hashable.hash[Algo.Blake3](treeC)

    assertEquals(ha.hash, hb.hash)
    assertNotEquals(hb.hash, hc.hash)
  }

  test("foldable fallback provides Hashable for NonEmptyList") {
    val nelHashable: Hashable[NonEmptyList[Int]] =
      summon[Hashable[NonEmptyList[Int]]]

    val a = NonEmptyList.of(1, 2, 3)
    val b = NonEmptyList.of(1, 2, 4)

    val ha = nelHashable.hash(a)(using Algo.blake3Algo)
    val hb = Hashable.hash[Algo.Blake3](b)

    assertNotEquals(ha.hash, hb.hash)
  }
}
