package chapter4_monads

object ReaderMonad extends App {

  import cats.data.Reader
  final case class Cat(name: String, favoriteFood: String)
  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)
  // catName: Reader[Cat, String] = Kleisli(<function1>)

  catName.run(Cat("Garfield", "lasagne"))
  // res1: cats.package.Id[String] = "Garfield"

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello $name")
  greetKitty.run(Cat("Heathcliff", "junk food"))
  // res2: cats.package.Id[String] = "Hello Heathcliff"

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet. $feed."

  greetAndFeed(Cat("Garfield", "lasagne"))
  // res3: cats.package.Id[String] = "Hello Garfield. Have a nice bowl of lasagne."
  greetAndFeed(Cat("Heathcliff", "junk food"))
  // res4: cats.package.Id[String] = "Hello Heathcliff. Have a nice bowl of junk food."

  // Hacking on Readers
  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def findUsernameSimple(db: Db, userId: Int): Option[String] =
    db.usernames.get(userId)

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  def checkPasswordSimple(db: Db, username: String, password: String): Boolean =
    db.passwords.get(username).contains(password)

  import cats.syntax.applicative._

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      passwordOk <- username.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
    } yield passwordOk

  def checkLoginSimple(db: Db, userId: Int, password: String): Boolean =
    findUsernameSimple(db, userId)
      .exists(checkPasswordSimple(db, _, password))

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  checkLogin(1, "zerocool").run(db)
  // res7: cats.package.Id[Boolean] = true
  checkLogin(4, "davinci").run(db)
  // res8: cats.package.Id[Boolean] = false
}
