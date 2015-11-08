package day11

import scala.language.implicitConversions // eyeroll
import scala.language.higherKinds

import scalaz._
import Scalaz._

object MonocleEx {

  // This is a Scalaz port of the example from the Monocle
  // documentation[1].
  //
  // [1]: https://github.com/julien-truffaut/Monocle

  case class Street(name: String)
  case class Address(street: Street)
  case class Company(name: String, address: Address)
  case class Employee(name: String, company: Company)

  // From the Monocle docs:
  //
  // val _name   : Lens[Street  , String]  =
  // val _street : Lens[Address , Street]  = ...
  // val _address: Lens[Company , Address] = ...
  // val _company: Lens[Employee, Company] = ...
  //
  // (_company ^|-> _address ^|-> _street ^|-> _name).modify(_.capitalize)(employee)

  val companyAddress: Lens[Company, Address] =
    Lens.lensu[Company, Address] (
      (a, value) => a.copy(address = value),
      _.address
    )

  val addressStreet: Lens[Address, Street] =
    Lens.lensu[Address, Street] (
      (a, value) => a.copy(street = value),
      _.street
    )

  val streetName: Lens[Street, String] =
    Lens.lensu[Street, String] (
      (a, value) => a.copy(name = value),
      _.name
    )

  val companyStreetName: Lens[Company, String] =
    companyAddress >=> addressStreet >=> streetName

  val capitalizeStreetName1: Company => Company =
    company => companyStreetName mod(_.capitalize, company)

  val capitalizeStreetName2: Company => Company =
    companyStreetName =>= { _.capitalize }

  val capitalizeStreetName3: State[Company, String] =
    (companyStreetName %= { x: String => x.capitalize })

  val moveCompany: Company => Company =
    company => companyStreetName set(company, "Somewhere Else Pl.")

}

object MapLensesEx {

  case class Id(value: String)
  case class User(name: String, email: String)

  type Users = Map[Id, User]

  def user(id: Id): Lens[Users, Option[User]] =
    Lens.lensu[Users, Option[User]] (
      (users, userO) => userO match {
        case Some(user) => users + (id -> user)
        case None => users - id
      },
      _.get(id)
    )

  val email: Lens[User, String] =
    Lens.lensu[User, String] (
      (user, email) => user.copy(email = email),
      _.email
    )

  // def somePLens[A]: PLens[Option[A], A] = PLensFamily.somePLens

  def somePLens[A]: PLens[Option[A], A] =
    Lens.lensu[Option[A], A] (
      (x, y) => Some(y),
      _.get
    ).partial

  def userEmail(id: Id): PLens[Users, String] =
    user(id).partial >=> somePLens >=> email.partial

}

object Main extends App {

  println()

  {
    import MonocleEx._

    val street = Street("main st.")
    val address = Address(street)
    val company = Company("The Very Big Corporation", address)
    val employee = Employee("Bob Worker", company)

    println("companyStreetName get company:")
    println(companyStreetName get company) // main st.
    println()

    println("companyStreetName get capitalizeStreetName1(company):")
    println(companyStreetName get capitalizeStreetName1(company)) // Main st.
    println()

    println("companyStreetName get capitalizeStreetName2(company):")
    println(companyStreetName get capitalizeStreetName2(company)) // Main st.
    println()

    println("companyStreetName get (capitalizeStreetName3 run company)._1:")
    println(companyStreetName get (capitalizeStreetName3 run company)._1) // Main st.
    println()

    println("companyStreetName get moveCompany(company):")
    println(companyStreetName get moveCompany(company)) // Somewhere Else Pl.
    println()

  }

  {
    import MapLensesEx._

    val users = Map(
      Id("1") -> User("Foo Bar", "foo@bar"),
      Id("2") -> User("Baz Raz", "baz@raz")
    )

    println(userEmail(Id("2")) get users)
    println()

  }

}
