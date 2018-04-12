package example

case class DomainModel(a: String, b: Int, c: Boolean)

case class ApiRepresentation(x: String, y: Int, z: Boolean)

case class ApiReprABC(a: String, b: Int, c: Boolean)

case class FiveFields(a: String, b: Int, c: Boolean, d: String, e: Double)

case class FourFields(a: String, b: Int, c: Boolean, e: Double)

case class ApiRepr(a: String, b: Option[Int], c: Option[Boolean])

object UsingShapeless extends App {

  import shapeless._
  import shapeless.labelled._
  import shapeless.poly._

  object WrapWithOptionIfNecessary extends Poly1 {

    // Reflexively convert any field to itself by doing nothing
    implicit def refl[K, V]: Case.Aux[FieldType[K, V], FieldType[K, V]] =
      at[FieldType[K, V]](Predef.identity)

    // Convert a field `x: A` into a field `x: Option[A]` by wrapping it in Some()
    implicit def wrapWithOption[K, V]: Case.Aux[FieldType[K, V], FieldType[K, Option[V]]] =
      at[FieldType[K, V]](x => field[K](Some(x)))

  }

  /**
    * Massages an HList of type `From` into an HList of type `To`
    * using the polymorphic function `WrapWithOptionIfNecessary` 
    * to convert elements appropriately.
    */
  trait Massage[From <: HList, To <: HList] extends DepFn1[From] {
    type Out = To
  }

  /**
    * An HNil can be trivially massaged to itself.
    */
  implicit val massageHNilToHNil: Massage[HNil, HNil] = new Massage[HNil, HNil] {
    def apply(hlist: HNil): HNil = HNil
  }

  /**
    * An HList of type `FromHead :: FromTail` can be massaged into a type `ToHead :: ToTail` if:
    * - we can inductively massage `FromTail` into `ToTail`, and
    * - we can convert a value of type `FromHead` into a value of type `ToHead`
    */
  implicit def massageHConsToHCons[FromHead,
                                   ToHead,
                                   FromTail <: HList,
                                   ToTail <: HList](implicit
      massageRemainder: Massage[FromTail, ToTail],
      convertHead: Case1.Aux[WrapWithOptionIfNecessary.type, FromHead, ToHead]
    ): Massage[FromHead :: FromTail, ToHead :: ToTail] =
    new Massage[FromHead :: FromTail, ToHead :: ToTail] {
      def apply(from: FromHead :: FromTail): ToHead :: ToTail =
        convertHead(from.head) :: massageRemainder(from.tail)
   }

  // DSL

  class PartiallyApplied[FromRepr <: HList](fromRepr: FromRepr) {
    def to[To]: PartiallyApplied2[FromRepr, To] = new PartiallyApplied2(fromRepr)
  }

  class PartiallyApplied2[FromRepr <: HList, To](fromRepr: FromRepr) {
    def please[ToRepr <: HList](implicit 
      labelledGen: LabelledGeneric.Aux[To, ToRepr], massager: Massage[FromRepr, ToRepr]): To =
      labelledGen.from(massager.apply(fromRepr))
  }

  def massage[From, FromRepr <: HList](from: From)(implicit 
      labelledGen: LabelledGeneric.Aux[From, FromRepr]) =
    new PartiallyApplied[FromRepr](labelledGen.to(from))

  val domainModel = DomainModel("yeah", 123, true)
  println(massage(domainModel).to[ApiRepr].please)

}

