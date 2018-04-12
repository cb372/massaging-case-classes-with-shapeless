package example

object UsingChimney extends App {

  import io.scalaland.chimney._

  implicit def wrapWithOption[A]: Transformer[A, Option[A]] = 
    new Transformer[A, Option[A]] {
      def transform(a: A): Option[A] = Some(a)
    }

  val domainModel = DomainModel("yeah", 123, true)

  import io.scalaland.chimney.dsl._

  println(domainModel.into[ApiRepr].transform)

}
