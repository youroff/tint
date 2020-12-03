package utils

object Utils {

  implicit class OptionsOps[A](val value: Option[A]) extends AnyVal { 
    
    def getOrThrow(msg: String): A = value.getOrElse(throw new AssertionError(msg))
  }
}
