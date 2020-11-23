// import $ivy.`com.lihaoyi::mill-contrib-bloop:0.8.0`
import mill._
import scalalib._
import scalajslib._
import scalajslib.api.ModuleKind

object tint extends ScalaJSModule {
  def scalaVersion = "2.13.3"
  def scalaJSVersion = "1.2.0"

  def ivyDeps = Agg(ivy"org.scala-js::scalajs-linker::1.2.0") 

  def moduleKind = T(ModuleKind.CommonJSModule)

  object test extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.5")
    def testFrameworks = Seq("utest.runner.Framework")
    def moduleKind = T(ModuleKind.CommonJSModule)
  }
}

object sample extends ScalaJSModule {
  def scalaVersion = "2.13.3"
  def scalaJSVersion = "1.2.0"  
}
