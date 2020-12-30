import $ivy.`com.lihaoyi::mill-contrib-bloop:0.9.4`
import mill._
import scalalib._
import scalajslib._
import scalajslib.api.{ModuleKind, JsEnvConfig}

object tint extends ScalaJSModule {
  def scalaVersion = "2.13.4"
  def scalaJSVersion = "1.3.1"

  def jsEnvConfig: T[JsEnvConfig] = T { JsEnvConfig.NodeJs(
    args=List("--stack-size=100000")
  ) }

  def ivyDeps = Agg(ivy"org.scala-js::scalajs-linker::1.3.1")
  def moduleKind = T(ModuleKind.CommonJSModule)
  def useECMAScript2015 = true

  object test extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.5")
    def testFrameworks = Seq("utest.runner.Framework")
    def moduleKind = T(ModuleKind.CommonJSModule)
  }
}

object sample extends ScalaJSModule {
  def scalaVersion = "2.13.4"
  def scalaJSVersion = "1.3.1"
  def useECMAScript2015 = true
  def moduleKind = T(ModuleKind.CommonJSModule)
}
