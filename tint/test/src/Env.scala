package tint

import utest._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.Position
import org.scalajs.ir.OriginalName

object EnvTests extends TestSuite{
  implicit val position = Position.NoPosition

  val tests = Tests {
    test("multiworld splitting and propagating updates") {
      val env = Env.empty

      val identX = LocalIdent(LocalName("x"))
      val envX = env.bind(identX, 0)

      val identY = LocalIdent(LocalName("y"))
      val envY = envX.bind(identY, 1)
      envX.read(identX) ==> envY.read(identX)

      envY.assign(identX, 10)
      envX.read(identX).asInstanceOf[Int] ==> 10
      envY.read(identX).asInstanceOf[Int] ==> 10
      
      val envShadowed = envX.bind(identX, 0)
      envX.assign(identX, 5)
      assert(envShadowed.read(identX) != envX.read(identX))
    }

    test("missing binding") {
      val env = Env.empty
      val ident = LocalIdent(LocalName("x"))
      val e = intercept[NoSuchElementException] {
        env.read(ident)
      }
      e.getMessage() ==> "No variable LocalIdent(LocalName<x>) in Env"
    }

    test("this binding") {
      val className = ClassName("FooBar")
      val instance = new Instance(className)
      val env = Env.empty.setThis(instance)
      env.getThis ==> instance
    }

    test("missing this binding") {
      val env = Env.empty
      val e = intercept[NoSuchElementException] {
        env.getThis
      }
      e.getMessage() ==> "No THIS in current Env"
    }
  }
}