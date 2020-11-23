package tint

import org.scalajs.ir.Trees._
import org.scalajs.ir.Position

object Sample {
  implicit val noPosition: Position = Position.NoPosition

  def helloWorld: Tree = JSMethodApply(
    JSGlobalRef("console"),
    StringLiteral("log"),
    List(
      StringLiteral("Hello world!")
    )
  )
}