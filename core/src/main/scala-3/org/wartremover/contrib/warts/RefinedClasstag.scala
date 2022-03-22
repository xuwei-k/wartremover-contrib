package org.wartremover.contrib.warts

import org.wartremover.WartTraverser
import org.wartremover.WartUniverse

object RefinedClasstag extends WartTraverser {
  def ctMessage(typeName: String): String =
    s"Refined types should not be used in Classtags since only the first type will be checked at runtime. Type found: $typeName"
  def mfMessage(typeName: String): String =
    s"Refined types should not be used in Manifests since only the first type will be checked at runtime. Type found: $typeName"

  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _ if hasWartAnnotation(tree) =>
          case t: TypeTree =>
            t.tpe.asType match {
              case '[Class[t1 & t2]] =>
                error(t.pos, ctMessage("aaa"))
              case _ =>
                println("other " + t.show)
                super.traverseTree(tree)(owner)
            }
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
