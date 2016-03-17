package net.suunto3rdparty
package fit

import java.io.InputStream

import com.garmin.fit
import com.garmin.fit._
import java.time.ZonedDateTime

import scala.collection.JavaConverters._
import org.scalatest.{FlatSpec, Matchers}

class DumpFit extends FlatSpec with Matchers {

  "Decoder" should "dump a fit file" in {
    val in = getClass.getResourceAsStream("/decodeTest.fit")
    decodeFile(in)
  }

  "Decoder" should "dump extra information from a fit file" in {
    val in = getClass.getResourceAsStream("/decodeTestExt.fit")
    decodeFile(in, "record")
  }

  def decodeFile(in: InputStream, ignoreMessages: String*): Unit = {
    val decode = new Decode
    try {
      val listener = new MesgListener {
        override def onMesg(mesg: Mesg): Unit = {
          if (!ignoreMessages.contains(mesg.getName)) {
            println(s"${mesg.getName}")
            val fields = mesg.getFields.asScala
            for (f <- fields) {
              f.getName match {
                case "timestamp" =>
                  val time = new DateTime(f.getLongValue)
                  println(s"  ${f.getName}:${time.toString}")
                case _ =>
                  println(s"  ${f.getName}:${f.getValue}")
              }
            }
          }
        }
      }
      decode.read(in, listener)
    } finally {
      in.close()
    }
  }
}
