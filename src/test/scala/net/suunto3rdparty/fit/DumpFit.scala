package net.suunto3rdparty
package fit

import com.garmin.fit
import com.garmin.fit._
import java.time.ZonedDateTime
import scala.collection.JavaConverters._

import org.scalatest.{FlatSpec, Matchers}

class DumpFit extends FlatSpec with Matchers {

  "Decoder" should "dump a fit file" in {

    val decode = new Decode
    val in = getClass.getResourceAsStream("/decodeTest.fit")

    try {
      val listener = new MesgListener {
        override def onMesg(mesg: Mesg): Unit = {
          println(s"${mesg.getName}")
          val fields = mesg.getFields.asScala
          for (f <- fields) {
            println(s"  ${f.getName}:${f.getValue}")
          }
        }
      }
      decode.read(in, listener)
    } finally {
      in.close()
    }
    //mesgBroadcaster.run(in)
  }

}
