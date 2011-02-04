package org.scala2json

import org.specs._
import java.io.StringWriter

object JsonSpecs extends Specification{
    "Json" should {
        "print correctly" in{
            val writer = new StringWriter()
            JsonObject(
                Map("field"->JsonString("string")
                    ,"array"->JsonArray(Array(JsonNumber(10), JsonBool(true)))
                    ,"object"->JsonObject(
                        Map("field2"->JsonObject(
                            Map("field3"->JsonString("value3")))))))
                    .print(writer)
                    writer.toString must beEqualTo("""{"field":"string","array":[10,true],"object":{"field2":{"field3":"value3"}}}""")
            
        }
    }
}
