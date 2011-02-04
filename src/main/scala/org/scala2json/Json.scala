package org.scala2json

import java.io.Writer

trait Json{
    def print(writer:Writer)
}

case object JsonNull extends Json{
    def print(writer:Writer)=writer.write("null")
}

case class JsonNumber(value:Number) extends Json{
    def print(writer:Writer) = writer.write(String.valueOf(value))
}

case class JsonBool(value:Boolean) extends Json{
    def print(writer:Writer) = writer.write(String.valueOf(value))
}

case class JsonString(value:String) extends Json{
    def print(writer:Writer) = writer.write("\"" + value + "\"")
}

case class JsonArray(values:Seq[Json]) extends Json{
    def print(writer:Writer) = {
        writer.write("[")
        if(!values.isEmpty){
            values.head.print(writer)
            values.tail.foreach{j=>{writer.write(","); j.print(writer)}}
        }
        writer.write("]")
    }
}

case class JsonObject(attributes:collection.Map[String,Json]) extends Json{
    def print(writer:Writer) = {
        writer.write("{")
        val attrs = attributes.toSeq
        if(!attrs.isEmpty){
            val (name,value) = attrs.head
            writer.write("\""+name+"\":")
            value.print(writer)
            attrs.tail.foreach{(e) =>
                writer.write(",\"" + e._1+"\":")
                e._2.print(writer)                
            }            
        }
        writer.write("}")
    }
}


