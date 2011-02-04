package org.scala2json

import java.io.StringWriter
import org.specs.Specification
import JsonBuilder._

object JsonBuilderSpecs extends Specification{
    
    "JsonBuilder" should{
        "build basic objects" in{
            new JsonBuilder build("Test") must beEqualTo(JsonString("Test"))
            new JsonBuilder build(10) must beEqualTo(JsonNumber(10))
            new JsonBuilder build(true) must beEqualTo(JsonBool(true))
            new JsonBuilder build(null) must beEqualTo(JsonNull)
            new JsonBuilder build(Array(1,2,3)) must beEqualTo(JsonArray(Seq(JsonNumber(1),JsonNumber(2),JsonNumber(3))))
            new JsonBuilder build(Seq(1,2,3)) must beEqualTo(JsonArray(Seq(JsonNumber(1),JsonNumber(2),JsonNumber(3))))
            new JsonBuilder build(Map(1->"one",2->"two")) must beEqualTo(JsonObject(Map("1"->JsonString("one"), "2"->JsonString("two"))))
        }
        
        "build nested objects" in{
            val json = new JsonBuilder()
                .using(allFieldsOf[Foo])
                .using(selectedFieldsOf[Bar](b=>(b.name, b.foo)))
                .build(new Bar(5,"bar",new Foo("foo")))
            
            val writer = new StringWriter 
            json.print(writer)
            println(writer.toString)
            json must beEqualTo(JsonObject(Map("name"->JsonString("bar")
                , "foo"->JsonObject(Map("name"->JsonString("foo"))))))
        }
        
    }

}

class Foo(val name:String)

class Bar(val id:Int, val name:String, val foo:Foo)



