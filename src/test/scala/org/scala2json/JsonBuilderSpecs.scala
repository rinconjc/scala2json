package org.scala2json

import java.io.StringWriter
import org.specs.Specification
import JsonBuilder._

object JsonBuilderSpecs extends Specification{
    
    "JsonBuilder " should{
        "work" in{
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



