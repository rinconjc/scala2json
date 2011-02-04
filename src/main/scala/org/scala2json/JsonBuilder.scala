package org.scala2json

import java.io.Writer
import scala.collection.JavaConversions._
import java.util.Date
import java.text.SimpleDateFormat

trait JsonFactory[T] extends Function2[T,JsonBuilder,Json]

object JsonBuilder{
    
    val blacklist = List("toString","hashCode","getClass")
    
    def allFieldsOf[T](implicit m:Manifest[T]):JsonFactory[T]={
        val validMethods = m.erasure.getMethods.filterNot(m=>m.getReturnType == classOf[Unit] || m.getParameterTypes.size>0 
            || blacklist.contains(m.getName))
        new JsonFactory[T]{
            def apply(target:T, builder:JsonBuilder)=JsonObject(validMethods.map(m=>
                (m.getName, builder.build(m.invoke(target)))).toMap)
        }
    }
    
    def selectedFieldsOf[T<:AnyRef](fields:(T)=>Any)(implicit m:Manifest[T]):JsonFactory[T]={
        val callback = new ProxyUtils.SimpleInterceptor(null, null)
        val proxy = ProxyUtils.createProxy(callback)
        new JsonFactory[T]{
            def apply(target:T, builder:JsonBuilder)={            
                callback.target=target
                callback.results=collection.mutable.Map[String,Any]()
                fields(proxy)
            JsonObject(callback.results.map{e=>(e._1, builder.build(e._2))})
            }
        }
    }
    
    def datesWithFormat(fmt:String)=new JsonFactory[Date]{
        def apply(date:Date, builder:JsonBuilder)=JsonString(new SimpleDateFormat(fmt).format(date))
    }
    
}

class JsonBuilder{
    
    private val factories = collection.mutable.Map[Class[_], JsonFactory[_]]()
    
    def using[T](factory:JsonFactory[T])(implicit m:Manifest[T])={
        factories+=((m.erasure, factory))
        this
    }
    
    def build[T](target:T):Json=target match{
        case map:Map[_,_]=>JsonObject(map.map(e=>(e._1.toString, build(e._2))))
        case traversable:Traversable[_]=>JsonArray(traversable.toSeq.map(build(_)))
        case array:Array[_]=>JsonArray(array.toSeq.map(build(_)))
        case null => JsonNull
        case num:Number=>JsonNumber(num)
        case b:Boolean=>JsonBool(b)
        case s:String => JsonString(s)
        case x:AnyRef if(factories.contains(x.getClass)) => { val factory = factories(x.getClass).asInstanceOf[JsonFactory[AnyRef]]
                factory(x,this)
            }
        case y => JsonString(y.toString) //fallback if no factory defined for this class
    }
    
    def print[T](target:T, writer:Writer)={
        build(target).print(writer)
    }
}
