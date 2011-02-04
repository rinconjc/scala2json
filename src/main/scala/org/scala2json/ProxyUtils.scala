package org.scala2json

import java.lang.reflect.Method
import net.sf.cglib.proxy.{Enhancer, Callback, MethodInterceptor, MethodProxy}

object ProxyUtils{
    
    def createProxy[T](callback:Callback)(implicit m:Manifest[T]):T={
        val enhancer = new Enhancer
        val cls = m.erasure 
        enhancer.setSuperclass(cls)
        enhancer.setClassLoader(cls.getClassLoader)
        enhancer.setCallback(callback)
        val argTypes = cls.getConstructors()(0).getParameterTypes
        val args = argTypes map{t=>if(t.isPrimitive)defaultPrimitive(t).asInstanceOf[AnyRef] else null}
        val proxy = enhancer.create(argTypes, args).asInstanceOf[T]
        proxy        
    }
    
    class SimpleInterceptor(var target:AnyRef, var results: collection.mutable.Map[String,Any]) extends MethodInterceptor{
        def intercept(obj:Object, method:Method, args:Array[AnyRef], proxyMethod:MethodProxy):AnyRef={
            if(target!=null){
                val value = proxyMethod.invoke(target, args)
                results+=((method.getName,value))
                value
            }else proxyMethod.invokeSuper(obj,args)
        }
    }
    
    private def defaultPrimitive[T](clazz:Class[T]):Any=clazz match{
        case java.lang.Long.TYPE=>0L
        case java.lang.Integer.TYPE=>0
        case java.lang.Character.TYPE=>0
        case java.lang.Byte.TYPE=>0
        case java.lang.Short.TYPE=>0
        case java.lang.Float.TYPE=>0f
        case java.lang.Double.TYPE=>0d
        case java.lang.Boolean.TYPE=>false
    }
    
}
