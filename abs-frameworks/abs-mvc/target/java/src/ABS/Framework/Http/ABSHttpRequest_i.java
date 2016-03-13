package ABS.Framework.Http;
// ABSHttpRequest.abs:5:0: 
public interface ABSHttpRequest_i extends abs.backend.java.lib.types.ABSInterface {
    // ABSHttpRequest.abs:7:1: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_getInput(abs.backend.java.lib.types.ABSString key);
    // ABSHttpRequest.abs:7:1: 
    public  abs.backend.java.lib.types.ABSString getInput(abs.backend.java.lib.types.ABSString key);
    // ABSHttpRequest.abs:8:1: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_getRequestProperty(abs.backend.java.lib.types.ABSString key);
    // ABSHttpRequest.abs:8:1: 
    public  abs.backend.java.lib.types.ABSString getRequestProperty(abs.backend.java.lib.types.ABSString key);
}
