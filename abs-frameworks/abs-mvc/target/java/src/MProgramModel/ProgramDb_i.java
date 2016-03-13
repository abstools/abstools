package MProgramModel;
// Program.abs:56:0: 
public interface ProgramDb_i extends abs.backend.java.lib.types.ABSInterface {
    // Program.abs:57:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.List<MProgramModel.Program_i>> async_findAll(abs.backend.java.lib.types.ABSString className);
    // Program.abs:57:1: 
    public  ABS.StdLib.List<MProgramModel.Program_i> findAll(abs.backend.java.lib.types.ABSString className);
    // Program.abs:58:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.List<MProgramModel.Program_i>> async_findAllByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString query);
    // Program.abs:58:1: 
    public  ABS.StdLib.List<MProgramModel.Program_i> findAllByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString query);
    // Program.abs:59:1: 
    public  abs.backend.java.lib.runtime.ABSFut<MProgramModel.Program_i> async_find(abs.backend.java.lib.types.ABSString className);
    // Program.abs:59:1: 
    public  MProgramModel.Program_i find(abs.backend.java.lib.types.ABSString className);
    // Program.abs:60:1: 
    public  abs.backend.java.lib.runtime.ABSFut<MProgramModel.Program_i> async_findByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString query);
    // Program.abs:60:1: 
    public  MProgramModel.Program_i findByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString query);
    // Program.abs:61:1: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_save(MProgramModel.Program_i object);
    // Program.abs:61:1: 
    public  abs.backend.java.lib.types.ABSUnit save(MProgramModel.Program_i object);
    // Program.abs:62:1: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_delete(MProgramModel.Program_i object);
    // Program.abs:62:1: 
    public  abs.backend.java.lib.types.ABSUnit delete(MProgramModel.Program_i object);
    // Program.abs:63:1: 
    public  abs.backend.java.lib.runtime.ABSFut<MProgramModel.Program_i> async_update(MProgramModel.Program_i object);
    // Program.abs:63:1: 
    public  MProgramModel.Program_i update(MProgramModel.Program_i object);
    // Program.abs:64:1: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_log(abs.backend.java.lib.types.ABSString log);
    // Program.abs:64:1: 
    public  abs.backend.java.lib.types.ABSString log(abs.backend.java.lib.types.ABSString log);
}
