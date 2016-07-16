package MProgramDbImpl;
// ProgramDb.abs:8:0: 
public interface ProgramDb_i extends abs.backend.java.lib.types.ABSInterface {
    // ProgramDb.abs:9:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.List<MProgramModel.Program_i>> async_findAll(abs.backend.java.lib.types.ABSString className);
    // ProgramDb.abs:9:1: 
    public  ABS.StdLib.List<MProgramModel.Program_i> findAll(abs.backend.java.lib.types.ABSString className);
    // ProgramDb.abs:10:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.List<MProgramModel.Program_i>> async_findAllByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString query);
    // ProgramDb.abs:10:1: 
    public  ABS.StdLib.List<MProgramModel.Program_i> findAllByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString query);
    // ProgramDb.abs:11:1: 
    public  abs.backend.java.lib.runtime.ABSFut<MProgramModel.Program_i> async_find(abs.backend.java.lib.types.ABSString className);
    // ProgramDb.abs:11:1: 
    public  MProgramModel.Program_i find(abs.backend.java.lib.types.ABSString className);
    // ProgramDb.abs:12:1: 
    public  abs.backend.java.lib.runtime.ABSFut<MProgramModel.Program_i> async_findByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString query);
    // ProgramDb.abs:12:1: 
    public  MProgramModel.Program_i findByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString query);
    // ProgramDb.abs:13:1: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_save(MProgramModel.Program_i object);
    // ProgramDb.abs:13:1: 
    public  abs.backend.java.lib.types.ABSUnit save(MProgramModel.Program_i object);
    // ProgramDb.abs:14:1: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSUnit> async_delete(MProgramModel.Program_i object);
    // ProgramDb.abs:14:1: 
    public  abs.backend.java.lib.types.ABSUnit delete(MProgramModel.Program_i object);
    // ProgramDb.abs:15:1: 
    public  abs.backend.java.lib.runtime.ABSFut<MProgramModel.Program_i> async_update(MProgramModel.Program_i object);
    // ProgramDb.abs:15:1: 
    public  MProgramModel.Program_i update(MProgramModel.Program_i object);
    // ProgramDb.abs:16:1: 
    public  abs.backend.java.lib.runtime.ABSFut<abs.backend.java.lib.types.ABSString> async_log(abs.backend.java.lib.types.ABSString log);
    // ProgramDb.abs:16:1: 
    public  abs.backend.java.lib.types.ABSString log(abs.backend.java.lib.types.ABSString log);
}
