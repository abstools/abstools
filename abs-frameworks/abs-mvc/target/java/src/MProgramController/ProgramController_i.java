package MProgramController;
// ProgramController.abs:6:0: 
public interface ProgramController_i extends abs.backend.java.lib.types.ABSInterface {
    // ProgramController.abs:8:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_list(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:8:1: 
    public  ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> list(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:9:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_add(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:9:1: 
    public  ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> add(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:10:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_save(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:10:1: 
    public  ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> save(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:11:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_edit(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:11:1: 
    public  ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> edit(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:12:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_update(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:12:1: 
    public  ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> update(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:13:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_delete(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:13:1: 
    public  ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> delete(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:14:1: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>>> async_detail(ABS.Framework.Http.ABSHttpRequest_i request);
    // ProgramController.abs:14:1: 
    public  ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<MProgramModel.Program_i>> detail(ABS.Framework.Http.ABSHttpRequest_i request);
    // DGuidePageController.abs:6:9: 
    public  abs.backend.java.lib.runtime.ABSFut<ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<abs.backend.java.lib.types.ABSString>>> async_guide(ABS.Framework.Http.ABSHttpRequest_i request);
    // DGuidePageController.abs:6:9: 
    public  ABS.StdLib.Pair<abs.backend.java.lib.types.ABSString,ABS.StdLib.List<abs.backend.java.lib.types.ABSString>> guide(ABS.Framework.Http.ABSHttpRequest_i request);
}
