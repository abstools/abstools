package abs.fli.java;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import abs.backend.java.lib.runtime.ABSInitObjectTask;
import abs.backend.java.lib.runtime.ABSObject;
import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.ABSThread;
import abs.backend.java.lib.runtime.COG;
import abs.backend.java.lib.runtime.Task;

/**
 * 
 * @author pwong
 *
 */
public class ABSFactory {
	
	/**
	 * XXX Copied from generated {@code __ABS_createNewCOG(ABSObject)} 
	 * @param <T> the type of the {@link ABSObject} to be created
	 * @param from
	 * @param ABSName
	 * @param constructor
	 * @return
	 */
	@SuppressWarnings({ "unchecked" })
	public static final <T extends ABSObject> T createNewCOG(ABSObject from,
			String ABSName, Constructor<T> constructor, Object...initargs) {

		final ABSRuntime runtime = ABSRuntime.getCurrentRuntime();
		final COG cog = new COG(runtime, constructor.getDeclaringClass());
		final ABSThread thread = ABSRuntime.getCurrentThread();
		final COG oldCOG = ABSRuntime.getCurrentCOG();
		final Task<?> sendingTask = ABSRuntime.getCurrentTask();

		thread.setCOG(cog);
		try {
			T result = (T) runtime.getForeignObject(ABSName);
			if (result == null) {
				result = constructor.newInstance(initargs);
			}
			runtime.cogCreated(result);
			cog.getScheduler()
			   .addTask(new ABSInitObjectTask<T>(sendingTask, from,	result));
			
			return result;
		} catch (InstantiationException e) {
			e.printStackTrace();
			return null;
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			return null;
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
			return null;
		} catch (InvocationTargetException e) {
			e.printStackTrace();
			return null;
		} finally {
			thread.setCOG(oldCOG);
		}
	}

}
