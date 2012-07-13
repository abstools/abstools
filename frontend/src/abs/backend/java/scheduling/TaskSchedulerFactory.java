/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.scheduling;

import abs.backend.java.lib.runtime.ABSRuntime;
import abs.backend.java.lib.runtime.ABSThreadManager;
import abs.backend.java.lib.runtime.COG;

public interface TaskSchedulerFactory {
    TaskScheduler createTaskScheduler(ABSRuntime absRuntime, COG cog, ABSThreadManager m, ScheduableTasksFilter filter);
}
