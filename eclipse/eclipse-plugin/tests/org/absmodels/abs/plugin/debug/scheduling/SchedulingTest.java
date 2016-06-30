/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.absmodels.abs.plugin.debug.scheduling;


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.absmodels.abs.plugin.debug.scheduling.GUIScheduler;
import org.absmodels.abs.plugin.debug.scheduling.NStepScheduler;
import org.absmodels.abs.plugin.debug.scheduling.RandomScheduler;
import org.absmodels.abs.plugin.debug.scheduling.RunToLineScheduler;
import org.absmodels.abs.plugin.debug.scheduling.SchedulingStrategy;
import org.absmodels.abs.plugin.debug.scheduling.StepOverScheduler;
import org.absmodels.abs.plugin.debug.scheduling.TotalScheduler;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.stubbing.OngoingStubbing;

import abs.backend.java.observing.TaskStackFrameView;
import abs.backend.java.observing.TaskStackView;
import abs.backend.java.observing.TaskView;
import abs.backend.java.scheduling.ScheduleAction;
import abs.backend.java.scheduling.ScheduleOptions;
import abs.backend.java.scheduling.SimpleTaskScheduler.TaskInfo;
import abs.backend.java.scheduling.TaskScheduler;

public class SchedulingTest {

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}
	
	@Test
	public void testRandomScheduler(){
		RandomScheduler rs = new RandomScheduler(new Random());
		ScheduleOptions so = mock(ScheduleOptions.class);
		List<ScheduleAction> sal = new ArrayList<ScheduleAction>();
		for(int i=0; i<100; i++){
			sal.add(mock(ScheduleAction.class));
		}
		when(so.allOptions()).thenReturn(sal);
		when(so.numOptions()).thenReturn(100);
		
		ScheduleAction chosenActions;
		long seed = new Random().nextLong();
		rs = new RandomScheduler(new Random(seed));
		List<ScheduleAction> testActions = new ArrayList<ScheduleAction>();
		for(int i=0; i<20; i++){
			chosenActions = rs.choose(so);
			assertTrue(sal.contains(chosenActions));
			testActions.add(chosenActions);
		}
		
		RandomScheduler rs2 = new RandomScheduler(new Random(seed));
		List<ScheduleAction> testActions2 = new ArrayList<ScheduleAction>();
		for(int i=0; i<20; i++){
			chosenActions = rs2.choose(so);
			assertTrue(sal.contains(chosenActions));
			testActions2.add(chosenActions);
		}
		
		for(int i=0; i<testActions.size(); i++){
			assertTrue("Element "+i+" should be equal!", testActions.get(i).equals(testActions2.get(i)));
		}
		
		//TODO maybe write test for scheduling
	}
	
//	@Test
//	public void testRunTaskScheduler(){
//		SchedulingStrategy ss = mock(SchedulingStrategy.class);
//		TaskView tv = mock(TaskView.class);
//		COGView cogv = mock(COGView.class);
//		int testCogId = new Random().nextInt();
//		int testTaskId = new Random().nextInt();
//		when(cogv.getID()).thenReturn(testCogId);
//		when(tv.getCOG()).thenReturn(cogv);
//		when(tv.getID()).thenReturn(testTaskId);
//		RunTaskScheduler rts = new RunTaskScheduler(ss);
//		rts.setTask(tv);
//		
//		ScheduleOptions so = mock(ScheduleOptions.class);
//		List<ScheduleAction> sal = new ArrayList<ScheduleAction>();
//		ScheduleTask st;
//		COG cogfa;
//		int cogId = testCogId;
//		Random cogIdRandom = new Random();
//		for(int i=0; i<40; i++){
//			st = mock(ScheduleTask.class);
//			while(cogId==testCogId){
//				cogId = cogIdRandom.nextInt();
//			}
//			cogfa = mock(COG.class);
//			when(cogfa.getID()).thenReturn(cogId);
//			when(st.getCOG()).thenReturn(cogfa);
//			sal.add(st);
//		}
//		
//		ScheduleAction sa;
//		TaskView tvfa;
//		int taskId = testTaskId;
//		for(int i=0; i<40; i++){
//			sa = mock(ScheduleAction.class);
//			while(taskId==testTaskId){
//				taskId = cogIdRandom.nextInt();
//			}
//			tvfa = mock(TaskView.class);
//			when(tvfa.getID()).thenReturn(taskId);
//			when(sa.getTask()).thenReturn(tvfa);
//			sal.add(sa);
//		}
//		
//		
//		for(int i=0; i<10; i++){
//			st = mock(ScheduleTask.class);
//			cogfa = mock(COG.class);
//			when(cogfa.getID()).thenReturn(testCogId);
//			when(st.getCOG()).thenReturn(cogfa);
//			sal.add(st);
//		}
//		for(int i=0; i<10; i++){
//			sa = mock(ScheduleAction.class);
//			tvfa = mock(TaskView.class);
//			when(tvfa.getID()).thenReturn(testTaskId);
//			when(sa.getTask()).thenReturn(tvfa);
//			sal.add(sa);
//		}
//		when(so.allOptions()).thenReturn(sal);
//		when(so.numOptions()).thenReturn(100);
//		
//		//TODO choose and remove actions till no more actions to choose -> switch to GUI
//		ScheduleAction csa;
//		for(int i=0; i<20; i++){
//			csa = rts.choose(so);
//			if(csa instanceof ScheduleTask){
//				assertEquals("Element "+i+" must correspond to the task.", csa.getCOG().getID(), testCogId);
//			} else{
//				assertEquals("Element "+i+" must correspond to the task.", csa.getTask().getID(), testTaskId);
//			}
//		}
//	}
	
	@Test
	public void testGUIScheduler(){
		SchedulingStrategy ss = mock(SchedulingStrategy.class);
		doCallRealMethod().when(ss).setBaseScheduler(any(TotalScheduler.class));
		doCallRealMethod().when(ss).setCurrentScheduler(any(TotalScheduler.class));
		doCallRealMethod().when(ss).doSingleStep();
		TotalScheduler ts = mock(TotalScheduler.class);
		ScheduleAction scheduleAction = mock(ScheduleAction.class, Mockito.RETURNS_DEEP_STUBS);
		when(ts.choose(any(ScheduleOptions.class))).thenReturn(scheduleAction);
		TaskInfo taskInfo = mock(TaskInfo.class);
		when(ts.schedule(any(TaskScheduler.class), any(List.class))).thenReturn(taskInfo);
		ss.setBaseScheduler(ts);
		final GUIScheduler gs = new GUIScheduler(ss);
		ss.setCurrentScheduler(gs);
		
		Thread schedulingThread = new Thread(new Runnable() {
			
			@Override
			public void run() {
				ScheduleOptions scheduleOptions = mock(ScheduleOptions.class, Mockito.RETURNS_DEEP_STUBS);
				ScheduleAction chosenAction = gs.choose(scheduleOptions);
			}
		});
		try {
			schedulingThread.start();
			ss.doSingleStep();
			schedulingThread.join(1000);
			assertEquals(gs, ss.curScheduler);
			assertEquals(ts, ss.baseScheduler);
		} catch (InterruptedException e) {
			fail();
		}
	}
	
	@Test
	public void testNStepScheduler(){
		final SchedulingStrategy ss = mock(SchedulingStrategy.class);
		doCallRealMethod().when(ss).setBaseScheduler(any(TotalScheduler.class));
		doCallRealMethod().when(ss).setCurrentScheduler(any(TotalScheduler.class));
		doCallRealMethod().when(ss).doSingleStep();
		TotalScheduler ts = mock(TotalScheduler.class);
		ScheduleAction scheduleAction = mock(ScheduleAction.class, Mockito.RETURNS_DEEP_STUBS);
		when(ts.choose(any(ScheduleOptions.class))).thenReturn(scheduleAction);
		TaskInfo taskInfo = mock(TaskInfo.class);
		when(ts.schedule(any(TaskScheduler.class), any(List.class))).thenReturn(taskInfo);
		ss.setBaseScheduler(ts);
		final int numberSteps = new Random().nextInt(100);
		final NStepScheduler nss = new NStepScheduler(ss, numberSteps);
		ss.setCurrentScheduler(nss);
		
		Thread schedulingThread = new Thread(new Runnable() {
			
			@Override
			public void run() {
				ScheduleOptions scheduleOptions = mock(ScheduleOptions.class, Mockito.RETURNS_DEEP_STUBS);
				for(int i=0; i<numberSteps; i++){
					ss.curScheduler.choose(scheduleOptions);
				}
				assertEquals(nss, ss.curScheduler);
				ss.curScheduler.choose(scheduleOptions);
			}
		});
		try {
			schedulingThread.start();
			schedulingThread.join();
			verify(ss).awaitGUIAction(any(ScheduleOptions.class));
		} catch (InterruptedException e) {
			fail();
		}
		
	}
	
	@Test
	public void testRunToLineScheduler(){
		final SchedulingStrategy ss = mock(SchedulingStrategy.class);
		doCallRealMethod().when(ss).setBaseScheduler(any(TotalScheduler.class));
		doCallRealMethod().when(ss).setCurrentScheduler(any(TotalScheduler.class));
		doCallRealMethod().when(ss).doSingleStep();
		TotalScheduler ts = mock(TotalScheduler.class);
		ScheduleAction scheduleAction = mock(ScheduleAction.class, Mockito.RETURNS_DEEP_STUBS);
		when(ts.choose(any(ScheduleOptions.class))).thenReturn(scheduleAction);
		TaskInfo taskInfo = mock(TaskInfo.class);
		when(ts.schedule(any(TaskScheduler.class), any(List.class))).thenReturn(taskInfo);
		ss.setBaseScheduler(ts);
		
		final int line = new Random().nextInt(1000);
		final String fileName = "filename";
		
		abs.backend.java.debugging.TaskInfo ti = mock(abs.backend.java.debugging.TaskInfo.class);
		when(ti.getCurrentFile()).thenReturn("filename");
		when(ti.getCurrentLine()).thenReturn(line-5)
			.thenReturn(line-4)
			.thenReturn(line-3)
			.thenReturn(line-2)
			.thenReturn(line-1)
			.thenReturn(line);
		ss.steppedTask = ti;
		RunToLineScheduler rtls = new RunToLineScheduler(ss, fileName, line);
		ss.setCurrentScheduler(rtls);
		
		ScheduleOptions scheduleOptions = mock(ScheduleOptions.class, Mockito.RETURNS_DEEP_STUBS);
		for(int i=0; i<6; i++){
			ss.curScheduler.choose(scheduleOptions);
		}
		
		verify(ss).awaitGUIAction(any(ScheduleOptions.class));
	}
	
	@Test
	public void testStepOverScheduler(){
		final SchedulingStrategy ss = mock(SchedulingStrategy.class);
		doCallRealMethod().when(ss).setBaseScheduler(any(TotalScheduler.class));
		doCallRealMethod().when(ss).setCurrentScheduler(any(TotalScheduler.class));
		doCallRealMethod().when(ss).doSingleStep();
		TotalScheduler ts = mock(TotalScheduler.class);
		ScheduleAction scheduleAction = mock(ScheduleAction.class, Mockito.RETURNS_DEEP_STUBS);
		when(ts.choose(any(ScheduleOptions.class))).thenReturn(scheduleAction);
		TaskInfo taskInfo = mock(TaskInfo.class);
		when(ts.schedule(any(TaskScheduler.class), any(List.class))).thenReturn(taskInfo);
		ss.setBaseScheduler(ts);
		
		TaskStackFrameView tsfv = mock(TaskStackFrameView.class);
		StepOverScheduler sos = new StepOverScheduler(ss, tsfv);
		ss.setCurrentScheduler(sos);
		
		ScheduleOptions scheduleOptions = mock(ScheduleOptions.class, Mockito.RETURNS_DEEP_STUBS);
		ss.curScheduler.choose(scheduleOptions);
		verify(ts).reset();
		verify(ts).choose(any(ScheduleOptions.class));
		
		abs.backend.java.debugging.TaskInfo ti = mock(abs.backend.java.debugging.TaskInfo.class);
		TaskView tv = mock(TaskView.class);
		TaskStackView tsv = mock(TaskStackView.class);
		when(tsv.hasFrames()).thenReturn(true);
		
		TaskStackFrameView testStackFrame = mock(TaskStackFrameView.class);
		OngoingStubbing<TaskStackFrameView> ongoingStubbing = when(tsv.getCurrentFrame()).thenReturn(testStackFrame);
		for(int i=0; i<9; i++){
			testStackFrame = mock(TaskStackFrameView.class);
			ongoingStubbing = ongoingStubbing.thenReturn(testStackFrame);
		}
		ongoingStubbing.thenReturn(tsfv);
		
		
		when(tv.getStack()).thenReturn(tsv);
		when(ti.getTaskView()).thenReturn(tv);
		ss.steppedTask = ti;
		
		for(int i=0; i<10; i++){
			ss.curScheduler.choose(scheduleOptions);
		}
		assertEquals(sos, ss.curScheduler);
		verify(ss, never()).awaitGUIAction(any(ScheduleOptions.class));
		ss.curScheduler.choose(scheduleOptions);
		verify(ss).awaitGUIAction(any(ScheduleOptions.class));
	}

}
