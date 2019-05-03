---
title: "Deadlock Analysis with DSA"
description: "The Deadlock Static Analyser (DSA) performs static and sound deadlock analysis of ABS programs."
date: 2019-03-31
showDate: false
type: post
draft: true
---

## General Overview

The Deadlock Static Analyser (DSA) performs static and sound deadlock analysis
of ABS programs described in the papers http://dx.doi.org/10.1007/s10270-014-0444-y and http://dx.doi.org/10.1007/978-3-662-44584-6_6 .  Deadlocks may be particularly hard to
detect in systems with unbounded recursion and dynamic resource creation, such
as server applications, which creates an unbounded number of processes.  In
these systems, the interaction protocols are extremely complex and
state-of-the-art solutions either give imprecise answers or do not scale.

In order to augment precision and scalability we propose a modular framework
combining several techniques.  We meet the scalability requirement by
designing a front-end inference system that automatically extracts abstract
behavioral descriptions pertinent to deadlock analysis, called *behavioural
types*, from code.  The inference system is *modular* because it (partially)
supports separate inference of modules.  The inference system mostly collects
method behaviors and uses constraints to enforce consistencies among
behaviors.  Then a standard semiunification technique is used for solving the
set of generated constraints.

Our behavioural types feature recursion and resource creation; therefore their
underlying models are infinite state.  Our tool is particularly precise with
this kind of programs, because it implements a fixpoint algorithm, which is
proven to be a decision algorithm on the model.

This tutorial will show how to use the DSA features and the kind of deadlock
it is able to detect.


## Deadlock Analysis

In this section we present how to check wether an ABS program is
deadlock-free.

First, select `Deadlock Analysis (DSA)` from the pull-down menu at the top of
the window on the center-left.  The parameters of the selected analysis are
automatically set, so there is nothing to be configured in the `Settings`
section in the top-left corner.

As an example, open the program {{< eifilelink app="dsa"
path="/collaboratory/examples/Deadlock/BOL/uglyChain.abs" >}}:

```abs
module UglyChain;

interface Object {
}

class Object implements Object {
}

interface C {
    Unit m(C c);
    Unit n(C a) ;
    Unit q() ;
}

class C implements C {
    Unit m(C c){
        C w = new C() ;
        w!m(this) ;
        c!n(this) ; 
    }
    Unit n(C a){  	
        Fut<Unit> x = a!q() ;
        x.get ;
    }
    Unit q(){
    }

}


{
    C a = new C() ;
    C b = new C() ;
    Fut<Unit> x = a!m(b) ;
}
```

Let us analyze the program. Click on `Run` to perform the analysis.

The output of the analysis is shown in the console:
```
Possibile Deadlock in main:  false
```
meaning that the program is deadlock-free.

Additional information are also given, such as the current release of the tool
and the time for performing the analysis.

Consider, now, the example {{< eifilelink app="dsa" path="/collaboratory/examples/Deadlock/BOL/SchedulerChoice.abs" >}}:

```abs
module Test_Inference_d;

interface Object {
}

class Object implements Object {
}

interface C {
   C m();
   C n(C c);
}

class C implements C {
   C m(){
	C x = new C();
	return x;
   }
   C n(C c){
	Fut<C> fut = c!m();
	return fut.get;
   }
}


{
C i = new C();
C j = new C();
Fut<C> fut4 = i!n(j);
Fut<C> fut5 = j!n(i);

}
```

This is an example of possible run-time deadlock due to specific scheduler’s
choices.

Click on the `Clear` button for removing previous results.  Then click the
`Run` button for executing the analysis.

In this case, a possibile deadlock is detected:
```
Possibile Deadlock in main:  true
```
