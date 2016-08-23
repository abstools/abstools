/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package deadlock.analyser.detection;

import java.io.PrintStream;
import java.util.List;
import java.util.Map;
import java.util.Set;

import deadlock.analyser.factory.Contract;
import deadlock.analyser.factory.ContractElement;
import deadlock.analyser.factory.ContractElementAwait;
import deadlock.analyser.factory.ContractElementGet;
import deadlock.analyser.factory.ContractElementInvk;
import deadlock.analyser.factory.ContractElementInvkA;
import deadlock.analyser.factory.ContractElementInvkG;
import deadlock.analyser.factory.ContractElementParallel;
import deadlock.analyser.factory.ContractElementSyncInvk;
import deadlock.analyser.factory.ContractElementUnion;
import deadlock.analyser.factory.Factory;
import deadlock.analyser.factory.GroupName;
import deadlock.analyser.factory.MainMethodContract;
import deadlock.analyser.factory.MethodContract;
import deadlock.analyser.factory.MethodInterface;
import deadlock.analyser.factory.IRecord;
import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;

public class FixPointSolver2 extends DASolver {
    
    public FixPointSolver2(Factory f, Map<String, MethodContract> map, MainMethodContract mmc) {
        super(f, map, mmc);
        // TODO Auto-generated constructor stub
    }

   
    @Override
    public String getName() {return "Fix Point 2.0";}
    
    //this method performs the deadlock analysis
    @Override
    public void computeSolution(){
        Integer prevNumberOfDep = -1;
        
        //perform an infinite cycle, since the lams state domain is a finite lattice a fix point will be found
        while(true){
            
            //perform one step expansion for each method
            for(String mName : methodMap.keySet()){
              
                //Fresh Names for function body
                Set<GroupName> bTilde = lampMap.get(mName).getbTilde();
                VarSubstitution subFresh = new VarSubstitution();
                for(GroupName v : bTilde) subFresh.addSub( v, df.newGroupName(true));
                
                DoubleLam expansionPresent = wSeq(mName, methodMap.get(mName).getContractPresent(), subFresh);
                DoubleLam expansionFuture = wSeq(mName, methodMap.get(mName).getContractFuture(), subFresh);
                
                expansionPresent.seqComposition(expansionFuture);
                
                //Update the lam with the expansion result
                BigLam lam = lampMap.get(mName);
                
                lam.setFirst(expansionPresent.getW());
                lam.setSecond(expansionPresent.getWPrime());
                
                lam.expandAndClean();
//                lam.getFirst().updateStackTrace(mName);
//                lam.getSecond().updateStackTrace(mName);
            }
            
            //if the number of dependencies has not change in this iteration then a fix point is found
            //check if the number of dependencies is different of previous, if not, stop the analysis
            int newNumberOfDep = 0;
            for(String mName : lampMap.keySet()){
                newNumberOfDep += lampMap.get(mName).getFirst().numberOfDep();
                newNumberOfDep += lampMap.get(mName).getSecond().numberOfDep();
            }
            
            if(newNumberOfDep == prevNumberOfDep) break;
            else prevNumberOfDep = newNumberOfDep;
        }
        
        //BigLam blMain = lampMap.get("Main.main");
        BigLam blMain = new BigLam("Main.main", mmc);
        
        DoubleLam expansionPresent = wSeq(null, mmc.getContractPresent(), new VarSubstitution());
        DoubleLam expansionFuture = wSeq(null, mmc.getContractFuture(), new VarSubstitution());
        
        expansionPresent.seqComposition(expansionFuture);
        
        blMain.setFirst(expansionPresent.getW());
        blMain.setSecond(expansionPresent.getWPrime());
        
        blMain.expandAndClean();
        
        //if the cycle found is no a livelock then this lock is indeed a deadlock
        this.deadlock = blMain.hasReflexiveState();
        if(this.deadlock){
            this.deadlockStates = blMain.getReflexiveStates();
        }
        //is it possible for a program to have more than one potential deadlocks and 
        //livelocks at the same time but the algorithm stop at the first cycle so
        //only one cyclic dependency is reported
        return;
        
        //fix point reached, analysis complete
    }
    // The rule W-Gzero of the Analysis
    private DoubleLam wGet(String mName, ContractElementGet cGet, VarSubstitution bFresh){
        
        // it will contains the result of the application of the rule
        DoubleLam l = new DoubleLam();                
        // here we extract the 2 variable from the contractGet and apply on them the fresh renaming (bTilde)
        GroupName a = cGet.whosWaiting();
        GroupName b = cGet.whosWaited();
        a = bFresh.apply(a);
        b = bFresh.apply(b);            
        
        // here we calculate the solution of the application of the rule
        Lam w = new Lam();
        Lam wPrime = new Lam();
        w.addCouple(a, b);
       
        // compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        return l;       
    }

    // The rule W-Azero of the Analysis
    private DoubleLam wAwait(String mName, ContractElementAwait cAwait, VarSubstitution bFresh){
        // it will contains the result of the application of the rule
        DoubleLam l = new DoubleLam();

        // here we extract the 2 variable from the contractGet and apply on them the fresh renaming (bTilde)
        GroupName a = cAwait.whosWaiting();
        GroupName b = cAwait.whosWaited();
        a = bFresh.apply(a);
        b = bFresh.apply(b);

        // here we calculate the solution of the application of the rule
        Lam w = new Lam();
        Lam wPrime = new Lam();
        w.addCoupleAwait(a, b);
        
        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        return l;       
    }

    // The rule W-Invk of the Analysis
    private DoubleLam wInvk(String mName, ContractElementInvk cInvk, VarSubstitution bFresh){
        // it will contains the result of the application of the rule
        DoubleLam l = new DoubleLam();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() + "." + cInvk.getMethodName();

        //here we recover the bigLamp of the method called and also is methodContract
        BigLam bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
     
        //The two Lamp of methodInvokation rule
        Lam w = new Lam();
        Lam wfirstPrime = new Lam(); //this is done to avoid side effect
        wfirstPrime.addLamp(bLamp.getFirst());
        Lam wsecondPrime = new Lam();
        wsecondPrime.addLamp(bLamp.getSecond());

        Lam wPrime = new Lam();
        wPrime.addLamp(wfirstPrime);
        wPrime.addLamp(wsecondPrime);


        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        IRecord thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        IRecord thisCalled = interfaceCalled.getThis();
      
        subThis = findSub(thisCaller, thisCalled, bFresh);
        
        wPrime.apply(subThis);

        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;

        List<IRecord> argsCaller = interfaceCaller.getParameters();

        List<IRecord> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            wPrime.apply(subArgs);
        }

        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        IRecord retCaller =  interfaceCaller.getResult();
        IRecord retCalled =  interfaceCalled.getResult();
       
        subRet = findSub(retCaller, retCalled, bFresh);
        
        wPrime.apply(subRet);

        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        l.updateStackTrace(method);
        return l;       
    }
    
    // The rule W-Invk of the Analysis
    private DoubleLam wSyncInvk(String mName, ContractElementSyncInvk cInvk, VarSubstitution bFresh){
        // it will contains the result of the application of the rule
        DoubleLam l = new DoubleLam();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() + "." + cInvk.getMethodName();

        //here we recover the bigLamp of the method called and also is methodContract
        BigLam bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
     
        //The two Lamp of methodInvokation rule
        Lam w = new Lam();
        Lam wPrime = new Lam(); //this is done to avoid side effect
        w.addLamp(bLamp.getFirst());
        wPrime.addLamp(bLamp.getSecond());

        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        IRecord thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        IRecord thisCalled = interfaceCalled.getThis();
      
        subThis = findSub(thisCaller, thisCalled, bFresh);
       
        w.apply(subThis);
        wPrime.apply(subThis);

        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;

        List<IRecord> argsCaller = interfaceCaller.getParameters();

        List<IRecord> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            w.apply(subArgs);
            wPrime.apply(subArgs);
        }

        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        IRecord retCaller =  interfaceCaller.getResult();
        IRecord retCalled =  interfaceCalled.getResult();
      
        subRet = findSub(retCaller, retCalled, bFresh);
        
        w.apply(subRet);
        wPrime.apply(subRet);

        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        l.updateStackTrace(method);
        return l;       
    }
    
    // The rule W-GInvk of the Analysis
    private DoubleLam wGInvk(String mName, ContractElementInvkG cGInvk, VarSubstitution bFresh){
        // it will contains the result of the application of the rule
        DoubleLam l = new DoubleLam();

        // here I split cGInvk between the invocation cInvk and the get cGet
        ContractElementInvk cInvk = cGInvk.getInvk();
        ContractElementGet cGet = cGInvk.getGet();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() +"."+ cInvk.getMethodName();
       

        //here we recover the bigLamp of the method called and also is methodContract
        BigLam bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
     
        Lam w = new Lam(); //this is done to avoid side effect
        w.addLamp(bLamp.getFirst());
        Lam wPrime = new Lam();
        wPrime.addLamp(bLamp.getSecond());

        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        IRecord thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        IRecord thisCalled = interfaceCalled.getThis();
      
        subThis = findSub(thisCaller, thisCalled, bFresh);

        w.apply(subThis);
        wPrime.apply(subThis);

        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;
        List<IRecord> argsCaller = interfaceCaller.getParameters();

        List<IRecord> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            w.apply(subArgs);
            wPrime.apply(subArgs);
        }


        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        IRecord retCaller =  interfaceCaller.getResult();
        IRecord retCalled =  interfaceCalled.getResult();
       
        subRet = findSub(retCaller, retCalled, bFresh);
      
        w.apply(subRet);
        wPrime.apply(subRet);


        // here we extract the 2 variable from the contractGet and apply on them the fresh renaming (bTilde)
        GroupName a = cGet.whosWaiting();
        GroupName b = cGet.whosWaited();
   
        a = bFresh.apply(a);
        b = bFresh.apply(b);

        // we add the get Pair of names at the two lamps
        w.addCouple(a, b);
        
       // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        l.updateStackTrace(method);
        return l;       
    }

    // The rule W-AInvk of the Analysis
    private DoubleLam wAInvk(String mName, ContractElementInvkA cAInvk, VarSubstitution bFresh){
        // it will contains the result of the application of the rule
        DoubleLam l = new DoubleLam();

        // here I split cGInvk between the invocation cInvk and the get cGet
        ContractElementInvk cInvk = cAInvk.getInvk();
        ContractElementAwait cAwait = cAInvk.getAwait();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() +"."+ cInvk.getMethodName();
        //System.out.println("method called is " + method.toString());

        //here we recover the bigLamp of the method called and also is methodContract
        BigLam bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
  
        //The two Lamp of methodInvokation rule
        Lam w = new Lam(); //this is done to avoid side effect
        w.addLamp(bLamp.getFirst());
        Lam wPrime = new Lam();
        wPrime.addLamp(bLamp.getSecond());

        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        IRecord thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        IRecord thisCalled = interfaceCalled.getThis();
        //System.out.println("thisCaller = " + thisCaller.toString());
        //System.out.println("thisCalled = " + thisCalled.toString());
        subThis = findSub(thisCaller, thisCalled, bFresh);
        //System.out.println("subThis = " + subThis.toString());
        w.apply(subThis);
        wPrime.apply(subThis);

        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;
        List<IRecord> argsCaller = interfaceCaller.getParameters();

        List<IRecord> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            w.apply(subArgs);
            wPrime.apply(subArgs);
        }

        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        IRecord retCaller =  interfaceCaller.getResult();
        IRecord retCalled =  interfaceCalled.getResult();
      
        subRet = findSub(retCaller, retCalled, bFresh);
       
        w.apply(subRet);
        wPrime.apply(subRet);

        // here we extract the 2 variable from the contractGet and apply on them the fresh renaming (bTilde)
        GroupName a = cAwait.whosWaiting();
        GroupName b = cAwait.whosWaited();
       
        a = bFresh.apply(a);
        b = bFresh.apply(b);
     
        // we add the get Pair of names at the two lamps
        w.addCoupleAwait(a, b);
        
        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        l.updateStackTrace(method);
        return l;       
    }

    // The rule W-Union of the Analysis
    private DoubleLam wUnion(String mName, ContractElementUnion contr, VarSubstitution bFresh){

        Contract c1 = contr.getBranchOne();
        Contract c2 = contr.getBranchTwo();

        DoubleLam l1 = wSeq(mName, (Contract) c1, bFresh);
        DoubleLam l2 = wSeq(mName, (Contract) c2, bFresh);

        DoubleLam l = new DoubleLam();
        l.union(l1, l2);

        return l;       
    }       
    
    // The rule W-Par of the Analysis
    private DoubleLam wParallel(String mName, ContractElementParallel contr, VarSubstitution bFresh){
        
        List<Contract> contracts = contr.getContracts();
                
        DoubleLam l = new DoubleLam();
        for(Contract c : contracts){
            DoubleLam l1 = wSeq(mName, c, bFresh);
            DoubleLam res = new DoubleLam();
            res.parallel(l, l1);
            
            l = res;
        }
        
        return l;       
    }       


    // The rule W-Seq of the Analysis
    public DoubleLam wSeq(String mName, Contract contr, VarSubstitution bFresh){
        List<ContractElement> contracts = ((Contract) contr).getList();
        DoubleLam l = new DoubleLam();
        
        for(ContractElement c : contracts){
            if(c instanceof ContractElementGet) {
                DoubleLam lr = wGet(mName, (ContractElementGet) c, bFresh);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementAwait) {
                DoubleLam lr = wAwait(mName, (ContractElementAwait) c, bFresh);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementSyncInvk){ //this means that contr is a ContractSyncInvk
                DoubleLam lr = wSyncInvk(mName, (ContractElementSyncInvk) c, bFresh);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvk){ //this means that contr is a ContractInvk
                DoubleLam lr = wInvk(mName, (ContractElementInvk) c, bFresh);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvkG){
                DoubleLam lr = wGInvk(mName, (ContractElementInvkG) c, bFresh);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvkA){
                DoubleLam lr = wAInvk(mName, (ContractElementInvkA) c, bFresh);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementUnion){
                DoubleLam lr = wUnion(mName, (ContractElementUnion) c, bFresh);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementParallel){
                DoubleLam lr = wParallel(mName, (ContractElementParallel) c, bFresh);
                l.seqComposition(lr);   
            }
        }
        return l;       
    }       
    
    public VarSubstitution findSub(Term t1, Term t2, VarSubstitution preSub){
        VarSubstitution sub = new VarSubstitution();
        if(t1 instanceof GroupName && t2 instanceof GroupName){
            GroupName v = preSub.apply((GroupName) t1);
            sub.addSub((GroupName) t2, v);
            return sub;
        }

        if(t1 instanceof TermStructured && t2 instanceof TermStructured){
            List<Term> lt1 = ((TermStructured) t1).getSubTerms();
            List<Term> lt2 = ((TermStructured) t2).getSubTerms();
            VarSubstitution subsub = new VarSubstitution();
            for(Integer i = 0 ; i<lt1.size() ; i++){
                subsub = findSub(lt1.get(i), lt2.get(i), preSub);
                sub.addSub(subsub);
            }
        }

        return sub;     
    }


    @Override
    public void printDeadlockDetails(PrintStream out) {
        int indent = 2;
        out.println(printTab(indent*2)+"The following trace(s) could potentially reach a deadlock state:");
        indent++;
        
        for(State s : this.deadlockStates){
            int currentIndent = indent;
            out.println(printTab(currentIndent*2)+">>Main");
            for(String call : s.di.callStack){
                currentIndent++;
                out.println(printTab(currentIndent*2)+">>"+call);
            }
            out.println();
        }
    }
    
    private String printTab(int n) {
        StringBuffer buffer = new StringBuffer (); 
        for (int i = 0; i < n; i++) {
            buffer.append(" ");
        }
        return buffer.toString();
    }

  
}
