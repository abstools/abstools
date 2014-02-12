package deadlock.analyser.detection;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.io.PrintStream;

import deadlock.analyser.factory.ContractElement;
import deadlock.analyser.factory.ContractElementAwait;
import deadlock.analyser.factory.ContractElementGet;
import deadlock.analyser.factory.ContractElementInvk;
import deadlock.analyser.factory.ContractElementInvkA;
import deadlock.analyser.factory.ContractElementInvkG;
import deadlock.analyser.factory.ContractElementSyncInvk;
import deadlock.analyser.factory.ContractElementUnion;
import deadlock.analyser.factory.Factory;
import deadlock.analyser.factory.GroupName;
import deadlock.analyser.factory.MethodContract;
import deadlock.analyser.factory.Contract;
import deadlock.analyser.factory.MethodInterface;
import deadlock.analyser.factory.Record;

import deadlock.constraints.term.Term;
import deadlock.constraints.term.TermStructured;
//import deadlock.constraints.term.Variable;
//import deadlock.constraints.term.TermVariable;


public class DASolver {

    Map<String, Term> methodMap;
    Factory df;

    Integer nOfIetation;

    Map<String, BigLamp> lampMap;

    Integer nOfDep;

    Boolean saturation;
    Boolean deadlock;
    Boolean possibleLock;
    Boolean cylceOfAwait;
    
    PrintStream out;

    public DASolver(Factory f, Map<String, Term> map, Integer i, PrintStream out){
        this.df = f;
        this.methodMap = map;
        this.nOfIetation = i;
        this.nOfDep = 0;

        this.saturation = false;
        this.deadlock = false;
        this.possibleLock = false;
        this.cylceOfAwait = false;
        
        this.out = out;

        this.lampMap = new HashMap<String, BigLamp>();

        for(String mName : methodMap.keySet()){
            this.lampMap.put(mName, new BigLamp(mName, methodMap.get(mName)));
        }
    }

    //this method compute a solution step without consider the await dependency
    public void computeSolution(){
        for(Integer i=0; i < this.nOfIetation; i++){
            for(String mName : methodMap.keySet()){
                Term contr = methodMap.get(mName);

                out.println("Iteration: " + i + " and method: " + mName);


                // I want to isolate the contract (body contract), only Main.main has already the right contract
                if(contr instanceof MethodContract){
                    contr = ((MethodContract) contr).getContract();
                }

                // In this first version of algorithm I want to work only with contract single (not contractSeq)
                // but inference return always a contractSeq, even if it is a single contract, so, I 'clear' it, if 
                // I have a contractSeq with only one subterm inside, it will be our contract to check
                if(((Contract) contr).getList().size() == 1){
                    contr = ((Contract) contr).getList().get(0);
                }

                Set<GroupName> bTilde = lampMap.get(mName).getbTilde();
                VarSubstitution subFresh = new VarSubstitution();
                for(GroupName v : bTilde) subFresh.addSub( v, df.newGroupName());

                //I create the substitution list and put this one calculated above



                if(contr instanceof ContractElementGet) {
                    DoubleLamp l = wGet(mName, (ContractElementGet) contr, subFresh, false);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                    lampMap.get(mName).setLastBFresh(subFresh);
                }else if(contr instanceof ContractElementAwait) {
                    DoubleLamp l = wAwait(mName, (ContractElementAwait) contr, subFresh, false);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                    lampMap.get(mName).setLastBFresh(subFresh);
                }else if(contr instanceof ContractElementInvk){
                    DoubleLamp l = wInvk(mName, (ContractElementInvk) contr, subFresh, false);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                    lampMap.get(mName).setLastBFresh(subFresh);
                }else if(contr instanceof ContractElementSyncInvk){
                    DoubleLamp l = wSyncInvk2(mName, (ContractElementSyncInvk) contr, subFresh, false);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                    lampMap.get(mName).setLastBFresh(subFresh);
                }else if(contr instanceof ContractElementInvkG){
                    DoubleLamp l = wGInvk(mName, (ContractElementInvkG) contr, subFresh, false);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                    lampMap.get(mName).setLastBFresh(subFresh);
                }else if(contr instanceof ContractElementInvkA){
                    DoubleLamp l = wAInvk(mName, (ContractElementInvkA) contr, subFresh, false);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                    lampMap.get(mName).setLastBFresh(subFresh);
                }else if(contr instanceof ContractElementUnion){ //FOR NOW I DO NOT IMPLEMENT RULE FOR ContractUnion
                    DoubleLamp l = wUnion(mName, (ContractElementUnion) contr, subFresh, false);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                    lampMap.get(mName).setLastBFresh(subFresh);
                }else /*if((((TermStructured) contr).getConstructor()).equals("ContractSeq"))*/{					
                    DoubleLamp l = wSeq(mName, (Contract) contr, subFresh, false);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                    lampMap.get(mName).setLastBFresh(subFresh);
                }
            }

            //now I check if I introduced new name/couple, if not, I stop the analysis
            Integer newNumberOfDep = 0;
            for(String mName : lampMap.keySet()){
                newNumberOfDep += lampMap.get(mName).getFirst().numberOfDep();
                newNumberOfDep += lampMap.get(mName).getSecond().numberOfDep();
            }

            //System.out.println("Iteration " + i + " number of dep at the end of this iteration is " + newNumberOfDep);

            //if I have cycle in main I stop the analysis
            if(lampMap.get("Main.main").getFirst().hasCycle() || lampMap.get("Main.main").getSecond().hasCycle()){
				moreInfoMainCycle();
				return;
	    }

            /*if(newNumberOfDep == this.nOfDep){
                lampMap.get("Main.main").getFirst().hasCycle();
                lampMap.get("Main.main").getSecond().hasCycle();
                LinkedList<ASTNode> nodeLocked = lampMap.get("Main.main").getFirst().hasCycle2();
                nodeLocked.addAll(lampMap.get("Main.main").getSecond().hasCycle2());
                System.out.println("###" + nodeLocked);
                if(nodeLocked != null && nodeLocked.size() > 0)
                    moreInfoMainCycle();
                return;
            }
            else this.nOfDep = newNumberOfDep;*/
           
            if(newNumberOfDep.equals(this.nOfDep)) return;
            else this.nOfDep = newNumberOfDep;
        }

        this.saturation = true;
        computeSolutionSatured();
        return;

    }


    //this method compute a solution step without consider the await dependency
    public void computeSolutionSatured(){
        out.println("SATURATION");
        Integer i = 0;
        while(true){
            for(String mName : methodMap.keySet()){
                Term contr = methodMap.get(mName);

                // I want to isolate the contract (body contract), only Main.main has already the right contract
                if(contr instanceof MethodContract){
                    contr = ((MethodContract) contr).getContract();
                }

                // In this first version of algorithm I want to work only with contract single (not contractSeq)
                // but inference return always a contractSeq, even if it is a single contract, so, I 'clear' it, if 
                // I have a contractSeq with only one subterm inside, it will be our contract to check
                if(((Contract) contr).getList().size() == 1){
                    contr = ((Contract) contr).getList().get(0);
                }

                VarSubstitution subFresh = lampMap.get(mName).getLastBFresh();


                if(contr instanceof ContractElementGet) {
                    DoubleLamp l = wGet(mName, (ContractElementGet) contr, subFresh, true);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                }else if(contr instanceof ContractElementAwait) {
                    DoubleLamp l = wAwait(mName, (ContractElementAwait) contr, subFresh, true);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                }else if(contr instanceof ContractElementInvk){ //this means that contr is a ContractInvk
                    DoubleLamp l = wInvk(mName, (ContractElementInvk) contr, subFresh, true);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                }else if(contr instanceof ContractElementSyncInvk){ //this means that contr is a ContractInvk
                    DoubleLamp l = wSyncInvk2(mName, (ContractElementSyncInvk) contr, subFresh, true);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                }else if(contr instanceof ContractElementInvkG){
                    DoubleLamp l = wGInvk(mName, (ContractElementInvkG) contr, subFresh, true);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                }else if(contr instanceof ContractElementInvkA){
                    DoubleLamp l = wAInvk(mName, (ContractElementInvkA) contr, subFresh, true);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                }else if(contr instanceof ContractElementUnion){
                    DoubleLamp l = wUnion(mName, (ContractElementUnion) contr, subFresh, true);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                }else /*if((((TermStructured) contr).getConstructor()).equals("ContractSeq"))*/{
                    //in a sequence the lastSub is not a single one but a new list
                    DoubleLamp l = wSeq(mName, (Contract) contr, subFresh, true);
                    lampMap.get(mName).setFirst(l.getW());
                    lampMap.get(mName).setSecond(l.getWPrime());
                }
            }

            //now I check if I introduced new name/couple, if not, I stop the analysis
            Integer newNumberOfDep = 0;
            for(String mName : lampMap.keySet()){
                newNumberOfDep += lampMap.get(mName).getFirst().numberOfDep();
                newNumberOfDep += lampMap.get(mName).getSecond().numberOfDep();
            }

            out.println("Iteration of saturation " + i + " number of dep at the end of this iteration is " + newNumberOfDep);
            i++;

            //if I have cycle in main I stop the analysis
            if(lampMap.get("Main.main").getFirst().hasCycle() || lampMap.get("Main.main").getSecond().hasCycle()){
				moreInfoMainCycle();
				return;
	    }

            /*if(newNumberOfDep.equals(this.nOfDep)){
                LinkedList<ASTNode> nodeLocked = lampMap.get("Main.main").getFirst().hasCycle2();
                nodeLocked.addAll(lampMap.get("Main.main").getSecond().hasCycle2());
                if(nodeLocked != null && nodeLocked.size() > 0)
                    moreInfoMainCycle();
                return;
            }
            else this.nOfDep = newNumberOfDep;*/

            if(newNumberOfDep.equals(this.nOfDep)) return;
            else this.nOfDep = newNumberOfDep;
        }		
    }




    // The rule W-Gzero of the Analysis
    public DoubleLamp wGet(String mName, ContractElementGet cGet, VarSubstitution bFresh, Boolean sat){
        // it will contains the result of the application of the rule
        DoubleLamp l = new DoubleLamp();		
        // here we extract the 2 variable from the contractGet and apply on them the fresh renaming (bTilde)
        GroupName a = cGet.whosWaiting();
        GroupName b = cGet.whosWaited();
        a = bFresh.apply(a);
        b = bFresh.apply(b);		
        // here we calculate the solution of the application of the rule
        Lamp w = new Lamp();
        Lamp wPrime = new Lamp();
        w.addCouple(a, b, cGet.getPosition());
        //I learn reading again the paper that only the first lamp obtain the couple
        //wPrime.addCouple(a, b);		
        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        return l;	
    }

    // The rule W-Azero of the Analysis
    public DoubleLamp wAwait(String mName, ContractElementAwait cAwait, VarSubstitution bFresh, Boolean sat){
        // it will contains the result of the application of the rule
        DoubleLamp l = new DoubleLamp();

        // here we extract the 2 variable from the contractGet and apply on them the fresh renaming (bTilde)

        /* THESE TWO ERRORS WILL BE FIXED WHEN WE PASS ON THE NEW DEFINITION OF CONTRACT get and await */
        GroupName a = cAwait.whosWaiting();
        GroupName b = cAwait.whosWaited();
        a = bFresh.apply(a);
        b = bFresh.apply(b);

        // here we calculate the solution of the application of the rule
        Lamp w = new Lamp();
        Lamp wPrime = new Lamp();
        w.addCoupleAwait(a, b, cAwait.getPosition());
        //I learn reading again the paper that only the first lamp obtain the couple
        //wPrime.addCouple(a, b);

        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        return l;	
    }

    // The rule W-Invk of the Analysis
    public DoubleLamp wInvk(String mName, ContractElementInvk cInvk, VarSubstitution bFresh, Boolean sat){
        // it will contains the result of the application of the rule
        DoubleLamp l = new DoubleLamp();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() + "." + cInvk.getMethodName();
        //System.out.println("method called is " + method.toString());

        //here we recover the bigLamp of the method called and also is methodContract
        BigLamp bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
        //if(bLamp != null) System.out.println("BigLamp of method called is: " + bLamp.toString());
        //if(methodContract != null) System.out.println("methodContract of method called is: " + methodContract.toString());
        //System.out.println("fresh renaming is: " + bFresh.toString());

        //The two Lamp of methodInvokation rule
        Lamp w = new Lamp();
        Lamp wfirstPrime = new Lamp(); //this is done to avoid side effect
        wfirstPrime.addLamp(bLamp.getFirst());
        Lamp wsecondPrime = new Lamp();
        wsecondPrime.addLamp(bLamp.getSecond());

        Lamp wPrime = new Lamp();
        wPrime.addLamp(wfirstPrime);
        wPrime.addLamp(wsecondPrime);

        if(!sat){
            //here we recover the formal parameter of the method invoked
            Set<GroupName> aTilde = bLamp.getaTilde();
            //System.out.println("aTilde of method called is: " + aTilde);

            //here we create the fresh substitution for NON formal parameter
            VarSubstitution subParam = new VarSubstitution();

            //here we recover ALL the free variable of the lamp of the method invoked and remove the variable of the formal parameters
            Set<GroupName> aPrimeTilde = bLamp.getFirst().fv();
            aPrimeTilde.addAll(bLamp.getSecond().fv());
            Set<GroupName> aTildeTermVar = new TreeSet<GroupName>();
            for(GroupName v : aTilde) aTildeTermVar.add(v);
            aPrimeTilde.removeAll(aTildeTermVar);
            //System.out.println("aPrimeTilde to substitute is :" + aPrimeTilde.toString() );

            for(GroupName v : aPrimeTilde) subParam.addSub(v, df.newGroupName());

            //I apply the first substitution, the once for formal parameter
            wPrime.apply(subParam);
        }



        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        Record thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        Record thisCalled = interfaceCalled.getThis();
        //System.out.println("thisCaller = " + thisCaller.toString());
        //System.out.println("thisCalled = " + thisCalled.toString());
        subThis = findSub(thisCaller, thisCalled, bFresh);
        //System.out.println("subThis = " + subThis.toString());
        wPrime.apply(subThis);

        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;

        List<Record> argsCaller = interfaceCaller.getParameters();

        List<Record> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            wPrime.apply(subArgs);
        }

        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        Record retCaller =  interfaceCaller.getResult();
        Record retCalled =  interfaceCalled.getResult();
        //System.out.println("retCaller = " + retCaller.toString());
        //System.out.println("retCalled = " + retCalled.toString());
        subRet = findSub(retCaller, retCalled, bFresh);
        //System.out.println("subRet = " + subRet.toString());
        wPrime.apply(subRet);

        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        return l;	
    }
    
    // The rule W-Invk of the Analysis
    public DoubleLamp wSyncInvk(String mName, ContractElementSyncInvk cInvk, VarSubstitution bFresh, Boolean sat){
        // it will contains the result of the application of the rule
        DoubleLamp l = new DoubleLamp();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() + "." + cInvk.getMethodName();
        //System.out.println("method called is " + method.toString());

        //here we recover the bigLamp of the method called and also is methodContract
        BigLamp bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
        //if(bLamp != null) System.out.println("BigLamp of method called is: " + bLamp.toString());
        //if(methodContract != null) System.out.println("methodContract of method called is: " + methodContract.toString());
        //System.out.println("fresh renaming is: " + bFresh.toString());

        //The two Lamp of methodInvokation rule
        Lamp w = new Lamp();
        Lamp wfirstPrime = new Lamp(); //this is done to avoid side effect
        wfirstPrime.addLamp(bLamp.getFirst());
        Lamp wsecondPrime = new Lamp();
        wsecondPrime.addLamp(bLamp.getSecond());

        Lamp wPrime = new Lamp();
        wPrime.addLamp(wfirstPrime);
        wPrime.addLamp(wsecondPrime);

        if(!sat){
            //here we recover the formal parameter of the method invoked
            Set<GroupName> aTilde = bLamp.getaTilde();
            //System.out.println("aTilde of method called is: " + aTilde);

            //here we create the fresh substitution for NON formal parameter
            VarSubstitution subParam = new VarSubstitution();

            //here we recover ALL the free variable of the lamp of the method invoked and remove the variable of the formal parameters
            Set<GroupName> aPrimeTilde = bLamp.getFirst().fv();
            aPrimeTilde.addAll(bLamp.getSecond().fv());
            Set<GroupName> aTildeTermVar = new TreeSet<GroupName>();
            for(GroupName v : aTilde) aTildeTermVar.add(v);
            aPrimeTilde.removeAll(aTildeTermVar);
            //System.out.println("aPrimeTilde to substitute is :" + aPrimeTilde.toString() );

            for(GroupName v : aPrimeTilde) subParam.addSub(v, df.newGroupName());

            //I apply the first substitution, the once for formal parameter
            wPrime.apply(subParam);
        }



        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        Record thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        Record thisCalled = interfaceCalled.getThis();
        //System.out.println("thisCaller = " + thisCaller.toString());
        //System.out.println("thisCalled = " + thisCalled.toString());
        subThis = findSub(thisCaller, thisCalled, bFresh);
        //System.out.println("subThis = " + subThis.toString());
        wPrime.apply(subThis);

        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;

        List<Record> argsCaller = interfaceCaller.getParameters();

        List<Record> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            wPrime.apply(subArgs);
        }

        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        Record retCaller =  interfaceCaller.getResult();
        Record retCalled =  interfaceCalled.getResult();
        //System.out.println("retCaller = " + retCaller.toString());
        //System.out.println("retCalled = " + retCalled.toString());
        subRet = findSub(retCaller, retCalled, bFresh);
        //System.out.println("subRet = " + subRet.toString());
        wPrime.apply(subRet);

        // we compose and return the solution <w,wPrime>
        l.setW(wPrime);
        l.setWPrime(w);
        return l;       
    }

    // The rule W-Invk of the Analysis
    public DoubleLamp wSyncInvk2(String mName, ContractElementSyncInvk cInvk, VarSubstitution bFresh, Boolean sat){
        // it will contains the result of the application of the rule
        DoubleLamp l = new DoubleLamp();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() + "." + cInvk.getMethodName();
        //System.out.println("method called is " + method.toString());

        //here we recover the bigLamp of the method called and also is methodContract
        BigLamp bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
        //if(bLamp != null) System.out.println("BigLamp of method called is: " + bLamp.toString());
        //if(methodContract != null) System.out.println("methodContract of method called is: " + methodContract.toString());
        //System.out.println("fresh renaming is: " + bFresh.toString());

        //The two Lamp of methodInvokation rule
        Lamp w = new Lamp();
        Lamp wPrime = new Lamp(); //this is done to avoid side effect
        w.addLamp(bLamp.getFirst());
        wPrime.addLamp(bLamp.getSecond());


        if(!sat){
            //here we recover the formal parameter of the method invoked
            Set<GroupName> aTilde = bLamp.getaTilde();
            //System.out.println("aTilde of method called is: " + aTilde);

            //here we create the fresh substitution for NON formal parameter
            VarSubstitution subParam = new VarSubstitution();

            //here we recover ALL the free variable of the lamp of the method invoked and remove the variable of the formal parameters
            Set<GroupName> aPrimeTilde = bLamp.getFirst().fv();
            aPrimeTilde.addAll(bLamp.getSecond().fv());
            Set<GroupName> aTildeTermVar = new TreeSet<GroupName>();
            for(GroupName v : aTilde) aTildeTermVar.add(v);
            aPrimeTilde.removeAll(aTildeTermVar);
            //System.out.println("aPrimeTilde to substitute is :" + aPrimeTilde.toString() );

            for(GroupName v : aPrimeTilde) subParam.addSub(v, df.newGroupName());

            //I apply the first substitution, the once for formal parameter
            w.apply(subParam);
            wPrime.apply(subParam);
        }



        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        Record thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        Record thisCalled = interfaceCalled.getThis();
        //System.out.println("thisCaller = " + thisCaller.toString());
        //System.out.println("thisCalled = " + thisCalled.toString());
        subThis = findSub(thisCaller, thisCalled, bFresh);
        //System.out.println("subThis = " + subThis.toString());
        w.apply(subThis);
        wPrime.apply(subThis);

        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;

        List<Record> argsCaller = interfaceCaller.getParameters();

        List<Record> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            w.apply(subArgs);
            wPrime.apply(subArgs);
        }

        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        Record retCaller =  interfaceCaller.getResult();
        Record retCalled =  interfaceCalled.getResult();
        //System.out.println("retCaller = " + retCaller.toString());
        //System.out.println("retCalled = " + retCalled.toString());
        subRet = findSub(retCaller, retCalled, bFresh);
        //System.out.println("subRet = " + subRet.toString());
        w.apply(subRet);
        wPrime.apply(subRet);

        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        return l;       
    }
    
    // The rule W-GInvk of the Analysis
    public DoubleLamp wGInvk(String mName, ContractElementInvkG cGInvk, VarSubstitution bFresh, Boolean sat){
        // it will contains the result of the application of the rule
        DoubleLamp l = new DoubleLamp();

        // here I split cGInvk between the invocation cInvk and the get cGet
        ContractElementInvk cInvk = cGInvk.getInvk();
        ContractElementGet cGet = cGInvk.getGet();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() +"."+ cInvk.getMethodName();
        //System.out.println("method called is " + method.toString());

        //here we recover the bigLamp of the method called and also is methodContract
        BigLamp bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
        //if(bLamp != null) System.out.println("BigLamp of method called is: " + bLamp.toString());
        //if(methodContract != null) System.out.println("methodContract of method called is: " + methodContract.toString());
        //System.out.println("fresh renaming is: " + bFresh.toString());

        Lamp w = new Lamp(); //this is done to avoid side effect
        w.addLamp(bLamp.getFirst());
        Lamp wPrime = new Lamp();
        wPrime.addLamp(bLamp.getSecond());

        if(!sat){
            //here we recover the formal parameter of the method invoked
            Set<GroupName> aTilde = bLamp.getaTilde();
            //System.out.println("aTilde of method called is: " + aTilde);


            //here we create the fresh substitution for NON formal parameter
            VarSubstitution subParam = new VarSubstitution();

            //here we recover ALL the free variable of the lamp of the method invoked and remove the variable of the formal parameters
            Set<GroupName> aPrimeTilde = bLamp.getFirst().fv();
            aPrimeTilde.addAll(bLamp.getSecond().fv());
            Set<GroupName> aTildeTermVar = new TreeSet<GroupName>();
            for(GroupName v : aTilde) aTildeTermVar.add(v);
            aPrimeTilde.removeAll(aTildeTermVar);

            //System.out.println("aPrimeTilde to substitute is :" + aPrimeTilde.toString() );

            for(GroupName v : aPrimeTilde) subParam.addSub(v, df.newGroupName());
            //The two Lamp of methodInvokation rule

            //I apply the first substitution, the once for formal parameter
            w.apply(subParam);
            wPrime.apply(subParam);
        }







        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        Record thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        Record thisCalled = interfaceCalled.getThis();
        //System.out.println("thisCaller = " + thisCaller.toString());
        //System.out.println("thisCalled = " + thisCalled.toString());
        subThis = findSub(thisCaller, thisCalled, bFresh);
        //System.out.println("subThis = " + subThis.toString());
        w.apply(subThis);
        wPrime.apply(subThis);


        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;
        List<Record> argsCaller = interfaceCaller.getParameters();

        List<Record> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            w.apply(subArgs);
            wPrime.apply(subArgs);
        }


        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        Record retCaller =  interfaceCaller.getResult();
        Record retCalled =  interfaceCalled.getResult();
        //System.out.println("retCaller = " + retCaller.toString());
        //System.out.println("retCalled = " + retCalled.toString());
        subRet = findSub(retCaller, retCalled, bFresh);
        //System.out.println("subRet = " + subRet.toString());
        w.apply(subRet);
        wPrime.apply(subRet);


        // here we extract the 2 variable from the contractGet and apply on them the fresh renaming (bTilde)
        GroupName a = cGet.whosWaiting();
        GroupName b = cGet.whosWaited();
        //System.out.println("getVar are = " + a.toString() + " and " + b.toString());
        a = bFresh.apply(a);
        b = bFresh.apply(b);

        //System.out.println("getVar after Sub = " + a.toString() + " and " + b.toString());

        // we add the get Pair of names at the two lamps
        w.addCouple(a, b, cGInvk.getPosition());
        //same comment of the rule w-Gzero
        //wPrime.addCouple(a, b);


        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        return l;	
    }

    // The rule W-AInvk of the Analysis
    public DoubleLamp wAInvk(String mName, ContractElementInvkA cAInvk, VarSubstitution bFresh, Boolean sat){
        // it will contains the result of the application of the rule
        DoubleLamp l = new DoubleLamp();

        // here I split cGInvk between the invocation cInvk and the get cGet
        ContractElementInvk cInvk = cAInvk.getInvk();
        ContractElementAwait cAwait = cAInvk.getAwait();

        // here I recover and clear the method called, after that I have a string that identify it
        String method = cInvk.getClassName() +"."+ cInvk.getMethodName();
        //System.out.println("method called is " + method.toString());

        //here we recover the bigLamp of the method called and also is methodContract
        BigLamp bLamp = lampMap.get(method);
        MethodContract methodContract = (MethodContract) methodMap.get(method);
        //if(bLamp != null) System.out.println("BigLamp of method called is: " + bLamp.toString());
        //if(methodContract != null) System.out.println("methodContract of method called is: " + methodContract.toString());
        //System.out.println("fresh renaming is: " + bFresh.toString());

        //The two Lamp of methodInvokation rule
        Lamp w = new Lamp(); //this is done to avoid side effect
        w.addLamp(bLamp.getFirst());
        Lamp wPrime = new Lamp();
        wPrime.addLamp(bLamp.getSecond());

        if(!sat){
            //here we recover the formal parameter of the method invoked
            Set<GroupName> aTilde = bLamp.getaTilde();
            //System.out.println("aTilde of method called is: " + aTilde);

            //here we create the fresh substitution for NON formal parameter
            VarSubstitution subParam = new VarSubstitution();

            //here we recover ALL the free variable of the lamp of the method invoked and remove the variable of the formal parameters
            Set<GroupName> aPrimeTilde = bLamp.getFirst().fv();
            aPrimeTilde.addAll(bLamp.getSecond().fv());
            Set<GroupName> aTildeTermVar = new TreeSet<GroupName>();
            for(GroupName v : aTilde) aTildeTermVar.add(v);
            aPrimeTilde.removeAll(aTildeTermVar);

            //System.out.println("aPrimeTilde to substitute is :" + aPrimeTilde.toString() );

            for(GroupName v : aPrimeTilde) subParam.addSub(v, df.newGroupName());
            //I apply the first substitution, the once for formal parameter
            w.apply(subParam);
            wPrime.apply(subParam);
        }



        //here we create and apply the second substitution, 'thisRecord' of method called got to be replaced with
        //'thisRecord' of the call inside the contract that we are analyzing
        VarSubstitution subThis;
        MethodInterface interfaceCaller = cInvk.getMethodInterface();
        Record thisCaller = interfaceCaller.getThis();
        MethodInterface interfaceCalled = methodContract.getMethodInterface();
        Record thisCalled = interfaceCalled.getThis();
        //System.out.println("thisCaller = " + thisCaller.toString());
        //System.out.println("thisCalled = " + thisCalled.toString());
        subThis = findSub(thisCaller, thisCalled, bFresh);
        //System.out.println("subThis = " + subThis.toString());
        w.apply(subThis);
        wPrime.apply(subThis);

        //here we create and apply the third substitution, 'argsRecord' of method called got to be replaced with
        //'argsRecord' of the call inside the contract that we are analyzing
        VarSubstitution subArgs;
        List<Record> argsCaller = interfaceCaller.getParameters();

        List<Record> argsCalled = interfaceCalled.getParameters();

        for(Integer i = 0 ; i<argsCaller.size() ; i++){
            subArgs = findSub(argsCaller.get(i), argsCalled.get(i), bFresh);
            w.apply(subArgs);
            wPrime.apply(subArgs);
        }

        //here we create and apply the third substitution, 'retRecord' of method called got to be replaced with
        //'retRecord' of the call inside the contract that we are analyzing
        VarSubstitution subRet;
        Record retCaller =  interfaceCaller.getResult();
        Record retCalled =  interfaceCalled.getResult();
        //System.out.println("retCaller = " + retCaller.toString());
        //System.out.println("retCalled = " + retCalled.toString());
        subRet = findSub(retCaller, retCalled, bFresh);
        //System.out.println("subRet = " + subRet.toString());
        w.apply(subRet);
        wPrime.apply(subRet);

        // here we extract the 2 variable from the contractGet and apply on them the fresh renaming (bTilde)
        GroupName a = cAwait.whosWaiting();
        GroupName b = cAwait.whosWaited();
        //System.out.println("awaitVar are = " + a.toString() + " and " + b.toString());
        a = bFresh.apply(a);
        b = bFresh.apply(b);
        //System.out.println("awaitVar after Sub = " + a.toString() + " and " + b.toString());

        // we add the get Pair of names at the two lamps
        w.addCoupleAwait(a, b, cAInvk.getPosition());
        //same comment of the rule w-Gzero
        //wPrime.addCouple(a, b);

        // we compose and return the solution <w,wPrime>
        l.setW(w);
        l.setWPrime(wPrime);
        return l;	
    }

    // The rule W-Union of the Analysis
    public DoubleLamp wUnion(String mName, ContractElementUnion contr, VarSubstitution bFresh, Boolean sat){

        Contract c1 = contr.getBranchOne();
        Contract c2 = contr.getBranchTwo();

        DoubleLamp l1 = wSeq(mName, (Contract) c1, bFresh, sat);
        DoubleLamp l2 = wSeq(mName, (Contract) c2, bFresh, sat);

        DoubleLamp l = new DoubleLamp();
        l.Union(l1, l2);

        return l;       
    }       


    // The rule W-Seq of the Analysis
    public DoubleLamp wSeq(String mName, Contract contr, VarSubstitution bFresh, Boolean sat){
        List<ContractElement> contracts = ((Contract) contr).getList();
        DoubleLamp l = new DoubleLamp();
        
        for(ContractElement c : contracts){
            if(c instanceof ContractElementGet) {
                DoubleLamp lr = wGet(mName, (ContractElementGet) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementAwait) {
                DoubleLamp lr = wAwait(mName, (ContractElementAwait) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementSyncInvk){ //this means that contr is a ContractSyncInvk
                DoubleLamp lr = wSyncInvk(mName, (ContractElementSyncInvk) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvk){ //this means that contr is a ContractInvk
                DoubleLamp lr = wInvk(mName, (ContractElementInvk) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvkG){
                DoubleLamp lr = wGInvk(mName, (ContractElementInvkG) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvkA){
                DoubleLamp lr = wAInvk(mName, (ContractElementInvkA) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementUnion){
                DoubleLamp lr = wUnion(mName, (ContractElementUnion) c, bFresh, sat);
                l.seqComposition(lr);   
            }
        }
        return l;       
    }       

    // The rule W-SeqInUnion of the Analysis
    public DoubleLamp wSeqInUnion(String mName, Contract contr, VarSubstitution bFresh, Boolean sat){
        List<ContractElement> contracts = ((Contract) contr).getList();
        DoubleLamp l = new DoubleLamp();
        for(ContractElement c : contracts){
            if(c instanceof ContractElementGet) {
                DoubleLamp lr = wGet(mName, (ContractElementGet) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementAwait) {
                DoubleLamp lr = wAwait(mName, (ContractElementAwait) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvk){ //this means that contr is a ContractInvk
                DoubleLamp lr = wInvk(mName, (ContractElementInvk) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvkG){
                DoubleLamp lr = wGInvk(mName, (ContractElementInvkG) c, bFresh, sat);
                l.seqComposition(lr);   
            }else if(c instanceof ContractElementInvkA){
                DoubleLamp lr = wAInvk(mName, (ContractElementInvkA) c, bFresh, sat);
                l.seqComposition(lr);   
            }
        }
        return l;       
    }       

    //final info like saturation and various lock

    public Boolean isSatured(){
        return this.saturation;
    }

    public void moreInfoMainCycle(){
        if(lampMap.get("Main.main").getFirst().hasCycleGet() || lampMap.get("Main.main").getSecond().hasCycleGet()){
            this.deadlock = true;
            return;
        }
        if(lampMap.get("Main.main").getFirst().hasCycleAwait() || lampMap.get("Main.main").getSecond().hasCycleAwait()){
            this.cylceOfAwait = true;
            return;
        }

        this.possibleLock = true;
    }

    public Boolean isCycleMain(){
        return this.possibleLock;
    }

    public Boolean isDeadlockMain(){
        return this.deadlock;
    }

    public Boolean isAwaitLoopMain(){
        return this.cylceOfAwait;
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

    public String toString(){
        String res = "";
        for(String name : this.lampMap.keySet()){
            res += (name + " = \n" + lampMap.get(name).toString() + "\n");
        }
        return res;
    }

}
