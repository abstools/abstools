package deadlock.analyser.detection;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.stringtemplate.v4.misc.STNoSuchAttributeException;

import deadlock.analyser.factory.Contract;
import deadlock.analyser.factory.GroupName;
import deadlock.analyser.factory.MainMethodContract;
import deadlock.analyser.factory.MethodContract;
import deadlock.analyser.factory.MethodInterface;
import deadlock.analyser.factory.IRecord;
import com.gzoumix.semisolver.term.Term;


// a BigLam is a quadruple <methodName, methodContract, Lam w, Lam wPrime>
// and it contains the set of variable bTilde (new fresh name created in the method) and the set of variable aTilde (formal parameter of a method)

public class BigLam {

    String methodName;
    Term methodContract;
    Set<GroupName> bTilde;
    Set<GroupName> aTilde;

    VarSubstitution lastBFresh;


    Lam w;
    Lam wPrime;

 // constructor from methodName and methodContract
    public BigLam(String method, MainMethodContract methodContractInferred){
        this.methodName = method;
        this.methodContract = methodContractInferred;

        this.lastBFresh = new VarSubstitution();

        this.w = new Lam();
        this.wPrime = new Lam();
        
        this.bTilde = new TreeSet<GroupName>();
        this.aTilde = new TreeSet<GroupName>();
    }
    
    // constructor from methodName and methodContract
    public BigLam(String method, Term methodContractInferred){
        //initialize the lam for the method MethodName, and use this MethodContract obtained form inference algorithm to produce
        //bTilde

        this.methodName = method;
        this.methodContract = methodContractInferred;

        this.lastBFresh = new VarSubstitution();

        this.w = new Lam();
        this.wPrime = new Lam();


        if(methodContractInferred instanceof MethodContract){
            MethodInterface methodInterface = ((MethodContract) methodContractInferred).getMethodInterface();
            Contract contractP = ((MethodContract) methodContractInferred).getContractPresent();
            Contract contractF = ((MethodContract) methodContractInferred).getContractPresent();

            IRecord _this = methodInterface.getThis();
            IRecord ret =  methodInterface.getResult();


            List<IRecord> args = methodInterface.getParameters();

            Set<GroupName> bTildeTemp = contractP.fn();
            bTildeTemp.addAll(contractF.fn());
            
            this.bTilde = bTildeTemp;
            this.bTilde.addAll(ret.fn());
            this.bTilde.removeAll(_this.fn());
            this.aTilde = new TreeSet<GroupName>();
            this.aTilde.addAll(_this.fn());
            for(IRecord t : args){
                this.bTilde.removeAll(t.fn());
                this.aTilde.addAll(t.fn());
            }
        }
        else {
            this.bTilde = new TreeSet<GroupName>();
            this.aTilde = new TreeSet<GroupName>();
        }

    }

    //Getter and Setter for Lam
    public Lam getFirst(){
        return this.w;
    }

    public Lam getSecond(){
        return this.wPrime;
    }

    public void setFirst(Lam l){
        Lam l2 = l.minimize();
        this.w = l2;
    }

    public void setSecond(Lam l){
        Lam l2 = l.minimize();
        this.wPrime = l2;
    }

    //Getter for aTilde and bTilde
    public Set<GroupName> getbTilde(){
        return this.bTilde;
    }

    public Set<GroupName> getaTilde(){
        return this.aTilde;
    }

    //Getter for lastBFresh used for saturation
    public VarSubstitution getLastBFresh(){
        return this.lastBFresh;
    }

    public void setLastBFresh(VarSubstitution sub){
        this.lastBFresh = sub;
    }


    //Getter for methodName and methodContract
    public String getMethodName(){
        return this.methodName;
    }

    public Term getMethodContract(){
        return this.methodContract;
    }

    //Return the freeVariable of a BigLamp
    public Set<GroupName> fv(){
        Set<GroupName> fv = new TreeSet<GroupName>();
        fv.addAll(this.w.fv());
        fv.addAll(this.wPrime.fv());
        return fv;
    }

    //check for Cycle
    public Boolean hasCycle(){
        //there is a Cycle when one of the two cycle types is present
        return w.hasCycle() || wPrime.hasCycle();
    }


   
    //check just for Get Cycle
    public Boolean hasCycleGet(){
        //it has cycle if any of the two lamps is cyclic
        return w.hasCycleGet() || wPrime.hasCycleGet();
    }

    
  
    //check just for Await Cycle
    public Boolean hasCycleAwait(){
      //it has cycle if any of the two lamps is cyclic
        return w.hasCycleAwait() || wPrime.hasCycleAwait();
    }
    
    //toString method
    public String toString(){
        return "< \n" + this.w.toString() + " , \n" + this.wPrime.toString() + ">";
    }

    //According to version2 of the fix point algorithm this method adds the missing dependencies from the transitive closure 
    //of the dependencies of not new names and removes all dependencies with new names
    public void expandAndClean() {
        
        w.expandAndClean();
        wPrime.expandAndClean();
    }

    public Boolean hasReflexiveState() {
        // TODO Auto-generated method stub
        return w.hasReflexiveState() || wPrime.hasReflexiveState();
    }
    
    public LinkedList<State> getReflexiveStates(){
        LinkedList<State> res = new LinkedList<>();
        res.addAll(w.getReflexiveStates());
        res.addAll(wPrime.getReflexiveStates());
        return res;
    }


}
