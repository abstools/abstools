package deadlock.analyser.detection;

import deadlock.analyser.factory.Contract;
import deadlock.analyser.factory.GroupName;
import deadlock.analyser.factory.MethodInterface;
import deadlock.analyser.factory.Record;
import deadlock.constraints.term.Term;
//import deadlock.constraints.term.TermStructured;
import deadlock.analyser.factory.MethodContract;
//import deadlock.constraints.term.TermVariable;
//import deadlock.constraints.term.Variable;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;


// a BigLamp is a quadruple <methodName, methodContract, Lamp w, Lamp wPrime>
// and it contain the set of variable bTilde (new fresh name created in the method) and the set of variable aTilde (formal parameter of a method)

public class BigLamp {

    String methodName;
    Term methodContract;
    Set<GroupName> bTilde;
    Set<GroupName> aTilde;

    VarSubstitution lastBFresh;


    Lamp w;
    Lamp wPrime;

    // constructor from method name and methodContract
    public BigLamp(String method, Term methodContractInferred){
        //initialize the lamp for the method MethodName, and use this MethodContract obtained form inference algorithm to produce
        //bTilde

        this.methodName = method;
        this.methodContract = methodContractInferred;

        this.lastBFresh = new VarSubstitution();

        this.w = new Lamp();
        this.wPrime = new Lamp();


        if(methodContractInferred instanceof MethodContract){
            MethodInterface methodInterface = ((MethodContract) methodContractInferred).getMethodInterface();
            Contract contract = ((MethodContract) methodContractInferred).getContract();

            Record _this = methodInterface.getThis();
            Record ret =  methodInterface.getResult();


            List<Record> args = methodInterface.getParameters();

            this.bTilde = contract.fn();
            this.bTilde.addAll(ret.fn());
            this.bTilde.removeAll(_this.fn());
            this.aTilde = new TreeSet<GroupName>();
            this.aTilde.addAll(_this.fn());
            for(Record t : args){
                this.bTilde.removeAll(t.fn());
                this.aTilde.addAll(t.fn());
            }
        }
        else {
            this.bTilde = new TreeSet<GroupName>();
            this.aTilde = new TreeSet<GroupName>();
        }

        //System.out.println("DEBUG: bTilde = " + bTilde);
        //System.out.println("DEBUG: aTilde = " + aTilde);

    }

    //Getter and Setter for Lamp
    public Lamp getFirst(){
        return this.w;
    }

    public Lamp getSecond(){
        return this.wPrime;
    }

    public void setFirst(Lamp l){
        Lamp l2 = l.minimize();
        this.w = l2;
    }

    public void setSecond(Lamp l){
        Lamp l2 = l.minimize();
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

    //toString method
    public String toString(){
        return "< \n" + this.w.toString() + " , \n" + this.wPrime.toString() + ">";
    }


}
