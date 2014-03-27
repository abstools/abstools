package deadlock.analyser.detection;

import java.util.LinkedList;
import java.util.Set;
import java.util.TreeSet;

import abs.frontend.ast.ASTNode;

import deadlock.analyser.factory.GroupName;
//import deadlock.constraints.term.TermVariable;

public class Lamp {

    /*String MethodName;
	Term MethodContract;
	Set<Variable> bTilde;*/

    LinkedList<State> states;

    /*public Lamp(String Method, Term MethodContractInferred){
		//initialize the lamp for the method MethodName, and use this MethodContract obtained form inference algorithm to produce
		//bTilde
		this.MethodName = Method;
		this.MethodContract = MethodContractInferred;

		if(((TermStructured) MethodContractInferred).getConstructor().equals("MethodContract")){
			Term MethodInterface = ((TermStructured) MethodContractInferred).getSubTerms().get(0);
			Term Contract = ((TermStructured) MethodContractInferred).getSubTerms().get(1);

			Term This = ((TermStructured) MethodInterface).getSubTerms().get(0);
			Term Ret =  ((TermStructured) MethodInterface).getSubTerms().get(  ((TermStructured) MethodInterface).getSubTerms().size() -1 );
			List<Term> Arg = ((TermStructured) MethodInterface).getSubTerms().subList(1, ((TermStructured) MethodInterface).getSubTerms().size() -2);

			this.bTilde = Contract.fv();
			this.bTilde.addAll(Ret.fv());
			this.bTilde.removeAll(This.fv());
			for(Term t : Arg){
				this.bTilde.removeAll(t.fv());
			}
		}
		else this.bTilde = new TreeSet<Variable>();

	}*/

    // Constructor
    public Lamp(){
        this.states = new LinkedList<State>();
    }

    // Constructor copy
    public Lamp(Lamp l){
        this.states = new LinkedList<State>(l.getStates());
    }

    // Constructor copy2
    public Lamp(LinkedList<State> s){
        this.states = s;
    }

    // getter and setter for the states
    public LinkedList<State> getStates(){
        return this.states;
    }

    public void setStates(LinkedList<State> s){
        this.states = s;
    }

    //add state to lamp
    public void addState(State s){
        Boolean b = false;
        for(State sthis : this.states){
            b = b || sthis.containState(s);
        }
        if(b) return;
        this.states.add(s);
    }

    //minimize a lamp
    public Lamp minimize(){
        Lamp l = new Lamp();
        //LinkedList<State> s = new LinkedList<State>();
        for(int i = 0; i< this.states.size(); i++){
            boolean contained = false;
            for(int j = 0; j < this.states.size(); j++){
                if(i==j) continue;
                if(this.states.get(j).containState(this.states.get(i))) contained = true;
            }
            if(!contained)
                l.addState(this.states.get(i));
        }
        return l;
    }

    // add couple of dependencies (get) on a Lamp, it means to add that couple to all the states in the lamp
    public void addCouple(GroupName a, GroupName b){
        for(State s : states){
            s.addCouple(a, b);
        }
        if(states.isEmpty()){
            State s = new State();
            s.addCouple(a, b);
            states.add(s);
        }
    }

    // add couple of dependencies (await) on a Lamp, it means to add that couple to all the states in the lamp
    public void addCoupleAwait(GroupName a, GroupName b){
        for(State s : states){
            s.addCoupleAwait(a, b);
        }
        if(states.isEmpty()){
            State s = new State();
            s.addCoupleAwait(a, b);
            states.add(s);
        }
    }



    // add a Lamp l to this lamp
    public void addLamp(Lamp l){
        LinkedList<State> sts = l.getStates();
        for(State s : sts){
            Boolean contained = false;
            for(State s1 : this.states){
                contained = contained || s1.containState(s);
            }
            if(!contained){
                State sNew = new State();
                sNew.addState(s);
                this.states.add(sNew);
            }
        }
    }

    // parallel between this Lamp and Lamp l
    public void parallel(Lamp l){
        LinkedList<State> sts = l.getStates();
        // if Lamp l is empty, the result of parallel is this
        if(sts.isEmpty()) return;
        // if Lamp this is empty, the result of parallel is l
        if(this.states.isEmpty()){
            this.addLamp(l);
            return;
        }

        //now we can work assuming that both l and this have at least one state
        //we need to calculate the cartesian product
        LinkedList<State> cartesianState = new LinkedList<State>();
        for(State s1 : this.states){
            for(State s2 : sts){
                State s3 = new State();
                s3.addState(s1);
                s3.addState(s2);
                cartesianState.add(s3);
            }
        }
        //in 'this' we already have the state of this, ok, we surely have to add the states of l
        this.addLamp(l);
        //now we have to add the cartesian product
        for(State s : cartesianState){
            this.addState(s);
        }
        //this.states.addAll(cartesianState);
    }



    // get the FreeVariable of a Lamp
    public Set<GroupName> fv(){
        Set<GroupName> fv = new TreeSet<GroupName>();
        for(State s : states){
            fv.addAll(s.fv());
        }
        return fv;
    }

    public void apply(VarSubstitution s){
        for(State st : states){
            st.apply(s);
        }
    }

    public Integer numberOfDep(){
        Integer i = 0;
        for(State st : states)
            i+=st.numberOfDep();
        return i;
    }

    public Boolean hasCycle(){
        Boolean res = false;
        for(State st : states)
            res = res || st.hasCycle();
        return res;
    }
    
    public Boolean hasNewCycle(){
        Boolean res = false;
        for(State st : states)
            res = res || st.hasNewCycle();
        return res;
    }


    public Boolean hasCycleGet(){
        Boolean res = false;
        for(State st : states)
            res = res || st.hasCycleGet();
        return res;
    }
    
    public Boolean hasNewCycleGet(){
        Boolean res = false;
        for(State st : states)
            res = res || st.hasNewCycleGet();
        return res;
    }

    
    public Boolean hasCycleAwait(){
        Boolean res = false;
        for(State st : states)
            res = res || st.hasCycleAwait();
        return res;
    }
    
    public Boolean hasNewCycleAwait(){
        Boolean res = false;
        for(State st : states)
            res = res || st.hasNewCycleAwait();
        return res;
    }
    

    public String toString(){
        String res = "";
        for(State s : states){
            res += s.toString() + "\n";
        }
        return res;
    }

}
