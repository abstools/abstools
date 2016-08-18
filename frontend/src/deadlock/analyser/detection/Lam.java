package deadlock.analyser.detection;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Set;
import java.util.TreeSet;

import deadlock.analyser.factory.GroupName;


public class Lam {

    //The collection of states for a Lam
    LinkedList<State> states;

    // Constructor
    public Lam(){
        this.states = new LinkedList<State>();
    }

    // Constructor copy
    public Lam(Lam l){
        this.states = new LinkedList<State>(l.getStates());
    }

    // Constructor copy2
    public Lam(LinkedList<State> s){
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
       
        for(State sthis : this.states){
            if(sthis.containState(s))
                return;
        }
        
        this.states.add(s);
    }

    //TODO ABEL: check correctness
    //minimize a lamp
    public Lam minimize(){
        Lam l = new Lam();
        for(int i = 0; i< this.states.size(); i++){
            boolean contained = false;
            for(int j = 0; j < l.states.size(); j++){
                if(i==j) continue;
                if(l.states.get(j).containState(this.states.get(i))) contained = true;
            }
            if(!contained)
                l.addState(this.states.get(i));
        }
        return l;
    }

    // add couple of dependencies (get) on a Lamp, it means to add that couple to all the states in the lamp
    public void addCouple(GroupName a, GroupName b){
        if(states.isEmpty()){
            states.add(new State());
        }
        for(State s : states){
            s.addCouple(a, b);
        }
        
    }

    // add couple of dependencies (await) on a Lamp, it means to add that couple to all the states in the lamp
    public void addCoupleAwait(GroupName a, GroupName b){
        if(states.isEmpty()){
            states.add(new State());
        }
        
        for(State s : states){
            s.addCoupleAwait(a, b);
        }
       
    }



    // add a Lamp l to this lam, this is the + lam operator
    public void addLamp(Lam l){
        //check all l states and add those that are not present in the current states
        for(State s : l.states){
            Boolean contained = false;
            for(State s1 : this.states){
                //if any current state contains s there is no need to continue searching
                if(contained = s1.containState(s))
                    break;
            }
            
            //if s is not contained then add the state
            if(!contained){
                State sNew = new State();
                sNew.addState(s);
                sNew.setDebugInfo(s.getDebugInfo());
                this.states.add(sNew);
            }
        }
    }

    //TODO ABEL: Check correctness
    // parallel between this Lam and Lam l, this is the || lam operator
    public void parallel(Lam l){
        
        // if Lamp l is empty, the result of parallel is this
        if(l.states.isEmpty()) return;
        
        // if Lamp this is empty, the result of parallel is l
        if(this.states.isEmpty()){
            this.addLamp(l);
            return;
        }

        //now we can work assuming that both l and this have at least one state
        //we need to calculate the cartesian product
        LinkedList<State> cartesianState = new LinkedList<State>();
        for(State s1 : this.states){
            for(State s2 : l.states){
                State s3 = new State();
                s3.addState(s1);
                s3.addState(s2);
                cartesianState.add(s3);
            }
        }
        
        this.states = cartesianState;
    }



    //TODO ABEL: Check correctness
    // get the FreeVariable of a Lamp
    public Set<GroupName> fv(){
        Set<GroupName> fv = new TreeSet<GroupName>();
        for(State s : states){
            fv.addAll(s.fv());
        }
        return fv;
    }

    // realize the name substitution defined in s
    public void apply(VarSubstitution s){
        for(State st : states){
            st.apply(s);
        }
    }

    // calculates the current number of dependencies, used in the fix point iteration to determine
    // if there are new changes
    public Integer numberOfDep(){
        Integer i = 0;
        for(State st : states)
            i+=st.numberOfDep();
        return i;
    }
    
    
    //check for Cycle that might be composed by any kind of dependencies
    //if there is a cycle and there is at least one get dependency then this is a deadlock
    //otherwise this is a livelock and the result will be the same of hasAwaitCycle
    public Boolean hasCycle(){
      //it has cycle if there is any cyclic state
        for(State st : states)
            if(st.hasCycle())
                return true;
        return false;
    }


   
    //check for a pure get dependencies cycle
    public Boolean hasCycleGet(){
        //it has cycle if there is any cyclic state
        for(State st : states)
            if(st.hasCycleGet())
                return true;
        return false;
    }

    
    //check just for Await Cycle, this check for what is called livelock
    public Boolean hasCycleAwait(){
        //it has cycle if there is any cyclic state
        for(State st : states)
            if(st.hasCycleAwait())
                return true;
        return false;
    }
    

    public String toString(){
        String res = "";
        for(State s : states){
            res += s.toString() + "\n";
        }
        return res;
    }

    //According to version2 of the fix point algorithm this method adds the missing dependencies from the transitive closure 
    //of the dependencies of not new names and removes all dependencies with new names
    public void expandAndClean() {
        for(State s: states)
            s.expandAndClean();
    }

    public boolean hasReflexiveState() {
        // TODO Auto-generated method stub
        for(State s: states)
            if(s.hasReflexiveState())
                return true;
        
        return false;
    }

    public void updateStackTrace(String method) {
        for(State s: states) {
            LinkedList<String> callStack = s.getDebugInfo().callStack;
            if(!callStack.contains(method))
                callStack.add(method);
        }
    }

    public LinkedList<State> getReflexiveStates() {
        LinkedList<State> res = new LinkedList<>();
        for(State s: states)
            if(s.hasReflexiveState())
                 res.add(s);
        
        return res;
    }

}
