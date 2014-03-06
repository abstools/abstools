package deadlock.analyser.detection;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.TreeSet;

import abs.frontend.ast.ASTNode;

import deadlock.analyser.factory.GroupName;
//import deadlock.constraints.term.TermVariable;

//Class State implement the structure that contains a "list of couple (a,b)", this list is implement like an HashMap, the key is a TermVariable and the value is a 
//list of other TermVariable which appear in the right side of a couple dependency.

public class State {

    //First TermVariable is for the first element of a couple (a,b) , then there is a list that contain all of the second element of couple (starting with the same element)
    //List is not a simple TermVariable List but contain a Couple of TermVariable and Boolean value, this boolean value is used to easily find loop, is a flag that
    //say if I have already visited this TermVariable objectName
    HashMap<GroupName, List<Couple>> depCouple;

    Boolean cycle;

    public State(){
        this.depCouple = new HashMap<GroupName, List<Couple>>();
        this.cycle = false;
    }


    public HashMap<GroupName, List<Couple>> getDepCouple(){
        return this.depCouple;
    }

    //Method for add (a,b) to this State
    public void addCouple(GroupName a, GroupName b){ 	
        for(GroupName v : depCouple.keySet()){
            if(v.toString().equals(a.toString())){
                for(Couple c : depCouple.get(v)){
                    if(c.getVar().toString().equals(b.toString()) && c.isGet()) return; // (a,b) is already in the State
                    if(c.getVar().toString().equals(b.toString()) && !c.isGet()){ // (a,b)@ is already in the State and becames (a,b)
                        c.setGet(); 
                        return;
                    }
                } // (a,b) is not in the State but I have a list with Head a
                List<Couple> dep = depCouple.get(v);
                dep.add(new Couple(b, false));
                depCouple.put(a, dep);
                return;
            }
        } // (a,b) is not in the State and there is not a list with head a
        List<Couple> dep = new LinkedList<Couple>();
        dep.add(new Couple(b, false));
        depCouple.put(a, dep);

        //I can add a check if I insert a cycle with if(LookForCycle(a,a) == true) ...
    }

    //Method for add (a,b)@ to this State
    public void addCoupleAwait(GroupName a, GroupName b){ 	
        for(GroupName v : depCouple.keySet()){
            if(v.toString().equals(a.toString())){
                for(Couple c : depCouple.get(v)){
                    if(c.getVar().toString().equals(b.toString())) return; // (a,b) is already in the State
                } // (a,b) is not in the State but I have a list with Head a
                List<Couple> dep = depCouple.get(v);
                dep.add(new Couple(b, false, false));
                depCouple.put(a, dep);
                return;
            }
        } // (a,b) is not in the State and there is not a list with head a
        List<Couple> dep = new LinkedList<Couple>();
        dep.add(new Couple(b, false, false));
        depCouple.put(a, dep);

        //I can add a check if I insert a cycle with if(LookForCycle(a,a) == true) ...
    }

    //Method for add entire State s to this State, useful for parallel operation
    public void addState(State s){
        //if s has cycle, also this + s will have once
        if(s.getCycle() == true) this.cycle = true;

        HashMap<GroupName, List<Couple>> depCoupleS = s.getDepCouple();
        for(GroupName a : depCoupleS.keySet()){
            for(Couple c: depCoupleS.get(a)){
                if(c.isGet()) this.addCouple(a, c.getVar());
                else		  this.addCoupleAwait(a, c.getVar());
            }
        }
    }

    //Method to know if a state is contained in this state
    public Boolean containState(State s){
        Boolean b = true;
        for(GroupName v : s.getDepCouple().keySet()){
            for(Couple c : s.getDepCouple().get(v)){
                if(c.isGet()) b = b && containCouple(v,c.getVar());
                else b = b && containCoupleAwait(v,c.getVar());
            }
        }
        return b;
    }

    //Method to know if a couple await is contained in this state
    public Boolean containCoupleAwait(GroupName a, GroupName b){
        for(GroupName v : depCouple.keySet()){
            if(v.toString().equals(a.toString())){
                for(Couple c : depCouple.get(v)){
                    if(c.getVar().toString().equals(b.toString())) return true;
                }
            }
        }
        return false;
    }

    //Method to know if a couple get is contained in this state
    public Boolean containCouple(GroupName a, GroupName b){
        for(GroupName v : depCouple.keySet()){
            if(v.toString().equals(a.toString())){
                for(Couple c : depCouple.get(v)){
                    if(c.getVar().toString().equals(b.toString()) && c.isGet()) return true;
                }
            }
        }
        return false;
    }


    //Method for Test if there is a cycle and Set the field Cycle
    public Boolean hasCycle(){
        Boolean res = false;
        if(this.cycle == true) return true;
        else{ // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
            for(GroupName v : depCouple.keySet()){
                res = res || LookForCycle(v,v);
                ClearAllFlag();
            }
        }

        // if I find Cycle, Cycle will be into this State forever
        if(res == true) this.cycle = true;

        return res;
    }


    // I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
    public Boolean LookForCycle(GroupName a, GroupName look){
        Boolean res = false;

        // check if TermVariable a is and head_TermVariable
        if(depCouple.containsKey(a)){
            for(Couple c : depCouple.get(a)) if(c.getVar().toString().equals(look.toString())) return true; //first I try to find 'look' TermVariable
            for(Couple c : depCouple.get(a)){ //then I do a visit on the TermVariable that I've do not visit yet
                if(c.getFlag() == false){
                    c.setFlag();
                    res = res || LookForCycle(c.getVar(), look);
                }
            }

        }
        return res;
    }



    //Method for Test if there is a pure cycle of get and Set the field Cycle
    public Boolean hasCycleGet(){
        Boolean res = false;
        // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
        for(GroupName v : depCouple.keySet()){
            res = res || LookForCycleGet(v,v);
            ClearAllFlag();
        }

        // if I find Cycle, Cycle will be into this State forever
        if(res == true) this.cycle = true;

        return res;
    }

    // I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
    public Boolean LookForCycleGet(GroupName a, GroupName look){
        Boolean res = false;

        // check if TermVariable a is and head_TermVariable
        if(depCouple.containsKey(a)){
            for(Couple c : depCouple.get(a)) if(c.getVar().toString().equals(look.toString()) && c.isGet()) return true; //first I try to find 'look' TermVariable in a get Var
            for(Couple c : depCouple.get(a)){ //then I do a visit on the TermVariable that I've do not visit yet and only if they are get Variable
                if(c.getFlag() == false && c.isGet()){
                    c.setFlag();
                    res = res || LookForCycleGet(c.getVar(), look);
                }
            }

        }
        return res;
    }

    //Method for Test if there is a pure cycle of get and Set the field Cycle
    public Boolean hasCycleAwait(){
        Boolean res = false;
        // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
        for(GroupName v : depCouple.keySet()){
            res = res || LookForCycleAwait(v,v);
            ClearAllFlag();
        }

        // if I find Cycle, Cycle will be into this State forever
        if(res == true) this.cycle = true;

        return res;
    }


    // I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
    public Boolean LookForCycleAwait(GroupName a, GroupName look){
        Boolean res = false;

        // check if TermVariable a is and head_TermVariable
        if(depCouple.containsKey(a)){
            for(Couple c : depCouple.get(a)) if(c.getVar().toString().equals(look.toString()) && !c.isGet()) return true; //first I try to find 'look' TermVariable in a get Var
            for(Couple c : depCouple.get(a)){ //then I do a visit on the TermVariable that I've do not visit yet and only if they are get Variable
                if(c.getFlag() == false && !c.isGet()){
                    c.setFlag();
                    res = res || LookForCycleAwait(c.getVar(), look);
                }
            }

        }
        return res;
    }



    // Clear all flag
    public void ClearAllFlag(){
        for(GroupName v : depCouple.keySet())
            for(Couple c : depCouple.get(v))
                c.clearFlag();
    }
    
    // Clear all flag
    public void ClearAllNewFlag(){
        for(GroupName v : depCouple.keySet())
                v.visited = false;
    }


    //toString method
    public String toString(){
        String res = "";
        for(GroupName v : depCouple.keySet()){
            res += v.toString() + ": ";
            for(Couple c : depCouple.get(v)){
                res += c.toString() + " ";
            }
            res += "\n";
        }
        return res;
    }

    //getter method for cycle info
    public Boolean getCycle(){
        return this.cycle;
    }

    //VarSubstitution (renaming) application
    public void apply(VarSubstitution s){

        //in this first loop I substitute only the Value of the HashMap, not the key
        for(GroupName v : depCouple.keySet()){
            for(Couple c : depCouple.get(v)){
                c.apply(s);
            }
        }

        //calculate the key that have to be renamed
        Set<GroupName> keys = depCouple.keySet();
        for(GroupName v : s.getMap().keySet()){
            if(keys.contains(v)){
                depCouple.put(s.apply(v), depCouple.get(v));
                depCouple.remove(v);
            }
        }

    }

    public Integer numberOfDep(){
        Integer i = 0;
        for(GroupName v : depCouple.keySet())
            for(Couple c : depCouple.get(v))
                i++;
        return i;
    }

    public Set<GroupName> fv(){
        Set<GroupName> fv = new TreeSet<GroupName>();
        for(GroupName v : depCouple.keySet()){
            fv.add(v);
            for(Couple c : depCouple.get(v)){
                fv.add(c.getVar());
            }
        }
        return fv;
    }

    
    //Method for Test if there is a cycle and Set the field Cycle
    public Boolean hasNewCycle(){
        ClearAllNewFlag();

        Stack<GroupName> S = new Stack<GroupName>();
        
        Boolean res = false;
         // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
            for(GroupName v : depCouple.keySet()){
                if(v.visited == false)
                    res = NewLookForCycle(v,S);
                if(res)
                    break;
            }
        

        // if I find Cycle, Cycle will be into this State forever
        if(res == true) this.cycle = true;
        return res;
    }
    
    // I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
    public Boolean NewLookForCycle(GroupName a, Stack<GroupName> S){
        a.visited = true;
        Boolean res = false;

        if(depCouple.get(a) == null || depCouple.get(a).isEmpty()){
            return false;
        }
        S.push(a);

        for(Couple c : depCouple.get(a)){
            GroupName b = c.getVar();
            if(b.visited == false){
                res = NewLookForCycle(b,S);
                if(res) 
                    return true;
            }
            else if(S.contains(b))
                return true;
        }
        S.pop();
        return false;
    }

    //Method for Test if there is a cycle and Set the field Cycle
    public Boolean hasNewCycleGet(){
        ClearAllNewFlag();
        Stack<GroupName> S = new Stack<GroupName>();
        
        Boolean res = false;
         // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
            for(GroupName v : depCouple.keySet()){
                if(v.visited == false)
                    res = NewLookForCycleGet(v,S);
                if(res)
                    break;
            }
        

        // if I find Cycle, Cycle will be into this State forever
        if(res == true) this.cycle = true;
        return res;
    }
    
    // I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
    public Boolean NewLookForCycleGet(GroupName a, Stack<GroupName> S){
        a.visited = true;
        Boolean res = false;

        if(depCouple.get(a) == null || depCouple.get(a).isEmpty())
            return false;
        S.push(a);
        for(Couple c : depCouple.get(a)){

            GroupName b = c.getVar();

            if(!c.isGet()){
                continue;
            }
            if(b.visited == false){
                res = NewLookForCycleGet(b,S);
                if(res) 
                    return true;
            }
            else if(S.contains(b))
                return true;
        }
        S.pop();
        return false;
    }
    
    //Method for Test if there is a cycle and Set the field Cycle
    public Boolean hasNewCycleAwait(){
        ClearAllNewFlag();

        Stack<GroupName> S = new Stack<GroupName>();
        
        Boolean res = false;
        // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
            for(GroupName v : depCouple.keySet()){
                if(v.visited == false)
                    res = NewLookForCycleAwait(v,S);
                if(res)
                    break;
            }
        

        // if I find Cycle, Cycle will be into this State forever
        if(res == true) this.cycle = true;
        return res;
    }
    
    // I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
    public Boolean NewLookForCycleAwait(GroupName a, Stack<GroupName> S){
        a.visited = true;
        Boolean res = false;

        if(depCouple.get(a) == null || depCouple.get(a).isEmpty())
            return false;
        S.push(a);

        for(Couple c : depCouple.get(a)){
            GroupName b = c.getVar();
            if(c.isGet())
                continue;
            if(b.visited == false){
                res = NewLookForCycleAwait(b,S);
                if(res) 
                    return true;
            }
            else if(S.contains(b))
                return true;
        }
        S.pop();
        return false;
    }


}
