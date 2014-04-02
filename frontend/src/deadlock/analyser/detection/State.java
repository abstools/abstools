package deadlock.analyser.detection;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import deadlock.analyser.factory.GroupName;
//import deadlock.constraints.term.TermVariable;

//Class State implement the structure that contains a "list of couple (a,b)", this list is implement like an HashMap, the key is a TermVariable and the value is a 
//list of other TermVariable which appear in the right side of a couple dependency.

//TODO ABEL: review DONE

public class State {

    //First TermVariable is for the first element of a couple (a,b) , then there is a list that contain all of the second element of couple (starting with the same element)
    //List is not a simple TermVariable List but contain a Couple of TermVariable and Boolean value, this boolean value is used to easily find loop, is a flag that
    //say if I have already visited this TermVariable objectName
    HashMap<GroupName, HashSet<GroupName>> depCouple;
    HashMap<GroupName, HashSet<GroupName>> depCoupleAwait;
    
    Boolean hasCycle;

    public State(){
        this.depCouple = new HashMap<GroupName, HashSet<GroupName>>();
        this.depCoupleAwait = new HashMap<GroupName, HashSet<GroupName>>();
        this.hasCycle = false;
    }


    //TODO ABEL: ERASE THIS
//    public HashMap<GroupName, List<Couple>> getDepCouple(){
//        return this.depCouple;
//    }

    //Method for add (a,b) to this State
    public void addCouple(GroupName a, GroupName b){
        if(!depCouple.containsKey(a))
            depCouple.put(a, new HashSet<GroupName>());
        
        HashSet<GroupName> aCouples = depCouple.get(a);            
        
        if(!aCouples.contains(b))
            aCouples.add(b);
        
        HashSet<GroupName> aCouplesAwait;
        if((aCouplesAwait = depCoupleAwait.get(a)) != null && aCouplesAwait.contains(b))
            aCouplesAwait.remove(b);

        //TODO ABEL: ERASE THIS
//        for(GroupName v : depCouple.keySet()){
//            if(v.toString().equals(a.toString())){
//                for(Couple c : depCouple.get(v)){
//                    if(c.getVar().toString().equals(b.toString()) && c.isGet()) return; // (a,b) is already in the State
//                    if(c.getVar().toString().equals(b.toString()) && !c.isGet()){ // (a,b)@ is already in the State and becames (a,b)
//                        c.setGet(); 
//                        return;
//                    }
//                } // (a,b) is not in the State but I have a list with Head a
//                List<Couple> dep = depCouple.get(v);
//                dep.add(new Couple(b, false));
//                depCouple.put(a, dep);
//                return;
//            }
//        } // (a,b) is not in the State and there is not a list with head a
//        List<Couple> dep = new LinkedList<Couple>();
//        dep.add(new Couple(b, false));
//        depCouple.put(a, dep);

        //I can add a check if I insert a cycle with if(LookForCycle(a,a) == true) ...
    }

    //Method for add (a,b)@ to this State
    
    public void addCoupleAwait(GroupName a, GroupName b){ 	
        if(!containsCouple(a, b))
        {
            if(!depCoupleAwait.containsKey(a))
                depCoupleAwait.put(a, new HashSet<GroupName>());
            
            HashSet<GroupName> aCouplesAwait = depCoupleAwait.get(a);            
            
            if(!aCouplesAwait.contains(b))
                aCouplesAwait.add(b);
        }
    
    //TODO ABEL: ERASE THIS
//        for(GroupName v : depCouple.keySet()){
//            if(v.toString().equals(a.toString())){
//                for(Couple c : depCouple.get(v)){
//                    if(c.getVar().toString().equals(b.toString())) return; // (a,b) is already in the State
//                } // (a,b) is not in the State but I have a list with Head a
//                List<Couple> dep = depCouple.get(v);
//                dep.add(new Couple(b, false, false));
//                depCouple.put(a, dep);
//                return;
//            }
//        } // (a,b) is not in the State and there is not a list with head a
//        List<Couple> dep = new LinkedList<Couple>();
//        dep.add(new Couple(b, false, false));
//        depCouple.put(a, dep);

        //I can add a check if I insert a cycle with if(LookForCycle(a,a) == true) ...
    }

    //Method for add entire State s to this State, useful for parallel operation
    public void addState(State s){
        hasCycle = hasCycle || s.hasCycle;

        for(GroupName a : s.depCouple.keySet())
            for(GroupName b: s.depCouple.get(a))
                addCouple(a, b);
        
        for(GroupName a : s.depCoupleAwait.keySet())
            for(GroupName b: s.depCoupleAwait.get(a))
                addCoupleAwait(a, b);
        
        //TODO ABEL: ERASE THIS
        //if s has cycle, also this + s will have once
//        if(s.getCycle() == true) this.cycle = true;
//
//        HashMap<GroupName, List<Couple>> depCoupleS = s.getDepCouple();
//        for(GroupName a : depCoupleS.keySet()){
//            for(Couple c: depCoupleS.get(a)){
//                if(c.isGet()) this.addCouple(a, c.getVar());
//                else		  this.addCoupleAwait(a, c.getVar());
//            }
//        }
    }

    //Method to know if a state is contained in this state
    
    public Boolean containState(State s){
        
        
        for(GroupName a : s.depCouple.keySet())
            for(GroupName b: s.depCouple.get(a))
                if(!containsCoupleGet(a, b))
                    return false;
        
        for(GroupName a : s.depCoupleAwait.keySet())
            for(GroupName b: s.depCoupleAwait.get(a))
                //TODO QUESTION ABEL: this methods says that if an await couple from s is contained as a get couple in this
                //is correct to say that this contains s
                if(!containsCoupleAwait(a, b))
                    return false;
        
        return true;
        //TODO ABEL: ERASE THIS
//        Boolean b = true;
//        for(GroupName v : s.getDepCouple().keySet()){
//            for(Couple c : s.getDepCouple().get(v)){
//                if(c.isGet()) b = b && containCouple(v,c.getVar());
//                else b = b && containCoupleAwait(v,c.getVar());
//            }
//        }
//        return b;
    }
    
    
    //TODO ABEL: ERASE THIS METHOD

    //Method to know if a couple await is contained in this state
//    public Boolean containCoupleAwait(GroupName a, GroupName b){
//        for(GroupName v : depCouple.keySet()){
//            if(v.toString().equals(a.toString())){
//                for(Couple c : depCouple.get(v)){
//                    if(c.getVar().toString().equals(b.toString())) return true;
//                }
//            }
//        }
//        return false;
//    }

    //Method to know if a couple get is contained in this state
    
    private boolean containsCoupleGet(GroupName a, GroupName b) {
        HashSet<GroupName> aGroup;
        return ((aGroup = depCouple.get(a)) != null && aGroup.contains(b)) ;
    }
    
    private boolean containsCoupleAwait(GroupName a, GroupName b) {
        HashSet<GroupName> aGroup;
        return ((aGroup = depCoupleAwait.get(a)) != null && aGroup.contains(b)) ;
    }


    public Boolean containsCouple(GroupName a, GroupName b){
        HashSet<GroupName> aGroup, aGroupAwait;
        return ((aGroup = depCouple.get(a)) != null && aGroup.contains(b)) || ((aGroupAwait = depCoupleAwait.get(a)) != null && aGroupAwait.contains(b));
        
        //TODO ABEL: ERASE THIS
//        for(GroupName v : depCouple.keySet()){
//            if(v.toString().equals(a.toString())){
//                for(Couple c : depCouple.get(v)){
//                    if(c.getVar().toString().equals(b.toString()) && c.isGet()) return true;
//                }
//            }
//        }
//        return false;
    }


    //Method for Test if there is a cycle and Set the field Cycle
//    ******************TODO ABEL: ERASE ALL THESE METHODS (NOT USED)*****************************************
//public Boolean hasCycle(){
//  Boolean res = false;
//  if(this.cycle == true) return true;
//  else{ // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
//      for(GroupName v : depCouple.keySet()){
//          res = res || LookForCycle(v,v);
//          ClearAllFlag();
//      }
//  }
//
//  // if I find Cycle, Cycle will be into this State forever
//  if(res == true) this.cycle = true;
//
//  return res;
//}
//
//
//// I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
//public Boolean LookForCycle(GroupName a, GroupName look){
//  Boolean res = false;
//
//  // check if TermVariable a is and head_TermVariable
//  if(depCouple.containsKey(a)){
//      for(Couple c : depCouple.get(a)) if(c.getVar().toString().equals(look.toString())) return true; //first I try to find 'look' TermVariable
//      for(Couple c : depCouple.get(a)){ //then I do a visit on the TermVariable that I've do not visit yet
//          if(c.getFlag() == false){
//              c.setFlag();
//              res = res || LookForCycle(c.getVar(), look);
//          }
//      }
//
//  }
//  return res;
//}
//
//
//
////Method for Test if there is a pure cycle of get and Set the field Cycle
//public Boolean hasCycleGet(){
//  Boolean res = false;
//  // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
//  for(GroupName v : depCouple.keySet()){
//      res = res || LookForCycleGet(v,v);
//      ClearAllFlag();
//  }
//
//  // if I find Cycle, Cycle will be into this State forever
//  if(res == true) this.cycle = true;
//
//  return res;
//}
//
//// I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
//public Boolean LookForCycleGet(GroupName a, GroupName look){
//  Boolean res = false;
//
//  // check if TermVariable a is and head_TermVariable
//  if(depCouple.containsKey(a)){
//      for(Couple c : depCouple.get(a)) if(c.getVar().toString().equals(look.toString()) && c.isGet()) return true; //first I try to find 'look' TermVariable in a get Var
//      for(Couple c : depCouple.get(a)){ //then I do a visit on the TermVariable that I've do not visit yet and only if they are get Variable
//          if(c.getFlag() == false && c.isGet()){
//              c.setFlag();
//              res = res || LookForCycleGet(c.getVar(), look);
//          }
//      }
//
//  }
//  return res;
//}
//
////Method for Test if there is a pure cycle of get and Set the field Cycle
//public Boolean hasCycleAwait(){
//  Boolean res = false;
//  // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
//  for(GroupName v : depCouple.keySet()){
//      res = res || LookForCycleAwait(v,v);
//      ClearAllFlag();
//  }
//
//  // if I find Cycle, Cycle will be into this State forever
//  if(res == true) this.cycle = true;
//
//  return res;
//}
//
//
//// I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
//public Boolean LookForCycleAwait(GroupName a, GroupName look){
//  Boolean res = false;
//
//  // check if TermVariable a is and head_TermVariable
//  if(depCouple.containsKey(a)){
//      for(Couple c : depCouple.get(a)) if(c.getVar().toString().equals(look.toString()) && !c.isGet()) return true; //first I try to find 'look' TermVariable in a get Var
//      for(Couple c : depCouple.get(a)){ //then I do a visit on the TermVariable that I've do not visit yet and only if they are get Variable
//          if(c.getFlag() == false && !c.isGet()){
//              c.setFlag();
//              res = res || LookForCycleAwait(c.getVar(), look);
//          }
//      }
//
//  }
//  return res;
//}
//
//

// Clear all flag
//    public void ClearAllFlag(){
//        for(GroupName v : depCouple.keySet())
//            for(Couple c : depCouple.get(v))
//                c.clearFlag();
//    }
    
//    public Boolean getCycle(){
//        return this.cycle;
//    }
//*******************************************************************************************************

    
    // Clear all flag
    


    //toString method
    public String toString(){
        String res = "";
        for(GroupName v : depCouple.keySet()){
            res += v.toString() + ": ";
            for(GroupName c : depCouple.get(v)){
                res += c.toString() + " ";
            }
            res += "\n";
        }
        
        for(GroupName v : depCoupleAwait.keySet()){
            res += v.toString() + ": ";
            for(GroupName c : depCoupleAwait.get(v)){
                res += c.toString() + " ";
            }
            res += "\n";
        }
        return res;
    }

    //getter method for cycle info
    

    //VarSubstitution (renaming) application
    public void apply(VarSubstitution s){
        
        depCouple = RefreshHashMap(s, depCouple);
        depCoupleAwait = RefreshHashMap(s, depCoupleAwait);
        
        //TODO ABEL: ERASE THIS
//        //in this first loop I substitute only the Value of the HashMap, not the key
//        for(GroupName v : depCouple.keySet()){
//            for(Couple c : depCouple.get(v)){
//                c.apply(s);
//            }
//        }
//
//        //calculate the key that have to be renamed
//        Set<GroupName> keys = depCouple.keySet();
//        for(GroupName v : s.getMap().keySet()){
//            if(keys.contains(v)){
//                depCouple.put(s.apply(v), depCouple.get(v));
//                depCouple.remove(v);
//            }
//        }

    }

    private HashMap<GroupName, HashSet<GroupName>> RefreshHashMap(VarSubstitution s, HashMap<GroupName, HashSet<GroupName>> toRefresh) {
        
        HashMap<GroupName, HashSet<GroupName>> temp = new HashMap<GroupName, HashSet<GroupName>>(toRefresh.size());
        
        for(GroupName a: toRefresh.keySet())
        {
            GroupName key = s.apply(a);
            if(!temp.containsKey(key)){
              temp.put(key, new HashSet<GroupName>());
            }
            
            for (GroupName b : toRefresh.get(a)){
                temp.get(key).add(s.apply(b));
                
                System.out.println("\t Substitution: " + a + "," + b + " -> " + s.apply(a)+ "," + s.apply(b));
            }
            
        }
        
        return temp;
    }

    
    public Integer numberOfDep(){
        Integer i = 0;
        for(GroupName v : depCouple.keySet())
           i+= depCouple.get(v).size();
        for(GroupName v : depCoupleAwait.keySet())
            i+= depCoupleAwait.get(v).size();
        return i;
    }

    
    public Set<GroupName> fv(){
        Set<GroupName> fv = new TreeSet<GroupName>();
        for(GroupName a : depCouple.keySet()){
            fv.add(a);
            for(GroupName b : depCouple.get(a))
                fv.add(b);
        }
        for(GroupName a : depCoupleAwait.keySet()){
            fv.add(a);
            for(GroupName b : depCoupleAwait.get(a))
                fv.add(b);
        }
        return fv;
    }

    
    public boolean HasCycleGet()
    {
        return HasCycle(depCouple);
    }
    
    public boolean HasCycleAwait()
    {
        return HasCycle(depCoupleAwait);
    }
    
    public boolean HasCycle()
    {
        //return hasCycle = HasCycleGet() || HasCycleAwait();
        
        HashMap<GroupName, HashSet<GroupName>> allTogether = new  HashMap<GroupName, HashSet<GroupName>>();
        
        for(GroupName a : depCouple.keySet())
            for(GroupName b: depCouple.get(a)){
            if(!allTogether.containsKey(a))
                allTogether.put(a, new HashSet<GroupName>());
            
            allTogether.get(a).add(b);
            }
        for(GroupName a : depCoupleAwait.keySet())
            for(GroupName b: depCoupleAwait.get(a)){
            if(!allTogether.containsKey(a))
                allTogether.put(a, new HashSet<GroupName>());
            
            allTogether.get(a).add(b);
            }
        
        return HasCycle(allTogether);
    }
    
    private boolean HasCycle(HashMap<GroupName, HashSet<GroupName>> graph)
    {
        HashSet<GroupName> visited = new HashSet<GroupName>();
        HashSet<GroupName> recorded = new HashSet<GroupName>();
        
        for(GroupName a : graph.keySet())
        {
            if(HasCycleUtil(graph, recorded, visited, a))
                return true;
        }
            
        
        return false;
    }
    
    private boolean HasCycleUtil(HashMap<GroupName, HashSet<GroupName>> graph, HashSet<GroupName> recorded, HashSet<GroupName> visited, GroupName current)
    {
       if(!visited.contains(current))
       {
           visited.add(current);
           recorded.add(current);
           
           if(graph.containsKey(current)){
               for(GroupName b : graph.get(current))
               {
                   if(!visited.contains(b) && HasCycleUtil(graph, recorded, visited, b))
                       return true;
                   if(recorded.contains(b))
                       return true;
               }
           }
       }
       recorded.remove(current);
       return false;
        
    }
    
//*******************TODO ABEL: ERASE OLD CYCLE DETECTION*************************
//  public void ClearAllNewFlag(){
//  for(GroupName v : depCouple.keySet())
//          v.visited = false;
//}
////Method for Test if there is a cycle and Set the field Cycle
//public Boolean hasNewCycle(){
//  ClearAllNewFlag();
//
//  Stack<GroupName> S = new Stack<GroupName>();
//  
//  Boolean res = false;
//   // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
//      for(GroupName v : depCouple.keySet()){
//          if(v.visited == false)
//              res = NewLookForCycle(v,S);
//          if(res)
//              break;
//      }
//  
//
//  // if I find Cycle, Cycle will be into this State forever
//  if(res == true) this.cycle = true;
//  return res;
//}
//
//// I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
//public Boolean NewLookForCycle(GroupName a, Stack<GroupName> S){
//  a.visited = true;
//  Boolean res = false;
//
//  if(depCouple.get(a) == null || depCouple.get(a).isEmpty()){
//      return false;
//  }
//  S.push(a);
//
//  for(Couple c : depCouple.get(a)){
//      GroupName b = c.getVar();
//      if(b.visited == false){
//          res = NewLookForCycle(b,S);
//          if(res) 
//              return true;
//      }
//      else if(S.contains(b))
//          return true;
//  }
//  S.pop();
//  return false;
//}
//
////Method for Test if there is a cycle and Set the field Cycle
//public Boolean hasNewCycleGet(){
//  ClearAllNewFlag();
//  Stack<GroupName> S = new Stack<GroupName>();
//  
//  Boolean res = false;
//   // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
//      for(GroupName v : depCouple.keySet()){
//          if(v.visited == false)
//              res = NewLookForCycleGet(v,S);
//          if(res)
//              break;
//      }
//  
//
//  // if I find Cycle, Cycle will be into this State forever
//  if(res == true) this.cycle = true;
//  return res;
//}
//
//// I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
//public Boolean NewLookForCycleGet(GroupName a, Stack<GroupName> S){
//  a.visited = true;
//  Boolean res = false;
//
//  if(depCouple.get(a) == null || depCouple.get(a).isEmpty())
//      return false;
//  S.push(a);
//  for(Couple c : depCouple.get(a)){
//
//      GroupName b = c.getVar();
//
//      if(!c.isGet()){
//          continue;
//      }
//      if(b.visited == false){
//          res = NewLookForCycleGet(b,S);
//          if(res) 
//              return true;
//      }
//      else if(S.contains(b))
//          return true;
//  }
//  S.pop();
//  return false;
//}
//
////Method for Test if there is a cycle and Set the field Cycle
//public Boolean hasNewCycleAwait(){
//  ClearAllNewFlag();
//
//  Stack<GroupName> S = new Stack<GroupName>();
//  
//  Boolean res = false;
//  // I try to find a generic loop (v,v1) -> (v1,v2) -> ... -> (v_n, v) 
//      for(GroupName v : depCouple.keySet()){
//          if(v.visited == false)
//              res = NewLookForCycleAwait(v,S);
//          if(res)
//              break;
//      }
//  
//
//  // if I find Cycle, Cycle will be into this State forever
//  if(res == true) this.cycle = true;
//  return res;
//}
//
//// I look into the structure to find a Circularity, if I find TermVariable 'look' into the list of head 'a' there is a Cycle
//public Boolean NewLookForCycleAwait(GroupName a, Stack<GroupName> S){
//  a.visited = true;
//  Boolean res = false;
//
//  if(depCouple.get(a) == null || depCouple.get(a).isEmpty())
//      return false;
//  S.push(a);
//
//  for(Couple c : depCouple.get(a)){
//      GroupName b = c.getVar();
//      if(c.isGet())
//          continue;
//      if(b.visited == false){
//          res = NewLookForCycleAwait(b,S);
//          if(res) 
//              return true;
//      }
//      else if(S.contains(b))
//          return true;
//  }
//  S.pop();
//  return false;
//}

//********************************************************************************
}
