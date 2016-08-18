package deadlock.analyser.detection;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import deadlock.analyser.factory.GroupName;
import deadlock.analyser.factory.GroupNameUnique;

//Class State implement the structure that contains a "list of couple (a,b)", this list is implement like an HashMap, the key is a TermVariable and the value is a 
//list of other TermVariable which appear in the right side of a couple dependency.



public class State {

    //hashmaps storing dependency couples
    HashMap<GroupName, HashSet<GroupName>> depCouple;
    HashMap<GroupName, HashSet<GroupName>> depCoupleAwait;
    
    DebugInfo di = new DebugInfo();
    
    public DebugInfo getDebugInfo(){
        return di;
    }
    
    public void setDebugInfo(DebugInfo di){
        this.di = di;
    }
    
    public State(){
        this.depCouple = new HashMap<GroupName, HashSet<GroupName>>();
        this.depCoupleAwait = new HashMap<GroupName, HashSet<GroupName>>(); 
    }

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
   
    }

    //Method for add entire State s to this State, useful for parallel operation
    public void addState(State s){
        
        for(GroupName a : s.depCouple.keySet())
            for(GroupName b: s.depCouple.get(a))
                addCouple(a, b);
        
        for(GroupName a : s.depCoupleAwait.keySet())
            for(GroupName b: s.depCoupleAwait.get(a))
                addCoupleAwait(a, b);
   
    }

    //Method to know if a state is contained in this state
    public Boolean containState(State s){
        
        for(GroupName a : s.depCouple.keySet())
            for(GroupName b: s.depCouple.get(a))
                if(!containsCoupleGet(a, b))
                    return false;
        
        for(GroupName a : s.depCoupleAwait.keySet())
            for(GroupName b: s.depCoupleAwait.get(a))
                if(!containsCoupleAwait(a, b))
                    return false;
        
        return true;
    }

    //Method to know if a couple get is contained in this state
        private boolean containsCoupleGet(GroupName a, GroupName b) {
        HashSet<GroupName> aGroup;
        return ((aGroup = depCouple.get(a)) != null && aGroup.contains(b)) ;
    }
    
    //Method to know if a couple await is contained in this state
    private boolean containsCoupleAwait(GroupName a, GroupName b) {
        HashSet<GroupName> aGroup;
        return ((aGroup = depCoupleAwait.get(a)) != null && aGroup.contains(b)) ;
    }

    //Method to know if a couple is contained in this state
    public Boolean containsCouple(GroupName a, GroupName b){
        HashSet<GroupName> aGroup, aGroupAwait;
        return ((aGroup = depCouple.get(a)) != null && aGroup.contains(b)) || ((aGroupAwait = depCoupleAwait.get(a)) != null && aGroupAwait.contains(b));
    }

    //VarSubstitution (renaming) application
    public void apply(VarSubstitution s){
        
        depCouple = refreshHashMap(s, depCouple);
        depCoupleAwait = refreshHashMap(s, depCoupleAwait);
        
    }

    //performs a variable name substitution in dependency couples hashmap, returns new instance with new names
    private static HashMap<GroupName, HashSet<GroupName>> refreshHashMap(VarSubstitution s, HashMap<GroupName, HashSet<GroupName>> toRefresh) {
        
        HashMap<GroupName, HashSet<GroupName>> temp = new HashMap<GroupName, HashSet<GroupName>>(toRefresh.size());
        
        for(GroupName a: toRefresh.keySet())
        {
            GroupName key = (a instanceof GroupNameUnique)? a: s.apply(a);
                      
            if(!temp.containsKey(key)){
              temp.put(key, new HashSet<GroupName>());
            }
            
            for (GroupName b : toRefresh.get(a)){
                temp.get(key).add((b instanceof GroupNameUnique)? b: s.apply(b));
            }
            
        }
        
        return temp;
    }

    //Calculates de number of dependencies
    public int numberOfDep(){
        int i = 0;
        for(GroupName v : depCouple.keySet())
           i+= depCouple.get(v).size();
        for(GroupName v : depCoupleAwait.keySet())
            i+= depCoupleAwait.get(v).size();
        return i;
    }

    //calculates all the variables of the state
    //TODO ABEL: check correctness
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

    //Determines if there is a pure get dependencies cycle (Deadlock)
    public boolean hasCycleGet()
    {
        return hasCycle(depCouple, depCouple);
    }
    
    //Determines if there is a pure await dependencies cycle (Livelock)
    public boolean hasCycleAwait()
    {
        return hasCycle(depCouple, depCoupleAwait);
    }
    
    //Determines if there is a cycle combining both kind of dependencies (Lock)
    public boolean hasCycle()
    {
        //this doesn't work because there can be a deadlock combining dependencies from both kinds
        //return hasCycle = HasCycleGet() || HasCycleAwait();
        
        //this is the correct approach, to create the Union Graph and then check for cycles
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
        
        return hasCycle(depCouple, allTogether);
    }
    
    //Checks for a cycle in a dependency couples hashmap
    private static boolean hasCycle(HashMap<GroupName, HashSet<GroupName>> depCouple, HashMap<GroupName, HashSet<GroupName>> graph)
    {
        HashSet<GroupName> visited = new HashSet<GroupName>();
        //HashSet<GroupName> recorded = new HashSet<GroupName>();
        
        ArrayList<GroupName> recorded = new ArrayList<GroupName>();
        
        for(GroupName a : graph.keySet())
        {
            if(hasCycleUtil(graph, recorded, visited, a))
                if(reviewCycle(depCouple, recorded))
                    return true;
        }
            
        
        return false;
    }
    
    private static boolean reviewCycle(HashMap<GroupName, HashSet<GroupName>> depCouple, ArrayList<GroupName> recorded) {
        // TODO Auto-generated method stub
        for(int i = 0; i < recorded.size(); i++)
        {
            GroupName a = recorded.get(i);
            GroupName b = recorded.get((i + 1) % recorded.size());
            
            if(depCouple.containsKey(a) && depCouple.get(a).contains(b))
                return true;
        }
        
        return false;
    }

    //recursive method for cycling detection
    private static boolean hasCycleUtil(HashMap<GroupName, HashSet<GroupName>> graph, ArrayList<GroupName> recorded, HashSet<GroupName> visited, GroupName current)
    {
       //this method performs a classic Breath First Search to check for cycles in an undirected graph and
       //keeps tracks of ancestors to determine if the closing edge is in deed a back edge
       if(!visited.contains(current))
       {
           visited.add(current);
           recorded.add(current);
           
           if(graph.containsKey(current)){
               for(GroupName b : graph.get(current))
               {
                   if(!visited.contains(b) && hasCycleUtil(graph, recorded, visited, b))
                       return true;
                   if(recorded.contains(b))
                       return true;
               }
           }
           
           recorded.remove(current);
       }
       
       return false;
        
    }
    
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

//    public void expandAndClean() {
//        // TODO Auto-generated method stub
//        expandAndClean();
//        
//        //depCoupleAwait = cleanHashMap(depCoupleAwait);
//    }
    
   
    
    //TODO ABEL: this method can be more efficient
    public void expandAndClean(){
        HashSet<GroupName> vertexes = new HashSet<GroupName>();
        
        //get all vertex
        for(GroupName v : depCouple.keySet())
        {
            vertexes.add(v);
            for(GroupName v2 : depCouple.get(v))
               vertexes.add(v2);
        }
        
        for(GroupName v : depCoupleAwait.keySet())
        {                     
            vertexes.add(v);
            for(GroupName v2 : depCoupleAwait.get(v))
               vertexes.add(v2);
            
        }
        
        //Floyd Warshall Algorithm
        GroupName [] vertexMap = vertexes.toArray(new GroupName[0] );
        
        //initialize graph
        int [][] graph = new int[vertexMap.length][];
        
        //get all edges
        for (int i = 0; i < vertexMap.length; i++){
            graph[i] = new int[vertexMap.length];
            for (int j = 0; j < vertexMap.length; j++)
                graph[i][j] = (depCouple.containsKey(vertexMap[i]) && depCouple.get(vertexMap[i]).contains(vertexMap[j])) ? 2: ((depCoupleAwait.containsKey(vertexMap[i]) && depCoupleAwait.get(vertexMap[i]).contains(vertexMap[j]))? 1: 0);
        }
        
        //calculate transitive closure
        for (int k = 0; k < vertexMap.length; k++)
            for (int i = 0; i < vertexMap.length; i++)
                for (int j = 0; j < vertexMap.length; j++){
                    graph[i][j] = Math.max(graph[i][j],  (graph[i][k] * graph[k][j]));
                    if(graph[i][j] > 2) graph[i][j] = 2; //normalize to avoid overflow
                }
        
        //clean existing
        depCouple = new HashMap<GroupName, HashSet<GroupName>>();
        depCoupleAwait = new HashMap<GroupName, HashSet<GroupName>>();
        
        //add new couples
        for (int i = 0; i < vertexMap.length; i++)
            for (int j = 0; j < vertexMap.length; j++)
                if(graph[i][j] > 0) {
                    if((!vertexMap[i].isFresh && !vertexMap[j].isFresh)){
                        if(graph[i][j] > 1)//is a get couple
                            this.addCouple(vertexMap[i], vertexMap[j]); 
                        else //is an await couple
                            this.addCoupleAwait(vertexMap[i], vertexMap[j]);
                    }                    
                    else if  (vertexMap[i].isFresh && vertexMap[j].isFresh && i == j){
                        if(graph[i][j] > 1)//is a get couple
                            this.addCouple(GroupNameUnique.GetInstance(), GroupNameUnique.GetInstance()); 
                        else //is an await couple
                            this.addCoupleAwait(GroupNameUnique.GetInstance(), GroupNameUnique.GetInstance());                        
                    }                    
                }
        
    }

    public boolean hasReflexiveState() {
        // TODO Auto-generated method stub
        for(GroupName a : depCouple.keySet())
            if(depCouple.get(a).contains(a))
                return true;
        
        return false;
    }
}


