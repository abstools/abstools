package deadlock.analyser.detection;

import deadlock.analyser.factory.GroupName;
import abs.frontend.ast.ASTNode;

//import deadlock.constraints.term.TermVariable;

//The Class Couple implement the structure used for a single dependency between two GroupName, it contains the TermVariable name and the flag that will
//be use in the State visit

//TODO ABEL: Review DONE (DELETE UNUSED CLASS)

public class Couple {
	
	GroupName v;
	Boolean coupleIsGet;
	Boolean visited;
	
	//used by get couple with node information
//        public Couple(GroupName a, Boolean b){
//                this.v = a;
//                this.visited = b;
//                this.coupleIsGet = true;
//        }
//	
	
	//used by await couple with node information
//        public Couple(GroupName a, Boolean b, Boolean await){
//                this.v = a;
//                this.visited = b;
//                this.coupleIsGet = false;
//        }
	
	
	
	//VarSubstitution (renaming) application
	public void apply(VarSubstitution s){
		this.v = s.apply(this.v);
	}
	
	//getter and setter method
	

	public GroupName getVar(){
		return this.v;
	}
	
	public Boolean getFlag(){
		return this.visited;
	}
	
	public Boolean isGet(){
		return this.coupleIsGet;
	}
	
	public void setGet(){
		this.coupleIsGet = true;
	}
	
	public void setFlag(){
		this.visited = true;
	}
	
	public void clearFlag(){
		this.visited = false;
	}

	//We use the convention that an await couple is printed in upperCase
	public String toString(){
		if(this.coupleIsGet) return v.toString();
		return v.toString().toUpperCase();
	}
}
