package deadlock.analyser.factory;

import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.AwaitStmt;
import abs.frontend.ast.Stmt;
import abs.frontend.typechecker.DataTypeType;
import com.gzoumix.semisolver.substitution.Substitution;
import com.gzoumix.semisolver.term.Variable;
import com.gzoumix.semisolver.term.Term;
import com.gzoumix.semisolver.term.TermStructured;
import com.gzoumix.semisolver.term.TermVariable;
import com.gzoumix.semisolver.constraint.Constraint;


public class Factory extends com.gzoumix.semisolver.factory.Factory {

    private boolean verbose;
    private IRecord dummyDataType;

    /* Constructor */
    public Factory(boolean v) { verbose = v; dummyDataType = new RecordDataType(RecordDataType._prefix + "DUMMY", new LinkedList<Term>()); }

  
  /* 1. RECORDS */

  public RecordField newRecordField(String name, IRecord r) {
    RecordField res = new RecordField(name, r);
    return res;
  }

  public RecordPresent newRecordPresent(GroupName a, List<RecordField> fields){
    RecordPresent res = new RecordPresent(a, fields);
    return res;
  }

  public RecordFuture newRecordFuture(GroupName a, IRecord r) {
    RecordFuture res = new RecordFuture(a, r);
    return res;
  }

  public RecordDataType newRecordDataType(DataTypeType data, List<IRecord> r) {
    RecordDataType res = new RecordDataType(data,r);
    return res;
  }
  
  public GroupName newGroupName() {
    GroupName res = new GroupName(new Variable());
    return res;
  }
  
  public GroupName newGroupName(boolean isFresh) {
      GroupName res = new GroupName(new Variable(), isFresh);
      return res;
    }

  public RecordVariable newRecordVariable() {
    RecordVariable res = new RecordVariable(new Variable());
    return res;
  }

  
  
  /* 2. CONTRACTS */

  public Contract newContractEmpty() { return new Contract(); }

  public Contract newContractSequence(List<Contract> ce) {
    List<Term> l = new LinkedList<Term>();
    for(Contract contract: ce) { l.addAll(((TermStructured)contract).getSubTerms()); }
    return new Contract(l);
  }

  public Contract newContractAwait(ASTNode n, GroupName a, GroupName b) {
    List<Term> l = new ArrayList<Term>();
    l.add(new ContractElementAwait(n, a, b));
    return new Contract(l);
  }

  public Contract newContractGet(ASTNode n, GroupName a, GroupName b) {
    List<Term> l = new LinkedList<Term>();
    l.add(new ContractElementGet(n, a, b));
    return new Contract(l);
  }

  public Contract newContractInvk(ASTNode n, String nameClass, String nameMethod, MethodInterface mi) {
    List<Term> l = new LinkedList<Term>();
    l.add(new ContractElementInvk(n, nameClass, nameMethod, mi));
    return new Contract(l);
  }

  public Contract newContractSyncInvk(ASTNode n, String nameClass, String nameMethod, MethodInterface mi) {
    List<Term> l = new LinkedList<Term>();
    l.add(new ContractElementSyncInvk(n, nameClass, nameMethod, mi));
    return new Contract(l);
  }

  public Contract newContractInvkA(ASTNode n, ContractElementInvk i, ContractElementAwait a) {
    List<Term> l = new LinkedList<Term>();
    l.add(new ContractElementInvkA(n, i, a));
    return new Contract(l);
  }

  public Contract newContractInvkG(ASTNode n, ContractElementInvk i, ContractElementGet g) {
    List<Term> l = new LinkedList<Term>();
    l.add(new ContractElementInvkG(n, i, g));
    return new Contract(l);
  }

  public Contract newContractUnion(ASTNode n,  Contract c1, Contract c2) {
    List<Term> l = new LinkedList<Term>();
    l.add(new ContractElementUnion(n, c1, c2));
    return new Contract(l);
  }

  public Contract newContractParallel(ASTNode n,  List<Contract> l) {
      List<Term> l1 = new LinkedList<Term>();
      l1.add(new ContractElementParallel(n, l));
      return new Contract(l1);
    }

  /* 4. Methods */

  public MethodInterface newMethodInterface(IRecord r, List<IRecord> s, IRecord res){
    return new MethodInterface(r, s, res);
  }
	
  public MethodContract newMethodContract(MethodInterface mi, Contract cp,Contract cf){
    return new MethodContract(mi, cp, cf);
  }



  /* 5. Reimplementation of Generic News */

  public TermVariable freshTermVariableFromTerm(Term t) {
    if(t instanceof GroupName) { return newGroupName(); }
    else if(t instanceof IRecord) { return newRecordVariable(); }   
    else{ System.out.println("WHAT THE HELL !!!???"); return super.freshTermVariableFromTerm(t); }
  }

  public Term newTerm(String c, List<Term> l) {
      if(c.equals(Contract.name)) { return new Contract(l); }
      else if(c.equals(ContractElementAwait.name)) { return new ContractElementAwait(l); }
      else if(c.equals(ContractElementGet.name)) { return new ContractElementGet(l); }
      else if(c.startsWith(ContractElementInvk.prefix)) { return new ContractElementInvk(c, l); }
      else if(c.equals(ContractElementInvkA.name)) { return new ContractElementInvkA(l); }
      else if(c.equals(ContractElementInvkG.name)) { return new ContractElementInvkG(l); }
      else if(c.equals(ContractElementParallel.name)) { return new ContractElementParallel(l); }
      else if(c.startsWith(ContractElementSyncInvk.prefix)) { return new ContractElementSyncInvk(c, l); }
      else if(c.equals(ContractElementUnion.name)) {  return new ContractElementUnion(l); }
      else if(c.equals(FunctionInterface.name)) { return new FunctionInterface(l); }
      else if(c.equals(MethodContract.name)) {  return new MethodContract(l); }
      else if(c.equals(MethodInterface.name)) {  return new MethodInterface(l); }
      else if(c.startsWith(RecordDataType._prefix)) { return new RecordDataType(c, l); }
      else if(c.startsWith(RecordField.prefix)) { return new RecordField(c, l); }
      else if(c.equals(RecordFuture.name)) { return new RecordFuture(l); }
      else if(c.equals(RecordPresent.name)) { return new RecordPresent(l); }
      else {return super.newTerm(c, l); } // should never occur
  }

  public Constraint newConstraint() {
    Constraint res = super.newConstraint();
    if(verbose) { res.setDebugFile(System.out); }
    return res;
  }


/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////


  /* Tool for substitution? *//*
  public Term freshTerm(Term t){// TODO give a better semantics or name
    Map<Variable,Term> mapFresh = new HashMap<Variable,Term>();
    for(Variable var : t.fv()){
      mapFresh.put(var, this.freshTermVariable());
    }
    Substitution s = new Substitution(this, mapFresh);
    return s.apply(t);
  }*/


/*

  //
  public Term accumulateContract(Term c, Term cGetorAwait){
		//If c is null then return cGetorAwait
		if(c == null) return cGetorAwait;
		//If cGetorAwait is null return c
		if(cGetorAwait == null) return c;

		
		//Case cGetorAwait is a ContractGet
		if((((TermStructured) cGetorAwait).getConstructor()).equals("ContractGet")){
			//Case c is a ContractInvk
			if((((TermStructured) c).getConstructor()).startsWith("Class")) return newContractGInvk(c,cGetorAwait);
			//Case c is a ContractSeq
			else if((((TermStructured) c).getConstructor()).equals("ContractSeq")){
				//We analyze the last contract of the ContractSeq
				Term lastTerm;
				List<Term> subs = ((TermStructured) c).getSubTerms();
				lastTerm = subs.get(subs.size() - 1);
				//Case LastContract is not a ContractInvk
				if(!(((TermStructured) lastTerm).getConstructor()).startsWith("Class")){
					subs.add(cGetorAwait);
					//ther's no concatenation and we add together c and cGetorAwait 
					return newContractSequence(subs);
				}
				//Case LastContract is a ContractInvk
				else {
					//We replace LastTerm and create a new ContractSeq
					lastTerm = newContractGInvk(lastTerm,cGetorAwait);
					subs.remove(subs.size() - 1);
					subs.add(lastTerm);
					return newContractSequence(subs);
				}
			}
			//case c is not ContractInvk and not a ContracSeq
			else{
				//we add together c and cGetorAwait
				List<Term> t = new LinkedList<Term>();
				t.add(c);
				t.add(cGetorAwait);
				return newContractSequence(t);
			}
		//similar to the first case, this time we have ContractAwait and we replace newContractGInvk with newContractAInvk
		} else if((((TermStructured) cGetorAwait).getConstructor()).equals("ContractAwait")){
			if((((TermStructured) c).getConstructor()).startsWith("Class")) return newContractAInvk(c,cGetorAwait);
			else if((((TermStructured) c).getConstructor()).equals("ContractSeq")){
				Term lastTerm;
				List<Term> subs = ((TermStructured) c).getSubTerms();
				lastTerm = subs.get(subs.size() - 1);
				if(!(((TermStructured) lastTerm).getConstructor()).startsWith("Class")){
					subs.add(cGetorAwait);
					return newContractSequence(subs);
				}
				else {
					lastTerm = newContractAInvk(lastTerm,cGetorAwait);
					subs.remove(subs.size() - 1);
					subs.add(lastTerm);
					return newContractSequence(subs);
				}
			} else{
				List<Term> t = new LinkedList<Term>();
				t.add(c);
				t.add(cGetorAwait);
				return newContractSequence(t);
			}	
		//last case where cGetorAwait is neither a get or an await contract and we add it to c
		} else return addContract(c,cGetorAwait);
	}
	
*/
	public Term cleanContractMethod(Term cm){
		//this method has sense only if Term cm is a MethodContract
		if(((TermStructured) cm).getConstructor().equals("MethodContract")){
			//I recover the 2 main term inside the MethodContract, in particular, the contract c
			Term mi = ((TermStructured) cm).getSubTerms().get(0);
			Term c = ((TermStructured) cm).getSubTerms().get(1);
			
			//if c is a contractSeq we need to control it
			if(((TermStructured) c).getConstructor().equals("ContractSeq")){
				//but only if the contractSeq contains 2 or more contract we need to analyze it
				if(((TermStructured) c).getSubTerms().size() > 1){
					List<Term> cCopy = new LinkedList<Term>();
					Iterator<Term> i = ((TermStructured) c).getSubTerms().iterator();
					while(i.hasNext()){
						Term j=i.next();
						if(((TermStructured) j).getConstructor().equals("ContractAInvk")){
							Term cAwait = ((TermStructured) j).getSubTerms().get(1);
							Term v1 = ((TermStructured) cAwait).getSubTerms().get(0);
							Term v2 = ((TermStructured) cAwait).getSubTerms().get(1);
							if(i.hasNext()){
								Term j2=i.next();
								if(((TermStructured) j2).getConstructor().equals("ContractGet")){
									Term v12 = ((TermStructured) j2).getSubTerms().get(0);
									Term v22 = ((TermStructured) j2).getSubTerms().get(1);
									if(v1.toString().equals(v12.toString()) && v2.toString().equals(v22.toString())){
										cCopy.add(j);
									}else{
										cCopy.add(j);
										cCopy.add(j2);
									}
								}else{
									cCopy.add(j);
									cCopy.add(j2);
								}
							}else{
								cCopy.add(j);
							}
						}else{
							cCopy.add(j);
						}
					}
					List<Term> newMc = new LinkedList<Term>();
					newMc.add(mi);
					newMc.add(newTerm("ContractSeq", cCopy));
					return newTerm("MethodContract",newMc);
					
				} else return cm;
			} else return cm;
			
		} else if(((TermStructured) cm).getConstructor().equals("ContractSeq")){
			//but only if the contractSeq contains 2 or more contract we need to analyze it
			if(((TermStructured) cm).getSubTerms().size() > 1){
				List<Term> cCopy = new LinkedList<Term>();
				Iterator<Term> i = ((TermStructured) cm).getSubTerms().iterator();
				while(i.hasNext()){
					Term j=i.next();
					if(((TermStructured) j).getConstructor().equals("ContractAInvk")){
						Term cAwait = ((TermStructured) j).getSubTerms().get(1);
						Term v1 = ((TermStructured) cAwait).getSubTerms().get(0);
						Term v2 = ((TermStructured) cAwait).getSubTerms().get(1);
						if(i.hasNext()){
							Term j2=i.next();
							if(((TermStructured) j2).getConstructor().equals("ContractGet")){
								Term v12 = ((TermStructured) j2).getSubTerms().get(0);
								Term v22 = ((TermStructured) j2).getSubTerms().get(1);
								if(v1.toString().equals(v12.toString()) && v2.toString().equals(v22.toString())){
									cCopy.add(j);
								}else{
									cCopy.add(j);
									cCopy.add(j2);
								}
							}else{
								cCopy.add(j);
								cCopy.add(j2);
							}
						}else{
							cCopy.add(j);
						}
					}else{
						cCopy.add(j);
					}
				}
				return newTerm("ContractSeq", cCopy);	
			} else return cm;
		}else return cm;

	}


	

    public ContractElementParallel newContractElementParallel(Contract c1, ContractElementParallel c2) {
        
        c2.getSubTerms().add(c1);
        return c2;
        
    }


    public Contract newContract(ContractElement e) {
        return new Contract(e);
    }


    public ITypingEnvironmentVariableType dummyDataType() {
        //return dummyDataType;
        return this.newRecordVariable();
    }
	

} // end class DeadlockFactory
