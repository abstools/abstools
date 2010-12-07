package abs.frontend.typechecker.locationtypes.infer;

import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sat4j.maxsat.WeightedMaxSatDecorator;
import org.sat4j.maxsat.reader.WDimacsReader;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.pb.IPBSolver;
import org.sat4j.reader.LecteurDimacs;
import org.sat4j.reader.ParseFormatException;
import org.sat4j.reader.Reader;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

import abs.frontend.analyser.TypeError;
import abs.frontend.typechecker.locationtypes.LocationType;

public class SatGenerator {
    final Set<Constraint> constraints;
    final Set<LocationTypeVariable> vars;
    
    final List<List<Integer>> output;
    
    final Environment e;
    
    public final static int MAX = 2147483647;
    
    
    
    public SatGenerator(Set<Constraint> constraints) {
        this.constraints = constraints;
        initializeConstraints();
        e = new Environment();
        output = new ArrayList<List<Integer>>();
        vars = new HashSet<LocationTypeVariable>();
    }
    
    private void initializeConstraints() {
        constraints.add(Constraint.declConstraint(LocationTypeVariable.ALWAYS_BOTTOM));
        constraints.add(Constraint.constConstraint(LocationTypeVariable.ALWAYS_BOTTOM, LocationType.BOTTOM));
        constraints.add(Constraint.declConstraint(LocationTypeVariable.ALWAYS_NEAR));
        constraints.add(Constraint.constConstraint(LocationTypeVariable.ALWAYS_NEAR, LocationType.NEAR));
        constraints.add(Constraint.declConstraint(LocationTypeVariable.ALWAYS_FAR));
        constraints.add(Constraint.constConstraint(LocationTypeVariable.ALWAYS_FAR, LocationType.FAR));
    }

    public Map<LocationTypeVariable, LocationType> generate() {
        Map<LocationTypeVariable, LocationType> tvl = new HashMap<LocationTypeVariable, LocationType>();
        for (Constraint c : constraints) {
            System.out.println(c);
            output.addAll(c.generateSat(e));
            c.variables(vars);
        }
        
        StringBuffer weights = new StringBuffer();
        for (LocationTypeVariable tv : vars) {
            weights.append(2);
            weights.append(" ");
            weights.append(e.get(tv, LocationType.NEAR));
            weights.append(" ");
            weights.append(e.get(tv, LocationType.FAR));
            weights.append(" ");
            weights.append("0\n");
        }
        
        StringBuffer sb = new StringBuffer();
        int nbclauses = output.size() + vars.size();
        int nbvars = e.current;
        sb.append("p wcnf ");
        sb.append(nbvars);
        sb.append(" ");
        sb.append(nbclauses);
        sb.append(" ");
        sb.append(MAX);
        sb.append("\n");
        
        for (List<Integer> line : output) {
            sb.append(MAX);
            sb.append(" ");
            for (Integer i : line) {
                sb.append(i);
                sb.append(" ");
            }
            sb.append("0\n");
        }
        
        sb.append(weights);
        
        
        
        System.out.println("Number of variables: " + nbvars);
        
        System.out.println("Number of clauses: " + nbclauses);
        
        //System.out.println(sb);
        
        IPBSolver solver = org.sat4j.maxsat.SolverFactory.newDefault();//instance().defaultSolver();
        
        //System.exit(0);
        
        WDimacsReader reader = new WDimacsReader(new WeightedMaxSatDecorator(solver));
        
        try {
            InputStream is = new ByteArrayInputStream(sb.toString().getBytes("UTF-8"));
            IProblem problem = reader.parseInstance(is);
            int[] model;
            if ((model = problem.findModel()) != null) {
                //int[] model = problem.model();
                //model = problem.findModel();
                System.out.println("Model generated");
                problem.printInfos(new PrintWriter(System.out), "INFO: ");
                System.out.println(Arrays.toString(model));
                for (int i : model) {
                    if (i > 0 && i <= nbvars) {
                        TypedVar tv = e.vars().get(i-1);
                        //System.out.println(tv.v + " : " + tv.t);
                        tvl.put(tv.v, tv.t);
                    }
                    
                }
            } else {
                throw new InferenceFailedException();
            }
            return tvl;
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ParseFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (ContradictionException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (TimeoutException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;
    }
    
}