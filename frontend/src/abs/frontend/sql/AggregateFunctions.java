/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.frontend.sql;

import java.util.HashMap;
import java.util.Map;

import abs.common.Constants;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.List;
import abs.frontend.ast.PureExp;
import abs.frontend.ast.SqlAggregateFunction;
import abs.frontend.ast.SqlTupleConstant;
import abs.frontend.ast.SqlTupleScalarFunction;

/**
 * Encapsulates supported aggregate functions. Maps names of aggregate functions as used in the SQL language
 * extension to {@link FunctionExpGenerator}s which support generating pure expressions according to
 * the functions which can be used in the transformed abstract syntax tree.
 * 
 * @author Marko Martin
 * @see SqlAggregateFunction#createDbAggregateFunctionExp()
 */
public class AggregateFunctions {
    
    public static String[][] UNARY_FUNCTIONS = { // Maps function identifiers in the language extension to constructor names.
        {"max", "Maximum"},
        {"min", "Minimum"},
        {"sum", "AggregateSum"},
        {"avg", "Average"}};
    
    public interface FunctionExpGenerator {
        public PureExp createDbAggregateFunctionExp(SqlTupleScalarFunction tupleScalarFunction) throws abs.frontend.sql.SqlRewritingException;
    }
    
    private static abstract class AbstractFunctionExpGenerator implements FunctionExpGenerator {
        
        private String name;
        
        public AbstractFunctionExpGenerator(String name) {
            this.name = name;
        }
        
        @Override
        public PureExp createDbAggregateFunctionExp(SqlTupleScalarFunction tupleScalarFunction) throws abs.frontend.sql.SqlRewritingException {
            return new DataConstructorExp(
                    Constants.DB_OPERATORS_STRUCTURE_LIB_NAME + "." + name,
                    createConstructorArguments(tupleScalarFunction));
        }
        
        /**
         * @param tupleScalarFunction
         * @return The default implementation returns an empty list.
         */
        protected List<PureExp> createConstructorArguments(SqlTupleScalarFunction tupleScalarFunction) throws abs.frontend.sql.SqlRewritingException {
            return new List<PureExp>();
        }
        
    }
    
    private static class NullaryFunctionExpGenerator extends AbstractFunctionExpGenerator {
        
        public NullaryFunctionExpGenerator(String name) {
            super(name);
        }
        
    }
    
    private static class UnaryFunctionExpGenerator extends AbstractFunctionExpGenerator {
        
        public UnaryFunctionExpGenerator(String name) {
            super(name);
        }

        @Override
        protected List<PureExp> createConstructorArguments(SqlTupleScalarFunction tupleScalarFunction) throws abs.frontend.sql.SqlRewritingException {
            List<PureExp> arguments = super.createConstructorArguments(tupleScalarFunction);
            return arguments.add(tupleScalarFunction.createDbTupleScalarFunctionExp());
        }
        
    }

    private static Map<String, AbstractFunctionExpGenerator> functionExpGeneratorsMap = new HashMap<String, AbstractFunctionExpGenerator>();
    
    static {
        functionExpGeneratorsMap.put("count", new NullaryFunctionExpGenerator("Count"));
        functionExpGeneratorsMap.put("unique", new UnaryFunctionExpGenerator("Unique") {
            @Override
            public PureExp createDbAggregateFunctionExp(SqlTupleScalarFunction tupleScalarFunction)
                    throws SqlRewritingException {
                if (tupleScalarFunction instanceof SqlTupleConstant)
                    return new DataConstructorExp(
                        Constants.DB_OPERATORS_STRUCTURE_LIB_NAME + ".AggregateConstant",
                        new List<PureExp>().add(((SqlTupleConstant) tupleScalarFunction).createDbValueExp()));
                return super.createDbAggregateFunctionExp(tupleScalarFunction);
            }
        });
        for (String[] function : UNARY_FUNCTIONS)
            functionExpGeneratorsMap.put(function[0], new UnaryFunctionExpGenerator(function[1]));
    }
    
    public static FunctionExpGenerator getExpGeneratorForFunctionName(String name) {
        return functionExpGeneratorsMap.get(name);
    }
    
}
