package org.abs_models.frontend.variablechecker;

import org.abs_models.frontend.ast.*;

public class ApplySimplifier {
    public static AppCond and(AppCond left, AppCond right){
        if(left instanceof AppCondTrue)
            return right;
        if(right instanceof AppCondTrue)
            return left;
        if(right instanceof AppCondFalse || left instanceof AppCondFalse )
            return new AppCondFalse();
        return new AppCondAnd(left, right);
    }
    public static AppCond or(AppCond left, AppCond right){
        if(left instanceof AppCondFalse)
            return right;
        if(right instanceof AppCondFalse)
            return left;
        if(right instanceof AppCondTrue || left instanceof AppCondTrue )
            return new AppCondTrue();
        return new AppCondOr(left, right);

    }
    public static AppCond not(AppCond inner){
        if(inner instanceof AppCondTrue)
            return new AppCondFalse();
        if(inner instanceof AppCondFalse)
            return new AppCondTrue();
        return new AppCondNot(inner.treeCopyNoTransform());
    }
}
