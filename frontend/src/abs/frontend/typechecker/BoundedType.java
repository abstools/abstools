package abs.frontend.typechecker;

public class BoundedType extends Type {
    private Type boundType;

    public BoundedType() {

    }

    public void bindTo(Type t) {
        boundType = t;
    }

    public Type getBoundType() {
        return boundType;
    }

    @Override
    public Object getMetaData(Object key) {
        if (hasBoundType()) {
            return boundType.getMetaData(key);
        }
        return super.getMetaData(key);
    }
    
    @Override
    public void addMetaData(Object key, Object value) {
        if (hasBoundType()) {
            boundType.addMetaData(key,value);
        } else  {
            super.addMetaData(key, value);
        }
    }
    
    @Override
    public boolean isAssignable(Type t) {
        if (hasBoundType())
            return boundType.isAssignable(t);
        boundType = t;
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof Type))
            return false;

        if (this == o)
            return true;

        if (hasBoundType())
            return boundType.equals(o);

        return true;
    }

    @Override
    public boolean isBoundedType() {
        return true;
    }

    public boolean hasBoundType() {
        return boundType != null;
    }

    @Override
    public String toString() {
        if (hasBoundType()) {
            return boundType.toString();
        }
        return "Unbound Type";
    }
    
    @Override
    public String getSimpleName() {
        if (hasBoundType()) {
            return boundType.getSimpleName();
        }
        return "Unbound Type";
    }

    @Override
    public boolean canBeBoundTo(Type t) {
        if (!hasBoundType()) {
            boundType = t;
            return true;
        }
        return false;
    }

    @Override
    public Type copy() {
        BoundedType copy = new BoundedType();
        copy.boundType = boundType;
        return boundType;
    }
    
    public Type fullCopy() {
        if (hasBoundType()) {
            BoundedType copy = (BoundedType) copy();
            copy.boundType.metaData.putAll(boundType.metaData);
            return copy;
        } else
            return super.fullCopy();
    }

}
