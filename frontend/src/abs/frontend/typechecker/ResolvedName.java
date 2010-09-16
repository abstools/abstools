package abs.frontend.typechecker;

public abstract class ResolvedName {
	public abstract String getQualifiedString();
	
	public abstract ResolvedModuleName getModuleName();
	
	public String getSimpleName() {
		String s = getQualifiedString();
		return s.substring(s.lastIndexOf('.')+1);
	}
	
	public boolean isModuleName() {
		return false;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || !(obj instanceof ResolvedName))
			return false;
		return ((ResolvedName)obj).getQualifiedString().equals(getQualifiedString());
	}
	
	@Override
	public int hashCode() {
		return getQualifiedString().hashCode();
	}
}
