package org.abs_models.backend.scala;

import java.util.Objects;

public class VarDefinition implements Comparable<VarDefinition> {

	private final String name;
	private final String type;

	public VarDefinition(String name, String type) {
		this.name = name;
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public String getType() {
		return type;
	}

	@Override
	public boolean equals(Object o) {
		if (o == null) {
			return false;
		}
		if (o == this) {
			return true;
		}
		if (o instanceof VarDefinition == false) {
			return false;
		}
		VarDefinition vd = (VarDefinition) o;
		return Objects.equals(this.name, vd.name);
		//return Objects.equals(this.name, vd.name) && Objects.equals(this.type, vd.type);
	}

	@Override
	public String toString() {
		return type + " " + name;
	}

	@Override
	public int compareTo(VarDefinition o) {
		// TODO Auto-generated method stub
		return name.compareTo(o.name);
	}

}
