package com.fmse.abs.orm;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.ResultSet;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

import java.util.List; 
import java.util.Iterator;

public class AbsDbOrm  {
	public ABS.StdLib.List<abs.backend.java.lib.types.ABSValue> findAllByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString condition) {
        try {
            ABS.StdLib.List<abs.backend.java.lib.types.ABSValue> listObject = new ABS.StdLib.List_Nil();
            Class c = Class.forName(className.getString());
            Object object = c.newInstance();

            AbsJavaReflection reflection = AbsJavaReflection.createReflection(object);
            ResultSet rs = this.select(reflection, condition.getString());

            while(rs.next()) {
                object = c.newInstance();
                reflection = AbsJavaReflection.createReflection(object);
                this.createObjectFromResult(rs, reflection);
                listObject = ABS.StdLib.appendright_f.apply(listObject, (abs.backend.java.lib.types.ABSValue) object);
            }

            return listObject;
        } catch(Exception ex) {
            ex.printStackTrace();
        }
        return null;
	}

	public ABS.StdLib.List<abs.backend.java.lib.types.ABSValue> findAll(abs.backend.java.lib.types.ABSString className) {
		return this.findAllByAttributes(className, abs.backend.java.lib.types.ABSString.fromString(""));
	}

	public abs.backend.java.lib.runtime.ABSObject findByAttributes(abs.backend.java.lib.types.ABSString className, abs.backend.java.lib.types.ABSString condition) {
		try {
			Class c = Class.forName(className.getString());
			Object object = c.newInstance();
			
			AbsJavaReflection reflection = AbsJavaReflection.createReflection(object);
			ResultSet rs = this.select(reflection, condition.getString());
            if(rs.next()) {
                this.createObjectFromResult(rs, reflection);
            }

            if(object == null) {
                return null;
            } else {
                return (abs.backend.java.lib.runtime.ABSObject) object;
            }
		} catch(Exception ex) {
			ex.printStackTrace();
		}
		return null;
	}

	public abs.backend.java.lib.runtime.ABSObject find(abs.backend.java.lib.types.ABSString className) {
		return this.findByAttributes(className, abs.backend.java.lib.types.ABSString.fromString(""));
	}

	public abs.backend.java.lib.types.ABSUnit save(abs.backend.java.lib.runtime.ABSObject object) {
		AbsJavaReflection reflection = AbsJavaReflection.createReflection(object);
		String className = reflection.getClassName();
		Map<String, String> attributes = new HashMap<String, String>();
		
		for(Method m : reflection.getGetterMethod()) {
			String methodName = m.getName();
			String fieldName = this.createFieldName(methodName);
			String value = reflection.getStringValueFromInvokeMethod(m);

			if(value != null) {
				attributes.put(fieldName, value);
			}
		}
		
		AbsSqlCommandBuilder command = AbsSqlCommandBuilder.getCommandBuilder();
		String query = command.insert(className, attributes);
		AbsJdbcTransaction transaction = AbsJdbcTransaction.createTransaction();
		transaction.createUpdateStatement(query);
		return null;
	}

	public abs.backend.java.lib.types.ABSUnit delete(abs.backend.java.lib.runtime.ABSObject object) {
        AbsJavaReflection reflection = AbsJavaReflection.createReflection(object);
        String className = reflection.getClassName();
		AbsJdbcTransaction transaction = AbsJdbcTransaction.createTransaction();
        String primaryKeyName = transaction.getPrimaryKeyName(className);

        Map<String, String> condition = new HashMap<String, String>();
        for(Method m : reflection.getGetterMethod()) {
            String methodName = m.getName();
            String fieldName = this.createFieldName(methodName);
            if(fieldName.equals(primaryKeyName)) {
                String value = reflection.getStringValueFromInvokeMethod(m);
                condition.put(fieldName, value);
                AbsSqlCommandBuilder command = AbsSqlCommandBuilder.getCommandBuilder();
                String query = command.delete(className, condition);
                transaction.createUpdateStatement(query);
                object = null;
                break;
            }
        }
        return null;
	}

	public abs.backend.java.lib.runtime.ABSObject update(abs.backend.java.lib.runtime.ABSObject object) {
        AbsJavaReflection reflection = AbsJavaReflection.createReflection(object);
        String className = reflection.getClassName();
        AbsJdbcTransaction transaction = AbsJdbcTransaction.createTransaction();
        String primaryKeyName = transaction.getPrimaryKeyName(className);

        Map<String, String> attributes = new HashMap<String, String>();
        Map<String, String> condition = new HashMap<String, String>();
        for(Method m : reflection.getGetterMethod()) {
            String methodName = m.getName();
            String fieldName = this.createFieldName(methodName);
            String value = reflection.getStringValueFromInvokeMethod(m);
            if(fieldName.equals(primaryKeyName)) {
                condition.put(fieldName, value);
            } else {
                attributes.put(fieldName, value);
            }
        }

        AbsSqlCommandBuilder command = AbsSqlCommandBuilder.getCommandBuilder();
        String query = command.update(className, attributes, condition);
        transaction.createUpdateStatement(query);

        return null;
    }

	private ResultSet select(AbsJavaReflection reflection, String condition) {
		String tableName = reflection.getClassName();
		
		AbsSqlCommandBuilder command = AbsSqlCommandBuilder.getCommandBuilder();
		List<String> conditionList = null;
		if(!condition.equals("")) {
			conditionList = this.createListCondition(condition);
		}
		String query = command.select(tableName, conditionList);
		return AbsJdbcTransaction.createTransaction().createQueryStatement(query);
	}
	
	private String createFieldName(String methodName) {
		String methodNameWithoutPrefix = methodName.substring(3, methodName.length());
		return new StringBuilder().append(methodNameWithoutPrefix.substring(0,1).toLowerCase())
							.append(methodNameWithoutPrefix.substring(1,methodNameWithoutPrefix.length()))
							.toString();
	}
	
	private void createObjectFromResult(ResultSet result, AbsJavaReflection reflection) {
		try {
			if(result != null) {
				for(Method m : reflection.getSetterMethod()) {
					String methodName = m.getName();
					String fieldName = this.createFieldName(methodName);
					
					String return_type = m.getReturnType().toString();
					String value = result.getString(fieldName);
					String parameterType = reflection.getParameterType(m);
					if(parameterType.equals(AbsJavaReflection.ABSStringClassName)) {
						reflection.invokeMethod(m, (Object) abs.backend.java.lib.types.ABSString.fromString(value));
					} else if(parameterType.equals(AbsJavaReflection.ABSIntegerClassName)) {
						reflection.invokeMethod(m, (Object) abs.backend.java.lib.types.ABSInteger.fromString(value));
					} else if(parameterType.equals(AbsJavaReflection.ABSRationalClassName)) {
						reflection.invokeMethod(m, (Object) abs.backend.java.lib.types.ABSRational.fromString(value));
					}
				}
			}
		} catch (SQLException ex) {
			ex.printStackTrace();
		}
	}
	
	private List<String> createListCondition(String condition) {
		List<String> list = new ArrayList<String>();
		for(String c : condition.split(",")) {
			list.add(c);
		}
		return list;
	}
}

class AbsJdbcTransaction {
	private static final String url = "jdbc:mysql://localhost:3306/";
    private static final String dbname = "abs_studikasus";
    private static final String driver = "com.mysql.jdbc.Driver";
    private static final String username = "root";
    private static final String password = "";
	private Connection conn = null;
	
	private AbsJdbcTransaction() {}
	
	public static AbsJdbcTransaction createTransaction() {
		return new AbsJdbcTransaction();
	}
	
	public void createUpdateStatement(String query) {
        //System.out.println(query);
		try {
            Class.forName(driver).newInstance();
            Connection conn = this.getConnection();
			Statement stat = conn.createStatement();
            stat.executeUpdate(query);
		} catch (Exception ex) {
			ex.printStackTrace();
        }
	}
	
	public ResultSet createQueryStatement(String query) {
        //System.out.println(query);
		try {
            Class.forName(AbsJdbcTransaction.driver).newInstance();
            Connection conn = this.getConnection();
			Statement stat = conn.createStatement();
            return stat.executeQuery(query);
		} catch (Exception ex) {
			ex.printStackTrace();
        }
		return null;
	}
	
	private Connection getConnection() {
		try {
			return DriverManager.getConnection(
				AbsJdbcTransaction.url + AbsJdbcTransaction.dbname, 
				AbsJdbcTransaction.username, 
				AbsJdbcTransaction.password);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return null;
	}

    public String getPrimaryKeyName(String tableName) {
        try {
            ResultSet rs = null;
            Connection conn = this.getConnection();
            DatabaseMetaData meta = conn.getMetaData();
            rs = meta.getPrimaryKeys(null, null, tableName);
            rs.next();
            return rs.getString("COLUMN_NAME");
        } catch(SQLException ex) {
            ex.printStackTrace();
        }
        return "";
    }
}
	
class AbsJavaReflection {
    public static final String ABSStringClassName = "class abs.backend.java.lib.types.ABSString";
    public static final String ABSIntegerClassName = "class abs.backend.java.lib.types.ABSInteger";
    public static final String ABSRationalClassName = "class abs.backend.java.lib.types.ABSRational";
    public static final String ABSUnitClassName = "class abs.backend.java.lib.types.ABSUnit";

	private Object object;
	
	private AbsJavaReflection(Object object) {
		this.object = object;
	}
	
	public static AbsJavaReflection createReflection(Object object) {
		return new AbsJavaReflection(object);
	}
	
	public String getClassName() {
		Class<?> c = this.object.getClass();
		String className = c.getName();
		String[] name = className.split("\\.");
		String nameTmp = name[name.length - 1];
		return nameTmp.substring(0,nameTmp.length() - 2);
	}
	
	public String getInterfaceName() {
		Class<?> c = this.object.getClass();
		Class<?>[] interfaces = c.getInterfaces();
		String className = interfaces[1].getName();
		String[] name = className.split("\\.");
		return name[name.length - 1];
	}

	public List<Method> getGetterMethod() {
		List<Method> methodList = new ArrayList<Method>();
		try {
			Class<?> c = this.object.getClass();
			Method[] method = c.getMethods();
			
			for(Method m : method) {
				String method_name = ((Method) m).getName();
				String return_type = ((Method) m).getReturnType().toString();
	
				if(method_name.length() >= 3 && method_name.substring(0,3).equals("get")) {
					if(return_type.equals(AbsJavaReflection.ABSStringClassName)
					|| return_type.equals(AbsJavaReflection.ABSIntegerClassName)
					|| return_type.equals(AbsJavaReflection.ABSRationalClassName)) {
						methodList.add(m);
					}
				}
			}
			
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return methodList;
	}
	
	public List<Method> getSetterMethod() {
		List<Method> methodList = new ArrayList<Method>();
		try {
			Class<?> c = this.object.getClass();
			Method[] method = c.getMethods();
			
			for(Method m : method) {
				String method_name = ((Method) m).getName();
				String return_type = ((Method) m).getReturnType().toString();
	
				if(method_name.length() >= 3 && method_name.substring(0,3).equals("set")) {
					if(return_type.equals(AbsJavaReflection.ABSUnitClassName)) {
						methodList.add(m);
					}
				}
			}
			
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return methodList;
	}
	
	public List<Method> getMethod() {
		List<Method> methodList = new ArrayList<Method>();
		try {
			Class<?> c = this.object.getClass();
			Method[] method = c.getMethods();
			
			for(Method m : method) {
				methodList.add(m);
			}
			
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return methodList;
	}
	
	public Object invokeMethod(Method method) {
		try {
			return ((Method) method).invoke(this.object);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return null;
	}
	
	public Object invokeMethod(Method method, Object param) {
		try {
			return ((Method) method).invoke(this.object, param);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return null;
	}

    public String getStringValueFromInvokeMethod(Method m) {
        String return_type = m.getReturnType().toString();
        String value = "";
        if(return_type.equals(AbsJavaReflection.ABSStringClassName) && (this.invokeMethod(m)) != null) {
            value = ((abs.backend.java.lib.types.ABSString)(this.invokeMethod(m))).getString();
        } else if(return_type.equals(AbsJavaReflection.ABSIntegerClassName) && (this.invokeMethod(m)) != null) {
            int int_value = ((abs.backend.java.lib.types.ABSInteger)(this.invokeMethod(m))).toInt();
            StringBuilder string = new StringBuilder();
            string.append("");
            string.append(int_value);
            value = string.toString();
        } else if(return_type.equals(AbsJavaReflection.ABSRationalClassName) && (this.invokeMethod(m)) != null) {
            int int_value = ((abs.backend.java.lib.types.ABSRational)(this.invokeMethod(m))).toInt();
            StringBuilder string = new StringBuilder();
            string.append("");
            string.append(int_value);
            value = string.toString();
        }
        return value;
    }
	
	public String getParameterType(Method method) {
		Class<?>[] parameterType  = method.getParameterTypes();
		return parameterType[0].toString();
	}
}

class AbsSqlCommandBuilder {
	private StringBuilder command;
	
	private AbsSqlCommandBuilder() {
		this.command = new StringBuilder();
	}
	
	public static AbsSqlCommandBuilder getCommandBuilder() {
		return new AbsSqlCommandBuilder();
	}
	
	public String insert(String table, Map<String, String> attributes) {
		StringBuilder valueString = new StringBuilder();
	
		this.command.append("INSERT INTO ");
		this.command.append(table);
		this.command.append("(");
		
		Iterator iterator = attributes.entrySet().iterator();
		while(iterator.hasNext()) {
			Map.Entry pair = (Map.Entry) iterator.next();
			this.command.append(pair.getKey());

			valueString.append("\"");
			valueString.append(pair.getValue());
			valueString.append("\"");
			
			if(iterator.hasNext()) {
				this.command.append(", ");
				valueString.append(", ");
			}
		}
		this.command.append(") ");
		this.command.append("VALUES(");
		this.command.append(valueString.toString());
		this.command.append(")");
		
		return this.command.toString();
	}
	
	public String select(String table, List<String> condition) {
        this.command.append("SELECT * FROM ");
        this.command.append(table);
        if(condition != null) {
            this.command.append(" WHERE ");
            Iterator<String> iterator = condition.iterator();
            while(iterator.hasNext()) {
                this.command.append(iterator.next());
                if(iterator.hasNext()) {
                    this.command.append(" AND ");
                }
            }
        }
        return this.command.toString();
    }

    public String delete(String table, Map<String, String> condition) {
        this.command.append("DELETE FROM ");
        this.command.append(table);
        if(condition != null) {
            this.command.append(" WHERE ");
            Iterator iterator = condition.entrySet().iterator();
            while(iterator.hasNext()) {
                Map.Entry pair = (Map.Entry) iterator.next();
                this.command.append(pair.getKey());
                this.command.append("=");
                this.command.append("\"");
                this.command.append(pair.getValue());
                this.command.append("\"");

                if(iterator.hasNext()) {
                    this.command.append(" AND ");
                }
            }
        }
        return this.command.toString();
	}
	
	public String update(String table, Map<String, String> attributes, Map<String, String> condition) {
        this.command.append("UPDATE ");
        this.command.append(table);

        if(attributes != null) {
            this.command.append(" SET ");
            Iterator iterator = attributes.entrySet().iterator();
            while(iterator.hasNext()) {
                Map.Entry pair = (Map.Entry) iterator.next();
                this.command.append(pair.getKey());
                this.command.append("=");
                this.command.append("\"");
                this.command.append(pair.getValue());
                this.command.append("\"");

                if(iterator.hasNext()) {
                    this.command.append(", ");
                }
            }
        }

        if(condition != null) {
            this.command.append(" WHERE ");
            Iterator iterator = condition.entrySet().iterator();
            while(iterator.hasNext()) {
                Map.Entry pair = (Map.Entry) iterator.next();
                this.command.append(pair.getKey());
                this.command.append("=");
                this.command.append("\"");
                this.command.append(pair.getValue());
                this.command.append("\"");

                if(iterator.hasNext()) {
                    this.command.append(" AND ");
                }
            }
        }

        return this.command.toString();
	}
}
