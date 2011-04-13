/** 
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package eu.hatsproject.absplugin.internal;

import static eu.hatsproject.absplugin.util.Constants.ABSFRONTEND_PLUGIN_ID;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;

import abs.frontend.analyser.SemanticErrorList;
import abs.frontend.ast.ASTNode;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.List;
import abs.frontend.ast.Model;
import abs.frontend.parser.Main;
import abs.frontend.typechecker.locationtypes.LocationType;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension;
import abs.frontend.typechecker.locationtypes.infer.LocationTypeInferrerExtension.LocationTypingPrecision;

public class IncrementalModelBuilder {

	private Model model  = null;
    private LocationTypeInferrerExtension ltie;
	
	public LocationTypeInferrerExtension getLocationTypeInferrerExtension() {
        return ltie;
    }

    @SuppressWarnings("rawtypes")
	private static void flushAll(ASTNode node){
		for(int i=0; i<node.getNumChild(); i++){
			flushAll(node.getChild(i));
		}
		node.flushCache();
	}
	
    
    public synchronized void addCompilationUnits(Iterable<CompilationUnit> units) throws IOException, NoModelException {
       for (CompilationUnit u : units) {
          addCompilationUnit(u);
       }
    }
    
	public synchronized void addCompilationUnit(CompilationUnit cu) throws IOException, NoModelException  {
		if(model == null){
			model = new Model();
			
			model.addCompilationUnit(getStdLibCompilationUnit());
			model.addCompilationUnit(cu);
			return;
		}
		String filename  = cu.getFileName();
		assert filename != null;
		
		CompilationUnit cuold = getCompilationUnit(filename);
		List<CompilationUnit> culist = model.getCompilationUnitList();
		int cindex = culist.getIndexOfChild(cuold);
		if(cindex>0){
			model.setCompilationUnit(cu, cindex);
		}
		else {
			model.addCompilationUnit(cu);
		}
//		model.flushCache();
		flushAll(model);
	}

   private CompilationUnit getStdLibCompilationUnit() throws IOException {
      CompilationUnit stdLib = new Main().getStdLib();
      File bundle = FileLocator.getBundleFile(Platform.getBundle(ABSFRONTEND_PLUGIN_ID));

      File src = new File(bundle, stdLib.getFileName());
      if (!src.exists()) {
      	src = new File(bundle, "src/"+stdLib.getFileName());
      }
      stdLib.setName(src.getAbsolutePath());
      return stdLib;
   }
	
	public synchronized void removeCompilationUnit(CompilationUnit cu) throws NoModelException{
		if(model == null)
			throw new NoModelException();
		String filename  = cu.getFileName();
		assert filename != null;
		
		CompilationUnit cuold = getCompilationUnit(filename);
		List<CompilationUnit>  culist = model.getCompilationUnitList();
		int cindex = culist.getIndexOfChild(cuold);
		if(cindex>0){
			culist.removeChild(cindex);
		}
//		model.flushCache();
		flushAll(model);
	}
	
	public synchronized CompilationUnit getCompilationUnit(String fileName) throws NoModelException{
		if(model == null)
			throw new NoModelException();
		Iterator<CompilationUnit> iter = model.getCompilationUnits().iterator();
		while(iter.hasNext()){
			CompilationUnit cu = iter.next();
			if(fileName.equals(cu.getFileName())){
				return cu;
			}
		}
		return null;
	}
	
	public synchronized SemanticErrorList typeCheckModel(boolean locationTypeChecking, String defaultloctype, String locationTypePrecision) throws NoModelException, TypecheckInternalException{
		if(model == null)
			throw new NoModelException();
		
		if(model.hasParserErrors())
			return new SemanticErrorList(); // don't typecheck if the model has parsererrors
//			throw new TypecheckInternalException(new Exception("Model has parser errors!"));
//		model.flushCache();
		flushAll(model);
		SemanticErrorList typeerrors = new SemanticErrorList();
		if (locationTypeChecking) {
			LocationType defaultLocType = LocationType.createFromName(defaultloctype);
			LocationTypeInferrerExtension ltie = new LocationTypeInferrerExtension(model);
			this.ltie = ltie;
			ltie.setDefaultType(defaultLocType);
			ltie.setLocationTypingPrecision(LocationTypingPrecision.valueOf(locationTypePrecision));
	        model.registerTypeSystemExtension(ltie);
		} 
		try {
			typeerrors = model.typeCheck();
			return typeerrors;
		} catch (RuntimeException e) {
			e.printStackTrace();
			throw new TypecheckInternalException(e);
		}
	}
	
	public synchronized Model getCompleteModel(){
		return model;
	}
	
	public synchronized void cleanModel(){
		model = null;
	}

}
