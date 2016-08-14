package org.absmodels.abs.plugin.editor.reconciling;

import static org.absmodels.abs.plugin.util.Constants.PARSE_MARKER_TYPE;

import java.io.IOException;
import java.io.StringReader;
import java.util.Collections;
import java.util.List;

import org.absmodels.abs.plugin.builder.AbsNature;
import org.absmodels.abs.plugin.editor.ABSEditor;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;

import abs.frontend.ast.CompilationUnit;
import abs.frontend.parser.Main;
import abs.frontend.parser.ParserError;

/**
 * parses an Abs document when the user stops typing 
 * notifies the outline and updates error markers accordingly
 */
public class ABSReconcilingStrategy implements IReconcilingStrategy {

    private IDocument document;
    private ABSEditor editor;
    private Main absParser;

    public ABSReconcilingStrategy(ABSEditor editor) {
        this.editor = editor;
        editor.setReconciler(this);
        absParser = new Main();
        absParser.setAllowIncompleteExpr(true);
        absParser.setTypeChecking(false);
    }

    @Override
    public void setDocument(IDocument document) {
        this.document = document;
    }

    @Override
    public void reconcile(DirtyRegion dirtyRegion, IRegion subRegion) {
        reconcile();
    }

    @Override
    public void reconcile(IRegion partition) {
        reconcile();
    }

    private void reconcile() {
        reconcile(editor.getAbsNature(), true);
    }

    public synchronized CompilationUnit reconcile(AbsNature nature, boolean withTypechecks) {
        CompilationUnit compilationUnit = parseDocument();
        if (nature == null) {
            setParseErrorMarkers(compilationUnit.getParserErrors());
            editor.onCompilationUnitChange(compilationUnit);
            return compilationUnit;
        }
        if (compilationUnit.getParserErrors() == null) {
            compilationUnit.setParserErrors(Collections.<ParserError>emptyList());
        }

        setParseErrorMarkers(compilationUnit.getParserErrors());

        AbsModelManager manager = nature.getModelManager();
        manager.updateModel(compilationUnit, withTypechecks);
        

        editor.onCompilationUnitChange(manager.getCompilationUnit(editor.getAbsoluteFilePath()));
        return compilationUnit;
    }

    private CompilationUnit parseDocument() {
        String code = document.get();
        CompilationUnit compilationUnit = parse(code);
        if (compilationUnit == null || compilationUnit.getNumModuleDecl() + compilationUnit.getNumFeatureDecl() == 0) {
            // if compilation was not successful, try inserting a semicolon
            
            try {
                // add semicolon to end of current line and try to parse again
                int caretPos = editor.getCaretPos();
                IRegion lineInfo = document.getLineInformationOfOffset(caretPos);
                int endOfLine = lineInfo.getOffset() + lineInfo.getLength();
                code = document.get(0, endOfLine) + ";" + document.get(endOfLine, document.getLength() - endOfLine);
                System.out.println(code);
                CompilationUnit compilationUnit2 = parse(code);
                if (compilationUnit2 != null && compilationUnit2.getNumModuleDecl() + compilationUnit2.getNumFeatureDecl() > 0) {
                    // compilationUnit2 has better results, so use it
                    compilationUnit = compilationUnit2;
                }
            } catch (BadLocationException e) {
                e.printStackTrace();
            }
        }
        return compilationUnit;
    }

    private void setParseErrorMarkers(List<ParserError> parserErrors) {
        IResource r = editor.getResource();
        try {
            r.deleteMarkers(PARSE_MARKER_TYPE, true, IResource.DEPTH_ZERO);

            for (ParserError err : parserErrors) {
                AbsNature.addMarker(r, err);
            }

        } catch (CoreException e) {
            e.printStackTrace();
        }

    }

    private CompilationUnit parse(String code) {
        try {
            return absParser.parseUnit(editor.getFile(), code, new StringReader(code));
        } catch (IOException e) {
            return new CompilationUnit();
        }
    }




}
