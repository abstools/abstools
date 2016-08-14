package org.absmodels.abs.plugin.editor;

import java.util.*;
import java.util.List;

import org.absmodels.abs.plugin.Activator;
import org.absmodels.abs.plugin.util.InternalASTNode;
import org.absmodels.abs.plugin.util.UtilityFunctions;
import org.absmodels.abs.plugin.util.UtilityFunctions.EditorPosition;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.hyperlink.AbstractHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.IHyperlink;

import abs.frontend.ast.*;
import abs.frontend.typechecker.*;
import abs.frontend.typechecker.KindedName.Kind;

/**
 * a hyperlink detector
 * detects links in abs documents
 * (ctrl + left mouse can be used to jump to declaration)
 */
public class AbsHyperlinkDetector extends AbstractHyperlinkDetector {

    private abstract static class AbsHyperlink implements IHyperlink {
        private final int endOffset;
        private final int startOffset;
        protected ABSEditor editor;

        private AbsHyperlink(ABSEditor editor, int startOffset, int endOffset) {
            this.editor = editor;
            this.endOffset = endOffset;
            this.startOffset = startOffset;
        }

        @Override
        public String getTypeLabel() {
            return null;
        }

        @Override
        public IRegion getHyperlinkRegion() {
            return new Region(startOffset, endOffset+1-startOffset);
        }
    }

    private static final class JumpToDeclaration extends AbsHyperlink {
        private final EditorPosition targetPos;
        private final String name;

        private JumpToDeclaration(ABSEditor editor, int startOffset, int endOffset, EditorPosition targetPos) {
            this(editor, startOffset, endOffset, targetPos, "");
        }

        public JumpToDeclaration(ABSEditor editor, int startOffset, int endOffset, EditorPosition targetPos,
                String name) {
            super(editor, startOffset, endOffset);
            this.targetPos = targetPos;
            this.name = name;
        }

        @Override
        public void open() {
            jumpToPosition(editor, targetPos);
        }

        @Override
        public String getHyperlinkText() {
           return "Open declaration " + name;
        }

    }

    private static final class JumpToImplementation extends AbsHyperlink {
        private final String methodName;
        private final Type calleeType;

        public JumpToImplementation(ABSEditor editor, int startOffset, int endOffset, String methodName, Type calleeType) {
            super(editor, startOffset, endOffset);
            this.methodName = methodName;
            this.calleeType = calleeType;
        }

        @Override
        public void open() {
            InterfaceDecl i = (InterfaceDecl) calleeType.getDecl();
            final List<MethodImpl> implementingMethods = new ArrayList<MethodImpl>();
            for (HasTypeHierarchy t : i.getSubTypes()) {
                if (t instanceof ClassDecl) {
                    ClassDecl c = (ClassDecl) t;
                    MethodImpl m = c.lookupMethod(methodName);
                    if (m != null) {
                        implementingMethods.add(m);
                    }
                }
            }
            if (implementingMethods.size() == 1) {
                // only one implementing method => directly jump to it
                jumpToPosition(editor, getPosition(implementingMethods.get(0)));
            } else if (implementingMethods.size() > 1) {
                // more than one implementing method => show alternatives in a list
                AbsInformationPresenter p = editor.getInformationPresenter();
                p.setInformationControl(new HyperlinkInformationControl(
                        editor, "Select implementing class ...", implementingMethods));
                p.showInformation();
            }
        }

        @Override
        public String getHyperlinkText() {
           return "Open implementation";
        }

    }

    private ABSEditor editor;

    public AbsHyperlinkDetector(ABSEditor editor) {
        this.editor = editor;
    }

    @Override
    public IHyperlink[] detectHyperlinks(ITextViewer textViewer, final IRegion region, boolean canShowMultipleHyperlinks) {
        return getHyperlinks(editor, region.getOffset());
    }

    public static IHyperlink[] getHyperlinks(ABSEditor editor, int offset) {
        try {
            InternalASTNode<CompilationUnit> cu = editor.getCompilationUnit();
            if (cu == null) {
                return null;
            }
            IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());

            EditorPosition targetPos = null;
            ASTNode<?> node;
            ASTNode<?> decl;
            synchronized (cu.getNature()) {
                node = UtilityFunctions.getASTNodeOfOffset(doc, cu.getASTNode(), offset);
                if (node == null) {
                    return null;
                }
                decl = getDecl(cu.getASTNode(), node);
                targetPos = getPosition(decl);
            }

            if (targetPos == null) {
                return null;
            }

            final int startOffset = getOffset(doc, node.getStartLine(), node.getStartColumn());
            final int endOffset = getOffset(doc, node.getEndLine(), node.getEndColumn());

            if (decl instanceof MethodSig) {
                MethodSig methodSig = (MethodSig) decl;
                if (methodSig.getContextDecl() instanceof InterfaceDecl) {
                    // decl is an interface method
                    Type typ = new InterfaceType((InterfaceDecl) methodSig.getContextDecl());
                    if (node instanceof Call) {
                        // in case of a call the type can be determined more exactly
                        Call call = (Call) node;
                        typ = call.getCallee().getType();
                    }

                    return new IHyperlink[]{
                            new JumpToDeclaration(editor, startOffset, endOffset, targetPos),
                            new JumpToImplementation(editor, startOffset, endOffset, methodSig.getName(), typ)
                    };
                } else if (methodSig.getContextDecl() instanceof ClassDecl) {
                    // cursor is on a class method => provide links to methods in interfaces
                    ClassDecl classDecl = (ClassDecl) methodSig.getContextDecl();
                    List<IHyperlink> links = new ArrayList<IHyperlink>();
                    Set<MethodSig> addedMethods = new HashSet<MethodSig>();
                    for (InterfaceDecl sup : classDecl.getSuperTypes()) {
                        MethodSig lookupMethod = sup.lookupMethod(methodSig.getName());
                        if (lookupMethod != null && !addedMethods.contains(lookupMethod)) {
                            addedMethods.add(lookupMethod);
                            targetPos = getPosition(lookupMethod);
                            String name = lookupMethod.getContextDecl().getName() + "." + lookupMethod.getName();
                            links.add(new JumpToDeclaration(editor, startOffset, endOffset, targetPos, name));
                        }
                    }
                    if (links.size() > 0) {
                        return links.toArray(new IHyperlink[links.size()]);
                    } else {
                        return null;
                    }
                }
            }

            return new IHyperlink[]{
                    new JumpToDeclaration(editor, startOffset, endOffset, targetPos)
            };
        } catch (BadLocationException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * gets the offset for a given line & column
     * @param doc
     * @param position line & column
     * @return
     * @throws BadLocationException
     */
    private static int getOffset(IDocument doc, int line, int col) throws BadLocationException {
        return doc.getLineOffset(line-1) + col-1;
    }

    /**
     * jumps to a given position
     *
     * @param currentEditor the current editor
     * @param pos position to jump to. Must be in same project as current editor.
     */
    public static void jumpToPosition(ABSEditor currentEditor, EditorPosition pos) {
        IProject project = currentEditor.getProject();
        boolean opened = UtilityFunctions.jumpToPosition(project, pos);
        if (!opened) {
            currentEditor.openInformation("File not found!",
                    "Could not find file "+pos.getPath().toOSString());
            return;
        }
    }

    /**
     * returns the position of a given node
     */
    public static EditorPosition getPosition(ASTNode<?> node) {
        if(node == null || node instanceof UnknownDecl){
            return null;
        }
        CompilationUnit declcu = node.getCompilationUnit();

        return new EditorPosition(new Path(declcu.getFileName()), node.getStartLine(), node.getStartColumn(), node.getEndLine(), node.getEndColumn());
    }

    /**
     * get the declaration associated with a given node
     */
    private static ASTNode<?> getDecl(CompilationUnit cu, ASTNode<?> node) {
        ASTNode<?> decl = null;
        try {
            if(node instanceof FnApp){
                FnApp fnapp = (FnApp)node;
                String name = fnapp.getName();
                decl = fnapp.lookup(new KindedName(Kind.FUN, name));
            } else if(node instanceof VarOrFieldUse){
                VarOrFieldUse vofu = (VarOrFieldUse)node;
                String name = vofu.getName();
                decl = vofu.lookupVarOrFieldName(name,false);
            }  else if (node instanceof PatternVarUse) {
                PatternVarUse patternVarUse = (PatternVarUse) node;
                String name = patternVarUse.getName();
                decl = patternVarUse.lookupVarOrFieldName(name, false);
            } else if(node instanceof Call){
                Call call = (Call)node;
                String mname = call.getMethod();
                if (call.getCallee() instanceof ThisExp) {
                    ClassDecl classDecl = (ClassDecl) call.getContextDecl();
                    decl = classDecl.lookupMethod(mname);
                } else {
                    Type type = call.getCallee().getType();
                    decl = type.lookupMethod(mname);
                }
            } else if(node instanceof DataConstructorExp){
                DataConstructorExp exp = (DataConstructorExp)node;
                decl = exp.getDecl();
            } else if (node instanceof ConstructorPattern) {
                ConstructorPattern exp = (ConstructorPattern) node;
                if (exp.getType().isDataType()) {
                    DataTypeType type = (DataTypeType) exp.getType();
                    decl = type.getDecl();
                }
            } else if(node instanceof TypeUse){
                TypeUse tu = (TypeUse)node;
                decl = tu.getDecl();
            } else if(node instanceof NewExp){
                NewExp newexp = (NewExp)node;
                String classname = newexp.getClassName();
                decl = newexp.lookup(new KindedName(Kind.CLASS, classname));
            } else if(node instanceof FromImport){
                FromImport fimport = (FromImport)node;
                String moduleName = fimport.getModuleName();
                decl = fimport.lookupModule(moduleName);
            } else if(node instanceof StarImport){
                StarImport fimport = (StarImport)node;
                String moduleName = fimport.getModuleName();
                decl = fimport.lookupModule(moduleName);
            } else if(node instanceof FromExport){
                FromExport fexport = (FromExport)node;
                decl = fexport.getModuleDecl();
            } else if(node instanceof StarExport){
                StarExport fexport = (StarExport)node;
                decl = fexport.getModuleDecl();
            } else if(node instanceof Name){
                Name name = (Name)node;
                String simpleName = name.getString();
                decl = cu.lookupModule(simpleName);
            } else if (node instanceof DeltaClause) {
                DeltaClause d = (DeltaClause) node;
                decl = getDeltaDecl(cu,d.getDeltaspec());
            } else if (node instanceof Deltaspec) {
                Deltaspec deltaspec = (Deltaspec) node;
                decl = getDeltaDecl(cu,deltaspec);
            } else if (node instanceof DeltaID) {
            	decl = getDeltaDecl(cu, ((DeltaID)node).getName());
            } else if (node instanceof Feature) {
            	return null; // TODO #395
            } else if (node instanceof ModifyClassModifier) {
                ModifyClassModifier cm = (ModifyClassModifier) node;
                decl = cm.findClass();
            } else if (node instanceof ModifyInterfaceModifier) {
                ModifyInterfaceModifier m = (ModifyInterfaceModifier) node;
                decl = m.findInterface();
                // TODO: handle all other Modify/Remove-modifiers
            } else if (node instanceof MethodSig) {
                decl = node;
            }
        } catch (TypeCheckerException e) {
            // Nada - may come from resolveName() on broken models.
            Activator.logException(e);
        }
        return decl;
    }

    private static DeltaDecl getDeltaDecl(CompilationUnit cu, String dName) {
        Model m = cu.getModel();
        assert m != null : dName;

        return m.getDeltaDeclsMap().get(dName);
        /* There may be more than one! Think of delta D(True) when ... delta(False) when.
         * We pick the first one.

           [ramus] I don't see how there could be more than one DeltaDecl with the same name.
         */
    }

	private static DeltaDecl getDeltaDecl(CompilationUnit cu, Deltaspec deltaspec) {
		return getDeltaDecl(cu,deltaspec.getDeltaID());
    }
}
