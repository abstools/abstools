package eu.hatsproject.absplugin.editor;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.hyperlink.AbstractHyperlinkDetector;
import org.eclipse.jface.text.hyperlink.IHyperlink;

import abs.frontend.ast.ASTNode;
import abs.frontend.ast.Call;
import abs.frontend.ast.ClassDecl;
import abs.frontend.ast.CompilationUnit;
import abs.frontend.ast.ConstructorPattern;
import abs.frontend.ast.DataConstructorExp;
import abs.frontend.ast.DeltaClause;
import abs.frontend.ast.Deltaspec;
import abs.frontend.ast.FnApp;
import abs.frontend.ast.FromExport;
import abs.frontend.ast.FromImport;
import abs.frontend.ast.ModifyClassModifier;
import abs.frontend.ast.ModuleDecl;
import abs.frontend.ast.Name;
import abs.frontend.ast.NewExp;
import abs.frontend.ast.PatternVarUse;
import abs.frontend.ast.StarExport;
import abs.frontend.ast.StarImport;
import abs.frontend.ast.ThisExp;
import abs.frontend.ast.TypeUse;
import abs.frontend.ast.UnknownDecl;
import abs.frontend.ast.VarOrFieldUse;
import abs.frontend.typechecker.DataTypeType;
import abs.frontend.typechecker.KindedName;
import abs.frontend.typechecker.KindedName.Kind;
import abs.frontend.typechecker.ResolvedName;
import abs.frontend.typechecker.Type;
import abs.frontend.typechecker.TypeCheckerException;
import beaver.Symbol;
import eu.hatsproject.absplugin.Activator;
import eu.hatsproject.absplugin.util.InternalASTNode;
import eu.hatsproject.absplugin.util.UtilityFunctions;
import eu.hatsproject.absplugin.util.UtilityFunctions.EditorPosition;

/**
 * a hyperlink detector
 * detects links in abs documents
 * (ctrl + left mouse can be used to jump to declaration) 
 */
public class AbsHyperlinkDetector extends AbstractHyperlinkDetector {

    private static final class AbsHyperlink implements IHyperlink {
        private final int endOffset;
        private final int startOffset;
        private final EditorPosition pos;
        private ABSEditor editor;

        private AbsHyperlink(ABSEditor editor, int endOffset, int startOffset, EditorPosition pos) {
            this.editor = editor;
            this.endOffset = endOffset;
            this.startOffset = startOffset;
            this.pos = pos;
        }

        @Override
        public void open() {
            jumpToPosition(editor, pos);
        }

        @Override
        public String getTypeLabel() {
            return null;
        }

        @Override
        public String getHyperlinkText() {
            return null;
        }

        @Override
        public IRegion getHyperlinkRegion() {
            return new Region(startOffset, endOffset+1-startOffset);
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

            EditorPosition pos = null;
            ASTNode<?> node;
            synchronized (cu.getNature()) {
                node = UtilityFunctions.getASTNodeOfOffset(doc, cu.getASTNode(), offset);
                if (node == null) {
                    return null;
                }
                pos = getPosition(cu.getASTNode(), node);
            }

            if (pos == null) {
                return null;
            }

            final int startOffset = getOffset(doc, node.getStart());
            final int endOffset = getOffset(doc, node.getEnd());

            return new IHyperlink[]{
                    new AbsHyperlink(editor, endOffset, startOffset, pos)
            };
        } catch (BadLocationException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * gets the offset for a given line & column
     * @param doc
     * @param position line & column packed
     * @return
     * @throws BadLocationException
     */
    private static int getOffset(IDocument doc, int position)	throws BadLocationException {
        return doc.getLineOffset(Symbol.getLine(position)-1) + Symbol.getColumn(position)-1;
    }

    /**
     * jumps to a given position
     * 
     * @param currentEditor the current editor
     * @param pos position to jump to. Must be in same project as current editor.
     */
    public static void jumpToPosition(ABSEditor currentEditor, EditorPosition pos) {
        IDocument doc;
        IProject project = currentEditor.getProject();
        ABSEditor targeteditor = 
            UtilityFunctions.openABSEditorForFile(pos.getPath(),project );
        if(targeteditor==null){
            currentEditor.openInformation("File not found!",
                    "Could not find file "+pos.getPath().toOSString());
            return;
        }
        doc = targeteditor.getDocumentProvider().getDocument(targeteditor.getEditorInput());
        try {    
            int startoff = doc.getLineOffset(pos.getLinestart()-1) + pos.getColstart()-1;
            targeteditor.getSelectionProvider().setSelection(new TextSelection(startoff,0));
        } catch(BadLocationException ex){
            Activator.logException(ex);
        }
    }

    /**
     * @param cu
     * @param node
     * @return the position of a declaration linked to the given node
     * or null if there is no declaration
     */
    public static EditorPosition getPosition(CompilationUnit cu, ASTNode<?> node) {
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
                decl = getDeltaDecl(d.getDeltaspec());
            } else if (node instanceof Deltaspec) {
                Deltaspec deltaspec = (Deltaspec) node;
                decl = getDeltaDecl(deltaspec);
            } else if (node instanceof ModifyClassModifier) {
                ModifyClassModifier cm = (ModifyClassModifier) node;
                String mName = cm.moduleName();
                ModuleDecl dm = cu.lookupModule(mName);
                decl = dm.lookup(new KindedName(Kind.CLASS, cm.className()));
            }
        } catch (TypeCheckerException e) {
            // Nada - may come from resolveName() on broken models.
            Activator.logException(e);
        }

        if(decl == null || decl instanceof UnknownDecl){
            return null;
        }
        CompilationUnit declcu = decl.getCompilationUnit();

        int start = decl.getStartPos();
        int end = decl.getEndPos();
        
        return new EditorPosition(new Path(declcu.getFileName()), Symbol.getLine(start), Symbol.getColumn(start), Symbol.getLine(end), Symbol.getColumn(end));
    }

    private static ASTNode<?> getDeltaDecl(Deltaspec deltaspec) {
        String dName = deltaspec.getName();
        /* XXX: [stolz] Is 'm' ALWAYS the right scope to lookup in? */
        ModuleDecl m = deltaspec.getModuleDecl();
        ResolvedName res = m.resolveName(new KindedName(Kind.TYPE_DECL, dName));
        if (res != null)
            return res.getDecl();
        return null;
    }


}
