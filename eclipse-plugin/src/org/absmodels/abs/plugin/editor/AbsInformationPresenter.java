package org.absmodels.abs.plugin.editor;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.information.IInformationProviderExtension2;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.swt.widgets.Shell;

/**
 * an information presenter can display information at the current cursor position 
 */
public class AbsInformationPresenter extends InformationPresenter {

    private static class InformationControlCreator implements IInformationControlCreator {

        @Override
        public IInformationControl createInformationControl(Shell parent) {
            // should never be called
            throw new Error("not implemented");
        }

    }
    
    /**
     * information provider which presents information from 
     */
    private class InformationProvider implements IInformationProvider, IInformationProviderExtension2{

        @Override
        public IRegion getSubject(ITextViewer textViewer, int offset) {
            return new Region(offset, 1);
        }

        @Override
        public String getInformation(ITextViewer textViewer, IRegion subject) {
            return "This is an nonempty String which is not shown anywhere";
        }

        @Override
        public IInformationControlCreator getInformationPresenterControlCreator() {
            return new IInformationControlCreator() {
                @Override
                public IInformationControl createInformationControl(Shell parent) {
                    return informationControl;
                }
            };
        }
    }

    private IInformationControl informationControl;

    public AbsInformationPresenter() {
        super(new InformationControlCreator());
        IInformationProvider provider = new InformationProvider();
        setInformationProvider(provider, IDocument.DEFAULT_CONTENT_TYPE);
    }

    /**
     * set the information control which is shown when showInformation() is called 
     */
    public void setInformationControl(IInformationControl c) {
        this.informationControl = c;
    }

}
