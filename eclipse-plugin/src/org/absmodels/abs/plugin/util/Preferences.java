package org.absmodels.abs.plugin.util;

import static org.absmodels.abs.plugin.util.Constants.SYNTAXCOLOR_BOLD;
import static org.absmodels.abs.plugin.util.Constants.SYNTAXCOLOR_COLOR;
import static org.absmodels.abs.plugin.util.Constants.SYNTAXCOLOR_ITALIC;
import static org.absmodels.abs.plugin.util.Constants.SYNTAXCOLOR_STRIKETHROUGH;
import static org.absmodels.abs.plugin.util.Constants.SYNTAXCOLOR_UNDERLINE;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;


public class Preferences {
    
    /**
     * Compute the attributes like bold, italic, underline and strike through for the given key-postfix
     * @param store the preference store the attributes are stored in
     * @param postfix the postfix of the key. It is combined with e.g. {@link Constants.SYNTAXCOLOR_BOLD}
     * @return the int corresponding to the set of attributes
     */
    @Deprecated
    public static int computeAttributes(IPreferenceStore store, String postfix) {
        int funattr = 0;
        //bold
        boolean attrbold = store.getBoolean(SYNTAXCOLOR_BOLD + postfix);
        if(attrbold) funattr = funattr | SWT.BOLD;
        //italic
        boolean attritalic = store.getBoolean(SYNTAXCOLOR_ITALIC + postfix);
        if(attritalic) funattr = funattr | SWT.ITALIC;
        //underline
        boolean attrunderline = store.getBoolean(SYNTAXCOLOR_UNDERLINE + postfix);
        if(attrunderline) funattr = funattr | TextAttribute.UNDERLINE;
        //strike through
        boolean attrstrikethrough = store.getBoolean(SYNTAXCOLOR_STRIKETHROUGH + postfix);
        if(attrstrikethrough) funattr = funattr | TextAttribute.STRIKETHROUGH;
        return funattr;
    }
    
    public static IToken getToken(IPreferenceStore store, String postfix) {
        Color color = new Color(Display.getCurrent(),PreferenceConverter.getColor(store, SYNTAXCOLOR_COLOR + postfix));
        IToken token = new Token(new TextAttribute(color, null, computeAttributes(store, postfix)));
        return token;
    }
    
}
