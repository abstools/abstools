/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.java.lib.net;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

import abs.backend.java.lib.net.GMLParser.Pattern;
import abs.backend.java.lib.net.NetworkGraph.AttributeStore;
import abs.backend.java.lib.net.NetworkGraph.GraphNode;

/**
 * A parser for the GML language
 * see {@link https://secure.wikimedia.org/wikipedia/en/wiki/Graph_Modelling_Language}
 * 
 * <h2>Example</h2>
 * <pre>
 * graph [
 *    node [
 *       id = 1
 *    ]
 *    
 *    node [
 *       id = 2
 *    ]
 *    
 *    edge [
 *      source = 1
 *      target = 2
 *    ]
 *    
 *    
 * ]
 * </pre>
 * 
 * @author jschaefer
 *
 */
public class GMLParser {
    
    private NetworkGraph graph;
    private Reader reader;
    private StringBuilder readText = new StringBuilder();
    
    public NetworkGraph parseString(String s) throws IOException {
        return parseReader(new StringReader(s));
    }
    
    public NetworkGraph parseStream(InputStream s) throws IOException {
        return parseReader(new InputStreamReader(s));
    }
        
    public NetworkGraph parseReader(Reader r) throws IOException {
        reader = new BufferedReader(r);
        readText.delete(0, readText.length());
        return parseNetwork();
    }

    private NetworkGraph parseNetwork() throws IOException {
        graph = new NetworkGraph();
        
        scan(Seq(ZeroOrMore(WS)));
        scan("graph");
        skipWhitespaces();
        scan("[");
        skipWhitespaces();
        scan(ZeroOrMore(new ElementParser("node")));
        scan("]");
        
        return graph;
    }
    
    interface Pattern {
        MatchResult matchNext(char c);
        void reset();
    }
    
    enum MatchResult {
        FAILED, OK, MORE_EXPECTED, OK_LAST, FINISHED
    }
    
    static abstract class StatelessPattern implements Pattern {
        @Override
        public void reset() {
        }
    }
    
    public static final Pattern WS = new WS();
    public static Pattern Opt(Pattern p) {
        return new OptPattern(p);
    }
    public static Pattern Seq(Pattern... p) {
        return new Seq(p);
    }
    public static Pattern ZeroOrMore(Pattern p) {
        return new ZeroOrMorePattern(p);
    }
    
    static class WS extends StatelessPattern {
        @Override
        public MatchResult matchNext(char c) {
            return Character.isWhitespace(c) ? MatchResult.OK_LAST : MatchResult.FAILED; 
        }
    }
    
    static class CompositePattern implements Pattern {
        protected final Pattern pattern;

        public CompositePattern(Pattern p) {
            this.pattern = p;
        }
        
        @Override
        public MatchResult matchNext(char c) {
            return pattern.matchNext(c);
        }
        
        @Override
        public void reset() {
            pattern.reset();
        }
        
        public Pattern getPattern() {
            return pattern;
        }
        
    }
    
    static class OptPattern extends CompositePattern {
        boolean tested = false;
        
        public OptPattern(Pattern p) {
            super(p);
        }
        
        @Override
        public MatchResult matchNext(char c) {
            if (tested)
                return MatchResult.FAILED;
            MatchResult r = pattern.matchNext(c);
            if (r != MatchResult.FAILED)
                return r;
            else
                return MatchResult.FINISHED;
        }
        
        @Override
        public void reset() {
            super.reset();
            tested = false;
        }
    }

    
    static class Seq implements Pattern {

        private final Pattern[] patterns;
        private int curPattern;
        public Seq(Pattern... p) {
            this.patterns = p;
        }
        
        @Override
        public MatchResult matchNext(char c) {
            if (curPattern >= patterns.length)
                return MatchResult.FINISHED;
            
            Pattern p = patterns[curPattern];
            MatchResult r = p.matchNext(c);
            switch (r) {
            case FINISHED: curPattern++; return matchNext(c); 
            case OK_LAST:  
                curPattern++;
                if (curPattern == patterns.length) {
                    return MatchResult.OK_LAST;
                } 
                return MatchResult.OK;
            default: return r;
            }
        }

        @Override
        public void reset() {
            curPattern = 0;
            for (Pattern p : patterns) {
                p.reset();
            }
                
        }
        
    }
    
    static class ZeroOrMorePattern extends CompositePattern {
        public ZeroOrMorePattern(Pattern p) {
            super(p);
        }
        
        @Override
        public MatchResult matchNext(char c) {
            MatchResult r = pattern.matchNext(c);
            switch (r) {
            case FINISHED: 
            case OK_LAST: pattern.reset(); return MatchResult.OK;
            case FAILED: return MatchResult.FINISHED;
            default: return r;
            }
        }
        
    }
    
    
    Pattern Str(String s) {
        return new StringPattern(s);
    }
    
    static class StringPattern implements Pattern {
        public final String value;
        private int pos;
        public StringPattern(String v) {
            value = v;
        }
        
        @Override
        public MatchResult matchNext(char c) {
            int oldPos = pos;
            pos++;
            if (oldPos >= value.length())
                return MatchResult.FAILED;
            
            if (value.charAt(oldPos) == c) {
                if (oldPos == value.length() - 1)
                    return MatchResult.OK_LAST;
                else
                    return MatchResult.MORE_EXPECTED;
            } else {
                return MatchResult.FAILED;
            }
        }

        @Override
        public void reset() {
            pos = 0;
        }
    }
    
    private AttributeStore parseAttributes() {
        return new AttributeStore();
    }

    static abstract class Parser extends CompositePattern {
        public Parser(Pattern p) {
            super(p);
        }

        @Override
        public MatchResult matchNext(char c) {
            MatchResult r = super.matchNext(c);
            switch (r) {
            case FINISHED:
            case OK_LAST:
                onMatch();
            }
            return r;
        }
        
        public abstract void onMatch();
    }
    
    
    
    class ElementParser extends Parser {
        private String name;

        public ElementParser(String name) {
            super(Seq(ZeroOrMore(WS), 
                    Str(name), ZeroOrMore(WS), 
                    Str("["), ZeroOrMore(WS),
                    Str("]")));
            this.name = name;
        }

        @Override
        public void onMatch() {
            //System.out.println(name+" found");
        }
    }
    
    
    class ScanException extends RuntimeException {

        public ScanException(String string) {
            super(string);
        }    
       
    }
    
    private boolean scanZeroOrMore(Pattern p) throws IOException {
        return scan(reader, new ZeroOrMorePattern(p));
    }
    
    private boolean scanOpt(Pattern p) throws IOException {
        return scan(new OptPattern(p));
    }
    
    private boolean scanOpt(String word) throws IOException {
        return scanOpt(new StringPattern(word));
    }

    private boolean scan(String word) throws IOException {
        return scan(new StringPattern(word));
    }

    private boolean scan(Pattern pattern) throws IOException  {
        return scan(reader,pattern);
    }
    
    private boolean scan(Reader reader, Pattern pattern) throws IOException  {
        MatchResult res = null;
        do {
            int i = readText.length();
            reader.mark(Short.MAX_VALUE);
            int ic = reader.read();
            if (ic == -1) {
                if (res == MatchResult.MORE_EXPECTED)
                    throw new ScanException("End of stream reached before pattern could match");
                else
                    return true;
            }
            char c = (char) ic;
            readText.append(c);
            System.out.println(readText);
            res = pattern.matchNext(c);
            if (res == MatchResult.FAILED)
                throw new ScanException("Expected pattern "+pattern+" did not match");
            if (res == MatchResult.FINISHED) {
                reader.reset();
                readText.delete(i, readText.length());
                return true;
            }
        } while (res != MatchResult.OK_LAST);
        return true;
    }
    
    private void skipWhitespaces() throws IOException {
        scanZeroOrMore(WS);
    }

    public static void main(String[] args) throws IOException {
        GMLParser p = new GMLParser();
        
        System.out.println(p.parseString("graph[]"));
        System.out.println(p.parseString(" graph [ ] "));
        System.out.println(p.parseString("  graph  [    ] "));
        System.out.println(p.parseString("  graph  [  node [ ]  ] "));
        
        System.out.println(p.parseString(" graph [ node [ ] node[] ]"));
    }
}
