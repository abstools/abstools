package org.abs_models.backend.java.observing;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.abs_models.backend.java.lib.runtime.ABSObject;
import org.abs_models.backend.java.lib.runtime.COG;
import org.abs_models.backend.java.lib.runtime.Logging;
import org.abs_models.backend.java.lib.types.ABSAlgebraicDataType;
import org.abs_models.backend.java.lib.types.ABSInterface;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFormatter;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.Lang;
import org.apache.jena.sparql.ARQConstants;
import org.apache.jena.sparql.core.Prologue;
import org.apache.jena.sparql.resultset.ResultsWriter;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * An observer that follows cog (and, transitively, object) creation.
 * It is used to create an object graph upon request.
 */
public class GraphObserver extends DefaultSystemObserver implements ObjectCreationObserver {

    protected static final Logger log = Logging.getLogger(GraphObserver.class.getName());

    /** The namespace prefixes used by the ABS ontology. */
    public static final Map<String, String> absNamespaces = Map.of(
        "rdfs", RDFS.label.getNameSpace(),
        // the abs language ontology prefix
        "abs", "http://abs-models.org/ns/abs/",
        "prog", "http://abs-models.org/ns/prog/",
        "run", "http://abs-models.org/ns/run/");

    static String sparqlPrefix = absNamespaces.entrySet()
        .stream()
        .map(e -> "PREFIX " + e.getKey() + ": <" + e.getValue() + ">\n")
        .collect(Collectors.joining());

    /** The program model, generated at compile time */
    static Model progModel = ModelFactory.createDefaultModel();
    static Map<String, Resource> knownClasses = new HashMap<>();
    static Map<String, Resource> knownInterfaces = new HashMap<>();
    static Map<String, Resource> knownDatatypes = new HashMap<>();
    static Map<String, Resource> knownConstructors = new HashMap<>();

    static {
        try (InputStream is = GraphObserver.class.getResourceAsStream("/resources/prog.ttl")) {
            if (is != null) {
                String progModelString = new String(is.readAllBytes(), StandardCharsets.UTF_8);
                progModel.read(new StringReader(progModelString), null, "TURTLE");
            }
        } catch (IOException e) {
            log.warning("Failed to read the abs and program ontologies, RDF model only contains the runtime ontology");
        }
        String absns = absNamespaces.get("abs");
        progModel.listSubjectsWithProperty(RDF.type, progModel.createResource(absns + "class"))
            .forEach(
                classres -> {
                    if (classres.hasProperty(RDFS.label)) {
                        knownClasses.put(classres.getProperty(RDFS.label).getString(), classres);
                    }
                });
        progModel.listSubjectsWithProperty(RDF.type, progModel.createResource(absns + "interface"))
            .forEach(
                interfaceres -> {
                    if (interfaceres.hasProperty(RDFS.label)) {
                        knownInterfaces.put(interfaceres.getProperty(RDFS.label).getString(), interfaceres);
                    }
                });
        progModel.listSubjectsWithProperty(RDF.type, progModel.createResource(absns + "datatype"))
            .forEach(
                datatyperes -> {
                    if (datatyperes.hasProperty(RDFS.label)) {
                        knownDatatypes.put(datatyperes.getProperty(RDFS.label).getString(), datatyperes);
                    }
                });
        progModel.listSubjectsWithProperty(RDF.type, progModel.createResource(absns + "dataconstructor"))
            .forEach(
                constructorres -> {
                    if (constructorres.hasProperty(RDFS.label)) {
                        knownConstructors.put(constructorres.getProperty(RDFS.label).getString(), constructorres);
                    }
                });
    }

    // Note: iterate over the sets below using a snapshot to avoid gc
    // messing with us: `for (Object obj : Set.copyOf(objectSet)) { }`
    private static Set<ObjectView> objectSet = Collections.newSetFromMap(new WeakHashMap<ObjectView, Boolean>());
    private static Set<COGView> cogSet = Collections.newSetFromMap(new WeakHashMap<COGView, Boolean>());

    /**
     * Add the ABS standard namespaces to {@code model}.
     */
    public static void initNamespaces(Model model) {
        for (var e : absNamespaces.entrySet()) {
            model.setNsPrefix(e.getKey(), e.getValue());
        }
    }

    /**
     * Deactivate garbage collection of registered objects and sets,
     * and make these sets immmutable.  Should be done at the end of
     * the model run to preserve the ending state.
     */
    public static synchronized void freezeObserver() {
        objectSet = Set.copyOf(objectSet);
        cogSet = Set.copyOf(cogSet);
    }

    /**
     * Note an object to be added to the object graph.
     */
    public static synchronized void addObject(ObjectView o) {
        objectSet.add(o);
    }

    /**
     * Note a cog to be added to the object graph.
     */
    public static synchronized void addCog(COGView c) {
        cogSet.add(c);
    }

    @Override
    public void newCOGCreated(COGView cog, ObjectView initialObject) {
        cog.registerObjectCreationListener(this);
        cogSet.add(cog);
        this.objectCreated(initialObject);
    }

    @Override
    public void objectCreated(ObjectView o) {
        addObject(o);
    }

    @Override
    public void objectInitialized(ObjectView o) { }

    /**
     * Run a SPARQL query over the current ABS state.  The {@code
     * abs:}, {@code prog:} and {@code run:} namespaces are added to
     * the query string, so do not need to be defined.
     */
    public static List<QuerySolution> runQuery(Model model, String queryString) {
        queryString = sparqlPrefix + queryString;
        Query query = QueryFactory.create(queryString);
        try (QueryExecution qexec = QueryExecutionFactory.create(query, model)) {
            ResultSet results = qexec.execSelect();
            return ResultSetFormatter.toList(results);
        }
    }

    public static String runQuery(Model model, String queryString, Lang language) {
        queryString = sparqlPrefix + queryString;
        String result = "";
        Query query = QueryFactory.create(queryString);
        try (QueryExecution qexec = QueryExecutionFactory.create(query, model);
             ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
            ResultSet results = qexec.execSelect();
            ResultsWriter.create()
                .lang(language)
                // see ResultSetFormat.out(OutputStream, ResultSet, Prologue)
                // TODO: figure out why text format uses namespace prefixes but json, xml etc. don't
                .set(ARQConstants.symPrologue, new Prologue(query.getPrefixMapping()))
                .write(baos, results);
            result = baos.toString();
        } catch (IOException e) {
            log.warning("Caught exception closing a string output stream; ignoring it since this should not happen.");
        }
        return result;
    }

    /**
     * Print an RDF graph of the current ABS state, in TRTL format.
     */
    public static void printGraph(Model model) {
        model.write(System.out, "TURTLE");
    }

    /**
     * Return an RDF model of the current ABS state.
     */
    public static Model getModel() {
        Set<ObjectView> objects = Set.copyOf(objectSet);
        Set<COGView> cogs = Set.copyOf(cogSet);
        Model model = ModelFactory.createDefaultModel();
        initNamespaces(model);
        model.add(progModel);
        for (ObjectView view : objects) {
            addObjectTriples(model, view);
        }
        for (COGView view : cogs) {
            addCogTriples(model, view);
        }
        try (InputStream is = GraphObserver.class.getResourceAsStream("/resources/prog.ttl")) {
            if (is != null) {
                String progModelString = new String(is.readAllBytes(), StandardCharsets.UTF_8);
                Model progModel = ModelFactory.createDefaultModel();
                progModel.read(new StringReader(progModelString), null, "TURTLE");
                model.add(progModel);
            }
        } catch (IOException e) {
            log.warning("Failed to read the abs and program ontologies, RDF model only contains the runtime ontology");
        }
        return model;
    }

    public static void addCogTriples(Model model, COGView view) {
        COG cog = view.getCOG();
        ABSInterface dc = cog.getDC();
        String absNS = absNamespaces.get("abs");
        String runNS = absNamespaces.get("run");
        Resource cogRes = model.createResource(runNS + "cog" + cog.hashCode(),
            model.createResource(absNS + "cog"));
        Property inProp = model.createProperty(absNS + "in");
        cogRes.addProperty(inProp, model.createResource(runNS + "obj" + dc.hashCode()));
    }

    /**
     * Add all triples defining the given object to the model.
     */
    static void addObjectTriples(Model model, ObjectView view) {
        ABSObject obj = view.getObject();
        String packagename = obj.getClass().getPackageName();
        String type = packagename + "." + obj.getClassName();
        String absNS = absNamespaces.get("abs");
        String progNS = absNamespaces.get("prog");
        String runNS = absNamespaces.get("run");
        Resource classRes = knownClasses.getOrDefault(type, model.createResource(progNS + type));
        Resource objRes = model.createResource(runNS + "obj" + obj.hashCode(), classRes);
        Property inProp = model.createProperty(absNS + "in");
        objRes.addProperty(inProp, model.createResource(runNS + "cog" + obj.getCOG().hashCode()));
        for (String fieldName : obj.getFieldNames()) {
            Property fieldProp = model.createProperty(progNS + packagename + "." + fieldName);
            try {
                addFieldTriples(model, objRes, fieldProp, obj.getView().getFieldValue(fieldName));
            } catch (NoSuchFieldException e) {
                continue;
                // should never happen
            }
        }
    }

    /**
     * Queue entry for data structure reflection method
     */
    private record WorkItem(Resource resource, Property property, Object value) {}

    static void addFieldTriples(Model model, Resource res, Property fieldProp, Object value) {
        String runNS = absNamespaces.get("run");
        String progNS = absNamespaces.get("prog");

        Deque<WorkItem> queue = new ArrayDeque<>();
        queue.addLast(new WorkItem(res, fieldProp, value));

        while (!queue.isEmpty()) {
            WorkItem item = queue.removeFirst();
            Resource currentRes = item.resource;
            Property currentProp = item.property;
            Object currentValue = item.value;

            switch (currentValue) {
                case null:
                    currentRes.addLiteral(currentProp, model.createTypedLiteral("null"));
                    break;
                case Apint i:
                    currentRes.addLiteral(currentProp, model.createTypedLiteral(i.toBigInteger()));
                    break;
                case Aprational r:
                    currentRes.addLiteral(currentProp, model.createTypedLiteral(r.doubleValue()));
                    break;
                case ABSObject o2:
                    currentRes.addProperty(currentProp, model.createResource(runNS + "obj" + o2.hashCode()));
                    break;
                case ABSAlgebraicDataType a:
                    // Create the resource for this algebraic data type
                    String type = a.getClass().getPackageName() + "." + a.getConstructorName();
                    Resource cons = knownConstructors.getOrDefault(type, model.createResource(progNS + type));
                    Resource dataRes = model.createResource(cons);
                    // Link it to the parent
                    currentRes.addProperty(currentProp, dataRes);
                    // Add all of its arguments to the queue
                    for (int i = 0; i < a.getNumArgs(); i++) {
                        Property argProp = model.createProperty(progNS + "arg" + i);
                        Object argValue = a.getArg(i);
                        queue.addLast(new WorkItem(dataRes, argProp, argValue));
                    }
                    break;
                default:
                    currentRes.addLiteral(currentProp, model.createTypedLiteral(currentValue));
            }
        }
    }
}
