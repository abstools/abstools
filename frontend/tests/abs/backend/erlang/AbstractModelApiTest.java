/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.erlang;

import static org.junit.Assert.assertFalse;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;

import org.junit.AfterClass;
import org.junit.BeforeClass;

import com.eclipsesource.json.Json;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;
import com.google.common.io.Files;

import abs.ABSTest;
import abs.backend.common.InternalBackendException;
import abs.common.WrongProgramArgumentException;
import abs.frontend.ast.Model;

public abstract class AbstractModelApiTest extends ABSTest {
    
    private enum RequestType {
        GET, POST;
    }
    
    // these fields have to be static in order to enable use of @BeforeClass and @AfterClass for initialization
    private static ErlangTestDriver driver = new ErlangTestDriver();
    private static Process serverProcess;
    private static int port_nr = -1;
    
    @BeforeClass
    public static void setup() throws IOException, WrongProgramArgumentException, InternalBackendException, InterruptedException {
        startModelApiServer(new File("tests/abssamples/backend/modelApi/modelapi_tests.abs"));
    }
    
    @AfterClass
    public static void teardown() {
        stopServer();
    }
    
    protected static void startModelApiServer(File file) throws IOException, WrongProgramArgumentException, InternalBackendException, InterruptedException {
        File tmpdir = Files.createTempDir();
        Model model = ABSTest.assertParseFileOk(file.getPath(), Config.WITH_STD_LIB, Config.TYPE_CHECK, Config.WITHOUT_MODULE_NAME);
        assertFalse(model.hasParserErrors());
        assertFalse(model.hasTypeErrors());
        String mainModule = driver.genCode(model, tmpdir, false);
        File runFile = new File(tmpdir, "run");

        // initialize with port number = 0 to get a random port
        ProcessBuilder pb = new ProcessBuilder(runFile.getAbsolutePath(), mainModule, "-p", "0");
        pb.directory(tmpdir);
        pb.redirectErrorStream(true);
        serverProcess = pb.start();
        
        extractPortNoFromProcess(serverProcess);
        
        // TODO: use thread pool?
//        Thread t = new Thread(new TimeoutThread(p));
//        t.start();
    }

    private static void extractPortNoFromProcess(Process process) throws IOException {
        String firstLine = "";
        try (BufferedReader in = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            // server provides port number in the first line in format "[text] port #####, [text]"
            // only the first line - if we read too far, we get stuck because process is still running
            firstLine = in.readLine();
        }
        int portNoStart = firstLine.indexOf("port ", 0) + 5; // index + length of search key
        int portNoEnd = firstLine.indexOf(",", 0);
        
        String portNoString = firstLine.substring(portNoStart, portNoEnd);
        port_nr = Integer.parseInt(portNoString);
    }
    
    protected static void stopServer() {
        if (port_nr != -1) {
            if(serverProcess == null || !serverProcess.isAlive()) {
                throw new IllegalStateException("Server is not running");
            }
            
            serverProcess.destroy();
            port_nr = -1;
        }
    }
    
    protected String sendGetRequest(String request) throws IOException {
        return sendRequest(request, RequestType.GET, null);
    }
    
    protected String sendPostRequest(String request, JsonValue payload) throws IOException {
        return sendRequest(request, RequestType.POST, payload.toString());
    }
    
    protected String sendPostRequest(String request, String payload) throws IOException {
        return sendRequest(request, RequestType.POST, payload);
    }
    
    private String sendRequest(String request, RequestType requestType, String payload) throws IOException {
        URL obj = new URL("http://localhost:" + Integer.toString(port_nr) + request);
        HttpURLConnection con = (HttpURLConnection) obj.openConnection();
        con.setRequestMethod(requestType.toString());
        if(RequestType.POST == requestType) {
            con.setDoOutput(true);
            con.setDoInput(true);
            con.setRequestProperty("Content-Type", "application/json");
            con.setRequestProperty("Accept", "application/json");
            try (OutputStreamWriter wr = new OutputStreamWriter(con.getOutputStream())) {
                wr.write(payload);
                wr.flush();
            }
        }
        con.setConnectTimeout(10000);
        con.connect();
        
        int responseCode = con.getResponseCode();
        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
        String inputLine;
        StringBuffer response = new StringBuffer();

        while ((inputLine = in.readLine()) != null) {
                response.append(inputLine);
        }
        in.close();
        return response.toString();
    }
    
    protected JsonValue getValueFromResponse(String response) {
        JsonValue value = Json.parse(response);
        JsonObject valueObject = value.asObject();
        JsonValue result = valueObject.get("result");
        return result;
    }
}
