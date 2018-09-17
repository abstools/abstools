/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import static org.junit.Assert.assertFalse;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.WrongProgramArgumentException;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;

import com.eclipsesource.json.Json;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;
import com.google.common.io.Files;

import org.abs_models.ABSTest;
import org.abs_models.frontend.ast.Model;

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
        startModelApiServer(new File("abssamples/backend/ModelApi/modelapi_tests.abs"));
    }
    
    @AfterClass
    public static void teardown() {
        stopServer();
    }
    
    protected static void startModelApiServer(File file) throws IOException, WrongProgramArgumentException, InternalBackendException, InterruptedException {
        File tmpdir = Files.createTempDir();
        tmpdir.deleteOnExit();
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
    }

    private static void extractPortNoFromProcess(Process process) throws IOException {
        final Pattern portnr_pattern = Pattern.compile("Starting server on port (\\d+), abort with Ctrl-C");
        String line = "";
        String port_nr_s = null;
        BufferedReader in = new BufferedReader(new InputStreamReader(process.getInputStream()));
        while (port_nr_s == null) {
            line = in.readLine();
            System.out.println(line);
            Matcher matcher = portnr_pattern.matcher(line);
            if (matcher.find()) {
                port_nr_s = matcher.group(1);
            }
        }
        port_nr = Integer.parseInt(port_nr_s);
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
    
    protected String sendGetRequest(String request, int expected_response) throws IOException {
        return sendRequest(request, RequestType.GET, null, expected_response);
    }
    
    protected String sendPostRequest(String request, JsonValue payload, int expected_response) throws IOException {
        return sendRequest(request, RequestType.POST, payload.toString(), expected_response);
    }
    
    protected String sendPostRequest(String request, String payload, int expected_response) throws IOException {
        return sendRequest(request, RequestType.POST, payload, expected_response);
    }
    
    private String sendRequest(String request, RequestType requestType, String payload, int expected_response) throws IOException {
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

        if (con.getResponseCode() != expected_response) {
            Assert.fail("Expected response code " + expected_response + ", got " + con.getResponseCode());
        }
        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
        String response = in.lines().collect(Collectors.joining());
        in.close();
        return response;
    }
    
    protected JsonValue getValueFromResponse(String response) {
        JsonValue value = Json.parse(response);
        JsonObject valueObject = value.asObject();
        JsonValue result = valueObject.get("result");
        return result;
    }
}
