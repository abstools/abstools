/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import org.abs_models.ABSTest;
import org.abs_models.backend.common.InternalBackendException;
import org.abs_models.common.WrongProgramArgumentException;
import org.junit.AfterClass;
import org.junit.BeforeClass;

public abstract class AbstractModelApiTest extends ABSTest {

    // these fields have to be static in order to enable use of @BeforeClass and @AfterClass for initialization
    protected static ErlangTestDriver driver = new ErlangTestDriver();

    @BeforeClass
    public static void setup() throws IOException, WrongProgramArgumentException, InternalBackendException, InterruptedException {
        driver.startModelApi(new File("abssamples/backend/ModelApi/modelapi_tests.abs"));
    }

    @AfterClass
    public static void teardown() {
        driver.shutdownModelApi();
    }

    protected String sendGetRequest(String request, int expected_response) throws IOException, URISyntaxException {
        return driver.sendGetRequest(request, expected_response);
    }

    // protected String sendPostRequest(String request, JsonNode payload, int expected_response) throws IOException {
    //     return sendRequest(request, RequestType.POST, payload.toString(), expected_response);
    // }

    protected String sendPostRequest(String request, String payload, int expected_response) throws IOException, URISyntaxException {
        return driver.sendPostRequest(request, payload, expected_response);
    }

}
