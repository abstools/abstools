/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import java.io.IOException;
import java.net.URISyntaxException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class ErlangModelApiTests extends AbstractModelApiTest {

    private static final ObjectMapper mapper = new ObjectMapper()
        .enable(JsonParser.Feature.ALLOW_SINGLE_QUOTES);

    protected JsonNode getValueFromResponse(String response) throws JsonMappingException, JsonProcessingException {
        JsonNode body = mapper.readTree(response);
        JsonNode result = body.at("/result");
        return result;
    }

    @Test
    public void test_intByURL() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        String response = sendGetRequest("/call/test/test_int?p=5", 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isIntegralNumber());
        int value = result.asInt();
        assertEquals(5, value);
    }

    @Test
    public void test_intByJson() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode().put("p", 5);
        String response = sendPostRequest("/call/test/test_int",
            parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isIntegralNumber());
        assertEquals(5, result.asInt());
    }

    @Test
    public void test_boolByURL() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        String response = sendGetRequest("/call/test/test_bool?p=true", 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isBoolean());
        assertTrue(result.asBoolean());
    }

    @Test
    public void test_boolByJson() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode().put("p", true);
        String response = sendPostRequest("/call/test/test_bool", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isBoolean());
        assertTrue(result.asBoolean());
    }

    @Test
    public void test_floatByURL() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        String response = sendGetRequest("/call/test/test_float?p=5.1", 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isFloatingPointNumber());
        float value = result.floatValue();
        assertEquals(5.1f, value, 0);
    }

    @Test
    public void test_floatByJson() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode().put("p", 5.1);
        String response = sendPostRequest("/call/test/test_float", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isFloatingPointNumber());
        float value = result.floatValue();
        assertEquals(5.1f, value, 0);
    }

    @Test
    public void test_stringByURL() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        String response = sendGetRequest("/call/test/test_string?p=lalala", 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isTextual());
        String value = result.asText();
        assertEquals("lalala", value);
    }

    @Test
    public void test_stringByJson() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode().put("p", "lalala");
        String response = sendPostRequest("/call/test/test_string", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isTextual());
        String value = result.asText();
        assertEquals("lalala", value);
    }

    @Test
    public void test_intList() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode()
            .set("p", mapper.createArrayNode()
                .add(1).add(2).add(3));
        String response = sendPostRequest("/call/test/test_list_int", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isArray());
        ArrayNode array = (ArrayNode)result;
        assertEquals(1, array.get(0).asInt());
        assertEquals(2, array.get(1).asInt());
        assertEquals(3, array.get(2).asInt());
    }

    @Test
    public void test_boolList() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode()
            .set("p", mapper.createArrayNode()
                .add(true).add(false));
        String response = sendPostRequest("/call/test/test_list_bool", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isArray());
        ArrayNode array = (ArrayNode)result;
        assertTrue(array.get(0).asBoolean());
        assertFalse(array.get(1).asBoolean());
    }

    @Test
    public void test_floatList() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode()
            .set("p", mapper.createArrayNode()
                .add(1.1).add(2.2).add(3.3));
        String response = sendPostRequest("/call/test/test_list_float", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isArray());
        ArrayNode array = (ArrayNode)result;
        assertEquals(1.1, array.get(0).floatValue(), 0.001);
        assertEquals(2.2, array.get(1).floatValue(), 0.001);
        assertEquals(3.3, array.get(2).floatValue(), 0.001);
    }

    @Test
    public void test_stringList() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode()
            .set("p", mapper.createArrayNode()
                .add("lalala").add("lololo"));
        String response = sendPostRequest("/call/test/test_list_string", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isArray());
        ArrayNode array = (ArrayNode)result;
        assertEquals("lalala", array.get(0).asText());
        assertEquals("lololo", array.get(1).asText());
    }

    @Test
    public void test_intMap() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode()
            .set("p", mapper.createObjectNode()
                .put("a", 1)
                .put("b", 2)
                .put("c", 3));
        String response = sendPostRequest("/call/test/test_map_int", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isObject());
        ObjectNode object = (ObjectNode)result;
        assertEquals(1, object.get("a").asInt());
        assertEquals(2, object.get("b").asInt());
        assertEquals(3, object.get("c").asInt());
    }

    @Test
    public void test_boolMap() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode()
            .set("p", mapper.createObjectNode()
                .put("a", true)
                .put("b", false)
                .put("c", true));
        String response = sendPostRequest("/call/test/test_map_bool", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isObject());
        ObjectNode object = (ObjectNode)result;
        assertTrue(object.get("a").asBoolean());
        assertFalse(object.get("b").asBoolean());
        assertTrue(object.get("c").asBoolean());
    }

    @Test
    public void test_floatMap() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode()
            .set("p",mapper.createObjectNode()
                .put("a", 1.1)
                .put("b", 2.2)
                .put("c", 3.3));
        String response = sendPostRequest("/call/test/test_map_float", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isObject());
        ObjectNode object = (ObjectNode)result;
        assertEquals(1.1, object.get("a").floatValue(), 0.001);
        assertEquals(2.2, object.get("b").floatValue(), 0.001);
        assertEquals(3.3, object.get("c").floatValue(), 0.001);
    }

    @Test
    public void test_stringMap() throws IOException, URISyntaxException {
        assumeTrue("Only meaningful with Model API support", driver.supportsModelApi());
        JsonNode parameter = mapper.createObjectNode()
            .set("p",mapper.createObjectNode()
                .put("a", "lalala")
                .put("b", "lololo")
                .put("c", "lululu"));
        String response = sendPostRequest("/call/test/test_map_string", parameter.toString(), 200);
        JsonNode result = getValueFromResponse(response);

        assertTrue(result.isObject());
        ObjectNode object = (ObjectNode)result;
        assertEquals("lalala", object.get("a").textValue());
        assertEquals("lololo", object.get("b").textValue());
        assertEquals("lululu", object.get("c").textValue());
    }
}
