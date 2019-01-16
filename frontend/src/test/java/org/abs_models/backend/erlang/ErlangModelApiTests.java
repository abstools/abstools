/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.erlang;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

import com.eclipsesource.json.Json;
import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;

public class ErlangModelApiTests extends AbstractModelApiTest {
  
    @Test
    public void test_intByURL() throws IOException {
        String response = sendGetRequest("/call/test/test_int?p=5", 200);
        JsonValue result = getValueFromResponse(response);
 
        int value = result.asInt();
        Assert.assertEquals(5, value);
    }
    
    @Test
    public void test_intByJson() throws IOException {
        JsonObject parameter = Json.object().add("p", 5);
        String response = sendPostRequest("/call/test/test_int", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        int value = result.asInt();
        Assert.assertEquals(5, value);
    }
    
    @Test
    public void test_boolByURL() throws IOException {
        String response = sendGetRequest("/call/test/test_bool?p=true", 200);
        JsonValue result = getValueFromResponse(response);

        boolean value = result.asBoolean();
        Assert.assertTrue(value);
    }
    
    @Test
    public void test_boolByJson() throws IOException {
        JsonObject parameter = Json.object().add("p", true);
        String response = sendPostRequest("/call/test/test_bool", parameter, 200);
        JsonValue result = getValueFromResponse(response);

        boolean value = result.asBoolean();
        Assert.assertTrue(value);
    }
    
    @Test
    public void test_floatByURL() throws IOException {
        String response = sendGetRequest("/call/test/test_float?p=5.1", 200);
        JsonValue result = getValueFromResponse(response);

        float value = result.asFloat();
        Assert.assertEquals(5.1f, value, 0);
    }
    
    @Test
    public void test_floatByJson() throws IOException {
        JsonObject parameter = Json.object().add("p", 5.1);
        String response = sendPostRequest("/call/test/test_float", parameter, 200);
        JsonValue result = getValueFromResponse(response);

        float value = result.asFloat();
        Assert.assertEquals(5.1f, value, 0);
    }
    
    @Test
    public void test_stringByURL() throws IOException {
        String response = sendGetRequest("/call/test/test_string?p=lalala", 200);
        JsonValue result = getValueFromResponse(response);

        String value = result.asString();
        Assert.assertEquals("lalala", value);
    }
    
    @Test
    public void test_stringByJson() throws IOException {
        JsonObject parameter = Json.object().add("p", "lalala");
        String response = sendPostRequest("/call/test/test_string", parameter, 200);
        JsonValue result = getValueFromResponse(response);

        String value = result.asString();
        Assert.assertEquals("lalala", value);
    }
    
    @Test
    public void test_intList() throws IOException {
        JsonObject parameter = Json.object().add("p", Json.array(1, 2, 3));
        String response = sendPostRequest("/call/test/test_list_int", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        JsonArray array = result.asArray();
        Assert.assertEquals(1, array.get(0).asInt());
        Assert.assertEquals(2, array.get(1).asInt());
        Assert.assertEquals(3, array.get(2).asInt());
    }
    
    @Test
    public void test_boolList() throws IOException {
        JsonObject parameter = Json.object().add("p", Json.array(true, false));
        String response = sendPostRequest("/call/test/test_list_bool", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        JsonArray array = result.asArray();
        Assert.assertTrue(array.get(0).asBoolean());
        Assert.assertFalse(array.get(1).asBoolean());
    }
    
    @Test
    public void test_floatList() throws IOException {
        JsonObject parameter = Json.object().add("p", Json.array(1.1, 2.2, 3.3));
        String response = sendPostRequest("/call/test/test_list_float", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        JsonArray array = result.asArray();
        Assert.assertEquals(1.1, array.get(0).asFloat(), 0.001);
        Assert.assertEquals(2.2, array.get(1).asFloat(), 0.001);
        Assert.assertEquals(3.3, array.get(2).asFloat(), 0.001);
    }
    
    @Test
    public void test_stringList() throws IOException {
        JsonObject parameter = Json.object().add("p", Json.array("lalala", "lololo"));
        String response = sendPostRequest("/call/test/test_list_string", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        JsonArray array = result.asArray();
        Assert.assertEquals("lalala", array.get(0).asString());
        Assert.assertEquals("lololo", array.get(1).asString());
    }

    @Test
    public void test_intMap() throws IOException {
        JsonObject parameter = Json.object().add("p",
                                                 Json.object().add("a", 1)
                                                 .add("b", 2)
                                                 .add("c", 3));
        String response = sendPostRequest("/call/test/test_map_int", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        JsonObject object = result.asObject();
        Assert.assertEquals(1, object.get("a").asInt());
        Assert.assertEquals(2, object.get("b").asInt());
        Assert.assertEquals(3, object.get("c").asInt());
    }
    
    @Test
    public void test_boolMap() throws IOException {
        JsonObject parameter = Json.object().add("p",
                                                 Json.object().add("a", true)
                                                 .add("b", false)
                                                 .add("c", true));
        String response = sendPostRequest("/call/test/test_map_bool", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        JsonObject object = result.asObject();
        Assert.assertTrue(object.get("a").asBoolean());
        Assert.assertFalse(object.get("b").asBoolean());
        Assert.assertTrue(object.get("c").asBoolean());
    }
    
    @Test
    public void test_floatMap() throws IOException {
        JsonObject parameter = Json.object().add("p",
                                                 Json.object().add("a", 1.1)
                                                 .add("b", 2.2)
                                                 .add("c", 3.3));
        String response = sendPostRequest("/call/test/test_map_float", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        JsonObject object = result.asObject();
        Assert.assertEquals(1.1, object.get("a").asFloat(), 0.001);
        Assert.assertEquals(2.2, object.get("b").asFloat(), 0.001);
        Assert.assertEquals(3.3, object.get("c").asFloat(), 0.001);
    }
    
    @Test
    public void test_stringMap() throws IOException {
        JsonObject parameter = Json.object().add("p",
                                                 Json.object().add("a", "lalala")
                                                 .add("b", "lololo")
                                                 .add("c", "lululu"));
        String response = sendPostRequest("/call/test/test_map_string", parameter.toString(), 200);
        JsonValue result = getValueFromResponse(response);

        JsonObject object = result.asObject();
        Assert.assertEquals("lalala", object.get("a").asString());
        Assert.assertEquals("lololo", object.get("b").asString());
        Assert.assertEquals("lululu", object.get("c").asString());
    }



}
