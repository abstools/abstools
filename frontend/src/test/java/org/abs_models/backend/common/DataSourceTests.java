/** 
 * Copyright (c) 2021, Rudolf Schlatte.
 * This file is licensed under the terms of the Modified BSD License.
 */
package org.abs_models.backend.common;

import java.io.File;

import org.abs_models.ABSTest;
import org.abs_models.backend.BackendTestDriver;
import org.junit.Assume;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class DataSourceTests extends SemanticTests {

    public DataSourceTests(BackendTestDriver d) {
        super(d);
    }

    @Test
    public void testQueryContainingStringLiteral() throws Exception {
        ABSTest.assertParse(
            """
            def List<String> fstring_with_literal() = builtin(sqlite3,
            "test.sqlite3",
            `SELECT string_value FROM test_table WHERE string_value = "hello"`);
            """);
    }

    @Test
    public void readFromSQLite3() throws Exception {
        Assume.assumeTrue("Only meaningful with SQLite support", driver.supportsSQLite());
        assertEvalTrueWithTestfiles(new File("abssamples/backend/DatasourceTests/sqlite3.abs"),
                                    new File("sqlite3/test.sqlite3"));
     }

    @Test
    public void readFromSQLite3WithParameters() throws Exception {
        Assume.assumeTrue("Only meaningful with SQLite support", driver.supportsSQLite());
        assertEvalTrueWithTestfiles(new File("abssamples/backend/DatasourceTests/sqlite3_parameters.abs"),
            new File("sqlite3/test.sqlite3"));
    }

    @Test
    public void readFromSQLite3WithFunctions() throws Exception {
        Assume.assumeTrue("Only meaningful with SQLite support", driver.supportsSQLite());
        assertEvalTrueWithTestfiles(new File("abssamples/backend/DatasourceTests/sqlite3_functioncall_parameters.abs"),
            new File("sqlite3/test.sqlite3"));
    }
}
