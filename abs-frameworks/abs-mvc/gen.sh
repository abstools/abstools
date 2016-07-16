rm src/abs/model/*Db.abs
echo "Generate SQL Queries from ABS Models"
java -Xmx512m -jar ../../frontend/dist/absfrontend.jar -dbschema -product=Default src/abs/framework/Feature*.abs src/abs/framework/Products.abs src/abs/framework/ProductModelConfig.abs src/abs/model/*.abs src/abs/delta/DModel*.abs> gen.sql
echo "Generate SQL Queries Succesfully"
echo "Apply SQL Queries to Database"
mysql -u root abs_studikasus < gen.sql
echo "Apply SQL Queries Succesfully"
echo "=================================="
echo "Generate ABS Fli Code & Java Fli Code"
absc -flimodel src/abs/model/*.abs
echo "Move Generated ABS Fli Code to src/abs/model"
mv gen_abs_db_fli/* src/abs/model/
echo "Move Succesfully"
rm -rf gen_abs_db_fli
echo "Move Generated ABS Java Code to src/java/fli"
mv gen_java_db_fli/* src/java/fli/
echo "Move Succesfully"
rm -rf gen_java_db_fli
echo "Build App"
ant -Dabsproduct=Default abs.deploy
echo "Build App Succesfully"
