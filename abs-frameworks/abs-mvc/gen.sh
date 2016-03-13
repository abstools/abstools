echo "Generating SQL Queries"
java -Xmx512m -jar ../../frontend/dist/absfrontend.jar -dbschema -product=Default src/abs/framework/Feature*.abs src/abs/framework/Products.abs src/abs/framework/ProductModelConfig.abs src/abs/model/*.abs src/abs/delta/DModel*.abs> gen.sql
echo "Generating SQL Queries Succesfully"
#echo "Applying SQL Queries to Database"
mysql -u root abs_studikasus < gen.sql
#echo "Applying SQL Queries Succesfully"
