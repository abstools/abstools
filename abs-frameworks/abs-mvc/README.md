ABS MVC Framework
Created by Salman
Modified by Kandito
+ Add ORM to connect DBMS
+ Possibility to run as standalone server, or as Tomcat's application (jar)

TODO:
- Integrate with Niken's scheme generator

More about ABS MVC Framework:

[http://fmse.cs.ui.ac.id/?open=abs/absmvc/index](http://fmse.cs.ui.ac.id/?open=abs/absmvc/index)

[https://github.com/sir-muamua/ABSMVCFramework](https://github.com/sir-muamua/ABSMVCFramework)



# ABS MVC Training
ABS MVC Case Study for ABS Training Session at Faculty of Computer Science Universitas Indonesia.

## Struktur direktor
* src/abs/controller: merupakan direktori yang digunakan untuk meletakan modul Controller ABS yang dibuat.
* src/abs/view: merupakan direktori yang digunakan untuk menyimpan halaman HTML yang dibuat.
* src/abs/model: merupakan direktori yang digunakan untuk meletakan setiap modul model yang dibuat.
* src/abs/framewok: merupakan direktori yang digunakan untuk meletakan modul internal framework yang dibuat oleh penulis seperti modul
ABSHttpRequest, RouteConfig, ProductConfig dan Products
* src/abs/delta: merupakan direktori yang digunakan untuk meletakan seluruh kode delta yang dibuat pada saat menerapkan delta modeling dalam pengembangan aplikasi.

## Compile & Run
Untuk melakukan proses kompilasi kode ABS yang dibuat silahkan masuk kedalam direktori folder dan jalankan perintah:
>	ant -Dabsproduct=Default abs.deploy

untuk dapat menjalankan perintah diatas diperlukan program tambahan yang bernama [Apache Ant](http://ant.apache.org/bindownload.cgi). Perintah tersebut akan menghasilkan sebuah file bernama `app.jar` di dalam direktori `web`. Setelah proses kompilasi selesai, langkah berikutnya adalah menjalankan ABSServer dengan mengetikkan perintah:
>	java -jar absserver.jar

Untuk dapat melihat aplikasi web yang telah berhasil dibuat, silahkan buka web browser dan masukkan alamat `http://localhost:8080`