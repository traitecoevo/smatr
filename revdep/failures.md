# ecostats ()

* GitHub: <https://github.com/traitecoevo/smatr>
* Email: <mailto:daniel.falster@unsw.edu.au>

Run `revdepcheck::revdep_details(, "ecostats")` for more info

## Error before installation

### Devel

```
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsElt.c -o dotsElt.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsLength.c -o dotsLength.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsNames.c -o dotsNames.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c init.c -o init.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o backports.so dotsElt.o dotsLength.o dotsNames.o init.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c base64.c -o base64.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dummy.c -o dummy.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c uriencode.c -o uriencode.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c utf8.c -o utf8.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o base64enc.so base64.o dummy.o uriencode.o utf8.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
...
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -g -O2  -c init.c -o init.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_doc.cpp -o xml2_doc.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -g -O2  -c xml2_init.c -o xml2_init.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_namespace.cpp -o xml2_namespace.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_node.cpp -o xml2_node.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_output.cpp -o xml2_output.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_schema.cpp -o xml2_schema.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_url.cpp -o xml2_url.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_xpath.cpp -o xml2_xpath.o
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o xml2.so connection.o init.o xml2_doc.o xml2_init.o xml2_namespace.o xml2_node.o xml2_output.o xml2_schema.o xml2_url.o xml2_xpath.o -lxml2 -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR


trying URL 'https://cran.rstudio.com/src/contrib/abind_1.4-8.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/ade4_1.7-24.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/adegenet_2.1.11.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/adephylo_1.1-17.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/alabama_2025.1.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/ape_5.8-1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/askpass_1.2.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/backports_1.5.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/base64enc_0.1-6.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/betareg_3.2-5.tar.gz'
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (xml2)


```
### CRAN

```
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsElt.c -o dotsElt.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsLength.c -o dotsLength.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsNames.c -o dotsNames.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c init.c -o init.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o backports.so dotsElt.o dotsLength.o dotsNames.o init.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c base64.c -o base64.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dummy.c -o dummy.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c uriencode.c -o uriencode.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c utf8.c -o utf8.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o base64enc.so base64.o dummy.o uriencode.o utf8.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
...
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -g -O2  -c init.c -o init.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_doc.cpp -o xml2_doc.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -g -O2  -c xml2_init.c -o xml2_init.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_namespace.cpp -o xml2_namespace.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_node.cpp -o xml2_node.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_output.cpp -o xml2_output.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_schema.cpp -o xml2_schema.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_url.cpp -o xml2_url.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG -I../inst/include  -DUCHAR_TYPE=wchar_t -DU_SHOW_CPLUSPLUS_API=0 -DSTRICT_R_HEADERS -DR_NO_REMAP  -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include   -fvisibility=hidden -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c xml2_xpath.cpp -o xml2_xpath.o
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o xml2.so connection.o init.o xml2_doc.o xml2_init.o xml2_namespace.o xml2_node.o xml2_output.o xml2_schema.o xml2_url.o xml2_xpath.o -lxml2 -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR


trying URL 'https://cran.rstudio.com/src/contrib/abind_1.4-8.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/ade4_1.7-24.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/adegenet_2.1.11.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/adephylo_1.1-17.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/alabama_2025.1.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/ape_5.8-1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/askpass_1.2.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/backports_1.5.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/base64enc_0.1-6.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/betareg_3.2-5.tar.gz'
...
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (xml2)


```
# ggpmisc ()

* GitHub: <https://github.com/traitecoevo/smatr>
* Email: <mailto:daniel.falster@unsw.edu.au>

Run `revdepcheck::revdep_details(, "ggpmisc")` for more info

## Error before installation

### Devel

```
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsElt.c -o dotsElt.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsLength.c -o dotsLength.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsNames.c -o dotsNames.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c init.c -o init.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o backports.so dotsElt.o dotsLength.o dotsNames.o init.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c base64.c -o base64.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dummy.c -o dummy.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c uriencode.c -o uriencode.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c utf8.c -o utf8.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o base64enc.so base64.o dummy.o uriencode.o utf8.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
...
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o ggrepel.so RcppExports.o repel_boxes.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c RcppExports.cpp -o RcppExports.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c bl-r-bindings.cpp -o bl-r-bindings.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c grid-renderer.cpp -o grid-renderer.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c grid.cpp -o grid.o
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o gridtext.so RcppExports.o bl-r-bindings.o grid-renderer.o grid.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/cpp11/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c cpp11.cpp -o cpp11.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/cpp11/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c marquee.cpp -o marquee.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/cpp11/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c md4c.c -o md4c.o
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o marquee.so cpp11.o marquee.o md4c.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR


trying URL 'https://cran.rstudio.com/src/contrib/askpass_1.2.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/backports_1.5.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/base64enc_0.1-6.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bitops_1.0-9.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/boot_1.3-32.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/brio_1.1.5.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/broom_1.0.13.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/broom.mixed_0.2.9.7.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bslib_0.11.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/cachem_1.1.0.tar.gz'
...
* installing *source* package ‘multcomp’ ...
** this is package ‘multcomp’ version ‘1.4-31’
** package ‘multcomp’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading


```
### CRAN

```
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsElt.c -o dotsElt.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsLength.c -o dotsLength.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsNames.c -o dotsNames.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c init.c -o init.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o backports.so dotsElt.o dotsLength.o dotsNames.o init.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c base64.c -o base64.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dummy.c -o dummy.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c uriencode.c -o uriencode.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c utf8.c -o utf8.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o base64enc.so base64.o dummy.o uriencode.o utf8.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
...
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o ggrepel.so RcppExports.o repel_boxes.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c RcppExports.cpp -o RcppExports.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c bl-r-bindings.cpp -o bl-r-bindings.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c grid-renderer.cpp -o grid-renderer.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c grid.cpp -o grid.o
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o gridtext.so RcppExports.o bl-r-bindings.o grid-renderer.o grid.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/cpp11/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c cpp11.cpp -o cpp11.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/cpp11/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c marquee.cpp -o marquee.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/ggpmisc/cpp11/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c md4c.c -o md4c.o
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o marquee.so cpp11.o marquee.o md4c.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR


trying URL 'https://cran.rstudio.com/src/contrib/askpass_1.2.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/backports_1.5.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/base64enc_0.1-6.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bitops_1.0-9.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/boot_1.3-32.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/brio_1.1.5.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/broom_1.0.13.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/broom.mixed_0.2.9.7.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bslib_0.11.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/cachem_1.1.0.tar.gz'
...
* installing *source* package ‘multcomp’ ...
** this is package ‘multcomp’ version ‘1.4-31’
** package ‘multcomp’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading


```
# httk ()

* GitHub: <https://github.com/traitecoevo/smatr>
* Email: <mailto:daniel.falster@unsw.edu.au>

Run `revdepcheck::revdep_details(, "httk")` for more info

## Error before installation

### Devel

```
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsElt.c -o dotsElt.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsLength.c -o dotsLength.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsNames.c -o dotsNames.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c init.c -o init.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o backports.so dotsElt.o dotsLength.o dotsNames.o init.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c base64.c -o base64.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dummy.c -o dummy.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c uriencode.c -o uriencode.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c utf8.c -o utf8.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o base64enc.so base64.o dummy.o uriencode.o utf8.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
...
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/fn_selectors.o src/fn_selectors.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/color_maps.o src/color_maps.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/environment.o src/environment.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/ast_fwd_decl.o src/ast_fwd_decl.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/bind.o src/bind.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/file.o src/file.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/util.o src/util.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/util_string.o src/util_string.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/json.o src/json.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/units.o src/units.cpp


trying URL 'https://cran.rstudio.com/src/contrib/abind_1.4-8.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/askpass_1.2.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/backports_1.5.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/base64enc_0.1-6.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bit_4.6.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bit64_4.8.2.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bitops_1.0-9.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/blob_1.3.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/boot_1.3-32.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/brio_1.1.5.tar.gz'
...
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (scales)


```
### CRAN

```
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsElt.c -o dotsElt.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsLength.c -o dotsLength.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsNames.c -o dotsNames.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c init.c -o init.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o backports.so dotsElt.o dotsLength.o dotsNames.o init.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c base64.c -o base64.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dummy.c -o dummy.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c uriencode.c -o uriencode.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c utf8.c -o utf8.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o base64enc.so base64.o dummy.o uriencode.o utf8.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
...
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/fn_selectors.o src/fn_selectors.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/color_maps.o src/color_maps.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/environment.o src/environment.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/ast_fwd_decl.o src/ast_fwd_decl.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/bind.o src/bind.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/file.o src/file.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/util.o src/util.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/util_string.o src/util_string.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/json.o src/json.cpp
clang++ -ftemplate-depth-256 -Wall -O2 -std=c++11 -I ./include  -stdlib=libc++ -fPIC -c -o src/units.o src/units.cpp


trying URL 'https://cran.rstudio.com/src/contrib/abind_1.4-8.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/askpass_1.2.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/backports_1.5.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/base64enc_0.1-6.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bit_4.6.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bit64_4.8.2.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bitops_1.0-9.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/blob_1.3.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/boot_1.3-32.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/brio_1.1.5.tar.gz'
...
** R
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (scales)


```
# httkexamples ()

* GitHub: <https://github.com/traitecoevo/smatr>
* Email: <mailto:daniel.falster@unsw.edu.au>

Run `revdepcheck::revdep_details(, "httkexamples")` for more info

## Error before installation

### Devel

```
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsElt.c -o dotsElt.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsLength.c -o dotsLength.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsNames.c -o dotsNames.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c init.c -o init.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o backports.so dotsElt.o dotsLength.o dotsNames.o init.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c base64.c -o base64.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dummy.c -o dummy.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c uriencode.c -o uriencode.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c utf8.c -o utf8.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o base64enc.so base64.o dummy.o uriencode.o utf8.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
...
ar rcs bcrypt/libstatbcrypt.a bcrypt/bcrypt_pbkdf.o bcrypt/blowfish.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o openssl.so aes.o base64.o bignum.o cert.o compatibility.o diffie.o envelope.o error.o hash.o info.o keygen.o keys.o onload.o openssh.o password.o pbkdf.o pem.o pkcs12.o pkcs7.o rand.o rsa.o signing.o ssl.o stream.o write.o x25519.o -Lbcrypt -lstatbcrypt -L/opt/homebrew/Cellar/openssl@3/3.6.3/lib -lssl.3 -lcrypto.3 -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c RcppExports.cpp -o RcppExports.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c helper_functions.cpp -o helper_functions.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c load_workbook.cpp -o load_workbook.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c read_workbook.cpp -o read_workbook.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c write_data.cpp -o write_data.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c write_file.cpp -o write_file.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c write_file_2.cpp -o write_file_2.o
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o openxlsx.so RcppExports.o helper_functions.o load_workbook.o read_workbook.o write_data.o write_file.o write_file_2.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR


trying URL 'https://cran.rstudio.com/src/contrib/abind_1.4-8.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/askpass_1.2.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/backports_1.5.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/base64enc_0.1-6.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bdsmatrix_1.3-7.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/BiocManager_1.30.27.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bit_4.6.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bit64_4.8.2.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bitops_1.0-9.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/blob_1.3.0.tar.gz'
...
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (openssl)
* installing *source* package ‘openxlsx’ ...
** this is package ‘openxlsx’ version ‘4.2.8.1’
** package ‘openxlsx’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.1.1.101)’
using SDK: ‘MacOSX26.5.sdk’


```
### CRAN

```
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsElt.c -o dotsElt.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsLength.c -o dotsLength.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dotsNames.c -o dotsNames.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c init.c -o init.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o backports.so dotsElt.o dotsLength.o dotsNames.o init.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c base64.c -o base64.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c dummy.c -o dummy.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c uriencode.c -o uriencode.o
clang -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG   -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -g -O2  -c utf8.c -o utf8.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o base64enc.so base64.o dummy.o uriencode.o utf8.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
...
ar rcs bcrypt/libstatbcrypt.a bcrypt/bcrypt_pbkdf.o bcrypt/blowfish.o
clang -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o openssl.so aes.o base64.o bignum.o cert.o compatibility.o diffie.o envelope.o error.o hash.o info.o keygen.o keys.o onload.o openssh.o password.o pbkdf.o pem.o pkcs12.o pkcs7.o rand.o rsa.o signing.o ssl.o stream.o write.o x25519.o -Lbcrypt -lstatbcrypt -L/opt/homebrew/Cellar/openssl@3/3.6.3/lib -lssl.3 -lcrypto.3 -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c RcppExports.cpp -o RcppExports.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c helper_functions.cpp -o helper_functions.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c load_workbook.cpp -o load_workbook.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c read_workbook.cpp -o read_workbook.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c write_data.cpp -o write_data.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c write_file.cpp -o write_file.o
clang++ -ftemplate-depth-256 -I"/opt/homebrew/Cellar/r/4.6.0/lib/R/include" -DNDEBUG  -I'/Users/z2209343/GitHub/packages/smatr/revdep/library.noindex/httkexamples/Rcpp/include' -I/opt/homebrew/opt/gettext/include -I/opt/homebrew/opt/readline/include -I/opt/homebrew/opt/xz/include -I/opt/homebrew/include    -fPIC  -O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function  -c write_file_2.cpp -o write_file_2.o
clang++ -ftemplate-depth-256 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -L/opt/homebrew/opt/gettext/lib -L/opt/homebrew/opt/readline/lib -L/opt/homebrew/opt/xz/lib -L/opt/homebrew/lib -o openxlsx.so RcppExports.o helper_functions.o load_workbook.o read_workbook.o write_data.o write_file.o write_file_2.o -L/opt/homebrew/Cellar/r/4.6.0/lib/R/lib -lR


trying URL 'https://cran.rstudio.com/src/contrib/abind_1.4-8.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/askpass_1.2.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/backports_1.5.1.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/base64enc_0.1-6.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bdsmatrix_1.3-7.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/BiocManager_1.30.27.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bit_4.6.0.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bit64_4.8.2.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/bitops_1.0-9.tar.gz'
trying URL 'https://cran.rstudio.com/src/contrib/blob_1.3.0.tar.gz'
...
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (openssl)
* installing *source* package ‘openxlsx’ ...
** this is package ‘openxlsx’ version ‘4.2.8.1’
** package ‘openxlsx’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 21.0.0 (clang-2100.1.1.101)’
using SDK: ‘MacOSX26.5.sdk’


```
