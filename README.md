## kyotocaml  


### What is *kyotocaml*?
*kyotocaml* is an ocaml-binding for the *Kyoto Cabinet* library.  
*Kyoto Cabinet* is a dbm-like database engine (key-value store).  
For informations about *Kyoto Cabinet* visit the [FAL Labs Homepage](http://fallabs.com/kyotocabinet/).

*kyotocaml* implements wrapper functions for *Kyoto Cabinet* in a shared library and implements two interface-modules to this layer:
* Kyotocaml: one-to-one interface for the most functions defined in <kclangc.h>
* Kcdbm: simple (Caml)Dbm-like interface

### Status
* The bindings for most (of the important) functions are implemented  
* Most functions are (successfully) tested with (O-)Unit-tests  
* The interface of the library is somewhat stable; probably there will only be additions in future  
* Documentation is still incomplete  
* till today the build process was only tested in one os-environment (Ubuntu 12.04 64bit)

### Version
0.31 (beta)

### Building *kyotocaml*
Requirements:  
* Ocaml Version 3.12 or later (older 3.x versions may work too)  
* Kyoto Cabinet version 1.2.76 (older versions: may work, but not tested yet)  
* make, gcc (version: somewhat up-to-date)  
* to perform the tests in ./tests the library OUnit must be installed (see [OUnit homepage](http://ounit.forge.ocamlcore.org/))  
* It is assumed that Kyoto Cabinet is installed in directory /usr/local/lib; otherwise ./src/Makefile must be edited (Variable KCPATH).  

If OUnit is installed, start the build process with
> make all  

If there is no OUnit installed on your system or you dont't want to build tests, type
> make all-without-tests

Then type
> [sudo] make intstall

The kyotocaml-files will be installed in a new created subdirectory of your ocaml library standard directory 
(usally /usr/lib/ocaml/kyotocaml).

To generate documentation type (HTML-format)
> make htdoc

The doc-files will be stored in ./doc/...

If you want to start a clean build process from the beginning type first
> make clean-all

To remove kyotocaml installation type 
> [sudo] make unintstall

### License
*kyotocaml* is distributed under the GNU LESSER GENERAL PUBLIC LICENSE (see LICENSE file).  
For *Kyoto Cabinet* license see the [FAL Labs Homepage/license](http://fallabs.com/kyotocabinet/spex.html#license) and read the corrresponding files of the *Kyoto Cabinet* source package.  

