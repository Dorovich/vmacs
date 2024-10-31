<div align="center">
  <h1>
    <img src="https://www.gnu.org/savannah-checkouts/gnu/emacs/images/emacs.png" alt="GNU Emacs" width="120" height="120"/>
  </h1>
  Mi configuración personal de <b>GNU Emacs</b>, centrada en la programación en C/C++.
</div>

## ¿Por dónde empiezo?
Antes de nada debes *instalar* Emacs. Si ya lo has hecho recomiento que hagas el tutorial interactivo que incluye Emacs donde puedes aprender sus controles básicos. Para iniciarlo pulsa `C-h t`, es decir, “Control+h” seguido de “t”.

En esta configuración uso [Evil](https://github.com/emacs-evil/evil), un paquete que implementa los movimientos de Vim en Emacs, por lo que si ya estás familiarizado con ese programa te resultará fácil.

### Instalación (binarios)
Puedes instalar Emacs desde cualquier gestor de paquetes que haya en tu sistema operativo, o de otra distribución binaria. Puede que estas versiones precompiladas no incluyan algunas características como compilación nativa de emacs lisp, treesitter, u optimizaciones propias del compilador. Si te interesa tenerlas, sigue leyendo.


### Instalación (compilación propia)
Si no ha habido suerte con los binarios o prefieres ajustar a tu gusto las características de Emacs, puedes compilarlo desde su fuente. Para hacerlo puedes seguir estos breves pasos (como ejemplo):

```sh
$ git clone -b emacs-29 git://git.savannah.gnu.org/emacs.git
$ cd emacs
$ ./autogen.sh
$ CFLAGS="-march=native" CC=/usr/bin/gcc-12 CXX=/usr/bin/gcc-12
$ ./configure --with-modules --with-native-compilation --with-json
$ make -j $(nproc)
$ sudo make -j $(nproc) install
```

Asegúrate de tener las dependencias a mano. En Debian y derivados las esenciales son las siguientes (probablemente te hagan falta algunas más, dependiendo de las características que quieras):

```
build-essential gcc-12 g++-12 libgccjit0 libgccjit-12-dev autoconf libjansson4 libjansson-dev
```

## Inspiración
- Andrea Corallo. *[The original magic Emacs garbage collection hack](https://akrl.sdf.org/#orgc15a10d)*.
- David Wilson (aka *Systemcrafters*). *[Emacs From Scratch](https://systemcrafters.net/emacs-from-scratch/)*.
- Protesilaos Stavrou. *[My packages and/or custom code for GNU Emacs](https://protesilaos.com/emacs/)*.
- Derek Taylor (aka *DistroTube*). *[Configuring Emacs](https://www.youtube.com/playlist?list=PL5--8gKSku15e8lXf7aLICFmAHQVo0KXX)*.
- John Wiegley. *[A use-package declaration for simplifying your .emacs](https://jwiegley.github.io/use-package/)*.
- Emacs community @ Taiwan. *[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs)*.
- Karthinks. *([More](https://karthinks.com/software/more-batteries-included-with-emacs/)) [Batteries included with Emacs](https://karthinks.com/software/batteries-included-with-emacs/)*.
- Bozhidar Batsov. *[The Ultimate Collection of Emacs Resources](https://batsov.com/articles/2011/11/30/the-ultimate-collection-of-emacs-resources/)*.
- Abidán Brito. *[Build GNU Emacs from source](https://gist.github.com/abidanBrito/2b5e447f191bb6bb70c9b6fe6f9e7956#file-build-emacs-sh)*.
- Howard Abrams. *[My Emacs Configuration](https://howardabrams.com/hamacs/)*.
- Rahul M. Juliato. *[Emacs-Kick(starter)](https://github.com/LionyxML/emacs-kick)*
