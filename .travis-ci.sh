# This script is due to Anil Madhavapeddy
# http://anil.recoil.org/2013/09/30/travis-and-ocaml.html
# https://gist.github.com/avsm/6757425
#########################################################

# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind xml-light"

case "$OCAML_VERSION,$OPAM_VERSION" in
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
4.01.0,1.2.1) ppa=avsm/ocaml41+opam12 ;;
4.02.1,1.1.2) ppa=avsm/ocaml42+opam11 ;;
4.02.3,1.2.2) ppa=avsm/ocaml42+opam12 ;;
4.02.1,1.2.0) ppa=avsm/ocaml42+opam120 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers opam
sudo apt-get install graphviz
export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version
opam init 
opam install ${OPAM_DEPENDS}
eval `opam config env`

# Install new version of graphviz from source
#bash install-graphviz-2.40.1.sh
#dot -V

export OS=amd64-linux
export JAVA_HEAP_SIZE=3g
export MEMALLOY_ROOT_DIR="$( pwd )"

make
make moretests
