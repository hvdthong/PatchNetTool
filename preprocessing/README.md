This directory is concerned with the preprocessing of patches.  The tool
here produces a representation of data that can be passed to the tool found
in the deeplearning directory.  This tool is standalone because in practice
it may be useful to perform the preprocessing once, and then experiment
with the hyperparameters of the deep learning model to improve the results.

The following installation has been tested on Ubuntu 16.4 and 18.4 systems.

The preprocessing requires that the following software be installed:

OCaml: https://opam.ocaml.org/
Coccinelle: https://github.com/coccinelle/coccinelle
Parmap: https://github.com/rdicosmo/parmap

Parmap can also be installed using opam:

https://opam.ocaml.org/
https://opam.ocaml.org/packages/parmap/

The use of this code on Windows is discouraged, as parmap (and thus
parallelism) will not be available.

The preprocessor assumes the availability of python3 and the following
python3 libraries.

nltk: pip3 install nltk
enchant: pip3 install pyenchant

In python, it is necessary to run

>>> import nltk
>>> nltk.download('stopwords')

once before running the preprocessor.

---------------------------------

A typical command line is

   ./getinfo --commit-list <commit list> --git <git path> -o <prefix>

It is possible to specify --nolog to ignore the commit logs, --balance to
produce a balanced dataset by discarding some elements, and -j to indicate
the number of cores to use.  The default number of cores is 4.

This produces the files <prefix>.tmp (intermediate file), <prefix>.out
(representation of commits) and <prefix>.dict (dictionary for the commits).
Only <prefix>.out has to be provided to the deep learning process via the
--data command line argument.

For simplicity, the commit list requires labels for both training and
production data, but for production data the labels are ignored.

testdata in the current directory is a sample labelled list of commits.  These
commits are found in the Linux kernel, available at:

git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git

---------------------------------

All code found in this directory or any subdirectory is covered by GPLv2,
as described in license.txt.
The source code of PatchNet is available at
https://github.com/hvdthong/PatchNetTool
