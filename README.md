Peercoin POS/POW calculator
===========================

Proof-Of-Stake (POS) and Proof-Of-Work (POW) reward and probability calculator for Peercoin.

## Details

This program gives you the probability of generating a POS or POW block within 10 minutes, 24 hours, 31 days, 90 days and 1 year, as well as the reward that can be expected.

Written in Common Lisp.

The GUI is using Qt.

## Dependencies

* A Common Lisp implementation. Tested with:
  * [sbcl](http://www.sbcl.org/)
  * [clozurecl](http://ccl.clozure.com/)
  * [ecl](http://ecls.sourceforge.net/)
* [CommonQt](http://common-lisp.net/project/commonqt/).

## Installation

* Install CommonQt, the instructions can be found on [http://common-lisp.net/project/commonqt/](http://common-lisp.net/project/commonqt/).
* Copy the source code of the peercoin calculator where you want it to be.
* Tell your Common Lisp implementation where to find the sources:
  * ```(push "directory-where-the-sources-are/" asdf:*central-registry*)```
  * If you don't want to type this line every time, you can add it to the initialization file (e.g.: .sbclrc, .ccl-init.lisp, .eclrc).

## Start

To start the GUI:

    (require :peercoin-calculator)
    (peercoin-calculator:gui)

Other available functions in the peercoin-calculator package:

    (pos-reward coins days)
    (pos-prob-day coins days difficulty)
    (pos-prob-days coins days difficulty n)
    (pow-reward difficulty)
    (pow-prob-time difficulty rate time)

## Donations

If you find this program useful and want to make a donation, you can send coins to the following Peercoin address: **PWFNV1Cvq7nQBRyRueuYzwmDNXUGpgNkBC**.
