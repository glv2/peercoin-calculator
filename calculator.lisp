#|
Copyright 2014-2015 Guillaume LE VAILLANT

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#


(in-package :peercoin-calculator)


(defconstant +max-hash+ (coerce (expt 2 256) 'long-float))
(defconstant +max-target+ (coerce (expt 2 224) 'long-float))


(defun prob-or (p n)
  "Calculate the probability that an event of probability P occurs at least once in N attempts."
  (cond ((zerop n) 0)
        ((= n 1) p)
        ((evenp n) (prob-or (- (+ p p) (* p p)) (/ n 2)))
        ((oddp n) (let ((prob (prob-or (- (+ p p) (* p p)) (floor n 2))))
                    (- (+ prob p) (* prob p))))))


;;; Proof-Of-Stake

(defconstant +min-age+ 30)
(defconstant +max-age+ 90)

(defun pos-reward (coins days)
  "Calculate the reward for a block minted with a coin-age of COINS * DAYS."
  (if (> days +min-age+)
    (* 0.01 (/ (* days coins 33) (+ (* 365 33) 8)))
    0))

(defun pos-prob-hash (coins days difficulty)
  "Calculate the probability of a hash test of some DIFFICULTY passing with a coin-age of COINS * DAYS."
  (let ((prob 0) target day-weight)
    (when (> days +min-age+)
      (setf target (/ +max-target+ difficulty))
      (setf day-weight (- (min days +max-age+) +min-age+))
      (setf prob (/ (* target coins day-weight) +max-hash+)))
    (coerce prob 'long-float)))

(defun pos-prob-hashes (coins days difficulty n)
  "Calculate the probability of at least one hash test of some DIFFICULTY passing with a coin-age of COINS * DAYS in N tests."
  (let ((prob (pos-prob-hash coins days difficulty)))
    (prob-or prob n)))

(defun pos-prob-day (coins days difficulty)
  "Calculate the probability of minting a proof-of-stake block within one day with a coin-age of COINS * DAYS given some proof-of-stake DIFFICULTY."
  (pos-prob-hashes coins days difficulty (* 60 60 24)))

(defun pos-prob-days (coins days difficulty n)
  "Calculate the probability of minting a proof-of-stake block within N days with a coin-age of COINS * DAYS given some proof-of-stake DIFFICULTY."
  (do (p
       (prob 1)
       (i 0 (1+ i)))
      ((>= i n) (- 1 prob))
    (setf p (pos-prob-day coins (+ days i) difficulty))
    (setf prob (* prob (- 1 p)))))


;;; Proof-Of-Work

(defconstant +max-reward+ 9999)

(defun pow-reward (difficulty)
  "Calculate the reward for a block generated given some proof-of-work DIFFICULTY."
  (let ((target (floor +max-target+ difficulty))
        (lower-bound 1/100)
        (upper-bound +max-reward+))
    (do (mid-value)
        ((> (+ lower-bound 1/100) upper-bound))
      (setf mid-value (/ (+ lower-bound upper-bound) 2))
      (if (> (* (expt mid-value 4) +max-target+)
             (* (expt +max-reward+ 4) target))
          (setf upper-bound mid-value)
          (setf lower-bound mid-value)))
    (coerce (min upper-bound +max-reward+) 'float)))

(defun pow-prob-hash (difficulty)
  "Calculate the probability of a hash test of some DIFFICULTY passing."
  (let (prob target)
    (setf target (floor +max-target+ difficulty))
    (setf prob (/ target +max-hash+))
    (coerce prob 'long-float)))

(defun pow-prob-hashes (difficulty n)
  "Calculate the probability of at least one hash test of some DIFFICULTY passing in N tests."
  (let ((prob (pow-prob-hash difficulty)))
    (prob-or prob n)))

(defun pow-prob-time (difficulty rate time)
  "Calculate the probability of generating a proof-of-work block within some TIME (in seconds) given some proof-of-work DIFFICULTY."
  (pow-prob-hashes difficulty (* rate time)))

(defun pow-pool-reward (difficulty rate time)
  "Calculate the reward expected from mining in a pool during some TIME (in seconds) given some proof-of-work DIFFICULTY."
  (let ((block-reward (pow-reward difficulty))
        (prob (pow-prob-hash difficulty)))
    (* block-reward time prob rate)))
