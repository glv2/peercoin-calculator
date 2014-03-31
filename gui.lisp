#|
Copyright 2014 Guillaume LE VAILLANT

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


(named-readtables:in-readtable :qt)

(defclass main-window ()
  ((coins :accessor coins)
   (age :accessor age)
   (pos-difficulty :accessor pos-difficulty)
   (pow-difficulty :accessor pow-difficulty)
   (hash-rate :accessor hash-rate)
   (probabilities :accessor probabilities)
   (rewards :accessor rewards))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("calculate()" calculate)))

(defmethod calculate ((instance main-window))
  (flet ((print-as-percentage (x) (format nil "~,6f%" (* x 100)))
         (print-as-fixed-float (x) (format nil "~,2f" x)))
    (let (coins age pos-difficulty pow-difficulty hash-rate)
      (setf coins (read-from-string (#_text (coins instance)))
            age (read-from-string (#_text (age instance)))
            pos-difficulty (read-from-string (#_text (pos-difficulty instance)))
            pow-difficulty (read-from-string (#_text (pow-difficulty instance)))
            hash-rate (read-from-string (#_text (hash-rate instance))))

      ;; Probability table
      (#_setItem (probabilities instance) 0 0 (#_new QTableWidgetItem (print-as-percentage (pos-prob-hashes coins age pos-difficulty (* 60 10)))))
      (#_setItem (probabilities instance) 0 1 (#_new QTableWidgetItem (print-as-percentage (pos-prob-day coins age pos-difficulty))))
      (#_setItem (probabilities instance) 0 2 (#_new QTableWidgetItem (print-as-percentage (pos-prob-days coins age pos-difficulty 31))))
      (#_setItem (probabilities instance) 0 3 (#_new QTableWidgetItem (print-as-percentage (pos-prob-days coins age pos-difficulty 90))))
      (#_setItem (probabilities instance) 0 4 (#_new QTableWidgetItem (print-as-percentage (pos-prob-days coins age pos-difficulty 365))))
      (#_setItem (probabilities instance) 1 0 (#_new QTableWidgetItem (print-as-percentage (pow-prob-time pow-difficulty hash-rate (* 60 10)))))
      (#_setItem (probabilities instance) 1 1 (#_new QTableWidgetItem (print-as-percentage (pow-prob-time pow-difficulty hash-rate (* 60 60 24)))))
      (#_setItem (probabilities instance) 1 2 (#_new QTableWidgetItem (print-as-percentage (pow-prob-time pow-difficulty hash-rate (* 60 60 24 31)))))
      (#_setItem (probabilities instance) 1 3 (#_new QTableWidgetItem (print-as-percentage (pow-prob-time pow-difficulty hash-rate (* 60 60 24 90)))))
      (#_setItem (probabilities instance) 1 4 (#_new QTableWidgetItem (print-as-percentage (pow-prob-time pow-difficulty hash-rate (* 60 60 24 365)))))

      ;; Reward table
      (let* ((reward (pos-reward coins age))
             (total (+ coins reward)))
        (#_setItem (rewards instance) 0 0 (#_new QTableWidgetItem (print-as-fixed-float reward)))
        (#_setItem (rewards instance) 0 1 (#_new QTableWidgetItem (print-as-fixed-float total))))
      (let* ((reward (pow-reward pow-difficulty))
             (total (+ coins reward)))
        (#_setItem (rewards instance) 1 0 (#_new QTableWidgetItem (print-as-fixed-float reward)))
        (#_setItem (rewards instance) 1 1 (#_new QTableWidgetItem (print-as-fixed-float total)))))))

(defmethod initialize-instance :after ((instance main-window) &key)
  (new instance)
  (let ((calculate-btn (#_new QPushButton "Calculate"))
        (layout (#_new QVBoxLayout))
        (layout-entries (#_new QHBoxLayout))
        (groupbox-pos (#_new QGroupBox "Proof-Of-Stake (POS)"))
        (layout-pos-entries (#_new QFormLayout))
        (groupbox-pow (#_new QGroupBox "Proof-Of-Work (POW)"))
        (layout-pow-entries (#_new QFormLayout))
        (layout-calculate-btn (#_new QHBoxLayout)))
    (setf (coins instance) (#_new QLineEdit "100")
          (age instance) (#_new QLineEdit "31")
          (pos-difficulty instance) (#_new QLineEdit "10")
          (pow-difficulty instance) (#_new QLineEdit "100000000")
          (hash-rate instance) (#_new QLineEdit "0")
          (probabilities instance) (#_new QTableWidget 2 5)
          (rewards instance) (#_new QTableWidget 2 2))

    (connect calculate-btn "clicked()" instance "calculate()")

    ;; Probability table
    (#_setHorizontalHeaderItem (probabilities instance) 0 (#_new QTableWidgetItem "10 min"))
    (#_setHorizontalHeaderItem (probabilities instance) 1 (#_new QTableWidgetItem "24 hours"))
    (#_setHorizontalHeaderItem (probabilities instance) 2 (#_new QTableWidgetItem "31 days"))
    (#_setHorizontalHeaderItem (probabilities instance) 3 (#_new QTableWidgetItem "90 days"))
    (#_setHorizontalHeaderItem (probabilities instance) 4 (#_new QTableWidgetItem "1 year"))
    (#_setVerticalHeaderItem (probabilities instance) 0 (#_new QTableWidgetItem "Probability of POS block"))
    (#_setVerticalHeaderItem (probabilities instance) 1 (#_new QTableWidgetItem "Probability of POW block"))

    ;; Reward table
    (#_setHorizontalHeaderItem (rewards instance) 0 (#_new QTableWidgetItem "Reward"))
    (#_setHorizontalHeaderItem (rewards instance) 1 (#_new QTableWidgetItem "Total"))
    (#_setVerticalHeaderItem (rewards instance) 0 (#_new QTableWidgetItem "POS block"))
    (#_setVerticalHeaderItem (rewards instance) 1 (#_new QTableWidgetItem "POW block"))

    ;; Layout
    (#_setWindowTitle instance "Peercoin Calculator")
    (#_addRow layout-pos-entries (#_new QLabel "Number of coins") (coins instance))
    (#_addRow layout-pos-entries (#_new QLabel "Age of coins (days)") (age instance))
    (#_addRow layout-pos-entries (#_new QLabel "POS difficulty") (pos-difficulty instance))
    (#_setLayout groupbox-pos layout-pos-entries)
    (#_addRow layout-pow-entries (#_new QLabel "Hash rate") (hash-rate instance))
    (#_addRow layout-pow-entries (#_new QLabel "POW difficulty") (pow-difficulty instance))
    (#_setLayout groupbox-pow layout-pow-entries)
    (#_addWidget layout-entries groupbox-pos)
    (#_addWidget layout-entries groupbox-pow)
    (#_addLayout layout layout-entries)
    (#_setMaximumWidth calculate-btn 200)
    (#_addWidget layout-calculate-btn calculate-btn)
    (#_addLayout layout layout-calculate-btn)
    (#_addWidget layout (probabilities instance))
    (#_addWidget layout (rewards instance))
    (#_setLayout instance layout)
    (#_resize instance 720 400)))

(defun mk-qapplication (name &rest args)
  "A rewrite of QT:MAKE-QAPPLICATION to allow setting an application name other than 'argv0dummy'."
  (cond (*qapplication*)
        (t
         (ensure-smoke :qtcore)
         (ensure-smoke :qtgui)
         (let ((instance (#_QCoreApplication::instance)))
           (setf *qapplication*
                 (if (null-qobject-p instance)
                     (qt::%make-qapplication (cons name args))
                     instance))))))

(defun gui (&optional style)
  "Start the Peercoin Calculator GUI."
  (mk-qapplication "Peercoin Calculator")
  (with-main-window (window (make-instance 'main-window))
    (when style
      (#_QApplication::setStyle
       (#_QStyleFactory::create (ecase style
                                  (:cde "CDE")
                                  (:macintosh "Macintosh")
                                  (:windows "Windows")
                                  (:motif "Motif")))))))
