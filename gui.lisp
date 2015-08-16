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


(named-readtables:in-readtable :qt)

(defun tr (string)
  (#_QCoreApplication::translate "peercoin-calculator" string))

(defclass main-window ()
  ((coins :accessor coins)
   (age :accessor age)
   (pos-difficulty :accessor pos-difficulty)
   (pow-difficulty :accessor pow-difficulty)
   (hash-rate :accessor hash-rate)
   (results-pos :accessor results-pos)
   (results-pow :accessor results-pow))
  (:metaclass qt-class)
  (:qt-superclass "QWidget")
  (:slots ("calculate()" calculate)))

(defun print-as-percentage (x n)
  "Make a string where X is represented as a percentage with N decimals."
  (let ((fmtstr (concatenate 'string "~," (format nil "~d" n) "f%")))
    (format nil fmtstr (* x 100))))

(defun print-as-fixed-float (x n)
  "Make a string where X is represented as a float with N decimals."
  (let ((fmtstr (concatenate 'string "~," (format nil "~d" n) "f")))
    (format nil fmtstr x)))

(defmethod calculate ((instance main-window))
  (let ((coins (read-from-string (#_text (coins instance))))
        (age (read-from-string (#_text (age instance))))
        (pos-difficulty (read-from-string (#_text (pos-difficulty instance))))
        (pow-difficulty (read-from-string (#_text (pow-difficulty instance))))
        (hash-rate (read-from-string (#_text (hash-rate instance)))))

    ;; POS table
    (#_setItem (results-pos instance) 0 0 (#_new QTableWidgetItem (print-as-percentage (pos-prob-hashes coins age pos-difficulty (* 60 10)) 6)))
    (#_setItem (results-pos instance) 1 0 (#_new QTableWidgetItem (print-as-fixed-float (pos-reward coins age) 2)))
    (#_setItem (results-pos instance) 0 1 (#_new QTableWidgetItem (print-as-percentage (pos-prob-day coins age pos-difficulty) 6)))
    (#_setItem (results-pos instance) 1 1 (#_new QTableWidgetItem (print-as-fixed-float (pos-reward coins (+ age 1)) 2)))
    (let ((times (list 31 90 365)))
      (dotimes (i 3)
        (let ((days (elt times i)))
          (#_setItem (results-pos instance) 0 (+ i 2) (#_new QTableWidgetItem (print-as-percentage (pos-prob-days coins age pos-difficulty days) 6)))
          (#_setItem (results-pos instance) 1 (+ i 2) (#_new QTableWidgetItem (print-as-fixed-float (pos-reward coins (+ age days)) 2))))))

    ;; POW table
    (let ((times (list (* 60 10) (* 60 60 24) (* 60 60 24 31) (* 60 60 24 90) (* 60 60 24 365))))
      (dotimes (i 5)
        (let ((s (elt times i)))
          (#_setItem (results-pow instance) 0 i (#_new QTableWidgetItem (print-as-percentage (pow-prob-time pow-difficulty hash-rate s) 6)))
          (#_setItem (results-pow instance) 1 i (#_new QTableWidgetItem (print-as-fixed-float (pow-reward pow-difficulty) 2)))
          (#_setItem (results-pow instance) 2 i (#_new QTableWidgetItem (print-as-fixed-float (pow-pool-reward pow-difficulty hash-rate s) 6))))))))

(defmethod initialize-instance :after ((instance main-window) &key)
  (new instance)
  (let ((calculate-btn (#_new QPushButton (tr "Calculate")))
        (layout (#_new QVBoxLayout))
        (layout-entries (#_new QHBoxLayout))
        (groupbox-pos (#_new QGroupBox (tr "Proof-Of-Stake (POS)")))
        (layout-pos-entries (#_new QFormLayout))
        (groupbox-pow (#_new QGroupBox (tr "Proof-Of-Work (POW)")))
        (layout-pow-entries (#_new QFormLayout))
        (layout-calculate-btn (#_new QHBoxLayout)))
    (setf (coins instance) (#_new QLineEdit "100")
          (age instance) (#_new QLineEdit "31")
          (pos-difficulty instance) (#_new QLineEdit "10")
          (pow-difficulty instance) (#_new QLineEdit "100000000")
          (hash-rate instance) (#_new QLineEdit "1000000000")
          (results-pos instance) (#_new QTableWidget 2 5)
          (results-pow instance) (#_new QTableWidget 3 5))

    (connect calculate-btn "clicked()" instance "calculate()")

    ;; POS table
    (#_setHorizontalHeaderItem (results-pos instance) 0 (#_new QTableWidgetItem (tr "10 min")))
    (#_setHorizontalHeaderItem (results-pos instance) 1 (#_new QTableWidgetItem (tr "24 hours")))
    (#_setHorizontalHeaderItem (results-pos instance) 2 (#_new QTableWidgetItem (tr "31 days")))
    (#_setHorizontalHeaderItem (results-pos instance) 3 (#_new QTableWidgetItem (tr "90 days")))
    (#_setHorizontalHeaderItem (results-pos instance) 4 (#_new QTableWidgetItem (tr "1 year")))
    (#_setVerticalHeaderItem (results-pos instance) 0 (#_new QTableWidgetItem (tr "Probability of POS block")))
    (#_setVerticalHeaderItem (results-pos instance) 1 (#_new QTableWidgetItem (tr "Reward of POS block")))

    ;; POW table
    (#_setHorizontalHeaderItem (results-pow instance) 0 (#_new QTableWidgetItem (tr "10 min")))
    (#_setHorizontalHeaderItem (results-pow instance) 1 (#_new QTableWidgetItem (tr "24 hours")))
    (#_setHorizontalHeaderItem (results-pow instance) 2 (#_new QTableWidgetItem (tr "31 days")))
    (#_setHorizontalHeaderItem (results-pow instance) 3 (#_new QTableWidgetItem (tr "90 days")))
    (#_setHorizontalHeaderItem (results-pow instance) 4 (#_new QTableWidgetItem (tr "1 year")))
    (#_setVerticalHeaderItem (results-pow instance) 0 (#_new QTableWidgetItem (tr "Probability of POW block")))
    (#_setVerticalHeaderItem (results-pow instance) 1 (#_new QTableWidgetItem (tr "Reward of POW block")))
    (#_setVerticalHeaderItem (results-pow instance) 2 (#_new QTableWidgetItem (tr "Reward in a pool")))

    ;; Layout
    (#_setWindowTitle instance (tr "Peercoin Calculator"))
    (#_addRow layout-pos-entries (#_new QLabel (tr "Number of coins")) (coins instance))
    (#_addRow layout-pos-entries (#_new QLabel (tr "Age of coins (days)")) (age instance))
    (#_addRow layout-pos-entries (#_new QLabel (tr "POS difficulty")) (pos-difficulty instance))
    (#_setLayout groupbox-pos layout-pos-entries)
    (#_addRow layout-pow-entries (#_new QLabel (tr "Hash rate")) (hash-rate instance))
    (#_addRow layout-pow-entries (#_new QLabel (tr "POW difficulty")) (pow-difficulty instance))
    (#_setLayout groupbox-pow layout-pow-entries)
    (#_addWidget layout-entries groupbox-pos)
    (#_addWidget layout-entries groupbox-pow)
    (#_addLayout layout layout-entries)
    (#_setMaximumWidth calculate-btn 200)
    (#_addWidget layout-calculate-btn calculate-btn)
    (#_addLayout layout layout-calculate-btn)
    (#_addWidget layout (results-pos instance))
    (#_addWidget layout (results-pow instance))
    (#_setLayout instance layout)
    (#_resize instance 800 420)))

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
  (let* ((qt-translator (#_new QTranslator))
         (app-translator (#_new QTranslator))
         (locale (#_name (#_QLocale::system)))
         (lang (subseq locale 0 (min 2 (length locale))))
         (qt-translator-file (concatenate 'string
                                          (#_QLibraryInfo::location (#_QLibraryInfo::TranslationsPath))
                                          "/qt_"
                                          locale))
         (app-translator-file (concatenate 'string
                                           (namestring (asdf:system-source-directory 'peercoin-calculator))
                                           "translations/"
                                           lang)))
    (#_QTextCodec::setCodecForTr (#_QTextCodec::codecForName "UTF-8"))
    (#_load qt-translator qt-translator-file)
    (#_installTranslator *qapplication* qt-translator)
    (#_load app-translator app-translator-file)
    (#_installTranslator *qapplication* app-translator))

  (with-main-window (window (make-instance 'main-window))
    (when style
      (#_QApplication::setStyle
       (#_QStyleFactory::create (ecase style
                                  (:cde "CDE")
                                  (:macintosh "Macintosh")
                                  (:windows "Windows")
                                  (:motif "Motif")))))))
