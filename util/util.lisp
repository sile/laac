(in-package :laac.util)

(defmacro with-oop-like ((var package) &body body)
  (let ((method (gensym))
        (args   (gensym)))
    `(macrolet ((,var (,method &rest ,args)
                  (let ((fn (find-symbol (symbol-name ,method) ,package)))
                    (cons fn (cons ',var ,args)))))
       ,@body)))
