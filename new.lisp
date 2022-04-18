(defun new (class &rest cons-args)
  (let ((this (make-instance class)))
    (map nil
         (lambda (cons-arg)
           (let ((key (car cons-arg))
                 (val (cdr cons-arg)))
             (setf (slot-value this key) val)))
         cons-args)
    this))
