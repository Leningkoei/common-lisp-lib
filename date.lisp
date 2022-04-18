(load "define")
(load "new")
(defclass Date ()
  (day month year))
(defmethod Date-day ((this Date))
  (slot-value this 'day))
(defmethod Date-month ((this Date))
  (slot-value this 'month))
(defmethod Date-year ((this Date))
  (slot-value this 'year))
(funcall
 (lambda ()
   (define
     ((month-day-map (list 31 28 31 30 31 30 31 31 30 31 30 31))
      (zero-year 1970)
      (leap?
       (lambda (year)
         (and (= 0 (mod year 4))
              (or (= 0 (mod year 400))
                  (not (= 0 (mod year 100))))))))
     (define
       ((month-day
         (lambda (month year)
           (if (and (funcall leap? year) (= 2 month)) 29
             (nth (1- month) month-day-map))))
        (year-day
         (lambda (year)
           (if (funcall leap? year) 366 365))))
       (define
         ((day->num
           (lambda (day month year)
             (if (<= zero-year year) (1- day)
               (let ((month-day (funcall month-day month year)))
                 (- (1+ (- month-day day)))))))
          (month->num
           (lambda (month year)
             (let ((initial-month (if (<= zero-year year) 1 12))
                   (f1 (if (<= zero-year year) '+ '-))
                   (f2 (if (<= zero-year year) '1+ '1-)))
               (define
                 ((iterator
                   (lambda*
                    (result current-month year)
                    (if (= month current-month) result
                      (funcall* iterator
                                (funcall f1
                                         result
                                         (funcall month-day current-month year))
                                (funcall f2 current-month)
                                year)))))
                 (funcall* iterator 0 initial-month year)))))
          (year->num
           (lambda (year)
             (let ((f1 (if (< zero-year year) '+ '-))
                   (f2 (if (< zero-year year) '1- '1+)))
               (define
                 ((iterator
                   (lambda* (result current-year)
                            (if (or (= zero-year current-year)
                                    (= (1- zero-year) current-year))
                                result
                              (funcall*
                               iterator
                               (funcall f1
                                        result
                                        (funcall year-day
                                                 (funcall f2
                                                          current-year)))
                               (funcall f2 current-year))))))
                 (funcall* iterator 0 year)))))
          (num->day->date
           (lambda (current-year current-month num)
             (let ((current-day
                    (if (<= 0 num) (1+ num)
                      (let ((month-day (funcall month-day
                                                current-month
                                                current-year)))
                        (+ month-day (1+ num))))))
               (print current-day)
               (print current-month)
               (print current-year)
               (new 'Date
                    (cons 'day current-day)
                    (cons 'month current-month)
                    (cons 'year current-year))))))
         (define
           ((num->month+->day->date
             (lambda (current-year num)
               (let ((f1 (if (<= 0 num) '+ '-))
                     (f2 (if (<= 0 num) '1+ '1-))
                     (f3 (if (<= 0 num) '> '>=))
                     (initial-month (if (<= 0 num) 1 12))
                     (num (abs num)))
                 (define
                   ((iterator
                     (lambda* (current-month num)
                              (let ((month-day (funcall month-day
                                                        current-month
                                                        current-year)))
                                (if (funcall f3 month-day num)
                                    (funcall num->day->date
                                             current-year
                                             current-month
                                             (funcall f1 num))
                                  (funcall* iterator
                                            (funcall f2 current-month)
                                            (- num month-day)))))))
                   (funcall* iterator initial-month num))))))
           (define
             ((num->year+->month+->day->date
               (lambda (num)
                 (let ((f1 (if (<= 0 num) '+ '-))
                       (f2 (if (<= 0 num) '1+ '1-))
                       (f3 (if (<= 0 num) '> '>=))
                       (initial-year (if (<= 0 num) 1970 1969))
                       (num (abs num)))
                   (define
                     ((iterator
                       (lambda* (current-year num)
                                (let ((year-day
                                       (funcall year-day current-year)))
                                  (if (funcall f3 year-day num)
                                      (funcall num->month+->day->date
                                               current-year
                                               (funcall f1 num))
                                    (funcall* iterator
                                              (funcall f2 current-year)
                                              (- num year-day)))))))
                     (funcall* iterator initial-year num))))))
             (defmethod date->num ((this Date))
               (let ((day (date-day this))
                     (month (date-month this))
                     (year (date-year this)))
                 (+ (funcall day->num day month year)
                    (funcall month->num month year)
                    (funcall year->num year))))
             (defmethod num->date (num)
               (funcall num->year+->month+->day->date num)))))))))
(defmethod date+ ((date Date) day-num)
  (let ((date-num (date->num date)))
    (let ((total-date-num (+ day-num date-num)))
      (num->date total-date-num))))
(defmethod date- ((date-a Date) (date-b Date))
  (let ((date-a-num (date->num date-a))
        (date-b-num (date->num date-b)))
    (- date-a-num date-b-num)))
(defmethod stringify ((this Date))
  (let ((day (date-day this))
        (month (date-month this))
        (year (date-year this)))
    (format nil "~d/~d/~d"
            day month year)))

(defun test ()
  (let ((date-a (new 'Date (cons 'day 1) (cons 'month 1) (cons 'year 1970)))
        (date-b (new 'Date (cons 'day 1) (cons 'month 1) (cons 'year 1900))))
    (print (stringify date-a))
    (print (stringify date-b))
    (let ((num-a-sub-b (date- date-a date-b)))
      (print num-a-sub-b)
      (let ((date-b-kai (date+ date-a (- num-a-sub-b))))
        (print (stringify date-b-kai))
        nil))))
