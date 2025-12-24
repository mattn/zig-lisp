(defun fizzbuzz ()
  (dotimes (i 100)
    (setq n (1+ i))
    (print
     (cond
      ((= 0 (mod n 15)) "FizzBuzz")
      ((= 0 (mod n 3)) "Fizz")
      ((= 0 (mod n 5)) "Buzz")
      (t n)))))

(fizzbuzz)
