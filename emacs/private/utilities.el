(defun tor/odds-to-probs (h d a)
  "Computes the odds of home H, draw D, and away A."
  (let* ((tot (odds-total h d a)))
    (list
     tot
     (list (/ 1.0 (* h tot))
           (/ 1.0 (* d tot))
           (/ 1.0 (* a tot))))))
