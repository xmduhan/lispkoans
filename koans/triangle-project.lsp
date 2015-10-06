;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


"you need to write the triangle method"

(defun list-left-shift (l)
  (append (cdr l) (list (car l))))

(defun is-scalene(a b c)
  (let* ((l (list a b c)) (y (max a b c)) (r (delete y l)))
    (= (expt y 2) (+ (expt (first r) 2) (expt (second r) 2)))
  )
)

(define-condition triangle-error  (error) ())

(defun triangle (a b c)
  (let ((l (list a b c)))
    (dotimes (i 3)
      (if (<= (+ (first l) (second l)) (third l)) (error 'triangle-error))
      (setf l (list-left-shift l)))
    (if (= a b c) (return-from triangle :equilateral))
    (dotimes (i 3)
      (if (= (first l) (second l)) (return-from triangle :isosceles))
      (setf l (list-left-shift l)))
  )
  :scalene
)

(define-test test-equilateral-triangles-have-equal-sides
    (assert-equal :equilateral (triangle 2 2 2))
    (assert-equal :equilateral (triangle 10 10 10)))


(define-test test-isosceles-triangles-have-two-equal-sides
    (assert-equal :isosceles (triangle 3 4 4))
    (assert-equal :isosceles (triangle 4 3 4))
    (assert-equal :isosceles (triangle 4 4 3))
    (assert-equal :isosceles (triangle 10 10 2)))


(define-test test-scalene-triangles-have-no-equal-sides
    (assert-equal :scalene (triangle 3 4 5))
    (assert-equal :scalene (triangle 10 11 12))
    (assert-equal :scalene (triangle 5 4 2)))


(define-test test-illegal-triangles-throw-exceptions
    (assert-error 'triangle-error (triangle 0 0 0))
    (assert-error 'triangle-error (triangle 3 4 -5))
    (assert-error 'triangle-error (triangle 1 1 3))
    (assert-error 'triangle-error (triangle 2 4 2)))
