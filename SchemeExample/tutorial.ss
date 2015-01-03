

;;liste icinde eleman ariyor
;;eleman bulunursa true , bulunmuyorsa false
;;listeyi lineer olarak dusunerek tasarlandi
(define search
    (lambda (key lst)

    (if
        (null? lst)
        #f
        (if
            (equal? key (car lst))
            #t
            (search key (cdr lst))
        )
    )
))

;;verilen listenin tersini donduruyor
(define reverse_list
    (lambda (lst)

    (if
        (null? lst)
        '()
        (append (reverse_list (cdr lst)) (list (car lst)))
    )
))

;;listenin eleman sayisini return eder
(define length_list
    (lambda (lst)

    (if
        (null? lst)
        0
        (+ 1 (length_list(cdr lst)))
    )
))

;;listedeki elemanlarin index'ini bulur
;;liste bos veya eleman yoksa -1 return eder
;;elemani bulursa index'ini return eder
(define find_index
    (lambda (lst key)
    (if
        (null? lst)
        -1
        (if
            (equal? (car lst) key)
            0
            (if
                (= (find_index (cdr lst) key) -1)
                -1
                (+ 1 (find_index (cdr lst) key))
            )
        )
    )
))

;;verilen index'deki elemani bulur
;;index<0 veya index >= liste uzunlugu veya liste bos ise false return eder
(define find_element
    (lambda (lst index)

    (if 
        (or (null? lst) (< index 0) (>= index (length lst)))
        #f
        (if
            (equal? index 0)
            (car lst)
            (find_element (cdr lst) (- index 1))
        )
    )
))

;;faktoriyel hesabi yapan fonksiyon
(define factorial
    (lambda (n)
    (if
        (= n 1)
        1
        (* n (factorial (- n 1)))
    )
))

;;listenin en kucuk elemanini dondurur
(define findSmallestElement
    (lambda (lst)

    (if
        (null? (cdr lst))
        (car lst)
        (cond
            ((< (car lst) (findSmallestElement (cdr lst))) (car lst))
            (else (findSmallestElement(cdr lst)))
        )
    )
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Bu fonksiyonlar icin listenin sayilardan olustuguna dikkat edin!


;;listedeki elemanlarin toplamini verir
(define findSum 
    (lambda (lst)

    (if
        (null? (cdr lst))
        (car lst)
        (+ (car lst) (findSum (cdr lst)))
    )
))

;;listenin elemanlarinin toplaminin ortalamasini verir
;;liste bossa 0 dondurur
(define findAverage
    (lambda (lst)
    (if
        (null? lst)
        0
        ( / (findSum lst) (length lst))
    )
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


