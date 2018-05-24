#lang planet neil/sicp

;Huffman Encoding Tree
;Leaves
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;Tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;Decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;Set Of Weighted Elements
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;Set
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;========================================================
;Exercise 2.67
; Use the decode procedure to decode the message, and give the result.
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(display (decode sample-message sample-tree))

;Exercise 2.68
; The encode procedure takes as arguments a message and a tree and produces the list of bits
; that gives the encoded message.
; Encode-symbol is a procedure, which you must write,
; that returns the list of bits that encodes a given symbol according to a given tree.
; You should design encode-symbol so that it signals an error if the symbol is not in the tree at all.
; Test your procedure by encoding the result you obtained in exercise 2.67 with the sample tree
; and seeing whether it is the same as the original sample message.
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((null? tree) (error "bad symbol -- ENCODE-SYMBOL" symbol))
        ((leaf? tree) nil)
         ((element-of-set? symbol (symbols (left-branch tree)))
          (cons '0 (encode-symbol symbol (left-branch tree))))
         ((element-of-set? symbol (symbols (right-branch tree)))
          (cons '1 (encode-symbol symbol (right-branch tree))))
         (else (error "bad symbol -- Encode-SYMBOL" symbol))))

 (display (encode '(a d a b b c a) sample-tree))
