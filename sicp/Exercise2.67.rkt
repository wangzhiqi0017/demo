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

;Display
(define (display-newline data)
  (display data)
  (newline))

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
(display-newline (decode sample-message sample-tree))

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

 (display-newline  (encode '(a d a b b c a) sample-tree))

;Exercise 2.69
; The following procedure takes as its argument a list of symbol-frequency pairs
; (where no symbol appears in more than one pair)
; and generates a Huffman encoding tree according to the Huffman algorithm.
; Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves.
; Successive-merge is the procedure you must write,
; using make-code-tree to successively merge the smallest-weight elements of the set until there is only one element left,
; which is the desired Huffman tree.
; (This procedure is slightly tricky, but not really complicated.
;  If you find yourself designing a complex procedure, then you are almost certainly doing something wrong.
;  You can take significant advantage of the fact that we are using an ordered set representation.)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge
               (adjoin-set
                (make-code-tree (car leaf-set) (cadr leaf-set))
                (cddr leaf-set))))))

(define huf-tree (generate-huffman-tree (list '(C 1) '(B 2) '(A 4) '(D 1))))
(display-newline sample-tree)
(display-newline huf-tree)
(display-newline (encode '(a d a b b c a) huf-tree))
(display-newline (decode sample-message huf-tree))

;Exercise 2.70
; The following eight-symbol alphabet with associated relative frequencies
; was designed to efficiently encode the lyrics of 1950s rock songs.
; (Note that the ``symbols'' of an ``alphabet'' need not be individual letters.)
;
; A	2	NA	16
; BOOM	1	SHA	3
; GET	2	YIP	9
; JOB	2	WAH	1
; 
; Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree,
; and use encode (exercise 2.68) to encode the following message:
;
; Get a job
; Sha na na na na na na na na
; Get a job
; Sha na na na na na na na na
; Wah yip yip yip yip yip yip yip yip yip
; Sha boom
; 
; How many bits are required for the encoding?
; What is the smallest number of bits that would be needed to encode this song
; if we used a fixed-length code for the eight-symbol alphabet?
; fixed-length: bits per symbol * bits count => log,2,8 * 36 = 3.36 = 108
; variable-length: 14*2 + 12*2 + 23 + 9 = 84
(define lyric-tree
         (generate-huffman-tree
                     (list '(A 2) '(NA 16) '(BOOM 1) '(SHA 3) '(GET 2) '(YIP 9) '(JOB 2) '(WAH 1))))
(display-newline lyric-tree)
(display-newline (encode '(Get a job) lyric-tree))
(display-newline (encode '(Sha na na na na na na na na) lyric-tree))
(display-newline (encode '(Wah yip yip yip yip yip yip yip yip yip) lyric-tree))
(display-newline (encode '(Sha boom) lyric-tree))

;Exercice 2.71
; Suppose we have a Huffman tree for an alphabet of n symbols,
; and that the relative frequencies of the symbols are 1, 2, 4, ..., 2n-1.
; Sketch the tree for n=5; for n=10.
; In such a tree (for general n) how many bits are required to encode the most frequent symbol? the least frequent symbol?

;Exercise 2.72
; Consider the encoding procedure that you designed in exercise 2.68.
; What is the order of growth in the number of steps needed to encode a symbol?
; Be sure to include the number of steps needed to search the symbol list at each node encountered.
; To answer this question in general is difficult.
; Consider the special case where the relative frequencies of the n symbols are as described in exercise 2.71,
; and give the order of growth (as a function of n) of the number of steps needed
; to encode the most frequent and least frequent symbols in the alphabet.